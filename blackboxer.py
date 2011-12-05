def findPrimitiveType(s):
    bool = ['#t', '#T', '#f', '#F']
    if (s in bool) :
        return 'boolean'
    elif(s.isdigit()) :
        return 'int'
    elif(s.isalpha() or s.isalnum()) :
        return 'string'
    else :
        return 'char'

class ReducedExpr:
    def __init__(self, expr, reduct, adtName):
        self.expr = expr
        self.reduct = reduct
        self.adtName = adtName

class Expr:
    def __init__(self,op,args=None):
        self.op = op
        if args:
            self.args = args
        else :
            self.args = []
    
    # class method that turns Expr objects 
    # into Scheme expressions :-) 
    def toSexpr(self):
        sexpr = "(" +  str(self.op)

        for arg in self.args:
            if (isinstance(arg['Value'], Expr)): 
                sexpr += " " + arg['Value'].toSexpr()
            else: 
                sexpr += " " + str(arg['Value']) 

        sexpr += ")"     
        return sexpr

    # returns all operations used in the Expr
    def findAllOps(self):
        ops = [self.op]

        for arg in self.args:
            if ( isinstance( arg['Value'], Expr )):
                ops += arg['Value'].findAllOps()

        return ops

    # returns all values within the Expr
    def findAllValues(self):
        vals = []

        for arg in self.args:
            if( isinstance( arg['Value'], Expr ) ):
                vals += arg['Value'].findAllValues()
            else:
                vals.append( arg['Value'] )

        return vals
    
def equalExpr(expr1, expr2):
    if (expr1.op != expr2.op) :
        return False
    else :
        if (len(expr1.args) == 0 and len(expr2.args) == 0):
            return True
        elif (len(expr1.args) == len(expr2.args)):
            for i in range(len(expr1.args)) :
                if (isinstance(expr1.args[i].get('Value'), Expr) and isinstance(expr2.args[i].get('Value'), Expr)):
                    return equalExpr(expr1.args[i].get('Value'),expr2.args[i].get('Value'))
                else :
                    return (expr1.args[i].get('Value') == expr2.args[i].get('Value'))
        else:
            return False
    
def removeDups(rExprs):
    seen = set()
    isUnique = True

    for rExpr in rExprs:
        for item in seen:
            #if equalExpr(rExpr.expr, item.expr):
            if equalExpr(rExpr.expr, item.expr):
                isUnique = False
        
        if isUnique:        
            seen.add(rExpr)
            isUnique = True
                    
    return seen;

def sortExpr(rExprs):
    return sorted(rExprs, key=lambda reducedExpr: reducedExpr.adtName)
    
def writeBlackBoxer(signatures, output, fileName, reducedExprs):
    # remove duplicate exprs
    reducedExprs = removeDups(reducedExprs)
    reducedExprs = sortExpr(reducedExprs)
    
    output.write(';;; A black-box test program for the ')
    if len(signatures) == 1:
        output.write(signatures[0].typename + ' ')
    elif len(signatures) == 2:
        output.write(signatures[0].typename + ' and ' + signatures[1].typename)
    else :
        for sig in signatures[:-1]:
            output.write(sig.typename + ', ')
        output.write('and ' + signatures[-1].typename)
    output.write(' ADTS\n')
    output.write(';;; specified by ' + fileName + '.\n\n')
    output.write('''(import (rnrs base)
    \t(rnrs exceptions)        ; for the guard syntax
    \t(rnrs io simple)         ; for display etc\n''')
    if len(signatures) == 1:
        output.write('\t(testing ' + signatures[0].typename + ' ))\n')
    else:
        for sig in signatures[:-1]:
            output.write('        (testing ' + sig.typename + ')\n')
        output.write('\t(testing ' + signatures[-1].typename + '))\n\n')
    output.write(''';;; Counters for the summary report when testing is complete.
    
    (define tests-run 0)
    (define tests-passed 0)
    (define tests-failed 0)
    
    ;;; Syntax to make testing more convenient.
    ;;;
    ;;; (test <name> <expr>) evaluates <expr>.
    ;;; If <expr> evaluates to a true value (any value other #f),
    ;;; then the test has been passed.
    ;;; If <expr> evaluates to #f, then the test has been failed.
    ;;; If an exception occurs during evaluation of <expr>, then
    ;;; the test has been failed.
    
    (define-syntax test
      (syntax-rules ()
        ((_ name expr)
         (begin (set! tests-run (+ tests-run 1))
                (if (guard (exn (else #f))
                      expr)
                    (set! tests-passed (+ tests-passed 1))
                    (begin (set! tests-failed (+ tests-failed 1))
                           (display "Failed test: ")
                           (display name)
                           (newline)))))))
    
    ;;; The black-box tests.\n\n''')
    lastAdt = ''
    testCount = 0
    for r in reducedExprs:
        if(lastAdt != r.adtName):
            lastAdt = r.adtName
            testCount = 0
        output.write("\t(test \"" + r.adtName + unicode(testCount) +"\" (")
        type = findPrimitiveType(r.reduct)
        if (type == 'int'):
            output.write("= ")
        else:
            output.write("equal? ")
        output.write(r.reduct + " " + r.expr.toSexpr() + ")\n")
        
        testCount += 1
    
    output.write('''
    ;;; Summary of results.
    
    (display "SUMMARY: failed ")
    (display tests-failed)
    (display " of ")
    (display tests-run)
    (display " tests.")
    (newline)
    
    ;;; Sanity check.
    
    (if (not (= tests-run
                (+ tests-passed tests-failed)))
        (begin (display "Oops...") (newline)))
                                                ''')
    