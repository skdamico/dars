import primitive
import ExprGen as gen

#removes all duplicates from a list of reducedExprs    
def removeDups(rExprs):
    seen = []
    isUnique = True
    for rExpr in rExprs:
        for item in seen:
            #if equalExpr(rExpr.expr, item.expr):
            if gen.equalExpr(rExpr.expr, item.expr) and rExpr.reduct == item.reduct:
                isUnique = False
        
        if isUnique:        
            seen.append(rExpr)
        isUnique = True
                    
    return seen;

#sortExpr - sorts a list of reducedExprs
def sortExpr(rExprs):
    return sorted(rExprs, key=lambda reducedExpr: reducedExpr.adtName)

#writeBlackBoxer - creates the black box tester output    
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
            output.write('\t\t(testing ' + sig.typename + ')\n')
        output.write('\t\t(testing ' + signatures[-1].typename + '))\n\n')
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
        type = primitive.findPrimitiveType(r.reduct)
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
    
