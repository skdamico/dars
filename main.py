import darslex
import yacc
import Queue
import codecs, sys
import random
import string
import re
from collections import defaultdict

tokens = darslex.tokens

#Rewrites System.out to unicode 
sys.stdout = codecs.getwriter('utf-8')(sys.stdout)

#globals
signatures = []
equations = []

#############################################
#############################################
####  Defining Grammar                  #####
#############################################
#############################################

precedence = (
    ('left', 'DIGIT'),
)

#Define grammar rules for the parser
def p_input(p):
    'input : SIGNATURE signatures EQUATIONS equations'
    p[0] = Node('input', [p[2], p[4]])

def p_signatures(p): 
    '''signatures : signature signatures2'''
    p[0] = Node('signatures', [p[1], p[2]])

def p_signatures2(p):
    '''signatures2 : signatures
                   | empty'''
    p[0] = p[1]

def p_signature(p): 
    'signature : ADT typename operation_specs'
    p[0] = Node('signature', [p[2], p[3]])

def p_operation_specs(p):
    '''operation_specs : operation_spec operation_specs2'''
    p[0] = Node('operation-specs', [p[1], p[2]])

def p_operation_specs2(p):
    '''operation_specs2 : operation_specs
                        | empty'''
    p[0] = p[1]

def p_operation_spec(p):
    '''operation_spec : operation COLON operation_spec2'''
    p[0] = Node('operation-spec', [p[1], p[3]])

def p_operation_spec2(p):
    '''operation_spec2 : ARROW type 
                       | arg_types ARROW type'''
    if len(p) == 3 :
        p[0] = p[2]
    else :
        p[0] = Node('operation-spec-args', [p[1], p[3]])

def p_operation(p):
    'operation : identifier'
    p[0] = Node('operation', [p[1]])

def p_arg_types(p):
    '''arg_types : type arg_types2'''
    p[0] = Node('arg-types', [p[1], p[2]])

def p_arg_types2(p):
    '''arg_types2 : STAR arg_types
                  | empty'''
    if len(p) == 3 :
        p[0] = p[2]
    else :
        p[0] = p[1]

def p_type(p): 
    '''type : TYPENAME
            | typename'''
    if (isinstance(p[1], str)) :
        p[0] = Node('type', [], p[1])
    else:
        p[0] = Node('type', [p[1]])

def p_typename(p):
    'typename : identifier'
    p[0] = Node('typename', [p[1]])


def p_identifier(p):
    '''identifier : ID'''
    p[0] = Node('identifier', [], p[1])

## 
#def p_identifier(p):
#    '''identifier : initial subsequent_star
#                  | peculiar_identifier'''
#   if len(p) == 2:         
#       p[0] = Node('identifier', [p[1]])
#   else
#       p[0] = Node('identifier, [p[1], p[2]])

##Scheme grammmars which were commented out for Assignment 4
#def p_peculiar_identifier(p):
#    '''peculiar_identifier : ARROW subsequent_star 
#                           | ELLIPSIS
#                           | PLUS
#                           | MINUS'''
#   if len(p) == 2: 
#       p[0] = Node('peculiar-identifier', [], p[1])
#   else:
#       p[0] = Node('peculiar-identifier', [p[2]])
#
#def p_subsequent_star(p):
#    '''subsequent_star : subsequent subsequent_star2'''
#    p[0] = Node('subsequent-star', [p[1], p[2]])
#
#def p_subsequent_star2(p):
#    '''subsequent_star2 : subsequent_star
#                        | empty'''
#    p[0] = p[1] 
#
#def p_subsequent(p):
#    '''subsequent : initial
#                  | special_subsequent
#                  | unicode_subsequent
#                  | DIGIT'''
#
#def p_special_subsequent(p):
#    '''special_subsequent : PLUS 
#                          | PERIOD
#                          | MINUS
#                          | AT_SYMBOL'''
#    p[0] = ('special-subsequent', p[1])
#
#def p_special_initial(p):
#    '''special_initial : SPECIAL_INITIAL''' 
#    p[0] = ('special-initial', p[1])
#
#def p_constituent(p):
#    '''constituent : unicode_constituent
#                   | LETTER'''
#    p[0] = ('constituent', p[1])
#
#def p_unicode_constituent(p):
#    '''unicode_constituent : UNICODE_LU
#                           | UNICODE_LL
#                           | UNICODE_LT
#                           | UNICODE_LM
#                           | UNICODE_LO
#                           | UNICODE_MN
#                           | UNICODE_NL
#                           | UNICODE_NO
#                           | UNICODE_PD
#                           | UNICODE_PC
#                           | UNICODE_PO
#                           | UNICODE_SC
#                           | UNICODE_SM
#                           | UNICODE_SK
#                           | UNICODE_SO
#                           | UNICODE_CO'''
#    p[0] = p[1]
#
#def p_inline_hex_escape(p):
#    'inline_hex_escape : ESCAPE LETTER hex_scalar_value SEMICOLON'
#    if p[2] == 'x' : 
#       p[0] = Node('inline-hex-escape', [r"\x", p[3], ";"])
#
#def p_hex_scalar_value(p):
#    'hex_scalar_value : hex_digit_plus'
#    p[0] = ('hex-scalar-value', p[1])
#
#def p_hex_digit_plus(p):
#                           | UNICODE_PO
#                           | UNICODE_SC
#                           | UNICODE_SM
#                           | UNICODE_SK
#                           | UNICODE_SO
#                           | UNICODE_CO'''
#    p[0] = p[1]
#
#def p_inline_hex_escape(p):
#    'inline_hex_escape : ESCAPE LETTER hex_scalar_value SEMICOLON'
#    if p[2] == 'x' : p[0] = ('inline-hex-escape', (r"\x", p[3], ";"))
#
#def p_hex_scalar_value(p):
#    'hex_scalar_value : hex_digit_plus'
#    p[0] = ('hex-scalar-value', p[1])
#
#def p_hex_digit_plus(p):
#    '''hex_digit_plus : hex_digit hex_digit_plus2'''
#    p[0] = ('hex-digit-plus', (p[1], p[2]))
#
#def p_hex_digit_plus2(p):
#    '''hex_digit_plus2 : hex_digit_plus
#                       | empty'''
#    p[0] = ('hex-digit-plus2', p[1])
#
#def p_hex_digit(p):
#    '''hex_digit : DIGIT
#                 | LETTER'''
#    p[0] = ('hex-digit', p[1])

def p_equations(p): 
    '''equations : equation equations 
                 | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = Node('equations', [p[1], p[2]])

def p_equation(p):
    '''equation : term EQUAL rhs'''
    p[0] = Node('equation', [p[1], p[3]])
    
def p_term(p):
    '''term : identifier
            | LEFT_PARENS operation args RIGHT_PARENS'''
    if (len(p) == 2) :
        p[0] = Node('term', [p[1]])
    else :
        p[0] = Node('term', [p[2], p[3]]) 

def p_args(p):
    '''args : term args
            | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = Node('args', [p[1], p[2]])

def p_rhs(p): 
    '''rhs : BOOLEAN
           | uinteger_10
           | identifier 
           | LEFT_PARENS primitive rhs_args RIGHT_PARENS
           | LEFT_PARENS operation rhs_args RIGHT_PARENS'''
    if len(p) == 2: 
        p[0] = Node('rhs', [p[1]])
    else :
        p[0] = Node('rhs', [p[2], p[3]])

## rhs_args is same exact thing as args... should just reuse?
def p_rhs_args(p): 
    '''rhs_args : rhs rhs_args 
                | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = Node('rhs_args', [p[1], p[2]])

def p_primitive(p):
    '''primitive : NOT 
                 | SPECIAL_PRIMITIVE
                 | STAR
                 | PLUS
                 | MINUS'''
    p[0] = Node('primitive', [], p[1])

def p_uinteger_10(p): 
    '''uinteger_10 : digit_plus'''
    p[0] = Node('uinteger_10', [p[1]])

def p_digit_plus(p):
    '''digit_plus : DIGIT digit_plus2'''
    p[0] = p[2]

def p_digit_plus2(p):
    '''digit_plus2 : digit_plus
                   | empty'''
    p[0] = p[1]   

def p_empty(p):
    'empty :'
    p[0] = Node('empty', [])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

#############################################
#############################################
####  Support Classes                   #####
#############################################
#############################################


#Simple implementation of an AST node
class Node:
    def __init__(self,type,children=None,value=None):
        self.type = type
        if children :
            self.children = children
        else :
            self.children = []
        self.value = value 

class SignatureStruct:
    def __init__(self,typename,opspecs):
        self.typename = typename
        self.opspecs = opspecs
        
    def getAllOpNames(self):
        opNames = []

        for opspec in self.opspecs:
            opNames.append( opspec.operation )

        return opNames

    def containsOperation(self, opName):
        if (opName in self.getAllOpNames()):
            return True

        return False

    
class OperationSpecStruct:
    def __init__(self,operation,args,output):
        self.operation = operation
        self.args = args
        self.output = output 

    def toString(self):
        string = self.operation + ": "
        args = []
        args += self.args

        while ( len(args) > 0 ):
            arg = args.pop(0)
            string += arg + " "

            if not( len(args) <= 0 ):
                string += " * "

        string += "-> " + self.output

        return string

class EquationStruct:
    def __init__(self,left,right):
        self.left = left
        self.right = right

    def getOpList(self):
        ops = []

        if isinstance( self.left, Expr ):
            ops.append( self.left.findAllOps() )

        if isinstance( self.right, Expr ):
            ops.append( self.right.findAllOps() )

        return ops 

    def toString(self):
        text = ""

        if isinstance(self.left, Expr):
            text += self.left.toSexpr()
        else:
            text += self.left

        text += " = "
    
        if isinstance(self.right, Expr):
            text += self.right.toSexpr()
        else:
            text += self.right

        return text

    def validRhs(self):
        valid = True
        leftIds     = []
        rightVals    = []

        # locates all identifiers on the left side of an equation and adds 
        # them to an array. If an identifier appears more than once 
        # immediately returns and states the equation is invalid
        if isinstance( self.left, Expr ):
            if containDups( self.left.findAllValues() ):
                return False

            leftIds += self.left.findAllValues()
        else:
            leftIds.append( self.left )

        # finds all the identifiers and all the allowed values for the 
        # right hand side of an equation
        if isinstance( self.right, Expr ):
            rightVals += self.right.findAllValues()
        else:
            rightVals.append( self.right )

        for val in rightVals:
            if ( not( val.isdecimal()) ) and not( isBool(val) ):
                if not( val in leftIds ) :
                    return False

        return valid 

    def validOps(self):
        results = []

        for opList in self.getOpList():
            for op in opList:
                sub_result = []
                
                if not( isPrimitive( op ) ):

                    for sig in signatures:
                        sub_result.append( sig.containsOperation(op))

                    if not(True in sub_result):
                        results.append( False )
                

        return not( False in results )



    def valid(self):
        return self.validOps() and self.validRhs()


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

class ReducedExpr:
    def __init__(self, expr, reduct):
        self.expr = expr
        self.reduct = reduct
        

#############################################
#############################################
####  Standalone Functions              #####
#############################################
#############################################

#retrieveValues - takes in a tree of nodes, goes through each branch,
#finds the values at the end of each branch, and maps it 
def retrieveSigValues(n, sigFlag, opsFlag, processed):
    if (isinstance(n, unicode)) :
        processed.put(dict({'afterops': n}))
    elif (isinstance(n.type, str) and n.type == 'empty') :
        pass
    elif (n.value is None) :
        if (n.type == 'signature') :
            sigFlag = True
        elif (n.type == 'operation') :
            opsFlag = True
        retrieveSigValues(n.children[0], sigFlag, opsFlag, processed)
        if (len(n.children) > 1) :
            sigFlag = False
            retrieveSigValues(n.children[1], sigFlag, opsFlag, processed)
    else:
        if (isinstance(n.type, str)) :
            if (n.type == 'identifier') :
                if (sigFlag) :
                    processed.put(dict({'signame' : n.value}))
                    sigFlag = False
                elif (opsFlag) :
                    processed.put(dict({'operation' : n.value}))
                    opsFlag = False
                else :
                    processed.put(dict({'afterops' : n.value}))

def retrieveEqNodes(n, equations):
    if (n.type == 'empty') :
        pass
    elif (n.type == 'equation') :
        equations.append(n)
    else :
        retrieveEqNodes(n.children[0], equations)
        if (len(n.children) > 1) :
            retrieveEqNodes(n.children[1], equations)

def getTypesFromExpr(expr, types):
    for i in range(len(expr.args)):
        if (not isinstance(expr.args[i].get('Value'), unicode)):
            getTypesFromExpr(expr.args[i].get('Value'), types)
        else:
            types[expr.args[i]['Value']] = expr.args[i].get('ArgType')

def findPrimitiveType(s):
    bool = ['#t', '#T', '#f', '#F']
    if (str(s) in bool) :
        return 'boolean'
    elif(s.isnumeric()) :
        return 'int'
    elif(s.isalpha() or str(s).isalnum()) :
        return 'string'
    else :
        return 'char'
            
def retrieveTermValues(term, expr, exprTypes = None):
    if(isinstance(term.type, str) and term.type == 'empty'):
        pass
    elif(term.value is None):
        if(term.type == 'term' or term.type == 'rhs'):
            if(isinstance(term.children[0], unicode)):
                expr.args.append(dict({'ArgType' : findPrimitiveType(term.children[0]), 'Value' : term.children[0]}))            
            elif (term.children[0].type == 'operation'):
                newarg = createOpArg(term.children[0])
                expr.args.append(newarg)
                retrieveTermValues(term.children[1], expr.args[len(expr.args) - 1].get('Value'), exprTypes)
            elif(term.children[0].type == 'identifier'):
                if (exprTypes is None):
                    argType = findArgType(expr.op, len(expr.args))
                else :
                    if (term.children[0].value in exprTypes):
                        argType = exprTypes[term.children[0].value]
                    else :
                        argType = findPrimitiveType(term.children[0].value)
                expr.args.append(dict({'ArgType' : argType, 'Value' : term.children[0].value}))
            elif(term.children[0].type == 'primitive'):
                newarg = createOpArg(term.children[0])
                expr.args.append(newarg)
                retrieveTermValues(term.children[1], expr.args[len(expr.args) - 1].get('Value'), exprTypes)            
        else:
            retrieveTermValues(term.children[0], expr, exprTypes)
            if(len(term.children) > 1):
                retrieveTermValues(term.children[1], expr)

             
def createOpArg(expr):
    if(expr.type == 'operation'):
        return createOpArg(expr.children[0])
    elif(expr.type == 'identifier'):
        return dict({'ArgType' : findOutputType(expr.value), 'Value' : Expr(expr.value)})
    elif(expr.type == 'primitive'):
        return dict({'ArgType' : findPrimOutputType(expr.value), 'Value' : Expr(expr.value)})

def findPrimOutputType(op):
    intTypes = ['+', '-', '*']
    if (op in intTypes):
        return 'int'
    else: return 'boolean'

def findOutputType(op):
    global signatures
    for sig in signatures:
        for opspec in sig.opspecs:
            if(opspec.operation == op):
                return opspec.output

def findArgType(op, argIndex):
    global signatures
    for sig in signatures:
        for opspec in sig.opspecs:
            if(opspec.operation == op):
                return opspec.args[argIndex]      

#uses the dictionaries we made in order to create the
#signature and operation structures 
def determineSignatures(processed):
    typename = ''
    operation = ''
    args = []
    foundFirstTypeName = False
    listOfSignatures = []
    foundFirstOperation = False
    listOfOperationSpecs = []
    while (not processed.empty()) :
        element = processed.get()
        if ('signame' in element) :
            if (not foundFirstTypeName) :
                foundFirstTypeName = True
                typename = element.get('signame')
            else :
                output = args.pop()
                opspec = OperationSpecStruct(operation, args, output)
                listOfOperationSpecs.append(opspec)
                sig = SignatureStruct(typename, listOfOperationSpecs)
                listOfSignatures.append(sig)
                typename = element.get('signame')
                listOfOperationSpecs = []
                args = []
                foundFirstOperation = False
        elif ('operation' in element) :
            if (not foundFirstOperation) :
                foundFirstOperation = True
                operation = element.get('operation')
            else:
                #last element that was put on the list must be the output type
                output = args.pop()
                opspec = OperationSpecStruct(operation, args, output)
                operation = element.get('operation')
                args = []
                listOfOperationSpecs.append(opspec)
        elif ('afterops' in element) :
            args.append(element.get('afterops')) 
    #queue is now empty, add last signature onto list
    output = args.pop()
    opspec = OperationSpecStruct(operation, args, output)
    listOfOperationSpecs.append(opspec)

    sig = SignatureStruct(typename, listOfOperationSpecs)
    listOfSignatures.append(sig)
    
    return listOfSignatures

#Retrieves signatures for an AST
def retrieveSignatures(tree):
    processed = Queue.Queue()
    sigFlag = False
    opsFlag = False
    retrieveSigValues(tree, sigFlag, opsFlag, processed)
    return determineSignatures(processed)

def retrieveEquations(tree):
    equations = []
    retrieveEqNodes(tree, equations)

    leftExpr = Expr("term")
    rightExpr = Expr("term")

    listOfEquationStructs = []
    for eq in equations:
        retrieveTermValues(eq.children[0], leftExpr)
        leftExprTypes = {}
        getTypesFromExpr(leftExpr, leftExprTypes)
        retrieveTermValues(eq.children[1], rightExpr, leftExprTypes)
        
        equa = EquationStruct( leftExpr.args[0].get('Value'), rightExpr.args[0].get('Value') )
        if equa.valid():
            # Append the Equation to list, but remove the top level "term" op
            listOfEquationStructs.append( equa )

        leftExpr = Expr("term")
        rightExpr = Expr("term")

    return listOfEquationStructs

def equalExprToTerm(expr, term):
    if (expr.op != term.op) :
        return False
    else :
        if (len(expr.args) == 0 and len(term.args) == 0):
            return True
        elif (len(expr.args) == len(term.args)):
            for i in range(len(expr.args)) :
                if (isinstance(expr.args[i].get('Value'), Expr) and isinstance(term.args[i].get('Value'), Expr)):
                    return equalExprToTerm(expr.args[i].get('Value'),term.args[i].get('Value'))
                else :
                    return (expr.args[i].get('ArgType') == term.args[i].get('ArgType'))
        else:
            return False

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
               
#largely assumes the method with no args and returns the typename is the base
def findBaseCases(sigs):
    baseCases = {}
    for signature in sigs:
        for spec in signature.opspecs:
            if len(spec.args) == 0 :
                baseCases[spec.output] = Expr(spec.operation)
    
    return baseCases
    #sys.exit('No base case found for ' + sigStruct.typename)

def randomTypeGen(type) :
    if type == 'int' :
        return str(random.randrange(0,99999999999))
    elif type =='boolean' :
        bool = ['#t', '#T', '#f', '#F']
        return random.choice(bool)
    elif type == 'string' :
        return ''.join(random.choice(string.ascii_letters + string.digits) for i in range(random.randrange(0,20)))
    elif type == 'char' :
        randomString = ''.join(random.choice(string.ascii_letters + string.digits) for i in range(random.randrange(0,10)))
        return repr(unicode(randomString, "utf-8" ))
    
def generateBaseExpressions(sig, baseCases):
    generatedExprs = defaultdict(list)
    for signature in sig:
    # turn each signature into a Scheme Expression
        for spec in signature.opspecs:
            expr = Expr(spec.operation)
            for arg in spec.args:
                if (arg in baseCases) :
                    expr.args.append(dict({'ArgType' : arg, 'Value' : baseCases[arg]}))
                else: 
                    expr.args.append(dict({'ArgType' : arg, 'Value' : str(randomTypeGen(arg))}))
                
                generatedExprs[spec.output].append(expr)
        # generate some random bases for primitives
                if spec.output in ["int","boolean","string","char"] :
                    for n in range(0, 3):
                        generatedExprs[spec.output].append(randomTypeGen(spec.output))
    return generatedExprs
    
def generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases):
    for signature in sigs:
        for spec in signature.opspecs:
            if (not spec.output in baseCases or (spec.output in baseCases and spec.operation != baseCases[spec.output].op)) :
                newExpr = Expr(spec.operation)
                for arg in spec.args :
                    newExpr.args.append(dict({'ArgType' : arg, 'Value' : random.choice(exprs[arg])}))
    
                reduction = rewriteExpr(newExpr)
                if(reduction):
                    #if(isinstance(reduction, str)):
                    reducedExprs.append(ReducedExpr(newExpr, reduction))
                exprs[spec.output].append(newExpr)
                
                def exprf(x): return not isinstance(x, str)
                filterExprs = filter(exprf, exprs[spec.output])
                if(len(filterExprs) > 3):
                    exprs[spec.output] = filterExprs
    



def getVariableMapping(expr, term, vmap):
    if(isinstance(term, unicode)):
        vmap[term] = expr
    elif(isinstance(expr, Expr) and isinstance(term, Expr)):
        for i in range(len(expr.args)):
            getVariableMapping(expr.args[i].get('Value'), term.args[i].get('Value'), vmap)


def getSubstitutedRewrite(term, vmap):
    if(isinstance(term, unicode)):
        if(term in vmap):
            return vmap[term]
        else:
            return term
    elif(isinstance(term, Expr)):
        newargs = []
        for arg in term.args:
            newargs.append(dict({'ArgType' : arg.get('ArgType'), 'Value' : getSubstitutedRewrite(arg.get('Value'), vmap)}))
        return Expr(term.op, newargs)

def getRewrite(expr, lterm, rterm):
    # get mapping of variables in rewrite rule to actual values
    vmap = {}
    getVariableMapping(expr, lterm, vmap)

    # parse through right term replacing vars with actual value
    return getSubstitutedRewrite(rterm, vmap)

# Try to rewrite given expr and all args that are exprs 
def rewriteExpr(expr):
    if(isinstance(expr, Expr)):
        r = rewriteSingleExpr(expr)
        if(r):
            if(isinstance(r, Expr)):
                for i in range(len(r.args)):
                    r.args[i]['Value'] = rewriteExpr(r. args[i]['Value'])
                return rewriteExpr(r)
            else:
                return r
        else:
            for i in range(len(expr.args)):
                expr.args[i]['Value'] = rewriteExpr(expr.args[i]['Value'])
            return expr
    else:
        return expr

# if expr looks like a rewrite rule, get the rewritten expr
def rewriteSingleExpr(expr):
    global equations
    
    for eq in equations:
        # check equality to term
        if equalExprToTerm(expr, eq.left):
            r = getRewrite(expr, eq.left, eq.right)
            return r



def printNumIterExpressions(num, sigs):
    reducedExprs = []
    # find all base exprs for signatures
    baseCases = findBaseCases(sigs)
    exprs = generateBaseExpressions(sigs, baseCases)

    num -= 1 
    while num > 0 :
        generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases)
        # adding non base cases to the output file 
        for rexpr in reducedExprs:
            iterExpr = rexpr.expr.toSexpr() + '\n'
            if(isinstance(rexpr.reduct, Expr)):
                iterExpr += rexpr.reduct.toSexpr() + '\n'
            else:
                iterExpr += str(rexpr.reduct) + '\n'
            output.write(unicode(iterExpr+'\n'))
        num -= 1

#############################################
#############################################
####  Validation                        #####
#############################################
#############################################

def containDups(collection):
    seen = set()

    for item in collection:
        if item in seen:
            return True
        seen.add(item)

    return False

def isPrimitive(string):
    return re.match( r'[not|\+|-|\*|=|<|>]', string, re.U )

def isBool(string):
    return re.match( r'[#t|#f]', string, re.U )


#############################################
#############################################
####  Main Excecution                   #####
#############################################
#############################################


# Checks to make sure an input file and output file are given
if len(sys.argv) < 3:
    sys.exit('Usage: %s input-file output-file' % sys.argv[0])

fileName = sys.argv[1]
outputName = sys.argv[2] 

#Opens input and output file
file = codecs.open(fileName, "r", "utf-8")
output = codecs.open(outputName, "w", "utf-8")

# concat input from file
inp = ''
for line in file:
    inp += line
 
#lexer.input(inp)
yacc.yacc(start='input')

yada = yacc.parse(inp)

signatures = retrieveSignatures(yada.children[0])
equations = retrieveEquations(yada.children[1]);

for eq in equations:
    print eq.toString()

#expr1 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : "empty"}), dict({'ArgType' : "int" ,'Value' : 2})])})])
expr2 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#expr3 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("pop", [dict({'ArgType' : 'StackInt', 'Value', Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : 2})]})]})]}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#print rewriteExpr(expr2)

#for sig in signatures :
    #base = findBaseCase(sig)
#printNumIterExpressions(20, signatures)

file.close()
output.close()

