import global_var as g
import classes as c
import asttransformer as a
import darslex
import yacc
import blackboxer
import primitive
import ExprGen
import codecs, sys

#import re
#import random
#import Queue
#from collections import defaultdict

tokens = darslex.tokens

#Rewrites System.out to unicode 
sys.stdout = codecs.getwriter('utf-8')(sys.stdout)

#these globals are now in global_var.py
#globals
#signatures = []
#equations = []
#baseTypes = ["int", "boolean", "character", "string"]
#primitiveOps = [ "not", "+", "-", "*", "=", "<", ">" ]

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
    p[0] = c.Node('input', [p[2], p[4]])

def p_signatures(p): 
    '''signatures : signature signatures2'''
    p[0] = c.Node('signatures', [p[1], p[2]])

def p_signatures2(p):
    '''signatures2 : signatures
                   | empty'''
    p[0] = p[1]

def p_signature(p): 
    'signature : ADT typename operation_specs'
    p[0] = c.Node('signature', [p[2], p[3]])

def p_operation_specs(p):
    '''operation_specs : operation_spec operation_specs2'''
    p[0] = c.Node('operation-specs', [p[1], p[2]])

def p_operation_specs2(p):
    '''operation_specs2 : operation_specs
                        | empty'''
    p[0] = p[1]

def p_operation_spec(p):
    '''operation_spec : operation COLON operation_spec2'''
    p[0] = c.Node('operation-spec', [p[1], p[3]])

def p_operation_spec2(p):
    '''operation_spec2 : ARROW type 
                       | arg_types ARROW type'''
    if len(p) == 3 :
        p[0] = p[2]
    else :
        p[0] = c.Node('operation-spec-args', [p[1], p[3]])

def p_operation(p):
    'operation : identifier'
    p[0] = c.Node('operation', [p[1]])

def p_arg_types(p):
    '''arg_types : type arg_types2'''
    p[0] = c.Node('arg-types', [p[1], p[2]])

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
        p[0] = c.Node('type', [], p[1])
    else:
        p[0] = c.Node('type', [p[1]])

def p_typename(p):
    'typename : identifier'
    p[0] = c.Node('typename', [p[1]])


def p_identifier(p):
    '''identifier : ID'''
    p[0] = c.Node('identifier', [], p[1])

##Scheme grammars are not working!!!
#def p_identifier(p):
#    '''identifier : initial subsequent_star
#                  | peculiar_identifier'''
#   if len(p) == 2:         
#       p[0] = c.Node('identifier', [p[1]])
#   else
#       p[0] = c.Node('identifier, [p[1], p[2]])

##Scheme grammmars which were commented out for Assignment 4
#def p_peculiar_identifier(p):
#    '''peculiar_identifier : ARROW subsequent_star 
#                           | ELLIPSIS
#                           | PLUS
#                           | MINUS'''
#   if len(p) == 2: 
#       p[0] = c.Node('peculiar-identifier', [], p[1])
#   else:
#       p[0] = c.Node('peculiar-identifier', [p[2]])
#
#def p_subsequent_star(p):
#    '''subsequent_star : subsequent subsequent_star2'''
#    p[0] = c.Node('subsequent-star', [p[1], p[2]])
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
#       p[0] = c.Node('inline-hex-escape', [r"\x", p[3], ";"])
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
        p[0] = c.Node('equations', [p[1], p[2]])

def p_equation(p):
    '''equation : term EQUAL rhs'''
    p[0] = c.Node('equation', [p[1], p[3]])
    
def p_term(p):
    '''term : identifier
            | LEFT_PARENS operation args RIGHT_PARENS'''
    if (len(p) == 2) :
        p[0] = c.Node('term', [p[1]])
    else :
        p[0] = c.Node('term', [p[2], p[3]]) 

def p_args(p):
    '''args : term args
            | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = c.Node('args', [p[1], p[2]])

def p_rhs(p): 
    '''rhs : BOOLEAN
           | uinteger_10
           | identifier 
           | LEFT_PARENS primitive rhs_args RIGHT_PARENS
           | LEFT_PARENS operation rhs_args RIGHT_PARENS'''
    if len(p) == 2: 
        p[0] = c.Node('rhs', [p[1]])
    else :
        p[0] = c.Node('rhs', [p[2], p[3]])

## rhs_args is same exact thing as args... should just reuse?
def p_rhs_args(p): 
    '''rhs_args : rhs rhs_args 
                | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = c.Node('rhs_args', [p[1], p[2]])

def p_primitive(p):
    '''primitive : NOT 
                 | SPECIAL_PRIMITIVE
                 | STAR
                 | PLUS
                 | MINUS'''
    p[0] = c.Node('primitive', [], p[1])

def p_uinteger_10(p): 
    '''uinteger_10 : digit_plus'''
    p[0] = c.Node('uinteger_10', [p[1]])

def p_digit_plus(p):
    '''digit_plus : DIGIT digit_plus2'''
    p[0] = p[2]

def p_digit_plus2(p):
    '''digit_plus2 : digit_plus
                   | empty'''
    p[0] = p[1]   

def p_empty(p):
    'empty :'
    p[0] = c.Node('empty', [])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

#All classes in classes.py
#############################################
#############################################
####  Support Classes                   #####
#############################################
#############################################
#Simple implementation of an AST node
#class Node:
#    def __init__(self,type,children=None,value=None):
#        self.type = type
#        if children :
#            self.children = children
#        else :
#            self.children = []
#        self.value = value 
        
#class SignatureStruct:
#    def __init__(self,typename,opspecs):
#        self.typename = typename
#        self.opspecs = opspecs
# 
#    # checks to see if all the types contained within this signature
#    # are defined as adts in the given environment provided by the
#    def isValid(self, env):
#        valid = True
#
#        for opspec in self.opspecs:
#            if not opspec.isValid( env ):
#                valid = False
#                break
#
#        return valid
#        
#
#    def getAllOpNames(self):
#        opNames = []
#
#        for opspec in self.opspecs:
#            opNames.append( opspec.operation )
#
#        return opNames
#
#    def containsOperation(self, opName):
#        if (opName in self.getAllOpNames()):
#            return True
#
#        return False
#
#    def toString(self):
#        text = "ADT: " + self.typename;
#
#        for op in self.opspecs:
#            text += "\n   " + op.toString()
#
#        return text

    
#class OperationSpecStruct:
#    def __init__(self,operation,args,output):
#        self.operation = operation
#        self.args = args
#        self.output = output 
#
#    # checks to see if all the types contained within
#    def isValid(self, adts):
#        valid = True
#
#        for arg in self.args:
#            if not( arg in baseTypes or arg in adts ):
#                valid = False
#                break
#        
#        if not( self.output in baseTypes or self.output in adts ):
#            valid = False
#
#        return valid
#            
#
#    def toString(self):
#        string = self.operation + ": "
#        args = []
#        args += self.args
#
#        while ( len(args) > 0 ):
#            arg = args.pop(0)
#            string += arg + " "
#
#            if not( len(args) <= 0 ):
#                string += " * "
#
#        string += "-> " + self.output
#
#        return string
#
#class EquationStruct:
#    def __init__( self, left, right ):
#        self.left = left
#        self.right = right
#
#    def getOpList( self ):
#        ops = []
#
#        if isinstance( self.left, Expr ):
#            ops.append( self.left.findAllOps() )
#
#        if isinstance( self.right, Expr ):
#            ops.append( self.right.findAllOps() )
#
#        return ops 
#
#    def toString( self ):
#        text = ""
#
#        if isinstance( self.left, Expr ):
#            text += self.left.toSexpr()
#        else:
#            text += self.left
#
#        text += " = "
#    
#        if isinstance( self.right, Expr ):
#            text += self.right.toSexpr()
#        else:
#            text += self.right
#
#        return text
#
#    def validEqIds( self ):
#        valid = True
#        leftIds     = []
#        rightVals    = []
#
#        # locates all identifiers on the left side of an equation and adds 
#        # them to an array. If an identifier appears more than once 
#        # immediately returns and states the equation is invalid
#        if isinstance( self.left, Expr ):
#            if containDups( self.left.findAllValues() ):
#                return False
#
#            leftIds += self.left.findAllValues()
#        else:
#            leftIds.append( self.left )
#
#        # finds all the identifiers and all the allowed values for the 
#        # right hand side of an equation
#        if isinstance( self.right, Expr ):
#            rightVals += self.right.findAllValues()
#        else:
#            rightVals.append( self.right )
#
#        for val in rightVals:
#            if ( not( val.isdecimal()) ) and not( isBool(val) ):
#                if not( val in leftIds ) :
#                    return False
#
#        return valid 
#
#    def validOps( self, sigs ):
#        results = []
#
#        for opList in self.getOpList():
#            for op in opList:
#                sub_result = []
#                
#                if not( op in primitiveOps ):
#                    for sig in sigs:
#                        sub_result.append( sig.containsOperation(op) )
#
#                    if not(True in sub_result):
#                        results.append( False )
#                else:
#                    print "s " + op
#                
#
#        return not( False in results )
#
#
#    def isValid( self, sigs ):
#        return self.validOps( sigs ) and self.validEqIds()
#
#
#class Expr:
#    def __init__(self,op,args=None):
#        self.op = op
#        if args:
#            self.args = args
#        else :
#            self.args = []
#    
#    # class method that turns Expr objects 
#    # into Scheme expressions :-) 
#    def toSexpr(self):
#        sexpr = "(" +  str(self.op)
#
#        for arg in self.args:
#            if (isinstance(arg['Value'], Expr)): 
#                sexpr += " " + arg['Value'].toSexpr()
#            else: 
#                sexpr += " " + str(arg['Value']) 
#
#        sexpr += ")"     
#        return sexpr
#
#    # returns all operations used in the Expr
#    def findAllOps(self):
#        ops = [self.op]
#
#        for arg in self.args:
#            if ( isinstance( arg['Value'], Expr )):
#                ops += arg['Value'].findAllOps()
#
#        return ops
#
#    # returns all values within the Expr
#    def findAllValues(self):
#        vals = []
#
#        for arg in self.args:
#            if( isinstance( arg['Value'], Expr ) ):
#                vals += arg['Value'].findAllValues()
#            else:
#                vals.append( arg['Value'] )
#
#        return vals
#
#class ReducedExpr:
#    def __init__(self, expr, reduct, adtName):
#        self.expr = expr
#        self.reduct = reduct
#        self.adtName = adtName
#        

#asttransformation only works for signatures, not equations
#############################################
#############################################
####  Standalone Functions              #####
#############################################
#############################################
#
##retrieveValues - takes in a tree of nodes, goes through each branch,
##finds the values at the end of each branch, and maps it 
#def retrieveSigValues(n, sigFlag, opsFlag, processed):
#    if (isinstance(n, unicode)) :
#        processed.put(dict({'afterops': n}))
#    elif (isinstance(n.type, str) and n.type == 'empty') :
#        pass
#    elif (n.value is None) :
#        if (n.type == 'signature') :
#            sigFlag = True
#        elif (n.type == 'operation') :
#            opsFlag = True
#        retrieveSigValues(n.children[0], sigFlag, opsFlag, processed)
#        if (len(n.children) > 1) :
#            sigFlag = False
#            retrieveSigValues(n.children[1], sigFlag, opsFlag, processed)
#    else:
#        if (isinstance(n.type, str)) :
#            if (n.type == 'identifier') :
#                if (sigFlag) :
#                    processed.put(dict({'signame' : n.value}))
#                    sigFlag = False
#                elif (opsFlag) :
#                    processed.put(dict({'operation' : n.value}))
#                    opsFlag = False
#                else :
#                    processed.put(dict({'afterops' : n.value}))
#
#retrieveEqNodes - takes in a tree of equations, goes through all the branches,
#and for each equation, appends it to the list of equations
def retrieveEqNodes(n, equations):
    if (n.type == 'empty') :
        pass
    elif (n.type == 'equation') :
        equations.append(n)
    else :
        retrieveEqNodes(n.children[0], equations)
        if (len(n.children) > 1) :
            retrieveEqNodes(n.children[1], equations)

#getTypesFromExpr - gets all the argument types from an expression
#and puts it into a dictionary
def getTypesFromExpr(expr, types):
    for i in range(len(expr.args)):
        if (not isinstance(expr.args[i].get('Value'), unicode)):
            getTypesFromExpr(expr.args[i].get('Value'), types)
        else:
            types[expr.args[i]['Value']] = expr.args[i].get('ArgType')


#retrieveTermValues - retrieves the terms for given equation and maps it to an argument type          
def retrieveTermValues(term, expr, exprTypes = None):
    if(isinstance(term.type, str) and term.type == 'empty'):
        pass
    elif(term.value is None):
        if(term.type == 'term' or term.type == 'rhs'):
            if(isinstance(term.children[0], unicode)):
                expr.args.append(dict({'ArgType' : primitive.findUniPrimitiveType(term.children[0]), 'Value' : term.children[0]}))            
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
                        argType = primitive.findUniPrimitiveType(term.children[0].value)
                expr.args.append(dict({'ArgType' : argType, 'Value' : term.children[0].value}))
            elif(term.children[0].type == 'primitive'):
                newarg = createOpArg(term.children[0])
                expr.args.append(newarg)
                retrieveTermValues(term.children[1], expr.args[len(expr.args) - 1].get('Value'), exprTypes)            
        else:
            retrieveTermValues(term.children[0], expr, exprTypes)
            if(len(term.children) > 1):
                retrieveTermValues(term.children[1], expr)

#createOpArg - returns the dictionary mapping of an argument and its type              
def createOpArg(expr):
    if(expr.type == 'operation'):
        return createOpArg(expr.children[0])
    elif(expr.type == 'identifier'):
        return dict({'ArgType' : findOutputType(expr.value), 'Value' : c.Expr(expr.value)})
    elif(expr.type == 'primitive'):
        return dict({'ArgType' : primitive.findPrimOutputType(expr.value), 'Value' : c.Expr(expr.value)})

#findOutputType - returns the operation's output type
def findOutputType(op):
    #global signatures
    for sig in g.signatures:
        for opspec in sig.opspecs:
            if(opspec.operation == op):
                return opspec.output

#findArgType - returns the type of an operation's argument
def findArgType(op, argIndex):
    #global signatures
    for sig in g.signatures:
        for opspec in sig.opspecs:
            if(opspec.operation == op):
                return opspec.args[argIndex]      

##determineSignatures - uses the dictionaries we made in order to create the
##signature and operation structures 
#def determineSignatures(processed):
#    typename = ''
#    operation = ''
#    args = []
#    foundFirstTypeName = False
#    listOfSignatures = []
#    foundFirstOperation = False
#    listOfOperationSpecs = []
#    while (not processed.empty()) :
#        element = processed.get()
#        if ('signame' in element) :
#            if (not foundFirstTypeName) :
#                foundFirstTypeName = True
#                typename = element.get('signame')
#            else :
#                output = args.pop()
#                opspec = OperationSpecStruct(operation, args, output)
#                listOfOperationSpecs.append(opspec)
#                sig = SignatureStruct(typename, listOfOperationSpecs)
#                listOfSignatures.append(sig)
#                typename = element.get('signame')
#                listOfOperationSpecs = []
#                args = []
#                foundFirstOperation = False
#        elif ('operation' in element) :
#            if (not foundFirstOperation) :
#                foundFirstOperation = True
#                operation = element.get('operation')
#            else:
#                #last element that was put on the list must be the output type
#                output = args.pop()
#                opspec = OperationSpecStruct(operation, args, output)
#                operation = element.get('operation')
#                args = []
#                listOfOperationSpecs.append(opspec)
#        elif ('afterops' in element) :
#            args.append(element.get('afterops')) 
#    #queue is now empty, add last signature onto list
#    output = args.pop()
#    opspec = OperationSpecStruct(operation, args, output)
#    listOfOperationSpecs.append(opspec)
#
#    sig = SignatureStruct(typename, listOfOperationSpecs)
#    listOfSignatures.append(sig)
#    
#    return listOfSignatures
#
##retrieveSignatures - Retrieves signatures for an AST
#def retrieveSignatures(tree):
#    processed = Queue.Queue()
#    sigFlag = False
#    opsFlag = False
#    retrieveSigValues(tree, sigFlag, opsFlag, processed)
#    return determineSignatures(processed)


##retrieveEquations - Retrieves equations for an AST
def retrieveEquations(tree):
    equations = []
    retrieveEqNodes(tree, equations)

    leftExpr = c.Expr("term")
    rightExpr = c.Expr("term")

    listOfEquationStructs = []
    for eq in equations:
        retrieveTermValues(eq.children[0], leftExpr)
        leftExprTypes = {}
        getTypesFromExpr(leftExpr, leftExprTypes)
        retrieveTermValues(eq.children[1], rightExpr, leftExprTypes)
        
        # Append the Equation to list, but remove the top level "term" op
        equation = c.EquationStruct( leftExpr.args[0].get('Value'), rightExpr.args[0].get('Value') )

        listOfEquationStructs.append( equation )

        leftExpr = c.Expr("term")
        rightExpr = c.Expr("term")

    return listOfEquationStructs

# all rewriting/reducing and expression generation in ExprGen.py
##equalExprToTerm - Checks to see if an expression is equal to a term
#def equalExprToTerm(expr, term):
#    if (expr.op != term.op) :
#        return False
#    else :
#        if (len(expr.args) == 0 and len(term.args) == 0):
#            return True
#        elif (len(expr.args) == len(term.args)):
#            for i in range(len(expr.args)) :
#                if (isinstance(expr.args[i].get('Value'), c.Expr) and isinstance(term.args[i].get('Value'), c.Expr)):
#                    return equalExprToTerm(expr.args[i].get('Value'),term.args[i].get('Value'))
#                else :
#                    return (expr.args[i].get('ArgType') == term.args[i].get('ArgType'))
#        else:
#            return False
#
##equalExpr - Checks to see if two expressions are equal to one another
#def equalExpr(expr1, expr2):
#    if (expr1.op != expr2.op) :
#        return False
#    else :
#        if (len(expr1.args) == 0 and len(expr2.args) == 0):
#            return True
#        elif (len(expr1.args) == len(expr2.args)):
#            for i in range(len(expr1.args)) :
#                if (isinstance(expr1.args[i].get('Value'), c.Expr) and isinstance(expr2.args[i].get('Value'), c.Expr)):
#                    return equalExpr(expr1.args[i].get('Value'),expr2.args[i].get('Value'))
#                else :
#                    return (expr1.args[i].get('Value') == expr2.args[i].get('Value'))
#        else:
#            return False
#               
##findBaseCase - finds base case of an ADT signature
#def findBaseCases(sigs):
#    baseCases = {}
#    for signature in sigs:
#        for spec in signature.opspecs:
#            if len(spec.args) == 0 :
#                baseCases[spec.output] = c.Expr(spec.operation)
#    return baseCases
#    #sys.exit('No base case found for ' + sigStruct.typename)
#
##genrateBaseExpressions - generates the base expressions    
#def generateBaseExpressions(sig, baseCases):
#    generatedExprs = defaultdict(list)
#    for signature in sig:
#    # turn each signature into a Scheme Expression
#        for spec in signature.opspecs:
#            expr = c.Expr(spec.operation)
#            for arg in spec.args:
#                if (arg in baseCases) :
#                    expr.args.append(dict({'ArgType' : arg, 'Value' : baseCases[arg]}))
#                else: 
#                    expr.args.append(dict({'ArgType' : arg, 'Value' : str(primitive.randomTypeGen(arg))}))
#                
#                generatedExprs[spec.output].append(expr)
#        # generate some random bases for primitives
#                if spec.output in baseTypes :
#                    for n in range(0, 3):
#                        generatedExprs[spec.output].append(primitive.randomTypeGen(spec.output))
#    return generatedExprs
#
#
##generateNonBaseExpressions - generates non-base expressions    
#def generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases):
#    for signature in sigs:
#        for spec in signature.opspecs:
#            if (not spec.output in baseCases or (spec.output in baseCases and spec.operation != baseCases[spec.output].op)) :
#                newExpr = c.Expr(spec.operation)
#                for arg in spec.args :
#                    newExpr.args.append(dict({'ArgType' : arg, 'Value' : random.choice(exprs[arg])}))
#                reduction = rewriteExpr(newExpr, 0)
#                if(reduction):
#                    if(isinstance(reduction, str)):
#                        reducedExprs.append(c.ReducedExpr(newExpr, reduction, signature.typename))
#                exprs[spec.output].append(newExpr)
#    
#def getVariableMapping(expr, term, vmap):
#    if(isinstance(term, unicode)):
#        vmap[term] = expr
#    elif(isinstance(expr, c.Expr) and isinstance(term, c.Expr)):
#        for i in range(len(expr.args)):
#            getVariableMapping(expr.args[i].get('Value'), term.args[i].get('Value'), vmap)
#
#def getSubstitutedRewrite(term, vmap):
#    if(isinstance(term, unicode)):
#        if(term in vmap):
#            return vmap[term]
#        else:
#            return term
#    elif(isinstance(term, c.Expr)):
#        newargs = []
#        for arg in term.args:
#            newargs.append(dict({'ArgType' : arg.get('ArgType'), 'Value' : getSubstitutedRewrite(arg.get('Value'), vmap)}))
#        return c.Expr(term.op, newargs)
#
#def getRewrite(expr, lterm, rterm):
#    # get mapping of variables in rewrite rule to actual values
#    vmap = {}
#    getVariableMapping(expr, lterm, vmap)
#
#    # parse through right term replacing vars with actual value
#    return getSubstitutedRewrite(rterm, vmap)
#
## Try to rewrite given expr and all args that are exprs 
#def rewriteExpr(expr, count):
#    if(count > 100):
#        return expr
#    if(isinstance(expr, c.Expr)):
#        for i in range(len(expr.args)):
#            expr.args[i]['Value'] = rewriteExpr(expr.args[i]['Value'], count)
#        r = rewriteSingleExpr(expr)
#        if(r):
#            if(isinstance(r, c.Expr)):
#                count += 1
#                return rewriteExpr(r, count)
#            else:
#                return r
#        else:
#            return expr
#    else:
#        return expr
#
## if expr looks like a rewrite rule, get the rewritten expr
#def rewriteSingleExpr(expr):
#    #global equations
#    for eq in g.equations:
#        # check equality to term
#        if equalExprToTerm(expr, eq.left):
#            r = getRewrite(expr, eq.left, eq.right)
#            return r
#
#def genNumIterExpressions(num, sigs):
#    reducedExprs = []
#    # find all base exprs for signatures
#    baseCases = findBaseCases(sigs)
#    exprs = generateBaseExpressions(sigs, baseCases)
#    num -= 1 
#    while num > 0 :
#        generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases)
#        num -= 1
#    return reducedExprs
    

#############################################
#############################################
####  Validation                        #####
#############################################
#############################################

#helper functions moved to classes.py
#def containDups(collection):
#    seen = set()
#
#    for item in collection:
#        if item in seen:
#            return True
#        seen.add(item)
#
#    return False

#def isBool(string):
#    return re.match( r'[#t|#f]', string, re.U )

def validateSignatures( signatures ):
    valid = True
    adts = getADTNames( signatures )

    for sig in signatures:
        if not( sig.isValid( adts ) ):
            print adts
            valid = False
            print "Invalid Signature found: \n" + sig.toString()
            print "Terminating program"
            break

    return valid

def validateEquations( equations, signatures ):
    valid = True

    for eq in equations:
        if not( eq.isValid( signatures ) ):
            valid = False
            print "Invalid Equation found: " + eq.toString()
            print "Terminating program"
            break

    return valid


def getADTNames( signatures ):
    adts = []

    for sig in signatures:
        adts.append( sig.typename )

    return adts

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
 
yacc.yacc( start='input' )

yada = yacc.parse( inp )

g.signatures = a.retrieveSignatures( yada.children[0] )

# validates signatues are correct before doing anything else
# if any formating is incorrect prints a message and then
# exits the program
if validateSignatures( g.signatures ):
    g.equations = retrieveEquations( yada.children[1] )

if( validateEquations( g.equations, g.signatures ) ):

##### Tests #####
#expr1 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : "empty"}), dict({'ArgType' : "int" ,'Value' : 2})])})])
#expr2 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#expr3 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("pop", [dict({'ArgType' : 'StackInt', 'Value', Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : 2})]})]})]}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#print rewriteExpr(expr2, 0)
#print rewriteExpr(expr3)
##### Tests #####
#expr1 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : "empty"}), dict({'ArgType' : "int" ,'Value' : 2})])})])
#expr2 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#expr3 = Expr('top', [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("pop", [dict({'ArgType' : 'StackInt', 'Value', Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}),dict({'ArgType' : 'int', 'Value' : 2})]})]})]}),dict({'ArgType' : 'int', 'Value' : Expr("top", [dict({'ArgType' : 'StackInt', 'Value' : Expr("push", [dict({'ArgType' : 'StackInt', 'Value' : Expr("empty", [])}), dict({'ArgType' : 'int', 'Value' : 2})])})])})])})])
#print rewriteExpr(expr2, 0)
#print rewriteExpr(expr3)

#for sig in signatures :
    #base = findBaseCase(sig)
    rExprs = ExprGen.genNumIterExpressions(400, g.signatures)
    blackboxer.writeBlackBoxer(g.signatures, output, fileName, rExprs)

file.close()
output.close()
