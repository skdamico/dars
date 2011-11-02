import darslex
import yacc
import Queue
import codecs, sys
import random
import string

tokens = darslex.tokens

#Rewrites System.out to unicode 
sys.stdout = codecs.getwriter('utf-8')(sys.stdout)
#Global variable to be used in multiple functions
o = ''

#Define grammar rules for the parser
def p_input(p):
    'input : SIGNATURE COLON signatures EQUATIONS COLON equations'
    p[0] = Node('input', [p[3], p[6]])

def p_signatures(p): 
    '''signatures : signature signatures2'''
    p[0] = Node('signatures', [p[1], p[2]])

def p_signatures2(p):
    '''signatures2 : signatures
                   | empty'''
    p[0] = p[1]

def p_signature(p): 
    'signature : ADT COLON typename operation_specs'
    p[0] = Node('signature', [p[3], p[4]])

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
    '''identifier : ID
                  | BOOLEAN'''
    p[0] = Node('identifier', [], p[1])

##Scheme grammmars which were commented out for Assignment 4
#def p_peculiar_identifier(p):
#    '''peculiar_identifier : subsequent_star 
#                           | ellipsis'''
#    p[0] = ('peculiar-identifier', p[1])
#
#def p_subsequent_star(p):
#    '''subsequent_star : subsequent subsequent_star2'''
#    p[0] = ('subsequent-star', (p[1], p[2]))
#
#def p_subsequent_star2(p):
#    '''subsequent_star2 : subsequent_star
#                        | empty'''
#    p[0] = ('subsequent-star2', p[1])
#
#def p_subsequent(p):
#    '''subsequent : initial
#                  | special_subsequent
#                  | unicode_subsequent
#                  | DIGIT'''
#    p[0] = ('subsequent', p[1])
#
#def p_unicode_subsequent(p):
#    '''unicode_subsequent : UNICODE_ND
#                          | UNICODE_MC
#                          | UNICODE_ME'''
#    p[0] = p[1]
#
#def p_initial(p):
#    '''initial : constituent 
#               | special_initial 
#               | inline_hex_escape'''
#    p[0] = ('initial', p[1])
#
#def p_ellipsis(p):
#    'ellipsis : PERIOD PERIOD PERIOD'
#    p[0] = ('ellipsis', '...')
#
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
#

def p_equations(p): 
    '''equations : equation equations 
                 | empty'''
    if (len(p) == 2) :
        p[0] = p[1]
    else :
        p[0] = Node('equations', [p[1], p[2]])

def p_equation(p):
    '''equation : term EQUAL term'''
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
    else:
        p[0] = Node('args', [p[1], p[2]])

def p_empty(p):
    'empty :'
    p[0] = Node('empty', [])

def p_error(p):
    print("Syntax error at '%s'" % p.value)

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

class OperationSpecStruct:
    def __init__(self,operation,args,output):
        self.operation = operation
        self.args = args
        self.output = output 

class EquationStruct:
    def __init__(self,left,right):
        self.left = left
        self.right = right

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

def retrieveTermValues(term, opsFlag, termMap):
    if (isinstance(term.type, str) and term.type == 'empty') :
        pass
    elif (term.value is None) :
        if (term.type == 'operation') :
            opsFlag = True
        retrieveTermValues(term.children[0], opsFlag, termMap)
        if(len(term.children) > 1) :
            retrieveTermValues(term.children[1], False, termMap)
    else :
        if (isinstance(term.type, str)):
            if(term.type == 'identifier'):
                if(opsFlag):
                    termMap.append(dict({'operation': term.value}))
                else:
                    termMap.append(dict({'args': term.value}))


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
                sig = SignatureStruct(typename, listOfOperationSpecs)
                listOfSignatures.append(sig)
                typename = element.get('signame')
                listOfOperationSpecs = []
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

    leftMap = []
    rightMap = []

    listOfEquationStructs = []
    for eq in equations:
        opsFlag = False
        retrieveTermValues(eq.children[0], opsFlag, leftMap)
        opsFlag = False
        retrieveTermValues(eq.children[1], opsFlag, rightMap)
        listOfEquationStructs.append(EquationStruct(leftMap, rightMap))
        leftMap = []
        rightMap = []
    return listOfEquationStructs

#largely assumes the method with no args and returns the typename is the base
def findBaseCase(sigStruct):
    opSpecs = sigStruct.opspecs  
    for spec in opSpecs:
        if len(spec.args) == 0 :
            if (spec.output == sigStruct.typename) :
                return spec.operation
    sys.exit('No base case found for ' + sigStruct.typename)

def mapExprToTypes(exprs):
    from collections import defaultdict
    remappedExprs = defaultdict(list)
    for k, v in exprs.iteritems():
        remappedExprs[v].append(k)
    return remappedExprs

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
    
def generateBaseExpressions(sigstruct, type):
    global o
    generatedExprs = dict()
        # turn each signature into a Scheme Expression
    for spec in sigstruct.opspecs:
        expr =  "(" + spec.operation + ' '
        for arg in spec.args:
            if (arg == sigstruct.typename) :
                expr += type + ' '
            else: 
                expr += str(randomTypeGen(arg)) + ' '
        expr = string.strip(expr)
        expr += ')'
        generatedExprs[expr] = spec.output
        # generate some random bases for primitives
        if spec.output in ["int","boolean","string","char"] :
            for n in range(0, 3):
                generatedExprs[randomTypeGen(spec.output)] = spec.output
        o += expr + '\n'
    
    return mapExprToTypes(generatedExprs)
    
def generateNonBaseExpressions(exprs, sigstruct):
    global o
    generatedExprs = dict()
    specs = sigstruct.opspecs
    base = findBaseCase(sigstruct).split('(', 2)[0]
    for spec in specs:
        if (spec.operation != base) :
            newExpr = "(" + spec.operation + ' '
            for arg in spec.args :
                newExpr += random.choice(exprs[arg]) + ' '
            newExpr = string.strip(newExpr)
            newExpr += ') '
            generatedExprs[newExpr] = spec.output
            o += newExpr + '\n'
    
    return mapExprToTypes(generatedExprs)

def printNumIterExpressions(num, sigstruct):
    global o
    o = ''
    iter = generateBaseExpressions(sigstruct, findBaseCase(sig))
    num -= 1
    while num > 0 :
        iter = generateNonBaseExpressions(iter, sig)
        num -= 1
    return o

def mapAlgebraicSpecs(tree):
    pass
    

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

# test lexer
#while True:
#    tok = lexer.token()
#    if not tok: break      # No more input
#    print tok

yada = yacc.parse(inp)
signatures = retrieveSignatures(yada.children[0])
equations = retrieveEquations(yada.children[1])

for sig in signatures :
    expressions = printNumIterExpressions(10, sig)
    output.write(unicode(expressions))

for eq in equations :
    for l in eq.left :
        print l
    for r in eq.right :
        print r

file.close()
output.close()

