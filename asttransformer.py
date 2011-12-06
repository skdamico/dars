import primitive
import Queue
import classes as c
import global_var as g

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

#determineSignatures - uses the dictionaries we made in order to create the
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
                opspec = c.OperationSpecStruct(operation, args, output)
                listOfOperationSpecs.append(opspec)
                sig = c.SignatureStruct(typename, listOfOperationSpecs)
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
                opspec = c.OperationSpecStruct(operation, args, output)
                operation = element.get('operation')
                args = []
                listOfOperationSpecs.append(opspec)
        elif ('afterops' in element) :
            args.append(element.get('afterops')) 
    #queue is now empty, add last signature onto list
    output = args.pop()
    opspec = c.OperationSpecStruct(operation, args, output)
    listOfOperationSpecs.append(opspec)

    sig = c.SignatureStruct(typename, listOfOperationSpecs)
    listOfSignatures.append(sig)
    
    return listOfSignatures

#retrieveSignatures - Retrieves signatures for an AST
def retrieveSignatures(tree):
    processed = Queue.Queue()
    sigFlag = False
    opsFlag = False
    retrieveSigValues(tree, sigFlag, opsFlag, processed)
    return determineSignatures(processed)

#retrieveEquations - Retrieves equations for an AST
def retrieveEquations(tree):
    #equations = []
    retrieveEqNodes(tree, g.equations)

    leftExpr = c.Expr("term")
    rightExpr = c.Expr("term")

    listOfEquationStructs = []
    for eq in g.equations:
        retrieveTermValues(eq.children[0], leftExpr)
        leftExprTypes = {}
        getTypesFromExpr(leftExpr, leftExprTypes)
        retrieveTermValues(eq.children[1], rightExpr, leftExprTypes)
        
        # Append the Equation to list, but remove the top level "term" op
        g.equation = c.EquationStruct( leftExpr.args[0].get('Value'), rightExpr.args[0].get('Value') )

        listOfEquationStructs.append( g.equation )

        leftExpr = c.Expr("term")
        rightExpr = c.Expr("term")

    return listOfEquationStructs

