import classes as c
import global_var as g
import primitive 
import random
from collections import defaultdict

#equalExprToTerm - Checks to see if an expression is equal to a term
def equalExprToTerm(expr, term):
    if (expr.op != term.op) :
        return False
    else :
        if (len(expr.args) == 0 and len(term.args) == 0):
            return True
        elif (len(expr.args) == len(term.args)):
            for i in range(len(expr.args)) :
                if (isinstance(expr.args[i].get('Value'), c.Expr) and isinstance(term.args[i].get('Value'), c.Expr)):
                    return equalExprToTerm(expr.args[i].get('Value'),term.args[i].get('Value'))
                else :
                    return (expr.args[i].get('ArgType') == term.args[i].get('ArgType'))
        else:
            return False

#equalExpr - Checks to see if two expressions are equal to one another
def equalExpr(expr1, expr2):
    if (expr1.op != expr2.op) :
        return False
    else :
        if (len(expr1.args) == 0 and len(expr2.args) == 0):
            return True
        elif (len(expr1.args) == len(expr2.args)):
            for i in range(len(expr1.args)) :
                if (isinstance(expr1.args[i].get('Value'), c.Expr) and isinstance(expr2.args[i].get('Value'), c.Expr)):
                    return equalExpr(expr1.args[i].get('Value'),expr2.args[i].get('Value'))
                else :
                    return (expr1.args[i].get('Value') == expr2.args[i].get('Value'))
        else:
            return False
               
#findBaseCase - finds base case of an ADT signature
def findBaseCases(sigs):
    baseCases = {}
    for signature in sigs:
        for spec in signature.opspecs:
            if len(spec.args) == 0 :
                baseCases[spec.output] = c.Expr(spec.operation)
    return baseCases
    #sys.exit('No base case found for ' + sigStruct.typename)

#genrateBaseExpressions - generates the base expressions    
def generateBaseExpressions(sig, baseCases):
    generatedExprs = defaultdict(list)
    for signature in sig:
    # turn each signature into a Scheme Expression
        for spec in signature.opspecs:
            expr = c.Expr(spec.operation)
            for arg in spec.args:
                if (arg in baseCases) :
                    expr.args.append(dict({'ArgType' : arg, 'Value' : baseCases[arg]}))
                else: 
                    expr.args.append(dict({'ArgType' : arg, 'Value' : str(primitive.randomTypeGen(arg))}))
                
                generatedExprs[spec.output].append(expr)
        # generate some random bases for primitives
                if spec.output in g.baseTypes :
                    for n in range(0, 3):
                        generatedExprs[spec.output].append(primitive.randomTypeGen(spec.output))
    return generatedExprs


#generateNonBaseExpressions - generates non-base expressions    
def generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases):
    for signature in sigs:
        for spec in signature.opspecs:
            if (not spec.output in baseCases or (spec.output in baseCases and spec.operation != baseCases[spec.output].op)) :
                newExpr = c.Expr(spec.operation)
                for arg in spec.args :
                    newExpr.args.append(dict({'ArgType' : arg, 'Value' : random.choice(exprs[arg])}))
                reduction = rewriteExpr(newExpr, 0)
                if(reduction):
                    if(isinstance(reduction, str)):
                        reducedExprs.append(c.ReducedExpr(newExpr, reduction, signature.typename))
                exprs[spec.output].append(newExpr)
    
def getVariableMapping(expr, term, vmap):
    if(isinstance(term, unicode)):
        vmap[term] = expr
    elif(isinstance(expr, c.Expr) and isinstance(term, c.Expr)):
        for i in range(len(expr.args)):
            getVariableMapping(expr.args[i].get('Value'), term.args[i].get('Value'), vmap)

def getSubstitutedRewrite(term, vmap):
    if(isinstance(term, unicode)):
        if(term in vmap):
            return vmap[term]
        else:
            return term
    elif(isinstance(term, c.Expr)):
        newargs = []
        for arg in term.args:
            newargs.append(dict({'ArgType' : arg.get('ArgType'), 'Value' : getSubstitutedRewrite(arg.get('Value'), vmap)}))
        return c.Expr(term.op, newargs)

def getRewrite(expr, lterm, rterm):
    # get mapping of variables in rewrite rule to actual values
    vmap = {}
    getVariableMapping(expr, lterm, vmap)

    # parse through right term replacing vars with actual value
    return getSubstitutedRewrite(rterm, vmap)

# Try to rewrite given expr and all args that are exprs 
def rewriteExpr(expr, count):
    if(count > 100):
        return expr
    if(isinstance(expr, c.Expr)):
        for i in range(len(expr.args)):
            expr.args[i]['Value'] = rewriteExpr(expr.args[i]['Value'], count)
        r = rewriteSingleExpr(expr)
        if(r):
            if(isinstance(r, c.Expr)):
                count += 1
                return rewriteExpr(r, count)
            else:
                return r
        else:
            return expr
    else:
        return expr

# if expr looks like a rewrite rule, get the rewritten expr
def rewriteSingleExpr(expr):
    #global equations
    for eq in g.equations:
        # check equality to term
        if equalExprToTerm(expr, eq.left):
            r = getRewrite(expr, eq.left, eq.right)
            return r

def genNumIterExpressions(num, sigs):
    reducedExprs = []
    # find all base exprs for signatures
    baseCases = findBaseCases(sigs)
    exprs = generateBaseExpressions(sigs, baseCases)
    num -= 1 
    while num > 0 :
        generateNonBaseExpressions(exprs, reducedExprs, sigs, baseCases)
        num -= 1
    return reducedExprs
