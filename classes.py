import global_var as g
import re




#############################################
####  Node                              #####
#############################################
class Node:
    def __init__(self,type,children=None,value=None):
        self.type = type
        if children :
            self.children = children
        else :
            self.children = []
        self.value = value 
 
#############################################
####  SignatureStruct                   #####
############################################# 
class SignatureStruct:
    def __init__(self,typename,opspecs):
        self.typename = typename
        self.opspecs = opspecs
 
    # checks to see if all the types contained within this signature
    # are defined as adts in the given environment provided by the
    def isValid(self, env):
        valid = True

        for opspec in self.opspecs:
            if not opspec.isValid( env ):
                valid = False
                break

        return valid
        

    def getAllOpNames(self):
        opNames = []

        for opspec in self.opspecs:
            opNames.append( opspec.operation )

        return opNames

    def containsOperation(self, opName):
        if (opName in self.getAllOpNames()):
            return True

        return False

    def toString(self):
        text = "ADT: " + self.typename;

        for op in self.opspecs:
            text += "\n   " + op.toString()

        return text

#############################################
####  OperationSpecStruct               #####
#############################################  
class OperationSpecStruct:
    def __init__(self,operation,args,output):
        self.operation = operation
        self.args = args
        self.output = output 

    # checks to see if all the types contained within
    def isValid(self, adts):
        valid = True

        for arg in self.args:
            if not( arg in g.baseTypes or arg in adts ):
                valid = False
                break
        
        if not( self.output in g.baseTypes or self.output in adts ):
            valid = False

        return valid
            

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

#############################################
####  EquationStruct                    #####
#############################################  

##
# EquationStruct helper functions
def containDups(collection):
    seen = set()

    for item in collection:
        if item in seen:
            return True
        seen.add(item)

    return False

def isBool(string):
    return re.match( r'[#t|#f]', string, re.U )
##

class EquationStruct:
    def __init__( self, left, right ):
        self.left = left
        self.right = right

    def getOpList( self ):
        ops = []

        if isinstance( self.left, Expr ):
            ops.append( self.left.findAllOps() )

        if isinstance( self.right, Expr ):
            ops.append( self.right.findAllOps() )

        return ops 

    def toString( self ):
        text = ""

        if isinstance( self.left, Expr ):
            text += self.left.toSexpr()
        else:
            text += self.left

        text += " = "
    
        if isinstance( self.right, Expr ):
            text += self.right.toSexpr()
        else:
            text += self.right

        return text

    def validEqIds( self ):
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

    def validOps( self, sigs ):
        results = []

        for opList in self.getOpList():
            for op in opList:
                sub_result = []
                
                if not( op in g.primitiveOps ):
                    for sig in sigs:
                        sub_result.append( sig.containsOperation(op) )

                    if not(True in sub_result):
                        results.append( False )
                else:
                    print "s " + op
                

        return not( False in results )


    def isValid( self, sigs ):
        return self.validOps( sigs ) and self.validEqIds()

#############################################
####  Expr                              #####
#############################################  
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

#############################################
####  ReducedExpr                       #####
#############################################  
class ReducedExpr:
    def __init__(self, expr, reduct, adtName):
        self.expr = expr
        self.reduct = reduct
        self.adtName = adtName
