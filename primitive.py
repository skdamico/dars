import random

#findPrimitiveType - returns the primitive type of a string
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
    
#findUniPrimitiveType - returns the primitive type of a unicode
def findUniPrimitiveType(s):
    bool = ['#t', '#T', '#f', '#F']
    if (str(s) in bool) :
        return 'boolean'
    elif(s.isnumeric()) :
        return 'int'
    elif(s.isalpha() or str(s).isalnum()) :
        return 'string'
    else :
        return 'char'

#findPrimOutputType - returns the primitive operation type of the input
def findPrimOutputType(op):
    intTypes = ['+', '-', '*']
    if (op in intTypes):
        return 'int'
    else: return 'boolean'

#randomTypeGen - random type generator
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