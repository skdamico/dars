import ply.lex as lex
import ply.yacc as yacc
# <input>  ::=  "Signatures:" <signatures> "Equations:" <equations>

# <signatures>  ::=  <signature>
            # |  <signature> <signatures>

# <signature>  ::=  "ADT:" <typename> <operationSpecs>

# <typename>  ::=  <identifier>

# <operationSpecs>  ::=  <operationSpec>
            # |  <operationSpec> <operationSpecs>

# <operationSpec>  ::=  <operation> ":"            "->" <type>
            # |  <operation> ":" <argTypes> "->" <type>

# <operation>  ::=  <identifier>

# <argTypes>  ::=  <type>
            # |  <type> "*" <argTypes>

# <type>  ::=  "int"
            # |  "boolean"
            # |  "character"
            # |  "string"
            # |  <typename>

# <equations>  ::=  <empty>




# <identifier> ? <initial> <subsequent>*
         # | <peculiar identifier>
# <initial> ? <constituent> | <special initial>
         # | <inline hex escape>
# <letter> ? a | b | c | ... | z
         # | A | B | C | ... | Z
# <constituent> ? <letter>
         # | ?any character whose Unicode scalar value is greater than
             # 127, and whose category is Lu, Ll, Lt, Lm, Lo, Mn,
             # Nl, No, Pd, Pc, Po, Sc, Sm, Sk, So, or Co?
# <special initial> ? ! | $ | % | & | * | / | : | < | =
         # | > | ? | ^ | _ | ~
# <subsequent> ? <initial> | <digit>
         # | <any character whose category is Nd, Mc, or Me>
         # | <special subsequent>
# <digit> ? 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
# <hex digit> ? <digit>
         # | a | A | b | B | c | C | d | D | e | E | f | F
# <special subsequent> ? + | - | . | @
# <inline hex escape> ? \x<hex scalar value>;
# <hex scalar value> ? <hex digit>+
# <peculiar identifier> ? + | - | ... | -> <subsequent>*
# <boolean> ? #t | #T | #f | #F
# <character> ? #\<any character>
         # | #\<character name>
         # | #\x<hex scalar value>
# <character name> ? nul | alarm | backspace | tab
         # | linefeed | newline | vtab | page | return
         # | esc | space | delete
# <string> ? " <string element>* "
# <string element> ? <any character other than " or \>
         # | \a | \b | \t | \n | \v | \f | \r
         # | \" | \\
         # | \<intraline whitespace><line ending>
            # <intraline whitespace>
         # | <inline hex escape>
# <intraline whitespace> ? <character tabulation>
         # | <any character whose category is Zs>

tokens = (
    "LETTER",
    "DIGIT",
    "SPECIAL_SUBSEQUENT",
    "SPECIAL_INITIAL",
    "BOOLEAN",
    "UNICODE_CONSTITUENT",
    "CHARACTER_NAME",
    "STRING"
)

def t_STRING(t):
    ur'[\a\b\t\n\v\f\r\"\\]'
    return t

def t_CHARACTER_NAME(t):
    ur'nul | alarm | backspace | tab | linefeed | newline | vtab | page | return | esc | space | delete'
    return t

def t_UNICODE_CONSTITUENT(t):
    ur'[\u0080-\uFFFF]'
    return t

def t_BOOLEAN(t):
    ur'\#t|\#T|\#f|\#F'
    return t

def t_SPECIAL_INITIAL(t):
    ur'[!$%&*/:<=>?^_~]'
    return t

def t_SPECIAL_SUBSEQUENT(t):
    ur'[+-.@]'
    return t

def t_LETTER(t):
    ur'[a-zA-Z]'
    return t
    
def t_DIGIT(t):
    ur'[0-9]'
    return t

t_ignore = u'\t\n '

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)

lexer = lex.lex()

# Test it out
data = u'''
A b 2  5  h+ A1 .6 @ - ?@/:^$%<>=_~#t#y \u0081  nulalarm
'''

# Give the lexer some input
lexer.input(data)

# Tokenize
while True:
    tok = lexer.token()
    if not tok: break      # No more input
    print tok

def p_hex_digit(p):
    '''hex_digit : DIGIT
                 | LETTER'''
    if p[1] == ur'[a-fA-F]' : 
        p[0] = p[1]
        print "suck it"
    elif p[1] == ur'[0-9]' :
        p[0] = p[1]
        print "digit"

def p_hex_digit_plus(p):
    '''hex_digit_plus : hex_digit
                      | hex_digit hex_digit_plus'''
    if len(p) == 1 : 
        p[0] = p[1]
    elif len(p) > 1: 
        p[0] = p[1].append(p[2])

def p_hex_scalar_value(p):
    'hex_scalar_value : hex_digit_plus'
    p[0] = p[1]

def p_error(p):
    print("Syntax error at '%s'" % p.value)

yacc.yacc()

print yacc.parse("ab")
