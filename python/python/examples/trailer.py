# trailer
#     : OPEN_PAREN (argument (COMMA argument)* COMMA?)? CLOSE_PAREN
#     | OPEN_BRACKET subscript (COMMA subscript)* COMMA? CLOSE_BRACKET
#     | DOT name
#     ;

# DOT name
a.b

# OPEN_PAREN CLOSE_PAREN
x()

# OPEN_PAREN argument CLOSE_PAREN
x(a)

# OPEN_PAREN argument COMMA argument COMMA CLOSE_PAREN
x(a, b,)

# OPEN_PAREN argument COMMA argument COMMA argument CLOSE_PAREN
x(a, b, c)

# OPEN_BRACKET subscript CLOSE_BRACKET
x[a]

# OPEN_BRACKET subscript COMMA subscript COMMA CLOSE_BRACKET
x[a, b,]

# OPEN_BRACKET subscript COMMA subscript COMMA subscript CLOSE_BRACKET
x[a, b, c]