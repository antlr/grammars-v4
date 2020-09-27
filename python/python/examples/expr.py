# expr
#     : AWAIT? atom trailer*
#     | <assoc=right> expr op=POWER expr
#     | op=(ADD | MINUS | NOT_OP) expr
#     | expr op=(STAR | DIV | MOD | IDIV | AT) expr
#     | expr op=(ADD | MINUS) expr
#     | expr op=(LEFT_SHIFT | RIGHT_SHIFT) expr
#     | expr op=AND_OP expr
#     | expr op=XOR expr
#     | expr op=OR_OP expr
#     ;

# atom
"123"

# AWAIT atom
await 5

# atom trailer
"1231".lower()

# atom trailer trailer
"1231".lower().upper()

# AWAIT atom trailer trailer
await "1231".lower().upper()

# expr op=POWER expr op=POWER expr
2 ** 2 ** 3

# ADD expr
+6

# MINUS expr
-6

# NOT_OP expr
~6

# expr STAR expr
6 * 7

# expr DIV expr
6 / 7

# expr MOD expr
6 % 8

# expr IDIV expr
6 // 7

# expr AT expr
6 @ 2

# expr ADD expr
6 + 1

# expr MINUS expr
7 - 9

# expr LEFT_SHIFT expr
8 << 9

# expr RIGHT_SHIFT expr
4 >> 1

# expr op=AND_OP expr
4 & 6

# expr op=XOR expr
4 ^ 7

# expr op=OR_OP expr
7 | 1
