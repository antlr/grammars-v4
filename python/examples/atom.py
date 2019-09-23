# atom
#     : OPEN_PAREN (yield_expr | testlist_comp)? CLOSE_PAREN
#     | OPEN_BRACKET testlist_comp? CLOSE_BRACKET
#     | OPEN_BRACE dictorsetmaker? CLOSE_BRACE
#     | REVERSE_QUOTE testlist COMMA? REVERSE_QUOTE
#     | dotted_name
#     | ELLIPSIS
#     | name
#     | PRINT
#     | EXEC
#     | MINUS? number
#     | NONE
#     | STRING+
#     ;

# OPEN_PAREN CLOSE_PAREN
()

# OPEN_PAREN yield_expr CLOSE_PAREN
def f():
    (yield)

# OPEN_PAREN testlist_comp CLOSE_PAREN
(x, a, q == 1)

# OPEN_BRACKET CLOSE_BRACKET
[]

# OPEN_BRACKET testlist_comp CLOSE_BRACKET
[1, 3, b, p == 1]

# OPEN_BRACE CLOSE_BRACE
{}

# OPEN_BRACE dictorsetmaker CLOSE_BRACE
{x : y for x, y in a}

# dotted_name
b.a

# ELLIPSIS
...

# name
f

# number
90

# MINUS number
-1

# NONE
None

# STRING
"12312313"

# STRING STRING
"1231231" "123151"
