# testlist_comp
#     : (test | star_expr) (comp_for | (COMMA (test | star_expr))* COMMA?)
#     ;

# test
[x]

# star_expr comp_for
[z for z in a]

# test COMMA star_expr COMMA
[x, *a,]

# star_expr COMMA test COMMA star_expr
[*u, a, *i]
