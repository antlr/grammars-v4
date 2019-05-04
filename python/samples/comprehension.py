# comp_for
#     : FOR exprlist IN logical_test comp_iter?
#     ;
#
# comp_iter
#     : comp_for
#     | IF test comp_iter?
#     ;

# FOR exprlist IN logical_test
[x for x in a]

# FOR exprlist IN logical_test comp_for
[x for x in a for a in k]

# FOR exprlist IN logical_test IF test
[x for x in a if x == 1]

# FOR exprlist IN logical_test IF test IF test
[x for x in a if x == 1 if x != 9]

# FOR exprlist IN logical_test IF test comp_for
[x for x in a if x == 1 for a in q]
