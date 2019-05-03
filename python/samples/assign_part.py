# assign_part
#     : (ASSIGN testlist_star_expr)* (ASSIGN yield_expr)?
#     | COLON test (ASSIGN testlist)?
#     | op=( ADD_ASSIGN
#          | SUB_ASSIGN
#          | MULT_ASSIGN
#          | AT_ASSIGN
#          | DIV_ASSIGN
#          | MOD_ASSIGN
#          | AND_ASSIGN
#          | OR_ASSIGN
#          | XOR_ASSIGN
#          | LEFT_SHIFT_ASSIGN
#          | RIGHT_SHIFT_ASSIGN
#          | POWER_ASSIGN
#          | IDIV_ASSIGN
#          )
#       (yield_expr | testlist)
#     ;

# Yield tests (should not be used outside the function)
def f():
    # ASSIGN yield_expr
    x = yield

    # ASSIGN testlist_star_expr ASSIGN yield_expr
    x = y = yield

    # testlist_star_expr '+=' yield_expr
    x += yield

# ASSIGN testlist_star_expr
z = 5

# testlist_star_expr ASSIGN testlist_star_expr ASSIGN testlist_star_expr
x = y = z

# COLON test
x: int

# COLON test ASSIGN testlist
x: int = 8

# '-=' testlist
x -= 9
