# expr_stmt: testlist_star_expr assign_part?
#
# testlist_star_expr : (test | star_expr) (COMMA (test | star_expr))* COMMA?
#
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
    # test ASSIGN yield_expr
    x = yield

    # test COMMA test COMMA ASSIGN test COMMA star_expr ASSIGN yield_expr
    x, [*t], = y, *z = yield

    # test '+=' yield_expr
    x += yield

# test ASSIGN testlist_star_expr
z = 1, *y

# test ASSIGN test COMMA test ASSIGN test COMMA star_expr COMMA
x = y, a = z, *q,

# test COLON test
x: int

# COLON test ASSIGN testlist
x: int = 8

# test '-=' testlist
x -= 9
