# yield_expr
#     : YIELD yield_arg?
#     ;
#
# yield_arg
#     : FROM test
#     | testlist
#     ;

def f():

    # YIELD
    yield

    # YIELD FROM test
    yield from g

    # YIELD testlist
    yield x, a, b if x else a

