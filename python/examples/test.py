# test
#     : logical_test (IF logical_test ELSE test)?
#     | LAMBDA varargslist? COLON test
#     ;

# logical_test
x == y

# logical_test IF logical_test ELSE test
x == y if z == b else a == u

# LAMBDA COLON test
lambda: a

# LAMBDA varargslist COLON test
lambda x, y: a
