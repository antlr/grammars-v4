# argument
#     : test (comp_for | ASSIGN test)?
#     | (POWER | STAR) test
#     ;

# test
b(x)

# test comp_for
b(x for x in a)

# test ASSIGN test
b(x=i)

# test COMMA test ASSIGN test COMMA test ASSIGN test
b(z, x=i, y=u)

# POWER test
b(**z)

# STAR test
b(*z)

# test COMMA STAR test COMMA test ASSIGN test
b(y, *z, x=i)
