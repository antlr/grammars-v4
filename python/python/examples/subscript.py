# subscript
#     : ELLIPSIS
#     | test
#     | test? COLON test? sliceop?
#     ;
#
# sliceop
#     : COLON test?
#     ;

# ELLIPSIS
b[...]

# test
b[a]

# COLON
b[:]

# test COLON
b[a:]

# COLON test
b[:a]

# test COLON test
b[a:a]

# COLON COLON
b[::]

# COLON COLON test
b[::-1]

# test COLON COLON test
b[a::2]

# COLON test COLON test
b[:a:2]

# test COLON test COLON test
b[1:a:2]
