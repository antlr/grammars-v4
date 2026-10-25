# classdef: CLASS name (OPEN_PAREN arglist? CLOSE_PAREN)? COLON suite

# CLASS NAME COLON suite
class foo: pass

# CLASS NAME OPEN_PAREN CLOSE_PAREN COLON suite
class bar(): pass

# CLASS NAME OPEN_PAREN arglist CLOSE_PAREN COLON suite
class baz(foo):
    pass
