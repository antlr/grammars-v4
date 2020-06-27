# funcdef: ASYNC? DEF name OPEN_PAREN typedargslist? CLOSE_PAREN (ARROW test)? COLON suite

# DEF NAME OPEN_PAREN CLOSE_PAREN COLON suite
def foo(): pass

# ASYNC DEF NAME OPEN_PAREN typedargslist? CLOSE_PAREN COLON suite
async def bar(one): pass

# DEF NAME OPEN_PAREN typedargslist? CLOSE_PAREN ARROW test COLON suite
def baz(one, two) -> int: pass
