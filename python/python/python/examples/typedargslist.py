# typedargslist
#     : (def_parameters COMMA)? (args (COMMA def_parameters)? (COMMA kwargs)? | kwargs)
#     | def_parameters
#     ;

# def_parameters COMMA
def single(x, *, i): pass

# def_parameters COMMA kwargs
def f1(x, y, **z): pass

# def_parameters COMMA args COMMA def_parameters COMMA kwargs COMMA
def f1(x, y, *z: int, a, b, **c: int,): pass

# def_parameters COMMA args
def f1(x, y, *z): pass

