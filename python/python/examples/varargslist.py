# varargslist
#     : (vardef_parameters COMMA)? (varargs (COMMA vardef_parameters)? (COMMA varkwargs)? | varkwargs) COMMA?
#     | vardef_parameters COMMA?
#     ;
#
# vardef_parameters
#     : vardef_parameter (COMMA vardef_parameter)*
#     ;
#
# vardef_parameter
#     : name (ASSIGN test)?
#     ;
#
# varargs
#     : STAR name
#     ;
#
# varkwargs
#     : POWER name
#     ;

# vardef_parameters COMMA
# NAME COMMA
lambda x,: 5

# vardef_parameters COMMA
# NAME COMMA STAR COMMA NAME COMMA
lambda x, *, y,: 5

# vardef_parameters
# NAME COMMA STAR COMMA NAME ASSIGN test
lambda x, *, y=7: 5

# varargs
lambda *y: 8

# varargs COMMA vardef_parameters COMMA
lambda *y, z: 8

# vardef_parameters COMMA varargs COMMA vardef_parameters COMMA
lambda a, b, *y, z: 8

# vardef_parameters COMMA varargs COMMA vardef_parameters COMMA varkwargs
lambda a, b, *y, z, **k: 8

# varkwargs
lambda **z: 8

# vardef_parameters COMMA varkwargs
lambda a, b, c, **k: 8
