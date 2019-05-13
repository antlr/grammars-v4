# def_parameters: def_parameter (COMMA def_parameter)*;
# def_parameter: named_parameter (ASSIGN test)?;
# named_parameter: NAME (COLON test)?;

# NAME COLON test
def single_typed(x: int): pass

# NAME COLON test ASSIGN test
def single_default_typed(x: int = 4): pass

# NAME COMMA NAME ASSIGN test
def second_default(x, y = 4): pass
