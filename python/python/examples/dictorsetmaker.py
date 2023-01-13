# dictorsetmaker
#     : (test COLON test | POWER expr) (COMMA (test COLON test | POWER expr))* COMMA? // key_datum_list
#     | test COLON test comp_for                                                      // dict_comprehension
#     | testlist_comp
#     ;

# test COLON test
{d : y}

# POWER expr
{**x}

# test COLON test COMMA test COLON test
{d : y, x : z}

# test COLON test COMMA test COLON test COMMA
{d : y, x : z,}

# POWER expr COMMA test COLON test
{**x, d : y}

# test COLON test COMMA POWER expr
{d : y, **x}

# test COLON test comp_for
{d : y for d, y in x}

# testlist_comp
{x, q, y}
