# logical_test
#     : comparison
#     | NOT logical_test
#     | logical_test op=AND logical_test
#     | logical_test op=OR logical_test
#     ;
#
# comparison
#     : comparison (LESS_THAN | GREATER_THAN | EQUALS | GT_EQ | LT_EQ | NOT_EQ_1 | NOT_EQ_2 | optional=NOT? IN | IS optional=NOT?) comparison
#     | expr
#     ;

# expr EQUALS expr
a == b

# not expr
not a

# expr AND expr
a and b

# expr OR expr
a or b

# expr LESS_THAN expr
a < b

# expr GREATER_THAN expr
a > b

# expr GT_EQ expr
a >= b

# expr LT_EQ expr
a <= b

# expr NOT_EQ_2 expr
a != b

# expr IN expr
a in b

# expr NOT IN expr
a not in b

# expr IS expr
a is b

# expr IS NOT expr
a is not b
