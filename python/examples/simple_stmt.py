# simple_stmt: small_stmt (SEMI_COLON small_stmt)* SEMI_COLON? (NEWLINE | EOF)

# small_stmt NEWLINE
x = 5

# small_stmt SEMI_COLON small_stmt NEWLINE
x = 5 ; y = 7

# small_stmt SEMI_COLON NEWLINE
x = 5 ;

# small_stmt SEMI_COLON small_stmt SEMI_COLON small_stmt SEMI_COLON small_stmt SEMI_COLON EOF
x = 5 ; y = 7 ; z = 9 ;