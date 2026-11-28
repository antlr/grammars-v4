parser grammar RatforParser;
options { tokenVocab = RatforLexer; }

program
    : topLevel* EOF
    ;

topLevel
    : subroutineDecl
    | functionDecl
    | statement
    ;

subroutineDecl
    : SUBROUTINE IDENTIFIER (LPAREN paramList? RPAREN)? statement* END
    ;

functionDecl
    : typeSpec? FUNCTION IDENTIFIER LPAREN paramList? RPAREN statement* END
    ;

paramList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

statement
    : block
    | ifStmt
    | whileStmt
    | forStmt
    | doStmt
    | repeatStmt
    | breakStmt
    | nextStmt
    | returnStmt
    | stopStmt
    | callStmt
    | declStmt
    | dimensionStmt
    | commonStmt
    | printStmt
    | readStmt
    | writeStmt
    | assignStmt
    | labeledStmt
    ;

block
    : LBRACE statement* RBRACE
    ;

labeledStmt
    : INT_LIT COLON statement
    ;

ifStmt
    : IF LPAREN expr RPAREN statement (ELSE statement)?
    ;

whileStmt
    : WHILE LPAREN expr RPAREN statement
    ;

forStmt
    : FOR LPAREN assignStmt? SEMI expr? SEMI assignStmt? RPAREN statement
    ;

doStmt
    : DO IDENTIFIER EQSIGN expr COMMA expr (COMMA expr)? statement
    ;

repeatStmt
    : REPEAT statement UNTIL LPAREN expr RPAREN
    ;

breakStmt
    : BREAK
    ;

nextStmt
    : NEXT
    ;

returnStmt
    : RETURN (LPAREN expr RPAREN)?
    ;

stopStmt
    : STOP STRING_LIT?
    ;

callStmt
    : CALL IDENTIFIER (LPAREN exprList? RPAREN)?
    ;

typeSpec
    : INTEGER
    | REAL
    | DOUBLEPRECISION
    | LOGICAL
    | CHARACTER (STAR INT_LIT)?
    ;

declStmt
    : typeSpec declItem (COMMA declItem)*
    ;

declItem
    : IDENTIFIER (LPAREN exprList RPAREN)?
    ;

dimensionStmt
    : DIMENSION declItem (COMMA declItem)*
    ;

commonStmt
    : COMMON (SLASH IDENTIFIER SLASH)? declItem (COMMA declItem)*
    ;

printStmt
    : PRINT STAR COMMA exprList?
    ;

readStmt
    : READ LPAREN STAR COMMA STAR RPAREN exprList?
    ;

writeStmt
    : WRITE LPAREN STAR COMMA STAR RPAREN exprList?
    ;

assignStmt
    : lvalue EQSIGN expr
    ;

lvalue
    : IDENTIFIER (LPAREN exprList RPAREN)?
    ;

exprList
    : expr (COMMA expr)*
    ;

expr
    : orExpr
    ;

orExpr
    : orExpr (OROR | PIPE) andExpr
    | andExpr
    ;

andExpr
    : andExpr (ANDAND | AMP) notExpr
    | notExpr
    ;

notExpr
    : NOT notExpr
    | relExpr
    ;

relExpr
    : addExpr ((EQEQ | NE | LT | GT | LE | GE) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH) powExpr
    | powExpr
    ;

powExpr
    : unaryExpr (STAR2 powExpr)?
    ;

unaryExpr
    : (PLUS | MINUS) unaryExpr
    | primary
    ;

primary
    : LPAREN expr RPAREN
    | IDENTIFIER LPAREN exprList? RPAREN
    | IDENTIFIER
    | INT_LIT
    | REAL_LIT
    | STRING_LIT
    | TRUE_
    | FALSE_
    ;
