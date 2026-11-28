parser grammar SimscriptParser;
options { tokenVocab = SimscriptLexer; }

program
    : preamble? mainBlock routineDecl* EOF
    ;

preamble
    : PREAMBLE preambleItem* END
    ;

preambleItem
    : entityDecl
    | processesDecl
    | eventNoticesDecl
    | definitionStmt
    ;

entityDecl
    : EVERY IDENTIFIER HAS attrDecl (COMMA attrDecl)* DOT
    ;

attrDecl
    : (A | AN) IDENTIFIER
    ;

processesDecl
    : PROCESSES INCLUDE IDENTIFIER (COMMA IDENTIFIER)* DOT
    ;

eventNoticesDecl
    : EVENT NOTICES INCLUDE IDENTIFIER (COMMA IDENTIFIER)* DOT
    ;

definitionStmt
    : DEFINE IDENTIFIER (COMMA IDENTIFIER)* AS typeSpec DOT
    ;

typeSpec
    : (INTEGER | REAL | TEXT | ALPHA) VARIABLE?
    ;

mainBlock
    : MAIN statement* END
    ;

routineDecl
    : (ROUTINE | PROCESS | EVENT) TO IDENTIFIER (GIVEN paramList)? statement* END
    ;

paramList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

statement
    : letStmt
    | addStmt
    | subtractStmt
    | ifStmt
    | forStmt
    | whileStmt
    | scheduleStmt
    | causeStmt
    | callStmt
    | createStmt
    | destroyStmt
    | printStmt
    | returnStmt
    | stopStmt
    | workStmt
    ;

letStmt
    : LET lvalue ASSIGN expr
    ;

addStmt
    : ADD expr TO lvalue
    ;

subtractStmt
    : SUBTRACT expr FROM lvalue
    ;

ifStmt
    : IF expr statement* (ELSE statement*)? ALWAYS
    ;

forStmt
    : FOR EACH IDENTIFIER IN expr (WITH expr)? DO statement* LOOP
    ;

whileStmt
    : WHILE expr DO statement* LOOP
    ;

scheduleStmt
    : SCHEDULE (A | AN) IDENTIFIER (GIVEN exprList)? (IN expr UNITS)? (AT expr)?
    ;

causeStmt
    : CAUSE (A | AN) IDENTIFIER (GIVEN exprList)?
    ;

callStmt
    : CALL IDENTIFIER (GIVEN exprList)?
    ;

createStmt
    : CREATE (A | AN) IDENTIFIER (CALLED IDENTIFIER)?
    ;

destroyStmt
    : DESTROY IDENTIFIER
    ;

printStmt
    : PRINT exprList
    ;

returnStmt
    : RETURN expr?
    ;

stopStmt
    : STOP
    ;

workStmt
    : WORK expr UNITS?
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
    : orExpr OR andExpr
    | andExpr
    ;

andExpr
    : andExpr AND notExpr
    | notExpr
    ;

notExpr
    : NOT notExpr
    | relExpr
    ;

relExpr
    : addExpr ((ASSIGN | NE | LT | GT | LE | GE) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH | MOD) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : (PLUS | MINUS) unaryExpr
    | primary
    ;

primary
    : LPAREN expr RPAREN
    | IDENTIFIER (LPAREN exprList RPAREN)?
    | INT_LIT
    | REAL_LIT
    | STRING_LIT
    ;
