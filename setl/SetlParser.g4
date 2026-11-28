parser grammar SetlParser;
options { tokenVocab = SetlLexer; }

program
    : PROGRAM IDENTIFIER SEMI (declaration | procedureDecl | stmt)* END IDENTIFIER? SEMI? EOF
    ;

declaration
    : (VAR | CONST) declItem (COMMA declItem)* SEMI
    ;

declItem
    : IDENTIFIER (ASSIGN expr)?
    ;

procedureDecl
    : PROCEDURE IDENTIFIER LPAREN paramList? RPAREN SEMI stmt* END IDENTIFIER? SEMI
    ;

paramList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

stmt
    : declaration
    | ifStmt
    | whileStmt
    | forStmt
    | caseStmt
    | returnStmt
    | printStmt
    | assignStmt
    | exprStmt
    ;

ifStmt
    : IF expr THEN stmt* (ELSEIF expr THEN stmt*)* (ELSE stmt*)? END IF? SEMI
    ;

whileStmt
    : WHILE expr LOOP stmt* END LOOP? SEMI
    ;

forStmt
    : FOR iterator LOOP stmt* END LOOP? SEMI
    ;

iterator
    : IDENTIFIER IN expr (PIPE expr)?
    ;

caseStmt
    : CASE expr OF caseAlt+ (OTHERWISE ARROW stmt*)? END CASE? SEMI
    ;

caseAlt
    : exprList ARROW stmt*
    ;

returnStmt
    : RETURN expr? SEMI
    ;

printStmt
    : PRINT LPAREN exprList? RPAREN SEMI
    ;

assignStmt
    : lvalue ASSIGN expr SEMI
    ;

lvalue
    : IDENTIFIER (LPAREN exprList RPAREN | LBRACK expr RBRACK)*
    ;

exprStmt
    : expr SEMI
    ;

exprList
    : expr (COMMA expr)*
    ;

expr
    : quantExpr
    ;

quantExpr
    : (EXISTS | FORALL) IDENTIFIER IN expr (PIPE orExpr)?
    | orExpr
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
    : addExpr ((EQ | NE | LT | GT | LE | GE | IN | NOTIN | SUBSET) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS | UNION | LESS | WITH) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH | MOD | DIV | INTER) rangeExpr
    | rangeExpr
    ;

rangeExpr
    : unaryExpr (RANGE unaryExpr)?
    ;

unaryExpr
    : (MINUS | CARD) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : primary (LPAREN exprList? RPAREN | LBRACK expr RBRACK | DOT IDENTIFIER)*
    ;

primary
    : LPAREN expr RPAREN
    | setFormer
    | tupleFormer
    | IDENTIFIER
    | INT_LIT
    | REAL_LIT
    | STRING_LIT
    | TRUE_
    | FALSE_
    | OM
    ;

setFormer
    : LBRACE RBRACE
    | LBRACE exprList RBRACE
    | LBRACE expr COLON iterator (PIPE expr)? RBRACE
    ;

tupleFormer
    : LBRACK RBRACK
    | LBRACK exprList RBRACK
    | LBRACK expr COLON iterator (PIPE expr)? RBRACK
    ;
