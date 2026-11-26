/*
BSD License

Parser grammar for JOVIAL. See JovialLexer.g4 for background and sourcing
caveats.

JOVIAL's own operator precedence and the full, authoritative statement and
declaration grammar could not be confirmed against a primary source (see
JovialLexer.g4), so this grammar uses a conventional ALGOL-family reading:
a standard left-to-right precedence ladder for binary operators, and BEGIN
... END blocks as the only compound-statement delimiter.

Entry rule: program
*/

parser grammar JovialParser;

options {
    tokenVocab = JovialLexer;
}

program
    : programHeader? topItem* TERM?
      EOF
    ;

programHeader
    : (PROGRAM | COMPOOL | MODULE) IDENTIFIER SEMI
    ;

topItem
    : itemDecl
    | tableDecl
    | defDecl
    | constantDecl
    | typeDecl
    | procDecl
    | START SEMI
    ;

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

itemDecl
    : ITEM IDENTIFIER (typeSpec | STATUS LPAREN identList RPAREN) SEMI
    ;

typeSpec
    : IDENTIFIER INT_LIT?
    ;

identList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

tableDecl
    : TABLE IDENTIFIER LPAREN expr COLON expr RPAREN SEMI (block)?
    ;

defDecl
    : DEF IDENTIFIER DEFEQ expr SEMI
    ;

constantDecl
    : CONSTANT IDENTIFIER typeSpec EQ expr SEMI
    ;

typeDecl
    : TYPE IDENTIFIER STATUS LPAREN identList RPAREN SEMI
    ;

procDecl
    : PROC IDENTIFIER LPAREN paramList? RPAREN (COLON typeSpec)? SEMI block
    ;

paramList
    : IDENTIFIER (COLON IDENTIFIER)*
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

block
    : BEGIN_ stat* END_ SEMI?
    ;

stat
    : itemDecl
    | tableDecl
    | defDecl
    | constantDecl
    | assignStat SEMI
    | ifStat
    | forStat
    | whileStat
    | caseStat
    | gotoStat SEMI
    | callStat SEMI
    | returnStat SEMI
    | exitStat SEMI
    | block
    | labelStat
    | exprStat SEMI
    ;

assignStat
    : lvalue EQ expr
    ;

lvalue
    : IDENTIFIER (LPAREN argList? RPAREN)? (DOT IDENTIFIER)*
    ;

ifStat
    : IF expr SEMI stat (ELSE stat)?
    ;

forStat
    : FOR IDENTIFIER COLON expr BY expr WHILE expr SEMI stat
    ;

whileStat
    : WHILE expr SEMI stat
    ;

caseStat
    : CASE expr SEMI BEGIN_ caseAlt+ END_ SEMI?
    ;

caseAlt
    : INT_LIT (COMMA INT_LIT)* COLON stat
    ;

gotoStat
    : GOTO IDENTIFIER
    ;

callStat
    : CALL IDENTIFIER (LPAREN argList? RPAREN)?
    ;

returnStat
    : RETURN expr?
    ;

exitStat
    : EXIT
    ;

labelStat
    : IDENTIFIER COLON stat
    ;

exprStat
    : expr
    ;

argList
    : expr (COMMA expr)*
    ;

// ---------------------------------------------------------------------------
// Expressions (precedence, highest to lowest: unary, multiplicative,
// additive, relational, NOT, AND, OR)
// ---------------------------------------------------------------------------

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
    : addExpr ((EQ | NEQ | LT | GT | LE | GE) addExpr)?
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
    | postfixExpr
    ;

postfixExpr
    : postfixExpr LPAREN argList? RPAREN
    | postfixExpr DOT IDENTIFIER
    | factor
    ;

factor
    : LPAREN expr RPAREN
    | constant
    | IDENTIFIER
    ;

constant
    : INT_LIT
    | FLOAT_LIT
    | STRING_LIT
    ;
