/*
BSD License

Parser grammar for NATURAL. See NaturalLexer.g4 for background and
sourcing caveats.

Entry rule: program
*/

parser grammar NaturalParser;

options {
    tokenVocab = NaturalLexer;
}

program
    : defineDataStmt? (subroutineDecl | stmt)* END EOF
    ;

// ---------------------------------------------------------------------------
// DEFINE DATA
// ---------------------------------------------------------------------------

defineDataStmt
    : DEFINE DATA dataSection+ END_DEFINE
    ;

dataSection
    : (LOCAL | PARAMETER | GLOBAL) (USING IDENTIFIER | fieldDecl+)
    ;

// Level numbers (1, 2, 3, ...) indicate grouping in real NATURAL; this
// grammar accepts them without enforcing the hierarchy they describe.
fieldDecl
    : INT_LIT IDENTIFIER dataFormat?
    ;

// A format spec such as "(A20)" or "(N5.2)" lexes as a single IDENTIFIER
// token ("A20", "N5") optionally followed by ".<decimals>".
dataFormat
    : LPAREN IDENTIFIER (DOT INT_LIT)? RPAREN
    ;

subroutineDecl
    : DEFINE SUBROUTINE IDENTIFIER stmt* END_SUBROUTINE
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

stmt
    : moveStmt
    | computeStmt
    | addStmt
    | subtractStmt
    | multiplyStmt
    | divideStmt
    | ifStmt
    | forStmt
    | repeatStmt
    | decideStmt
    | performStmt
    | writeStmt
    | displayStmt
    | inputStmt
    | escapeStmt
    | stopStmt
    | findStmt
    | readStmt
    ;

moveStmt
    : MOVE expr TO variable
    ;

computeStmt
    : COMPUTE variable EQSIGN expr
    ;

addStmt
    : ADD expr TO variable
    ;

subtractStmt
    : SUBTRACT expr FROM variable
    ;

multiplyStmt
    : MULTIPLY variable BY expr
    ;

divideStmt
    : DIVIDE variable BY expr (GIVING variable)?
    ;

ifStmt
    : IF expr THEN? stmt* (ELSE stmt*)? END_IF
    ;

forStmt
    : FOR variable EQSIGN expr TO expr (STEP expr)? stmt* END_FOR
    ;

repeatStmt
    : REPEAT stmt* END_REPEAT
    ;

decideStmt
    : DECIDE ON (FIRST | EVERY)? (VALUE | VALUES) OF variable decideAlt+ (NONE (VALUE | VALUES) COLON? stmt*)? END_DECIDE
    ;

decideAlt
    : (VALUE | VALUES) exprList COLON? stmt*
    ;

performStmt
    : PERFORM IDENTIFIER exprList?
    ;

writeStmt
    : WRITE writeItem*
    ;

displayStmt
    : DISPLAY writeItem*
    ;

writeItem
    : STRING_LIT
    | expr
    ;

inputStmt
    : INPUT (STRING_LIT? variable)+
    ;

escapeStmt
    : ESCAPE (BOTTOM | TOP | ROUTINE) IMMEDIATE?
    ;

stopStmt
    : STOP
    ;

findStmt
    : FIND (NUMBER expr)? IDENTIFIER (WITH expr)? stmt* END_FIND
    ;

readStmt
    : READ IDENTIFIER (BY IDENTIFIER)? stmt* END_READ
    ;

// ---------------------------------------------------------------------------
// Variables and expressions (precedence, highest to lowest: unary,
// multiplicative, additive, relational, NOT, AND, OR)
// ---------------------------------------------------------------------------

variable
    : IDENTIFIER (DOT IDENTIFIER)*
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
    : addExpr ((EQSIGN | NESIGN | LTSIGN | GTSIGN | LESIGN | GESIGN | EQ | NE | LT | GT | LE | GE) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : (PLUS | MINUS) unaryExpr
    | factor
    ;

factor
    : LPAREN expr RPAREN
    | constant
    | IDENTIFIER LPAREN exprList RPAREN
    | variable
    ;

constant
    : INT_LIT
    | DEC_LIT
    | STRING_LIT
    | TRUE_
    | FALSE_
    ;
