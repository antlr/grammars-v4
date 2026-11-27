/*
BSD License

Parser grammar for Euclid. See EuclidLexer.g4 for background and sourcing
caveats - this grammar is a reconstruction from Euclid's well documented
characteristics (module imports/exports, closed-scope routines, no
floating point, loop/exit control flow in place of arbitrary gotos) plus
ordinary Pascal-family syntax, not a transcription of the (unreachable)
primary 1977 language report.

Entry rule: program
*/

parser grammar EuclidParser;

options {
    tokenVocab = EuclidLexer;
}

program
    : topDecl* EOF
    ;

topDecl
    : moduleDecl
    | procDecl
    | funcDecl
    | varDecl SEMI
    | constDecl SEMI
    | typeDecl SEMI
    ;

// ---------------------------------------------------------------------------
// Modules and routines
// ---------------------------------------------------------------------------

moduleDecl
    : PERVASIVE? MODULE IDENTIFIER EQ? importsClause? exportsClause? BEGIN_ block END_ IDENTIFIER
    ;

procDecl
    : PERVASIVE? PROCEDURE IDENTIFIER LPAREN paramList? RPAREN importsClause? BEGIN_ block END_ IDENTIFIER
    ;

funcDecl
    : PERVASIVE? FUNCTION IDENTIFIER LPAREN paramList? RPAREN COLON type importsClause? BEGIN_ block END_ IDENTIFIER
    ;

importsClause
    : IMPORTS LPAREN importItem (COMMA importItem)* RPAREN
    ;

importItem
    : (VAR | CONST)? identList (FROM IDENTIFIER)?
    ;

exportsClause
    : EXPORTS LPAREN identList RPAREN
    ;

paramList
    : param (COMMA param)*
    ;

param
    : VAR? identList COLON type
    ;

identList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

block
    : declPart* stmt*
    ;

declPart
    : varDecl SEMI
    | constDecl SEMI
    | typeDecl SEMI
    | procDecl
    | funcDecl
    ;

// ---------------------------------------------------------------------------
// Declarations
// ---------------------------------------------------------------------------

varDecl
    : VAR identList COLON type (EQ expr)?
    ;

constDecl
    : CONST IDENTIFIER (COLON type)? EQ expr
    ;

typeDecl
    : TYPE IDENTIFIER EQ typeDef
    ;

typeDef
    : type
    | RECORD fieldList END_ RECORD
    ;

fieldList
    : fieldGroup (SEMI fieldGroup)* SEMI?
    ;

fieldGroup
    : identList COLON type
    ;

type
    : INTEGER
    | BOOLEAN
    | CHAR
    | STRING_T
    | ARRAY LBRACK expr DOTDOT expr RBRACK OF type
    | POINTER TO type
    | COLLECTION OF POINTER TO type
    | IDENTIFIER
    ;

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

stmt
    : assignStmt SEMI
    | callStmt SEMI
    | exitStmt SEMI
    | assertStmt SEMI
    | assumeStmt SEMI
    | returnStmt SEMI
    | resultStmt SEMI
    | allocStmt SEMI
    | freeStmt SEMI
    | ifStmt
    | loopStmt
    | whileStmt
    | forStmt
    | caseStmt
    ;

assignStmt
    : variable ASSIGN expr
    ;

variable
    : IDENTIFIER (LBRACK expr RBRACK | DOT IDENTIFIER | CARET)*
    ;

callStmt
    : IDENTIFIER LPAREN exprList? RPAREN
    ;

exitStmt
    : EXIT (WHEN expr)?
    ;

assertStmt
    : ASSERT expr
    ;

assumeStmt
    : ASSUME expr
    ;

returnStmt
    : RETURN
    ;

resultStmt
    : RESULT expr
    ;

allocStmt
    : NEW LPAREN variable RPAREN
    ;

freeStmt
    : FREE LPAREN variable RPAREN
    ;

ifStmt
    : IF expr THEN stmt* (ELSE stmt*)? END_ IF
    ;

loopStmt
    : LOOP stmt* END_ LOOP
    ;

whileStmt
    : WHILE expr DO stmt* END_ LOOP
    ;

forStmt
    : FOR IDENTIFIER COLON expr (TO | DOWNTO) expr DO stmt* END_ LOOP
    ;

caseStmt
    : CASE expr OF caseAlt+ (OTHERWISE COLON? stmt*)? END_ CASE
    ;

caseAlt
    : exprList COLON stmt*
    ;

// ---------------------------------------------------------------------------
// Expressions (precedence, highest to lowest: unary, multiplicative,
// additive, relational, AND, OR)
// ---------------------------------------------------------------------------

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
    : andExpr AND relExpr
    | relExpr
    ;

relExpr
    : addExpr ((EQ | NE | LT | GT | LE | GE) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH | DIV | MOD) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : (NOT | MINUS) unaryExpr
    | postfixExpr
    ;

postfixExpr
    : postfixExpr LBRACK expr RBRACK
    | postfixExpr DOT IDENTIFIER
    | postfixExpr CARET
    | factor
    ;

factor
    : LPAREN expr RPAREN
    | constant
    | IDENTIFIER LPAREN exprList? RPAREN
    | IDENTIFIER
    ;

constant
    : INT_LIT
    | STRING_LIT
    | CHAR_LIT
    | TRUE
    | FALSE
    | NIL
    ;
