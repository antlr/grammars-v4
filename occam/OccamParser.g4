/*
BSD License

Parser grammar for occam. See OccamLexer.g4 for background.

occam has no formal, single published grammar as readily available as
some other languages', so this grammar is built from well attested occam
2 characteristics: SEQ/PAR/ALT/IF/WHILE/CASE constructor processes;
PROC/FUNCTION declarations; channel input ('?') and output ('!');
VALOF/RESULT expressions; and declarations that scope over "the rest of
the program" (a declaration is followed by one more process, which is
everything that uses it). As with the other indentation-sensitive
grammars in this project, each compound construct's body is exactly one
process - a SEQ is used explicitly whenever more than one statement is
needed in sequence, which is also how real occam programs are written.

Entry rule: program
*/

parser grammar OccamParser;

options {
    tokenVocab = OccamLexer;
}

program
    : NEWLINE? process EOF
    ;

// ---------------------------------------------------------------------------
// Processes
// ---------------------------------------------------------------------------

process
    : simpleProcess NEWLINE
    | compoundProcess
    ;

simpleProcess
    : SKIP_
    | STOP
    | assignment
    | inputProcess
    | outputProcess
    | procInstance
    ;

compoundProcess
    : seqProcess
    | parProcess
    | altProcess
    | ifProcess
    | whileProcess
    | caseProcess
    | declaration
    ;

assignment
    : variable (COMMA variable)* ASSIGN exprList
    ;

variable
    : IDENTIFIER (LBRACK expr RBRACK)*
    ;

inputProcess
    : variable QUESTION variable (SEMI variable)*
    ;

outputProcess
    : variable BANG expr (SEMI expr)*
    ;

procInstance
    : IDENTIFIER LPAREN exprList? RPAREN
    ;

seqProcess
    : SEQ replicator? NEWLINE INDENT process+ DEDENT
    ;

parProcess
    : PRI? PAR replicator? NEWLINE INDENT process+ DEDENT
    ;

altProcess
    : PRI? ALT replicator? NEWLINE INDENT altGuard+ DEDENT
    ;

altGuard
    : altGuardHead NEWLINE INDENT process DEDENT
    ;

altGuardHead
    : (expr AMP)? variable QUESTION variable
    ;

ifProcess
    : IF replicator? NEWLINE INDENT ifGuard+ DEDENT
    ;

ifGuard
    : expr NEWLINE INDENT process DEDENT
    ;

whileProcess
    : WHILE expr NEWLINE INDENT process DEDENT
    ;

caseProcess
    : CASE expr NEWLINE INDENT caseAlt+ elseAlt? DEDENT
    ;

caseAlt
    : exprList NEWLINE INDENT process DEDENT
    ;

elseAlt
    : ELSE NEWLINE INDENT process DEDENT
    ;

replicator
    : IDENTIFIER EQ expr FOR expr
    ;

valofProcess
    : VALOF NEWLINE INDENT process RESULT exprList NEWLINE DEDENT
    ;

// ---------------------------------------------------------------------------
// Declarations - each scopes over the process that textually follows it.
// ---------------------------------------------------------------------------

declaration
    : declHead NEWLINE process
    ;

declHead
    : varDeclHead
    | chanDeclHead
    | timerDeclHead
    | portDeclHead
    | abbrevDeclHead
    | protocolDeclHead
    | dataTypeDeclHead
    | procDeclHead
    | funcDeclHead
    ;

varDeclHead
    : type identList COLON
    ;

chanDeclHead
    : CHAN OF type identList COLON
    ;

timerDeclHead
    : TIMER identList COLON
    ;

portDeclHead
    : PORT OF type identList COLON
    ;

abbrevDeclHead
    : VAL? type IDENTIFIER IS expr COLON
    | VAL? CHAN OF type IDENTIFIER IS expr COLON
    ;

protocolDeclHead
    : PROTOCOL IDENTIFIER IS type (SEMI type)* COLON
    ;

dataTypeDeclHead
    : DATA TYPE IDENTIFIER IS type COLON
    ;

procDeclHead
    : PROC IDENTIFIER LPAREN formalList? RPAREN NEWLINE INDENT process DEDENT COLON
    ;

funcDeclHead
    : (type (COMMA type)* | LPAREN type (COMMA type)* RPAREN) FUNCTION IDENTIFIER LPAREN formalList? RPAREN NEWLINE INDENT funcBody DEDENT COLON
    ;

// A FUNCTION body is zero or more local declarations followed by the
// VALOF that produces its result.
funcBody
    : declHead NEWLINE funcBody
    | valofProcess
    ;

identList
    : IDENTIFIER (COMMA IDENTIFIER)*
    ;

formalList
    : formal (COMMA formal)*
    ;

formal
    : VAL? type identList
    | CHAN OF type identList
    | TIMER identList
    | PORT OF type identList
    ;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

type
    : LBRACK expr? RBRACK type
    | BOOL
    | BYTE
    | INT
    | INT16
    | INT32
    | INT64
    | REAL32
    | REAL64
    | IDENTIFIER
    ;

// ---------------------------------------------------------------------------
// Expressions (precedence, highest to lowest: unary, SIZE, index/call,
// multiplicative, additive, relational, NOT, AND, OR)
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
    : andExpr AND notExpr
    | notExpr
    ;

notExpr
    : NOT notExpr
    | relExpr
    ;

relExpr
    : addExpr ((EQ | NE | LT | GT | LE | GE | AFTER) addExpr)?
    ;

addExpr
    : addExpr (PLUS | MINUS | BITAND | BITOR) mulExpr
    | mulExpr
    ;

mulExpr
    : mulExpr (STAR | SLASH | BACKSLASH | REM) unaryExpr
    | unaryExpr
    ;

unaryExpr
    : (MINUS | BITNOT | TILDE) unaryExpr
    | sizeExpr
    ;

sizeExpr
    : SIZE LPAREN (expr | type) RPAREN
    | postfixExpr
    ;

postfixExpr
    : postfixExpr LBRACK expr RBRACK
    | factor
    ;

factor
    : LPAREN expr RPAREN
    | constant
    | valofProcess
    | type LPAREN expr RPAREN
    | IDENTIFIER LPAREN exprList? RPAREN
    | IDENTIFIER
    ;

constant
    : INT_LIT
    | HEX_LIT
    | REAL_LIT
    | STRING_LIT
    | CHAR_LIT
    | TRUE
    | FALSE
    ;
