/*
BSD License

JOVIAL grammar for ANTLR4.
JOVIAL ("Jules' Own Version of the International Algorithmic Language") is
an ALGOL-58-derived, block-structured systems/embedded programming language
designed at System Development Corporation by Jules Schwartz starting in
1959, long used in US military and aerospace software, and standardized as
MIL-STD-1589 (revised as MIL-STD-1589C, "J73").

The authoritative MIL-STD-1589C specification is an image-only (not
text-searchable) PDF and a cited DTIC copy of the J73 language manual
returned HTTP 403, so this grammar was written from the single verbatim
example in the Wikipedia article (https://en.wikipedia.org/wiki/JOVIAL,
reproducing the J73 manual's PROC RETRIEVE example) plus well known,
widely cited JOVIAL characteristics (BEGIN/END blocks, ITEM/TABLE/DEF/
CONSTANT/STATUS declarations, PROC procedures, the ": initial BY step
WHILE condition" FOR-loop form). See README.md for what is and is not
verified against a primary source.
*/

lexer grammar JovialLexer;

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

COMPOOL : 'COMPOOL';
PROGRAM : 'PROGRAM';
MODULE  : 'MODULE';
START   : 'START';
TERM    : 'TERM';
PROC    : 'PROC';
BEGIN_  : 'BEGIN';
END_    : 'END';
ITEM    : 'ITEM';
TABLE   : 'TABLE';
DEF     : 'DEF';
CONSTANT: 'CONSTANT';
TYPE    : 'TYPE';
STATUS  : 'STATUS';
IF      : 'IF';
ELSE    : 'ELSE';
FOR     : 'FOR';
BY      : 'BY';
WHILE   : 'WHILE';
CASE    : 'CASE';
GOTO    : 'GOTO';
CALL    : 'CALL';
RETURN  : 'RETURN';
EXIT    : 'EXIT';
AND     : 'AND';
OR      : 'OR';
NOT     : 'NOT';
MOD     : 'MOD';

// ---------------------------------------------------------------------------
// Identifiers and literals
// ---------------------------------------------------------------------------

IDENTIFIER: [A-Za-z] [A-Za-z0-9_]*;

FLOAT_LIT: DEC+ '.' DEC* ([eE] [+\-]? DEC+)?
         | '.' DEC+ ([eE] [+\-]? DEC+)?
         ;
INT_LIT  : DEC+;
fragment DEC: [0-9];

STRING_LIT: '\'' (~['\r\n])* '\'';

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN: '(';
RPAREN: ')';

DEFEQ: '==';
NEQ  : '~=';
LE   : '<=';
GE   : '>=';
EQ   : '=';
LT   : '<';
GT   : '>';

PLUS : '+';
MINUS: '-';
STAR : '*';
SLASH: '/';

COLON: ':';
SEMI : ';';
COMMA: ',';
DOT  : '.';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

// JOVIAL comments are delimited by double quotes, e.g. "this is a comment".
COMMENT: '"' (~["\r\n])* '"' -> skip;

WS: [ \t\r\n]+ -> skip;
