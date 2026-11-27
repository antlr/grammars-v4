/*
BSD License

Euclid grammar for ANTLR4.
Euclid (https://en.wikipedia.org/wiki/Euclid_(programming_language)) is a
Pascal-family systems programming language designed in the mid-1970s at
the University of Toronto (Butler Lampson, James Mitchell, Jim Horning,
Ralph London, Gerald Popek; implemented by Ric Holt and James Cordy)
specifically to support formal verification of programs. Its defining
restrictions, confirmed by Wikipedia: routines are closed scopes with no
side effects, must explicitly declare their "imports" (and "exports"),
have no global assignment, no nested functions, no aliasing between
parameters, no gotos, and - notably - no floating-point numbers. It
descends from Pascal (among others) and is itself the ancestor of
Concurrent Euclid and Turing.

The primary language report (Lampson, Horning, London, Mitchell, Popek,
"Report on the Programming Language Euclid", SIGPLAN Notices 12(2), 1977)
is only available behind the ACM Digital Library paywall and no free
mirror could be found, so this grammar could not be checked against a
primary source. It is reconstructed from the well-documented
characteristics above together with ordinary Pascal-family syntax
(which Euclid is explicitly descended from). See README.md for details
and caveats.
*/

lexer grammar EuclidLexer;

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

MODULE   : 'module';
IMPORTS  : 'imports';
EXPORTS  : 'exports';
FROM     : 'from';
PERVASIVE: 'pervasive';
VAR      : 'var';
CONST    : 'const';
TYPE     : 'type';
PROCEDURE: 'procedure';
FUNCTION : 'function';
BEGIN_   : 'begin';
END_     : 'end';
IF       : 'if';
THEN     : 'then';
ELSE     : 'else';
LOOP     : 'loop';
EXIT     : 'exit';
WHEN     : 'when';
WHILE    : 'while';
FOR      : 'for';
DO       : 'do';
TO       : 'to';
DOWNTO   : 'downto';
CASE     : 'case';
OF       : 'of';
OTHERWISE: 'otherwise';
RESULT   : 'result';
RETURN   : 'return';
ASSERT   : 'assert';
ASSUME   : 'assume';
RECORD   : 'record';
POINTER  : 'pointer';
COLLECTION: 'collection';
NEW      : 'new';
FREE     : 'free';
AND      : 'and';
OR       : 'or';
NOT      : 'not';
DIV      : 'div';
MOD      : 'mod';
TRUE     : 'true';
FALSE    : 'false';
NIL      : 'nil';
ARRAY    : 'array';
INTEGER  : 'integer';
BOOLEAN  : 'boolean';
CHAR     : 'char';
STRING_T : 'string';

// ---------------------------------------------------------------------------
// Identifiers and literals (Euclid has no floating-point numbers)
// ---------------------------------------------------------------------------

IDENTIFIER: [a-zA-Z] [a-zA-Z0-9_]*;

INT_LIT: DIGIT+;
fragment DIGIT: [0-9];

STRING_LIT: '"' ('""' | ~["\r\n])* '"';
CHAR_LIT  : '\'' ('\'\'' | ~['\r\n]) '\'';

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';

ASSIGN: ':=';
DOTDOT: '..';
NE    : '<>';
LE    : '<=';
GE    : '>=';
EQ    : '=';
LT    : '<';
GT    : '>';

PLUS : '+';
MINUS: '-';
STAR : '*';
SLASH: '/';
CARET: '^';

COLON: ':';
SEMI : ';';
COMMA: ',';
DOT  : '.';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

LINE_COMMENT: '%' ~[\r\n]* -> skip;

WS: [ \t\r\n\f]+ -> skip;
