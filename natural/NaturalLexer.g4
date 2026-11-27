/*
BSD License

NATURAL grammar for ANTLR4.
NATURAL is Software AG's fourth-generation programming language, long
used (together with the ADABAS database) for business application
development on mainframes and still documented at
https://documentation.softwareag.com/.

Software AG's documentation site returned HTTP 404 for every specific
page URL found via web search while writing this grammar (the search
results themselves confirmed the pages' titles and general content, but
the pages could not actually be fetched), so this grammar could not be
checked against the primary reference in detail. It targets NATURAL's
modern "structured mode" (as opposed to the older, line-numbered
"reporting mode"): a DEFINE DATA ... END-DEFINE block declaring
level-numbered, #-prefixed fields with a format letter and length in
parentheses (e.g. "1 #NAME (A20)"), followed by statements, each closed
by its own END-xxx keyword (END-IF, END-FOR, END-REPEAT, ...), with the
module as a whole terminated by a final END statement. See README.md
for details and caveats.
*/

lexer grammar NaturalLexer;

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

DEFINE        : 'DEFINE';
DATA          : 'DATA';
LOCAL         : 'LOCAL';
PARAMETER     : 'PARAMETER';
GLOBAL        : 'GLOBAL';
USING         : 'USING';
END_DEFINE    : 'END-DEFINE';
SUBROUTINE    : 'SUBROUTINE';
END_SUBROUTINE: 'END-SUBROUTINE';
IF            : 'IF';
THEN          : 'THEN';
ELSE          : 'ELSE';
END_IF        : 'END-IF';
FOR           : 'FOR';
TO            : 'TO';
STEP          : 'STEP';
END_FOR       : 'END-FOR';
REPEAT        : 'REPEAT';
END_REPEAT    : 'END-REPEAT';
DECIDE        : 'DECIDE';
ON            : 'ON';
OF            : 'OF';
FIRST         : 'FIRST';
EVERY         : 'EVERY';
VALUE         : 'VALUE';
VALUES        : 'VALUES';
NONE          : 'NONE';
END_DECIDE    : 'END-DECIDE';
PERFORM       : 'PERFORM';
MOVE          : 'MOVE';
COMPUTE       : 'COMPUTE';
ADD           : 'ADD';
SUBTRACT      : 'SUBTRACT';
MULTIPLY      : 'MULTIPLY';
DIVIDE        : 'DIVIDE';
GIVING        : 'GIVING';
BY            : 'BY';
FROM          : 'FROM';
WRITE         : 'WRITE';
DISPLAY       : 'DISPLAY';
INPUT         : 'INPUT';
ESCAPE        : 'ESCAPE';
BOTTOM        : 'BOTTOM';
TOP           : 'TOP';
ROUTINE       : 'ROUTINE';
IMMEDIATE     : 'IMMEDIATE';
STOP          : 'STOP';
FIND          : 'FIND';
END_FIND      : 'END-FIND';
READ          : 'READ';
END_READ      : 'END-READ';
NUMBER        : 'NUMBER';
WITH          : 'WITH';
AND           : 'AND';
OR            : 'OR';
NOT           : 'NOT';
EQ            : 'EQ';
NE            : 'NE';
LT            : 'LT';
GT            : 'GT';
LE            : 'LE';
GE            : 'GE';
TRUE_         : 'TRUE';
FALSE_        : 'FALSE';
END           : 'END';

// ---------------------------------------------------------------------------
// Identifiers (user-defined data names conventionally start with '#')
// ---------------------------------------------------------------------------

IDENTIFIER: '#'? [A-Za-z] [A-Za-z0-9_]*;

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

DEC_LIT: DIGIT+ '.' DIGIT+;
INT_LIT: DIGIT+;
fragment DIGIT: [0-9];

STRING_LIT: '\'' (~['\r\n])* '\'' | '"' (~["\r\n])* '"';

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN: '(';
RPAREN: ')';

NESIGN: '<>';
LESIGN: '<=';
GESIGN: '>=';
EQSIGN: '=';
LTSIGN: '<';
GTSIGN: '>';

PLUS : '+';
MINUS: '-';
STAR : '*';
SLASH: '/';

COMMA: ',';
DOT  : '.';
COLON: ':';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

// Classic NATURAL also treats a line whose first character is '*' as a
// full-line comment; that convention depends on column position, which
// this (position-independent) lexer does not track, so only the inline
// "/* ... " trailing-comment form is implemented here. See README.md.
LINE_COMMENT: '/*' ~[\r\n]* -> skip;

WS: [ \t\r\n\f]+ -> skip;
