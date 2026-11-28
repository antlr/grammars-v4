// SPDX-License-Identifier: MIT
// Copyright (c) 2026 AJGB (ajgb.eu)
//
// Minimal BASIC — ECMA-55, 1st edition (January 1978) / ANSI X3.60-1978.
// Section references (§x.y) point to ECMA-55.




lexer grammar MinimalBasicLexer;

options {
    caseInsensitive = true;
}

// §19 — the rest of the line belongs to the remark
REMARK: 'REM' REMARK_CHAR*;

// §17 — data items are lexed in their own mode so unquoted strings keep inner spaces
DATA: 'DATA' -> pushMode(DATA_MODE);

// §12 — GO space* TO / GO space* SUB
GOTO: 'GO' ' '* 'TO';
GOSUB: 'GO' ' '* 'SUB';

BASE: 'BASE';
DEF: 'DEF';
DIM: 'DIM';
END: 'END';
FOR: 'FOR';
IF: 'IF';
INPUT: 'INPUT';
LET: 'LET';
NEXT: 'NEXT';
ON: 'ON';
OPTION: 'OPTION';
PRINT: 'PRINT';
RANDOMIZE: 'RANDOMIZE';
READ: 'READ';
RESTORE: 'RESTORE';
RETURN: 'RETURN';
STEP: 'STEP';
STOP: 'STOP';
TAB: 'TAB';
THEN: 'THEN';
TO: 'TO';

// §9 — implementation-supplied functions
ABS: 'ABS';
ATN: 'ATN';
COS: 'COS';
EXP: 'EXP';
INT: 'INT';
LOG: 'LOG';
RND: 'RND';
SGN: 'SGN';
SIN: 'SIN';
SQR: 'SQR';
TAN: 'TAN';

// §10 — FN letter
FN_NAME: 'FN' [A-Z];

// §7 — variables
STRING_NAME: [A-Z] '$';
LETTER_DIGIT_NAME: [A-Z] [0-9];
LETTER_NAME: [A-Z];

// §6 — numeric-rep without sign (the sign belongs to the expression, §8)
INTEGER: [0-9]+;
REAL: ( [0-9]+ '.' [0-9]* | '.' [0-9]+) EXRAD? | [0-9]+ EXRAD;
fragment EXRAD: 'E' [+-]? [0-9]+;

// §6 — string-constant = quoted-string
STRING: '"' QUOTED_CHAR* '"';
// Catches quoted strings with characters outside §4.2 or without a closing quote, so the
// error points at the whole string. No parser rule uses it.
BAD_STRING: '"' ~["\r\n]* '"'?;

NE: '<>';
GE: '>=';
LE: '<=';
EQ: '=';
LT: '<';
GT: '>';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
CARET: '^';
LPAREN: '(';
RPAREN: ')';
COMMA: ',';
SEMICOLON: ';';

NEWLINE: '\r\n' | '\r' | '\n';
// §5.4 — spaces are insignificant except where the validator checks them
WS: ' '+ -> channel(HIDDEN);
// Anything else is not a Minimal BASIC character (§4)
ERROR_CHAR: .;

// §4.2 — quoted-string-character: any string-character except the quotation mark
fragment QUOTED_CHAR: [!#$%&'()*,/:;<=>?^_ ] | PLAIN_CHAR;
fragment REMARK_CHAR: QUOTED_CHAR | '"';
// §4.2 — plain-string-character
fragment PLAIN_CHAR: [+\-.0-9A-Z];

mode DATA_MODE;
DATA_WS: ' '+ -> channel(HIDDEN);
DATA_COMMA: ',';
DATA_QUOTED: '"' QUOTED_CHAR* '"';
DATA_BAD_QUOTED: '"' ~["\r\n]* '"'? -> type(BAD_STRING);
// §4.2 — unquoted-string: no leading or trailing spaces
DATA_UNQUOTED: PLAIN_CHAR ( ( PLAIN_CHAR | ' ')* PLAIN_CHAR)?;
DATA_NEWLINE: ('\r\n' | '\r' | '\n') -> type(NEWLINE), popMode
;
DATA_ERROR: . -> type(ERROR_CHAR);