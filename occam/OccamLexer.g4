/*
BSD License

occam grammar for ANTLR4.
occam (https://en.wikipedia.org/wiki/Occam_(programming_language)) is a
concurrent programming language based on Tony Hoare's Communicating
Sequential Processes (CSP), designed by INMOS for its transputer
microprocessors in the early 1980s.

Like Python, occam uses significant indentation (strictly 2 spaces per
level in real occam, though this grammar - like the other indentation-
sensitive grammars in this project - tracks indentation generically
rather than enforcing an exact column width) instead of braces or BEGIN/
END keywords to delimit the bodies of SEQ, PAR, ALT, IF, WHILE, CASE, and
PROC/FUNCTION. This grammar follows the same INDENT/DEDENT technique used
by python/python3, gdscript, and lobster elsewhere in this project: see
Java/OccamLexerBase.java and CSharp/OccamLexerBase.cs.
*/

lexer grammar OccamLexer;

tokens {
    INDENT,
    DEDENT
}

options {
    superClass = OccamLexerBase;
}

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

SEQ      : 'SEQ';
PAR      : 'PAR';
PRI      : 'PRI';
ALT      : 'ALT';
IF       : 'IF';
WHILE    : 'WHILE';
CASE     : 'CASE';
ELSE     : 'ELSE';
PROC     : 'PROC';
FUNCTION : 'FUNCTION';
IS       : 'IS';
VAL      : 'VAL';
VALOF    : 'VALOF';
RESULT   : 'RESULT';
SKIP_    : 'SKIP';
STOP     : 'STOP';
TRUE     : 'TRUE';
FALSE    : 'FALSE';
AND      : 'AND';
OR       : 'OR';
NOT      : 'NOT';
BITAND   : 'BITAND';
BITOR    : 'BITOR';
BITNOT   : 'BITNOT';
FOR      : 'FOR';
CHAN     : 'CHAN';
OF       : 'OF';
PROTOCOL : 'PROTOCOL';
DATA     : 'DATA';
TYPE     : 'TYPE';
RECORD   : 'RECORD';
TIMER    : 'TIMER';
AFTER    : 'AFTER';
REM      : 'REM';
SIZE     : 'SIZE';
RETYPES  : 'RETYPES';
RESHAPES : 'RESHAPES';
PLACE    : 'PLACE';
AT       : 'AT';
PORT     : 'PORT';
INT16    : 'INT16';
INT32    : 'INT32';
INT64    : 'INT64';
INT      : 'INT';
REAL32   : 'REAL32';
REAL64   : 'REAL64';
BOOL     : 'BOOL';
BYTE     : 'BYTE';

// ---------------------------------------------------------------------------
// Line breaks / indentation (handled by OccamLexerBase)
// ---------------------------------------------------------------------------

NEWLINE: ({atStartOfInput()}? SPACE | ('\r'? '\n' | '\r' | '\f') SPACE?) {onNewLine();};

// ---------------------------------------------------------------------------
// Identifiers (occam allows '.' inside a name, e.g. "my.chan")
// ---------------------------------------------------------------------------

IDENTIFIER: [a-zA-Z] [a-zA-Z0-9.]*;

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

REAL_LIT: DIGIT+ '.' DIGIT+ ([eE] [+\-]? DIGIT+)?;
INT_LIT : DIGIT+;
HEX_LIT : '#' HEXD+;
fragment DIGIT: [0-9];
fragment HEXD : [0-9a-fA-F];

// occam uses '*' (not '\') as the escape character in strings and chars:
// *n newline, *c return/tab-like controls, *" literal quote, ** literal
// star, *#hh a two hex-digit character code.
STRING_LIT: '"' (ESC_SEQ | ~["*\r\n])* '"';
CHAR_LIT  : '\'' (ESC_SEQ | ~['*\r\n]) '\'';
fragment ESC_SEQ: '*' ([a-zA-Z'"*] | '#' HEXD HEXD);

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN: '(' {openBrace();};
RPAREN: ')' {closeBrace();};
LBRACK: '[' {openBrace();};
RBRACK: ']' {closeBrace();};

ASSIGN: ':=';
COLON : ':';
SEMI  : ';';
COMMA : ',';

NE: '<>';
LE: '<=';
GE: '>=';
EQ: '=';
LT: '<';
GT: '>';

QUESTION : '?';
BANG     : '!';
AMP      : '&';
PLUS     : '+';
MINUS    : '-';
STAR     : '*';
SLASH    : '/';
BACKSLASH: '\\';
TILDE    : '~';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

LINE_COMMENT: '--' ~[\r\n]* -> skip;

WS: SPACE -> skip;

fragment SPACE: [ \t]+;
