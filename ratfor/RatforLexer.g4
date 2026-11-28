lexer grammar RatforLexer;

// Ratfor ("Rational Fortran"), designed by Brian Kernighan, is a
// preprocessor language that compiles down to Fortran 66/77 source, adding
// C-like structured control flow (if/else, while, for, do, repeat/until,
// break, next) and free-form syntax on top of plain Fortran statements.
//
// This grammar models Ratfor's control-flow and statement syntax. It does
// not attempt to parse the full grammar of every possible embedded Fortran
// statement (e.g. FORMAT strings, arbitrary I/O edit descriptors, COMMON /
// EQUIVALENCE layout rules) -- it supports the statement forms that appear
// in idiomatic Ratfor programs: assignment, CALL, RETURN, PRINT/READ/WRITE,
// simple type declarations (INTEGER/REAL/DOUBLE PRECISION/LOGICAL/CHARACTER
// with optional array dimensions), SUBROUTINE/FUNCTION/END, and the Ratfor
// control structures themselves. See README.md for the full list of
// deliberate simplifications.

IF: 'if';
ELSE: 'else';
WHILE: 'while';
FOR: 'for';
DO: 'do';
REPEAT: 'repeat';
UNTIL: 'until';
BREAK: 'break';
NEXT: 'next';
RETURN: 'return';
CALL: 'call';
SUBROUTINE: 'subroutine';
FUNCTION: 'function';
END: 'end';
STOP: 'stop';
PRINT: 'print';
READ: 'read';
WRITE: 'write';
DIMENSION: 'dimension';
COMMON: 'common';
INTEGER: 'integer';
REAL: 'real';
DOUBLEPRECISION: 'double precision';
LOGICAL: 'logical';
CHARACTER: 'character';
TRUE_: '.true.';
FALSE_: '.false.';

IDENTIFIER: [A-Za-z] [A-Za-z0-9_]*;

REAL_LIT: DIGIT+ '.' DIGIT* EXPONENT? | '.' DIGIT+ EXPONENT? | DIGIT+ EXPONENT;
INT_LIT: DIGIT+;
fragment DIGIT: [0-9];
fragment EXPONENT: [eEdD] [+-]? DIGIT+;

STRING_LIT: '\'' (~['\r\n])* '\'' | '"' (~["\r\n])* '"';

STAR2: '**';
EQEQ: '==';
NE: '!=' | '<>';
LE: '<=';
GE: '>=';
ANDAND: '&&';
OROR: '||';

LPAREN: '(';
RPAREN: ')';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
EQSIGN: '=';
LT: '<';
GT: '>';
AMP: '&';
PIPE: '|';
NOT: '!';
COMMA: ',';
SEMI: ';';
COLON: ':';
LBRACE: '{';
RBRACE: '}';

LINE_COMMENT: '#' ~[\r\n]* -> skip;
WS: [ \t\r\n\f]+ -> skip;
CONTINUATION: '\\' '\r'? '\n' -> skip;
