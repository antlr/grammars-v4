lexer grammar SimscriptLexer;

// SIMSCRIPT II.5 is a general-purpose, English-like discrete-event
// simulation language originally developed at RAND Corporation (the
// original SIMSCRIPT, 1963) and substantially extended by CACI as
// SIMSCRIPT II / SIMSCRIPT II.5 in the late 1960s/1970s. Programs declare
// a world of entities, attributes and sets in a PREAMBLE, and define
// behavior with EVENT/PROCESS routines scheduled on a simulated clock.
//
// SIMSCRIPT II.5's full grammar is a large, highly flexible
// "English-like" language that allows many interchangeable word orderings
// and optional filler words (e.g. several equivalent ways to phrase a FOR
// EACH loop or a PRINT statement). This grammar models one fixed,
// unambiguous word order for each construct rather than the full set of
// free-form phrasings -- see README.md for the complete list of scope
// decisions.

PREAMBLE: 'PREAMBLE';
MAIN: 'MAIN';
END: 'END';
EVERY: 'EVERY';
HAS: 'HAS';
A: 'A';
AN: 'AN';
DEFINE: 'DEFINE';
AS: 'AS';
INTEGER: 'INTEGER';
REAL: 'REAL';
TEXT: 'TEXT';
ALPHA: 'ALPHA';
VARIABLE: 'VARIABLE';
SETS: 'SETS';
OWNED: 'OWNED';
BY: 'BY';
PROCESSES: 'PROCESSES';
EVENT: 'EVENT';
NOTICES: 'NOTICES';
INCLUDE: 'INCLUDE';
ROUTINE: 'ROUTINE';
PROCESS: 'PROCESS';
TO: 'TO';
GIVEN: 'GIVEN';
RETURN: 'RETURN';
LET: 'LET';
ADD: 'ADD';
SUBTRACT: 'SUBTRACT';
FROM: 'FROM';
IF: 'IF';
ELSE: 'ELSE';
ALWAYS: 'ALWAYS';
FOR: 'FOR';
EACH: 'EACH';
IN: 'IN';
WITH: 'WITH';
DO: 'DO';
LOOP: 'LOOP';
WHILE: 'WHILE';
SCHEDULE: 'SCHEDULE';
UNITS: 'UNITS';
AT: 'AT';
CAUSE: 'CAUSE';
CALL: 'CALL';
CREATE: 'CREATE';
DESTROY: 'DESTROY';
CALLED: 'CALLED';
PRINT: 'PRINT';
STOP: 'STOP';
WORK: 'WORK';
AND: 'AND';
OR: 'OR';
NOT: 'NOT';
MOD: 'MOD';

IDENTIFIER: [A-Za-z_] [A-Za-z0-9_]*;

REAL_LIT: DIGIT+ '.' DIGIT+;
INT_LIT: DIGIT+;
fragment DIGIT: [0-9];

STRING_LIT: '"' (~["\r\n])* '"';

ASSIGN: '=';
NE: '<>';
LE: '<=';
GE: '>=';
LT: '<';
GT: '>';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
LPAREN: '(';
RPAREN: ')';
COMMA: ',';
DOT: '.';
COLON: ':';

LINE_COMMENT: '!!' ~[\r\n]* -> skip;
WS: [ \t\r\n\f]+ -> skip;
