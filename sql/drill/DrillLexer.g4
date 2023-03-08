/*
Apache Drill grammar.
The MIT License (MIT).

Copyright (c) 2023, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

lexer grammar DrillLexer;

options { caseInsensitive = true; }

ALL: 'ALL';
ALTER: 'ALTER';
AS: 'AS';
ASC: 'ASC';
ASSIGN: 'ASSIGN';
BIGINT: 'BIGINT';
BINARY: 'BINARY';
BOOLEAN: 'BOOLEAN';
BY: 'BY';
CHAR: 'CHAR';
CHARACTER: 'CHARACTER';
COLUMNS: 'COLUMNS';
CREATE: 'CREATE';
DATABASE: 'DATABASE';
DATABASES: 'DATABASES';
DATE: 'DATE';
DEC: 'DEC';
DECIMAL: 'DECIMAL';
DISTINCT: 'DISTINCT';
DESCRIBE: 'DESCRIBE';
DESC: 'DESC';
DOUBLE: 'DOUBLE';
DROP: 'DROP';
EXISTS: 'EXISTS';
FETCH: 'FETCH';
FILES: 'FILES';
FIRST: 'FIRST';
FLOAT: 'FLOAT';
FOR: 'FOR';
FROM: 'FROM';
FUNCTION: 'FUNCTION';
GROUP: 'GROUP';
HAVING: 'HAVING';
IF: 'IF';
IN: 'IN';
INT: 'INT';
INTEGER: 'INTEGER';
INTERVAL: 'INTERVAL';
JAR: 'JAR';
JOIN: 'JOIN';
LAST: 'LAST';
LIMIT: 'LIMIT';
LOAD: 'LOAD';
METADATA: 'METADATA';
NEXT: 'NEXT';
NONE: 'NONE';
NULLS: 'NULLS';
NULL_: 'NULL';
NUMERIC: 'NUMERIC';
OFFSET: 'OFFSET';
OR: 'OR';
ORDER: 'ORDER';
OVER: 'OVER';
PARTITION: 'PARTITION';
PATH: 'PATH';
PRECISION: 'PRECISION';
PROPERTIES: 'PROPERTIES';
REFRESH: 'REFRESH';
REPLACE: 'REPLACE';
RESET: 'RESET';
ROW: 'ROW';
ROWS: 'ROWS';
SCHEMA: 'SCHEMA';
SCHEMAS: 'SCHEMAS';
SELECT: 'SELECT';
SESSION: 'SESSION';
SET: 'SET';
SHOW: 'SHOW';
SMALLINT: 'SMALLINT';
SYSTEM: 'SYSTEM';
TABLE: 'TABLE';
TABLES: 'TABLES';
TEMPORARY: 'TEMPORARY';
TIME: 'TIME';
TIMESTAMP: 'TIMESTAMP';
USE: 'USE';
USING: 'USING';
VARCHAR: 'VARCHAR';
VARYING: 'VARYING';
VIEW: 'VIEW';
WHERE: 'WHERE';
WITH: 'WITH';


WHITE_SPACE:             [ \t\r\n]+                   -> channel(HIDDEN);

SQL_COMMENT:            '/*' (SQL_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT:           '--' ~[\r\n]*                 -> channel(HIDDEN);


SINGLE_QUOTE:           '\'';

ID:                     [A-Z_] [A-Z0-9_]*;

BS_STRING_LITERAL:           '`'  ~'`'+ '`';
SQ_STRING_LITERAL:           '\'' (~'\'' | '\'\'')* '\'';
DQ_STRING_LITERAL:           '"' ~'"'+ '"';

DECIMAL_LITERAL:             DEC_DIGIT+;
FLOAT_LITERAL:               DEC_DOT_DEC;
REAL_LITERAL:                (DECIMAL_LITERAL | DEC_DOT_DEC) ('E' [+-]? DEC_DIGIT+);
CHAR_LITERAL:                '\'' (~['\\\r\n] | EscapeSequence) '\'';

fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

fragment HexDigit
    : [0-9a-f]
    ;

//ARROW:               '->';
//ASSOC:               '=>';

NE:                  '!=';
LTGT:                '<>';
EQ:                  '=';
GT:                  '>';
GE:                  '>=';
LT:                  '<';
LE:                  '<=';
EXCLAMATION:         '!';
PIPE_PIPE:           '||';
DOT:                 '.';
UNDERLINE:           '_';
LRB:                 '(';
RRB:                 ')';
LSB:                 '[';
RSB:                 ']';
COMMA:               ',';
SEMI:                ';';
STAR:                '*';
DIVIDE:              '/';
MODULE:              '%';
PLUS:                '+';
MINUS:               '-';

PLACEHOLDER:         '?';

fragment LETTER:       [A-Z_];
fragment DEC_DOT_DEC:  (DEC_DIGIT+ '.' DEC_DIGIT+ |  DEC_DIGIT+ '.' | '.' DEC_DIGIT+);
fragment DEC_DIGIT:    [0-9];
