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

ALL               : 'ALL';
ALTER             : 'ALTER';
ANALYZE           : 'ANALYZE';
AND               : 'AND';
ANY               : 'ANY';
AS                : 'AS';
ASC               : 'ASC';
ASSIGN            : 'ASSIGN';
BETWEEN           : 'BETWEEN';
BIGINT            : 'BIGINT';
BINARY            : 'BINARY';
BOOLEAN           : 'BOOLEAN';
BY                : 'BY';
CAST              : 'CAST';
CHAR              : 'CHAR';
CHARACTER         : 'CHARACTER';
COLUMNS           : 'COLUMNS';
COMPUTE           : 'COMPUTE';
CREATE            : 'CREATE';
CROSS             : 'CROSS';
DATABASE          : 'DATABASE';
DATABASES         : 'DATABASES';
DATE              : 'DATE';
DAY               : 'DAY';
DEC               : 'DEC';
DECIMAL           : 'DECIMAL';
DEFAULT           : 'DEFAULT';
DESC              : 'DESC';
DESCRIBE          : 'DESCRIBE';
DISTINCT          : 'DISTINCT';
DOUBLE            : 'DOUBLE';
DROP              : 'DROP';
ESCAPE            : 'ESCAPE';
ESTIMATE          : 'ESTIMATE';
EXISTS            : 'EXISTS';
FALSE             : 'FALSE';
FETCH             : 'FETCH';
FILES             : 'FILES';
FIRST             : 'FIRST';
FLOAT             : 'FLOAT';
FOR               : 'FOR';
FORMAT            : 'FORMAT';
FROM              : 'FROM';
FULL              : 'FULL';
FUNCTION          : 'FUNCTION';
GROUP             : 'GROUP';
HAVING            : 'HAVING';
HOUR              : 'HOUR';
IF                : 'IF';
IN                : 'IN';
INNER             : 'INNER';
INT               : 'INT';
INTEGER           : 'INTEGER';
INTERVAL          : 'INTERVAL';
IS                : 'IS';
JAR               : 'JAR';
JOIN              : 'JOIN';
LAST              : 'LAST';
LATERAL           : 'LATERAL';
LEFT              : 'LEFT';
LEVEL             : 'LEVEL';
LIKE              : 'LIKE';
LIMIT             : 'LIMIT';
LOAD              : 'LOAD';
METADATA          : 'METADATA';
MINUTE            : 'MINUTE';
MONTH             : 'MONTH';
NATURAL           : 'NATURAL';
NEXT              : 'NEXT';
NONE              : 'NONE';
NOT               : 'NOT';
NULL_             : 'NULL';
NULLS             : 'NULLS';
NUMERIC           : 'NUMERIC';
OFFSET            : 'OFFSET';
ON                : 'ON';
OR                : 'OR';
ORDER             : 'ORDER';
OUTER             : 'OUTER';
OVER              : 'OVER';
PARTITION         : 'PARTITION';
PATH              : 'PATH';
PERCENT           : 'PERCENT';
PRECISION         : 'PRECISION';
PROPERTIES        : 'PROPERTIES';
REFRESH           : 'REFRESH';
REPLACE           : 'REPLACE';
RESET             : 'RESET';
RIGHT             : 'RIGHT';
ROW               : 'ROW';
ROWS              : 'ROWS';
SAMPLE            : 'SAMPLE';
SCHEMA            : 'SCHEMA';
SCHEMAS           : 'SCHEMAS';
SECOND            : 'SECOND';
SELECT            : 'SELECT';
SESSION           : 'SESSION';
SET               : 'SET';
SHOW              : 'SHOW';
SMALLINT          : 'SMALLINT';
SOME              : 'SOME';
STATISTICS        : 'STATISTICS';
SYSTEM            : 'SYSTEM';
TABLE             : 'TABLE';
TABLES            : 'TABLES';
TEMPORARY         : 'TEMPORARY';
TIME              : 'TIME';
TIMESTAMP         : 'TIMESTAMP';
TRUE              : 'TRUE';
UNION             : 'UNION';
UNNEST            : 'UNNEST';
USE               : 'USE';
USING             : 'USING';
VARCHAR           : 'VARCHAR';
VARYING           : 'VARYING';
VIEW              : 'VIEW';
WHERE             : 'WHERE';
WITH              : 'WITH';
YEAR              : 'YEAR';

WHITE_SPACE       : [ \t\r\n]+                   -> channel(HIDDEN);

SQL_COMMENT       : '/*' (SQL_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT      : '--' ~[\r\n]*                 -> channel(HIDDEN);

IDENTIFIER        : [A-Z_] [A-Z0-9_]*;

BS_STRING_LITERAL : '`'  ~'`'+ '`';
SQ_STRING_LITERAL : '\'' ~'\''+ '\'';
DQ_STRING_LITERAL : '"' ~'"'+ '"';

DECIMAL_LITERAL   : DEC_DIGIT+;
FLOAT_LITERAL     : DEC_DOT_DEC;
REAL_LITERAL      : (DECIMAL_LITERAL | DEC_DOT_DEC) 'E' [+-]? DEC_DIGIT+;
CHAR_LITERAL      : '\'' ~['\\\r\n] '\'';

NE                : '!=';
LTGT              : '<>';
EQ                : '=';
GT                : '>';
GE                : '>=';
LT                : '<';
LE                : '<=';
EXCLAMATION       : '!';
PIPE_PIPE         : '||';
DOT               : '.';
UNDERLINE         : '_';
LRB               : '(';
RRB               : ')';
LSB               : '[';
RSB               : ']';
LCB               : '{';
RCB               : '}';
COMMA             : ',';
SEMI              : ';';
STAR              : '*';
DIVIDE            : '/';
MODULE            : '%';
PLUS              : '+';
MINUS             : '-';

fragment LETTER      : [A-Z_];
fragment DEC_DOT_DEC : DEC_DIGIT+ '.' DEC_DIGIT+ |  DEC_DIGIT+ '.' | '.' DEC_DIGIT+;
fragment DEC_DIGIT   : [0-9];
