/*
Apache Derby grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

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

lexer grammar DerbyLexer;

options { caseInsensitive = true; }

ABS:                                                   'ABS';
ABSVAL:                                                'ABSVAL';
ACOS:                                                  'ACOS';
ACTION:                                                'ACTION';
ADD:                                                   'ADD';
AGGREGATE:                                             'AGGREGATE';
ALL:                                                   'ALL';
ALLOCATE:                                              'ALLOCATE';
ALTER:                                                 'ALTER';
ALWAYS:                                                'ALWAYS';
AND:                                                   'AND';
ANY:                                                   'ANY';
ARE:                                                   'ARE';
AS:                                                    'AS';
ASC:                                                   'ASC';
ASIN:                                                  'ASIN';
ASSERTION:                                             'ASSERTION';
AT:                                                    'AT';
ATAN2:                                                 'ATAN2';
ATAN:                                                  'ATAN';
AUTHORIZATION:                                         'AUTHORIZATION';
AVG:                                                   'AVG';
BEGIN:                                                 'BEGIN';
BETWEEN:                                               'BETWEEN';
BIGINT:                                                'BIGINT';
BINARY:                                                'BINARY';
BIT:                                                   'BIT';
BLOB:                                                  'BLOB';
BOOLEAN:                                               'BOOLEAN';
BOTH:                                                  'BOTH';
BY:                                                    'BY';
CALL:                                                  'CALL';
CALLED:                                                'CALLED';
CASCADE:                                               'CASCADE';
CASCADED:                                              'CASCADED';
CASE:                                                  'CASE';
CAST:                                                  'CAST';
CEIL:                                                  'CEIL';
CEILING:                                               'CEILING';
CHAR:                                                  'CHAR';
CHARACTER:                                             'CHARACTER';
CHARACTER_LENGTH:                                      'CHARACTER_LENGTH';
CHECK:                                                 'CHECK';
CLOB:                                                  'CLOB';
CLOSE:                                                 'CLOSE';
COALESCE:                                              'COALESCE';
COLLATE:                                               'COLLATE';
COLLATION:                                             'COLLATION';
COLUMN:                                                'COLUMN';
COMMIT:                                                'COMMIT';
COMMITTED:                                             'COMMITTED';
CONNECT:                                               'CONNECT';
CONNECTION:                                            'CONNECTION';
CONSTRAINT:                                            'CONSTRAINT';
CONSTRAINTS:                                           'CONSTRAINTS';
CONTAINS:                                              'CONTAINS';
CONTINUE:                                              'CONTINUE';
CONVERT:                                               'CONVERT';
CORRESPONDING:                                         'CORRESPONDING';
COS:                                                   'COS';
COUNT:                                                 'COUNT';
CREATE:                                                'CREATE';
CROSS:                                                 'CROSS';
CS:                                                    'CS';
CURRENT:                                               'CURRENT';
CURRENT_DATE:                                          'CURRENT_DATE';
CURRENT_ROLE:                                          'CURRENT_ROLE';
CURRENT_TIME:                                          'CURRENT_TIME';
CURRENT_TIMESTAMP:                                     'CURRENT_TIMESTAMP';
CURRENT_USER:                                          'CURRENT_USER';
CURSOR:                                                'CURSOR';
CYCLE:                                                 'CYCLE';
DATA:                                                  'DATA';
DATE:                                                  'DATE';
DAY:                                                   'DAY';
DEALLOCATE:                                            'DEALLOCATE';
DEC:                                                   'DEC';
DECIMAL:                                               'DECIMAL';
DECLARE:                                               'DECLARE';
DEFAULT:                                               'DEFAULT';
DEFERRABLE:                                            'DEFERRABLE';
DEFERRED:                                              'DEFERRED';
DEFINER:                                               'DEFINER';
DEGREES:                                               'DEGREES';
DELETE:                                                'DELETE';
DERBY:                                                 'DERBY';
DERBY_JDBC_RESULT_SET:                                 'DERBY_JDBC_RESULT_SET';
DESC:                                                  'DESC';
DESCRIBE:                                              'DESCRIBE';
DETERMINISTIC:                                         'DETERMINISTIC';
DIAGNOSTICS:                                           'DIAGNOSTICS';
DIRTY:                                                 'DIRTY';
DISCONNECT:                                            'DISCONNECT';
DISTINCT:                                              'DISTINCT';
DOUBLE:                                                'DOUBLE';
DROP:                                                  'DROP';
DYNAMIC:                                               'DYNAMIC';
ELSE:                                                  'ELSE';
END:                                                   'END';
END_EXEC:                                              'END_EXEC';
ESCAPE:                                                'ESCAPE';
EXCEPT:                                                'EXCEPT';
EXCEPTION:                                             'EXCEPTION';
EXCLUSIVE:                                             'EXCLUSIVE';
EXEC:                                                  'EXEC';
EXECUTE:                                               'EXEC' 'UTE'?;
EXISTS:                                                'EXISTS';
EXP:                                                   'EXP';
EXPLAIN:                                               'EXPLAIN';
EXTERNAL:                                              'EXTERNAL';
FALSE:                                                 'FALSE';
FETCH:                                                 'FETCH';
FIRST:                                                 'FIRST';
FLOAT:                                                 'FLOAT';
FLOOR:                                                 'FLOOR';
FOR:                                                   'FOR';
FOREIGN:                                               'FOREIGN';
FOUND:                                                 'FOUND';
FROM:                                                  'FROM';
FULL:                                                  'FULL';
FUNCTION:                                              'FUNCTION';
GENERATED:                                             'GENERATED';
GET:                                                   'GET';
GETCURRENTCONNECTION:                                  'GETCURRENTCONNECTION';
GIGA:                                                  'G';
GLOBAL:                                                'GLOBAL';
GO:                                                    'GO';
GOTO:                                                  'GOTO';
GRANT:                                                 'GRANT';
GROUP:                                                 'GROUP';
HAVING:                                                'HAVING';
HOUR:                                                  'HOUR';
IDENTITY:                                              'IDENTITY';
IDENTITY_VAL_LOCAL:                                    'IDENTITY_VAL_LOCAL';
IMMEDIATE:                                             'IMMEDIATE';
IN:                                                    'IN';
INCREMENT:                                             'INCREMENT';
INDEX:                                                 'INDEX';
INDICATOR:                                             'INDICATOR';
INITIALLY:                                             'INITIALLY';
INNER:                                                 'INNER';
INOUT:                                                 'INOUT';
INPUT:                                                 'INPUT';
INSENSITIVE:                                           'INSENSITIVE';
INSERT:                                                'INSERT';
INT:                                                   'INT';
INTEGER:                                               'INTEGER';
INTERSECT:                                             'INTERSECT';
INTO:                                                  'INTO';
INVOKER:                                               'INVOKER';
IS:                                                    'IS';
ISOLATION:                                             'ISOLATION';
JAVA:                                                  'JAVA';
JOIN:                                                  'JOIN';
KEY:                                                   'KEY';
KILO:                                                  'K';
LANGUAGE:                                              'LANGUAGE';
LARGE:                                                 'LARGE';
LAST:                                                  'LAST';
LCASE:                                                 'LCASE';
LEADING:                                               'LEADING';
LEFT:                                                  'LEFT';
LENGTH:                                                'LENGTH';
LIKE:                                                  'LIKE';
LN:                                                    'LN';
LOCATE:                                                'LOCATE';
LOCK:                                                  'LOCK';
LOCKSIZE:                                              'LOCKSIZE';
LOG10:                                                 'LOG10';
LOG:                                                   'LOG';
LOGGED:                                                'LOGGED';
LONG:                                                  'LONG';
LOWER:                                                 'LOWER';
LTRIM:                                                 'LTRIM';
MATCH:                                                 'MATCH';
MATCHED:                                               'MATCHED';
MAX:                                                   'MAX';
MAXVALUE:                                              'MAXVALUE';
MEGA:                                                  'M';
MERGE:                                                 'MERGE';
MIN:                                                   'MIN';
MINUTE:                                                'MINUTE';
MINVALUE:                                              'MINVALUE';
MOD:                                                   'MOD';
MODE:                                                  'MODE';
MODIFIES:                                              'MODIFIES';
MONTH:                                                 'MONTH';
NAME:                                                  'NAME';
NATIONAL:                                              'NATIONAL';
NATURAL:                                               'NATURAL';
NCHAR:                                                 'NCHAR';
NEXT:                                                  'NEXT';
NO:                                                    'NO';
NONE:                                                  'NONE';
NOT:                                                   'NOT';
NULL_:                                                 'NULL';
NULLIF:                                                'NULLIF';
NULLS:                                                 'NULLS';
NUMERIC:                                               'NUMERIC';
NVARCHAR:                                              'NVARCHAR';
OBJECT:                                                'OBJECT';
OF:                                                    'OF';
OFFSET:                                                'OFFSET';
ON:                                                    'ON';
ONLY:                                                  'ONLY';
OPEN:                                                  'OPEN';
OPTION:                                                'OPTION';
OR:                                                    'OR';
ORDER:                                                 'ORDER';
OUT:                                                   'OUT';
OUTER:                                                 'OUTER';
OUTPUT:                                                'OUTPUT';
OVERLAPS:                                              'OVERLAPS';
PAD:                                                   'PAD';
PARAMETER:                                             'PARAMETER';
PARTIAL:                                               'PARTIAL';
PI:                                                    'PI';
PRECISION:                                             'PRECISION';
PREPARE:                                               'PREPARE';
PRESERVE:                                              'PRESERVE';
PRIMARY:                                               'PRIMARY';
PRIOR:                                                 'PRIOR';
PRIVILEGES:                                            'PRIVILEGES';
PROCEDURE:                                             'PROCEDURE';
PUBLIC:                                                'PUBLIC';
RADIANS:                                               'RADIANS';
READ:                                                  'READ';
READS:                                                 'READS';
REAL:                                                  'REAL';
REFERENCES:                                            'REFERENCES';
RELATIVE:                                              'RELATIVE';
RENAME:                                                'RENAME';
REPEATABLE:                                            'REPEATABLE';
RESET:                                                 'RESET';
RESTART:                                               'RESTART';
RESTRICT:                                              'RESTRICT';
RESULT:                                                'RESULT';
RETURNS:                                               'RETURNS';
REVOKE:                                                'REVOKE';
RIGHT:                                                 'RIGHT';
ROLE:                                                  'ROLE';
ROLLBACK:                                              'ROLLBACK';
ROLLUP:                                                'ROLLUP';
ROW:                                                   'ROW';
ROWS:                                                  'ROWS';
RR:                                                    'RR';
RS:                                                    'RS';
RTRIM:                                                 'RTRIM';
SCHEMA:                                                'SCHEMA';
SCROLL:                                                'SCROLL';
SECOND:                                                'SECOND';
SECURITY:                                              'SECURITY';
SELECT:                                                'SELECT';
SEQUENCE:                                              'SEQUENCE';
SERIALIZABLE:                                          'SERIALIZABLE';
SESSION_USER:                                          'SESSION_USER';
SET:                                                   'SET';
SETS:                                                  'SETS';
SHARE:                                                 'SHARE';
SIN:                                                   'SIN';
SMALLINT:                                              'SMALLINT';
SOME:                                                  'SOME';
SPACE:                                                 'SPACE';
SQL:                                                   'SQL';
SQLCODE:                                               'SQLCODE';
SQLERROR:                                              'SQLERROR';
SQLID:                                                 'SQLID';
SQLSTATE:                                              'SQLSTATE';
SQRT:                                                  'SQRT';
STABILITY:                                             'STABILITY';
START:                                                 'START';
STDDEV_POP:                                            'STDDEV_POP';
STDDEV_SAMP:                                           'STDDEV_SAMP';
STYLE:                                                 'STYLE';
SUBSTR:                                                'SUBSTR';
SUBSTRING:                                             'SUBSTRING';
SUM:                                                   'SUM';
SYNONYM:                                               'SYNONYM';
SYSTEM_USER:                                           'SYSTEM_USER';
TABLE:                                                 'TABLE';
TAN:                                                   'TAN';
TEMPORARY:                                             'TEMPORARY';
THEN:                                                  'THEN';
TIME:                                                  'TIME';
TIMESTAMP:                                             'TIMESTAMP';
TIMEZONE_HOUR:                                         'TIMEZONE_HOUR';
TIMEZONE_MINUTE:                                       'TIMEZONE_MINUTE';
TO:                                                    'TO';
TRANSACTION:                                           'TRANSACTION';
TRANSLATE:                                             'TRANSLATE';
TRANSLATION:                                           'TRANSLATION';
TRIGGER:                                               'TRIGGER';
TRIM:                                                  'TRIM';
TRUE:                                                  'TRUE';
TRUNCATE:                                              'TRUNCATE';
TYPE:                                                  'TYPE';
UCASE:                                                 'UCASE';
UNCOMMITTED:                                           'UNCOMMITTED';
UNION:                                                 'UNION';
UNIQUE:                                                'UNIQUE';
UNKNOWN:                                               'UNKNOWN';
UPDATE:                                                'UPDATE';
UPPER:                                                 'UPPER';
UR:                                                    'UR';
USAGE:                                                 'USAGE';
USER:                                                  'USER';
USING:                                                 'USING';
VALUES:                                                'VALUES';
VAR_POP:                                               'VAR_POP';
VAR_SAMP:                                              'VAR_SAMP';
VARCHAR:                                               'VARCHAR';
VARYING:                                               'VARYING';
VIEW:                                                  'VIEW';
WHEN:                                                  'WHEN';
WHENEVER:                                              'WHENEVER';
WHERE:                                                 'WHERE';
WINDOW:                                                'WINDOW';
WITH:                                                  'WITH';
WORK:                                                  'WORK';
WRITE:                                                 'WRITE';
XML:                                                   'XML';
XMLEXISTS:                                             'XMLEXISTS';
XMLPARSE:                                              'XMLPARSE';
XMLQUERY:                                              'XMLQUERY';
XMLSERIALIZE:                                          'XMLSERIALIZE';
YEAR:                                                  'YEAR';


WHITE_SPACE:             [ \t\r\n]+                    -> skip;

SQL_COMMENT:            '/*' (SQL_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT:           '--' ~[\r\n]*                 -> channel(HIDDEN);

// TODO: ID can be not only Latin.
DOUBLE_QUOTE_ID:        '"' ~'"'+ '"';
SINGLE_QUOTE:           '\'';

ID:                     [A-Z_] [A-Z0-9_]*;


STRING_LITERAL:              '\'' (~'\'' | '\'\'')* '\'';
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
LR_BRACKET:          '(';
RR_BRACKET:          ')';
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


fragment FullWidthLetter options { caseInsensitive=false; }
    : '\u00c0'..'\u00d6'
    | '\u00d8'..'\u00f6'
    | '\u00f8'..'\u00ff'
    | '\u0100'..'\u1fff'
    | '\u2c00'..'\u2fff'
    | '\u3040'..'\u318f'
    | '\u3300'..'\u337f'
    | '\u3400'..'\u3fff'
    | '\u4e00'..'\u9fff'
    | '\ua000'..'\ud7ff'
    | '\uf900'..'\ufaff'
    | '\uff00'..'\ufff0'
    // | '\u10000'..'\u1F9FF'  //not support four bytes chars
    // | '\u20000'..'\u2FA1F'
    ;
