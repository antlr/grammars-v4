/*
Apache Phoenix grammar.
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

lexer grammar PhoenixLexer;

options { caseInsensitive = true; }

ADD:                                'ADD';
ALL:                                'ALL';
ALTER:                              'ALTER';
AND:                                'AND';
ANY:                                'ANY';
APPROX_COUNT_DISTINCT:              'APPROX_COUNT_DISTINCT';
ARRAY:                              'ARRAY';
AS:                                 'AS';
ASC:                                'ASC';
ASYNC:                              'ASYNC';
AVG:                                'AVG';
BETWEEN:                            'BETWEEN';
BIGINT:                             'BIGINT';
BINARY:                             'BINARY';
BY:                                 'BY';
CACHE:                              'CACHE';
CASCADE:                            'CASCADE';
CASE:                               'CASE';
CAST:                               'CAST';
CHAR:                               'CHAR';
CLOSE:                              'CLOSE';
COLUMN:                             'COLUMN';
CONSTANT:                           'CONSTANT';
CONSTRAINT:                         'CONSTRAINT';
COUNT:                              'COUNT';
CREATE:                             'CREATE';
CURRENT:                            'CURRENT';
CURSOR:                             'CURSOR';
CYCLE:                              'CYCLE';
DATE:                               'DATE';
DECIMAL:                            'DECIMAL';
DECLARE:                            'DECLARE';
DEFAULT:                            'DEFAULT';
DEFAULTVALUE:                       'DEFAULTVALUE';
DELETE:                             'DELETE';
DESC:                               'DESC';
DISABLE:                            'DISABLE';
DISTINCT:                           'DISTINCT';
DOUBLE:                             'DOUBLE';
DROP:                               'DROP';
DUPLICATE:                          'DUPLICATE';
ELSE:                               'ELSE';
END:                                'END';
EXISTS:                             'EXISTS';
EXPLAIN:                            'EXPLAIN';
FALSE:                              'FALSE';
FETCH:                              'FETCH';
FIRST:                              'FIRST';
FIRST_VALUE:                        'FIRST_VALUE';
FIRST_VALUES:                       'FIRST_VALUES';
FLOAT:                              'FLOAT';
FOR:                                'FOR';
FROM:                               'FROM';
FUNCTION:                           'FUNCTION';
GRANT:                              'GRANT';
GROUP:                              'GROUP';
HAVING:                             'HAVING';
IF:                                 'IF';
IGNORE:                             'IGNORE';
ILIKE:                              'ILIKE';
IN:                                 'IN';
INCLUDE:                            'INCLUDE';
INCREMENT:                          'INCREMENT';
INDEX:                              'INDEX';
INNER:                              'INNER';
INTEGER:                            'INTEGER';
INTO:                               'INTO';
IS:                                 'IS';
JAR:                                'JAR';
JOIN:                               'JOIN';
KEY:                                'KEY';
LAST:                               'LAST';
LAST_VALUE:                         'LAST_VALUE';
LAST_VALUES:                        'LAST_VALUES';
LEFT:                               'LEFT';
LIKE:                               'LIKE';
LIMIT:                              'LIMIT';
LOCAL:                              'LOCAL';
MAX:                                'MAX';
MAXVALUE:                           'MAXVALUE';
MIN:                                'MIN';
MINVALUE:                           'MINVALUE';
NEXT:                               'NEXT';
NO_CACHE:                           'NO_CACHE';
NO_CHILD_PARENT_JOIN_OPTIMIZATION:  'NO_CHILD_PARENT_JOIN_OPTIMIZATION';
NO_INDEX:                           'NO_INDEX';
NO_SEEK_TO_COLUMN:                  'NO_SEEK_TO_COLUMN';
NO_STAR_JOIN:                       'NO_STAR_JOIN';
NOT:                                'NOT';
NTH_VALUE:                          'NTH_VALUE';
NULL_:                              'NULL';
NULLS:                              'NULLS';
OFFSET:                             'OFFSET';
ON:                                 'ON';
ONLY:                               'ONLY';
OPEN:                               'OPEN';
OR:                                 'OR';
ORDER:                              'ORDER';
OUTER:                              'OUTER';
PERCENT_RANK:                       'PERCENT_RANK';
PERCENTILE_CONT:                    'PERCENTILE_CONT';
PERCENTILE_DISC:                    'PERCENTILE_DISC';
PRIMARY:                            'PRIMARY';
RANGE_SCAN:                         'RANGE_SCAN';
REBUILD:                            'REBUILD';
RETURNS:                            'RETURNS';
REVOKE:                             'REVOKE';
RIGHT:                              'RIGHT';
ROW:                                'ROW';
ROW_TIMESTAMP:                      'ROW_TIMESTAMP';
ROWS:                               'ROWS';
SCHEMA:                             'SCHEMA';
SEEK_TO_COLUMN:                     'SEEK_TO_COLUMN';
SELECT:                             'SELECT';
SEQUENCE:                           'SEQUENCE';
SERIAL:                             'SERIAL';
SET:                                'SET';
SKIP_SCAN:                          'SKIP_SCAN';
SMALL:                              'SMALL';
SMALLINT:                           'SMALLINT';
SPLIT:                              'SPLIT';
START:                              'START';
STATISTICS:                         'STATISTICS';
STDDEV_POP:                         'STDDEV_POP';
STDDEV_SAMP:                        'STDDEV_SAMP';
SUM:                                'SUM';
TABLE:                              'TABLE';
TABLESAMPLE:                        'TABLESAMPLE';
TEMPORARY:                          'TEMPORARY';
THEN:                               'THEN';
TIME:                               'TIME';
TIMESTAMP:                          'TIMESTAMP';
TINYINT:                            'TINYINT';
TO:                                 'TO';
TRUE:                               'TRUE';
UNION:                              'UNION';
UNSIGNED_DATE:                      'UNSIGNED_DATE';
UNSIGNED_DOUBLE:                    'UNSIGNED_DOUBLE';
UNSIGNED_FLOAT:                     'UNSIGNED_FLOAT';
UNSIGNED_INT:                       'UNSIGNED_INT';
UNSIGNED_LONG:                      'UNSIGNED_LONG';
UNSIGNED_SMALLINT:                  'UNSIGNED_SMALLINT';
UNSIGNED_TIME:                      'UNSIGNED_TIME';
UNSIGNED_TIMESTAMP:                 'UNSIGNED_TIMESTAMP';
UNSIGNED_TINYINT:                   'UNSIGNED_TINYINT';
UNUSABLE:                           'UNUSABLE';
UPDATE:                             'UPDATE';
UPSERT:                             'UPSERT';
USABLE:                             'USABLE';
USE:                                'USE';
USE_DATA_OVER_INDEX_TABLE:          'USE_DATA_OVER_INDEX_TABLE';
USE_INDEX_OVER_DATA_TABLE:          'USE_INDEX_OVER_DATA_TABLE';
USE_SORT_MERGE_JOIN:                'USE_SORT_MERGE_JOIN';
USING:                              'USING';
VALUE:                              'VALUE';
VALUES:                             'VALUES';
VARBINARY:                          'VARBINARY';
VARCHAR:                            'VARCHAR';
VIEW:                               'VIEW';
WHEN:                               'WHEN';
WHERE:                              'WHERE';
WITH:                               'WITH';


SEMI: ';';
COLON: ':';
COMMA: ',';
DOT: '.';
LP: '(';
RP: ')';
STAR: '*';
DIV: '/';
MOD: '%';
PLUS: '+';
MINUS: '-';
PIPEPIPE: '||';
LSB: '[';
RSC: ']';
EQ: '=';
NE: '<>';
NE2: '!=';
GT: '>';
GE: '>=';
LT: '<';
LE: '<=';
QM: '?';


WHITE_SPACE:             [ \t\r\n]+                   -> channel(HIDDEN);

SQL_COMMENT:            '/*' (SQL_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT:           '--' ~[\r\n]*                 -> channel(HIDDEN);
HINT_START:             '/*+';
HINT_END:               '*/';

DOUBLE_QUOTE_ID:        '"' ~'"'+ '"';
SINGLE_QUOTE:           '\'';

ID:                     [A-Z_] [A-Z0-9_]*;


STRING_LITERAL:              '\'' ~'\''* '\'';
DECIMAL_LITERAL:             DEC_DIGIT+;
FLOAT_LITERAL:               DEC_DOT_DEC;
REAL_LITERAL:                (DECIMAL_LITERAL | DEC_DOT_DEC) ('E' [+-]? DEC_DIGIT+);
CHAR_LITERAL:                '\'' (~['\\\r\n]) '\'';

fragment LETTER:       [A-Z_];

fragment DEC_DOT_DEC:  DEC_DIGIT+ '.' DEC_DIGIT* | '.' DEC_DIGIT+;

fragment DEC_DIGIT:    [0-9];
