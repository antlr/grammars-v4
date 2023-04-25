/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2023 by 455741807@qq.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project : InformixSQLParser; an ANTLR4 grammar for Informix database SQL language,
 * see as https://www.ibm.com/docs/en/informix-servers/14.10?topic=programming-guide-sql-syntax
 */


lexer grammar InformixSQLLexer;

options { caseInsensitive = true; }

SCOL:      ';';
DOT:       '.';
OPEN_PAR:  '(';
CLOSE_PAR: ')';
COMMA:     ',';
ASSIGN:    '=';
STAR:      '*';
PLUS:      '+';
MINUS:     '-';
TILDE:     '~';
PIPE2:     '||';
DIV:       '/';
MOD:       '%';
LT2:       '<<';
GT2:       '>>';
AMP:       '&';
PIPE:      '|';
LT:        '<';
LT_EQ:     '<=';
GT:        '>';
GT_EQ:     '>=';
EQ:        '==';
NOT_EQ1:   '!=';
NOT_EQ2:   '<>';

// Interval type Keywords
ABORT:             'ABORT';
ACTION:            'ACTION';
ADD:               'ADD';
AFTER:             'AFTER';
ALL:               'ALL';
ALTER:             'ALTER';
ANALYZE:           'ANALYZE';
AND:               'AND';
AS:                'AS';
ASC:               'ASC';
ATTACH:            'ATTACH';
AUTOINCREMENT:     'AUTOINCREMENT';
BEFORE:            'BEFORE';
BEGIN:             'BEGIN';
BETWEEN:           'BETWEEN';
BY:                'BY';
CASCADE:           'CASCADE';
CASE:              'CASE';
CAST:              'CAST';
CHECK:             'CHECK';
COLLATE:           'COLLATE';
COLUMN:            'COLUMN';
COMMIT:            'COMMIT';
CONFLICT:          'CONFLICT';
CONSTRAINT:        'CONSTRAINT';
CREATE:            'CREATE';
CROSS:             'CROSS';
CURRENT_DATE:      'CURRENT_DATE';
CURRENT_TIME:      'CURRENT_TIME';
CURRENT_TIMESTAMP: 'CURRENT_TIMESTAMP';
DATABASE:          'DATABASE';
DEFAULT:           'DEFAULT';
DEFERRABLE:        'DEFERRABLE';
DEFERRED:          'DEFERRED';
DELETE:            'DELETE';
DESC:              'DESC';
DETACH:            'DETACH';
DISTINCT:          'DISTINCT';
DROP:              'DROP';
EACH:              'EACH';
ELSE:              'ELSE';
END:               'END';
ESCAPE:            'ESCAPE';
EXCEPT:            'EXCEPT';
EXCLUSIVE:         'EXCLUSIVE';
EXISTS:            'EXISTS';
EXPLAIN:           'EXPLAIN';
FAIL:              'FAIL';
FOR:               'FOR';
FOREIGN:           'FOREIGN';
FROM:              'FROM';
FULL:              'FULL';
GLOB:              'GLOB';
GROUP:             'GROUP';
HAVING:            'HAVING';
IF:                'IF';
IGNORE:            'IGNORE';
IMMEDIATE:         'IMMEDIATE';
IN:                'IN';
INDEX:             'INDEX';
INDEXED:           'INDEXED';
INITIALLY:         'INITIALLY';
INNER:             'INNER';
INSERT:            'INSERT';
INSTEAD:           'INSTEAD';
INTERSECT:         'INTERSECT';
INTO:              'INTO';
IS:                'IS';
ISNULL:            'ISNULL';
JOIN:              'JOIN';
KEY:               'KEY';
LEFT:              'LEFT';
LIKE:              'LIKE';
LIMIT:             'LIMIT';
MATCH:             'MATCH';
NATURAL:           'NATURAL';
NO:                'NO';
NOT:               'NOT';
NOTNULL:           'NOTNULL';
NULL:              'NULL';
OF:                'OF';
OFFSET:            'OFFSET';
ON:                'ON';
OR:                'OR';
ORDER:             'ORDER';
OUTER:             'OUTER';
PLAN:              'PLAN';
PRAGMA:            'PRAGMA';
PRIMARY:           'PRIMARY';
QUERY:             'QUERY';
RAISE:             'RAISE';
RECURSIVE:         'RECURSIVE';
REFERENCES:        'REFERENCES';
REGEXP:            'REGEXP';
REINDEX:           'REINDEX';
RELEASE:           'RELEASE';
RENAME:            'RENAME';
REPLACE:           'REPLACE';
RESTRICT:          'RESTRICT';
RETURNING:         'RETURNING';
RIGHT:             'RIGHT';
ROLLBACK:          'ROLLBACK';
ROW:               'ROW';
ROLE:              'ROLE';
ROWS:              'ROWS';
SAVEPOINT:         'SAVEPOINT';
SELECT:            'SELECT';
SET:               'SET';
TABLE:             'TABLE';
TEMP:              'TEMP';
TEMPORARY:         'TEMPORARY';
THEN:              'THEN';
TO:                'TO';
TRANSACTION:       'TRANSACTION';
TRIGGER:           'TRIGGER';
UNION:             'UNION';
UNIQUE:            'UNIQUE';
UPDATE:            'UPDATE';
USER:              'USER';
USING:             'USING';
VACUUM:            'VACUUM';
VALUES:            'VALUES';
VIEW:              'VIEW';
VIRTUAL:           'VIRTUAL';
WHEN:              'WHEN';
WHERE:             'WHERE';
WITH:              'WITH';
WITHOUT:           'WITHOUT';
FIRST_VALUE:       'FIRST_VALUE';
OVER:              'OVER';
PARTITION:         'PARTITION';
RANGE:             'RANGE';
PRECEDING:         'PRECEDING';
UNBOUNDED:         'UNBOUNDED';
CURRENT:           'CURRENT';
FOLLOWING:         'FOLLOWING';
CUME_DIST:         'CUME_DIST';
DENSE_RANK:        'DENSE_RANK';
LAG:               'LAG';
LAST_VALUE:        'LAST_VALUE';
LEAD:              'LEAD';
NTH_VALUE:         'NTH_VALUE';
NTILE:             'NTILE';
PERCENT_RANK:      'PERCENT_RANK';
RANK:              'RANK';
ROW_NUMBER:        'ROW_NUMBER';
GENERATED:         'GENERATED';
ALWAYS:            'ALWAYS';
STORED:            'STORED';
TRUE:              'TRUE';
FALSE:             'FALSE';
WINDOW:            'WINDOW';
NULLS:             'NULLS';
FIRST:             'FIRST';
LAST:              'LAST';
FILTER:            'FILTER';
GROUPS:            'GROUPS';
EXCLUDE:           'EXCLUDE';
TIES:              'TIES';
OTHERS:            'OTHERS';
DO:                'DO';
NOTHING:           'NOTHING';

IDENTIFIER:
    '"' (~'"' | '""')* '"'
    | '`' (~'`' | '``')* '`'
    | '[' ~']'* ']'
    | [A-Z_] [A-Z_0-9]*
; // TODO check: needs more chars in set

NUMERIC_LITERAL: (DIGIT+ ('.' DIGIT*)? | '.' DIGIT+) ('E' [-+]? DIGIT+)? | '0x' HEX_DIGIT+;

BIND_PARAMETER: '?' DIGIT* | [:@$] IDENTIFIER;

STRING_LITERAL: '\'' (~'\'' | '\'\'')* '\'';

BLOB_LITERAL: 'X' STRING_LITERAL;

SINGLE_LINE_COMMENT: '--' ~[\r\n]* ('\r'? '\n' | EOF) -> channel(HIDDEN);

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

SPACES: [ \u000B\t\r\n] -> channel(HIDDEN);

UNEXPECTED_CHAR: .;

fragment HEX_DIGIT: [0-9A-F];
fragment DIGIT:     [0-9];
