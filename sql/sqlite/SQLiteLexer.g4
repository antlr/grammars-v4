/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2020 by Martin Mirchev
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
 * Project : sqlite-parser; an ANTLR4 grammar for SQLite https://github.com/bkiers/sqlite-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 */

// $antlr-format alignTrailingComments on, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments off, useTab off
// $antlr-format allowShortRulesOnASingleLine on, alignSemicolons ownLine, alignColons trailing

lexer grammar SQLiteLexer;

options {
    caseInsensitive = true;
}

SCOL      : ';';
DOT       : '.';
OPEN_PAR  : '(';
CLOSE_PAR : ')';
COMMA     : ',';
ASSIGN    : '=';
STAR      : '*';
PLUS      : '+';
MINUS     : '-';
TILDE     : '~';
PIPE2     : '||';
DIV       : '/';
MOD       : '%';
LT2       : '<<';
GT2       : '>>';
AMP       : '&';
PIPE      : '|';
LT        : '<';
LT_EQ     : '<=';
GT        : '>';
GT_EQ     : '>=';
EQ        : '==';
NOT_EQ1   : '!=';
NOT_EQ2   : '<>';
JPTR      : '->';
JPTR2     : '->>';

// http://www.sqlite.org/lang_keywords.html
ABORT_             : 'ABORT';
ACTION_            : 'ACTION';
ADD_               : 'ADD';
AFTER_             : 'AFTER';
ALL_               : 'ALL';
ALTER_             : 'ALTER';
ALWAYS_            : 'ALWAYS';
ANALYZE_           : 'ANALYZE';
AND_               : 'AND';
AS_                : 'AS';
ASC_               : 'ASC';
ATTACH_            : 'ATTACH';
AUTOINCREMENT_     : 'AUTOINCREMENT';
BEFORE_            : 'BEFORE';
BEGIN_             : 'BEGIN';
BETWEEN_           : 'BETWEEN';
BY_                : 'BY';
CASCADE_           : 'CASCADE';
CASE_              : 'CASE';
CAST_              : 'CAST';
CHECK_             : 'CHECK';
COLLATE_           : 'COLLATE';
COLUMN_            : 'COLUMN';
COMMIT_            : 'COMMIT';
CONFLICT_          : 'CONFLICT';
CONSTRAINT_        : 'CONSTRAINT';
CREATE_            : 'CREATE';
CROSS_             : 'CROSS';
CURRENT_           : 'CURRENT';
CURRENT_DATE_      : 'CURRENT_DATE';
CURRENT_TIME_      : 'CURRENT_TIME';
CURRENT_TIMESTAMP_ : 'CURRENT_TIMESTAMP';
DATABASE_          : 'DATABASE';
DEFAULT_           : 'DEFAULT';
DEFERRABLE_        : 'DEFERRABLE';
DEFERRED_          : 'DEFERRED';
DELETE_            : 'DELETE';
DESC_              : 'DESC';
DETACH_            : 'DETACH';
DISTINCT_          : 'DISTINCT';
DO_                : 'DO';
DROP_              : 'DROP';
EACH_              : 'EACH';
ELSE_              : 'ELSE';
END_               : 'END';
ESCAPE_            : 'ESCAPE';
EXCEPT_            : 'EXCEPT';
EXCLUDE_           : 'EXCLUDE';
EXCLUSIVE_         : 'EXCLUSIVE';
EXISTS_            : 'EXISTS';
EXPLAIN_           : 'EXPLAIN';
FAIL_              : 'FAIL';
FALSE_             : 'FALSE';
FILTER_            : 'FILTER';
FIRST_             : 'FIRST';
FOLLOWING_         : 'FOLLOWING';
FOR_               : 'FOR';
FOREIGN_           : 'FOREIGN';
FROM_              : 'FROM';
FULL_              : 'FULL';
GENERATED_         : 'GENERATED';
GLOB_              : 'GLOB';
GROUP_             : 'GROUP';
GROUPS_            : 'GROUPS';
HAVING_            : 'HAVING';
IF_                : 'IF';
IGNORE_            : 'IGNORE';
IMMEDIATE_         : 'IMMEDIATE';
IN_                : 'IN';
INDEX_             : 'INDEX';
INDEXED_           : 'INDEXED';
INITIALLY_         : 'INITIALLY';
INNER_             : 'INNER';
INSERT_            : 'INSERT';
INSTEAD_           : 'INSTEAD';
INTERSECT_         : 'INTERSECT';
INTO_              : 'INTO';
IS_                : 'IS';
ISNULL_            : 'ISNULL';
JOIN_              : 'JOIN';
KEY_               : 'KEY';
LAST_              : 'LAST';
LEFT_              : 'LEFT';
LIKE_              : 'LIKE';
LIMIT_             : 'LIMIT';
MATCH_             : 'MATCH';
MATERIALIZED_      : 'MATERIALIZED';
NATURAL_           : 'NATURAL';
NO_                : 'NO';
NOT_               : 'NOT';
NOTHING_           : 'NOTHING';
NOTNULL_           : 'NOTNULL';
NULL_              : 'NULL';
NULLS_             : 'NULLS';
OF_                : 'OF';
OFFSET_            : 'OFFSET';
ON_                : 'ON';
OR_                : 'OR';
ORDER_             : 'ORDER';
OTHERS_            : 'OTHERS';
OUTER_             : 'OUTER';
OVER_              : 'OVER';
PARTITION_         : 'PARTITION';
PLAN_              : 'PLAN';
PRAGMA_            : 'PRAGMA';
PRECEDING_         : 'PRECEDING';
PRIMARY_           : 'PRIMARY';
QUERY_             : 'QUERY';
RAISE_             : 'RAISE';
RANGE_             : 'RANGE';
RECURSIVE_         : 'RECURSIVE';
REFERENCES_        : 'REFERENCES';
REGEXP_            : 'REGEXP';
REINDEX_           : 'REINDEX';
RELEASE_           : 'RELEASE';
RENAME_            : 'RENAME';
REPLACE_           : 'REPLACE';
RESTRICT_          : 'RESTRICT';
RETURNING_         : 'RETURNING';
RIGHT_             : 'RIGHT';
ROLLBACK_          : 'ROLLBACK';
ROW_               : 'ROW';
ROWID_             : 'ROWID';
ROWS_              : 'ROWS';
SAVEPOINT_         : 'SAVEPOINT';
SELECT_            : 'SELECT';
SET_               : 'SET';
STORED_            : 'STORED';
STRICT_            : 'STRICT';
TABLE_             : 'TABLE';
TEMP_              : 'TEMP';
TEMPORARY_         : 'TEMPORARY';
THEN_              : 'THEN';
TIES_              : 'TIES';
TO_                : 'TO';
TRANSACTION_       : 'TRANSACTION';
TRIGGER_           : 'TRIGGER';
TRUE_              : 'TRUE';
UNBOUNDED_         : 'UNBOUNDED';
UNION_             : 'UNION';
UNIQUE_            : 'UNIQUE';
UPDATE_            : 'UPDATE';
USING_             : 'USING';
VACUUM_            : 'VACUUM';
VALUES_            : 'VALUES';
VIEW_              : 'VIEW';
VIRTUAL_           : 'VIRTUAL';
WHEN_              : 'WHEN';
WHERE_             : 'WHERE';
WINDOW_            : 'WINDOW';
WITH_              : 'WITH';
WITHIN_            : 'WITHIN';
WITHOUT_           : 'WITHOUT';

IDENTIFIER:
    '"' (~'"' | '""')* '"'
    | '`' (~'`' | '``')* '`'
    | '[' ~']'* ']'
    | [A-Z_\u007F-\uFFFF] [A-Z_0-9\u007F-\uFFFF]*
;

NUMERIC_LITERAL:
    (DIGIT+ ('_' DIGIT+)* ('.' (DIGIT+ ('_' DIGIT+)*)?)? | '.' DIGIT+ ('_' DIGIT+)*) (
        'E' [-+]? DIGIT+ ('_' DIGIT+)*
    )?
    | '0x' HEX_DIGIT+ ('_' HEX_DIGIT+)*
;

BIND_PARAMETER: '?' DIGIT* | [:@$] IDENTIFIER;

STRING_LITERAL: '\'' ( ~'\'' | '\'\'')* '\'';

BLOB_LITERAL: 'X' STRING_LITERAL;

SINGLE_LINE_COMMENT: '--' ~[\r\n]* ('\r'? '\n' | EOF) -> channel(HIDDEN);

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

SPACES: [ \u000B\t\r\n] -> channel(HIDDEN);

UNEXPECTED_CHAR: .;

fragment HEX_DIGIT : [0-9A-F];
fragment DIGIT     : [0-9];
