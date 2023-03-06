/*
AWS Athena grammar.
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

lexer grammar AthenaLexer;

options { caseInsensitive = true; }

ADD                : 'ADD';
ALL                : 'ALL';
ALTER              : 'ALTER';
ANALYZE            : 'ANALYZE';
AND                : 'AND';
ANY                : 'ANY';
ARRAY              : 'ARRAY';
AS                 : 'AS';
ASC                : 'ASC';
AVRO               : 'AVRO';
BETWEEN            : 'BETWEEN';
BIGINT             : 'BIGINT';
BIN_PACK           : 'BIN_PACK';
BINARY             : 'BINARY';
BOOLEAN            : 'BOOLEAN';
BUCKETS            : 'BUCKETS';
BY                 : 'BY';
CASCADE            : 'CASCADE';
CASE               : 'CASE';
CAST               : 'CAST';
CHAR               : 'CHAR';
CLUSTERED          : 'CLUSTERED';
COLLECTION         : 'COLLECTION';
COLUMNS            : 'COLUMNS';
COMMENT            : 'COMMENT';
CREATE             : 'CREATE';
DATA               : 'DATA';
DATABASE           : 'DATABASE';
DATABASES          : 'DATABASES';
DATE               : 'DATE';
DBPROPERTIES       : 'DBPROPERTIES';
DEALLOCATE         : 'DEALLOCATE';
DECIMAL            : 'DECIMAL';
DEFINED            : 'DEFINED';
DELETE             : 'DELETE';
DELIMITED          : 'DELIMITED';
DESC               : 'DESC';
DESCRIBE           : 'DESCRIBE';
DISTINCT           : 'DISTINCT';
DISTRIBUTED        : 'DISTRIBUTED';
DOUBLE             : 'DOUBLE';
DROP               : 'DROP';
ELSE               : 'ELSE';
END                : 'END';
ESCAPED            : 'ESCAPED';
EXCEPT             : 'EXCEPT';
EXECUTE            : 'EXECUTE';
EXISTS             : 'EXISTS';
EXPLAIN            : 'EXPLAIN';
EXTENDED           : 'EXTENDED';
EXTERNAL           : 'EXTERNAL';
FALSE              : 'FALSE';
FIELDS             : 'FIELDS';
FIRST              : 'FIRST';
FLOAT              : 'FLOAT';
FORMAT             : 'FORMAT';
FORMATTED          : 'FORMATTED';
FROM               : 'FROM';
GRAPHVIZ           : 'GRAPHVIZ';
GROUP              : 'GROUP';
HAVING             : 'HAVING';
IF                 : 'IF';
IN                 : 'IN';
INPUTFORMAT        : 'INPUTFORMAT';
INSERT             : 'INSERT';
INT                : 'INT';
INTEGER            : 'INTEGER';
INTERSECT          : 'INTERSECT';
INTO               : 'INTO';
IO                 : 'IO';
ION                : 'ION';
IS                 : 'IS';
ITEMS              : 'ITEMS';
JSON               : 'JSON';
KEYS               : 'KEYS';
LAST               : 'LAST';
LIKE               : 'LIKE';
LIMIT              : 'LIMIT';
LINES              : 'LINES';
LOCATION           : 'LOCATION';
LOGICAL            : 'LOGICAL';
MAP                : 'MAP';
MATCHED            : 'MATCHED';
MERGE              : 'MERGE';
MSCK               : 'MSCK';
NO                 : 'NO';
NOT                : 'NOT';
NULL_              : 'NULL';
NULLS              : 'NULLS';
OFFSET             : 'OFFSET';
ON                 : 'ON';
OPTIMIZE           : 'OPTIMIZE';
OR                 : 'OR';
ORC                : 'ORC';
ORDER              : 'ORDER';
OUTPUTFORMAT       : 'OUTPUTFORMAT';
PARQUET            : 'PARQUET';
PARTITION          : 'PARTITION';
PARTITIONED        : 'PARTITIONED';
PARTITIONS         : 'PARTITIONS';
PREPARE            : 'PREPARE';
RCFILE             : 'RCFILE';
RENAME             : 'RENAME';
REPAIR             : 'REPAIR';
REPLACE            : 'REPLACE';
RESTRICT           : 'RESTRICT';
REWRITE            : 'REWRITE';
ROW                : 'ROW';
ROWS               : 'ROWS';
SCHEMA             : 'SCHEMA';
SCHEMAS            : 'SCHEMAS';
SELECT             : 'SELECT';
SEQUENCEFILE       : 'SEQUENCEFILE';
SERDE              : 'SERDE';
SERDEPROPERTIES    : 'SERDEPROPERTIES';
SET                : 'SET';
SHOW               : 'SHOW';
SMALLINT           : 'SMALLINT';
SOME               : 'SOME';
STORED             : 'STORED';
STRING             : 'STRING';
STRUCT             : 'STRUCT';
TABLE              : 'TABLE';
TABLES             : 'TABLES';
TBLPROPERTIES      : 'TBLPROPERTIES';
TERMINATED         : 'TERMINATED';
TEXT               : 'TEXT';
TEXTFILE           : 'TEXTFILE';
THEN               : 'THEN';
TIMESTAMP          : 'TIMESTAMP';
TINYINT            : 'TINYINT';
TO                 : 'TO';
TRUE               : 'TRUE';
TYPE               : 'TYPE';
UNION              : 'UNION';
UNLOAD             : 'UNLOAD';
UPDATE             : 'UPDATE';
USING              : 'USING';
VACUUM             : 'VACUUM';
VALIDATE           : 'VALIDATE';
VALUES             : 'VALUES';
VARCHAR            : 'VARCHAR';
VIEW               : 'VIEW';
VIEWS              : 'VIEWS';
WHEN               : 'WHEN';
WHERE              : 'WHERE';
WITH               : 'WITH';

EQ : '=';
SEMI : ';';
LP : '(';
RP : ')';
DOT : '.';
COMMA : ',';
LT : '<';
GT : '>';
LE : '<=';
GE : '>=';
NE : '<>';
BOX : '!=';
COLON : ':';
QM : '?';
STAR: '*';
PLUS: '+';
MINUS: '-';
DIVIDE: '/';
MODULE: '%';

fragment
Letter
    : 'A'..'Z'
    ;

fragment
DIGIT
    : '0'..'9'
    ;

fragment
DEC_DOT_DEC
    :  (DIGIT+ '.' DIGIT+ |  DIGIT+ '.' | '.' DIGIT+)
    ;

IDENTIFIER
    : Letter (Letter | DIGIT | '_')*
    ;

SQ_STRING_LITERAL
    : '\'' ( ~('\''|'\\') | ('\\' .) )* '\''
    ;

DQ_STRING_LITERAL
    : '"' ( ~('"'|'\\') | ('\\' .) )* '"'
    ;

INTEGRAL_LITERAL
    : DIGIT+
    ;

FLOAT_LITERAL
    : DEC_DOT_DEC
    ;

REAL_LITERAL
    : (INTEGRAL_LITERAL | DEC_DOT_DEC) ('E' [+-]? DIGIT+)
    ;

WS
    : (' '|'\r'|'\t'|'\n') -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '--' (~('\n'|'\r'))* -> channel(HIDDEN)
    ;
