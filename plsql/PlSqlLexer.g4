/**
 * Oracle(c) PL/SQL 11g Parser
 *
 * Copyright (c) 2009-2011 Alexandre Porcelli <alexandre.porcelli@gmail.com>
 * Copyright (c) 2015-2017 Ivan Kochurkin (KvanTTT, kvanttt@gmail.com, Positive Technologies).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lexer grammar PlSqlLexer;

A_LETTER:                     'A';
ADD:                          'ADD';
AFTER:                        'AFTER';
AGENT:                        'AGENT';
AGGREGATE:                    'AGGREGATE';
ALL:                          'ALL';
ALTER:                        'ALTER';
ANALYZE:                      'ANALYZE';
AND:                          'AND';
ANY:                          'ANY';
ARRAY:                        'ARRAY';
AS:                           'AS';
ASC:                          'ASC';
ASSOCIATE:                    'ASSOCIATE';
AT:                           'AT';
ATTRIBUTE:                    'ATTRIBUTE';
AUDIT:                        'AUDIT';
AUTHID:                       'AUTHID';
AUTO:                         'AUTO';
AUTOMATIC:                    'AUTOMATIC';
AUTONOMOUS_TRANSACTION:       'AUTONOMOUS_TRANSACTION';
BATCH:                        'BATCH';
BEFORE:                       'BEFORE';
BEGIN:                        'BEGIN';
BETWEEN:                      'BETWEEN';
BFILE:                        'BFILE';
BINARY_DOUBLE:                'BINARY_DOUBLE';
BINARY_FLOAT:                 'BINARY_FLOAT';
BINARY_INTEGER:               'BINARY_INTEGER';
BLOB:                         'BLOB';
BLOCK:                        'BLOCK';
BODY:                         'BODY';
BOOLEAN:                      'BOOLEAN';
BOTH:                         'BOTH';
BREADTH:                      'BREADTH';
BULK:                         'BULK';
BY:                           'BY';
BYTE:                         'BYTE';
C_LETTER:                     'C';
CACHE:                        'CACHE';
CALL:                         'CALL';
CANONICAL:                    'CANONICAL';
CASCADE:                      'CASCADE';
CASE:                         'CASE';
CAST:                         'CAST';
CHAR:                         'CHAR';
CHAR_CS:                      'CHAR_CS';
CHARACTER:                    'CHARACTER';
CHECK:                        'CHECK';
CHR:                          'CHR';
CLOB:                         'CLOB';
CLOSE:                        'CLOSE';
CLUSTER:                      'CLUSTER';
COALESCE:                     'COALESCE';
COLLECT:                      'COLLECT';
COLUMN:                       'COLUMN';
COLUMNS:                      'COLUMNS';
COMMENT:                      'COMMENT';
COMMIT:                       'COMMIT';
COMMITTED:                    'COMMITTED';
COMPATIBILITY:                'COMPATIBILITY';
COMPILE:                      'COMPILE';
COMPOUND:                     'COMPOUND';
CONNECT:                      'CONNECT';
CONNECT_BY_ROOT:              'CONNECT_BY_ROOT';
CONSTANT:                     'CONSTANT';
CONSTRAINT:                   'CONSTRAINT';
CONSTRAINTS:                  'CONSTRAINTS';
CONSTRUCTOR:                  'CONSTRUCTOR';
CONTENT:                      'CONTENT';
CONTEXT:                      'CONTEXT';
CONTINUE:                     'CONTINUE';
CONVERT:                      'CONVERT';
CORRUPT_XID:                  'CORRUPT_XID';
CORRUPT_XID_ALL:              'CORRUPT_XID_ALL';
COST:                         'COST';
COUNT:                        'COUNT';
CREATE:                       'CREATE';
CROSS:                        'CROSS';
CUBE:                         'CUBE';
CURRENT:                      'CURRENT';
CURRENT_USER:                 'CURRENT_USER';
CURSOR:                       'CURSOR';
CUSTOMDATUM:                  'CUSTOMDATUM';
CYCLE:                        'CYCLE';
DATA:                         'DATA';
DATABASE:                     'DATABASE';
DATE:                         'DATE';
DAY:                          'DAY';
DB_ROLE_CHANGE:               'DB_ROLE_CHANGE';
DBTIMEZONE:                   'DBTIMEZONE';
DDL:                          'DDL';
DEBUG:                        'DEBUG';
DEC:                          'DEC';
DECIMAL:                      'DECIMAL';
DECLARE:                      'DECLARE';
DECOMPOSE:                    'DECOMPOSE';
DECREMENT:                    'DECREMENT';
DEFAULT:                      'DEFAULT';
DEFAULTS:                     'DEFAULTS';
DEFERRED:                     'DEFERRED';
DEFINER:                      'DEFINER';
DELETE:                       'DELETE';
DEPTH:                        'DEPTH';
DESC:                         'DESC';
DETERMINISTIC:                'DETERMINISTIC';
DIMENSION:                    'DIMENSION';
DISABLE:                      'DISABLE';
DISASSOCIATE:                 'DISASSOCIATE';
DISTINCT:                     'DISTINCT';
DOCUMENT:                     'DOCUMENT';
DOUBLE:                       'DOUBLE';
DROP:                         'DROP';
DSINTERVAL_UNCONSTRAINED:     'DSINTERVAL_UNCONSTRAINED';
EACH:                         'EACH';
ELEMENT:                      'ELEMENT';
ELSE:                         'ELSE';
ELSIF:                        'ELSIF';
EMPTY:                        'EMPTY';
ENABLE:                       'ENABLE';
ENCODING:                     'ENCODING';
END:                          'END';
ENTITYESCAPING:               'ENTITYESCAPING';
ERR:                          'ERR';
ERRORS:                       'ERRORS';
ESCAPE:                       'ESCAPE';
EVALNAME:                     'EVALNAME';
EXCEPTION:                    'EXCEPTION';
EXCEPTION_INIT:               'EXCEPTION_INIT';
EXCEPTIONS:                   'EXCEPTIONS';
EXCLUDE:                      'EXCLUDE';
EXCLUSIVE:                    'EXCLUSIVE';
EXECUTE:                      'EXECUTE';
EXISTS:                       'EXISTS';
EXIT:                         'EXIT';
EXPLAIN:                      'EXPLAIN';
EXTERNAL:                     'EXTERNAL';
EXTRACT:                      'EXTRACT';
FAILURE:                      'FAILURE';
FALSE:                        'FALSE';
FETCH:                        'FETCH';
FINAL:                        'FINAL';
FIRST:                        'FIRST';
FIRST_VALUE:                  'FIRST_VALUE';
FLOAT:                        'FLOAT';
FOLLOWING:                    'FOLLOWING';
FOLLOWS:                      'FOLLOWS';
FOR:                          'FOR';
FORALL:                       'FORALL';
FORCE:                        'FORCE';
FROM:                         'FROM';
FULL:                         'FULL';
FUNCTION:                     'FUNCTION';
GOTO:                         'GOTO';
GRANT:                        'GRANT';
GROUP:                        'GROUP';
GROUPING:                     'GROUPING';
HASH:                         'HASH';
HAVING:                       'HAVING';
HIDE:                         'HIDE';
HOUR:                         'HOUR';
IF:                           'IF';
IGNORE:                       'IGNORE';
IMMEDIATE:                    'IMMEDIATE';
IN:                           'IN';
INCLUDE:                      'INCLUDE';
INCLUDING:                    'INCLUDING';
INCREMENT:                    'INCREMENT';
INDENT:                       'INDENT';
INDEX:                        'INDEX';
INDEXED:                      'INDEXED';
INDICATOR:                    'INDICATOR';
INDICES:                      'INDICES';
INFINITE:                     'INFINITE';
INLINE:                       'INLINE';
INNER:                        'INNER';
INOUT:                        'INOUT';
INSERT:                       'INSERT';
INSTANTIABLE:                 'INSTANTIABLE';
INSTEAD:                      'INSTEAD';
INT:                          'INT';
INTEGER:                      'INTEGER';
INTERSECT:                    'INTERSECT';
INTERVAL:                     'INTERVAL';
INTO:                         'INTO';
INVALIDATE:                   'INVALIDATE';
IS:                           'IS';
ISOLATION:                    'ISOLATION';
ITERATE:                      'ITERATE';
JAVA:                         'JAVA';
JOIN:                         'JOIN';
KEEP:                         'KEEP';
LANGUAGE:                     'LANGUAGE';
LAST:                         'LAST';
LAST_VALUE:                   'LAST_VALUE';
LEADING:                      'LEADING';
LEFT:                         'LEFT';
LEVEL:                        'LEVEL';
LIBRARY:                      'LIBRARY';
LIKE:                         'LIKE';
LIKE2:                        'LIKE2';
LIKE4:                        'LIKE4';
LIKEC:                        'LIKEC';
LIMIT:                        'LIMIT';
LOCAL:                        'LOCAL';
LOCK:                         'LOCK';
LOCKED:                       'LOCKED';
LOG:                          'LOG';
LOGOFF:                       'LOGOFF';
LOGON:                        'LOGON';
LONG:                         'LONG';
LOOP:                         'LOOP';
MAIN:                         'MAIN';
MAP:                          'MAP';
MATCHED:                      'MATCHED';
MAXVALUE:                     'MAXVALUE';
MEASURES:                     'MEASURES';
MEMBER:                       'MEMBER';
MERGE:                        'MERGE';
MINUS:                        'MINUS';
MINUTE:                       'MINUTE';
MINVALUE:                     'MINVALUE';
MLSLABEL:                     'MLSLABEL';
MODE:                         'MODE';
MODEL:                        'MODEL';
MODIFY:                       'MODIFY';
MONTH:                        'MONTH';
MULTISET:                     'MULTISET';
NAME:                         'NAME';
NAN:                          'NAN';
NATURAL:                      'NATURAL';
NATURALN:                     'NATURALN';
NAV:                          'NAV';
NCHAR:                        'NCHAR';
NCHAR_CS:                     'NCHAR_CS';
NCLOB:                        'NCLOB';
NESTED:                       'NESTED';
NEW:                          'NEW';
NO:                           'NO';
NOAUDIT:                      'NOAUDIT';
NOCACHE:                      'NOCACHE';
NOCOPY:                       'NOCOPY';
NOCYCLE:                      'NOCYCLE';
NOENTITYESCAPING:             'NOENTITYESCAPING';
NOMAXVALUE:                   'NOMAXVALUE';
NOMINVALUE:                   'NOMINVALUE';
NONE:                         'NONE';
NOORDER:                      'NOORDER';
NOSCHEMACHECK:                'NOSCHEMACHECK';
NOT:                          'NOT';
NOWAIT:                       'NOWAIT';
NULL:                         'NULL';
NULLS:                        'NULLS';
NUMBER:                       'NUMBER';
NUMERIC:                      'NUMERIC';
NVARCHAR2:                    'NVARCHAR2';
OBJECT:                       'OBJECT';
OF:                           'OF';
OFF:                          'OFF';
OID:                          'OID';
OLD:                          'OLD';
ON:                           'ON';
ONLY:                         'ONLY';
OPEN:                         'OPEN';
OPTION:                       'OPTION';
OR:                           'OR';
ORADATA:                      'ORADATA';
ORDER:                        'ORDER';
ORDINALITY:                   'ORDINALITY';
OSERROR:                      'OSERROR';
OUT:                          'OUT';
OUTER:                        'OUTER';
OVER:                         'OVER';
OVERRIDING:                   'OVERRIDING';
PACKAGE:                      'PACKAGE';
PARALLEL_ENABLE:              'PARALLEL_ENABLE';
PARAMETERS:                   'PARAMETERS';
PARENT:                       'PARENT';
PARTITION:                    'PARTITION';
PASSING:                      'PASSING';
PATH:                         'PATH';
PERCENT_ISOPEN:               '%ISOPEN';
PERCENT_FOUND:                '%FOUND';
PERCENT_NOTFOUND:             '%NOTFOUND';
PERCENT_ROWCOUNT:             '%ROWCOUNT';
PERCENT_ROWTYPE:              '%ROWTYPE';
PERCENT_TYPE:                 '%TYPE';
PIPELINED:                    'PIPELINED';
PIVOT:                        'PIVOT';
PLAN:                         'PLAN';
PUBLIC:                       'PUBLIC';
PLS_INTEGER:                  'PLS_INTEGER';
POSITIVE:                     'POSITIVE';
POSITIVEN:                    'POSITIVEN';
PRAGMA:                       'PRAGMA';
PRECEDING:                    'PRECEDING';
PRECISION:                    'PRECISION';
PRESENT:                      'PRESENT';
PRIOR:                        'PRIOR';
PROCEDURE:                    'PROCEDURE';
RAISE:                        'RAISE';
RANGE:                        'RANGE';
RAW:                          'RAW';
READ:                         'READ';
REAL:                         'REAL';
RECORD:                       'RECORD';
REF:                          'REF';
REFERENCE:                    'REFERENCE';
REFERENCING:                  'REFERENCING';
REJECT:                       'REJECT';
RELIES_ON:                    'RELIES_ON';
RENAME:                       'RENAME';
REPLACE:                      'REPLACE';
RESPECT:                      'RESPECT';
RESTRICT_REFERENCES:          'RESTRICT_REFERENCES';
RESULT:                       'RESULT';
RESULT_CACHE:                 'RESULT_CACHE';
RETURN:                       'RETURN';
RETURNING:                    'RETURNING';
REUSE:                        'REUSE';
REVERSE:                      'REVERSE';
REVOKE:                       'REVOKE';
RIGHT:                        'RIGHT';
ROLLBACK:                     'ROLLBACK';
ROLLUP:                       'ROLLUP';
ROW:                          'ROW';
ROWID:                        'ROWID';
ROWS:                         'ROWS';
RULES:                        'RULES';
SAMPLE:                       'SAMPLE';
SAVE:                         'SAVE';
SAVEPOINT:                    'SAVEPOINT';
SCHEMA:                       'SCHEMA';
SCHEMACHECK:                  'SCHEMACHECK';
SCN:                          'SCN';
SEARCH:                       'SEARCH';
SECOND:                       'SECOND';
SEED:                         'SEED';
SEGMENT:                      'SEGMENT';
SELECT:                       'SELECT';
SELF:                         'SELF';
SEQUENCE:                     'SEQUENCE';
SEQUENTIAL:                   'SEQUENTIAL';
SERIALIZABLE:                 'SERIALIZABLE';
SERIALLY_REUSABLE:            'SERIALLY_REUSABLE';
SERVERERROR:                  'SERVERERROR';
SESSIONTIMEZONE:              'SESSIONTIMEZONE';
SET:                          'SET';
SETS:                         'SETS';
SETTINGS:                     'SETTINGS';
SHARE:                        'SHARE';
SHOW:                         'SHOW';
SHUTDOWN:                     'SHUTDOWN';
SIBLINGS:                     'SIBLINGS';
SIGNTYPE:                     'SIGNTYPE';
SIMPLE_INTEGER:               'SIMPLE_INTEGER';
SINGLE:                       'SINGLE';
SIZE:                         'SIZE';
SKIP_:                        'SKIP';
SMALLINT:                     'SMALLINT';
SNAPSHOT:                     'SNAPSHOT';
SOME:                         'SOME';
SPECIFICATION:                'SPECIFICATION';
SQLDATA:                      'SQLDATA';
SQLERROR:                     'SQLERROR';
STANDALONE:                   'STANDALONE';
START:                        'START';
STARTUP:                      'STARTUP';
STATEMENT:                    'STATEMENT';
STATEMENT_ID:                 'STATEMENT_ID';
STATIC:                       'STATIC';
STATISTICS:                   'STATISTICS';
STRING:                       'STRING';
SUBMULTISET:                  'SUBMULTISET';
SUBPARTITION:                 'SUBPARTITION';
SUBSTITUTABLE:                'SUBSTITUTABLE';
SUBTYPE:                      'SUBTYPE';
SUCCESS:                      'SUCCESS';
SUSPEND:                      'SUSPEND';
SYNONYM:                      'SYNONYM';
TABLE:                        'TABLE';
THE:                          'THE';
THEN:                         'THEN';
TIME:                         'TIME';
TIMESTAMP:                    'TIMESTAMP';
TIMESTAMP_LTZ_UNCONSTRAINED:  'TIMESTAMP_LTZ_UNCONSTRAINED';
TIMESTAMP_TZ_UNCONSTRAINED:   'TIMESTAMP_TZ_UNCONSTRAINED';
TIMESTAMP_UNCONSTRAINED:      'TIMESTAMP_UNCONSTRAINED';
TIMEZONE_ABBR:                'TIMEZONE_ABBR';
TIMEZONE_HOUR:                'TIMEZONE_HOUR';
TIMEZONE_MINUTE:              'TIMEZONE_MINUTE';
TIMEZONE_REGION:              'TIMEZONE_REGION';
TO:                           'TO';
TRAILING:                     'TRAILING';
TRANSACTION:                  'TRANSACTION';
TRANSLATE:                    'TRANSLATE';
TREAT:                        'TREAT';
TRIGGER:                      'TRIGGER';
TRUE:                         'TRUE';
TRUNCATE:                     'TRUNCATE';
TYPE:                         'TYPE';
UNBOUNDED:                    'UNBOUNDED';
UNDER:                        'UNDER';
UNION:                        'UNION';
UNIQUE:                       'UNIQUE';
UNLIMITED:                    'UNLIMITED';
UNPIVOT:                      'UNPIVOT';
UNTIL:                        'UNTIL';
UPDATE:                       'UPDATE';
UPDATED:                      'UPDATED';
UPSERT:                       'UPSERT';
UROWID:                       'UROWID';
USE:                          'USE';
USING:                        'USING';
VALIDATE:                     'VALIDATE';
VALUE:                        'VALUE';
VALUES:                       'VALUES';
VARCHAR:                      'VARCHAR';
VARCHAR2:                     'VARCHAR2';
VARIABLE:                     'VARIABLE';
VARRAY:                       'VARRAY';
VARYING:                      'VARYING';
VERSION:                      'VERSION';
VERSIONS:                     'VERSIONS';
WAIT:                         'WAIT';
WARNING:                      'WARNING';
WELLFORMED:                   'WELLFORMED';
WHEN:                         'WHEN';
WHENEVER:                     'WHENEVER';
WHERE:                        'WHERE';
WHILE:                        'WHILE';
WITH:                         'WITH';
WITHIN:                       'WITHIN';
WORK:                         'WORK';
WRITE:                        'WRITE';
XML:                          'XML';
XMLAGG:                       'XMLAGG';
XMLATTRIBUTES:                'XMLATTRIBUTES';
XMLCAST:                      'XMLCAST';
XMLCOLATTVAL:                 'XMLCOLATTVAL';
XMLELEMENT:                   'XMLELEMENT';
XMLEXISTS:                    'XMLEXISTS';
XMLFOREST:                    'XMLFOREST';
XMLNAMESPACES:                'XMLNAMESPACES';
XMLPARSE:                     'XMLPARSE';
XMLPI:                        'XMLPI';
XMLQUERY:                     'XMLQUERY';
XMLROOT:                      'XMLROOT';
XMLSERIALIZE:                 'XMLSERIALIZE';
XMLTABLE:                     'XMLTABLE';
YEAR:                         'YEAR';
YES:                          'YES';
YMINTERVAL_UNCONSTRAINED:     'YMINTERVAL_UNCONSTRAINED';
ZONE:                         'ZONE';

PREDICTION:                   'PREDICTION';
PREDICTION_BOUNDS:            'PREDICTION_BOUNDS';
PREDICTION_COST:              'PREDICTION_COST';
PREDICTION_DETAILS:           'PREDICTION_DETAILS';
PREDICTION_PROBABILITY:       'PREDICTION_PROBABILITY';
PREDICTION_SET:               'PREDICTION_SET';

CUME_DIST:                    'CUME_DIST';
DENSE_RANK:                   'DENSE_RANK';
LISTAGG:                      'LISTAGG';
PERCENT_RANK:                 'PERCENT_RANK';
PERCENTILE_CONT:              'PERCENTILE_CONT';
PERCENTILE_DISC:              'PERCENTILE_DISC';
RANK:                         'RANK';

AVG:                          'AVG';
CORR:                         'CORR';
COVAR_:                       'COVAR_';
DECODE:                       'DECODE';
LAG:                          'LAG';
LEAD:                         'LEAD';
MAX:                          'MAX';
MEDIAN:                       'MEDIAN';
MIN:                          'MIN';
NTILE:                        'NTILE';
NVL:                          'NVL';
RATIO_TO_REPORT:              'RATIO_TO_REPORT';
REGR_:                        'REGR_';
ROUND:                        'ROUND';
ROW_NUMBER:                   'ROW_NUMBER';
SUBSTR:                       'SUBSTR';
TO_CHAR:                      'TO_CHAR';
TRIM:                         'TRIM';
SUM:                          'SUM';
STDDEV:                       'STDDEV';
VAR_:                         'VAR_';
VARIANCE:                     'VARIANCE';

// Rule #358 <NATIONAL_CHAR_STRING_LIT> - subtoken typecast in <REGULAR_ID>, it also incorporates <character_representation>
//  Lowercase 'n' is a usual addition to the standard
NATIONAL_CHAR_STRING_LIT: 'N' '\'' (~('\'' | '\r' | '\n' ) | '\'' '\'' | NEWLINE)* '\'';

//  Rule #040 <BIT_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'b' is a usual addition to the standard
BIT_STRING_LIT: 'B' ('\'' [01]* '\'')+;

//  Rule #284 <HEX_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'x' is a usual addition to the standard
HEX_STRING_LIT: 'X' ('\'' [A-F0-9]* '\'')+;
DOUBLE_PERIOD: '..';
PERIOD:        '.';

//{ Rule #238 <EXACT_NUM_LIT>
//  This rule is a bit tricky - it resolves the ambiguity with <PERIOD> 
//  It also incorporates <mantisa> and <exponent> for the <APPROXIMATE_NUM_LIT>
//  Rule #501 <signed_integer> was incorporated directly in the token <APPROXIMATE_NUM_LIT>
//  See also the rule #617 <unsigned_num_lit>
/*
    : (
            UNSIGNED_INTEGER
            ( '.' UNSIGNED_INTEGER
            | {$type = UNSIGNED_INTEGER;}
            ) ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    | '.' UNSIGNED_INTEGER ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    )
    (D | F)?
    ;*/

UNSIGNED_INTEGER: UNSIGNED_INTEGER_FRAGMENT;
APPROXIMATE_NUM_LIT: FLOAT_FRAGMENT ('E' ('+'|'-')? (FLOAT_FRAGMENT | UNSIGNED_INTEGER_FRAGMENT))? ('D' | 'F')?;

// Rule #--- <CHAR_STRING> is a base for Rule #065 <char_string_lit> , it incorporates <character_representation>
// and a superfluous subtoken typecasting of the "QUOTE"
CHAR_STRING: '\'' (~('\'' | '\r' | '\n') | '\'' '\'' | NEWLINE)* '\'';

// Perl-style quoted string, see Oracle SQL reference, chapter String Literals
CHAR_STRING_PERL    : 'Q' ( QS_ANGLE | QS_BRACE | QS_BRACK | QS_PAREN) -> type(CHAR_STRING);
fragment QUOTE      : '\'' ;
fragment QS_ANGLE   : QUOTE '<' .*? '>' QUOTE ;
fragment QS_BRACE   : QUOTE '{' .*? '}' QUOTE ;
fragment QS_BRACK   : QUOTE '[' .*? ']' QUOTE ;
fragment QS_PAREN   : QUOTE '(' .*? ')' QUOTE ;
fragment QS_OTHER_CH: ~('<' | '{' | '[' | '(' | ' ' | '\t' | '\n' | '\r');

// Rule #163 <DELIMITED_ID>
DELIMITED_ID: '"' (~('"' | '\r' | '\n') | '"' '"')+ '"' ;

// Rule #546 <SQL_SPECIAL_CHAR> was split into single rules
PERCENT: '%';
AMPERSAND: '&';
LEFT_PAREN: '(';
RIGHT_PAREN: ')';
DOUBLE_ASTERISK: '**';
ASTERISK: '*';
PLUS_SIGN: '+';
MINUS_SIGN: '-';
COMMA: ',';
SOLIDUS: '/';
AT_SIGN: '@';
ASSIGN_OP: ':=';
    
// See OCI reference for more information about this
BINDVAR
    : ':' SIMPLE_LETTER  (SIMPLE_LETTER | [0-9] | '_')*
    | ':' DELIMITED_ID  // not used in SQL but spotted in v$sqltext when using cursor_sharing
    | ':' UNSIGNED_INTEGER
    | QUESTION_MARK // not in SQL, not in Oracle, not in OCI, use this for JDBC
    ;

COLON: ':';
SEMICOLON: ';';
LESS_THAN_OR_EQUALS_OP: '<=';
LESS_THAN_OP: '<';
GREATER_THAN_OR_EQUALS_OP: '>=';
NOT_EQUAL_OP: '!='| '<>'| '^='| '~=';
CARRET_OPERATOR_PART: '^';
TILDE_OPERATOR_PART: '~';
EXCLAMATION_OPERATOR_PART: '!';
GREATER_THAN_OP: '>';

fragment
QUESTION_MARK: '?';

// protected UNDERSCORE : '_' SEPARATOR ; // subtoken typecast within <INTRODUCER>
CONCATENATION_OP: '||';
VERTICAL_BAR: '|';
EQUALS_OP: '=';

// Rule #532 <SQL_EMBDD_LANGUAGE_CHAR> was split into single rules:
LEFT_BRACKET: '[';
RIGHT_BRACKET: ']';

//{ Rule #319 <INTRODUCER>
INTRODUCER
    : '_' //(SEPARATOR {$type = UNDERSCORE;})?
    ;

//{ Rule #479 <SEPARATOR>
//  It was originally a protected rule set to be filtered out but the <COMMENT> and <'-'> clashed. 
/*SEPARATOR
    : '-' -> type('-')
    | COMMENT -> channel(HIDDEN)
    | (SPACE | NEWLINE)+ -> channel(HIDDEN)
    ;*/
//}

SPACES: [ \t\r\n]+ -> skip;
    
//{ Rule #504 <SIMPLE_LETTER> - simple_latin _letter was generalised into SIMPLE_LETTER
//  Unicode is yet to be implemented - see NSF0
fragment
SIMPLE_LETTER
    : [A-Z]
    ;
//}

//  Rule #176 <DIGIT> was incorporated by <UNSIGNED_INTEGER> 
//{ Rule #615 <UNSIGNED_INTEGER> - subtoken typecast in <EXACT_NUM_LIT> 
fragment
UNSIGNED_INTEGER_FRAGMENT: [0-9]+ ;

fragment
FLOAT_FRAGMENT
    : UNSIGNED_INTEGER* '.'? UNSIGNED_INTEGER+
    ;

//{ Rule #097 <COMMENT>
SINGLE_LINE_COMMENT: '--' ~('\r' | '\n')* (NEWLINE | EOF)   -> channel(HIDDEN);
MULTI_LINE_COMMENT: '/*' .*? '*/'                           -> channel(HIDDEN);

// SQL*Plus prompt
// TODO should be grammar rule, but tricky to implement
PROMPT
    : 'prompt' SPACE ( ~('\r' | '\n') )* (NEWLINE|EOF)
    ;

START_CMD
    // TODO When using full word START there is a conflict with START WITH in sequences and CONNECT BY queries
    // 'start' SPACE ( ~( '\r' | '\n') )* (NEWLINE|EOF)
    : 'sta' SPACE ( ~('\r' | '\n') )* (NEWLINE|EOF)
    // TODO Single @ conflicts with a database link name, like employees@remote
    // | '@' ( ~('\r' | '\n') )* (NEWLINE|EOF)
    | '@@' ( ~('\r' | '\n') )* (NEWLINE|EOF)
    ;

//{ Rule #360 <NEWLINE>
fragment
NEWLINE: '\r'? '\n';
    
fragment
SPACE: [ \t];

//{ Rule #442 <REGULAR_ID> additionally encapsulates a few STRING_LITs.
//  Within testLiterals all reserved and non-reserved words are being resolved

REGULAR_ID: SIMPLE_LETTER (SIMPLE_LETTER | '$' | '_' | '#' | [0-9])*;
ZV: '@!' -> channel(HIDDEN);