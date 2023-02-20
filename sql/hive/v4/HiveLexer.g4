/**
   Licensed to the Apache Software Foundation (ASF) under one or more
   contributor license agreements.  See the NOTICE file distributed with
   this work for additional information regarding copyright ownership.
   The ASF licenses this file to You under the Apache License, Version 2.0
   (the "License"); you may not use this file except in compliance with
   the License.  You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
lexer grammar HiveLexer;

options { caseInsensitive = true; }

// Keywords
KW_ABORT                               : 'ABORT';
KW_ACTIVATE                            : 'ACTIVATE';
KW_ACTIVE                              : 'ACTIVE';
KW_ADD                                 : 'ADD';
KW_ADMIN                               : 'ADMIN';
KW_AFTER                               : 'AFTER';
KW_ALL                                 : 'ALL';
KW_ALLOC_FRACTION                      : 'ALLOC_FRACTION';
KW_ALTER                               : 'ALTER';
KW_ANALYZE                             : 'ANALYZE';
KW_AND                                 : 'AND';
KW_ANTI                                : 'ANTI';
KW_ANY                                 : 'ANY';
KW_APPLICATION                         : 'APPLICATION';
KW_ARCHIVE                             : 'ARCHIVE';
KW_ARRAY                               : 'ARRAY';
KW_AS                                  : 'AS';
KW_ASC                                 : 'ASC';
KW_AST                                 : 'AST';
KW_AT                                  : 'AT';
KW_AUTHORIZATION                       : 'AUTHORIZATION';
KW_AUTOCOMMIT                          : 'AUTOCOMMIT';
KW_BATCH                               : 'KW_BATCH';
KW_BEFORE                              : 'BEFORE';
KW_BETWEEN                             : 'BETWEEN';
KW_BIGINT                              : 'BIGINT';
KW_BINARY                              : 'BINARY';
KW_BOOLEAN                             : 'BOOLEAN';
KW_BOTH                                : 'BOTH';
KW_BUCKET                              : 'BUCKET';
KW_BUCKETS                             : 'BUCKETS';
KW_BY                                  : 'BY';
KW_CACHE                               : 'CACHE';
KW_CASCADE                             : 'CASCADE';
KW_CASE                                : 'CASE';
KW_CAST                                : 'CAST';
KW_CBO                                 : 'CBO';
KW_CHANGE                              : 'CHANGE';
KW_CHAR                                : 'CHAR';
KW_CHECK                               : 'CHECK';
KW_CLUSTER                             : 'CLUSTER';
KW_CLUSTERED                           : 'CLUSTERED';
KW_CLUSTERSTATUS                       : 'CLUSTERSTATUS';
KW_COLLECTION                          : 'COLLECTION';
KW_COLUMN                              : 'COLUMN';
KW_COLUMNS                             : 'COLUMNS';
KW_COMMENT                             : 'COMMENT';
KW_COMMIT                              : 'COMMIT';
KW_COMPACT                             : 'COMPACT';
KW_COMPACTIONS                         : 'COMPACTIONS';
KW_COMPACT_ID                          : 'COMPACTIONID';
KW_COMPUTE                             : 'COMPUTE';
KW_CONCATENATE                         : 'CONCATENATE';
KW_CONF                                : 'CONF';
KW_CONSTRAINT                          : 'CONSTRAINT';
KW_CONTINUE                            : 'CONTINUE';
KW_COST                                : 'COST';
KW_CREATE                              : 'CREATE';
KW_CRON                                : 'CRON';
KW_CROSS                               : 'CROSS';
KW_CUBE                                : 'CUBE';
KW_CURRENT                             : 'CURRENT';
KW_CURRENT_DATE                        : 'CURRENT_DATE';
KW_CURRENT_TIMESTAMP                   : 'CURRENT_TIMESTAMP';
KW_CURSOR                              : 'CURSOR';
KW_DATA                                : 'DATA';
KW_DATABASE                            : 'DATABASE';
KW_DATABASES                           : 'DATABASES';
KW_DATACONNECTOR                       : 'CONNECTOR';
KW_DATACONNECTORS                      : 'CONNECTORS';
KW_DATE                                : 'DATE';
KW_DATETIME                            : 'DATETIME';
KW_DAY                                 : 'DAY' 'S'?;
KW_DAYOFWEEK                           : 'KW_DAYOFWEEK';
KW_DBPROPERTIES                        : 'DBPROPERTIES';
KW_DCPROPERTIES                        : 'DCPROPERTIES';
KW_DDL                                 : 'DDL';
KW_DEBUG                               : 'DEBUG';
KW_DECIMAL                             : 'DEC' 'IMAL'? | 'NUMERIC';
KW_DEFAULT                             : 'DEFAULT';
KW_DEFERRED                            : 'DEFERRED';
KW_DEFINED                             : 'DEFINED';
KW_DELETE                              : 'DELETE';
KW_DELIMITED                           : 'DELIMITED';
KW_DEPENDENCY                          : 'DEPENDENCY';
KW_DESC                                : 'DESC';
KW_DESCRIBE                            : 'DESCRIBE';
KW_DETAIL                              : 'DETAIL';
KW_DIRECTORIES                         : 'DIRECTORIES';
KW_DIRECTORY                           : 'DIRECTORY';
KW_DISABLE                             : 'DISABLE' 'D'?;
KW_DISTINCT                            : 'DISTINCT';
KW_DISTRIBUTE                          : 'DISTRIBUTE';
KW_DISTRIBUTED                         : 'DISTRIBUTED';
KW_DO                                  : 'DO';
KW_DOUBLE                              : 'DOUBLE';
KW_DOW                                 : 'DAYOFWEEK';
KW_DROP                                : 'DROP';
KW_DUMP                                : 'DUMP';
KW_ELEM_TYPE                           : '$ELEM$';
KW_ELSE                                : 'ELSE';
KW_ENABLE                              : 'ENABLE' 'D'?;
KW_END                                 : 'END';
KW_ENFORCED                            : 'ENFORCED';
KW_ESCAPED                             : 'ESCAPED';
KW_EVERY                               : 'EVERY';
KW_EXCEPT                              : 'EXCEPT';
KW_EXCHANGE                            : 'EXCHANGE';
KW_EXCLUSIVE                           : 'EXCLUSIVE';
KW_EXECUTE                             : 'EXECUTE';
KW_EXECUTED                            : 'EXECUTED';
KW_EXISTS                              : 'EXISTS';
KW_EXPIRE_SNAPSHOTS                    : 'EXPIRE_SNAPSHOTS';
KW_EXPLAIN                             : 'EXPLAIN';
KW_EXPORT                              : 'EXPORT';
KW_EXPRESSION                          : 'EXPRESSION';
KW_EXTENDED                            : 'EXTENDED';
KW_EXTERNAL                            : 'EXTERNAL';
KW_EXTRACT                             : 'EXTRACT';
KW_FALSE                               : 'FALSE';
KW_FETCH                               : 'FETCH';
KW_FIELDS                              : 'FIELDS';
KW_FILE                                : 'FILE';
KW_FILEFORMAT                          : 'FILEFORMAT';
KW_FIRST                               : 'FIRST';
KW_FLOAT                               : 'FLOAT';
KW_FLOOR                               : 'FLOOR';
KW_FOLLOWING                           : 'FOLLOWING';
KW_FOR                                 : 'FOR';
KW_FORCE                               : 'FORCE';
KW_FOREIGN                             : 'FOREIGN';
KW_FORMAT                              : 'FORMAT';
KW_FORMATTED                           : 'FORMATTED';
KW_FROM                                : 'FROM';
KW_FULL                                : 'FULL';
KW_FUNCTION                            : 'FUNCTION';
KW_FUNCTIONS                           : 'FUNCTIONS';
KW_GRANT                               : 'GRANT';
KW_GROUP                               : 'GROUP';
KW_GROUPING                            : 'GROUPING';
KW_HAVING                              : 'HAVING';
KW_HOLD_DDLTIME                        : 'KW_HOLD_DDLTIME';
KW_HOUR                                : 'HOUR' 'S'?;
KW_IDXPROPERTIES                       : 'IDXPROPERTIES';
KW_IF                                  : 'IF';
KW_IGNORE                              : 'IGNORE';
KW_IMPORT                              : 'IMPORT';
KW_IN                                  : 'IN';
KW_INDEX                               : 'INDEX';
KW_INDEXES                             : 'INDEXES';
KW_INNER                               : 'INNER';
KW_INPATH                              : 'INPATH';
KW_INPUTDRIVER                         : 'INPUTDRIVER';
KW_INPUTFORMAT                         : 'INPUTFORMAT';
KW_INSERT                              : 'INSERT';
KW_INT                                 : 'INT' 'EGER'?;
KW_INTERSECT                           : 'INTERSECT';
KW_INTERVAL                            : 'INTERVAL';
KW_INTO                                : 'INTO';
KW_IS                                  : 'IS';
KW_ISOLATION                           : 'ISOLATION';
KW_ITEMS                               : 'ITEMS';
KW_JAR                                 : 'JAR';
KW_JOIN                                : 'JOIN';
KW_JOINCOST                            : 'JOINCOST';
KW_KEY                                 : 'KEY';
KW_KEYS                                : 'KEYS';
KW_KEY_TYPE                            : '$KEY$';
KW_KILL                                : 'KILL';
KW_LAST                                : 'LAST';
KW_LATERAL                             : 'LATERAL';
KW_LEADING                             : 'LEADING';
KW_LEFT                                : 'LEFT';
KW_LESS                                : 'LESS';
KW_LEVEL                               : 'LEVEL';
KW_LIKE                                : 'LIKE';
KW_LIMIT                               : 'LIMIT';
KW_LINES                               : 'LINES';
KW_LOAD                                : 'LOAD';
KW_LOCAL                               : 'LOCAL';
KW_LOCATION                            : 'LOCATION';
KW_LOCK                                : 'LOCK';
KW_LOCKS                               : 'LOCKS';
KW_LOGICAL                             : 'LOGICAL';
KW_LONG                                : 'LONG';
KW_MACRO                               : 'MACRO';
KW_MANAGED                             : 'MANAGED';
KW_MANAGEDLOCATION                     : 'MANAGEDLOCATION';
KW_MANAGEMENT                          : 'MANAGEMENT';
KW_MAP                                 : 'MAP';
KW_MAPJOIN                             : 'MAPJOIN';
KW_MAPPING                             : 'MAPPING';
KW_MATCHED                             : 'MATCHED';
KW_MATERIALIZED                        : 'MATERIALIZED';
KW_MERGE                               : 'MERGE';
KW_METADATA                            : 'METADATA';
KW_MINUS                               : 'MINUS';
KW_MINUTE                              : 'MINUTE' 'S'?;
KW_MONTH                               : 'MONTH' 'S'?;
KW_MORE                                : 'MORE';
KW_MOVE                                : 'MOVE';
KW_MSCK                                : 'MSCK';
KW_NONE                                : 'NONE';
KW_NORELY                              : 'NORELY';
KW_NOSCAN                              : 'NOSCAN';
KW_NOT                                 : 'NOT' | '!';
KW_NOVALIDATE                          : 'NOVALIDATE';
KW_NO_DROP                             : 'KW_NO_DROP';
KW_NULL                                : 'NULL';
KW_NULLS                               : 'NULLS';
KW_OF                                  : 'OF';
KW_OFFLINE                             : 'KW_OFFLINE';
KW_OFFSET                              : 'OFFSET';
KW_ON                                  : 'ON';
KW_ONLY                                : 'ONLY';
KW_OPERATOR                            : 'OPERATOR';
KW_OPTION                              : 'OPTION';
KW_OR                                  : 'OR';
KW_ORDER                               : 'ORDER';
KW_OUT                                 : 'OUT';
KW_OUTER                               : 'OUTER';
KW_OUTPUTDRIVER                        : 'OUTPUTDRIVER';
KW_OUTPUTFORMAT                        : 'OUTPUTFORMAT';
KW_OVER                                : 'OVER';
KW_OVERWRITE                           : 'OVERWRITE';
KW_OWNER                               : 'OWNER';
KW_PARTITION                           : 'PARTITION';
KW_PARTITIONED                         : 'PARTITIONED';
KW_PARTITIONS                          : 'PARTITIONS';
KW_PATH                                : 'PATH';
KW_PERCENT                             : 'PERCENT';
KW_PKFK_JOIN                           : 'PKFK_JOIN';
KW_PLAN                                : 'PLAN';
KW_PLANS                               : 'PLANS';
KW_PLUS                                : 'PLUS';
KW_POOL                                : 'POOL';
KW_PRECEDING                           : 'PRECEDING';
KW_PRECISION                           : 'PRECISION';
KW_PREPARE                             : 'PREPARE';
KW_PRESERVE                            : 'PRESERVE';
KW_PRIMARY                             : 'PRIMARY';
KW_PRINCIPALS                          : 'PRINCIPALS';
KW_PROCEDURE                           : 'PROCEDURE';
KW_PROTECTION                          : 'KW_PROTECTION';
KW_PURGE                               : 'PURGE';
KW_QUALIFY                             : 'QUALIFY';
KW_QUARTER                             : 'QUARTER';
KW_QUERY                               : 'QUERY';
KW_QUERY_PARALLELISM                   : 'QUERY_PARALLELISM';
KW_RANGE                               : 'RANGE';
KW_READ                                : 'READ';
KW_READONLY                            : 'KW_READONLY';
KW_READS                               : 'READS';
KW_REAL                                : 'REAL';
KW_REBUILD                             : 'REBUILD';
KW_RECORDREADER                        : 'RECORDREADER';
KW_RECORDWRITER                        : 'RECORDWRITER';
KW_REDUCE                              : 'REDUCE';
KW_REFERENCES                          : 'REFERENCES';
KW_REGEXP                              : 'REGEXP';
KW_RELOAD                              : 'RELOAD';
KW_RELY                                : 'RELY';
KW_REMOTE                              : 'REMOTE';
KW_RENAME                              : 'RENAME';
KW_REOPTIMIZATION                      : 'REOPTIMIZATION';
KW_REPAIR                              : 'REPAIR';
KW_REPL                                : 'REPL';
KW_REPLACE                             : 'REPLACE';
KW_REPLICATION                         : 'REPLICATION';
KW_RESOURCE                            : 'RESOURCE';
KW_RESPECT                             : 'RESPECT';
KW_RESTRICT                            : 'RESTRICT';
KW_REVOKE                              : 'REVOKE';
KW_REWRITE                             : 'REWRITE';
KW_RIGHT                               : 'RIGHT';
KW_RLIKE                               : 'RLIKE';
KW_ROLE                                : 'ROLE';
KW_ROLES                               : 'ROLES';
KW_ROLLBACK                            : 'ROLLBACK';
KW_ROLLUP                              : 'ROLLUP';
KW_ROW                                 : 'ROW';
KW_ROWS                                : 'ROWS';
KW_SCHEDULED                           : 'SCHEDULED';
KW_SCHEDULING_POLICY                   : 'SCHEDULING_POLICY';
KW_SCHEMA                              : 'SCHEMA';
KW_SCHEMAS                             : 'SCHEMAS';
KW_SECOND                              : 'SECOND' 'S'?;
KW_SELECT                              : 'SELECT';
KW_SEMI                                : 'SEMI';
KW_SERDE                               : 'SERDE';
KW_SERDEPROPERTIES                     : 'SERDEPROPERTIES';
KW_SERVER                              : 'SERVER';
KW_SET                                 : 'SET';
KW_SETS                                : 'SETS';
KW_SET_CURRENT_SNAPSHOT                : 'SET_CURRENT_SNAPSHOT';
KW_SHARED                              : 'SHARED';
KW_SHOW                                : 'SHOW';
KW_SHOW_DATABASE                       : 'SHOW_DATABASE';
KW_SKEWED                              : 'SKEWED';
KW_SMALLINT                            : 'SMALLINT';
KW_SNAPSHOT                            : 'SNAPSHOT';
KW_SOME                                : 'SOME';
KW_SORT                                : 'SORT';
KW_SORTED                              : 'SORTED';
KW_SPEC                                : 'SPEC';
KW_SSL                                 : 'SSL';
KW_START                               : 'START';
KW_STATISTICS                          : 'STATISTICS';
KW_STATUS                              : 'STATUS';
KW_STORED                              : 'STORED';
KW_STREAMTABLE                         : 'STREAMTABLE';
KW_STRING                              : 'STRING';
KW_STRUCT                              : 'STRUCT';
KW_SUMMARY                             : 'SUMMARY';
KW_SYNC                                : 'SYNC';
KW_SYSTEM_TIME                         : 'SYSTEM_TIME';
KW_SYSTEM_VERSION                      : 'SYSTEM_VERSION';
KW_TABLE                               : 'TABLE';
KW_TABLES                              : 'TABLES';
KW_TABLESAMPLE                         : 'TABLESAMPLE';
KW_TBLPROPERTIES                       : 'TBLPROPERTIES';
KW_TEMPORARY                           : 'TEMPORARY';
KW_TERMINATED                          : 'TERMINATED';
KW_THEN                                : 'THEN';
KW_TIME                                : 'TIME';
KW_TIMESTAMP                           : 'TIMESTAMP';
KW_TIMESTAMPLOCALTZ                    : 'TIMESTAMPLOCALTZ';
KW_TIMESTAMPTZ                         : 'KW_TIMESTAMPTZ';
KW_TINYINT                             : 'TINYINT';
KW_TO                                  : 'TO';
KW_TOUCH                               : 'TOUCH';
KW_TRAILING                            : 'TRAILING';
KW_TRANSACTION                         : 'TRANSACTION';
KW_TRANSACTIONAL                       : 'TRANSACTIONAL';
KW_TRANSACTIONS                        : 'TRANSACTIONS';
KW_TRANSFORM                           : 'TRANSFORM';
KW_TRIGGER                             : 'TRIGGER';
KW_TRIM                                : 'TRIM';
KW_TRUE                                : 'TRUE';
KW_TRUNCATE                            : 'TRUNCATE';
KW_TYPE                                : 'TYPE';
KW_UNARCHIVE                           : 'UNARCHIVE';
KW_UNBOUNDED                           : 'UNBOUNDED';
KW_UNDO                                : 'UNDO';
KW_UNION                               : 'UNION';
KW_UNIONTYPE                           : 'UNIONTYPE';
KW_UNIQUE                              : 'UNIQUE';
KW_UNIQUEJOIN                          : 'UNIQUEJOIN';
KW_UNKNOWN                             : 'UNKNOWN';
KW_UNLOCK                              : 'UNLOCK';
KW_UNMANAGED                           : 'UNMANAGED';
KW_UNSET                               : 'UNSET';
KW_UNSIGNED                            : 'UNSIGNED';
KW_UPDATE                              : 'UPDATE';
KW_URI                                 : 'URI';
KW_URL                                 : 'URL';
KW_USE                                 : 'USE';
KW_USER                                : 'USER';
KW_USING                               : 'USING';
KW_UTC                                 : 'UTC';
KW_UTCTIMESTAMP                        : 'UTC_TMESTAMP';
KW_VALIDATE                            : 'VALIDATE';
KW_VALUES                              : 'VALUES';
KW_VALUE_TYPE                          : '$VALUE$';
KW_VARCHAR                             : 'VARCHAR';
KW_VECTORIZATION                       : 'VECTORIZATION';
KW_VIEW                                : 'VIEW';
KW_VIEWS                               : 'VIEWS';
KW_WAIT                                : 'WAIT';
KW_WEEK                                : 'WEEK' 'S'?;
KW_WHEN                                : 'WHEN';
KW_WHERE                               : 'WHERE';
KW_WHILE                               : 'WHILE';
KW_WINDOW                              : 'WINDOW';
KW_WITH                                : 'WITH';
KW_WITHIN                              : 'WITHIN';
KW_WORK                                : 'WORK';
KW_WORKLOAD                            : 'WORKLOAD';
KW_WRITE                               : 'WRITE';
KW_YEAR                                : 'YEAR' 'S'?;
KW_ZONE                                : 'ZONE';

// Operators
// NOTE: if you add a new function/operator, add it to sysFuncNames so that describe function _FUNC_ will work.
DOT : '.'; // generated as a part of Number rule
COLON : ':' ;
COMMA : ',' ;
SEMICOLON : ';' ;

LPAREN : '(' ;
RPAREN : ')' ;
LSQUARE : '[' ;
RSQUARE : ']' ;
LCURLY : '{';
RCURLY : '}';

EQUAL : '=' | '==';
EQUAL_NS : '<=>';
NOTEQUAL : '<>' | '!=';
LESSTHANOREQUALTO : '<=';
LESSTHAN : '<';
GREATERTHANOREQUALTO : '>=';
GREATERTHAN : '>';

DIVIDE : '/';
PLUS : '+';
MINUS : '-';
STAR : '*';
MOD : '%';
DIV : 'DIV';

AMPERSAND : '&';
TILDE : '~';
BITWISEOR : '|';
CONCATENATE : '||';
BITWISEXOR : '^';
QUESTION : '?';
DOLLAR : '$';

// LITERALS

StringLiteral
    : ( '\'' ( ~('\''|'\\') | ('\\' .) )* '\''
    | '"' ( ~('"'|'\\') | ('\\' .) )* '"'
    )+
    ;

CharSetLiteral
    : StringLiteral
    | '0' 'X' (HexDigit | Digit)+
    ;

IntegralLiteral
    : Digit+ ('L' | 'S' | 'Y')
    ;

NumberLiteral
    : Number ('B'? 'D')
    ;

ByteLengthLiteral
    : Digit+ [BKMG]
    ;

Number
    : Digit+ (DOT Digit* Exponent? | Exponent)?
    ;

/*
An Identifier can be:
- tableName
- columnName
- select expr alias
- lateral view aliases
- database name
- view name
- subquery alias
- function name
- ptf argument identifier
- index name
- property name for: db,tbl,partition...
- fileFormat
- role name
- privilege name
- principal name
- macro name
- hint name
- window name
*/
Identifier
    : (Letter | Digit) (Letter | Digit | '_')*
    | QuotedIdentifier
    | '`' RegexComponent+ '`'
    ;

fragment
QuotedIdentifier
    : '`'  ('``' | ~'`')* '`'
    ;

fragment
Letter
    : 'A'..'Z'
    ;

fragment
HexDigit
    : 'A'..'F'
    ;

fragment
Digit
    : '0'..'9'
    ;

fragment
Exponent
    : ('E') ( PLUS|MINUS )? (Digit)+
    ;

fragment
RegexComponent
    : 'A'..'Z' | '0'..'9' | '_'
    | PLUS | STAR | QUESTION | MINUS | DOT
    | LPAREN | RPAREN | LSQUARE | RSQUARE | LCURLY | RCURLY
    | BITWISEXOR | BITWISEOR | DOLLAR | '!'
    ;

CharSetName
    : '_' (Letter | Digit | '_' | '-' | '.' | ':')+
    ;

WHITE_SPACE
    : (' '|'\r'|'\t'|'\n') -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '--' ~('\n' | '\r')* -> channel(HIDDEN)
    ;

QUERY_HINT
    : SHOW_HINT
    | HIDDEN_HINT
    ;

SHOW_HINT
    : '/*+' (QUERY_HINT | .)*? '*/' ->channel(HIDDEN)
    ;

HIDDEN_HINT
    : '/*' (QUERY_HINT | .)*? '*/' -> channel(HIDDEN)
    ;
