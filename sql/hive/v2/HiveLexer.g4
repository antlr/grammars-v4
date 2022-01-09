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

   @author wyruweso
   based on the Hive 3.1.2 grammar by Canwei He
*/

lexer grammar HiveLexer;

KW_TRUE : T R U E;
KW_FALSE : F A L S E;
KW_ALL : A L L;
KW_NONE: N O N E;
KW_AND : A N D;
KW_OR : O R;
KW_NOT : N O T | '!';
KW_LIKE : L I K E;
KW_ANY : A N Y;
KW_IF : I F;
KW_EXISTS : E X I S T S;
KW_ASC : A S C;
KW_DESC : D E S C;
KW_NULLS : N U L L S;
KW_LAST : L A S T;
KW_ORDER : O R D E R;
KW_GROUP : G R O U P;
KW_BY : B Y;
KW_HAVING : H A V I N G;
KW_WHERE : W H E R E;
KW_FROM : F R O M;
KW_AS : A S;
KW_SELECT : S E L E C T;
KW_DISTINCT : D I S T I N C T;
KW_INSERT : I N S E R T;
KW_OVERWRITE : O V E R W R I T E;
KW_OUTER : O U T E R;
KW_UNIQUEJOIN : U N I Q U E J O I N;
KW_PRESERVE : P R E S E R V E;
KW_JOIN : J O I N;
KW_LEFT : L E F T;
KW_RIGHT : R I G H T;
KW_FULL : F U L L;
KW_ON : O N;
KW_PARTITION : P A R T I T I O N;
KW_PARTITIONS : P A R T I T I O N S;
KW_TABLE: T A B L E;
KW_TABLES: T A B L E S;
KW_COLUMNS: C O L U M N S;
KW_INDEX: I N D E X;
KW_INDEXES: I N D E X E S;
KW_REBUILD: R E B U I L D;
KW_FUNCTIONS: F U N C T I O N S;
KW_SHOW: S H O W;
KW_MSCK: M S C K;
KW_REPAIR: R E P A I R;
KW_DIRECTORY: D I R E C T O R Y;
KW_LOCAL: L O C A L;
KW_TRANSFORM : T R A N S F O R M;
KW_USING: U S I N G;
KW_CLUSTER: C L U S T E R;
KW_DISTRIBUTE: D I S T R I B U T E;
KW_SORT: S O R T;
KW_UNION: U N I O N;
KW_EXCEPT: E X C E P T;
KW_LOAD: L O A D;
KW_EXPORT: E X P O R T;
KW_IMPORT: I M P O R T;
KW_REPLICATION: R E P L I C A T I O N;
KW_METADATA: M E T A D A T A;
KW_DATA: D A T A;
KW_INPATH: I N P A T H;
KW_IS: I S;
KW_NULL: N U L L;
KW_CREATE: C R E A T E;
KW_EXTERNAL: E X T E R N A L;
KW_ALTER: A L T E R;
KW_CHANGE: C H A N G E;
KW_COLUMN: C O L U M N;
KW_FIRST: F I R S T;
KW_AFTER: A F T E R;
KW_DESCRIBE: D E S C R I B E;
KW_DROP: D R O P;
KW_RENAME: R E N A M E;
KW_TO: T O;
KW_COMMENT: C O M M E N T;
KW_BOOLEAN: B O O L E A N;
KW_TINYINT: T I N Y I N T;
KW_SMALLINT: S M A L L I N T;
KW_INT: I N T | I N T E G E R;
KW_BIGINT: B I G I N T;
KW_FLOAT: F L O A T;
KW_DOUBLE: D O U B L E;
KW_PRECISION: P R E C I S I O N;
KW_DATE: D A T E;
KW_DATETIME: D A T E T I M E;
KW_TIMESTAMP: T I M E S T A M P;
KW_TIMESTAMPLOCALTZ: T I M E S T A M P L O C A L T Z;
KW_TIME: T I M E;
KW_ZONE: Z O N E;
KW_INTERVAL: I N T E R V A L;
KW_DECIMAL: D E C I M A L;
KW_STRING: S T R I N G;
KW_CHAR: C H A R;
KW_VARCHAR: V A R C H A R;
KW_ARRAY: A R R A Y;
KW_STRUCT: S T R U C T;
KW_MAP: M A P;
KW_UNIONTYPE: U N I O N T Y P E;
KW_REDUCE: R E D U C E;
KW_PARTITIONED: P A R T I T I O N E D;
KW_CLUSTERED: C L U S T E R E D;
KW_SORTED: S O R T E D;
KW_INTO: I N T O;
KW_BUCKETS: B U C K E T S;
KW_ROW: R O W;
KW_ROWS: R O W S;
KW_FORMAT: F O R M A T;
KW_DELIMITED: D E L I M I T E D;
KW_FIELDS: F I E L D S;
KW_TERMINATED: T E R M I N A T E D;
KW_ESCAPED: E S C A P E D;
KW_COLLECTION: C O L L E C T I O N;
KW_ITEMS: I T E M S;
KW_KEYS: K E Y S;
KW_KEY_TYPE: '$' K E Y '$';
KW_KILL: K I L L;
KW_LINES: L I N E S;
KW_STORED: S T O R E D;
KW_FILEFORMAT: F I L E F O R M A T;
KW_INPUTFORMAT: I N P U T F O R M A T;
KW_OUTPUTFORMAT: O U T P U T F O R M A T;
KW_INPUTDRIVER: I N P U T D R I V E R;
KW_OUTPUTDRIVER: O U T P U T D R I V E R;
KW_ENABLE: E N A B L E;
KW_DISABLE: D I S A B L E;
KW_LOCATION: L O C A T I O N;
KW_TABLESAMPLE: T A B L E S A M P L E;
KW_BUCKET: B U C K E T;
KW_OUT: O U T;
KW_OF: O F;
KW_PERCENT: P E R C E N T;
KW_CAST: C A S T;
KW_ADD: A D D;
KW_REPLACE: R E P L A C E;
KW_RLIKE: R L I K E;
KW_REGEXP: R E G E X P;
KW_TEMPORARY: T E M P O R A R Y;
KW_FUNCTION: F U N C T I O N;
KW_MACRO: M A C R O;
KW_FILE: F I L E;
KW_JAR: J A R;
KW_EXPLAIN: E X P L A I N;
KW_EXTENDED: E X T E N D E D;
KW_FORMATTED: F O R M A T T E D;
KW_DEPENDENCY: D E P E N D E N C Y;
KW_LOGICAL: L O G I C A L;
KW_SERDE: S E R D E;
KW_WITH: W I T H;
KW_DEFERRED: D E F E R R E D;
KW_SERDEPROPERTIES: S E R D E P R O P E R T I E S;
KW_DBPROPERTIES: D B P R O P E R T I E S;
KW_LIMIT: L I M I T;
KW_OFFSET: O F F S E T;
KW_SET: S E T;
KW_UNSET: U N S E T;
KW_TBLPROPERTIES: T B L P R O P E R T I E S;
KW_IDXPROPERTIES: I D X P R O P E R T I E S;
KW_VALUE_TYPE: '$' V A L U E '$';
KW_ELEM_TYPE: '$' E L E M '$';
KW_DEFINED: D E F I N E D;
KW_CASE: C A S E;
KW_WHEN: W H E N;
KW_THEN: T H E N;
KW_ELSE: E L S E;
KW_END: E N D;
KW_MAPJOIN: M A P J O I N;
KW_STREAMTABLE: S T R E A M T A B L E;
KW_CLUSTERSTATUS: C L U S T E R S T A T U S;
KW_UTC: U T C;
KW_UTCTIMESTAMP: U T C T I M E S T A M P;
KW_LONG: L O N G;
KW_DELETE: D E L E T E;
KW_PLUS: P L U S;
KW_MINUS: M I N U S;
KW_FETCH: F E T C H;
KW_INTERSECT: I N T E R S E C T;
KW_VIEW: V I E W;
KW_VIEWS: V I E W S;
KW_IN: I N;
KW_DATABASE: D A T A B A S E;
KW_DATABASES: D A T A B A S E S;
KW_MATERIALIZED: M A T E R I A L I Z E D;
KW_SCHEMA: S C H E M A;
KW_SCHEMAS: S C H E M A S;
KW_GRANT: G R A N T;
KW_REVOKE: R E V O K E;
KW_SSL: S S L;
KW_UNDO: U N D O;
KW_LOCK: L O C K;
KW_LOCKS: L O C K S;
KW_UNLOCK: U N L O C K;
KW_SHARED: S H A R E D;
KW_EXCLUSIVE: E X C L U S I V E;
KW_PROCEDURE: P R O C E D U R E;
KW_UNSIGNED: U N S I G N E D;
KW_WHILE: W H I L E;
KW_READ: R E A D;
KW_READS: R E A D S;
KW_PURGE: P U R G E;
KW_RANGE: R A N G E;
KW_ANALYZE: A N A L Y Z E;
KW_BEFORE: B E F O R E;
KW_BETWEEN: B E T W E E N;
KW_BOTH: B O T H;
KW_BINARY: B I N A R Y;
KW_CROSS: C R O S S;
KW_CONTINUE: C O N T I N U E;
KW_CURSOR: C U R S O R;
KW_TRIGGER: T R I G G E R;
KW_RECORDREADER: R E C O R D R E A D E R;
KW_RECORDWRITER: R E C O R D W R I T E R;
KW_SEMI: S E M I;
KW_LATERAL: L A T E R A L;
KW_TOUCH: T O U C H;
KW_ARCHIVE: A R C H I V E;
KW_UNARCHIVE: U N A R C H I V E;
KW_COMPUTE: C O M P U T E;
KW_STATISTICS: S T A T I S T I C S;
KW_USE: U S E;
KW_OPTION: O P T I O N;
KW_CONCATENATE: C O N C A T E N A T E;
KW_SHOW_DATABASE: S H O W '_' D A T A B A S E;
KW_UPDATE: U P D A T E;
KW_RESTRICT: R E S T R I C T;
KW_CASCADE: C A S C A D E;
KW_SKEWED: S K E W E D;
KW_ROLLUP: R O L L U P;
KW_CUBE: C U B E;
KW_DIRECTORIES: D I R E C T O R I E S;
KW_FOR: F O R;
KW_WINDOW: W I N D O W;
KW_UNBOUNDED: U N B O U N D E D;
KW_PRECEDING: P R E C E D I N G;
KW_FOLLOWING: F O L L O W I N G;
KW_CURRENT: C U R R E N T;
KW_CURRENT_DATE: C U R R E N T '_' D A T E;
KW_CURRENT_TIMESTAMP: C U R R E N T '_' T I M E S T A M P;
KW_LESS: L E S S;
KW_MORE: M O R E;
KW_OVER: O V E R;
KW_GROUPING: G R O U P I N G;
KW_SETS: S E T S;
KW_TRUNCATE: T R U N C A T E;
KW_NOSCAN: N O S C A N;
KW_USER: U S E R;
KW_ROLE: R O L E;
KW_ROLES: R O L E S;
KW_INNER: I N N E R;
KW_EXCHANGE: E X C H A N G E;
KW_URI: U R I;
KW_SERVER : S E R V E R;
KW_ADMIN: A D M I N;
KW_OWNER: O W N E R;
KW_PRINCIPALS: P R I N C I P A L S;
KW_COMPACT: C O M P A C T;
KW_COMPACTIONS: C O M P A C T I O N S;
KW_TRANSACTIONS: T R A N S A C T I O N S;
KW_REWRITE : R E W R I T E;
KW_AUTHORIZATION: A U T H O R I Z A T I O N;
KW_REOPTIMIZATION: R E O P T I M I Z A T I O N;
KW_CONF: C O N F;
KW_VALUES: V A L U E S;
KW_RELOAD: R E L O A D;
KW_YEAR: Y E A R | Y E A R S;
KW_QUERY: Q U E R Y;
KW_QUARTER: Q U A R T E R;
KW_MONTH: M O N T H | M O N T H S;
KW_WEEK:W E E K | W E E K S;
KW_DAY: D A Y | D A Y S;
KW_DOW: D O W;
KW_HOUR: H O U R | H O U R S;
KW_MINUTE: M I N U T E | M I N U T E S;
KW_SECOND: S E C O N D | S E C O N D S;
KW_START: S T A R T;
KW_TRANSACTION: T R A N S A C T I O N;
KW_COMMIT: C O M M I T;
KW_ROLLBACK: R O L L B A C K;
KW_WORK: W O R K;
KW_ONLY: O N L Y;
KW_WRITE: W R I T E;
KW_ISOLATION: I S O L A T I O N;
KW_LEVEL: L E V E L;
KW_SNAPSHOT: S N A P S H O T;
KW_AUTOCOMMIT: A U T O C O M M I T;
KW_CACHE: C A C H E;
KW_PRIMARY: P R I M A R Y;
KW_FOREIGN: F O R E I G N;
KW_REFERENCES: R E F E R E N C E S;
KW_CONSTRAINT: C O N S T R A I N T;
KW_ENFORCED: E N F O R C E D;
KW_VALIDATE: V A L I D A T E;
KW_NOVALIDATE: N O V A L I D A T E;
KW_RELY: R E L Y;
KW_NORELY: N O R E L Y;
KW_UNIQUE: U N I Q U E;
KW_KEY: K E Y;
KW_ABORT: A B O R T;
KW_EXTRACT: E X T R A C T;
KW_FLOOR: F L O O R;
KW_MERGE: M E R G E;
KW_MATCHED: M A T C H E D;
KW_REPL: R E P L;
KW_DUMP: D U M P;
KW_STATUS: S T A T U S;
KW_VECTORIZATION: V E C T O R I Z A T I O N;
KW_SUMMARY: S U M M A R Y;
KW_OPERATOR: O P E R A T O R;
KW_EXPRESSION: E X P R E S S I O N;
KW_DETAIL: D E T A I L;
KW_WAIT: W A I T;
KW_RESOURCE: R E S O U R C E;
KW_PLAN: P L A N;
KW_QUERY_PARALLELISM: Q U E R Y '_' P A R A L L E L I S M;
KW_PLANS: P L A N S;
KW_ACTIVATE: A C T I V A T E;
KW_DEFAULT: D E F A U L T;
KW_CHECK: C H E C K;
KW_POOL: P O O L;
KW_MOVE: M O V E;
KW_DO: D O;
KW_ALLOC_FRACTION: A L L O C  '_' F R A C T I O N;
KW_SCHEDULING_POLICY: S C H E D U L I N G '_' P O L I C Y;
KW_PATH: P A T H ;
KW_MAPPING: M A P P I N G;
KW_WORKLOAD: W O R K L O A D;
KW_MANAGEMENT: M A N A G E M E N T;
KW_ACTIVE: A C T I V E;
KW_UNMANAGED: U N M A N A G E D;
KW_APPLICATION: A P P L I C A T I O N;
KW_SYNC: S Y N C;

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
fragment
Letter
    : 'a'..'z' | 'A'..'Z'
    ;

fragment
HexDigit
    : 'a'..'f' | 'A'..'F'
    ;

fragment
Digit
    : '0'..'9'
    ;

fragment
Exponent
    : ('e' | 'E') ( PLUS|MINUS )? (Digit)+
    ;

fragment
RegexComponent
    : 'a'..'z' | 'A'..'Z' | '0'..'9' | '_'
    | PLUS | STAR | QUESTION | MINUS | DOT
    | LPAREN | RPAREN | LSQUARE | RSQUARE | LCURLY | RCURLY
    | BITWISEXOR | BITWISEOR | DOLLAR | '!'
    ;

fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];

StringLiteral
    : ( '\'' ( ~('\''|'\\') | ('\\' .) )* '\''
    | '"' ( ~('"'|'\\') | ('\\' .) )* '"'
    )+
    ;

CharSetLiteral
    : StringLiteral
    | '0' 'X' (HexDigit|Digit)+
    ;

IntegralLiteral
    : (Digit)+ ('L' | 'S' | 'Y')
    ;

NumberLiteral
    : Number ('D' | 'B' 'D')
    ;

ByteLengthLiteral
    : (Digit)+ ('b' | 'B' | 'k' | 'K' | 'm' | 'M' | 'g' | 'G')
    ;

Number
    : (Digit)+ ( DOT (Digit)* (Exponent)? | Exponent)?
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
    | QuotedIdentifier  /* though at the language level we allow all Identifiers to be QuotedIdentifiers;
                                              at the API level only columns are allowed to be of this form */
    | '`' RegexComponent+ '`'
    ;

QuotedIdentifier
    :
    '`'  ( '``' | ~('`') )* '`'
    ;

CharSetName
    : '_' (Letter | Digit | '_' | '-' | '.' | ':' )+
    ;

WS  : (' '|'\r'|'\t'|'\n') -> channel(HIDDEN)
    ;

LINE_COMMENT
    : '--' (~('\n'|'\r'))* -> channel(HIDDEN)
    ;

/*
QUERY_HINT有可能是一般注释，这时直接忽略。
也有可能是查询注释，这时用原来的逻辑提取出来
*/
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
