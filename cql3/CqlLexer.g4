
lexer grammar CqlLexer;


LR_BRACKET
   : '('
   ;


RR_BRACKET
   : ')'
   ;


LC_BRACKET
   : '{'
   ;


RC_BRACKET
   : '}'
   ;


LS_BRACKET
   : '['
   ;


RS_BRACKET
   : ']'
   ;


COMMA
   : ','
   ;


SEMI
   : ';'
   ;


COLON
   : ':'
   ;


SPACE
   : [ \t\r\n] + -> channel (HIDDEN)
   ;


SPEC_MYSQL_COMMENT
   : '/*!' . +? '*/' -> channel (HIDDEN)
   ;


COMMENT_INPUT
   : '/*' .*? '*/' -> channel (HIDDEN)
   ;


LINE_COMMENT
   : (('-- ' | '#' | '//') ~ [\r\n]* ('\r'? '\n' | EOF) | '--' ('\r'? '\n' | EOF)) -> channel (HIDDEN)
   ;


DOT
   : '.'
   ;


STAR
   : '*'
   ;


DIVIDE
   : '/'
   ;


MODULE
   : '%'
   ;


PLUS
   : '+'
   ;


MINUSMINUS
   : '--'
   ;


MINUS
   : '-'
   ;


DQUOTE
   : '"'
   ;


SQUOTE
   : '\''
   ;

// Keywords

K_ADD
   : A D D | 'ADD'
   ;


K_AGGREGATE
   : A G G R E G A T E | 'AGGREGATE'
   ;


K_ALL
   : A L L | 'ALL'
   ;


K_ALLOW
   : A L L O W | 'ALLOW'
   ;


K_ALTER
   : A L T E R | 'ALTER'
   ;


K_AND
   : A N D | 'AND'
   ;


K_ANY
   : A N Y | 'ANY'
   ;


K_APPLY
   : A P P L Y | 'APPLY'
   ;


K_AS
   : A S | 'AS'
   ;


K_ASC
   : A S C | 'ASC'
   ;


K_AUTHORIZE
   : A U T H O R I Z E | 'AUTHORIZE'
   ;


K_BATCH
   : B A T C H | 'BATCH'
   ;


K_BEGIN
   : B E G I N | 'BEGIN'
   ;


K_BY
   : B Y | 'BY'
   ;


K_CALLED
   : C A L L E D | 'CALLED'
   ;


K_CLUSTERING
   : C L U S T E R I N G | 'CLUSTERING'
   ;


K_COLUMNFAMILY
   : C O L U M N F A M I L Y | 'COLUMNFAMILY'
   ;


K_COMPACT
   : C O M P A C T | 'COMPACT'
   ;


K_CONSISTENCY
   : C O N S I S T E N C Y | 'CONSISTENCY'
   ;


K_CONTAINS
   : C O N T A I N S | 'CONTAINS'
   ;


K_CREATE
   : C R E A T E | 'CREATE'
   ;


K_CUSTOM
   : C U S T O M | 'CUSTOM'
   ;


K_DELETE
   : D E L E T E | 'DELETE'
   ;


K_DESC
   : D E S C | 'DESC'
   ;


K_DESCRIBE
   : D E S C R I B E | 'DESCRIBE'
   ;


K_DISTINCT
   : D I S T I N C T | 'DISTINCT'
   ;


K_DROP
   : D R O P | 'DROP'
   ;


K_DURABLE_WRITES
   : D U R A B L E '_' W R I T E S | 'DURABLE_WRITES'
   ;


K_EACH_QUORUM
   : E A C H '_' Q U O R U M | 'EACH_QUORUM'
   ;


K_ENTRIES
   : E N T R I E S | 'ENTRIES'
   ;


K_EXECUTE
   : E X E C U T E | 'EXECUTE'
   ;


K_EXISTS
   : E X I S T S | 'EXISTS'
   ;


K_FALSE
   : F A L S E | 'FALSE'
   ;


K_FILTERING
   : F I L T E R I N G | 'FILTERING'
   ;


K_FINALFUNC
   : F I N A L F U N C | 'FINALFUNC'
   ;


K_FROM
   : F R O M | 'FROM'
   ;


K_FULL
   : F U L L | 'FULL'
   ;


K_FUNCTION
   : F U N C T I O N | 'FUNCTION'
   ;


K_FUNCTIONS
   : F U N C T I O N S | 'FUNCTIONS'
   ;


K_GRANT
   : G R A N T | 'GRANT'
   ;


K_IF
   : I F | 'IF'
   ;


K_IN
   : I N | 'IN'
   ;


K_INDEX
   : I N D E X | 'INDEX'
   ;


K_INFINITY
   : I N F I N I T Y | 'INFINITY'
   ;


K_INITCOND
   : I N I T C O N D | 'INITCOND'
   ;


K_INPUT
   : I N P U T | 'INPUT'
   ;


K_INSERT
   : I N S E R T | 'INSERT'
   ;


K_INTO
   : I N T O | 'INTO'
   ;


K_IS
   : I S | 'IS'
   ;

K_JSON
   : J S O N | 'JSON'
   ;


K_KEY
   : K E Y | 'KEY'
   ;


K_KEYS
   : K E Y S | 'KEYS'
   ;


K_KEYSPACE
   : K E Y S P A C E | 'KEYSPACE'
   ;


K_KEYSPACES
   : K E Y S P A C E S | 'KEYSPACES'
   ;


K_LANGUAGE
   : L A N G U A G E | 'LANGUAGE'
   ;


K_LEVEL
   : L E V E L | 'LEVEL'
   ;


K_LIMIT
   : L I M I T | 'LIMIT'
   ;


K_LOCAL_ONE
   : L O C A L '_' O N E | 'LOCAL_ONE'
   ;


K_LOCAL_QUORUM
   : L O C A L '_' Q U O R U M | 'LOCAL_QUORUM'
   ;


K_LOGGED
   : L O G G E D | 'LOGGED'
   ;


K_LOGIN
   : L O G I N | 'LOGIN'
   ;


K_MATERIALIZED
   : M A T E R I A L I Z E D | 'MATERIALIZED'
   ;


K_MODIFY
   : M O D I F Y | 'MODIFY'
   ;


K_NAN
   : N A N | 'NAN'
   ;


K_NORECURSIVE
   : N O R E C U R S I V E | 'NORECURSIVE'
   ;


K_NOSUPERUSER
   : N O S U P E R U S E R | 'NOSUPERUSER'
   ;


K_NOT
   : N O T | 'NOT'
   ;


K_NULL
   : N U L L | 'NULL'
   ;


K_OF
   : O F | 'OF'
   ;


K_ON
   : O N | 'ON'
   ;


K_ONE
   : O N E | 'ONE'
   ;


K_OPTIONS
   : O P T I O N S | 'OPTIONS'
   ;


K_OR
   : O R | 'OR'
   ;


K_ORDER
   : O R D E R | 'ORDER'
   ;


K_PARTITION
   : P A R T I T I O N | 'PARTITION'
   ;


K_PASSWORD
   : P A S S W O R D | 'PASSWORD'
   ;


K_PER
   : P E R | 'PER'
   ;


K_PERMISSION
   : P E R M I S S I O N | 'PERMISSION'
   ;


K_PERMISSIONS
   : P E R M I S S I O N S | 'PERMISSIONS'
   ;


K_PRIMARY
   : P R I M A R Y | 'PRIMARY'
   ;


K_QUORUM
   : Q U O R U M | 'QUORUM'
   ;


K_RENAME
   : R E N A M E | 'RENAME'
   ;


K_REPLACE
   : R E P L A C E | 'REPLACE'
   ;


K_REPLICATION
   : R E P L I C A T I O N | 'REPLICATION'
   ;


K_RETURNS
   : R E T U R N S | 'RETURNS'
   ;


K_REVOKE
   : R E V O K E | 'REVOKE'
   ;


K_ROLE
   : R O L E | 'ROLE'
   ;


K_ROLES
   : R O L E S | 'ROLES'
   ;


K_SCHEMA
   : S C H E M A | 'SCHEMA'
   ;


K_SELECT
   : S E L E C T | 'SELECT'
   ;


K_SET
   : S E T | 'SET'
   ;


K_SFUNC
   : S F U N C | 'SFUNC'
   ;


K_STATIC
   : S T A T I C | 'STATIC'
   ;


K_STORAGE
   : S T O R A G E | 'STORAGE'
   ;


K_STYPE
   : S T Y P E | 'STYPE'
   ;


K_SUPERUSER
   : S U P E R U S E R | 'SUPERUSER'
   ;


K_TABLE
   : T A B L E | 'TABLE'
   ;


K_THREE
   : T H R E E | 'THREE'
   ;


K_TIMESTAMP
   : T I M E S T A M P | 'TIMESTAMP'
   ;


K_TO
   : T O | 'TO'
   ;


K_TOKEN
   : T O K E N | 'TOKEN'
   ;


K_TRIGGER
   : T R I G G E R | 'TRIGGER'
   ;


K_TRUE
   : T R U E | 'TRUE'
   ;


K_TRUNCATE
   : T R U N C A T E | 'TRUNCATE'
   ;


K_TTL
   : T T L | 'TTL'
   ;


K_TWO
   : T W O | 'TWO'
   ;


K_TYPE
   : T Y P E | 'TYPE'
   ;


K_UNLOGGED
   : U N L O G G E D | 'UNLOGGED'
   ;


K_UPDATE
   : U P D A T E | 'UPDATE'
   ;


K_USE
   : U S E | 'USE'
   ;


K_USER
   : U S E R | 'USER'
   ;


K_USING
   : U S I N G | 'USING'
   ;


K_UUID
   : U U I D | 'UUID'
   ;


K_VALUES
   : V A L U E S | 'VALUES'
   ;


K_VIEW
   : V I E W | 'VIEW'
   ;


K_WHERE
   : W H E R E | 'WHERE'
   ;


K_WITH
   : W I T H | 'WITH'
   ;


K_WRITETIME
   : W R I T E T I M E | 'WRITETIME'
   ;


K_ASCII
   : A S C I I
   ;


K_BIGINT
   : B I G I N T
   ;


K_BLOB
   : B L O B
   ;


K_BOOLEAN
   : B O O L E A N
   ;


K_COUNTER
   : C O U N T E R
   ;


K_DATE
   : D A T E
   ;


K_DECIMAL
   : D E C I M A L
   ;


K_DOUBLE
   : D O U B L E
   ;


K_FLOAT
   : F L O A T
   ;


K_FROZEN
   : F R O Z E N
   ;


K_INET
   : I N E T
   ;


K_INT
   : I N T
   ;


K_LIST
   : L I S T
   ;


K_MAP
   : M A P
   ;


K_SMALLINT
   : S M A L L I N T
   ;


K_TEXT
   : T E X T
   ;


K_TIMEUUID
   : T I M E U U I D
   ;


K_TIME
   : T I M E
   ;


K_TINYINT
   : T I N Y I N T
   ;


K_TUPLE
   : T U P L E
   ;


K_VARCHAR
   : V A R C H A R
   ;


K_VARINT
   : V A R I N T
   ;


fragment A
   : [aA]
   ;


fragment B
   : [bB]
   ;


fragment C
   : [cC]
   ;


fragment D
   : [dD]
   ;


fragment E
   : [eE]
   ;


fragment F
   : [fF]
   ;


fragment G
   : [gG]
   ;


fragment H
   : [hH]
   ;


fragment I
   : [iI]
   ;


fragment J
   : [jJ]
   ;


fragment K
   : [kK]
   ;


fragment L
   : [lL]
   ;


fragment M
   : [mM]
   ;


fragment N
   : [nN]
   ;


fragment O
   : [oO]
   ;


fragment P
   : [pP]
   ;


fragment Q
   : [qQ]
   ;


fragment R
   : [rR]
   ;


fragment S
   : [sS]
   ;


fragment T
   : [tT]
   ;


fragment U
   : [uU]
   ;


fragment V
   : [vV]
   ;


fragment W
   : [wW]
   ;


fragment X
   : [xX]
   ;


fragment Y
   : [yY]
   ;


fragment Z
   : [zZ]
   ;


fragment CODE_BLOCK_DELIMITER
   : '$$'
   ;


fragment CODE_BLOCK_FRAG
   : '$$' (~ '$' | ('$' ~ ('$')))* '$$'
   ;


fragment HEX_4DIGIT
   : [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F] [0-9a-fA-F]
   ;


fragment OBJECT_NAME_FRAG
   : [a-zA-Z] [A-Za-z0-9_$]* | '"' ~'"' + '"'
   ;


fragment SQUOTA_STRING
   : '\'' ('\\' . | '\'\'' | ~ ('\'' | '\\'))* '\''
   ;


CODE_BLOCK
   : CODE_BLOCK_FRAG
   ;


STRING_LITERAL
   : SQUOTA_STRING
   ;


DECIMAL_LITERAL
   : DEC_DIGIT +
   ;


FLOAT_LITERAL
   : (MINUS)? [0-9] + (DOT [0-9] +)?
   ;


HEXADECIMAL_LITERAL
   : 'X' '\'' (HEX_DIGIT HEX_DIGIT) + '\'' | '0X' HEX_DIGIT +
   ;


REAL_LITERAL
   : (DEC_DIGIT +)? '.' DEC_DIGIT + | DEC_DIGIT + '.' EXPONENT_NUM_PART | (DEC_DIGIT +)? '.' (DEC_DIGIT + EXPONENT_NUM_PART) | DEC_DIGIT + EXPONENT_NUM_PART
   ;


OBJECT_NAME
   : OBJECT_NAME_FRAG
   ;


UUID
   : HEX_4DIGIT HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT '-' HEX_4DIGIT HEX_4DIGIT HEX_4DIGIT
   ;


fragment HEX_DIGIT
   : [0-9A-F]
   ;


fragment DEC_DIGIT
   : [0-9]
   ;


fragment EXPONENT_NUM_PART
   : 'E' '-'? DEC_DIGIT +
   ;


fragment OPERATOR_EQ_FRAG
   : '='
   ;


fragment OPERATOR_LT_FRAG
   : '<'
   ;


fragment OPERATOR_GT_FRAG
   : '>'
   ;


fragment OPERATOR_GTE_FRAG
   : '>='
   ;


fragment OPERATOR_LTE_FRAG
   : '<='
   ;


OPERATOR_EQ
   : OPERATOR_EQ_FRAG
   ;


OPERATOR_LT
   : OPERATOR_LT_FRAG
   ;


OPERATOR_GT
   : OPERATOR_GT_FRAG
   ;


OPERATOR_LTE
   : OPERATOR_LTE_FRAG
   ;


OPERATOR_GTE
   : OPERATOR_GTE_FRAG
   ;


K_USERS
   : U S E R S | 'USERS'
   ;
