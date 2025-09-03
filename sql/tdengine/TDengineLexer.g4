lexer grammar TDengineLexer;


options { caseInsensitive = true; }
// =============================================================================

// Keywords (from keywordTable)

// =============================================================================

ACCOUNT
   : 'ACCOUNT'
   ;

ACCOUNTS
   : 'ACCOUNTS'
   ;

ADD
   : 'ADD'
   ;

AGGREGATE
   : 'AGGREGATE'
   ;

ALL
   : 'ALL'
   ;

ALTER
   : 'ALTER'
   ;

ALIVE
   : 'ALIVE'
   ;

ANALYZE
   : 'ANALYZE'
   ;

AND
   : 'AND'
   ;

ANTI
   : 'ANTI'
   ;

ANODE
   : 'ANODE'
   ;

ANODES
   : 'ANODES'
   ;

ANOMALY_WINDOW
   : 'ANOMALY_WINDOW'
   ;

APPS
   : 'APPS'
   ;

ARBGROUPS
   : 'ARBGROUPS'
   ;

AS
   : 'AS'
   ;

ASC
   : 'ASC'
   ;

ASOF
   : 'ASOF'
   ;

ASSIGN
   : 'ASSIGN'
   ;

AUTO
   : 'AUTO'
   ;

BALANCE
   : 'BALANCE'
   ;

BATCH_SCAN
   : 'BATCH_SCAN'
   ;

BETWEEN
   : 'BETWEEN'
   ;

BIGINT
   : 'BIGINT'
   ;

BINARY
   : 'BINARY'
   ;

BLOB
   : 'BLOB'
   ;

BNODE
   : 'BNODE'
   ;

BNODES
   : 'BNODES'
   ;

BOOL
   : 'BOOL'
   ;

BOTH
   : 'BOTH'
   ;

BUFFER
   : 'BUFFER'
   ;

BUFSIZE
   : 'BUFSIZE'
   ;

BY
   : 'BY'
   ;

BWLIMIT
   : 'BWLIMIT'
   ;

CACHE
   : 'CACHE'
   ;

CACHEMODEL
   : 'CACHEMODEL'
   ;

CACHESIZE
   : 'CACHESIZE'
   ;

CALC_NOTIFY_ONLY
   : 'CALC_NOTIFY_ONLY'
   ;

CASE
   : 'CASE'
   ;

CAST
   : 'CAST'
   ;

CHILD
   : 'CHILD'
   ;

CLIENT_VERSION
   : 'CLIENT_VERSION'
   ;

CLUSTER
   : 'CLUSTER'
   ;

COLUMN
   : 'COLUMN'
   ;

COLS
   : 'COLS'
   ;

COMMENT
   : 'COMMENT'
   ;

COMP
   : 'COMP'
   ;

COMPACT
   : 'COMPACT'
   ;

COMPACTS
   : 'COMPACTS'
   ;

COMPACT_INTERVAL
   : 'COMPACT_INTERVAL'
   ;

COMPACT_TIME_OFFSET
   : 'COMPACT_TIME_OFFSET'
   ;

COMPACT_TIME_RANGE
   : 'COMPACT_TIME_RANGE'
   ;

COMPOSITE
   : 'COMPOSITE'
   ;

CONNECTION
   : 'CONNECTION'
   ;

CONNECTIONS
   : 'CONNECTIONS'
   ;

CONNS
   : 'CONNS'
   ;

CONSUMER
   : 'CONSUMER'
   ;

CONSUMERS
   : 'CONSUMERS'
   ;

CONTAINS
   : 'CONTAINS'
   ;

COUNT
   : 'COUNT'
   ;

COUNT_WINDOW
   : 'COUNT_WINDOW'
   ;

CREATE
   : 'CREATE'
   ;

CREATEDB
   : 'CREATEDB'
   ;

CURRENT_USER
   : 'CURRENT_USER'
   ;

DATABASE
   : 'DATABASE'
   ;

DATABASES
   : 'DATABASES'
   ;

DBS
   : 'DBS'
   ;

DECIMAL
   : 'DECIMAL'
   ;

DELETE
   : 'DELETE'
   ;

DELETE_MARK
   : 'DELETE_MARK'
   ;

DELETE_OUTPUT_TABLE
   : 'DELETE_OUTPUT_TABLE'
   ;

DELETE_RECALC
   : 'DELETE_RECALC'
   ;

DESC
   : 'DESC'
   ;

DESCRIBE
   : 'DESCRIBE'
   ;

DISK_INFO
   : 'DISK_INFO'
   ;

DISTINCT
   : 'DISTINCT'
   ;

DISTRIBUTED
   : 'DISTRIBUTED'
   ;

DNODE
   : 'DNODE'
   ;

DNODES
   : 'DNODES'
   ;

DOUBLE
   : 'DOUBLE'
   ;

DROP
   : 'DROP'
   ;

DURATION
   : 'DURATION'
   ;

ELSE
   : 'ELSE'
   ;

ENABLE
   : 'ENABLE'
   ;

ENCRYPTIONS
   : 'ENCRYPTIONS'
   ;

ENCRYPT_ALGORITHM
   : 'ENCRYPT_ALGORITHM'
   ;

ENCRYPT_KEY
   : 'ENCRYPT_KEY'
   ;

END
   : 'END'
   ;

EVENT_TYPE
   : 'EVENT_TYPE'
   ;

EVENT_WINDOW
   : 'EVENT_WINDOW'
   ;

EVERY
   : 'EVERY'
   ;

EXISTS
   : 'EXISTS'
   ;

EXPIRED_TIME
   : 'EXPIRED_TIME'
   ;

EXPLAIN
   : 'EXPLAIN'
   ;

FILE
   : 'FILE'
   ;

FILL
   : 'FILL'
   ;

FILL_HISTORY
   : 'FILL_HISTORY'
   ;

FILL_HISTORY_FIRST
   : 'FILL_HISTORY_FIRST'
   ;

FIRST
   : 'FIRST'
   ;

FLOAT
   : 'FLOAT'
   ;

FLUSH
   : 'FLUSH'
   ;

FOR
   : 'FOR'
   ;

FORCE
   : 'FORCE'
   ;

FORCE_OUTPUT
   : 'FORCE_OUTPUT'
   ;

FROM
   : 'FROM'
   ;

FULL
   : 'FULL'
   ;

FUNCTION
   : 'FUNCTION'
   ;

FUNCTIONS
   : 'FUNCTIONS'
   ;

GEOMETRY
   : 'GEOMETRY'
   ;

GRANT
   : 'GRANT'
   ;

GRANTS
   : 'GRANTS'
   ;

GROUP
   : 'GROUP'
   ;

HASH_JOIN
   : 'HASH_JOIN'
   ;

HAVING
   : 'HAVING'
   ;

HOST
   : 'HOST'
   ;

IF
   : 'IF'
   ;

IGNORE
   : 'IGNORE'
   ;

IGNORE_DISORDER
   : 'IGNORE_DISORDER'
   ;

IGNORE_NODATA_TRIGGER
   : 'IGNORE_NODATA_TRIGGER'
   ;

IMPORT
   : 'IMPORT'
   ;

IN
   : 'IN'
   ;

INDEX
   : 'INDEX'
   ;

INDEXES
   : 'INDEXES'
   ;

INNER
   : 'INNER'
   ;

INSERT
   : 'INSERT'
   ;

INT
   : 'INT'
   ;

INTEGER
   : 'INTEGER'
   ;

INTERVAL
   : 'INTERVAL'
   ;

INTO
   : 'INTO'
   ;

IS
   : 'IS'
   ;

IS_IMPORT
   : 'IS_IMPORT'
   ;

JLIMIT
   : 'JLIMIT'
   ;

JOIN
   : 'JOIN'
   ;

JSON
   : 'JSON'
   ;

KEEP
   : 'KEEP'
   ;

KEEP_TIME_OFFSET
   : 'KEEP_TIME_OFFSET'
   ;

KEY
   : 'KEY'
   ;

KILL
   : 'KILL'
   ;

LANGUAGE
   : 'LANGUAGE'
   ;

LAST
   : 'LAST'
   ;

LAST_ROW
   : 'LAST_ROW'
   ;

LEADER
   : 'LEADER'
   ;

LEADING
   : 'LEADING'
   ;

LEFT
   : 'LEFT'
   ;

LICENCES
   : 'LICENCES'
   ;

LIKE
   : 'LIKE'
   ;

LIMIT
   : 'LIMIT'
   ;

LINEAR
   : 'LINEAR'
   ;

LOCAL
   : 'LOCAL'
   ;

LOGS
   : 'LOGS'
   ;

LOW_LATENCY_CALC
   : 'LOW_LATENCY_CALC'
   ;

MACHINES
   : 'MACHINES'
   ;

MATCH
   : 'MATCH'
   ;

MAXROWS
   : 'MAXROWS'
   ;

MAX_DELAY
   : 'MAX_DELAY'
   ;

MEDIUMBLOB
   : 'MEDIUMBLOB'
   ;

MERGE
   : 'MERGE'
   ;

META
   : 'META'
   ;

META_ONLY
   : 'META_ONLY'
   ;

MINROWS
   : 'MINROWS'
   ;

MINUS
   : 'MINUS'
   ;

MNODE
   : 'MNODE'
   ;

MNODES
   : 'MNODES'
   ;

MODIFY
   : 'MODIFY'
   ;

MODULES
   : 'MODULES'
   ;

MOUNT
   : 'MOUNT'
   ;

MOUNTS
   : 'MOUNTS'
   ;

NCHAR
   : 'NCHAR'
   ;

NEAR
   : 'NEAR'
   ;

NEXT
   : 'NEXT'
   ;

NMATCH
   : 'NMATCH'
   ;

NONE
   : 'NONE'
   ;

NORMAL
   : 'NORMAL'
   ;

NOT
   : 'NOT'
   ;

NOTIFY
   : 'NOTIFY'
   ;

NOTIFY_HISTORY
   : 'NOTIFY_HISTORY'
   ;

NOTIFY_OPTIONS
   : 'NOTIFY_OPTIONS'
   ;

NOW
   : 'NOW'
   ;

NO_BATCH_SCAN
   : 'NO_BATCH_SCAN'
   ;

NULL
   : 'NULL'
   ;

NULL_F
   : 'NULL_F'
   ;

NULLS
   : 'NULLS'
   ;

OFFSET
   : 'OFFSET'
   ;

ON
   : 'ON'
   ;

ONLY
   : 'ONLY'
   ;

OR
   : 'OR'
   ;

ORDER
   : 'ORDER'
   ;

OUTER
   : 'OUTER'
   ;

OUTPUTTYPE
   : 'OUTPUTTYPE'
   ;

OUTPUT_SUBTABLE
   : 'OUTPUT_SUBTABLE'
   ;

PAGES
   : 'PAGES'
   ;

PAGESIZE
   : 'PAGESIZE'
   ;

PARA_TABLES_SORT
   : 'PARA_TABLES_SORT'
   ;

PARTITION
   : 'PARTITION'
   ;

PARTITION_FIRST
   : 'PARTITION_FIRST'
   ;

PASS
   : 'PASS'
   ;

PERIOD
   : 'PERIOD'
   ;

PI
   : 'PI'
   ;

PORT
   : 'PORT'
   ;

POSITION
   : 'POSITION'
   ;

PPS
   : 'PPS'
   ;

PRECISION
   : 'PRECISION'
   ;

PREV
   : 'PREV'
   ;

PRIMARY
   : 'PRIMARY'
   ;

PRE_FILTER
   : 'PRE_FILTER'
   ;

PRIVILEGES
   : 'PRIVILEGES'
   ;

QNODE
   : 'QNODE'
   ;

QNODES
   : 'QNODES'
   ;

QTIME
   : 'QTIME'
   ;

QUERIES
   : 'QUERIES'
   ;

QUERY
   : 'QUERY'
   ;

RAND
   : 'RAND'
   ;

RANGE
   : 'RANGE'
   ;

RATIO
   : 'RATIO'
   ;

READ
   : 'READ'
   ;

RECALCULATE
   : 'RECALCULATE'
   ;

RECURSIVE
   : 'RECURSIVE'
   ;

REDISTRIBUTE
   : 'REDISTRIBUTE'
   ;

REGEXP
   : 'REGEXP'
   ;

RENAME
   : 'RENAME'
   ;

REPLACE
   : 'REPLACE'
   ;

REPLICA
   : 'REPLICA'
   ;

REPLICAS
   : 'REPLICAS'
   ;

RESET
   : 'RESET'
   ;

RESTORE
   : 'RESTORE'
   ;

RETENTIONS
   : 'RETENTIONS'
   ;

REVOKE
   : 'REVOKE'
   ;

RIGHT
   : 'RIGHT'
   ;

ROLLUP
   : 'ROLLUP'
   ;

SCHEMALESS
   : 'SCHEMALESS'
   ;

SCORES
   : 'SCORES'
   ;

SELECT
   : 'SELECT'
   ;

SEMI
   : 'SEMI'
   ;

SERVER_STATUS
   : 'SERVER_STATUS'
   ;

SERVER_VERSION
   : 'SERVER_VERSION'
   ;

SESSION
   : 'SESSION'
   ;

SET
   : 'SET'
   ;

SHOW
   : 'SHOW'
   ;

SINGLE_STABLE
   : 'SINGLE_STABLE'
   ;

SKIP_TSMA
   : 'SKIP_TSMA'
   ;

SLIDING
   : 'SLIDING'
   ;

SLIMIT
   : 'SLIMIT'
   ;

SMA
   : 'SMA'
   ;

SMALLDATA_TS_SORT
   : 'SMALLDATA_TS_SORT'
   ;

SMALLINT
   : 'SMALLINT'
   ;

SNODE
   : 'SNODE'
   ;

SNODES
   : 'SNODES'
   ;

SORT_FOR_GROUP
   : 'SORT_FOR_GROUP'
   ;

SOFFSET
   : 'SOFFSET'
   ;

SPLIT
   : 'SPLIT'
   ;

SSMIGRATE
   : 'SSMIGRATE'
   ;

SS_CHUNKPAGES
   : 'SS_CHUNKPAGES'
   ;

SS_COMPACT
   : 'SS_COMPACT'
   ;

SS_KEEPLOCAL
   : 'SS_KEEPLOCAL'
   ;

STABLE
   : 'STABLE'
   ;

STABLES
   : 'STABLES'
   ;

START
   : 'START'
   ;

STATE
   : 'STATE'
   ;

STATE_WINDOW
   : 'STATE_WINDOW'
   ;

STOP
   : 'STOP'
   ;

STORAGE
   : 'STORAGE'
   ;

STREAM
   : 'STREAM'
   ;

STREAMS
   : 'STREAMS'
   ;

STREAM_OPTIONS
   : 'STREAM_OPTIONS'
   ;

STRICT
   : 'STRICT'
   ;

STT_TRIGGER
   : 'STT_TRIGGER'
   ;

SUBSCRIBE
   : 'SUBSCRIBE'
   ;

SUBSCRIPTIONS
   : 'SUBSCRIPTIONS'
   ;

SUBSTR
   : 'SUBSTR'
   ;

SUBSTRING
   : 'SUBSTRING'
   ;

SYSINFO
   : 'SYSINFO'
   ;

SYSTEM
   : 'SYSTEM'
   ;

TABLE
   : 'TABLE'
   ;

TABLES
   : 'TABLES'
   ;

TABLE_PREFIX
   : 'TABLE_PREFIX'
   ;

TABLE_SUFFIX
   : 'TABLE_SUFFIX'
   ;

TAG
   : 'TAG'
   ;

TAGS
   : 'TAGS'
   ;

TBNAME
   : 'TBNAME'
   ;

THEN
   : 'THEN'
   ;

TIMESTAMP
   : 'TIMESTAMP'
   ;

TIMEZONE
   : 'TIMEZONE'
   ;

TINYINT
   : 'TINYINT'
   ;

TO
   : 'TO'
   ;

TODAY
   : 'TODAY'
   ;

TOPIC
   : 'TOPIC'
   ;

TOPICS
   : 'TOPICS'
   ;

TRAILING
   : 'TRAILING'
   ;

TRANSACTION
   : 'TRANSACTION'
   ;

TRANSACTIONS
   : 'TRANSACTIONS'
   ;

TRIM
   : 'TRIM'
   ;

TROWS
   : 'TROWS'
   ;

TRUE_FOR
   : 'TRUE_FOR'
   ;

TSDB_PAGESIZE
   : 'TSDB_PAGESIZE'
   ;

TSERIES
   : 'TSERIES'
   ;

TSMA
   : 'TSMA'
   ;

TSMAS
   : 'TSMAS'
   ;

TTL
   : 'TTL'
   ;

UNION
   : 'UNION'
   ;

UNSIGNED
   : 'UNSIGNED'
   ;

UNSAFE
   : 'UNSAFE'
   ;

UNTREATED
   : 'UNTREATED'
   ;

UPDATE
   : 'UPDATE'
   ;

USE
   : 'USE'
   ;

USER
   : 'USER'
   ;

USERS
   : 'USERS'
   ;

USING
   : 'USING'
   ;

VALUE
   : 'VALUE'
   ;

VALUES
   : 'VALUES'
   ;

VALUE_F
   : 'VALUE_F'
   ;

VARBINARY
   : 'VARBINARY'
   ;

VARCHAR
   : 'VARCHAR'
   ;

VARIABLES
   : 'VARIABLES'
   ;

VERBOSE
   : 'VERBOSE'
   ;

VGROUP
   : 'VGROUP'
   ;

VGROUPS
   : 'VGROUPS'
   ;

VIEW
   : 'VIEW'
   ;

VIEWS
   : 'VIEWS'
   ;

VIRTUAL
   : 'VIRTUAL'
   ;

VNODE
   : 'VNODE'
   ;

VNODES
   : 'VNODES'
   ;

VTABLE
   : 'VTABLE'
   ;

VTABLES
   : 'VTABLES'
   ;

WAL_FSYNC_PERIOD
   : 'WAL_FSYNC_PERIOD'
   ;

WAL_LEVEL
   : 'WAL_LEVEL'
   ;

WAL_RETENTION_PERIOD
   : 'WAL_RETENTION_PERIOD'
   ;

WAL_RETENTION_SIZE
   : 'WAL_RETENTION_SIZE'
   ;

WAL_ROLL_PERIOD
   : 'WAL_ROLL_PERIOD'
   ;

WAL_SEGMENT_SIZE
   : 'WAL_SEGMENT_SIZE'
   ;

WATERMARK
   : 'WATERMARK'
   ;

WHEN
   : 'WHEN'
   ;

WHERE
   : 'WHERE'
   ;

WINDOW
   : 'WINDOW'
   ;

WINDOW_CLOSE
   : 'WINDOW_CLOSE'
   ;

WINDOW_OFFSET
   : 'WINDOW_OFFSET'
   ;

WINDOW_OPEN
   : 'WINDOW_OPEN'
   ;

WITH
   : 'WITH'
   ;

WRITE
   : 'WRITE'
   ;
   // Internal/System keywords (prefixed with underscore)
   
ROWTS
   : '_C0'
   | '_ROWTS'
   ;

IROWTS
   : '_IROWTS'
   ;

IROWTS_ORIGIN
   : '_IROWTS_ORIGIN'
   ;

ISFILLED
   : '_ISFILLED'
   ;

QDURATION
   : '_QDURATION'
   ;

QEND
   : '_QEND'
   ;

QSTART
   : '_QSTART'
   ;

QTAGS
   : '_TAGS'
   ;

WDURATION
   : '_WDURATION'
   ;

WEND
   : '_WEND'
   ;

WSTART
   : '_WSTART'
   ;

FLOW
   : '_FLOW'
   ;

FHIGH
   : '_FHIGH'
   ;

FROWTS
   : '_FROWTS'
   ;

TPREV_TS
   : '_TPREV_TS'
   ;

TCURRENT_TS
   : '_TCURRENT_TS'
   ;

TNEXT_TS
   : '_TNEXT_TS'
   ;

TWSTART
   : '_TWSTART'
   ;

TWEND
   : '_TWEND'
   ;

TWDURATION
   : '_TWDURATION'
   ;

TWROWNUM
   : '_TWROWNUM'
   ;

TPREV_LOCALTIME
   : '_TPREV_LOCALTIME'
   ;

TNEXT_LOCALTIME
   : '_TNEXT_LOCALTIME'
   ;

TLOCALTIME
   : '_TLOCALTIME'
   ;

TGRPID
   : '_TGRPID'
   ;
   // =============================================================================
   
   // Non-Keyword Tokens (Operators, Literals, etc.)
   
   // =============================================================================
   
NK_ARROW
   : '->'
   ;

NK_CONCAT
   : '||'
   ;

NK_LSHIFT
   : '<<'
   ;

NK_RSHIFT
   : '>>'
   ;

NK_LE
   : '<='
   ;

NK_GE
   : '>='
   ;

NK_NE
   : '<>'
   | '!='
   ;

NK_PH
   : '%%'
   ;

NK_PLUS
   : '+'
   ;

NK_MINUS
   : '-'
   ;

NK_STAR
   : '*'
   ;

NK_SLASH
   : '/'
   ;

NK_REM
   : '%'
   ;

NK_EQ
   : '='
   ;

NK_LT
   : '<'
   ;

NK_GT
   : '>'
   ;

NK_BITAND
   : '&'
   ;

NK_BITOR
   : '|'
   ;

NK_BITNOT
   : '~'
   ;

NK_LP
   : '('
   ;

NK_RP
   : ')'
   ;

NK_COMMA
   : ','
   ;

NK_SEMI
   : ';'
   ;

NK_COLON
   : ':'
   ;

NK_QUESTION
   : '?'
   ;

NK_DOT
   : '.'
   ;
   // Literals
   
NK_BOOL
   : 'TRUE'
   | 'FALSE'
   ;

NK_VARIABLE
   : DIGIT+ ('B' | 'U' | 'A' | 'S' | 'M' | 'H' | 'D' | 'N' | 'Y' | 'W')
   ;

NK_IPTOKEN
   : DIGIT+ '.' DIGIT+ '.' DIGIT+ '.' DIGIT+
   ;

NK_FLOAT
   : DIGIT+ '.' DIGIT* EXPONENT?
   | '.' DIGIT+ EXPONENT?
   | DIGIT+ EXPONENT
   ;

NK_HEX
   : '0' ('X') HEX_DIGIT+
   ;

NK_BIN
   : '0' ('B') ('0' | '1')+
   ;

NK_INTEGER
   : DIGIT+
   ;

NK_STRING
   : '\'' (~ ('\'' | '\\') | '\\' . | '\'\'')* '\''
   | '"' (~ ('"' | '\\') | '\\' . | '""')* '"'
   ;
   // Identifier (must be last to not consume keywords)
   
NK_ID
   : [A-Z_] [A-Z0-9_]*
   | '`' (~ '`' | '``')* '`'
   ;

NK_HINT
   : '/*+' .*? '*/'
   ;

NK_COMMENT
   : ('--' ~ [\r\n]* | '/*' .*? '*/') -> skip
   ;

NK_SPACE
   : [ \t\r\n\f]+ -> skip
   ;

fragment DIGIT
   : [0-9]
   ;

fragment HEX_DIGIT
   : [0-9A-F]
   ;

fragment EXPONENT
   : ('E') ('+' | '-')? DIGIT+
   ;

