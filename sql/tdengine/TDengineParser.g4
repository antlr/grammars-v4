parser grammar TDengineParser;


options { tokenVocab = TDengineLexer; }
sqlCommandList
   : cmd (';' cmd)* ';'? EOF
   ;
/*
 * =============================================================================
 * Main command rule (cmd) combining all possible SQL statements
 * =============================================================================
 */


cmd
   : CREATE ACCOUNT NK_ID PASS NK_STRING accountOptions
   | ALTER ACCOUNT NK_ID alterAccountOptions
   | CREATE USER userName PASS NK_STRING sysinfoOpt isCreatedbOpt isImportOpt whiteListOpt
   | ALTER USER userName PASS NK_STRING
   | ALTER USER userName ENABLE NK_INTEGER
   | ALTER USER userName SYSINFO NK_INTEGER
   | ALTER USER userName CREATEDB NK_INTEGER
   | ALTER USER userName ADD whiteList
   | ALTER USER userName DROP whiteList
   | DROP USER userName
   | GRANT privileges ON privLevel withClauseOpt TO userName
   | REVOKE privileges ON privLevel withClauseOpt FROM userName
   | CREATE ENCRYPT_KEY NK_STRING
   | CREATE ANODE NK_STRING
   | UPDATE ANODE NK_INTEGER
   | UPDATE ALL ANODES
   | DROP ANODE NK_INTEGER
   | CREATE DNODE dnodeEndpoint (PORT NK_INTEGER)?
   | DROP DNODE (NK_INTEGER | dnodeEndpoint) FORCE
   | DROP DNODE (NK_INTEGER | dnodeEndpoint) UNSAFE
   | ALTER DNODE NK_INTEGER NK_STRING NK_STRING?
   | ALTER ALL DNODES NK_STRING NK_STRING?
   | RESTORE DNODE NK_INTEGER
   | ALTER CLUSTER NK_STRING NK_STRING?
   | ALTER LOCAL NK_STRING NK_STRING?
   | CREATE QNODE ON DNODE NK_INTEGER
   | DROP QNODE ON DNODE NK_INTEGER
   | RESTORE QNODE ON DNODE NK_INTEGER
   | CREATE SNODE ON DNODE NK_INTEGER
   | DROP SNODE ON DNODE NK_INTEGER
   | CREATE BNODE ON DNODE NK_INTEGER bnodeOptions
   | DROP BNODE ON DNODE NK_INTEGER
   | CREATE MNODE ON DNODE NK_INTEGER
   | DROP MNODE ON DNODE NK_INTEGER
   | RESTORE MNODE ON DNODE NK_INTEGER
   | RESTORE VNODE ON DNODE NK_INTEGER
   | CREATE DATABASE notExistsOpt dbName dbOptions
   | DROP DATABASE existsOpt dbName forceOpt
   | USE dbName
   | ALTER DATABASE dbName alterDbOptions
   | FLUSH DATABASE dbName
   | TRIM DATABASE dbName speedOpt
   | SSMIGRATE DATABASE dbName
   | COMPACT DATABASE dbName startOpt endOpt metaOnly
   | COMPACT dbNameCondOpt? VGROUPS IN NK_LP integerList NK_RP startOpt endOpt metaOnly
   | CREATE TABLE notExistsOpt fullTableName NK_LP columnDefList NK_RP tagsDefOpt tableOptions
   | CREATE TABLE multiCreateClause
   | CREATE TABLE notExistsOpt USING fullTableName NK_LP tagListOpt NK_RP FILE NK_STRING
   | CREATE STABLE notExistsOpt fullTableName NK_LP columnDefList NK_RP tagsDef tableOptions
   | CREATE VTABLE notExistsOpt fullTableName NK_LP columnDefList NK_RP
   | CREATE VTABLE notExistsOpt fullTableName NK_LP specificColumnRefList NK_RP USING fullTableName specificColsOpt TAGS NK_LP tagsLiteralList NK_RP
   | CREATE VTABLE notExistsOpt fullTableName NK_LP columnRefList NK_RP USING fullTableName specificColsOpt TAGS NK_LP tagsLiteralList NK_RP
   | CREATE VTABLE notExistsOpt fullTableName USING fullTableName specificColsOpt TAGS NK_LP tagsLiteralList NK_RP
   | DROP TABLE withOpt? multiDropClause
   | DROP STABLE withOpt? existsOpt fullTableName
   | DROP VTABLE withOpt? existsOpt fullTableName
   | ALTER TABLE alterTableClause
   | ALTER STABLE alterTableClause
   | ALTER VTABLE alterTableClause
   | CREATE MOUNT notExistsOpt mountName ON DNODE NK_INTEGER FROM NK_STRING
   | DROP MOUNT existsOpt mountName
   | SHOW DNODES
   | SHOW USERS FULL?
   | SHOW USER PRIVILEGES
   | SHOW dbKindOpt DATABASES
   | SHOW tableKindDbNameCondOpt TABLES likePatternOpt
   | SHOW tableKindDbNameCondOpt VTABLES likePatternOpt
   | SHOW tableKindDbNameCondOpt STABLES likePatternOpt
   | SHOW dbNameCondOpt? VGROUPS
   | SHOW MNODES
   | SHOW QNODES
   | SHOW ANODES FULL?
   | SHOW ARBGROUPS
   | SHOW FUNCTIONS
   | SHOW INDEXES FROM table_name_cond fromDbOpt
   | SHOW INDEXES FROM dbName NK_DOT tableName
   | SHOW dbNameCondOpt? STREAMS
   | SHOW ACCOUNTS
   | SHOW APPS
   | SHOW CONNECTIONS
   | SHOW LICENCES
   | SHOW GRANTS (FULL | LOGS)?
   | SHOW CLUSTER MACHINES
   | SHOW MOUNTS
   | SHOW CREATE DATABASE dbName
   | SHOW CREATE TABLE fullTableName
   | SHOW CREATE VTABLE fullTableName
   | SHOW CREATE STABLE fullTableName
   | SHOW ENCRYPTIONS
   | SHOW QUERIES
   | SHOW SCORES
   | SHOW TOPICS
   | SHOW (CLUSTER | LOCAL)? VARIABLES likePatternOpt
   | SHOW DNODE NK_INTEGER VARIABLES likePatternOpt
   | SHOW SNODES
   | SHOW BNODES
   | SHOW CLUSTER
   | SHOW TRANSACTIONS
   | SHOW TRANSACTION NK_INTEGER
   | SHOW TABLE DISTRIBUTED fullTableName
   | SHOW CONSUMERS
   | SHOW SUBSCRIPTIONS
   | SHOW TAGS FROM table_name_cond fromDbOpt
   | SHOW TAGS FROM dbName NK_DOT tableName
   | SHOW TABLE TAGS tagListOpt FROM table_name_cond fromDbOpt
   | SHOW TABLE TAGS tagListOpt FROM dbName NK_DOT tableName
   | SHOW VNODES (ON DNODE NK_INTEGER)?
   | SHOW dbNameCondOpt? ALIVE
   | SHOW CLUSTER ALIVE
   | SHOW dbNameCondOpt? VIEWS likePatternOpt
   | SHOW CREATE VIEW fullViewName
   | SHOW COMPACTS
   | SHOW COMPACT NK_INTEGER
   | SHOW dbNameCondOpt? DISK_INFO
   | CREATE TSMA notExistsOpt tsmaName ON fullTableName tsmaFuncList INTERVAL NK_LP durationLiteral NK_RP
   | CREATE RECURSIVE TSMA notExistsOpt tsmaName ON fullTableName INTERVAL NK_LP durationLiteral NK_RP
   | DROP TSMA existsOpt fullTsmaName
   | SHOW dbNameCondOpt? TSMAS
   | CREATE SMA INDEX notExistsOpt colName ON fullTableName indexOptions
   | CREATE INDEX notExistsOpt colName ON fullTableName NK_LP colNameList NK_RP
   | DROP INDEX existsOpt fullIndexName
   | CREATE TOPIC notExistsOpt topicName AS queryOrSubquery
   | CREATE TOPIC notExistsOpt topicName withMeta DATABASE dbName
   | CREATE TOPIC notExistsOpt topicName withMeta STABLE fullTableName whereClauseOpt?
   | DROP TOPIC existsOpt forceOpt topicName
   | DROP CONSUMER GROUP existsOpt forceOpt cgroupName ON topicName
   | (DESC | DESCRIBE) fullTableName
   | RESET QUERY CACHE
   | EXPLAIN analyzeOpt? explainOptions queryOrSubquery
   | EXPLAIN analyzeOpt? explainOptions insertQuery
   | CREATE orReplaceOpt? aggFuncOpt? FUNCTION notExistsOpt functionName AS NK_STRING OUTPUTTYPE typeName bufsizeOpt languageOpt
   | DROP FUNCTION existsOpt functionName
   | CREATE orReplaceOpt? VIEW fullViewName AS queryOrSubquery
   | DROP VIEW existsOpt fullViewName
   | CREATE STREAM notExistsOpt fullStreamName streamTrigger streamOuttableOpt asSubqueryOpt
   | DROP STREAM existsOpt fullStreamName
   | STOP STREAM existsOpt fullStreamName
   | START STREAM existsOpt ignoreOpt? fullStreamName
   | RECALCULATE STREAM fullStreamName recalculateRange
   | KILL CONNECTION NK_INTEGER
   | KILL QUERY NK_STRING
   | KILL TRANSACTION NK_INTEGER
   | KILL COMPACT NK_INTEGER
   | BALANCE VGROUP
   | ASSIGN LEADER FORCE
   | BALANCE VGROUP LEADER onVgroupId
   | BALANCE VGROUP LEADER DATABASE dbName
   | MERGE VGROUP NK_INTEGER NK_INTEGER
   | REDISTRIBUTE VGROUP NK_INTEGER dnodeList
   | SPLIT VGROUP NK_INTEGER forceOpt
   | DELETE FROM fullTableName whereClauseOpt?
   | queryOrSubquery
   | insertQuery
   ;
/*
 * =============================================================================
 * Clause and options definitions
 * =============================================================================
 */


accountOptions
   : (PPS literal | TSERIES literal | STORAGE literal | STREAMS literal | QTIME literal | DBS literal | USERS literal | CONNS literal | STATE literal)*
   ;

alterAccountOptions
   : alterAccountOption+
   ;

alterAccountOption
   : PASS literal
   | PPS literal
   | TSERIES literal
   | STORAGE literal
   | STREAMS literal
   | QTIME literal
   | DBS literal
   | USERS literal
   | CONNS literal
   | STATE literal
   ;

ipRangeList
   : NK_STRING (NK_COMMA NK_STRING)*
   ;

whiteList
   : HOST ipRangeList
   ;

whiteListOpt
   : whiteList?
   ;

isImportOpt
   : (IS_IMPORT NK_INTEGER)?
   ;

isCreatedbOpt
   : (CREATEDB NK_INTEGER)?
   ;

sysinfoOpt
   : (SYSINFO NK_INTEGER)?
   ;

privileges
   : ALL
   | privTypeList
   | SUBSCRIBE
   ;

privTypeList
   : privType (NK_COMMA privType)*
   ;

privType
   : READ
   | WRITE
   | ALTER
   ;

privLevel
   : NK_STAR NK_DOT NK_STAR
   | dbName NK_DOT NK_STAR
   | dbName NK_DOT tableName
   | topicName
   ;

withClauseOpt
   : (WITH searchCondition)?
   ;

dnodeEndpoint
   : NK_STRING
   | NK_ID
   | NK_IPTOKEN
   ;

forceOpt
   : FORCE?
   ;

unsafeOpt
   : UNSAFE
   ;

bnodeOptions
   : (NK_ID NK_STRING)*
   ;

notExistsOpt
   : (IF NOT EXISTS)?
   ;

existsOpt
   : (IF EXISTS)?
   ;

metaOnly
   : META_ONLY?
   ;

dbOptions
   : (BUFFER NK_INTEGER | CACHEMODEL NK_STRING | CACHESIZE NK_INTEGER | COMP NK_INTEGER | DURATION (NK_INTEGER | NK_VARIABLE) | MAXROWS NK_INTEGER | MINROWS NK_INTEGER | KEEP (integerList | variableList) | PAGES NK_INTEGER | PAGESIZE NK_INTEGER | TSDB_PAGESIZE NK_INTEGER | PRECISION NK_STRING | (REPLICAS | REPLICA) NK_INTEGER | VGROUPS NK_INTEGER | SINGLE_STABLE NK_INTEGER | RETENTIONS retentionList | SCHEMALESS NK_INTEGER | WAL_LEVEL NK_INTEGER | WAL_FSYNC_PERIOD NK_INTEGER | WAL_RETENTION_PERIOD NK_MINUS? NK_INTEGER | WAL_RETENTION_SIZE NK_MINUS? NK_INTEGER | WAL_ROLL_PERIOD NK_INTEGER | WAL_SEGMENT_SIZE NK_INTEGER | STT_TRIGGER NK_INTEGER | TABLE_PREFIX signed | TABLE_SUFFIX signed | SS_CHUNKPAGES NK_INTEGER | SS_KEEPLOCAL (NK_INTEGER | NK_VARIABLE) | SS_COMPACT NK_INTEGER | KEEP_TIME_OFFSET (NK_INTEGER | NK_VARIABLE) | ENCRYPT_ALGORITHM NK_STRING | DNODES NK_STRING | COMPACT_INTERVAL (NK_INTEGER | NK_VARIABLE) | COMPACT_TIME_RANGE signedDurationList | COMPACT_TIME_OFFSET (NK_INTEGER | NK_VARIABLE))*
   ;

alterDbOptions
   : alterDbOption+
   ;

alterDbOption
   : BUFFER NK_INTEGER
   | CACHEMODEL NK_STRING
   | CACHESIZE NK_INTEGER
   | WAL_FSYNC_PERIOD NK_INTEGER
   | KEEP (integerList | variableList)
   | PAGES NK_INTEGER
   | (REPLICA | REPLICAS) NK_INTEGER
   | WAL_LEVEL NK_INTEGER
   | STT_TRIGGER NK_INTEGER
   | MINROWS NK_INTEGER
   | WAL_RETENTION_PERIOD NK_MINUS? NK_INTEGER
   | WAL_RETENTION_SIZE NK_MINUS? NK_INTEGER
   | SS_KEEPLOCAL (NK_INTEGER | NK_VARIABLE)
   | SS_COMPACT NK_INTEGER
   | KEEP_TIME_OFFSET (NK_INTEGER | NK_VARIABLE)
   | ENCRYPT_ALGORITHM NK_STRING
   | COMPACT_INTERVAL (NK_INTEGER | NK_VARIABLE)
   | COMPACT_TIME_RANGE signedDurationList
   | COMPACT_TIME_OFFSET (NK_INTEGER | NK_VARIABLE)
   ;

integerList
   : NK_INTEGER (NK_COMMA NK_INTEGER)*
   ;

variableList
   : NK_VARIABLE (NK_COMMA NK_VARIABLE)*
   ;

signedDurationList
   : (signedVariable | signedInteger) (NK_COMMA (signedVariable | signedInteger))*
   ;

retentionList
   : retention (NK_COMMA retention)*
   ;

retention
   : (NK_VARIABLE | NK_MINUS) NK_COLON NK_VARIABLE
   ;

speedOpt
   : (BWLIMIT NK_INTEGER)?
   ;

startOpt
   : (START WITH (TIMESTAMP? (NK_INTEGER | NK_STRING)))?
   ;

endOpt
   : (END WITH (TIMESTAMP? (NK_INTEGER | NK_STRING)))?
   ;

withOpt
   : WITH
   ;

alterTableClause
   : fullTableName alterTableOptions
   | fullTableName ADD COLUMN columnName typeName columnOptions
   | fullTableName DROP COLUMN columnName
   | fullTableName MODIFY COLUMN columnName typeName
   | fullTableName MODIFY COLUMN columnName columnOptions
   | fullTableName RENAME COLUMN columnName columnName
   | fullTableName ADD TAG columnName typeName
   | fullTableName DROP TAG columnName
   | fullTableName MODIFY TAG columnName typeName
   | fullTableName RENAME TAG columnName columnName
   | fullTableName ALTER COLUMN columnName SET columnRef
   | fullTableName ALTER COLUMN columnName SET NULL
   | fullTableName SET TAG columnTagValueList
   ;

columnTagValueList
   : columnTagValue (NK_COMMA columnTagValue)*
   ;

columnTagValue
   : columnName NK_EQ tagsLiteral
   ;

multiCreateClause
   : createSubtableClause+
   ;

createSubtableClause
   : notExistsOpt fullTableName USING fullTableName specificColsOpt TAGS NK_LP tagsLiteralList NK_RP tableOptions
   ;

multiDropClause
   : dropTableClause (NK_COMMA dropTableClause)*
   ;

dropTableClause
   : existsOpt fullTableName
   ;

specificColsOpt
   : (NK_LP colNameList NK_RP)?
   ;

fullTableName
   : (dbName NK_DOT)? tableName
   ;

tagDefList
   : tagDef (NK_COMMA tagDef)*
   ;

tagDef
   : columnName typeName
   ;

columnDefList
   : columnDef (NK_COMMA columnDef)*
   ;

columnDef
   : columnName typeName columnOptions
   ;

specificColumnRefList
   : specificColumnRef (NK_COMMA specificColumnRef)*
   ;

specificColumnRef
   : columnName FROM columnRef
   ;

columnRefList
   : columnRef (NK_COMMA columnRef)*
   ;

typeName
   : BOOL
   | TINYINT UNSIGNED?
   | SMALLINT UNSIGNED?
   | (INT | INTEGER) UNSIGNED?
   | BIGINT UNSIGNED?
   | FLOAT
   | DOUBLE
   | BINARY NK_LP NK_INTEGER NK_RP
   | TIMESTAMP
   | NCHAR NK_LP NK_INTEGER NK_RP
   | JSON
   | VARCHAR NK_LP NK_INTEGER NK_RP
   | MEDIUMBLOB
   | BLOB
   | VARBINARY NK_LP NK_INTEGER NK_RP
   | GEOMETRY NK_LP NK_INTEGER NK_RP
   | DECIMAL NK_LP NK_INTEGER (NK_COMMA NK_INTEGER)? NK_RP
   ;

typeNameDefaultLen
   : BINARY
   | NCHAR
   | VARCHAR
   | VARBINARY
   ;

tagsDefOpt
   : tagsDef?
   ;

tagsDef
   : TAGS NK_LP tagDefList NK_RP
   ;

tableOptions
   : (COMMENT NK_STRING | MAX_DELAY durationList | WATERMARK durationList | ROLLUP NK_LP rollupFuncList NK_RP | TTL NK_INTEGER | SMA NK_LP colNameList NK_RP | DELETE_MARK durationList | KEEP (NK_INTEGER | NK_VARIABLE) | VIRTUAL NK_INTEGER)*
   ;

alterTableOptions
   : alterTableOption+
   ;

alterTableOption
   : COMMENT NK_STRING
   | TTL NK_INTEGER
   | KEEP (NK_INTEGER | NK_VARIABLE)
   ;

durationList
   : durationLiteral (NK_COMMA durationLiteral)*
   ;

rollupFuncList
   : rollupFuncName (NK_COMMA rollupFuncName)*
   ;

rollupFuncName
   : functionName
   | FIRST
   | LAST
   ;

colNameList
   : colName (NK_COMMA colName)*
   ;

colName
   : columnName
   | TBNAME
   ;

dbKindOpt
   : (USER | SYSTEM)?
   ;

tableKindDbNameCondOpt
   : (tableKind? dbName NK_DOT)
   | tableKind?
   ;

tableKind
   : NORMAL
   | CHILD
   | VIRTUAL
   ;

dbNameCondOpt
   : (dbName NK_DOT)
   ;

likePatternOpt
   : (LIKE NK_STRING)?
   ;

table_name_cond
   : tableName
   ;

fromDbOpt
   : (FROM dbName)?
   ;

tagListOpt
   : tagItem (NK_COMMA tagItem)*
   |
   ;

tagList
   : tagItem (NK_COMMA tagItem)*
   ;

tagItem
   : TBNAME
   | QTAGS
   | columnName (AS? columnAlias)?
   ;

fullTsmaName
   : (dbName NK_DOT)? tsmaName
   ;

tsmaFuncList
   : FUNCTION NK_LP funcList NK_RP
   ;

fullIndexName
   : (dbName NK_DOT)? indexName
   ;

indexOptions
   : FUNCTION NK_LP funcList NK_RP INTERVAL NK_LP durationLiteral (NK_COMMA durationLiteral)? NK_RP slidingOpt smaStreamOpt
   ;

funcList
   : func (NK_COMMA func)*
   ;

func
   : smaFuncName NK_LP expressionList NK_RP
   ;

smaFuncName
   : functionName
   | COUNT
   | FIRST
   | LAST
   | LAST_ROW
   ;

smaStreamOpt
   :
   ;

withMeta
   : AS
   | WITH META AS
   | ONLY META AS
   ;

analyzeOpt
   : ANALYZE
   ;

explainOptions
   : (VERBOSE NK_BOOL | RATIO NK_FLOAT)*
   ;

aggFuncOpt
   : AGGREGATE
   ;

bufsizeOpt
   : (BUFSIZE NK_INTEGER)?
   ;

languageOpt
   : (LANGUAGE NK_STRING)?
   ;

orReplaceOpt
   : (OR REPLACE)
   ;

fullViewName
   : (dbName NK_DOT)? viewName
   ;

recalculateRange
   : FROM timePoint (TO timePoint)?
   ;

fullStreamName
   : (dbName NK_DOT)? streamName
   ;

streamOuttableOpt
   : (INTO fullTableName outputSubtableOpt? columnNameOpt? streamTagsDefOpt?)?
   ;

streamTrigger
   : triggerType triggerTableOpt? streamPartitionByOpt? triggerOptionsOpt? notificationOpt?
   ;

triggerType
   : SESSION NK_LP columnReference NK_COMMA intervalSlidingDurationLiteral NK_RP
   | STATE_WINDOW NK_LP columnReference NK_RP trueForOpt
   | intervalOpt? SLIDING NK_LP slidingExpr NK_RP
   | EVENT_WINDOW NK_LP START WITH searchCondition END WITH searchCondition NK_RP trueForOpt
   | COUNT_WINDOW NK_LP countWindowArgs NK_RP
   | PERIOD NK_LP intervalSlidingDurationLiteral offsetOpt NK_RP
   ;

intervalOpt
   : (INTERVAL NK_LP intervalSlidingDurationLiteral (NK_COMMA intervalSlidingDurationLiteral)? NK_RP)
   ;

slidingExpr
   : intervalSlidingDurationLiteral (NK_COMMA intervalSlidingDurationLiteral)?
   ;

offsetOpt
   : (NK_COMMA intervalSlidingDurationLiteral)?
   ;

triggerTableOpt
   : (FROM fullTableName)
   ;

streamPartitionByOpt
   : (PARTITION BY columnNameList)
   ;

triggerOptionsOpt
   : (STREAM_OPTIONS NK_LP triggerOptionList NK_RP)
   ;

triggerOptionList
   : triggerOption (NK_BITOR triggerOption)*
   ;

triggerOption
   : CALC_NOTIFY_ONLY
   | DELETE_RECALC
   | DELETE_OUTPUT_TABLE
   | EXPIRED_TIME NK_LP durationLiteral NK_RP
   | FILL_HISTORY (NK_LP timePoint NK_RP)?
   | FILL_HISTORY_FIRST (NK_LP timePoint NK_RP)?
   | FORCE_OUTPUT
   | IGNORE_DISORDER
   | LOW_LATENCY_CALC
   | MAX_DELAY NK_LP durationLiteral NK_RP
   | PRE_FILTER NK_LP searchCondition NK_RP
   | WATERMARK NK_LP durationLiteral NK_RP
   | EVENT_TYPE NK_LP eventTypeList NK_RP
   | IGNORE_NODATA_TRIGGER
   ;

notificationOpt
   : (NOTIFY NK_LP notifyUrlList NK_RP notifyOnOpt whereClauseOpt? notifyOptionsOpt?)
   ;

notifyUrlList
   : NK_STRING (NK_COMMA NK_STRING)*
   ;

notifyOnOpt
   : (ON NK_LP eventTypeList NK_RP)?
   ;

notifyOptionsOpt
   : (NOTIFY_OPTIONS NK_LP notifyOptionsList NK_RP)
   ;

notifyOptionsList
   : notifyOption (NK_BITOR notifyOption)*
   ;

notifyOption
   : NOTIFY_HISTORY
   ;

timePoint
   : NK_INTEGER
   | NK_STRING
   ;

columnNameList
   : triggerColName (NK_COMMA triggerColName)*
   ;

triggerColName
   : columnName
   | TBNAME
   ;

eventTypeList
   : eventTypes (NK_BITOR eventTypes)*
   ;

eventTypes
   : WINDOW_OPEN
   | WINDOW_CLOSE
   ;

outputSubtableOpt
   : (OUTPUT_SUBTABLE NK_LP exprOrSubquery NK_RP)
   ;

columnNameOpt
   : columnNameUnit
   ;

streamTagsDefOpt
   : (TAGS NK_LP streamTagsDefList NK_RP)
   ;

streamTagsDefList
   : streamTagsDef (NK_COMMA streamTagsDef)*
   ;

streamTagsDef
   : columnName typeName AS expression
   ;

columnNameUnit
   : NK_LP columnStreamDefList NK_RP
   ;

columnStreamDefList
   : columnStreamDef (NK_COMMA columnStreamDef)*
   ;

columnStreamDef
   : columnName streamColOptions
   ;

streamColOptions
   : (PRIMARY KEY | COMPOSITE KEY)*
   ;

asSubqueryOpt
   : (AS queryOrSubquery)?
   ;

ignoreOpt
   : IGNORE UNTREATED
   ;

onVgroupId
   : (ON NK_INTEGER)?
   ;

dnodeList
   : (DNODE NK_INTEGER)+
   ;

insertQuery
   : INSERT INTO insertTableClause+
   ;

tagsLiteral
   : (NK_PLUS | NK_MINUS)? (NK_INTEGER | NK_BIN | NK_HEX) ((NK_PLUS | NK_MINUS) durationLiteral)?
   | (NK_PLUS | NK_MINUS)? NK_FLOAT
   | NK_STRING ((NK_PLUS | NK_MINUS) durationLiteral)?
   | NK_BOOL
   | NULL
   | literalFunc ((NK_PLUS | NK_MINUS) durationLiteral)?
   ;

tagsLiteralList
   : tagsLiteral (NK_COMMA tagsLiteral)*
   ;

literal
   : NK_INTEGER
   | NK_FLOAT
   | NK_STRING
   | NK_BOOL
   | TIMESTAMP NK_STRING
   | durationLiteral
   | NULL
   | NK_QUESTION
   ;

durationLiteral
   : NK_VARIABLE
   ;

signedVariable
   : (NK_PLUS | NK_MINUS)? NK_VARIABLE
   ;

signedInteger
   : (NK_PLUS | NK_MINUS)? NK_INTEGER
   ;

unsignedInteger
   : NK_INTEGER
   | NK_QUESTION
   ;

signedFloat
   : (NK_PLUS | NK_MINUS)? NK_FLOAT
   ;

signed
   : signedInteger
   | signedFloat
   ;

signedLiteral
   : signed
   | NK_STRING
   | NK_BOOL
   | TIMESTAMP NK_STRING
   | durationLiteral
   | NULL
   | literalFunc
   | NK_QUESTION
   | timeCalcLiteral
   ;

literalList
   : signedLiteral (NK_COMMA signedLiteral)*
   ;
/*
 * =============================================================================
 * Names and Identifiers
 * =============================================================================
 */


dbName
   : NK_ID
   ;

mountName
   : NK_ID
   ;

tableName
   : NK_ID
   ;

columnName
   : NK_ID
   ;

functionName
   : NK_ID
   ;

viewName
   : NK_ID
   ;

tableAlias
   : NK_ID
   ;

columnAlias
   : NK_ID
   ;

userName
   : NK_ID
   ;

topicName
   : NK_ID
   ;

streamName
   : NK_ID
   ;

cgroupName
   : NK_ID
   ;

indexName
   : NK_ID
   ;

tsmaName
   : NK_ID
   ;
/*
 * =============================================================================
 * Expressions and Conditions
 * =============================================================================
 */


exprOrSubquery
   : expression
   ;

expression
   : < assoc = right > expression NK_CONCAT expression
   | expression (NK_STAR | NK_SLASH | NK_REM) expression
   | expression (NK_PLUS | NK_MINUS) expression
   | expression (NK_BITAND | NK_BITOR | NK_LSHIFT | NK_RSHIFT | NK_PH) expression
   | expression (NK_LT | NK_GT | NK_LE | NK_GE | NK_EQ | NK_NE | LIKE | MATCH | NMATCH | REGEXP | CONTAINS) expression
   | expression BETWEEN expression AND expression
   | expression IS expression
   | expression IN expression
   | < assoc = right > NOT expression
   | < assoc = right > columnReference NK_ARROW NK_STRING
   | (NK_PLUS | NK_MINUS) exprOrSubquery
   | primaryExpression
   ;

primaryExpression
   : literal
   | pseudoColumn
   | columnReference
   | functionExpression
   | caseWhenExpression
   | NK_LP expression NK_RP
   ;

expressionList
   : exprOrSubquery (NK_COMMA exprOrSubquery)*
   ;

columnReference
   : (tableName NK_DOT)? columnName
   | (tableName NK_DOT)? columnAlias
   ;

pseudoColumn
   : ROWTS
   | (tableName NK_DOT)? TBNAME
   | QSTART
   | QEND
   | QDURATION
   | WSTART
   | WEND
   | WDURATION
   | IROWTS
   | ISFILLED
   | QTAGS
   | FLOW
   | FHIGH
   | FROWTS
   | IROWTS_ORIGIN
   | TPREV_TS
   | TCURRENT_TS
   | TNEXT_TS
   | TWSTART
   | TWEND
   | TWDURATION
   | TWROWNUM
   | TPREV_LOCALTIME
   | TNEXT_LOCALTIME
   | TLOCALTIME
   | TGRPID
   | NK_PH NK_INTEGER
   | NK_PH TBNAME
   ;

functionExpression
   : functionName NK_LP expressionList NK_RP
   | starFunc NK_LP starFuncParaList NK_RP
   | colsFunc NK_LP colsFuncParaList NK_RP
   | CAST NK_LP commonExpression AS (typeName | typeNameDefaultLen) NK_RP
   | POSITION NK_LP exprOrSubquery IN exprOrSubquery NK_RP
   | TRIM NK_LP exprOrSubquery NK_RP
   | TRIM NK_LP trimSpecificationType FROM exprOrSubquery NK_RP
   | TRIM NK_LP exprOrSubquery FROM exprOrSubquery NK_RP
   | TRIM NK_LP trimSpecificationType exprOrSubquery FROM exprOrSubquery NK_RP
   | substrFunc NK_LP expressionList NK_RP
   | substrFunc NK_LP exprOrSubquery FROM exprOrSubquery (FOR exprOrSubquery)? NK_RP
   | REPLACE NK_LP expressionList NK_RP
   | literalFunc
   | randFunc
   ;

literalFunc
   : noargFunc NK_LP NK_RP
   | NOW
   | TODAY
   ;

timeCalcLiteral
   : (NOW | TODAY) (NK_LP NK_RP)? (NK_PLUS | NK_MINUS) durationLiteral
   | durationLiteral (NK_PLUS | NK_MINUS) (NOW | TODAY) (NK_LP NK_RP)?
   ;

randFunc
   : RAND NK_LP NK_RP
   | RAND NK_LP expressionList NK_RP
   ;

substrFunc
   : SUBSTR
   | SUBSTRING
   ;

trimSpecificationType
   : BOTH
   | TRAILING
   | LEADING
   ;

noargFunc
   : NOW
   | TODAY
   | TIMEZONE
   | DATABASE
   | CLIENT_VERSION
   | SERVER_VERSION
   | SERVER_STATUS
   | CURRENT_USER
   | USER
   | PI
   ;

starFunc
   : COUNT
   | FIRST
   | LAST
   | LAST_ROW
   ;

colsFunc
   : COLS
   ;

colsFuncParaList
   : functionExpression NK_COMMA colsFuncExpressionList
   ;

colsFuncExpressionList
   : colsFuncExpression (NK_COMMA colsFuncExpression)*
   ;

colsFuncExpression
   : exprOrSubquery (AS? columnAlias)?
   | NK_STAR
   ;

starFuncParaList
   : NK_STAR
   | otherParaList
   ;

otherParaList
   : starFuncPara (NK_COMMA starFuncPara)*
   ;

starFuncPara
   : exprOrSubquery
   | tableName NK_DOT NK_STAR
   ;

caseWhenExpression
   : CASE commonExpression? whenThenList caseWhenElseOpt? END
   ;

whenThenList
   : whenThenExpr+
   ;

whenThenExpr
   : WHEN commonExpression THEN commonExpression
   ;

caseWhenElseOpt
   : (ELSE commonExpression)
   ;

predicate
   : exprOrSubquery compareOp exprOrSubquery
   | exprOrSubquery NOT? BETWEEN exprOrSubquery AND exprOrSubquery
   | exprOrSubquery IS NOT? NULL
   | exprOrSubquery inOp inPredicateValue
   ;

compareOp
   : NK_LT
   | NK_GT
   | NK_LE
   | NK_GE
   | NK_NE
   | NK_EQ
   | NOT? LIKE
   | MATCH
   | NMATCH
   | NOT? REGEXP
   | CONTAINS
   ;

inOp
   : NOT? IN
   ;

inPredicateValue
   : NK_LP literalList NK_RP
   ;

booleanValueExpression
   : booleanValueExpression (OR | AND) booleanValueExpression
   | NOT booleanPrimary
   | booleanPrimary
   ;

booleanPrimary
   : predicate
   | NK_LP booleanValueExpression NK_RP
   ;

commonExpression
   : exprOrSubquery
   | booleanValueExpression
   ;

searchCondition
   : commonExpression
   ;
/*
 * =============================================================================
 * Query Structure (FROM, SELECT, JOIN etc.) - CORRECTED SECTION
 * =============================================================================
 */


insertTableClause
   : fullTableName (NK_LP colNameList NK_RP) (usingClauseWithoutCols? dataClause)
   | fullTableName (usingClauseWithOptCols? dataClause)
   ;

usingClauseWithoutCols
   : USING fullTableName (NK_LP tagList NK_RP)? TAGS NK_LP tagsLiteralList NK_RP tableOptions
   ;

usingClauseWithOptCols
   : USING fullTableName (NK_LP tagList NK_RP)? TAGS NK_LP tagsLiteralList NK_RP (NK_LP colNameList NK_RP)? tableOptions
   ;

dataClause
   : VALUES inPredicateValue+
   ;

fromClauseOpt
   : (FROM tableReferenceList)
   ;

tableReferenceList
   : tableReference (NK_COMMA tableReference)*
   ;

tableReference
   : tablePrimary joinClause?
   ;

tablePrimary
   : (dbName NK_DOT)? tableName aliasOpt?
   | subquery aliasOpt?
   | NK_LP tableReference NK_RP
   | NK_PH TBNAME aliasOpt?
   | NK_PH TROWS aliasOpt?
   ;

aliasOpt
   : (AS? tableAlias)
   ;

joinClause
   : (INNER? JOIN) tableReference joinOnClauseOpt?
   | (LEFT | RIGHT | FULL) OUTER? JOIN tableReference joinOnClause
   | (LEFT | RIGHT) SEMI JOIN tableReference joinOnClause
   | (LEFT | RIGHT) ANTI JOIN tableReference joinOnClause
   | (LEFT | RIGHT) ASOF JOIN tableReference joinOnClauseOpt? jlimitClauseOpt?
   | (LEFT | RIGHT) WINDOW JOIN tableReference joinOnClauseOpt? windowOffsetClause jlimitClauseOpt?
   ;

joinOnClauseOpt
   : joinOnClause
   ;

joinOnClause
   : ON searchCondition
   ;

windowOffsetClause
   : WINDOW_OFFSET NK_LP windowOffsetLiteral NK_COMMA windowOffsetLiteral NK_RP
   ;

windowOffsetLiteral
   : NK_MINUS? NK_VARIABLE
   ;

jlimitClauseOpt
   : (JLIMIT unsignedInteger)
   ;

querySpecification
   : SELECT hintList? setQuantifierOpt? tagModeOpt? selectList fromClauseOpt? whereClauseOpt? partitionByClauseOpt? rangeOpt? everyOpt? interpFillOpt? twindowClauseOpt? groupByClauseOpt? havingClauseOpt?
   ;

hintList
   : NK_HINT
   ;

tagModeOpt
   : TAGS
   ;

setQuantifierOpt
   : (DISTINCT | ALL)
   ;

selectList
   : selectItem (NK_COMMA selectItem)*
   ;

selectItem
   : (tableName NK_DOT)? NK_STAR
   | commonExpression (AS? columnAlias)?
   ;

whereClauseOpt
   : (WHERE searchCondition)
   ;

partitionByClauseOpt
   : (PARTITION BY partitionList)
   ;

partitionList
   : partitionItem (NK_COMMA partitionItem)*
   ;

partitionItem
   : exprOrSubquery (AS? columnAlias)?
   ;

twindowClauseOpt
   : SESSION NK_LP columnReference NK_COMMA intervalSlidingDurationLiteral NK_RP
   | STATE_WINDOW NK_LP exprOrSubquery NK_RP trueForOpt
   | INTERVAL NK_LP intervalSlidingDurationLiteral (NK_COMMA (intervalSlidingDurationLiteral | AUTO))? NK_RP slidingOpt? fillOpt?
   | EVENT_WINDOW START WITH searchCondition END WITH searchCondition trueForOpt
   | COUNT_WINDOW NK_LP countWindowArgs NK_RP
   | ANOMALY_WINDOW NK_LP exprOrSubquery (NK_COMMA NK_STRING)? NK_RP
   ;

slidingOpt
   : (SLIDING NK_LP intervalSlidingDurationLiteral NK_RP)
   ;

intervalSlidingDurationLiteral
   : NK_VARIABLE
   | NK_STRING
   | NK_INTEGER
   ;

interpFillOpt
   : fillValue
   | FILL NK_LP fillPositionModeExtension NK_COMMA expressionList NK_RP
   | FILL NK_LP interpFillMode NK_RP
   ;

fillOpt
   : (FILL NK_LP fillMode NK_RP)
   | fillValue
   ;

fillValue
   : FILL NK_LP VALUE NK_COMMA expressionList NK_RP
   | FILL NK_LP VALUE_F NK_COMMA expressionList NK_RP
   ;

countWindowArgs
   : NK_INTEGER (NK_COMMA NK_INTEGER)? (NK_COMMA columnNameList)?
   ;

fillMode
   : NONE
   | NULL
   | NULL_F
   | LINEAR
   | fillPositionMode
   ;

fillPositionMode
   : PREV
   | NEXT
   ;

fillPositionModeExtension
   : fillPositionMode
   | NEAR
   ;

interpFillMode
   : fillMode
   | NEAR
   ;

groupByClauseOpt
   : (GROUP BY groupByList)
   ;

groupByList
   : exprOrSubquery (NK_COMMA exprOrSubquery)*
   ;

havingClauseOpt
   : (HAVING searchCondition)
   ;

rangeOpt
   : (RANGE NK_LP exprOrSubquery (NK_COMMA exprOrSubquery (NK_COMMA exprOrSubquery)?)? NK_RP)
   ;

everyOpt
   : (EVERY NK_LP durationLiteral NK_RP)
   ;

trueForOpt
   : (TRUE_FOR NK_LP intervalSlidingDurationLiteral NK_RP)?
   ;
/*
 * =============================================================================
 * Query Expression (SELECT, UNION etc.) - CORRECTED SECTION
 * =============================================================================
 */


queryOrSubquery
   : queryExpression
   | subquery
   ;

queryExpression
   : queryExpressionBody orderByClauseOpt? slimitClauseOpt? limitClauseOpt?
   ;

queryExpressionBody
   : queryTerm (UNION ALL? queryTerm)*
   ;

queryTerm
   : querySpecification
   | subquery
   ;

subquery
   : NK_LP queryExpression NK_RP
   ;

orderByClauseOpt
   : (ORDER BY sortSpecificationList)
   ;

slimitClauseOpt
   : (SLIMIT unsignedInteger (SOFFSET unsignedInteger | NK_COMMA unsignedInteger)?)
   ;

limitClauseOpt
   : (LIMIT unsignedInteger (OFFSET unsignedInteger | NK_COMMA unsignedInteger)?)
   ;

sortSpecificationList
   : sortSpecification (NK_COMMA sortSpecification)*
   ;

sortSpecification
   : exprOrSubquery orderingSpecificationOpt? nullOrderingOpt?
   ;

orderingSpecificationOpt
   : (ASC | DESC)
   ;

nullOrderingOpt
   : (NULLS (FIRST | LAST))
   ;

columnOptions
   : ((PRIMARY KEY | COMPOSITE KEY) | (NK_ID NK_STRING) | (FROM columnRef))*
   ;

columnRef
   : columnNameTriplet
   ;

columnNameTriplet
   : NK_ID (NK_DOT NK_ID)*
   ;

