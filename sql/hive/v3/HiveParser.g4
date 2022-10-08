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
parser grammar HiveParser;

import SelectClauseParser, FromClauseParser, IdentifiersParser;

options
{
  tokenVocab=HiveLexer;
}

// starting rule
statements
   : (statement statementSeparator)* EOF
   ;

statementSeparator
   : SEMICOLON
   |
   ;

statement
	: explainStatement
	| execStatement
	;

explainStatement
	: KW_EXPLAIN (
	    explainOption* execStatement
        | KW_REWRITE queryStatementExpression
      )
	;

explainOption
    : KW_EXTENDED
    | KW_FORMATTED
    | KW_DEPENDENCY
    | KW_LOGICAL
    | KW_AUTHORIZATION
    | KW_ANALYZE
    | KW_REOPTIMIZATION
    | (KW_VECTORIZATION vectorizationOnly? vectorizatonDetail?)
    ;

vectorizationOnly
    : KW_ONLY
    ;

vectorizatonDetail
    : KW_SUMMARY
    | KW_OPERATOR
    | KW_EXPRESSION
    | KW_DETAIL
    ;

execStatement
    : queryStatementExpression
    | loadStatement
    | exportStatement
    | importStatement
    | replDumpStatement
    | replLoadStatement
    | replStatusStatement
    | ddlStatement
    | deleteStatement
    | updateStatement
    | sqlTransactionStatement
    | mergeStatement
    ;

loadStatement
    : KW_LOAD KW_DATA KW_LOCAL? KW_INPATH StringLiteral KW_OVERWRITE? KW_INTO KW_TABLE tableOrPartition inputFileFormat?
    ;

replicationClause
    : KW_FOR KW_METADATA? KW_REPLICATION LPAREN StringLiteral RPAREN
    ;

exportStatement
    : KW_EXPORT
      KW_TABLE tableOrPartition
      KW_TO StringLiteral
      replicationClause?
    ;

importStatement
       : KW_IMPORT
         (KW_EXTERNAL? KW_TABLE tableOrPartition)?
         KW_FROM (path=StringLiteral)
         tableLocation?
    ;

replDumpStatement
      : KW_REPL KW_DUMP
        identifier (DOT identifier)?
        (KW_FROM Number
          (KW_TO Number)?
          (KW_LIMIT Number)?
        )?
        (KW_WITH replConfigs)?
    ;

replLoadStatement
      : KW_REPL KW_LOAD
        (identifier (DOT identifier)?)?
        KW_FROM StringLiteral
        (KW_WITH replConfigs)?
      ;

replConfigs
    : LPAREN replConfigsList RPAREN
    ;

replConfigsList
    : keyValueProperty (COMMA keyValueProperty)*
    ;

replStatusStatement
      : KW_REPL KW_STATUS
        identifier (DOT identifier)?
        (KW_WITH replConfigs)?
      ;

ddlStatement
    : createDatabaseStatement
    | switchDatabaseStatement
    | dropDatabaseStatement
    | createTableStatement
    | dropTableStatement
    | truncateTableStatement
    | alterStatement
    | descStatement
    | showStatement
    | metastoreCheck
    | createViewStatement
    | createMaterializedViewStatement
    | dropViewStatement
    | dropMaterializedViewStatement
    | createFunctionStatement
    | createMacroStatement
    | dropFunctionStatement
    | reloadFunctionStatement
    | dropMacroStatement
    | analyzeStatement
    | lockStatement
    | unlockStatement
    | lockDatabase
    | unlockDatabase
    | createRoleStatement
    | dropRoleStatement
    | grantPrivileges
    | revokePrivileges
    | showGrants
    | showRoleGrants
    | showRolePrincipals
    | showRoles
    | grantRole
    | revokeRole
    | setRole
    | showCurrentRole
    | abortTransactionStatement
    | killQueryStatement
    | createIndexStatement
    | dropIndexStatement
    ;

ifExists
    : KW_IF KW_EXISTS
    ;

restrictOrCascade
    : KW_RESTRICT
    | KW_CASCADE
    ;

ifNotExists
    : KW_IF KW_NOT KW_EXISTS
    ;

rewriteEnabled
    : KW_ENABLE KW_REWRITE
    ;

rewriteDisabled
    : KW_DISABLE KW_REWRITE
    ;

storedAsDirs
    : KW_STORED KW_AS KW_DIRECTORIES
    ;

orReplace
    : KW_OR KW_REPLACE
    ;

createDatabaseStatement
    : KW_CREATE (KW_DATABASE|KW_SCHEMA)
        ifNotExists?
        identifier
        databaseComment?
        dbLocation?
        (KW_WITH KW_DBPROPERTIES dbProperties)?
    ;

dbLocation
    :
      KW_LOCATION StringLiteral
    ;

dbProperties
    :
      LPAREN dbPropertiesList RPAREN
    ;

dbPropertiesList
    :
      keyValueProperty (COMMA keyValueProperty)*
    ;


switchDatabaseStatement
    : KW_USE identifier
    ;

dropDatabaseStatement
    : KW_DROP (KW_DATABASE|KW_SCHEMA) ifExists? identifier restrictOrCascade?
    ;

databaseComment

    : KW_COMMENT StringLiteral
    ;

createTableStatement
    : KW_CREATE KW_TEMPORARY? KW_EXTERNAL? KW_TABLE ifNotExists? tableName
      (  KW_LIKE tableName
         tableRowFormat?
         tableFileFormat?
         tableLocation?
         tablePropertiesPrefixed?
       | (LPAREN columnNameTypeOrConstraintList RPAREN)?
         tableComment?
         tablePartition?
         tableBuckets?
         tableSkewed?
         tableRowFormat?
         tableFileFormat?
         tableLocation?
         tablePropertiesPrefixed?
         (KW_AS selectStatementWithCTE)?
      )
    ;

truncateTableStatement
    : KW_TRUNCATE KW_TABLE tablePartitionPrefix (KW_COLUMNS LPAREN columnNameList RPAREN)?;

dropTableStatement
    : KW_DROP KW_TABLE ifExists? tableName KW_PURGE? replicationClause?
    ;

alterStatement
    : KW_ALTER KW_TABLE tableName alterTableStatementSuffix
    | KW_ALTER KW_VIEW tableName KW_AS? alterViewStatementSuffix
    | KW_ALTER KW_MATERIALIZED KW_VIEW tableName alterMaterializedViewStatementSuffix
    | KW_ALTER (KW_DATABASE|KW_SCHEMA) alterDatabaseStatementSuffix
    | KW_ALTER KW_INDEX alterIndexStatementSuffix
    ;

alterTableStatementSuffix
    : alterStatementSuffixRename
    | alterStatementSuffixDropPartitions
    | alterStatementSuffixAddPartitions
    | alterStatementSuffixTouch
    | alterStatementSuffixArchive
    | alterStatementSuffixUnArchive
    | alterStatementSuffixProperties
    | alterStatementSuffixSkewedby
    | alterStatementSuffixExchangePartition
    | alterStatementPartitionKeyType
    | alterStatementSuffixDropConstraint
    | alterStatementSuffixAddConstraint
    | partitionSpec? alterTblPartitionStatementSuffix
    | alterStatementSuffixSetOwner
    ;

alterTblPartitionStatementSuffix
  : alterStatementSuffixFileFormat
  | alterStatementSuffixLocation
  | alterStatementSuffixMergeFiles
  | alterStatementSuffixSerdeProperties
  | alterStatementSuffixRenamePart
  | alterStatementSuffixBucketNum
  | alterTblPartitionStatementSuffixSkewedLocation
  | alterStatementSuffixClusterbySortby
  | alterStatementSuffixCompact
  | alterStatementSuffixUpdateStatsCol
  | alterStatementSuffixUpdateStats
  | alterStatementSuffixRenameCol
  | alterStatementSuffixAddCol
  ;

alterStatementPartitionKeyType
	: KW_PARTITION KW_COLUMN LPAREN columnNameType RPAREN
	;

alterViewStatementSuffix
    : alterViewSuffixProperties
    | alterStatementSuffixRename
    | alterStatementSuffixAddPartitions
    | alterStatementSuffixDropPartitions
    | selectStatementWithCTE
    ;

alterMaterializedViewStatementSuffix
    : alterMaterializedViewSuffixRewrite
    | alterMaterializedViewSuffixRebuild
    ;

alterDatabaseStatementSuffix
    : alterDatabaseSuffixProperties
    | alterDatabaseSuffixSetOwner
    | alterDatabaseSuffixSetLocation
    ;

alterDatabaseSuffixProperties
    : identifier KW_SET KW_DBPROPERTIES dbProperties
    ;

alterDatabaseSuffixSetOwner
    : identifier KW_SET KW_OWNER principalName
    ;

alterDatabaseSuffixSetLocation
    : identifier KW_SET KW_LOCATION StringLiteral
    ;

alterStatementSuffixRename
    : KW_RENAME KW_TO tableName
    ;

alterStatementSuffixAddCol
    : (KW_ADD | KW_REPLACE) KW_COLUMNS LPAREN columnNameTypeList RPAREN restrictOrCascade?
    ;

alterStatementSuffixAddConstraint
   :  KW_ADD (alterForeignKeyWithName | alterConstraintWithName)
   ;

alterStatementSuffixDropConstraint
   : KW_DROP KW_CONSTRAINT identifier
   ;

alterStatementSuffixRenameCol
    : KW_CHANGE KW_COLUMN? identifier identifier colType alterColumnConstraint? (KW_COMMENT StringLiteral)? alterStatementChangeColPosition? restrictOrCascade?
    ;

alterStatementSuffixUpdateStatsCol
    : KW_UPDATE KW_STATISTICS KW_FOR KW_COLUMN? identifier KW_SET tableProperties (KW_COMMENT StringLiteral)?
    ;

alterStatementSuffixUpdateStats
    : KW_UPDATE KW_STATISTICS KW_SET tableProperties
    ;

alterStatementChangeColPosition
    : first=KW_FIRST|KW_AFTER identifier
    ;

alterStatementSuffixAddPartitions
    : KW_ADD ifNotExists? alterStatementSuffixAddPartitionsElement+
    ;

alterStatementSuffixAddPartitionsElement
    : partitionSpec partitionLocation?
    ;

alterStatementSuffixTouch
    : KW_TOUCH partitionSpec*
    ;

alterStatementSuffixArchive
    : KW_ARCHIVE partitionSpec*
    ;

alterStatementSuffixUnArchive
    : KW_UNARCHIVE partitionSpec*
    ;

partitionLocation
    : KW_LOCATION StringLiteral
    ;

alterStatementSuffixDropPartitions
    : KW_DROP ifExists? dropPartitionSpec (COMMA dropPartitionSpec)* KW_PURGE? replicationClause?
    ;

alterStatementSuffixProperties
    : KW_SET KW_TBLPROPERTIES tableProperties
    | KW_UNSET KW_TBLPROPERTIES ifExists? tableProperties
    ;

alterViewSuffixProperties
    : KW_SET KW_TBLPROPERTIES tableProperties
    | KW_UNSET KW_TBLPROPERTIES ifExists? tableProperties
    ;

alterMaterializedViewSuffixRewrite
    : (rewriteEnabled | rewriteDisabled)
    ;

alterMaterializedViewSuffixRebuild
    : KW_REBUILD
    ;

alterStatementSuffixSerdeProperties
    : KW_SET KW_SERDE StringLiteral (KW_WITH KW_SERDEPROPERTIES tableProperties)?
    | KW_SET KW_SERDEPROPERTIES tableProperties
    ;

alterIndexStatementSuffix
    : identifier KW_ON tableName
    partitionSpec?
    KW_REBUILD;

alterStatementSuffixFileFormat
	: KW_SET KW_FILEFORMAT fileFormat
	;

alterStatementSuffixClusterbySortby
  : KW_NOT KW_CLUSTERED
  | KW_NOT KW_SORTED
  | tableBuckets
  ;

alterTblPartitionStatementSuffixSkewedLocation
  : KW_SET KW_SKEWED KW_LOCATION skewedLocations
  ;

skewedLocations
    : LPAREN skewedLocationsList RPAREN
    ;

skewedLocationsList
    : skewedLocationMap (COMMA skewedLocationMap)*
    ;

skewedLocationMap
    : skewedValueLocationElement EQUAL StringLiteral
    ;

alterStatementSuffixLocation
  : KW_SET KW_LOCATION StringLiteral
  ;


alterStatementSuffixSkewedby
	: tableSkewed
	| KW_NOT KW_SKEWED
	| KW_NOT storedAsDirs
	;

alterStatementSuffixExchangePartition
    : KW_EXCHANGE partitionSpec KW_WITH KW_TABLE tableName
    ;

alterStatementSuffixRenamePart
    : KW_RENAME KW_TO partitionSpec
    ;

alterStatementSuffixStatsPart
    : KW_UPDATE KW_STATISTICS KW_FOR KW_COLUMN? identifier KW_SET tableProperties (KW_COMMENT StringLiteral)?
    ;

alterStatementSuffixMergeFiles
    : KW_CONCATENATE
    ;

alterStatementSuffixBucketNum
    : KW_INTO Number KW_BUCKETS
    ;

createIndexStatement
    : KW_CREATE KW_INDEX identifier KW_ON KW_TABLE tableName columnParenthesesList KW_AS StringLiteral
    (KW_WITH KW_DEFERRED KW_REBUILD)?
    (KW_IDXPROPERTIES tableProperties)?
    (KW_IN KW_TABLE tableName)?
    (KW_PARTITIONED KW_BY columnParenthesesList)?
    (tableRowFormat? tableFileFormat)?
    (KW_LOCATION locationPath)?
    tablePropertiesPrefixed?
    tableComment?;

locationPath
    : identifier (DOT identifier)*
    ;

dropIndexStatement
    : KW_DROP KW_INDEX identifier KW_ON tableName;

tablePartitionPrefix
  : tableName partitionSpec?
  ;

blocking
  : KW_AND KW_WAIT
  ;

alterStatementSuffixCompact
    : KW_COMPACT StringLiteral blocking? (KW_WITH KW_OVERWRITE KW_TBLPROPERTIES tableProperties)?
    ;

alterStatementSuffixSetOwner
    : KW_SET KW_OWNER principalName
    ;

fileFormat
    : KW_INPUTFORMAT StringLiteral KW_OUTPUTFORMAT StringLiteral KW_SERDE StringLiteral (KW_INPUTDRIVER StringLiteral KW_OUTPUTDRIVER StringLiteral)?
    | identifier
    ;

inputFileFormat
    : KW_INPUTFORMAT StringLiteral KW_SERDE StringLiteral
    ;

tabTypeExpr
   : identifier (DOT identifier)?
   ( identifier (DOT
   ( KW_ELEM_TYPE
   | KW_KEY_TYPE
   | KW_VALUE_TYPE
   | identifier )
   )*
   )?
   ;

partTypeExpr
    : tabTypeExpr partitionSpec?
    ;

tabPartColTypeExpr
    : tableName partitionSpec? extColumnName?
    ;

descStatement
    : (KW_DESCRIBE|KW_DESC)
    (
    (KW_DATABASE|KW_SCHEMA) KW_EXTENDED? identifier
    | KW_FUNCTION KW_EXTENDED? descFuncNames
    | ((KW_FORMATTED|KW_EXTENDED) tabPartColTypeExpr)
    | tabPartColTypeExpr
    )
    ;

analyzeStatement
    : KW_ANALYZE KW_TABLE (tableOrPartition)
      ( KW_COMPUTE KW_STATISTICS (KW_NOSCAN | (KW_FOR KW_COLUMNS columnNameList?))?
      | KW_CACHE KW_METADATA
      )
    ;

showStatement
    : KW_SHOW (KW_DATABASES|KW_SCHEMAS) (KW_LIKE showStmtIdentifier)?
    | KW_SHOW KW_TABLES ((KW_FROM|KW_IN) identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_VIEWS ((KW_FROM|KW_IN) identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_MATERIALIZED KW_VIEWS ((KW_FROM|KW_IN) identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_COLUMNS (KW_FROM|KW_IN) tableName ((KW_FROM|KW_IN) identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_FUNCTIONS (KW_LIKE showFunctionIdentifier|showFunctionIdentifier)?
    | KW_SHOW KW_PARTITIONS tableName partitionSpec?
    | KW_SHOW KW_CREATE (
        (KW_DATABASE|KW_SCHEMA) identifier
        |
        KW_TABLE tableName
      )
    | KW_SHOW KW_TABLE KW_EXTENDED ((KW_FROM|KW_IN) identifier)? KW_LIKE showStmtIdentifier partitionSpec?
    | KW_SHOW KW_TBLPROPERTIES tableName (LPAREN StringLiteral RPAREN)?
    | KW_SHOW KW_LOCKS
      (
      (KW_DATABASE|KW_SCHEMA) identifier KW_EXTENDED?
      |
      partTypeExpr? KW_EXTENDED?
      )
    | KW_SHOW KW_COMPACTIONS
    | KW_SHOW KW_TRANSACTIONS
    | KW_SHOW KW_CONF StringLiteral
    | KW_SHOW KW_RESOURCE
      (
        (KW_PLAN identifier)
        | KW_PLANS
      )
    ;

lockStatement
    : KW_LOCK KW_TABLE tableName partitionSpec? lockMode
    ;

lockDatabase
    : KW_LOCK (KW_DATABASE|KW_SCHEMA) identifier lockMode
    ;

lockMode
    : KW_SHARED | KW_EXCLUSIVE
    ;

unlockStatement
    : KW_UNLOCK KW_TABLE tableName partitionSpec?
    ;

unlockDatabase
    : KW_UNLOCK (KW_DATABASE|KW_SCHEMA) identifier
    ;

createRoleStatement
    : KW_CREATE KW_ROLE identifier
    ;

dropRoleStatement
    : KW_DROP KW_ROLE identifier
    ;

grantPrivileges
    : KW_GRANT privilegeList
      privilegeObject?
      KW_TO principalSpecification
      withGrantOption?
    ;

revokePrivileges
    : KW_REVOKE grantOptionFor? privilegeList privilegeObject? KW_FROM principalSpecification
    ;

grantRole
    : KW_GRANT KW_ROLE? identifier (COMMA identifier)* KW_TO principalSpecification withAdminOption?
    ;

revokeRole
    : KW_REVOKE adminOptionFor? KW_ROLE? identifier (COMMA identifier)* KW_FROM principalSpecification
    ;

showRoleGrants
    : KW_SHOW KW_ROLE KW_GRANT principalName
    ;


showRoles
    : KW_SHOW KW_ROLES
    ;

showCurrentRole
    : KW_SHOW KW_CURRENT KW_ROLES
    ;

setRole
    : KW_SET KW_ROLE
    (
    KW_ALL
    |
    KW_NONE
    |
    identifier
    )
    ;

showGrants
    : KW_SHOW KW_GRANT principalName? (KW_ON privilegeIncludeColObject)?
    ;

showRolePrincipals
    : KW_SHOW KW_PRINCIPALS identifier
    ;


privilegeIncludeColObject
    : KW_ALL
    | privObjectCols
    ;

privilegeObject
    : KW_ON privObject
    ;

// database or table type. Type is optional, default type is table
privObject
    : (KW_DATABASE|KW_SCHEMA) identifier
    | KW_TABLE? tableName partitionSpec?
    | KW_URI StringLiteral
    | KW_SERVER identifier
    ;

privObjectCols
    : (KW_DATABASE|KW_SCHEMA) identifier
    | KW_TABLE? tableName (LPAREN columnNameList RPAREN)? partitionSpec?
    | KW_URI StringLiteral
    | KW_SERVER identifier
    ;

privilegeList
    : privlegeDef (COMMA privlegeDef)*
    ;

privlegeDef
    : privilegeType (LPAREN columnNameList RPAREN)?
    ;

privilegeType
    : KW_ALL
    | KW_ALTER
    | KW_UPDATE
    | KW_CREATE
    | KW_DROP
    | KW_LOCK
    | KW_SELECT
    | KW_SHOW_DATABASE
    | KW_INSERT
    | KW_DELETE
    ;

principalSpecification
    : principalName (COMMA principalName)*
    ;

principalName
    : KW_USER principalIdentifier
    | KW_GROUP principalIdentifier
    | KW_ROLE identifier
    ;

withGrantOption
    : KW_WITH KW_GRANT KW_OPTION
    ;

grantOptionFor
    : KW_GRANT KW_OPTION KW_FOR
;

adminOptionFor
    : KW_ADMIN KW_OPTION KW_FOR
;

withAdminOption
    : KW_WITH KW_ADMIN KW_OPTION
    ;

metastoreCheck
    : KW_MSCK KW_REPAIR?
      (KW_TABLE tableName
        ((KW_ADD | KW_DROP | KW_SYNC) KW_PARTITIONS)? |
        partitionSpec?)
    ;

resourceList
  :
  resource (COMMA resource)*
  ;

resource
  : resourceType StringLiteral
  ;

resourceType
  : KW_JAR
  | KW_FILE
  | KW_ARCHIVE
  ;

createFunctionStatement
    : KW_CREATE KW_TEMPORARY? KW_FUNCTION functionIdentifier KW_AS StringLiteral
      (KW_USING resourceList)?
    ;

dropFunctionStatement
    : KW_DROP KW_TEMPORARY? KW_FUNCTION ifExists? functionIdentifier
    ;

reloadFunctionStatement
    : KW_RELOAD KW_FUNCTION
    ;

createMacroStatement
    : KW_CREATE KW_TEMPORARY KW_MACRO Identifier
      LPAREN columnNameTypeList? RPAREN expression
    ;

dropMacroStatement
    : KW_DROP KW_TEMPORARY KW_MACRO ifExists? Identifier
    ;

createViewStatement
    : KW_CREATE orReplace? KW_VIEW ifNotExists? tableName
        (LPAREN columnNameCommentList RPAREN)? tableComment? viewPartition?
        tablePropertiesPrefixed?
        KW_AS
        selectStatementWithCTE
    ;

createMaterializedViewStatement
    : KW_CREATE KW_MATERIALIZED KW_VIEW ifNotExists? tableName
        rewriteDisabled? tableComment? tableRowFormat? tableFileFormat? tableLocation?
        tablePropertiesPrefixed? KW_AS selectStatementWithCTE
    ;

viewPartition
    : KW_PARTITIONED KW_ON LPAREN columnNameList RPAREN
    ;

dropViewStatement
    : KW_DROP KW_VIEW ifExists? viewName
    ;

dropMaterializedViewStatement
    : KW_DROP KW_MATERIALIZED KW_VIEW ifExists? viewName
    ;

showFunctionIdentifier
    : functionIdentifier
    | StringLiteral
    ;

showStmtIdentifier
    : identifier
    | StringLiteral
    ;

tableComment
    : KW_COMMENT StringLiteral
    ;

tablePartition
    : KW_PARTITIONED KW_BY LPAREN columnNameTypeConstraint (COMMA columnNameTypeConstraint)* RPAREN
    ;

tableBuckets
    : KW_CLUSTERED KW_BY LPAREN columnNameList RPAREN (KW_SORTED KW_BY LPAREN columnNameOrderList RPAREN)? KW_INTO Number KW_BUCKETS
    ;

tableSkewed
    : KW_SKEWED KW_BY LPAREN columnNameList RPAREN KW_ON LPAREN skewedValueElement RPAREN storedAsDirs?
    ;

rowFormat
    : rowFormatSerde
    | rowFormatDelimited
    ;

recordReader
    : KW_RECORDREADER StringLiteral
    ;

recordWriter
    : KW_RECORDWRITER StringLiteral
    ;

rowFormatSerde
    : KW_ROW KW_FORMAT KW_SERDE StringLiteral (KW_WITH KW_SERDEPROPERTIES tableProperties)?
    ;

rowFormatDelimited
    : KW_ROW KW_FORMAT KW_DELIMITED tableRowFormatFieldIdentifier? tableRowFormatCollItemsIdentifier? tableRowFormatMapKeysIdentifier? tableRowFormatLinesIdentifier? tableRowNullFormat?
    ;

tableRowFormat
    : rowFormatDelimited
    | rowFormatSerde
    ;

tablePropertiesPrefixed
    : KW_TBLPROPERTIES tableProperties
    ;

tableProperties
    : LPAREN tablePropertiesList RPAREN
    ;

tablePropertiesList
    : keyValueProperty (COMMA keyValueProperty)*
    | keyProperty (COMMA keyProperty)*
    ;

keyValueProperty
    : StringLiteral EQUAL StringLiteral
    ;

keyProperty
    : StringLiteral
    ;

tableRowFormatFieldIdentifier
    : KW_FIELDS KW_TERMINATED KW_BY StringLiteral (KW_ESCAPED KW_BY StringLiteral)?
    ;

tableRowFormatCollItemsIdentifier
    : KW_COLLECTION KW_ITEMS KW_TERMINATED KW_BY StringLiteral
    ;

tableRowFormatMapKeysIdentifier
    : KW_MAP KW_KEYS KW_TERMINATED KW_BY StringLiteral
    ;

tableRowFormatLinesIdentifier
    : KW_LINES KW_TERMINATED KW_BY StringLiteral
    ;

tableRowNullFormat
    : KW_NULL KW_DEFINED KW_AS StringLiteral
    ;
tableFileFormat
    : KW_STORED KW_AS KW_INPUTFORMAT StringLiteral KW_OUTPUTFORMAT StringLiteral (KW_INPUTDRIVER StringLiteral KW_OUTPUTDRIVER StringLiteral)?
      | KW_STORED KW_BY StringLiteral
         (KW_WITH KW_SERDEPROPERTIES tableProperties)?
      | KW_STORED KW_AS identifier
    ;

tableLocation
    : KW_LOCATION StringLiteral
    ;

columnNameTypeList
    : columnNameType (COMMA columnNameType)*
    ;

columnNameTypeOrConstraintList
    : columnNameTypeOrConstraint (COMMA columnNameTypeOrConstraint)*
    ;

columnNameColonTypeList
    : columnNameColonType (COMMA columnNameColonType)*
    ;

columnNameList
    : columnName (COMMA columnName)*
    ;

columnName
    : identifier
    ;

extColumnName
    : identifier (DOT (KW_ELEM_TYPE | KW_KEY_TYPE | KW_VALUE_TYPE | identifier))*
    ;

columnNameOrderList
    : columnNameOrder (COMMA columnNameOrder)*
    ;

columnParenthesesList
    : LPAREN columnNameList RPAREN
    ;

enableValidateSpecification
    : enableSpecification validateSpecification?
    | enforcedSpecification
    ;

enableSpecification
    : KW_ENABLE
    | KW_DISABLE
    ;

validateSpecification
    : KW_VALIDATE
    | KW_NOVALIDATE
    ;

enforcedSpecification
    : KW_ENFORCED
    | KW_NOT KW_ENFORCED
    ;

relySpecification
    :  KW_RELY
    |  (KW_NORELY)?
    ;

createConstraint
    : (KW_CONSTRAINT identifier)? pkConstraint constraintOptsCreate?
    ;

alterConstraintWithName
    : KW_CONSTRAINT identifier pkConstraint constraintOptsAlter?
    ;

pkConstraint
    : tableConstraintPrimaryKey pkCols=columnParenthesesList
    ;

createForeignKey
    : (KW_CONSTRAINT identifier)? KW_FOREIGN KW_KEY columnParenthesesList  KW_REFERENCES tableName columnParenthesesList constraintOptsCreate?
    ;

alterForeignKeyWithName
    : KW_CONSTRAINT identifier KW_FOREIGN KW_KEY columnParenthesesList  KW_REFERENCES tableName columnParenthesesList constraintOptsAlter?
    ;

skewedValueElement
    : skewedColumnValues
    | skewedColumnValuePairList
    ;

skewedColumnValuePairList
    : skewedColumnValuePair (COMMA skewedColumnValuePair)*
    ;

skewedColumnValuePair
    : LPAREN skewedColumnValues RPAREN
    ;

skewedColumnValues
    : skewedColumnValue (COMMA skewedColumnValue)*
    ;

skewedColumnValue
    : constant
    ;

skewedValueLocationElement
    : skewedColumnValue
    | skewedColumnValuePair
    ;

orderSpecification
    : KW_ASC
    | KW_DESC
    ;

nullOrdering
    : KW_NULLS KW_FIRST
    | KW_NULLS KW_LAST
    ;

columnNameOrder
    : identifier orderSpecification? nullOrdering?
    ;

columnNameCommentList
    : columnNameComment (COMMA columnNameComment)*
    ;

columnNameComment
    : identifier (KW_COMMENT StringLiteral)?
    ;

columnRefOrder
    : expression orderSpecification? nullOrdering?
    ;

columnNameType
    : identifier colType (KW_COMMENT StringLiteral)?
    ;

columnNameTypeOrConstraint
    : ( tableConstraint )
    | ( columnNameTypeConstraint )
    ;

tableConstraint
    : ( createForeignKey )
    | ( createConstraint )
    ;

columnNameTypeConstraint
    : identifier colType columnConstraint? (KW_COMMENT StringLiteral)?
    ;

columnConstraint
    : ( foreignKeyConstraint )
    | ( colConstraint )
    ;

foreignKeyConstraint
    : (KW_CONSTRAINT identifier)? KW_REFERENCES tableName LPAREN columnName RPAREN constraintOptsCreate?
    ;

colConstraint
    : (KW_CONSTRAINT identifier)? tableConstraintPrimaryKey constraintOptsCreate?
    ;

alterColumnConstraint
    : ( alterForeignKeyConstraint )
    | ( alterColConstraint )
    ;

alterForeignKeyConstraint
    : (KW_CONSTRAINT identifier)? KW_REFERENCES tableName LPAREN columnName RPAREN constraintOptsAlter?
    ;

alterColConstraint
    : (KW_CONSTRAINT identifier)? tableConstraintPrimaryKey constraintOptsAlter?
    ;

tableConstraintPrimaryKey
    : KW_PRIMARY KW_KEY
    ;

constraintOptsCreate
    : enableValidateSpecification relySpecification
    ;

constraintOptsAlter
    : enableValidateSpecification relySpecification
    ;

columnNameColonType
    : identifier COLON colType (KW_COMMENT StringLiteral)?
    ;

colType
    : type_db_col
    ;

colTypeList
    : colType (COMMA colType)*
    ;

type_db_col
    : primitiveType
    | listType
    | structType
    | mapType
    | unionType
    ;

primitiveType
    : KW_TINYINT
    | KW_SMALLINT
    | KW_INT
    | KW_BIGINT
    | KW_BOOLEAN
    | KW_FLOAT
    | KW_DOUBLE KW_PRECISION?
    | KW_DATE
    | KW_DATETIME
    | KW_TIMESTAMP
    | KW_TIMESTAMPLOCALTZ
    | KW_TIMESTAMP KW_WITH KW_LOCAL KW_TIME KW_ZONE
    | KW_STRING
    | KW_BINARY
    | KW_DECIMAL (LPAREN Number (COMMA Number)? RPAREN)?
    | KW_VARCHAR LPAREN Number RPAREN
    | KW_CHAR LPAREN Number RPAREN
    ;

listType
    : KW_ARRAY LESSTHAN type_db_col GREATERTHAN
    ;

structType
    : KW_STRUCT LESSTHAN columnNameColonTypeList GREATERTHAN
    ;

mapType
    : KW_MAP LESSTHAN primitiveType COMMA type_db_col GREATERTHAN
    ;

unionType
    : KW_UNIONTYPE LESSTHAN colTypeList GREATERTHAN
    ;

setOperator
    : KW_UNION KW_ALL
    | KW_UNION KW_DISTINCT?
    | KW_INTERSECT KW_ALL
    | KW_INTERSECT KW_DISTINCT?
    | KW_EXCEPT KW_ALL
    | KW_EXCEPT KW_DISTINCT?
    | KW_MINUS KW_ALL
    | KW_MINUS KW_DISTINCT?
    ;

queryStatementExpression
    :
    /* Would be nice to do this as a gated semantic perdicate
       But the predicate gets pushed as a lookahead decision.
       Calling rule doesnot know about topLevel
    */
    withClause?
    queryStatementExpressionBody
    ;

queryStatementExpressionBody
    : fromStatement
    | regularBody
    ;

withClause
    : KW_WITH cteStatement (COMMA cteStatement)*
    ;

cteStatement
    : identifier KW_AS LPAREN queryStatementExpression RPAREN
    ;

fromStatement
    : (singleFromStatement)
	(setOperator singleFromStatement)*
	;


singleFromStatement
    : fromClause body+
    ;

/*
The valuesClause rule below ensures that the parse tree for
"insert into table FOO values (1,2),(3,4)" looks the same as
"insert into table FOO select a,b from (values(1,2),(3,4)) as BAR(a,b)" which itself is made to look
very similar to the tree for "insert into table FOO select a,b from BAR".
*/
regularBody
    : insertClause ( selectStatement | valuesClause)
    | selectStatement
    ;

atomSelectStatement
    : selectClause
    fromClause?
    whereClause?
    groupByClause?
    havingClause?
    window_clause?
    | LPAREN selectStatement RPAREN
    ;

selectStatement
    : atomSelectStatement setOpSelectStatement?  orderByClause?  clusterByClause?  distributeByClause?  sortByClause?  limitClause?
    ;

setOpSelectStatement
    :
    (setOperator atomSelectStatement)+
    ;

selectStatementWithCTE
    : withClause? selectStatement
    ;

body
    : insertClause
    selectClause
    lateralView?
    whereClause?
    groupByClause?
    havingClause?
    window_clause?
    orderByClause?
    clusterByClause?
    distributeByClause?
    sortByClause?
    limitClause?
    |
    selectClause
    lateralView?
    whereClause?
    groupByClause?
    havingClause?
    window_clause?
    orderByClause?
    clusterByClause?
    distributeByClause?
    sortByClause?
    limitClause?
    ;

insertClause
    : KW_INSERT KW_OVERWRITE destination ifNotExists?
    | KW_INSERT KW_INTO KW_TABLE? tableOrPartition (LPAREN columnNameList RPAREN)?
    ;

destination
   : KW_LOCAL? KW_DIRECTORY StringLiteral tableRowFormat? tableFileFormat?
   | KW_TABLE tableOrPartition
   ;

limitClause
   : KW_LIMIT ((Number COMMA)? Number)
   | KW_LIMIT Number KW_OFFSET Number
   ;

//DELETE FROM <tableName> WHERE ...;
deleteStatement
   : KW_DELETE KW_FROM tableName whereClause?
   ;

/*SET <columName> = (3 + col2)*/
columnAssignmentClause
   : tableOrColumn EQUAL expression
   ;

/*SET col1 = 5, col2 = (4 + col4), ...*/
setColumnsClause
   : KW_SET columnAssignmentClause (COMMA columnAssignmentClause)*
   ;

/*
  UPDATE <table>
  SET col1 = val1, col2 = val2... WHERE ...
*/
updateStatement
   : KW_UPDATE tableName setColumnsClause whereClause?
   ;

/*
BEGIN user defined transaction boundaries; follows SQL 2003 standard exactly except for addition of
"setAutoCommitStatement" which is not in the standard doc but is supported by most SQL engines.
*/
sqlTransactionStatement
  : startTransactionStatement
  | commitStatement
  | rollbackStatement
  | setAutoCommitStatement
  ;

startTransactionStatement
  : KW_START KW_TRANSACTION ( transactionMode  ( COMMA transactionMode )* )?
  ;

transactionMode
  : isolationLevel
  | transactionAccessMode
  ;

transactionAccessMode
  : KW_READ KW_ONLY
  | KW_READ KW_WRITE
  ;

isolationLevel
  : KW_ISOLATION KW_LEVEL levelOfIsolation
  ;

/*READ UNCOMMITTED | READ COMMITTED | REPEATABLE READ | SERIALIZABLE may be supported later*/
levelOfIsolation
  : KW_SNAPSHOT
  ;

commitStatement
  : KW_COMMIT KW_WORK?
  ;

rollbackStatement
  : KW_ROLLBACK KW_WORK?
  ;
setAutoCommitStatement
  : KW_SET KW_AUTOCOMMIT booleanValueTok
  ;
/*
END user defined transaction boundaries
*/

abortTransactionStatement
  : KW_ABORT KW_TRANSACTIONS Number+
  ;


/*
BEGIN SQL Merge statement
*/
mergeStatement
   : KW_MERGE KW_INTO tableName (KW_AS? identifier)? KW_USING joinSourcePart KW_ON expression whenClauses
   ;
/*
Allow 0,1 or 2 WHEN MATCHED clauses and 0 or 1 WHEN NOT MATCHED
Each WHEN clause may have AND <boolean predicate>.
If 2 WHEN MATCHED clauses are present, 1 must be UPDATE the other DELETE and the 1st one
must have AND <boolean predicate>
*/
whenClauses
   : (whenMatchedAndClause|whenMatchedThenClause)* whenNotMatchedClause?
   ;
whenNotMatchedClause
   : KW_WHEN KW_NOT KW_MATCHED (KW_AND expression)? KW_THEN KW_INSERT KW_VALUES valueRowConstructor
   ;
whenMatchedAndClause
   : KW_WHEN KW_MATCHED KW_AND expression KW_THEN updateOrDelete
   ;
whenMatchedThenClause
   : KW_WHEN KW_MATCHED KW_THEN updateOrDelete
   ;
updateOrDelete
   : KW_UPDATE setColumnsClause
   | KW_DELETE
   ;
/*
END SQL Merge statement
*/

killQueryStatement
  : KW_KILL KW_QUERY StringLiteral+
  ;
