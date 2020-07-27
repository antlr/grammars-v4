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

   @author Canwei He
*/
parser grammar HiveParser;

import SelectClauseParser, FromClauseParser, IdentifiersParser, ResourcePlanParser;

options
{
  tokenVocab=HiveLexer;
}

// starting rule
statement
	: explainStatement EOF
	| execStatement EOF
	;

explainStatement
	: KW_EXPLAIN (
	    explainOption* execStatement
        |
        KW_REWRITE queryStatementExpression
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
    : KW_LOAD KW_DATA (islocal=KW_LOCAL)? KW_INPATH (path=StringLiteral) (isoverwrite=KW_OVERWRITE)? KW_INTO KW_TABLE (tab=tableOrPartition) inputFileFormat?
    ;

replicationClause
    : KW_FOR (isMetadataOnly=KW_METADATA)? KW_REPLICATION LPAREN (replId=StringLiteral) RPAREN
    ;

exportStatement
    : KW_EXPORT
      KW_TABLE (tab=tableOrPartition)
      KW_TO (path=StringLiteral)
      replicationClause?
    ;

importStatement
       : KW_IMPORT
         ((ext=KW_EXTERNAL)? KW_TABLE (tab=tableOrPartition))?
         KW_FROM (path=StringLiteral)
         tableLocation?
    ;

replDumpStatement
      : KW_REPL KW_DUMP
        (dbName=identifier) (DOT tblName=identifier)?
        (KW_FROM (eventId=Number)
          (KW_TO (rangeEnd=Number))?
          (KW_LIMIT (batchSize=Number))?
        )?
        (KW_WITH replConf=replConfigs)?
    ;

replLoadStatement
      : KW_REPL KW_LOAD
        ((dbName=identifier) (DOT tblName=identifier)?)?
        KW_FROM (path=StringLiteral)
        (KW_WITH replConf=replConfigs)?
      ;

replConfigs
    :
      LPAREN replConfigsList RPAREN
    ;

replConfigsList
    :
      keyValueProperty (COMMA keyValueProperty)*
    ;

replStatusStatement
      : KW_REPL KW_STATUS
        (dbName=identifier) (DOT tblName=identifier)?
        (KW_WITH replConf=replConfigs)?
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
    | resourcePlanDdlStatements
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
        name=identifier
        databaseComment?
        dbLocation?
        (KW_WITH KW_DBPROPERTIES dbprops=dbProperties)?
    ;

dbLocation
    :
      KW_LOCATION locn=StringLiteral
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

    : KW_COMMENT comment=StringLiteral
    ;

createTableStatement
    : KW_CREATE (temp=KW_TEMPORARY)? (ext=KW_EXTERNAL)? KW_TABLE ifNotExists? name=tableName
      (  like=KW_LIKE likeName=tableName
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
    ;

alterTableStatementSuffix
    : (alterStatementSuffixRename)
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
  | alterStatementSuffixUpdateColumns
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
    : name=identifier KW_SET KW_DBPROPERTIES dbProperties
    ;

alterDatabaseSuffixSetOwner
    : dbName=identifier KW_SET KW_OWNER principalName
    ;

alterDatabaseSuffixSetLocation
    : dbName=identifier KW_SET KW_LOCATION newLocation=StringLiteral
    ;

alterStatementSuffixRename
    : KW_RENAME KW_TO tableName
    ;

alterStatementSuffixAddCol
    : (add=KW_ADD | replace=KW_REPLACE) KW_COLUMNS LPAREN columnNameTypeList RPAREN restrictOrCascade?
    ;

alterStatementSuffixAddConstraint
   :  KW_ADD (fk=alterForeignKeyWithName | alterConstraintWithName)
   ;

alterStatementSuffixUpdateColumns
    : KW_UPDATE KW_COLUMNS restrictOrCascade?
    ;

alterStatementSuffixDropConstraint
   : KW_DROP KW_CONSTRAINT cName=identifier
   ;

alterStatementSuffixRenameCol
    : KW_CHANGE KW_COLUMN? oldName=identifier newName=identifier colType alterColumnConstraint? (KW_COMMENT comment=StringLiteral)? alterStatementChangeColPosition? restrictOrCascade?
    ;

alterStatementSuffixUpdateStatsCol
    : KW_UPDATE KW_STATISTICS KW_FOR KW_COLUMN? colName=identifier KW_SET tableProperties (KW_COMMENT comment=StringLiteral)?
    ;

alterStatementSuffixUpdateStats
    : KW_UPDATE KW_STATISTICS KW_SET tableProperties
    ;

alterStatementChangeColPosition
    : first=KW_FIRST|KW_AFTER afterCol=identifier
    ;

alterStatementSuffixAddPartitions
    : KW_ADD ifNotExists? alterStatementSuffixAddPartitionsElement+
    ;

alterStatementSuffixAddPartitionsElement
    : partitionSpec partitionLocation?
    ;

alterStatementSuffixTouch
    : KW_TOUCH (partitionSpec)*
    ;

alterStatementSuffixArchive
    : KW_ARCHIVE (partitionSpec)*
    ;

alterStatementSuffixUnArchive
    : KW_UNARCHIVE (partitionSpec)*
    ;

partitionLocation
    :
      KW_LOCATION locn=StringLiteral
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
    : KW_SET KW_SERDE serdeName=StringLiteral (KW_WITH KW_SERDEPROPERTIES tableProperties)?
    | KW_SET KW_SERDEPROPERTIES tableProperties
    ;

tablePartitionPrefix
  : tableName partitionSpec?
  ;

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
    :
      LPAREN skewedLocationsList RPAREN
    ;

skewedLocationsList
    :
      skewedLocationMap (COMMA skewedLocationMap)*
    ;

skewedLocationMap
    :
      key=skewedValueLocationElement EQUAL value=StringLiteral
    ;

alterStatementSuffixLocation
  : KW_SET KW_LOCATION newLoc=StringLiteral
  ;


alterStatementSuffixSkewedby
	: tableSkewed
	| KW_NOT KW_SKEWED
	| KW_NOT storedAsDirs
	;

alterStatementSuffixExchangePartition
    : KW_EXCHANGE partitionSpec KW_WITH KW_TABLE exchangename=tableName
    ;

alterStatementSuffixRenamePart
    : KW_RENAME KW_TO partitionSpec
    ;

alterStatementSuffixStatsPart
    : KW_UPDATE KW_STATISTICS KW_FOR KW_COLUMN? colName=identifier KW_SET tableProperties (KW_COMMENT comment=StringLiteral)?
    ;

alterStatementSuffixMergeFiles
    : KW_CONCATENATE
    ;

alterStatementSuffixBucketNum
    : KW_INTO num=Number KW_BUCKETS
    ;

blocking
  : KW_AND KW_WAIT
  ;

alterStatementSuffixCompact
    : KW_COMPACT compactType=StringLiteral blocking? (KW_WITH KW_OVERWRITE KW_TBLPROPERTIES tableProperties)?
    ;

alterStatementSuffixSetOwner
    : KW_SET KW_OWNER principalName
    ;

fileFormat
    : KW_INPUTFORMAT inFmt=StringLiteral KW_OUTPUTFORMAT outFmt=StringLiteral KW_SERDE serdeCls=StringLiteral (KW_INPUTDRIVER inDriver=StringLiteral KW_OUTPUTDRIVER outDriver=StringLiteral)?
    | genericSpec=identifier
    ;

inputFileFormat
    : KW_INPUTFORMAT inFmt=StringLiteral KW_SERDE serdeCls=StringLiteral
    ;

tabTypeExpr
   : identifier (DOT identifier)?
   (identifier (DOT
   (
   KW_ELEM_TYPE
   |
   KW_KEY_TYPE
   |
   KW_VALUE_TYPE
   | identifier
   ))*
   )?
   ;

partTypeExpr
    :  tabTypeExpr partitionSpec?
    ;

tabPartColTypeExpr
    :  tableName partitionSpec? extColumnName?
    ;

descStatement
    :
    (KW_DESCRIBE|KW_DESC)
    (
    (KW_DATABASE|KW_SCHEMA) KW_EXTENDED? (dbName=identifier)
    |
    KW_FUNCTION KW_EXTENDED? (name=descFuncNames)
    |
    ((descOptions=KW_FORMATTED|descOptions=KW_EXTENDED) parttype=tabPartColTypeExpr)
    |
    parttype=tabPartColTypeExpr
    )
    ;

analyzeStatement
    : KW_ANALYZE KW_TABLE (parttype=tableOrPartition)
      (
      KW_COMPUTE KW_STATISTICS ((noscan=KW_NOSCAN) | (KW_FOR KW_COLUMNS (statsColumnName=columnNameList)?))?
      |
      KW_CACHE KW_METADATA
      )
    ;

showStatement
    : KW_SHOW (KW_DATABASES|KW_SCHEMAS) (KW_LIKE showStmtIdentifier)?
    | KW_SHOW KW_TABLES ((KW_FROM|KW_IN) db_name=identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_VIEWS ((KW_FROM|KW_IN) db_name=identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_MATERIALIZED KW_VIEWS ((KW_FROM|KW_IN) db_name=identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_COLUMNS (KW_FROM|KW_IN) tableName ((KW_FROM|KW_IN) db_name=identifier)? (KW_LIKE showStmtIdentifier|showStmtIdentifier)?
    | KW_SHOW KW_FUNCTIONS (KW_LIKE showFunctionIdentifier|showFunctionIdentifier)?
    | KW_SHOW KW_PARTITIONS tabName=tableName partitionSpec?
    | KW_SHOW KW_CREATE (
        (KW_DATABASE|KW_SCHEMA) db_name=identifier
        |
        KW_TABLE tabName=tableName
      )
    | KW_SHOW KW_TABLE KW_EXTENDED ((KW_FROM|KW_IN) db_name=identifier)? KW_LIKE showStmtIdentifier partitionSpec?
    | KW_SHOW KW_TBLPROPERTIES tableName (LPAREN prptyName=StringLiteral RPAREN)?
    | KW_SHOW KW_LOCKS
      (
      (KW_DATABASE|KW_SCHEMA) (dbName=identifier) (isExtended=KW_EXTENDED)?
      |
      (parttype=partTypeExpr)? (isExtended=KW_EXTENDED)?
      )
    | KW_SHOW KW_COMPACTIONS
    | KW_SHOW KW_TRANSACTIONS
    | KW_SHOW KW_CONF StringLiteral
    | KW_SHOW KW_RESOURCE
      (
        (KW_PLAN rp_name=identifier)
        | (KW_PLANS)
      )
    ;

lockStatement
    : KW_LOCK KW_TABLE tableName partitionSpec? lockMode
    ;

lockDatabase
    : KW_LOCK (KW_DATABASE|KW_SCHEMA) (dbName=identifier) lockMode
    ;

lockMode
    : KW_SHARED | KW_EXCLUSIVE
    ;

unlockStatement
    : KW_UNLOCK KW_TABLE tableName partitionSpec?
    ;

unlockDatabase
    : KW_UNLOCK (KW_DATABASE|KW_SCHEMA) (dbName=identifier)
    ;

createRoleStatement
    : KW_CREATE KW_ROLE roleName=identifier
    ;

dropRoleStatement
    : KW_DROP KW_ROLE roleName=identifier
    ;

grantPrivileges
    : KW_GRANT privList=privilegeList
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
    all=KW_ALL
    |
    none=KW_NONE
    |
    identifier
    )
    ;

showGrants
    : KW_SHOW KW_GRANT principalName? (KW_ON privilegeIncludeColObject)?
    ;

showRolePrincipals
    : KW_SHOW KW_PRINCIPALS roleName=identifier
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
    | KW_URI (path=StringLiteral)
    | KW_SERVER identifier
    ;

privObjectCols
    : (KW_DATABASE|KW_SCHEMA) identifier
    | KW_TABLE? tableName (LPAREN cols=columnNameList RPAREN)? partitionSpec?
    | KW_URI (path=StringLiteral)
    | KW_SERVER identifier
    ;

privilegeList
    : privlegeDef (COMMA privlegeDef)*
    ;

privlegeDef
    : privilegeType (LPAREN cols=columnNameList RPAREN)?
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
    : KW_MSCK (repair=KW_REPAIR)?
      (KW_TABLE tableName
        ((add=KW_ADD | drop=KW_DROP | sync=KW_SYNC) (parts=KW_PARTITIONS))? |
        (partitionSpec)?)
    ;

resourceList
  :
  resource (COMMA resource)*
  ;

resource
  :
  resType=resourceType resPath=StringLiteral
  ;

resourceType
  :
  KW_JAR
  |
  KW_FILE
  |
  KW_ARCHIVE
  ;

createFunctionStatement
    : KW_CREATE (temp=KW_TEMPORARY)? KW_FUNCTION functionIdentifier KW_AS StringLiteral
      (KW_USING rList=resourceList)?
    ;

dropFunctionStatement
    : KW_DROP (temp=KW_TEMPORARY)? KW_FUNCTION ifExists? functionIdentifier
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
    : KW_CREATE (orReplace)? KW_VIEW (ifNotExists)? name=tableName
        (LPAREN columnNameCommentList RPAREN)? tableComment? viewPartition?
        tablePropertiesPrefixed?
        KW_AS
        selectStatementWithCTE
    ;

createMaterializedViewStatement
    : KW_CREATE KW_MATERIALIZED KW_VIEW (ifNotExists)? name=tableName
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
    :
      KW_COMMENT comment=StringLiteral
    ;

tablePartition
    : KW_PARTITIONED KW_BY LPAREN columnNameTypeConstraint (COMMA columnNameTypeConstraint)* RPAREN
    ;

tableBuckets
    :
      KW_CLUSTERED KW_BY LPAREN bucketCols=columnNameList RPAREN (KW_SORTED KW_BY LPAREN sortCols=columnNameOrderList RPAREN)? KW_INTO num=Number KW_BUCKETS
    ;

tableSkewed
    :
     KW_SKEWED KW_BY LPAREN skewedCols=columnNameList RPAREN KW_ON LPAREN (skewedValues=skewedValueElement) RPAREN storedAsDirs?
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
    : KW_ROW KW_FORMAT KW_SERDE name=StringLiteral (KW_WITH KW_SERDEPROPERTIES serdeprops=tableProperties)?
    ;

rowFormatDelimited
    :
      KW_ROW KW_FORMAT KW_DELIMITED tableRowFormatFieldIdentifier? tableRowFormatCollItemsIdentifier? tableRowFormatMapKeysIdentifier? tableRowFormatLinesIdentifier? tableRowNullFormat?
    ;

tableRowFormat
    :
      rowFormatDelimited
    | rowFormatSerde
    ;

tablePropertiesPrefixed
    :
        KW_TBLPROPERTIES tableProperties
    ;

tableProperties
    :
      LPAREN tablePropertiesList RPAREN
    ;

tablePropertiesList
    :
      keyValueProperty (COMMA keyValueProperty)*
    |
      keyProperty (COMMA keyProperty)*
    ;

keyValueProperty
    :
      key=StringLiteral EQUAL value=StringLiteral
    ;

keyProperty
    :
      key=StringLiteral
    ;

tableRowFormatFieldIdentifier
    :
      KW_FIELDS KW_TERMINATED KW_BY fldIdnt=StringLiteral (KW_ESCAPED KW_BY fldEscape=StringLiteral)?
    ;

tableRowFormatCollItemsIdentifier
    :
      KW_COLLECTION KW_ITEMS KW_TERMINATED KW_BY collIdnt=StringLiteral
    ;

tableRowFormatMapKeysIdentifier
    :
      KW_MAP KW_KEYS KW_TERMINATED KW_BY mapKeysIdnt=StringLiteral
    ;

tableRowFormatLinesIdentifier
    :
      KW_LINES KW_TERMINATED KW_BY linesIdnt=StringLiteral
    ;

tableRowNullFormat
    :
      KW_NULL KW_DEFINED KW_AS nullIdnt=StringLiteral
    ;
tableFileFormat
    :
      KW_STORED KW_AS KW_INPUTFORMAT inFmt=StringLiteral KW_OUTPUTFORMAT outFmt=StringLiteral (KW_INPUTDRIVER inDriver=StringLiteral KW_OUTPUTDRIVER outDriver=StringLiteral)?
      | KW_STORED KW_BY storageHandler=StringLiteral
         (KW_WITH KW_SERDEPROPERTIES serdeprops=tableProperties)?
      | KW_STORED KW_AS genericSpec=identifier
    ;

tableLocation
    :
      KW_LOCATION locn=StringLiteral
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
    :
      identifier
    ;

extColumnName
    :
      identifier (DOT (KW_ELEM_TYPE | KW_KEY_TYPE | KW_VALUE_TYPE | identifier))*
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
    : (KW_CONSTRAINT constraintName=identifier)? tableLevelConstraint constraintOptsCreate?
    ;

alterConstraintWithName
    : KW_CONSTRAINT constraintName=identifier tableLevelConstraint constraintOptsAlter?
    ;

tableLevelConstraint
    : pkUkConstraint
    | checkConstraint
    ;

pkUkConstraint
    : tableConstraintType pkCols=columnParenthesesList
    ;

checkConstraint
    : KW_CHECK expression
    ;

createForeignKey
    : (KW_CONSTRAINT constraintName=identifier)? KW_FOREIGN KW_KEY fkCols=columnParenthesesList  KW_REFERENCES tabName=tableName parCols=columnParenthesesList constraintOptsCreate?
    ;

alterForeignKeyWithName
    : KW_CONSTRAINT constraintName=identifier KW_FOREIGN KW_KEY fkCols=columnParenthesesList  KW_REFERENCES tabName=tableName parCols=columnParenthesesList constraintOptsAlter?
    ;

skewedValueElement
    :
      skewedColumnValues
     | skewedColumnValuePairList
    ;

skewedColumnValuePairList
    : skewedColumnValuePair (COMMA skewedColumnValuePair)*
    ;

skewedColumnValuePair
    :
      LPAREN colValues=skewedColumnValues RPAREN
    ;

skewedColumnValues
    : skewedColumnValue (COMMA skewedColumnValue)*
    ;

skewedColumnValue
    :
      constant
    ;

skewedValueLocationElement
    :
      skewedColumnValue
     | skewedColumnValuePair
    ;

orderSpecification
    : KW_ASC | KW_DESC ;

nullOrdering
    : KW_NULLS KW_FIRST
    | KW_NULLS KW_LAST
    ;

columnNameOrder
    : identifier orderSpec=orderSpecification? nullSpec=nullOrdering?
    ;

columnNameCommentList
    : columnNameComment (COMMA columnNameComment)*
    ;

columnNameComment
    : colName=identifier (KW_COMMENT comment=StringLiteral)?
    ;

columnRefOrder
    : expression orderSpec=orderSpecification? nullSpec=nullOrdering?
    ;

columnNameType
    : colName=identifier colType (KW_COMMENT comment=StringLiteral)?
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
    : colName=identifier colType columnConstraint? (KW_COMMENT comment=StringLiteral)?
    ;

columnConstraint
    : ( foreignKeyConstraint )
    | ( colConstraint )
    ;

foreignKeyConstraint
    : (KW_CONSTRAINT constraintName=identifier)? KW_REFERENCES tabName=tableName LPAREN colName=columnName RPAREN constraintOptsCreate?
    ;

colConstraint
    : (KW_CONSTRAINT constraintName=identifier)? columnConstraintType constraintOptsCreate?
    ;

alterColumnConstraint
    : ( alterForeignKeyConstraint )
    | ( alterColConstraint )
    ;

alterForeignKeyConstraint
    : (KW_CONSTRAINT constraintName=identifier)? KW_REFERENCES tabName=tableName LPAREN colName=columnName RPAREN constraintOptsAlter?
    ;

alterColConstraint
    : (KW_CONSTRAINT constraintName=identifier)? columnConstraintType constraintOptsAlter?
    ;

columnConstraintType
    : KW_NOT KW_NULL
    | KW_DEFAULT defaultVal
    | checkConstraint
    | tableConstraintType
    ;

defaultVal
    : constant
    | function
    | castExpression
    ;

tableConstraintType
    : KW_PRIMARY KW_KEY
    | KW_UNIQUE
    ;

constraintOptsCreate
    : enableValidateSpecification relySpecification
    ;

constraintOptsAlter
    : enableValidateSpecification relySpecification
    ;

columnNameColonType
    : colName=identifier COLON colType (KW_COMMENT comment=StringLiteral)?
    ;

colType
    : type
    ;

colTypeList
    : colType (COMMA colType)*
    ;

type
    : primitiveType
    | listType
    | structType
    | mapType
    | unionType;

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
    | KW_DECIMAL (LPAREN prec=Number (COMMA scale=Number)? RPAREN)?
    | KW_VARCHAR LPAREN length=Number RPAREN
    | KW_CHAR LPAREN length=Number RPAREN
    ;

listType
    : KW_ARRAY LESSTHAN type GREATERTHAN
    ;

structType
    : KW_STRUCT LESSTHAN columnNameColonTypeList GREATERTHAN
    ;

mapType
    : KW_MAP LESSTHAN left=primitiveType COMMA right=type GREATERTHAN
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
    (w=withClause)?
    queryStatementExpressionBody
    ;

queryStatementExpressionBody
    :
    fromStatement
    | regularBody
    ;

withClause
  :
  KW_WITH cteStatement (COMMA cteStatement)*
;

cteStatement
   :
   identifier KW_AS LPAREN queryStatementExpression RPAREN
;

fromStatement
: (singleFromStatement)
	(u=setOperator r=singleFromStatement)*
	;


singleFromStatement
    :
    fromClause
    ( b+=body )+
    ;

/*
The valuesClause rule below ensures that the parse tree for
"insert into table FOO values (1,2),(3,4)" looks the same as
"insert into table FOO select a,b from (values(1,2),(3,4)) as BAR(a,b)" which itself is made to look
very similar to the tree for "insert into table FOO select a,b from BAR".
*/
regularBody
   :
   i=insertClause
   (
   s=selectStatement
     |
     valuesClause
   )
   |
   selectStatement
   ;

atomSelectStatement
   :
   s=selectClause
   f=fromClause?
   w=whereClause?
   g=groupByClause?
   h=havingClause?
   win=window_clause?
   |
   LPAREN selectStatement RPAREN
   ;

selectStatement
   :
   atomSelectStatement
   setOpSelectStatement?
   orderByClause?
   clusterByClause?
   distributeByClause?
   sortByClause?
   limitClause?
   ;

setOpSelectStatement
   :
   (u=setOperator b=atomSelectStatement)+
   ;

selectStatementWithCTE
    :
    (w=withClause)?
    selectStatement
    ;

body
   :
   insertClause
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
   :
     KW_INSERT KW_OVERWRITE destination ifNotExists?
   | KW_INSERT KW_INTO KW_TABLE? tableOrPartition (LPAREN targetCols=columnNameList RPAREN)?
   ;

destination
   :
     (local = KW_LOCAL)? KW_DIRECTORY StringLiteral tableRowFormat? tableFileFormat?
   | KW_TABLE tableOrPartition
   ;

limitClause
   :
   KW_LIMIT ((offset=Number COMMA)? num=Number)
   | KW_LIMIT num=Number KW_OFFSET offset=Number
   ;

//DELETE FROM <tableName> WHERE ...;
deleteStatement
   :
   KW_DELETE KW_FROM tableName (whereClause)?
   ;

/*SET <columName> = (3 + col2)*/
columnAssignmentClause
   :
   tableOrColumn EQUAL precedencePlusExpression
   ;

/*SET col1 = 5, col2 = (4 + col4), ...*/
setColumnsClause
   :
   KW_SET columnAssignmentClause (COMMA columnAssignmentClause)*
   ;

/*
  UPDATE <table>
  SET col1 = val1, col2 = val2... WHERE ...
*/
updateStatement
   :
   KW_UPDATE tableName setColumnsClause whereClause?
   ;

/*
BEGIN user defined transaction boundaries; follows SQL 2003 standard exactly except for addition of
"setAutoCommitStatement" which is not in the standard doc but is supported by most SQL engines.
*/
sqlTransactionStatement
  :
  startTransactionStatement
	|	commitStatement
	|	rollbackStatement
	| setAutoCommitStatement
	;

startTransactionStatement
  :
  KW_START KW_TRANSACTION ( transactionMode  ( COMMA transactionMode  )* )?
  ;

transactionMode
  :
  isolationLevel
  | transactionAccessMode
  ;

transactionAccessMode
  :
  KW_READ KW_ONLY
  | KW_READ KW_WRITE
  ;

isolationLevel
  :
  KW_ISOLATION KW_LEVEL levelOfIsolation
  ;

/*READ UNCOMMITTED | READ COMMITTED | REPEATABLE READ | SERIALIZABLE may be supported later*/
levelOfIsolation
  :
  KW_SNAPSHOT
  ;

commitStatement
  :
  KW_COMMIT ( KW_WORK )?
  ;

rollbackStatement
  :
  KW_ROLLBACK ( KW_WORK )?
  ;
setAutoCommitStatement
  :
  KW_SET KW_AUTOCOMMIT booleanValueTok
  ;
/*
END user defined transaction boundaries
*/

abortTransactionStatement
  :
  KW_ABORT KW_TRANSACTIONS ( Number )+
  ;


/*
BEGIN SQL Merge statement
*/
mergeStatement
   :
   KW_MERGE KW_INTO tableName (KW_AS? identifier)? KW_USING joinSourcePart KW_ON expression whenClauses
   ;
/*
Allow 0,1 or 2 WHEN MATCHED clauses and 0 or 1 WHEN NOT MATCHED
Each WHEN clause may have AND <boolean predicate>.
If 2 WHEN MATCHED clauses are present, 1 must be UPDATE the other DELETE and the 1st one
must have AND <boolean predicate>
*/
whenClauses
   :
   (whenMatchedAndClause|whenMatchedThenClause)* whenNotMatchedClause?
   ;
whenNotMatchedClause
   :
  KW_WHEN KW_NOT KW_MATCHED (KW_AND expression)? KW_THEN KW_INSERT KW_VALUES valueRowConstructor
  ;
whenMatchedAndClause
  :
  KW_WHEN KW_MATCHED KW_AND expression KW_THEN updateOrDelete
  ;
whenMatchedThenClause
  :
  KW_WHEN KW_MATCHED KW_THEN updateOrDelete
  ;
updateOrDelete
   :
   KW_UPDATE setColumnsClause
   |
   KW_DELETE
   ;
/*
END SQL Merge statement
*/

killQueryStatement
  :
  KW_KILL KW_QUERY ( StringLiteral )+
  ;
