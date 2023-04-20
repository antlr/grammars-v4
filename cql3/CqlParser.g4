/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Domagoj Kovačević
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
 * Project : cql-parser; an ANTLR4 grammar for Apache Cassandra CQL  https://github.com/kdcro101cql-parser
 */

parser grammar CqlParser;

options
   { tokenVocab = CqlLexer; }

root
   : cqls? MINUSMINUS? EOF
   ;

cqls
   : (cql MINUSMINUS? statementSeparator | empty_)* (cql (MINUSMINUS? statementSeparator)? | empty_)
   ;

statementSeparator
   : SEMI
   ;

empty_
   : statementSeparator
   ;

cql
   : alterKeyspace
   | alterMaterializedView
   | alterRole
   | alterTable
   | alterType
   | alterUser
   | applyBatch
   | createAggregate
   | createFunction
   | createIndex
   | createKeyspace
   | createMaterializedView
   | createRole
   | createTable
   | createTrigger
   | createType
   | createUser
   | delete_
   | dropAggregate
   | dropFunction
   | dropIndex
   | dropKeyspace
   | dropMaterializedView
   | dropRole
   | dropTable
   | dropTrigger
   | dropType
   | dropUser
   | grant
   | insert
   | listPermissions
   | listRoles
   | revoke
   | select_
   | truncate
   | update
   | use_
   ;

revoke
   : kwRevoke priviledge kwOn resource kwFrom role
   ;

listRoles
   : kwList kwRoles (kwOf role)? kwNorecursive?
   ;

listPermissions
   : kwList priviledge (kwOn resource)? (kwOf role)?
   ;

grant
   : kwGrant priviledge kwOn resource kwTo role
   ;

priviledge
   : (kwAll | kwAllPermissions)
   | kwAlter
   | kwAuthorize
   | kwDescibe
   | kwExecute
   | kwCreate
   | kwDrop
   | kwModify
   | kwSelect
   ;

resource
   : kwAll kwFunctions
   | kwAll kwFunctions kwIn kwKeyspace keyspace
   | kwFunction (keyspace DOT)? function_
   | kwAll kwKeyspaces
   | kwKeyspace keyspace
   | (kwTable)? (keyspace DOT)? table
   | kwAll kwRoles
   | kwRole role
   ;

createUser
   : kwCreate kwUser ifNotExist? user kwWith kwPassword stringLiteral (kwSuperuser | kwNosuperuser)?
   ;

createRole
   : kwCreate kwRole ifNotExist? role roleWith?
   ;

createType
   : kwCreate kwType ifNotExist? (keyspace DOT)? type_ syntaxBracketLr typeMemberColumnList syntaxBracketRr
   ;

typeMemberColumnList
   : column dataType (syntaxComma column dataType)*
   ;

createTrigger
   : kwCreate kwTrigger ifNotExist? (keyspace DOT)? trigger kwUsing triggerClass
   ;

createMaterializedView
   : kwCreate kwMaterialized kwView ifNotExist? (keyspace DOT)? materializedView kwAs kwSelect columnList kwFrom (keyspace DOT)? table materializedViewWhere kwPrimary kwKey syntaxBracketLr columnList syntaxBracketRr (kwWith materializedViewOptions)?
   ;

materializedViewWhere
   : kwWhere columnNotNullList (kwAnd relationElements)?
   ;

columnNotNullList
   : columnNotNull (kwAnd columnNotNull)*
   ;

columnNotNull
   : column kwIs kwNot kwNull
   ;

materializedViewOptions
   : tableOptions
   | tableOptions kwAnd clusteringOrder
   | clusteringOrder
   ;

// CREATE MATERIALIZED VIEW [IF NOT EXISTS] [keyspace_name.] view_name
// AS SELECT column_list
// FROM [keyspace_name.] base_table_name
// WHERE column_name IS NOT NULL [AND column_name IS NOT NULL ...]
//       [AND relation...]
// PRIMARY KEY ( column_list )
// [WITH [table_properties]
//       [AND CLUSTERING ORDER BY (cluster_column_name order_option )]]
createKeyspace
   : kwCreate kwKeyspace ifNotExist? keyspace kwWith kwReplication OPERATOR_EQ syntaxBracketLc replicationList syntaxBracketRc (kwAnd durableWrites)?
   ;

createFunction
   : kwCreate orReplace? kwFunction ifNotExist? (keyspace DOT)? function_ syntaxBracketLr paramList? syntaxBracketRr returnMode kwReturns dataType kwLanguage language kwAs codeBlock
   ;

codeBlock
   : CODE_BLOCK
   | STRING_LITERAL
   ;

paramList
   : param (syntaxComma param)*
   ;

returnMode
   : (kwCalled | kwReturns kwNull) kwOn kwNull kwInput
   ;

createAggregate
   : kwCreate orReplace? kwAggregate ifNotExist? (keyspace DOT)? aggregate syntaxBracketLr dataType syntaxBracketRr kwSfunc function_ kwStype dataType kwFinalfunc function_ kwInitcond initCondDefinition
   ;

// paramList
// :
initCondDefinition
   : constant
   | initCondList
   | initCondListNested
   | initCondHash
   ;

initCondHash
   : syntaxBracketLc initCondHashItem (syntaxComma initCondHashItem)* syntaxBracketRc
   ;

initCondHashItem
   : hashKey COLON initCondDefinition
   ;

initCondListNested
   : syntaxBracketLr initCondList (syntaxComma constant | initCondList)* syntaxBracketRr
   ;

initCondList
   : syntaxBracketLr constant (syntaxComma constant)* syntaxBracketRr
   ;

orReplace
   : kwOr kwReplace
   ;

alterUser
   : kwAlter kwUser user kwWith userPassword userSuperUser?
   ;

userPassword
   : kwPassword stringLiteral
   ;

userSuperUser
   : kwSuperuser
   | kwNosuperuser
   ;

alterType
   : kwAlter kwType (keyspace DOT)? type_ alterTypeOperation
   ;

alterTypeOperation
   : alterTypeAlterType
   | alterTypeAdd
   | alterTypeRename
   ;

alterTypeRename
   : kwRename alterTypeRenameList
   ;

alterTypeRenameList
   : alterTypeRenameItem (kwAnd alterTypeRenameItem)*
   ;

alterTypeRenameItem
   : column kwTo column
   ;

alterTypeAdd
   : kwAdd column dataType (syntaxComma column dataType)*
   ;

alterTypeAlterType
   : kwAlter column kwType dataType
   ;

alterTable
   : kwAlter kwTable (keyspace DOT)? table alterTableOperation
   ;

alterTableOperation
   : alterTableAdd
   | alterTableDropColumns
   | alterTableDropCompactStorage
   | alterTableRename
   | alterTableWith
   ;

alterTableWith
   : kwWith tableOptions
   ;

alterTableRename
   : kwRename column kwTo column
   ;

alterTableDropCompactStorage
   : kwDrop kwCompact kwStorage
   ;

alterTableDropColumns
   : kwDrop alterTableDropColumnList
   ;

alterTableDropColumnList
   : column (syntaxComma column)*
   ;

alterTableAdd
   : kwAdd alterTableColumnDefinition
   ;

alterTableColumnDefinition
   : column dataType (syntaxComma column dataType)*
   ;

alterRole
   : kwAlter kwRole role roleWith?
   ;

roleWith
   : kwWith (roleWithOptions (kwAnd roleWithOptions)*)
   ;

roleWithOptions
   : kwPassword OPERATOR_EQ stringLiteral
   | kwLogin OPERATOR_EQ booleanLiteral
   | kwSuperuser OPERATOR_EQ booleanLiteral
   | kwOptions OPERATOR_EQ optionHash
   ;

alterMaterializedView
   : kwAlter kwMaterialized kwView (keyspace DOT)? materializedView (kwWith tableOptions)?
   ;

dropUser
   : kwDrop kwUser ifExist? user
   ;

dropType
   : kwDrop kwType ifExist? (keyspace DOT)? type_
   ;

dropMaterializedView
   : kwDrop kwMaterialized kwView ifExist? (keyspace DOT)? materializedView
   ;

dropAggregate
   : kwDrop kwAggregate ifExist? (keyspace DOT)? aggregate
   ;

dropFunction
   : kwDrop kwFunction ifExist? (keyspace DOT)? function_
   ;

dropTrigger
   : kwDrop kwTrigger ifExist? trigger kwOn (keyspace DOT)? table
   ;

dropRole
   : kwDrop kwRole ifExist? role
   ;

dropTable
   : kwDrop kwTable ifExist? (keyspace DOT)? table
   ;

dropKeyspace
   : kwDrop kwKeyspace ifExist? keyspace
   ;

dropIndex
   : kwDrop kwIndex ifExist? (keyspace DOT)? indexName
   ;

createTable
   : kwCreate kwTable ifNotExist? (keyspace DOT)? table syntaxBracketLr columnDefinitionList syntaxBracketRr withElement?
   ;

withElement
   : kwWith tableOptions? clusteringOrder?
   ;

clusteringOrder
   : kwClustering kwOrder kwBy syntaxBracketLr column orderDirection? syntaxBracketRr
   ;

tableOptions
   : tableOptionItem (kwAnd tableOptionItem)*
   ;

tableOptionItem
   : tableOptionName OPERATOR_EQ tableOptionValue
   | tableOptionName OPERATOR_EQ optionHash
   ;

tableOptionName
   : OBJECT_NAME
   ;

tableOptionValue
   : stringLiteral
   | floatLiteral
   ;

optionHash
   : syntaxBracketLc optionHashItem (syntaxComma optionHashItem)* syntaxBracketRc
   ;

optionHashItem
   : optionHashKey COLON optionHashValue
   ;

optionHashKey
   : stringLiteral
   ;

optionHashValue
   : stringLiteral
   | floatLiteral
   ;

columnDefinitionList
   : (columnDefinition) (syntaxComma columnDefinition)* (syntaxComma primaryKeyElement)?
   ;

//
columnDefinition
   : column dataType primaryKeyColumn?
   ;

//
primaryKeyColumn
   : kwPrimary kwKey
   ;

primaryKeyElement
   : kwPrimary kwKey syntaxBracketLr primaryKeyDefinition syntaxBracketRr
   ;

primaryKeyDefinition
   : singlePrimaryKey
   | compoundKey
   | compositeKey
   ;

singlePrimaryKey
   : column
   ;

compoundKey
   : partitionKey (syntaxComma clusteringKeyList)
   ;

compositeKey
   : syntaxBracketLr partitionKeyList syntaxBracketRr (syntaxComma clusteringKeyList)
   ;

partitionKeyList
   : (partitionKey) (syntaxComma partitionKey)*
   ;

clusteringKeyList
   : (clusteringKey) (syntaxComma clusteringKey)*
   ;

partitionKey
   : column
   ;

clusteringKey
   : column
   ;

applyBatch
   : kwApply kwBatch
   ;

beginBatch
   : kwBegin batchType? kwBatch usingTimestampSpec?
   ;

batchType
   : kwLogged
   | kwUnlogged
   ;

alterKeyspace
   : kwAlter kwKeyspace keyspace kwWith kwReplication OPERATOR_EQ syntaxBracketLc replicationList syntaxBracketRc (kwAnd durableWrites)?
   ;

replicationList
   : (replicationListItem) (syntaxComma replicationListItem)*
   ;

replicationListItem
   : STRING_LITERAL COLON STRING_LITERAL
   | STRING_LITERAL COLON DECIMAL_LITERAL
   ;

durableWrites
   : kwDurableWrites OPERATOR_EQ booleanLiteral
   ;

use_
   : kwUse keyspace
   ;

truncate
   : kwTruncate (kwTable)? (keyspace DOT)? table
   ;

createIndex
   : kwCreate kwIndex ifNotExist? indexName? kwOn (keyspace DOT)? table syntaxBracketLr indexColumnSpec syntaxBracketRr
   ;

indexName
   : OBJECT_NAME
   | stringLiteral
   ;

indexColumnSpec
   : column
   | indexKeysSpec
   | indexEntriesSSpec
   | indexFullSpec
   ;

indexKeysSpec
   : kwKeys syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

indexEntriesSSpec
   : kwEntries syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

indexFullSpec
   : kwFull syntaxBracketLr OBJECT_NAME syntaxBracketRr
   ;

delete_
   : beginBatch? kwDelete deleteColumnList? fromSpec usingTimestampSpec? whereSpec (ifExist | ifSpec)?
   ;

deleteColumnList
   : (deleteColumnItem) (syntaxComma deleteColumnItem)*
   ;

deleteColumnItem
   : OBJECT_NAME
   | OBJECT_NAME LS_BRACKET (stringLiteral | decimalLiteral) RS_BRACKET
   ;

update
   : beginBatch? kwUpdate (keyspace DOT)? table usingTtlTimestamp? kwSet assignments whereSpec (ifExist | ifSpec)?
   ;

ifSpec
   : kwIf ifConditionList
   ;

ifConditionList
   : (ifCondition) (kwAnd ifCondition)*
   ;

ifCondition
   : OBJECT_NAME OPERATOR_EQ constant
   ;

assignments
   : (assignmentElement) (syntaxComma assignmentElement)*
   ;

assignmentElement
   : OBJECT_NAME OPERATOR_EQ (constant | assignmentMap | assignmentSet | assignmentList)
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) decimalLiteral
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentSet
   | OBJECT_NAME OPERATOR_EQ assignmentSet (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentMap
   | OBJECT_NAME OPERATOR_EQ assignmentMap (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME OPERATOR_EQ OBJECT_NAME (PLUS | MINUS) assignmentList
   | OBJECT_NAME OPERATOR_EQ assignmentList (PLUS | MINUS) OBJECT_NAME
   | OBJECT_NAME syntaxBracketLs decimalLiteral syntaxBracketRs OPERATOR_EQ constant
   ;

assignmentSet
   : syntaxBracketLc (constant (syntaxComma constant)*)?  syntaxBracketRc
   ;

assignmentMap
   : syntaxBracketLc (constant syntaxColon constant) (syntaxComma constant syntaxColon constant)* syntaxBracketRc
   ;

assignmentList
   : syntaxBracketLs constant (syntaxComma constant)* syntaxBracketRs
   ;

assignmentTuple
   : syntaxBracketLr ( expression (syntaxComma expression)* ) syntaxBracketRr
   ;

insert
   : beginBatch? kwInsert kwInto (keyspace DOT)? table insertColumnSpec? insertValuesSpec ifNotExist? usingTtlTimestamp?
   ;

usingTtlTimestamp
   : kwUsing ttl
   | kwUsing ttl kwAnd timestamp
   | kwUsing timestamp
   | kwUsing timestamp kwAnd ttl
   ;

timestamp
   : kwTimestamp decimalLiteral
   ;

ttl
   : kwTtl decimalLiteral
   ;

usingTimestampSpec
   : kwUsing timestamp
   ;

ifNotExist
   : kwIf kwNot kwExists
   ;

ifExist
   : kwIf kwExists
   ;

insertValuesSpec
   : kwValues '(' expressionList ')'
   | kwJson constant
   ;

insertColumnSpec
   : '(' columnList ')'
   ;

columnList
   : column (syntaxComma column)*
   ;

expressionList
   : expression (syntaxComma expression)*
   ;

expression
   : constant
   | functionCall
   | assignmentMap
   | assignmentSet
   | assignmentList
   | assignmentTuple
   ;

select_
   : kwSelect distinctSpec? kwJson? selectElements fromSpec whereSpec? orderSpec? limitSpec? allowFilteringSpec?
   ;

allowFilteringSpec
   : kwAllow kwFiltering
   ;

limitSpec
   : kwLimit decimalLiteral
   ;

fromSpec
   : kwFrom fromSpecElement
   ;

fromSpecElement
   : OBJECT_NAME
   | OBJECT_NAME '.' OBJECT_NAME
   ;

orderSpec
   : kwOrder kwBy orderSpecElement
   ;

orderSpecElement
   : OBJECT_NAME (kwAsc | kwDesc)?
   ;

whereSpec
   : kwWhere relationElements
   ;

distinctSpec
   : kwDistinct
   ;

selectElements
   : (star = '*' | selectElement) (syntaxComma selectElement)*
   ;

selectElement
   : OBJECT_NAME '.' '*'
   | OBJECT_NAME (kwAs OBJECT_NAME)?
   | functionCall (kwAs OBJECT_NAME)?
   ;

relationElements
   : (relationElement) (kwAnd relationElement)*
   ;

relationElement
   : OBJECT_NAME (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | OBJECT_NAME '.' OBJECT_NAME (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | functionCall (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) constant
   | functionCall (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) functionCall
   | OBJECT_NAME kwIn '(' functionArgs? ')'
   | '(' OBJECT_NAME (syntaxComma OBJECT_NAME)* ')' kwIn '(' assignmentTuple (syntaxComma assignmentTuple)* ')'
   | '(' OBJECT_NAME (syntaxComma OBJECT_NAME)* ')' (OPERATOR_EQ | OPERATOR_LT | OPERATOR_GT | OPERATOR_LTE | OPERATOR_GTE) ( assignmentTuple (syntaxComma assignmentTuple)* )
   | relalationContainsKey
   | relalationContains
   ;

relalationContains
   : OBJECT_NAME kwContains constant
   ;

relalationContainsKey
   : OBJECT_NAME (kwContains kwKey) constant
   ;

functionCall
   : OBJECT_NAME '(' STAR ')'
   | OBJECT_NAME '(' functionArgs? ')'
   | K_UUID '(' ')'
   ;

functionArgs
   : (constant | OBJECT_NAME | functionCall) (syntaxComma (constant | OBJECT_NAME | functionCall))*
   ;

constant
   : UUID
   | stringLiteral
   | decimalLiteral
   | floatLiteral
   | hexadecimalLiteral
   | booleanLiteral
   | codeBlock
   | kwNull
   ;

decimalLiteral
   : DECIMAL_LITERAL
   ;

floatLiteral
   : DECIMAL_LITERAL
   | FLOAT_LITERAL
   ;

stringLiteral
   : STRING_LITERAL
   ;

booleanLiteral
   : K_TRUE
   | K_FALSE
   ;

hexadecimalLiteral
   : HEXADECIMAL_LITERAL
   ;

keyspace
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   ;

table
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   ;

column
   : OBJECT_NAME
   | DQUOTE OBJECT_NAME DQUOTE
   ;

dataType
   : dataTypeName dataTypeDefinition?
   ;

dataTypeName
   : OBJECT_NAME
   | K_TIMESTAMP
   | K_SET
   | K_ASCII
   | K_BIGINT
   | K_BLOB
   | K_BOOLEAN
   | K_COUNTER
   | K_DATE
   | K_DECIMAL
   | K_DOUBLE
   | K_FLOAT
   | K_FROZEN
   | K_INET
   | K_INT
   | K_LIST
   | K_MAP
   | K_SMALLINT
   | K_TEXT
   | K_TIME
   | K_TIMEUUID
   | K_TINYINT
   | K_TUPLE
   | K_VARCHAR
   | K_VARINT
   | K_TIMESTAMP
   | K_UUID
   ;

dataTypeDefinition
   : syntaxBracketLa dataTypeName (syntaxComma dataTypeName)* syntaxBracketRa
   ;

orderDirection
   : kwAsc
   | kwDesc
   ;

role
   : OBJECT_NAME
   ;

trigger
   : OBJECT_NAME
   ;

triggerClass
   : stringLiteral
   ;

materializedView
   : OBJECT_NAME
   ;

type_
   : OBJECT_NAME
   ;

aggregate
   : OBJECT_NAME
   ;

function_
   : OBJECT_NAME
   ;

language
   : OBJECT_NAME
   ;

user
   : OBJECT_NAME
   ;

password
   : stringLiteral
   ;

hashKey
   : OBJECT_NAME
   ;

param
   : paramName dataType
   ;

paramName
   : OBJECT_NAME
   | K_INPUT
   ;

kwAdd
   : K_ADD
   ;

kwAggregate
   : K_AGGREGATE
   ;

kwAll
   : K_ALL
   ;

kwAllPermissions
   : K_ALL K_PERMISSIONS
   ;

kwAllow
   : K_ALLOW
   ;

kwAlter
   : K_ALTER
   ;

kwAnd
   : K_AND
   ;

kwApply
   : K_APPLY
   ;

kwAs
   : K_AS
   ;

kwAsc
   : K_ASC
   ;

kwAuthorize
   : K_AUTHORIZE
   ;

kwBatch
   : K_BATCH
   ;

kwBegin
   : K_BEGIN
   ;

kwBy
   : K_BY
   ;

kwCalled
   : K_CALLED
   ;

kwClustering
   : K_CLUSTERING
   ;

kwCompact
   : K_COMPACT
   ;

kwContains
   : K_CONTAINS
   ;

kwCreate
   : K_CREATE
   ;

kwDelete
   : K_DELETE
   ;

kwDesc
   : K_DESC
   ;

kwDescibe
   : K_DESCRIBE
   ;

kwDistinct
   : K_DISTINCT
   ;

kwDrop
   : K_DROP
   ;

kwDurableWrites
   : K_DURABLE_WRITES
   ;

kwEntries
   : K_ENTRIES
   ;

kwExecute
   : K_EXECUTE
   ;

kwExists
   : K_EXISTS
   ;

kwFiltering
   : K_FILTERING
   ;

kwFinalfunc
   : K_FINALFUNC
   ;

kwFrom
   : K_FROM
   ;

kwFull
   : K_FULL
   ;

kwFunction
   : K_FUNCTION
   ;

kwFunctions
   : K_FUNCTIONS
   ;

kwGrant
   : K_GRANT
   ;

kwIf
   : K_IF
   ;

kwIn
   : K_IN
   ;

kwIndex
   : K_INDEX
   ;

kwInitcond
   : K_INITCOND
   ;

kwInput
   : K_INPUT
   ;

kwInsert
   : K_INSERT
   ;

kwInto
   : K_INTO
   ;

kwIs
   : K_IS
   ;

kwJson
   : K_JSON
   ;

kwKey
   : K_KEY
   ;

kwKeys
   : K_KEYS
   ;

kwKeyspace
   : K_KEYSPACE
   ;

kwKeyspaces
   : K_KEYSPACES
   ;

kwLanguage
   : K_LANGUAGE
   ;

kwLimit
   : K_LIMIT
   ;

kwList
   : K_LIST
   ;

kwLogged
   : K_LOGGED
   ;

kwLogin
   : K_LOGIN
   ;

kwMaterialized
   : K_MATERIALIZED
   ;

kwModify
   : K_MODIFY
   ;

kwNosuperuser
   : K_NOSUPERUSER
   ;

kwNorecursive
   : K_NORECURSIVE
   ;

kwNot
   : K_NOT
   ;

kwNull
   : K_NULL
   ;

kwOf
   : K_OF
   ;

kwOn
   : K_ON
   ;

kwOptions
   : K_OPTIONS
   ;

kwOr
   : K_OR
   ;

kwOrder
   : K_ORDER
   ;

kwPassword
   : K_PASSWORD
   ;

kwPrimary
   : K_PRIMARY
   ;

kwRename
   : K_RENAME
   ;

kwReplace
   : K_REPLACE
   ;

kwReplication
   : K_REPLICATION
   ;

kwReturns
   : K_RETURNS
   ;

kwRole
   : K_ROLE
   ;

kwRoles
   : K_ROLES
   ;

kwSelect
   : K_SELECT
   ;

kwSet
   : K_SET
   ;

kwSfunc
   : K_SFUNC
   ;

kwStorage
   : K_STORAGE
   ;

kwStype
   : K_STYPE
   ;

kwSuperuser
   : K_SUPERUSER
   ;

kwTable
   : K_TABLE
   ;

kwTimestamp
   : K_TIMESTAMP
   ;

kwTo
   : K_TO
   ;

kwTrigger
   : K_TRIGGER
   ;

kwTruncate
   : K_TRUNCATE
   ;

kwTtl
   : K_TTL
   ;

kwType
   : K_TYPE
   ;

kwUnlogged
   : K_UNLOGGED
   ;

kwUpdate
   : K_UPDATE
   ;

kwUse
   : K_USE
   ;

kwUser
   : K_USER
   ;

kwUsing
   : K_USING
   ;

kwValues
   : K_VALUES
   ;

kwView
   : K_VIEW
   ;

kwWhere
   : K_WHERE
   ;

kwWith
   : K_WITH
   ;

kwRevoke
   : K_REVOKE
   ;

// BRACKETS
// L - left
// R - right
// a - angle
// c - curly
// r - rounded
syntaxBracketLr
   : LR_BRACKET
   ;

syntaxBracketRr
   : RR_BRACKET
   ;

syntaxBracketLc
   : LC_BRACKET
   ;

syntaxBracketRc
   : RC_BRACKET
   ;

syntaxBracketLa
   : OPERATOR_LT
   ;

syntaxBracketRa
   : OPERATOR_GT
   ;

syntaxBracketLs
   : LS_BRACKET
   ;

syntaxBracketRs
   : RS_BRACKET
   ;

syntaxComma
   : COMMA
   ;

syntaxColon
   : COLON
   ;
