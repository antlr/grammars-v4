/*
 * Licensed under the Apache License, Version 2.0_ (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS_ IS" BASIS,
 * WITHOUT_ WARRANTIES_ OR_ CONDITIONS_ OF_ ANY_ KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

parser grammar TrinoParser;

options {
    tokenVocab = TrinoLexer;
}

// Modified entrypoint
parse
    : statements* EOF
    ;

statements
    : singleStatement
    |standaloneExpression
    |standalonePathSpecification
    |standaloneType
    |standaloneRowPattern SEMICOLON_?
    ;

singleStatement
    : statement SEMICOLON_
    ;

standaloneExpression
    : expression SEMICOLON_
    ;

standalonePathSpecification
    : pathSpecification SEMICOLON_
    ;

standaloneType
    : type SEMICOLON_
    ;

standaloneRowPattern
    : rowPattern SEMICOLON_
    ;

statement
    : query                                                            #statementDefault
    | USE_ schema=identifier                                            #use
    | USE_ catalog=identifier DOT_ schema=identifier                     #use
    | CREATE_ SCHEMA_ (IF_ NOT_ EXISTS_)? qualifiedName
        (AUTHORIZATION_ principal)?
        (WITH_ properties)?                                             #createSchema
    | DROP_ SCHEMA_ (IF_ EXISTS_)? qualifiedName (CASCADE_ | RESTRICT_)?     #dropSchema
    | ALTER_ SCHEMA_ qualifiedName RENAME_ TO_ identifier                  #renameSchema
    | ALTER_ SCHEMA_ qualifiedName SET_ AUTHORIZATION_ principal           #setSchemaAuthorization
    | CREATE_ TABLE_ (IF_ NOT_ EXISTS_)? qualifiedName columnAliases?
        (COMMENT_ string_)?
        (WITH_ properties)? AS_ (query | LPAREN_ query RPAREN_)
        (WITH_ (NO_)? DATA_)?                                             #createTableAsSelect
    | CREATE_ TABLE_ (IF_ NOT_ EXISTS_)? qualifiedName
        LPAREN_ tableElement (COMMA_ tableElement)* RPAREN_
         (COMMENT_ string_)?
         (WITH_ properties)?                                            #createTable
    | DROP_ TABLE_ (IF_ EXISTS_)? qualifiedName                            #dropTable
    | INSERT_ INTO_ qualifiedName columnAliases? query                   #insertInto
    | DELETE_ FROM_ qualifiedName (WHERE_ booleanExpression)?             #delete
    | TRUNCATE_ TABLE_ qualifiedName                                     #truncateTable
    | COMMENT_ ON_ TABLE_ qualifiedName IS_ (string_ | NULL_)                #commentTable
    | COMMENT_ ON_ VIEW_ qualifiedName IS_ (string_ | NULL_)                 #commentView
    | COMMENT_ ON_ COLUMN_ qualifiedName IS_ (string_ | NULL_)               #commentColumn
    | ALTER_ TABLE_ (IF_ EXISTS_)? from=qualifiedName
        RENAME_ TO_ to=qualifiedName                                     #renameTable
    | ALTER_ TABLE_ (IF_ EXISTS_)? tableName=qualifiedName
        ADD_ COLUMN_ (IF_ NOT_ EXISTS_)? column=columnDefinition            #addColumn
    | ALTER_ TABLE_ (IF_ EXISTS_)? tableName=qualifiedName
        RENAME_ COLUMN_ (IF_ EXISTS_)? from=identifier TO_ to=identifier    #renameColumn
    | ALTER_ TABLE_ (IF_ EXISTS_)? tableName=qualifiedName
        DROP_ COLUMN_ (IF_ EXISTS_)? column=qualifiedName                   #dropColumn
    | ALTER_ TABLE_ (IF_ EXISTS_)? tableName=qualifiedName
        ALTER_ COLUMN_ columnName=identifier SET_ DATA_ TYPE_ type          #setColumnType
    | ALTER_ TABLE_ tableName=qualifiedName SET_ AUTHORIZATION_ principal   #setTableAuthorization
    | ALTER_ TABLE_ tableName=qualifiedName
        SET_ PROPERTIES_ propertyAssignments                             #setTableProperties
    | ALTER_ TABLE_ tableName=qualifiedName
        EXECUTE_ procedureName=identifier
        (LPAREN_ (callArgument (COMMA_ callArgument)*)? RPAREN_)?
        (WHERE_ where=booleanExpression)?                               #tableExecute
    | ANALYZE_ qualifiedName (WITH_ properties)?                         #analyze
    | CREATE_ (OR_ REPLACE_)? MATERIALIZED_ VIEW_
        (IF_ NOT_ EXISTS_)? qualifiedName
        (GRACE_ PERIOD_ interval)?
        (COMMENT_ string_)?
        (WITH_ properties)? AS_ query                                    #createMaterializedView
    | CREATE_ (OR_ REPLACE_)? VIEW_ qualifiedName
        (COMMENT_ string_)?
        (SECURITY_ (DEFINER_ | INVOKER_))? AS_ query                       #createView
    | REFRESH_ MATERIALIZED_ VIEW_ qualifiedName                          #refreshMaterializedView
    | DROP_ MATERIALIZED_ VIEW_ (IF_ EXISTS_)? qualifiedName                #dropMaterializedView
    | ALTER_ MATERIALIZED_ VIEW_ (IF_ EXISTS_)? from=qualifiedName
        RENAME_ TO_ to=qualifiedName                                     #renameMaterializedView
    | ALTER_ MATERIALIZED_ VIEW_ qualifiedName
        SET_ PROPERTIES_ propertyAssignments                             #setMaterializedViewProperties
    | DROP_ VIEW_ (IF_ EXISTS_)? qualifiedName                             #dropView
    | ALTER_ VIEW_ from=qualifiedName RENAME_ TO_ to=qualifiedName         #renameView
    | ALTER_ VIEW_ from=qualifiedName SET_ AUTHORIZATION_ principal        #setViewAuthorization
    | CALL_ qualifiedName LPAREN_ (callArgument (COMMA_ callArgument)*)? RPAREN_   #call
    | CREATE_ ROLE_ name=identifier
        (WITH_ ADMIN_ grantor)?
        (IN_ catalog=identifier)?                                       #createRole
    | DROP_ ROLE_ name=identifier (IN_ catalog=identifier)?               #dropRole
    | GRANT_
        roles
        TO_ principal (COMMA_ principal)*
        (WITH_ ADMIN_ OPTION_)?
        (GRANTED_ BY_ grantor)?
        (IN_ catalog=identifier)?                                       #grantRoles
    | REVOKE_
        (ADMIN_ OPTION_ FOR_)?
        roles
        FROM_ principal (COMMA_ principal)*
        (GRANTED_ BY_ grantor)?
        (IN_ catalog=identifier)?                                       #revokeRoles
    | SET_ ROLE_ (ALL_ | NONE_ | role=identifier)
        (IN_ catalog=identifier)?                                       #setRole
    | GRANT_
        (privilege (COMMA_ privilege)* | ALL_ PRIVILEGES_)
        ON_ (SCHEMA_ | TABLE_)? qualifiedName
        TO_ grantee=principal
        (WITH_ GRANT_ OPTION_)?                                           #grant
    | DENY_
        (privilege (COMMA_ privilege)* | ALL_ PRIVILEGES_)
        ON_ (SCHEMA_ | TABLE_)? qualifiedName
        TO_ grantee=principal                                           #deny
    | REVOKE_
        (GRANT_ OPTION_ FOR_)?
        (privilege (COMMA_ privilege)* | ALL_ PRIVILEGES_)
        ON_ (SCHEMA_ | TABLE_)? qualifiedName
        FROM_ grantee=principal                                         #revoke
    | SHOW_ GRANTS_ (ON_ TABLE_? qualifiedName)?                           #showGrants
    | EXPLAIN_ (LPAREN_ explainOption (COMMA_ explainOption)* RPAREN_)? statement  #explain
    | EXPLAIN_ ANALYZE_ VERBOSE_? statement                               #explainAnalyze
    | SHOW_ CREATE_ TABLE_ qualifiedName                                  #showCreateTable
    | SHOW_ CREATE_ SCHEMA_ qualifiedName                                 #showCreateSchema
    | SHOW_ CREATE_ VIEW_ qualifiedName                                   #showCreateView
    | SHOW_ CREATE_ MATERIALIZED_ VIEW_ qualifiedName                      #showCreateMaterializedView
    | SHOW_ TABLES_ ((FROM_ | IN_) qualifiedName)?
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showTables
    | SHOW_ SCHEMAS_ ((FROM_ | IN_) identifier)?
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showSchemas
    | SHOW_ CATALOGS_
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showCatalogs
    | SHOW_ COLUMNS_ (FROM_ | IN_) qualifiedName?
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showColumns
    | SHOW_ STATS_ FOR_ qualifiedName                                     #showStats
    | SHOW_ STATS_ FOR_ LPAREN_ query RPAREN_                                     #showStatsForQuery
    | SHOW_ CURRENT_? ROLES_ ((FROM_ | IN_) identifier)?                    #showRoles
    | SHOW_ ROLE_ GRANTS_ ((FROM_ | IN_) identifier)?                       #showRoleGrants
    | DESCRIBE_ qualifiedName                                           #showColumns
    | DESC_ qualifiedName                                               #showColumns
    | SHOW_ FUNCTIONS_
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showFunctions
    | SHOW_ SESSION_
        (LIKE_ pattern=string_ (ESCAPE_ escape=string_)?)?                 #showSession
    | SET_ SESSION_ qualifiedName EQ_ expression                          #setSession
    | RESET_ SESSION_ qualifiedName                                      #resetSession
    | START_ TRANSACTION_ (transactionMode (COMMA_ transactionMode)*)?      #startTransaction
    | COMMIT_ WORK_?                                                     #commit
    | ROLLBACK_ WORK_?                                                   #rollback
    | PREPARE_ identifier FROM_ statement                                #prepare
    | DEALLOCATE_ PREPARE_ identifier                                    #deallocate
    | EXECUTE_ identifier (USING_ expression (COMMA_ expression)*)?         #execute
    | DESCRIBE_ INPUT_ identifier                                        #describeInput
    | DESCRIBE_ OUTPUT_ identifier                                       #describeOutput
    | SET_ PATH_ pathSpecification                                       #setPath
    | SET_ TIME_ ZONE_ (LOCAL_ | expression)                               #setTimeZone
    | UPDATE_ qualifiedName
        SET_ updateAssignment (COMMA_ updateAssignment)*
        (WHERE_ where=booleanExpression)?                               #update
    | MERGE_ INTO_ qualifiedName (AS_? identifier)?
        USING_ relation ON_ expression mergeCase+                        #merge
    ;

query
    :  with? queryNoWith
    ;

with
    : WITH_ RECURSIVE_? namedQuery (COMMA_ namedQuery)*
    ;

tableElement
    : columnDefinition
    | likeClause
    ;

columnDefinition
    : identifier type (NOT_ NULL_)? (COMMENT_ string_)? (WITH_ properties)?
    ;

likeClause
    : LIKE_ qualifiedName (optionType=(INCLUDING_ | EXCLUDING_) PROPERTIES_)?
    ;

properties
    : LPAREN_ propertyAssignments RPAREN_
    ;

propertyAssignments
    : property (COMMA_ property)*
    ;

property
    : identifier EQ_ propertyValue
    ;

propertyValue
    : DEFAULT_       #defaultPropertyValue
    | expression    #nonDefaultPropertyValue
    ;

queryNoWith
    : queryTerm
      (ORDER_ BY_ sortItem (COMMA_ sortItem)*)?
      (OFFSET_ offset=rowCount (ROW_ | ROWS_)?)?
      ( LIMIT_ limit=limitRowCount
      | FETCH_ (FIRST_ | NEXT_) (fetchFirst=rowCount)? (ROW_ | ROWS_) (ONLY_ | WITH_ TIES_)
      )?
    ;

limitRowCount
    : ALL_
    | rowCount
    ;

rowCount
    : INTEGER_VALUE_
    | QUESTION_MARK_
    ;

queryTerm
    : queryPrimary                                                             #queryTermDefault
    | left=queryTerm operator=INTERSECT_ setQuantifier? right=queryTerm         #setOperation
    | left=queryTerm operator=(UNION_ | EXCEPT_) setQuantifier? right=queryTerm  #setOperation
    ;

queryPrimary
    : querySpecification                   #queryPrimaryDefault
    | TABLE_ qualifiedName                  #table
    | VALUES_ expression (COMMA_ expression)*  #inlineTable
    | LPAREN_ queryNoWith RPAREN_                  #subquery
    ;

sortItem
    : expression ordering=(ASC_ | DESC_)? (NULLS_ nullOrdering=(FIRST_ | LAST_))?
    ;

querySpecification
    : SELECT_ setQuantifier? selectItem (COMMA_ selectItem)*
      (FROM_ relation (COMMA_ relation)*)?
      (WHERE_ where=booleanExpression)?
      (GROUP_ BY_ groupBy)?
      (HAVING_ having=booleanExpression)?
      (WINDOW_ windowDefinition (COMMA_ windowDefinition)*)?
    ;

groupBy
    : setQuantifier? groupingElement (COMMA_ groupingElement)*
    ;

groupingElement
    : groupingSet                                            #singleGroupingSet
    | ROLLUP_ LPAREN_ (expression (COMMA_ expression)*)? RPAREN_         #rollup
    | CUBE_ LPAREN_ (expression (COMMA_ expression)*)? RPAREN_           #cube
    | GROUPING_ SETS_ LPAREN_ groupingSet (COMMA_ groupingSet)* RPAREN_   #multipleGroupingSets
    ;

groupingSet
    : LPAREN_ (expression (COMMA_ expression)*)? RPAREN_
    | expression
    ;

windowDefinition
    : name=identifier AS_ LPAREN_ windowSpecification RPAREN_
    ;

windowSpecification
    : (existingWindowName=identifier)?
      (PARTITION_ BY_ partition+=expression (COMMA_ partition+=expression)*)?
      (ORDER_ BY_ sortItem (COMMA_ sortItem)*)?
      windowFrame?
    ;

namedQuery
    : name=identifier (columnAliases)? AS_ LPAREN_ query RPAREN_
    ;

setQuantifier
    : DISTINCT_
    | ALL_
    ;

selectItem
    : expression (AS_? identifier)?                          #selectSingle
    | primaryExpression DOT_ ASTERISK_ (AS_ columnAliases)?    #selectAll
    | ASTERISK_                                              #selectAll
    ;

relation
    : left=relation
      ( CROSS_ JOIN_ right=sampledRelation
      | joinType JOIN_ rightRelation=relation joinCriteria
      | NATURAL_ joinType JOIN_ right=sampledRelation
      )                                                     #joinRelation
    | sampledRelation                                       #relationDefault
    ;

joinType
    : INNER_?
    | (LEFT_ | RIGHT_ | FULL_) OUTER_?
    ;

joinCriteria
    : ON_ booleanExpression
    | USING_ LPAREN_ identifier (COMMA_ identifier)* RPAREN_
    ;

sampledRelation
    : patternRecognition (
        TABLESAMPLE_ sampleType LPAREN_ percentage=expression RPAREN_
      )?
    ;

sampleType
    : BERNOULLI_
    | SYSTEM_
    ;

trimsSpecification
    : LEADING_
    | TRAILING_
    | BOTH_
    ;

listAggOverflowBehavior
    : ERROR_
    | TRUNCATE_ string_? listaggCountIndication
    ;

listaggCountIndication
    : (WITH_ | WITHOUT_) COUNT_
    ;

patternRecognition
    : aliasedRelation (
        MATCH_RECOGNIZE_ LPAREN_
          (PARTITION_ BY_ partition+=expression (COMMA_ partition+=expression)*)?
          (ORDER_ BY_ sortItem (COMMA_ sortItem)*)?
          (MEASURES_ measureDefinition (COMMA_ measureDefinition)*)?
          rowsPerMatch?
          (AFTER_ MATCH_ skipTo)?
          (INITIAL_ | SEEK_)?
          PATTERN_ LPAREN_ rowPattern RPAREN_
          (SUBSET_ subsetDefinition (COMMA_ subsetDefinition)*)?
          DEFINE_ variableDefinition (COMMA_ variableDefinition)*
        RPAREN_
        (AS_? identifier columnAliases?)?
      )?
    ;

measureDefinition
    : expression AS_ identifier
    ;

rowsPerMatch
    : ONE_ ROW_ PER_ MATCH_
    | ALL_ ROWS_ PER_ MATCH_ emptyMatchHandling?
    ;

emptyMatchHandling
    : SHOW_ EMPTY_ MATCHES_
    | OMIT_ EMPTY_ MATCHES_
    | WITH_ UNMATCHED_ ROWS_
    ;

skipTo
    : SKIP_
           ( TO_ (NEXT_ ROW_ | (FIRST_ | LAST_)? identifier)
           | PAST_ LAST_ ROW_
           )
    ;

subsetDefinition
    : name=identifier EQ_ LPAREN_ union+=identifier (COMMA_ union+=identifier)* RPAREN_
    ;

variableDefinition
    : identifier AS_ expression
    ;

aliasedRelation
    : relationPrimary (AS_? identifier columnAliases?)?
    ;

columnAliases
    : LPAREN_ identifier (COMMA_ identifier)* RPAREN_
    ;

relationPrimary
    : qualifiedName queryPeriod?                                      #tableName
    | LPAREN_ query RPAREN_                                                   #subqueryRelation
    | UNNEST_ LPAREN_ expression (COMMA_ expression)* RPAREN_ (WITH_ ORDINALITY_)?  #unnest
    | LATERAL_ LPAREN_ query RPAREN_                                           #lateral
    | TABLE_ LPAREN_ tableFunctionCall RPAREN_                                 #tableFunctionInvocation
    | LPAREN_ relation RPAREN_                                                #parenthesizedRelation
    ;

tableFunctionCall
    : qualifiedName LPAREN_ (tableFunctionArgument (COMMA_ tableFunctionArgument)*)?
      (COPARTITION_ copartitionTables (COMMA_ copartitionTables)*)? RPAREN_
    ;

tableFunctionArgument
    : (identifier RDOUBLEARROW_)? (tableArgument | descriptorArgument | expression) // descriptor before expression to avoid parsing descriptor as a function call
    ;

tableArgument
    : tableArgumentRelation
        (PARTITION_ BY_ (LPAREN_ (expression (COMMA_ expression)*)? RPAREN_ | expression))?
        (PRUNE_ WHEN_ EMPTY_ | KEEP_ WHEN_ EMPTY_)?
        (ORDER_ BY_ (LPAREN_ sortItem (COMMA_ sortItem)* RPAREN_ | sortItem))?
    ;

tableArgumentRelation
    : TABLE_ LPAREN_ qualifiedName RPAREN_ (AS_? identifier columnAliases?)?  #tableArgumentTable
    | TABLE_ LPAREN_ query RPAREN_ (AS_? identifier columnAliases?)?          #tableArgumentQuery
    ;

descriptorArgument
    : DESCRIPTOR_ LPAREN_ descriptorField (COMMA_ descriptorField)* RPAREN_
    | CAST_ LPAREN_ NULL_ AS_ DESCRIPTOR_ RPAREN_
    ;

descriptorField
    : identifier type?
    ;

copartitionTables
    : LPAREN_ qualifiedName COMMA_ qualifiedName (COMMA_ qualifiedName)* RPAREN_
    ;

expression
    : booleanExpression
    ;

booleanExpression
    : valueExpression predicate_?  #predicated
    | NOT_ booleanExpression                             #logicalNot
    | booleanExpression AND_ booleanExpression           #and
    | booleanExpression OR_ booleanExpression            #or
    ;

// workaround for https://github.com/antlr/antlr4/issues/780
predicate_
    : comparisonOperator right=valueExpression                            #comparison
    | comparisonOperator comparisonQuantifier LPAREN_ query RPAREN_               #quantifiedComparison
    | NOT_? BETWEEN_ lower=valueExpression AND_ upper=valueExpression        #between
    | NOT_? IN_ LPAREN_ expression (COMMA_ expression)* RPAREN_                        #inList
    | NOT_? IN_ LPAREN_ query RPAREN_                                               #inSubquery
    | NOT_? LIKE_ pattern=valueExpression (ESCAPE_ escape=valueExpression)?  #like
    | IS_ NOT_? NULL_                                                        #nullPredicate
    | IS_ NOT_? DISTINCT_ FROM_ right=valueExpression                         #distinctFrom
    ;

valueExpression
    : primaryExpression                                                                 #valueExpressionDefault
    | valueExpression AT_ timeZoneSpecifier                                              #atTimeZone
    | operator=(MINUS_ | PLUS_) valueExpression                                           #arithmeticUnary
    | left=valueExpression operator=(ASTERISK_ | SLASH_ | PERCENT_) right=valueExpression  #arithmeticBinary
    | left=valueExpression operator=(PLUS_ | MINUS_) right=valueExpression                #arithmeticBinary
    | left=valueExpression CONCAT_ right=valueExpression                                 #concatenation
    ;

primaryExpression
    : NULL_                                                                                #nullLiteral
    | interval                                                                            #intervalLiteral
    | identifier string_                                                                   #typeConstructor
    | DOUBLE_ PRECISION_ string_                                                             #typeConstructor
    | number                                                                              #numericLiteral
    | booleanValue                                                                        #booleanLiteral
    | string_                                                                              #stringLiteral
    | BINARY_LITERAL_                                                                      #binaryLiteral
    | QUESTION_MARK_                                                                       #parameter
    | POSITION_ LPAREN_ valueExpression IN_ valueExpression RPAREN_                                 #position
    | LPAREN_ expression (COMMA_ expression)+ RPAREN_                                                #rowConstructor
    | ROW_ LPAREN_ expression (COMMA_ expression)* RPAREN_                                            #rowConstructor
    | name=LISTAGG_ LPAREN_ setQuantifier? expression (COMMA_ string_)?
        (ON_ OVERFLOW_ listAggOverflowBehavior)? RPAREN_
        (WITHIN_ GROUP_ LPAREN_ ORDER_ BY_ sortItem (COMMA_ sortItem)* RPAREN_)                          #listagg
    | processingMode? qualifiedName LPAREN_ (label=identifier DOT_)? ASTERISK_ RPAREN_
        filter? over?                                                                     #functionCall
    | processingMode? qualifiedName LPAREN_ (setQuantifier? expression (COMMA_ expression)*)?
        (ORDER_ BY_ sortItem (COMMA_ sortItem)*)? RPAREN_ filter? (nullTreatment? over)?           #functionCall
    | identifier over                                                                     #measure
    | identifier RARROW_ expression                                                          #lambda
    | LPAREN_ (identifier (COMMA_ identifier)*)? RPAREN_ RARROW_ expression                             #lambda
    | LPAREN_ query RPAREN_                                                                       #subqueryExpression
    // This is an extension to ANSI_ SQL, which considers EXISTS_ to be a <boolean expression>
    | EXISTS_ LPAREN_ query RPAREN_                                                                #exists
    | CASE_ operand=expression whenClause+ (ELSE_ elseExpression=expression)? END_           #simpleCase
    | CASE_ whenClause+ (ELSE_ elseExpression=expression)? END_                              #searchedCase
    | CAST_ LPAREN_ expression AS_ type RPAREN_                                                     #cast
    | TRY_CAST_ LPAREN_ expression AS_ type RPAREN_                                                 #cast
    | ARRAY_ LSQUARE_ (expression (COMMA_ expression)*)? RSQUARE_                                       #arrayConstructor
    | value=primaryExpression LSQUARE_ index=valueExpression RSQUARE_                               #subscript
    | identifier                                                                          #columnReference
    | base_=primaryExpression DOT_ fieldName=identifier                                     #dereference
    | name=CURRENT_DATE_                                                                   #specialDateTimeFunction
    | name=CURRENT_TIME_ (LPAREN_ precision=INTEGER_VALUE_ RPAREN_)?                                #specialDateTimeFunction
    | name=CURRENT_TIMESTAMP_ (LPAREN_ precision=INTEGER_VALUE_ RPAREN_)?                           #specialDateTimeFunction
    | name=LOCALTIME_ (LPAREN_ precision=INTEGER_VALUE_ RPAREN_)?                                   #specialDateTimeFunction
    | name=LOCALTIMESTAMP_ (LPAREN_ precision=INTEGER_VALUE_ RPAREN_)?                              #specialDateTimeFunction
    | name=CURRENT_USER_                                                                   #currentUser
    | name=CURRENT_CATALOG_                                                                #currentCatalog
    | name=CURRENT_SCHEMA_                                                                 #currentSchema
    | name=CURRENT_PATH_                                                                   #currentPath
    | TRIM_ LPAREN_ (trimsSpecification? trimChar=valueExpression? FROM_)?
        trimSource=valueExpression RPAREN_                                                    #trim
    | TRIM_ LPAREN_ trimSource=valueExpression COMMA_ trimChar=valueExpression RPAREN_                #trim
    | SUBSTRING_ LPAREN_ valueExpression FROM_ valueExpression (FOR_ valueExpression)? RPAREN_       #substring
    | NORMALIZE_ LPAREN_ valueExpression (COMMA_ normalForm)? RPAREN_                                 #normalize
    | EXTRACT_ LPAREN_ identifier FROM_ valueExpression RPAREN_                                     #extract
    | LPAREN_ expression RPAREN_                                                                  #parenthesizedExpression
    | GROUPING_ LPAREN_ (qualifiedName (COMMA_ qualifiedName)*)? RPAREN_                              #groupingOperation
    | JSON_EXISTS_ LPAREN_ jsonPathInvocation (jsonExistsErrorBehavior ON_ ERROR_)? RPAREN_          #jsonExists
    | JSON_VALUE_ LPAREN_
        jsonPathInvocation
        (RETURNING_ type)?
        (emptyBehavior=jsonValueBehavior ON_ EMPTY_)?
        (errorBehavior=jsonValueBehavior ON_ ERROR_)?
      RPAREN_                                                                                 #jsonValue
    | JSON_QUERY_ LPAREN_
        jsonPathInvocation
        (RETURNING_ type (FORMAT_ jsonRepresentation)?)?
        (jsonQueryWrapperBehavior WRAPPER_)?
        ((KEEP_ | OMIT_) QUOTES_ (ON_ SCALAR_ TEXT_STRING_)?)?
        (emptyBehavior=jsonQueryBehavior ON_ EMPTY_)?
        (errorBehavior=jsonQueryBehavior ON_ ERROR_)?
      RPAREN_                                                                                 #jsonQuery
    | JSON_OBJECT_ LPAREN_
        (
          jsonObjectMember (COMMA_ jsonObjectMember)*
          (NULL_ ON_ NULL_ | ABSENT_ ON_ NULL_)?
          (WITH_ UNIQUE_ KEYS_? | WITHOUT_ UNIQUE_ KEYS_?)?
        )?
        (RETURNING_ type (FORMAT_ jsonRepresentation)?)?
      RPAREN_                                                                                 #jsonObject
    | JSON_ARRAY_ LPAREN_
        (
          jsonValueExpression (COMMA_ jsonValueExpression)*
          (NULL_ ON_ NULL_ | ABSENT_ ON_ NULL_)?
        )?
        (RETURNING_ type (FORMAT_ jsonRepresentation)?)?
     RPAREN_                                                                                  #jsonArray
    ;

jsonPathInvocation
    : jsonValueExpression COMMA_ path=string_
        (PASSING_ jsonArgument (COMMA_ jsonArgument)*)?
    ;

jsonValueExpression
    : expression (FORMAT_ jsonRepresentation)?
    ;

jsonRepresentation
    : JSON_ (ENCODING_ (UTF8_ | UTF16_ | UTF32_))? // TODO_ add implementation-defined JSON_ representation option
    ;

jsonArgument
    : jsonValueExpression AS_ identifier
    ;

jsonExistsErrorBehavior
    : TRUE_
    | FALSE_
    | UNKNOWN_
    | ERROR_
    ;

jsonValueBehavior
    : ERROR_
    | NULL_
    | DEFAULT_ expression
    ;

jsonQueryWrapperBehavior
    : WITHOUT_ ARRAY_?
    | WITH_ (CONDITIONAL_ | UNCONDITIONAL_)? ARRAY_?
    ;

jsonQueryBehavior
    : ERROR_
    | NULL_
    | EMPTY_ (ARRAY_ | OBJECT_)
    ;

jsonObjectMember
    : KEY_? expression VALUE_ jsonValueExpression
    | expression COLON_ jsonValueExpression
    ;

processingMode
    : RUNNING_
    | FINAL_
    ;

nullTreatment
    : IGNORE_ NULLS_
    | RESPECT_ NULLS_
    ;

// renamed from "string" to avoid golang name conflict
string_
    : STRING_                                #basicStringLiteral
    | UNICODE_STRING_ (UESCAPE_ STRING_)?      #unicodeStringLiteral
    ;

timeZoneSpecifier
    : TIME_ ZONE_ interval  #timeZoneInterval
    | TIME_ ZONE_ string_    #timeZoneString
    ;

comparisonOperator
    : EQ_ | NEQ_ | LT_ | LTE_ | GT_ | GTE_
    ;

comparisonQuantifier
    : ALL_ | SOME_ | ANY_
    ;

booleanValue
    : TRUE_ | FALSE_
    ;

interval
    : INTERVAL_ sign=(PLUS_ | MINUS_)? string_ from=intervalField (TO_ to=intervalField)?
    ;

intervalField
    : YEAR_ | MONTH_ | DAY_ | HOUR_ | MINUTE_ | SECOND_
    ;

normalForm
    : NFD_ | NFC_ | NFKD_ | NFKC_
    ;

type
    : ROW_ LPAREN_ rowField (COMMA_ rowField)* RPAREN_                                         #rowType
    | INTERVAL_ from=intervalField (TO_ to=intervalField)?                           #intervalType
    | base=TIMESTAMP_ (LPAREN_ precision = typeParameter RPAREN_)? (WITHOUT_ TIME_ ZONE_)?     #dateTimeType
    | base=TIMESTAMP_ (LPAREN_ precision = typeParameter RPAREN_)? WITH_ TIME_ ZONE_           #dateTimeType
    | base=TIME_ (LPAREN_ precision = typeParameter RPAREN_)? (WITHOUT_ TIME_ ZONE_)?          #dateTimeType
    | base=TIME_ (LPAREN_ precision = typeParameter RPAREN_)? WITH_ TIME_ ZONE_                #dateTimeType
    | DOUBLE_ PRECISION_                                                             #doublePrecisionType
    | ARRAY_ LT_ type GT_                                                           #legacyArrayType
    | MAP_ LT_ keyType=type COMMA_ valueType=type GT_                                  #legacyMapType
    | type ARRAY_ (LSQUARE_ INTEGER_VALUE_ RSQUARE_)?                                          #arrayType
    | identifier (LPAREN_ typeParameter (COMMA_ typeParameter)* RPAREN_)?                     #genericType
    ;

rowField
    : type
    | identifier type
    ;

typeParameter
    : INTEGER_VALUE_ | type
    ;

whenClause
    : WHEN_ condition=expression THEN_ result=expression
    ;

filter
    : FILTER_ LPAREN_ WHERE_ booleanExpression RPAREN_
    ;

mergeCase
    : WHEN_ MATCHED_ (AND_ condition=expression)? THEN_
        UPDATE_ SET_ targets+=identifier EQ_ values+=expression
          (COMMA_ targets+=identifier EQ_ values+=expression)*                  #mergeUpdate
    | WHEN_ MATCHED_ (AND_ condition=expression)? THEN_ DELETE_                  #mergeDelete
    | WHEN_ NOT_ MATCHED_ (AND_ condition=expression)? THEN_
        INSERT_ (LPAREN_ targets+=identifier (COMMA_ targets+=identifier)* RPAREN_)?
        VALUES_ LPAREN_ values+=expression (COMMA_ values+=expression)* RPAREN_         #mergeInsert
    ;

over
    : OVER_ (windowName=identifier | LPAREN_ windowSpecification RPAREN_)
    ;

windowFrame
    : (MEASURES_ measureDefinition (COMMA_ measureDefinition)*)?
      frameExtent
      (AFTER_ MATCH_ skipTo)?
      (INITIAL_ | SEEK_)?
      (PATTERN_ LPAREN_ rowPattern RPAREN_)?
      (SUBSET_ subsetDefinition (COMMA_ subsetDefinition)*)?
      (DEFINE_ variableDefinition (COMMA_ variableDefinition)*)?
    ;

// renamed start and stop to avoid Dart name conflict
frameExtent
    : frameType=RANGE_ start_=frameBound
    | frameType=ROWS_ start_=frameBound
    | frameType=GROUPS_ start_=frameBound
    | frameType=RANGE_ BETWEEN_ start_=frameBound AND_ end_=frameBound
    | frameType=ROWS_ BETWEEN_ start_=frameBound AND_ end_=frameBound
    | frameType=GROUPS_ BETWEEN_ start_=frameBound AND_ end_=frameBound
    ;

frameBound
    : UNBOUNDED_ boundType=PRECEDING_                 #unboundedFrame
    | UNBOUNDED_ boundType=FOLLOWING_                 #unboundedFrame
    | CURRENT_ ROW_                                   #currentRowBound
    | expression boundType=(PRECEDING_ | FOLLOWING_)  #boundedFrame
    ;

rowPattern
    : patternPrimary patternQuantifier?                 #quantifiedPrimary
    | rowPattern rowPattern                             #patternConcatenation
    | rowPattern VBAR_ rowPattern                         #patternAlternation
    ;

patternPrimary
    : identifier                                        #patternVariable
    | LPAREN_ RPAREN_                                           #emptyPattern
    | PERMUTE_ LPAREN_ rowPattern (COMMA_ rowPattern)* RPAREN_      #patternPermutation
    | LPAREN_ rowPattern RPAREN_                                #groupedPattern
    | CARET_                                               #partitionStartAnchor
    | DOLLAR_                                               #partitionEndAnchor
    | LCURLYHYPHEN_ rowPattern RCURLYHYPHEN_                              #excludedPattern
    ;

patternQuantifier
    : ASTERISK_ (reluctant=QUESTION_MARK_)?                                                       #zeroOrMoreQuantifier
    | PLUS_ (reluctant=QUESTION_MARK_)?                                                           #oneOrMoreQuantifier
    | QUESTION_MARK_ (reluctant=QUESTION_MARK_)?                                                  #zeroOrOneQuantifier
    | LCURLY_ exactly=INTEGER_VALUE_ RCURLY_ (reluctant=QUESTION_MARK_)?                                  #rangeQuantifier
    | LCURLY_ (atLeast=INTEGER_VALUE_)? COMMA_ (atMost=INTEGER_VALUE_)? RCURLY_ (reluctant=QUESTION_MARK_)?   #rangeQuantifier
    ;

updateAssignment
    : identifier EQ_ expression
    ;

explainOption
    : FORMAT_ value=(TEXT_ | GRAPHVIZ_ | JSON_)                 #explainFormat
    | TYPE_ value=(LOGICAL_ | DISTRIBUTED_ | VALIDATE_ | IO_)    #explainType
    ;

transactionMode
    : ISOLATION_ LEVEL_ levelOfIsolation    #isolationLevel
    | READ_ accessMode=(ONLY_ | WRITE_)      #transactionAccessMode
    ;

levelOfIsolation
    : READ_ UNCOMMITTED_                    #readUncommitted
    | READ_ COMMITTED_                      #readCommitted
    | REPEATABLE_ READ_                     #repeatableRead
    | SERIALIZABLE_                        #serializable
    ;

callArgument
    : expression                    #positionalArgument
    | identifier RDOUBLEARROW_ expression    #namedArgument
    ;

pathElement
    : identifier DOT_ identifier     #qualifiedArgument
    | identifier                    #unqualifiedArgument
    ;

pathSpecification
    : pathElement (COMMA_ pathElement)*
    ;

privilege
    : CREATE_ | SELECT_ | DELETE_ | INSERT_ | UPDATE_
    ;

qualifiedName
    : identifier (DOT_ identifier)*
    ;

queryPeriod
    : FOR_ rangeType AS_ OF_ end=valueExpression
    ;

rangeType
    : TIMESTAMP_
    | VERSION_
    ;

grantor
    : principal             #specifiedPrincipal
    | CURRENT_USER_          #currentUserGrantor
    | CURRENT_ROLE_          #currentRoleGrantor
    ;

principal
    : identifier            #unspecifiedPrincipal
    | USER_ identifier       #userPrincipal
    | ROLE_ identifier       #rolePrincipal
    ;

roles
    : identifier (COMMA_ identifier)*
    ;

identifier
    : IDENTIFIER_             #unquotedIdentifier
    | QUOTED_IDENTIFIER_      #quotedIdentifier
    | nonReserved            #unquotedIdentifier
    | BACKQUOTED_IDENTIFIER_  #backQuotedIdentifier
    | DIGIT_IDENTIFIER_       #digitIdentifier
    ;

number
    : MINUS_? DECIMAL_VALUE_  #decimalLiteral
    | MINUS_? DOUBLE_VALUE_   #doubleLiteral
    | MINUS_? INTEGER_VALUE_  #integerLiteral
    ;

nonReserved
    // IMPORTANT: this rule must only contain tokens. Nested rules are not supported. See SqlParser.exitNonReserved
    : ABSENT_ | ADD_ | ADMIN_ | AFTER_ | ALL_ | ANALYZE_ | ANY_ | ARRAY_ | ASC_ | AT_ | AUTHORIZATION_
    | BERNOULLI_ | BOTH_
    | CALL_ | CASCADE_ | CATALOGS_ | COLUMN_ | COLUMNS_ | COMMENT_ | COMMIT_ | COMMITTED_ | CONDITIONAL_ | COPARTITION_ | COUNT_ | CURRENT_
    | DATA_ | DATE_ | DAY_ | DEFAULT_ | DEFINE_ | DEFINER_ | DESC_ | DESCRIPTOR_ | DISTRIBUTED_ | DOUBLE_
    | EMPTY_ | ENCODING_ | ERROR_ | EXCLUDING_ | EXPLAIN_
    | FETCH_ | FILTER_ | FINAL_ | FIRST_ | FOLLOWING_ | FORMAT_ | FUNCTIONS_
    | GRACE_ | GRANT_ | DENY_ | GRANTED_ | GRANTS_ | GRAPHVIZ_ | GROUPS_
    | HOUR_
    | IF_ | IGNORE_ | INCLUDING_ | INITIAL_ | INPUT_ | INTERVAL_ | INVOKER_ | IO_ | ISOLATION_
    | JSON_
    | KEEP_ | KEY_ | KEYS_
    | LAST_ | LATERAL_ | LEADING_ | LEVEL_ | LIMIT_ | LOCAL_ | LOGICAL_
    | MAP_ | MATCH_ | MATCHED_ | MATCHES_ | MATCH_RECOGNIZE_ | MATERIALIZED_ | MEASURES_ | MERGE_ | MINUTE_ | MONTH_
    | NEXT_ | NFC_ | NFD_ | NFKC_ | NFKD_ | NO_ | NONE_ | NULLIF_ | NULLS_
    | OBJECT_ | OF_ | OFFSET_ | OMIT_ | ONE_ | ONLY_ | OPTION_ | ORDINALITY_ | OUTPUT_ | OVER_ | OVERFLOW_
    | PARTITION_ | PARTITIONS_ | PASSING_ | PAST_ | PATH_ | PATTERN_ | PER_ | PERIOD_ | PERMUTE_ | POSITION_ | PRECEDING_ | PRECISION_ | PRIVILEGES_ | PROPERTIES_ | PRUNE_
    | QUOTES_
    | RANGE_ | READ_ | REFRESH_ | RENAME_ | REPEATABLE_ | REPLACE_ | RESET_ | RESPECT_ | RESTRICT_ | RETURNING_ | REVOKE_ | ROLE_ | ROLES_ | ROLLBACK_ | ROW_ | ROWS_ | RUNNING_
    | SCALAR_ | SCHEMA_ | SCHEMAS_ | SECOND_ | SECURITY_ | SEEK_ | SERIALIZABLE_ | SESSION_ | SET_ | SETS_
    | SHOW_ | SOME_ | START_ | STATS_ | SUBSET_ | SUBSTRING_ | SYSTEM_
    | TABLES_ | TABLESAMPLE_ | TEXT_ | TEXT_STRING_ | TIES_ | TIME_ | TIMESTAMP_ | TO_ | TRAILING_ | TRANSACTION_ | TRUNCATE_ | TRY_CAST_ | TYPE_
    | UNBOUNDED_ | UNCOMMITTED_ | UNCONDITIONAL_ | UNIQUE_ | UNKNOWN_ | UNMATCHED_ | UPDATE_ | USE_ | USER_ | UTF16_ | UTF32_ | UTF8_
    | VALIDATE_ | VALUE_ | VERBOSE_ | VERSION_ | VIEW_
    | WINDOW_ | WITHIN_ | WITHOUT_ | WORK_ | WRAPPER_ | WRITE_
    | YEAR_
    | ZONE_
    ;
