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
parser grammar IdentifiersParser;

//-----------------------------------------------------------------------------------

// group by a,b
groupByClause
    : KW_GROUP KW_BY groupby_expression
    ;

// support for new and old rollup/cube syntax
groupby_expression
    : rollupStandard
    | rollupOldSyntax
    | groupByEmpty
    ;

groupByEmpty
    : LPAREN RPAREN
    ;

// standard rollup syntax
rollupStandard
    : (KW_ROLLUP | KW_CUBE)
    LPAREN expression ( COMMA expression)* RPAREN
    ;

// old hive rollup syntax
rollupOldSyntax
    : expressionsNotInParenthesis
    (KW_WITH KW_ROLLUP | KW_WITH KW_CUBE) ?
    (KW_GROUPING KW_SETS
    LPAREN groupingSetExpression ( COMMA groupingSetExpression)*  RPAREN ) ?
    ;


groupingSetExpression
   : groupingSetExpressionMultiple
   | groupingExpressionSingle
   ;

groupingSetExpressionMultiple
   : LPAREN
   expression? (COMMA expression)*
   RPAREN
   ;

groupingExpressionSingle
    : expression
    ;

havingClause
    : KW_HAVING havingCondition
    ;

havingCondition
    : expression
    ;

expressionsInParenthesis
    : LPAREN expressionsNotInParenthesis RPAREN
    ;

expressionsNotInParenthesis
    : expression expressionPart?
    ;

expressionPart
    : (COMMA expression)+
    ;

expressions
    : expressionsInParenthesis
    | expressionsNotInParenthesis
    ;

columnRefOrderInParenthesis
    : LPAREN columnRefOrder (COMMA columnRefOrder)* RPAREN
    ;

columnRefOrderNotInParenthesis
    : columnRefOrder (COMMA columnRefOrder)*
    ;

// order by a,b
orderByClause
    : KW_ORDER KW_BY columnRefOrder ( COMMA columnRefOrder)*
    ;

clusterByClause
    : KW_CLUSTER KW_BY expressions
    ;

partitionByClause
    : KW_PARTITION KW_BY expressions
    ;

distributeByClause
    : KW_DISTRIBUTE KW_BY expressions
    ;

sortByClause
    : KW_SORT KW_BY
    (
    columnRefOrderInParenthesis
    |
    columnRefOrderNotInParenthesis
    )
    ;

// fun(par1, par2, par3)
function_
    : functionName
    LPAREN
      (
        STAR
        | (KW_DISTINCT | KW_ALL)? (selectExpression (COMMA selectExpression)*)?
      )
    RPAREN (KW_OVER window_specification)?
    ;

functionName
    : // Keyword IF is also a function name
    functionIdentifier
    |
    sql11ReservedKeywordsUsedAsFunctionName
    ;

castExpression
    : KW_CAST
    LPAREN
          expression
          KW_AS
          primitiveType
    RPAREN
    ;

caseExpression
    : KW_CASE expression
    (KW_WHEN expression KW_THEN expression)+
    (KW_ELSE expression)?
    KW_END
    ;

whenExpression
    : KW_CASE
    ( KW_WHEN expression KW_THEN expression)+
    (KW_ELSE expression)?
    KW_END
    ;

floorExpression
    : KW_FLOOR
    LPAREN
          expression
          (KW_TO floorDateQualifiers)?
    RPAREN
    ;

floorDateQualifiers
    : KW_YEAR
    | KW_QUARTER
    | KW_MONTH
    | KW_WEEK
    | KW_DAY
    | KW_HOUR
    | KW_MINUTE
    | KW_SECOND
    ;

extractExpression
    : KW_EXTRACT
    LPAREN
          timeQualifiers
          KW_FROM
          expression
    RPAREN
    ;

timeQualifiers
    : KW_YEAR
    | KW_QUARTER
    | KW_MONTH
    | KW_WEEK
    | KW_DAY
    | KW_DOW
    | KW_HOUR
    | KW_MINUTE
    | KW_SECOND
    ;

constant
    : intervalLiteral
    | Number
    | dateLiteral
    | timestampLiteral
    | timestampLocalTZLiteral
    | StringLiteral
    | stringLiteralSequence
    | IntegralLiteral
    | NumberLiteral
    | charSetStringLiteral
    | booleanValue
    | KW_NULL
    ;

stringLiteralSequence
    : StringLiteral StringLiteral+
    ;

charSetStringLiteral
    : CharSetName CharSetLiteral
    ;

dateLiteral
    : KW_DATE StringLiteral
    | KW_CURRENT_DATE
    ;

timestampLiteral
    : KW_TIMESTAMP StringLiteral
    | KW_CURRENT_TIMESTAMP
    ;

timestampLocalTZLiteral
    : KW_TIMESTAMPLOCALTZ StringLiteral
    ;

intervalValue
    : StringLiteral
    | Number
    ;

intervalLiteral
    : intervalValue intervalQualifiers
    ;

intervalExpression
    : LPAREN intervalValue RPAREN intervalQualifiers
    | KW_INTERVAL intervalValue intervalQualifiers
    | KW_INTERVAL LPAREN expression RPAREN intervalQualifiers
    ;

intervalQualifiers
    : KW_YEAR KW_TO KW_MONTH
    | KW_DAY KW_TO KW_SECOND
    | KW_YEAR
    | KW_MONTH
    | KW_DAY
    | KW_HOUR
    | KW_MINUTE
    | KW_SECOND
    ;

atomExpression
    : constant
    | intervalExpression
    | castExpression
    | extractExpression
    | floorExpression
    | caseExpression
    | whenExpression
    | subQueryExpression
    | function_
    | tableOrColumn
    | expressionsInParenthesis
    ;

precedenceUnaryOperator
    : PLUS
    | MINUS
    | TILDE
    ;

isCondition
    : KW_NOT? (KW_NULL | KW_TRUE | KW_FALSE | KW_DISTINCT KW_FROM)
    ;

precedenceBitwiseXorOperator
    : BITWISEXOR
    ;

precedenceStarOperator
    : STAR
    | DIVIDE
    | MOD
    | DIV
    ;

precedencePlusOperator
    : PLUS
    | MINUS
    ;

precedenceConcatenateOperator
    : CONCATENATE
    ;

precedenceAmpersandOperator
    : AMPERSAND
    ;

precedenceBitwiseOrOperator
    : BITWISEOR
    ;

precedenceRegexpOperator
    : KW_LIKE
    | KW_RLIKE
    | KW_REGEXP
    ;

precedenceComparisonOperator
    : LESSTHANOREQUALTO
    | LESSTHAN
    | GREATERTHANOREQUALTO
    | GREATERTHAN
    | EQUAL
    | EQUAL_NS
    | NOTEQUAL
    ;

precedenceNotOperator
    : KW_NOT
    ;

precedenceLogicOperator
    : KW_AND
    | KW_OR
    ;

//precedenceFieldExpression
//precedenceUnaryPrefixExpression
//precedenceUnarySuffixExpression
//precedenceBitwiseXorExpression
//precedenceStarExpression
//precedencePlusExpression
//precedenceConcatenateExpression
//precedenceAmpersandExpression
//precedenceBitwiseOrExpression
//precedenceSimilarExpressionMain
//precedenceSimilarExpression
//precedenceEqualExpression
//precedenceNotExpression
//precedenceAndExpression
//precedenceOrExpression
expression
    : expression precedenceLogicOperator expression
    | LPAREN expression RPAREN
    | precedenceExpression
    ;

precedenceExpression
    : atomExpression ((LSQUARE expression RSQUARE) | (DOT identifier))*
    | precedenceUnaryOperator precedenceExpression
    | precedenceExpression KW_IS isCondition
    | precedenceExpression precedenceBitwiseXorOperator precedenceExpression
    | precedenceExpression precedenceStarOperator precedenceExpression
    | precedenceExpression precedencePlusOperator precedenceExpression
    | precedenceExpression precedenceConcatenateOperator precedenceExpression
    | precedenceExpression precedenceAmpersandOperator precedenceExpression
    | precedenceExpression precedenceBitwiseOrOperator precedenceExpression
    | precedenceExpression precedenceComparisonOperator precedenceExpression
    | precedenceExpression KW_NOT? precedenceRegexpOperator precedenceExpression
    | precedenceExpression KW_NOT? KW_LIKE (KW_ANY | KW_ALL) expressionsInParenthesis
    | precedenceExpression KW_NOT? KW_IN precedenceSimilarExpressionIn
    | precedenceExpression KW_NOT? KW_BETWEEN precedenceExpression KW_AND precedenceExpression
    | KW_EXISTS subQueryExpression
    | precedenceNotOperator precedenceExpression
    ;

precedenceSimilarExpressionIn
    : subQueryExpression
    | expressionsInParenthesis
    ;

subQueryExpression
    : LPAREN selectStatement RPAREN
    ;

booleanValue
    : KW_TRUE
    | KW_FALSE
    ;

booleanValueTok
   : KW_TRUE
   | KW_FALSE
   ;

tableOrPartition
   : tableName partitionSpec?
   ;

partitionSpec
   : KW_PARTITION
   LPAREN partitionVal (COMMA  partitionVal)* RPAREN
   ;

partitionVal
    : identifier (EQUAL constant)?
    ;

dropPartitionSpec
    : KW_PARTITION LPAREN dropPartitionVal (COMMA  dropPartitionVal )* RPAREN
    ;

dropPartitionVal
    : identifier dropPartitionOperator constant
    ;

dropPartitionOperator
    : EQUAL
    | NOTEQUAL
    | LESSTHANOREQUALTO
    | LESSTHAN
    | GREATERTHANOREQUALTO
    | GREATERTHAN
    ;

sysFuncNames
    : KW_AND
    | KW_OR
    | KW_NOT
    | KW_LIKE
    | KW_IF
    | KW_CASE
    | KW_WHEN
    | KW_FLOOR
    | KW_TINYINT
    | KW_SMALLINT
    | KW_INT
    | KW_BIGINT
    | KW_FLOAT
    | KW_DOUBLE
    | KW_BOOLEAN
    | KW_STRING
    | KW_BINARY
    | KW_ARRAY
    | KW_MAP
    | KW_STRUCT
    | KW_UNIONTYPE
    | EQUAL
    | EQUAL_NS
    | NOTEQUAL
    | LESSTHANOREQUALTO
    | LESSTHAN
    | GREATERTHANOREQUALTO
    | GREATERTHAN
    | DIVIDE
    | PLUS
    | MINUS
    | STAR
    | MOD
    | DIV
    | AMPERSAND
    | TILDE
    | BITWISEOR
    | BITWISEXOR
    | KW_RLIKE
    | KW_REGEXP
    | KW_IN
    | KW_BETWEEN
    ;

descFuncNames
    : sysFuncNames
    | StringLiteral
    | functionIdentifier
    ;

identifier
    : Identifier
    | nonReserved
    ;

functionIdentifier
    : identifier DOT identifier
    | identifier
    ;

principalIdentifier
    : identifier
    | QuotedIdentifier
    ;

// Here is what you have to do if you would like to add a new keyword.
// Note that non reserved keywords are basically the keywords that can be used as identifiers.
// (1) Add a new entry to HiveLexer, e.g., KW_TRUE : 'TRUE';
// (2) If it is reserved, you do NOT need to change IdentifiersParser.g
//                        because all the KW_* are automatically not only keywords, but also reserved keywords.
//                        However, you need to add a test to TestSQL11ReservedKeyWordsNegative.java.
//     Otherwise it is non-reserved, you need to put them in the nonReserved list below.
//If you are not sure, please refer to the SQL2011 column in
//http://www.postgresql.org/docs/9.5/static/sql-keywords-appendix.html
nonReserved
    : KW_ABORT | KW_ADD | KW_ADMIN | KW_AFTER | KW_ANALYZE | KW_ARCHIVE | KW_ASC | KW_BEFORE | KW_BUCKET | KW_BUCKETS
    | KW_CASCADE | KW_CHANGE | KW_CHECK | KW_CLUSTER | KW_CLUSTERED | KW_CLUSTERSTATUS | KW_COLLECTION | KW_COLUMNS
    | KW_COMMENT | KW_COMPACT | KW_COMPACTIONS | KW_COMPUTE | KW_CONCATENATE | KW_CONTINUE | KW_DATA | KW_DAY
    | KW_DATABASES | KW_DATETIME | KW_DBPROPERTIES | KW_DEFERRED | KW_DEFINED | KW_DELIMITED | KW_DEPENDENCY
    | KW_DESC | KW_DIRECTORIES | KW_DIRECTORY | KW_DISABLE | KW_DISTRIBUTE | KW_DOW | KW_ELEM_TYPE
    | KW_ENABLE | KW_ENFORCED | KW_ESCAPED | KW_EXCLUSIVE | KW_EXPLAIN | KW_EXPORT | KW_FIELDS | KW_FILE | KW_FILEFORMAT
    | KW_FIRST | KW_FORMAT | KW_FORMATTED | KW_FUNCTIONS | KW_HOUR | KW_IDXPROPERTIES
    | KW_INDEX | KW_INDEXES | KW_INPATH | KW_INPUTDRIVER | KW_INPUTFORMAT | KW_ITEMS | KW_JAR | KW_KILL
    | KW_KEYS | KW_KEY_TYPE | KW_LAST | KW_LIMIT | KW_OFFSET | KW_LINES | KW_LOAD | KW_LOCATION | KW_LOCK | KW_LOCKS | KW_LOGICAL | KW_LONG
    | KW_MAPJOIN | KW_MATERIALIZED | KW_METADATA | KW_MINUTE | KW_MONTH | KW_MSCK | KW_NOSCAN | KW_NULLS
    | KW_OPTION | KW_OUTPUTDRIVER | KW_OUTPUTFORMAT | KW_OVERWRITE | KW_OWNER | KW_PARTITIONED | KW_PARTITIONS | KW_PLUS
    | KW_PRINCIPALS | KW_PURGE | KW_QUERY | KW_QUARTER | KW_READ | KW_REBUILD | KW_RECORDREADER | KW_RECORDWRITER
    | KW_RELOAD | KW_RENAME | KW_REPAIR | KW_REPLACE | KW_REPLICATION | KW_RESTRICT | KW_REWRITE
    | KW_ROLE | KW_ROLES | KW_SCHEMA | KW_SCHEMAS | KW_SECOND | KW_SEMI | KW_SERDE | KW_SERDEPROPERTIES | KW_SERVER | KW_SETS | KW_SHARED
    | KW_SHOW | KW_SHOW_DATABASE | KW_SKEWED | KW_SORT | KW_SORTED | KW_SSL | KW_STATISTICS | KW_STORED
    | KW_STREAMTABLE | KW_STRING | KW_STRUCT | KW_TABLES | KW_TBLPROPERTIES | KW_TEMPORARY | KW_TERMINATED
    | KW_TINYINT | KW_TOUCH | KW_TRANSACTIONS | KW_UNARCHIVE | KW_UNDO | KW_UNIONTYPE | KW_UNLOCK | KW_UNSET
    | KW_UNSIGNED | KW_URI | KW_USE | KW_UTC | KW_UTCTIMESTAMP | KW_VALUE_TYPE | KW_VIEW | KW_WEEK | KW_WHILE | KW_YEAR
    | KW_WORK
    | KW_TRANSACTION
    | KW_WRITE
    | KW_ISOLATION
    | KW_LEVEL
    | KW_SNAPSHOT
    | KW_AUTOCOMMIT
    | KW_RELY
    | KW_NORELY
    | KW_VALIDATE
    | KW_NOVALIDATE
    | KW_KEY
    | KW_MATCHED
    | KW_REPL | KW_DUMP | KW_STATUS
    | KW_CACHE | KW_VIEWS
    | KW_VECTORIZATION
    | KW_SUMMARY
    | KW_OPERATOR
    | KW_EXPRESSION
    | KW_DETAIL
    | KW_WAIT
    | KW_ZONE
    | KW_DEFAULT
    | KW_REOPTIMIZATION
    | KW_RESOURCE | KW_PLAN | KW_PLANS | KW_QUERY_PARALLELISM | KW_ACTIVATE | KW_MOVE | KW_DO
    | KW_POOL | KW_ALLOC_FRACTION | KW_SCHEDULING_POLICY | KW_PATH | KW_MAPPING | KW_WORKLOAD | KW_MANAGEMENT | KW_ACTIVE | KW_UNMANAGED

;

//The following SQL2011 reserved keywords are used as function name only, but not as identifiers.
sql11ReservedKeywordsUsedAsFunctionName
    : KW_IF | KW_ARRAY | KW_MAP | KW_BIGINT | KW_BINARY | KW_BOOLEAN | KW_CURRENT_DATE | KW_CURRENT_TIMESTAMP | KW_DATE | KW_DOUBLE | KW_FLOAT | KW_GROUPING | KW_INT | KW_SMALLINT | KW_TIMESTAMP
    ;
