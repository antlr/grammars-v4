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

parser grammar FromClauseParser;

//-----------------------------------------------------------------------------------

tableAllColumns
    : STAR
    | tableName DOT STAR
    ;

// (table|column)
tableOrColumn
    : identifier
    ;

expressionList
    : expression (COMMA expression)*
    ;

aliasList
    : identifier (COMMA identifier)*
    ;

//----------------------- Rules for parsing fromClause ------------------------------
// from [col1, col2, col3] table1, [col4, col5] table2
fromClause
    : KW_FROM fromSource
    ;

fromSource
    : uniqueJoinToken uniqueJoinSource (COMMA uniqueJoinSource)+
    | joinSource
    ;


atomjoinSource
    : tableSource lateralView*
    | virtualTableSource lateralView*
    | subQuerySource lateralView*
    | partitionedTableFunction lateralView*
    | LPAREN joinSource RPAREN
    ;

joinSource
    : atomjoinSource (joinToken joinSourcePart (KW_ON expression | KW_USING columnParenthesesList)?)*
    ;

joinSourcePart
    : (tableSource | virtualTableSource | subQuerySource | partitionedTableFunction) lateralView*
    ;

uniqueJoinSource
    : KW_PRESERVE? uniqueJoinTableSource uniqueJoinExpr
    ;

uniqueJoinExpr
    : LPAREN expressionList RPAREN
    ;

uniqueJoinToken
    : KW_UNIQUEJOIN
    ;

joinToken
    : KW_JOIN
    | KW_INNER KW_JOIN
    | COMMA
    | KW_CROSS KW_JOIN
    | KW_LEFT  KW_OUTER? KW_JOIN
    | KW_RIGHT KW_OUTER? KW_JOIN
    | KW_FULL  KW_OUTER? KW_JOIN
    | KW_LEFT KW_SEMI KW_JOIN
    ;

lateralView
	: KW_LATERAL KW_VIEW KW_OUTER function_ tableAlias (KW_AS identifier (COMMA identifier)*)?
	| COMMA? KW_LATERAL KW_VIEW function_ tableAlias (KW_AS identifier (COMMA identifier)*)?
    | COMMA? KW_LATERAL KW_TABLE LPAREN valuesClause RPAREN KW_AS? tableAlias (LPAREN identifier (COMMA identifier)* RPAREN)?
	;

tableAlias
    : identifier
    ;

tableBucketSample
    : KW_TABLESAMPLE LPAREN KW_BUCKET Number KW_OUT KW_OF Number (KW_ON expression (COMMA expression)*)? RPAREN
    ;

splitSample
    : KW_TABLESAMPLE LPAREN  Number (KW_PERCENT|KW_ROWS) RPAREN
    | KW_TABLESAMPLE LPAREN  ByteLengthLiteral RPAREN
    ;

tableSample
    : tableBucketSample
    | splitSample
    ;

tableSource
    : tableName tableProperties? tableSample? (KW_AS? identifier)?
    ;

uniqueJoinTableSource
    : tableName tableSample? (KW_AS? identifier)?
    ;

tableName
    : identifier DOT identifier
    | identifier
    ;

viewName
    : (identifier DOT)? identifier
    ;

subQuerySource
    : LPAREN queryStatementExpression RPAREN KW_AS? identifier
    ;

//---------------------- Rules for parsing PTF clauses -----------------------------
partitioningSpec
   : partitionByClause orderByClause?
   | orderByClause
   | distributeByClause sortByClause?
   | sortByClause
   | clusterByClause
   ;

partitionTableFunctionSource
   : subQuerySource
   | tableSource
   | partitionedTableFunction
   ;

partitionedTableFunction
   : identifier LPAREN KW_ON
   partitionTableFunctionSource partitioningSpec?
   (Identifier LPAREN expression RPAREN ( COMMA Identifier LPAREN expression RPAREN)*)?
   RPAREN identifier?
   ;

//----------------------- Rules for parsing whereClause -----------------------------
// where a=b and ...
whereClause
    : KW_WHERE searchCondition
    ;

searchCondition
    : expression
    ;

//-----------------------------------------------------------------------------------

//-------- Row Constructor ----------------------------------------------------------
//in support of SELECT * FROM (VALUES(1,2,3),(4,5,6),...) as FOO(a,b,c) and
// INSERT INTO <table> (col1,col2,...) VALUES(...),(...),...
// INSERT INTO <table> (col1,col2,...) SELECT * FROM (VALUES(1,2,3),(4,5,6),...) as Foo(a,b,c)
/*
VALUES(1),(2) means 2 rows, 1 column each.
VALUES(1,2),(3,4) means 2 rows, 2 columns each.
VALUES(1,2,3) means 1 row, 3 columns
*/
valuesClause
    : KW_VALUES valuesTableConstructor
    ;

valuesTableConstructor
    : valueRowConstructor (COMMA valueRowConstructor)*
    ;

valueRowConstructor
    : expressionsInParenthesis
    ;

/*
This represents a clause like this:
TABLE(VALUES(1,2),(2,3)) as VirtTable(col1,col2)
*/
virtualTableSource
    : KW_TABLE LPAREN valuesClause RPAREN KW_AS? tableAlias (LPAREN identifier (COMMA identifier)*)? RPAREN
    ;

//-----------------------------------------------------------------------------------
