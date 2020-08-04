/*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2014 by Bart Kiers
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
 * Project : sqlite-parser; an ANTLR4 grammar for SQLite https://github.com/bkiers/sqlite-parser
 * Developed by : Bart Kiers, bart@big-o.nl Martin Mirchev, marti_2203@abv.bg
 */
parser grammar SQLiteParser;

options {
	tokenVocab = SQLiteLexer;
}

parse: ( sql_stmt_list | error)* EOF;

error:
	UNEXPECTED_CHAR { 
     throw new RuntimeException("UNEXPECTED_CHAR=" + $UNEXPECTED_CHAR.text); 
   };

sql_stmt_list: (';'* sql_stmt ';'+)+;

sql_stmt: (EXPLAIN ( QUERY PLAN)?)? (
		alter_table_stmt
		| analyze_stmt
		| attach_stmt
		| begin_stmt
		| commit_stmt
		| compound_select_stmt
		| create_index_stmt
		| create_table_stmt
		| create_trigger_stmt
		| create_view_stmt
		| create_virtual_table_stmt
		| delete_stmt
		| delete_stmt_limited
		| detach_stmt
		| drop_stmt
		| factored_select_stmt
		| insert_stmt
		| pragma_stmt
		| reindex_stmt
		| release_stmt
		| rollback_stmt
		| savepoint_stmt
		| simple_select_stmt
		| select_stmt
		| update_stmt
		| update_stmt_limited
		| vacuum_stmt
	);

alter_table_stmt:
	ALTER TABLE (database_name '.')? table_name (
		RENAME TO new_table_name
		| ADD COLUMN? column_def
	);

analyze_stmt:
	ANALYZE (
		database_name
		| (database_name '.')? table_or_index_name
	)?;

attach_stmt: ATTACH DATABASE? expr AS database_name;

begin_stmt:
	BEGIN (DEFERRED | IMMEDIATE | EXCLUSIVE)? (
		TRANSACTION transaction_name?
	)?;

commit_stmt: ( COMMIT | END) ( TRANSACTION transaction_name?)?;

common_table_statement:
	WITH RECURSIVE? common_table_expression (
		',' common_table_expression
	)*;

compound_select_stmt:
	common_table_statement? select_core (
		(UNION ALL? | INTERSECT | EXCEPT) select_core
	)+ order_by_stmt? limit_stmt?;

create_index_stmt:
	CREATE UNIQUE? INDEX (IF NOT EXISTS)? (database_name '.')? index_name ON table_name '('
		indexed_column (',' indexed_column)* ')' (WHERE expr)?;

create_table_stmt:
	CREATE (TEMP | TEMPORARY)? TABLE (IF NOT EXISTS)? (
		database_name '.'
	)? table_name (
		'(' column_def (',' column_def)* (',' table_constraint)* ')' (
			WITHOUT IDENTIFIER
		)?
		| AS select_stmt
	);

create_trigger_stmt:
	CREATE (TEMP | TEMPORARY)? TRIGGER (IF NOT EXISTS)? (
		database_name '.'
	)? trigger_name (BEFORE | AFTER | INSTEAD OF)? (
		DELETE
		| INSERT
		| UPDATE ( OF column_name ( ',' column_name)*)?
	) ON (database_name '.')? table_name (FOR EACH ROW)? (
		WHEN expr
	)? BEGIN (
		(update_stmt | insert_stmt | delete_stmt | select_stmt) ';'
	)+ END;

create_view_stmt:
	CREATE (TEMP | TEMPORARY)? VIEW (IF NOT EXISTS)? (
		database_name '.'
	)? view_name AS select_stmt;

create_virtual_table_stmt:
	CREATE VIRTUAL TABLE (IF NOT EXISTS)? (database_name '.')? table_name USING module_name (
		'(' module_argument (',' module_argument)* ')'
	)?;

delete_stmt:
	with_clause? DELETE FROM qualified_table_name (WHERE expr)?;

delete_stmt_limited:
	with_clause? DELETE FROM qualified_table_name (WHERE expr)? (
		order_by_stmt? limit_stmt
	)?;

detach_stmt: DETACH DATABASE? database_name;

drop_stmt:
	DROP object = (INDEX | TABLE | TRIGGER | VIEW) (IF EXISTS)? (
		database_name '.'
	)? any_name;

factored_select_stmt:
	common_table_statement? select_core (
		compound_operator select_core
	)* order_by_stmt? limit_stmt?;

order_by_stmt: ORDER BY ordering_term ( ',' ordering_term)*;

limit_stmt: LIMIT expr ( (OFFSET | ',') expr)?;

insert_stmt:
	with_clause? (
		INSERT
		| REPLACE
		| INSERT OR (REPLACE | ROLLBACK | ABORT | FAIL | IGNORE)
	) INTO (database_name '.')? table_name (
		'(' column_name ( ',' column_name)* ')'
	)? (
		VALUES '(' expr (',' expr)* ')' (
			',' '(' expr ( ',' expr)* ')'
		)*
		| select_stmt
		| DEFAULT VALUES
	);

pragma_stmt:
	PRAGMA (database_name '.')? pragma_name (
		'=' pragma_value
		| '(' pragma_value ')'
	)?;

reindex_stmt:
	REINDEX (
		collation_name
		| ( database_name '.')? ( table_name | index_name)
	)?;

release_stmt: RELEASE SAVEPOINT? savepoint_name;

rollback_stmt:
	ROLLBACK (TRANSACTION transaction_name?)? (
		TO SAVEPOINT? savepoint_name
	)?;

savepoint_stmt: SAVEPOINT savepoint_name;

simple_select_stmt:
	common_table_statement? select_core order_by_stmt? limit_stmt?;

select_stmt:
	common_table_statement? select_or_values (
		compound_operator select_or_values
	)* order_by_stmt? limit_stmt?;

select_or_values:
	SELECT (DISTINCT | ALL)? result_column (',' result_column)* (
		FROM (
			table_or_subquery (',' table_or_subquery)*
			| join_clause
		)
	)? (WHERE expr)? (GROUP BY expr ( ',' expr)* ( HAVING expr)?)?
	| VALUES '(' expr (',' expr)* ')' (
		',' '(' expr ( ',' expr)* ')'
	)*;

update_stmt:
	with_clause? UPDATE (
		OR (ROLLBACK | ABORT | REPLACE | FAIL | IGNORE)
	)? qualified_table_name SET column_name '=' expr (
		',' column_name '=' expr
	)* (WHERE expr)?;

update_stmt_limited:
	with_clause? UPDATE (
		OR (ROLLBACK | ABORT | REPLACE | FAIL | IGNORE)
	)? qualified_table_name SET column_name '=' expr (
		',' column_name '=' expr
	)* (WHERE expr)? (order_by_stmt? limit_stmt)?;

vacuum_stmt: VACUUM;

column_def: column_name type_name? column_constraint*;

type_name:
	name+ (
		'(' signed_number ')'
		| '(' signed_number ',' signed_number ')'
	)?;

column_constraint: (CONSTRAINT name)? (
		PRIMARY KEY (ASC | DESC)? conflict_clause? AUTOINCREMENT?
		| ((NOT? NULL) | UNIQUE) conflict_clause?
		| CHECK '(' expr ')'
		| DEFAULT (signed_number | literal_value | '(' expr ')')
		| COLLATE collation_name
		| foreign_key_clause
	);

conflict_clause:
	ON CONFLICT (ROLLBACK | ABORT | FAIL | IGNORE | REPLACE);

/*
 SQLite understands the following binary operators, in order from highest to lowest precedence:
 
 || / % + - << >> & | < <= > >= = == != <> IS IS NOT IN LIKE GLOB MATCH REGEXP AND OR
 */
expr:
	literal_value
	| BIND_PARAMETER
	| ( ( database_name '.')? table_name '.')? column_name
	| unary_operator expr
	| expr '||' expr
	| expr ( '*' | '/' | '%') expr
	| expr ( '+' | '-') expr
	| expr ( '<<' | '>>' | '&' | '|') expr
	| expr ( '<' | '<=' | '>' | '>=') expr
	| expr (
		'='
		| '=='
		| '!='
		| '<>'
		| IS
		| IS NOT
		| IN
		| LIKE
		| GLOB
		| MATCH
		| REGEXP
	) expr
	| expr AND expr
	| expr OR expr
	| function_name '(' (DISTINCT? expr ( ',' expr)* | '*')? ')'
	| '(' expr ')'
	| CAST '(' expr AS type_name ')'
	| expr COLLATE collation_name
	| expr NOT? (LIKE | GLOB | REGEXP | MATCH) expr (ESCAPE expr)?
	| expr ( ISNULL | NOTNULL | NOT NULL)
	| expr IS NOT? expr
	| expr NOT? BETWEEN expr AND expr
	| expr NOT? IN (
		'(' ( select_stmt | expr ( ',' expr)*)? ')'
		| ( database_name '.')? table_name
	)
	| ( ( NOT)? EXISTS)? '(' select_stmt ')'
	| CASE expr? ( WHEN expr THEN expr)+ ( ELSE expr)? END
	| raise_function
	| window_function;

foreign_key_clause:
	REFERENCES foreign_table (
		'(' column_name ( ',' column_name)* ')'
	)? (
		(
			ON (DELETE | UPDATE) (
				SET (NULL | DEFAULT)
				| CASCADE
				| RESTRICT
				| NO ACTION
			)
			| MATCH name
		)
	)* (NOT? DEFERRABLE ( INITIALLY (DEFERRED | IMMEDIATE))?)?;

raise_function:
	RAISE '(' (
		IGNORE
		| ( ROLLBACK | ABORT | FAIL) ',' error_message
	) ')';

indexed_column:
	column_name (COLLATE collation_name)? (ASC | DESC)?;

table_constraint: (CONSTRAINT name)? (
		(PRIMARY KEY | UNIQUE) '(' indexed_column (
			',' indexed_column
		)* ')' conflict_clause?
		| CHECK '(' expr ')'
		| FOREIGN KEY '(' column_name (',' column_name)* ')' foreign_key_clause
	);

with_clause:
	WITH RECURSIVE? cte_table_name AS '(' select_stmt ')' (
		',' cte_table_name AS '(' select_stmt ')'
	)*;

qualified_table_name: (database_name '.')? table_name (
		INDEXED BY index_name
		| NOT INDEXED
	)?;

ordering_term: expr ( COLLATE collation_name)? ( ASC | DESC)?;

pragma_value: signed_number | name | STRING_LITERAL;

common_table_expression:
	table_name ('(' column_name ( ',' column_name)* ')')? AS '(' select_stmt ')';

result_column:
	'*'
	| table_name '.' '*'
	| (expr | window_function) ( AS? column_alias)?;

window_function:
	FIRST_VALUE '(' expr ')' OVER '(' partition_by? order_by_expr_asc_desc frame_clause? ')'
	| CUMEDIST '(' ')' OVER '(' partition_by? order_by_expr? ')'
	| DENSERANK '(' ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| LAG '(' expr offset? default_value? ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| LASTVALUE '(' expr ')' OVER '(' partition_by? order_by_expr_asc_desc frame_clause? ')'
	| LEAD '(' expr offset? default_value? ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| NTHVALUE '(' expr ',' signed_number ')' OVER '(' partition_by? order_by_expr_asc_desc
		frame_clause? ')'
	| NTILE '(' expr ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| PERCENTRANK '(' ')' OVER '(' partition_by? order_by_expr? ')'
	| RANK '(' ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| ROWNUMBER '(' ')' OVER '(' partition_by? order_by_expr_asc_desc ')';

offset: ',' signed_number;

default_value: ',' signed_number;

partition_by: PARTITION BY expr+;

order_by_expr: ORDER BY expr+;

order_by_expr_asc_desc: ORDER BY order_by_expr_asc_desc;

expr_asc_desc: expr asc_desc? (',' expr asc_desc?)*;

asc_desc: ASC | DESC;

frame_clause: (RANGE | ROWS) (
		frame_start
		| BETWEEN frame_start AND frame_end
	);
frame_start:
	signed_number PRECEDING
	| UBOUNDED PRECEDING
	| CURRENT ROW;
frame_end:
	signed_number FOLLOWING
	| UBOUNDED FOLLOWING
	| CURRENT ROW;

table_or_subquery: (database_name '.')? table_name (
		AS? table_alias
	)? (INDEXED BY index_name | NOT INDEXED)?
	| '(' (
		table_or_subquery ( ',' table_or_subquery)*
		| join_clause
	) ')' (AS? table_alias)?
	| '(' select_stmt ')' ( AS? table_alias)?;

join_clause:
	table_or_subquery (
		join_operator table_or_subquery join_constraint
	)*;

join_operator:
	','
	| NATURAL? ( LEFT OUTER? | INNER | CROSS)? JOIN;

join_constraint: (
		ON expr
		| USING '(' column_name ( ',' column_name)* ')'
	)?;

select_core:
	SELECT (DISTINCT | ALL)? result_column (',' result_column)* (
		FROM (
			table_or_subquery (',' table_or_subquery)*
			| join_clause
		)
	)? (WHERE expr)? (GROUP BY expr ( ',' expr)* ( HAVING expr)?)?
	| VALUES '(' expr (',' expr)* ')' (
		',' '(' expr ( ',' expr)* ')'
	)*;

compound_operator: UNION | UNION ALL | INTERSECT | EXCEPT;

cte_table_name:
	table_name ('(' column_name ( ',' column_name)* ')')?;

signed_number: ( '+' | '-')? NUMERIC_LITERAL;

literal_value:
	NUMERIC_LITERAL
	| STRING_LITERAL
	| BLOB_LITERAL
	| NULL
	| CURRENT_TIME
	| CURRENT_DATE
	| CURRENT_TIMESTAMP;

unary_operator: '-' | '+' | '~' | NOT;

error_message: STRING_LITERAL;

module_argument: // TODO check what exactly is permitted here
	expr
	| column_def;

column_alias: IDENTIFIER | STRING_LITERAL;

keyword:
	ABORT
	| ACTION
	| ADD
	| AFTER
	| ALL
	| ALTER
	| ANALYZE
	| AND
	| AS
	| ASC
	| ATTACH
	| AUTOINCREMENT
	| BEFORE
	| BEGIN
	| BETWEEN
	| BY
	| CASCADE
	| CASE
	| CAST
	| CHECK
	| COLLATE
	| COLUMN
	| COMMIT
	| CONFLICT
	| CONSTRAINT
	| CREATE
	| CROSS
	| CURRENT_DATE
	| CURRENT_TIME
	| CURRENT_TIMESTAMP
	| DATABASE
	| DEFAULT
	| DEFERRABLE
	| DEFERRED
	| DELETE
	| DESC
	| DETACH
	| DISTINCT
	| DROP
	| EACH
	| ELSE
	| END
	| ESCAPE
	| EXCEPT
	| EXCLUSIVE
	| EXISTS
	| EXPLAIN
	| FAIL
	| FOR
	| FOREIGN
	| FROM
	| FULL
	| GLOB
	| GROUP
	| HAVING
	| IF
	| IGNORE
	| IMMEDIATE
	| IN
	| INDEX
	| INDEXED
	| INITIALLY
	| INNER
	| INSERT
	| INSTEAD
	| INTERSECT
	| INTO
	| IS
	| ISNULL
	| JOIN
	| KEY
	| LEFT
	| LIKE
	| LIMIT
	| MATCH
	| NATURAL
	| NO
	| NOT
	| NOTNULL
	| NULL
	| OF
	| OFFSET
	| ON
	| OR
	| ORDER
	| OUTER
	| PLAN
	| PRAGMA
	| PRIMARY
	| QUERY
	| RAISE
	| RECURSIVE
	| REFERENCES
	| REGEXP
	| REINDEX
	| RELEASE
	| RENAME
	| REPLACE
	| RESTRICT
	| RIGHT
	| ROLLBACK
	| ROW
	| SAVEPOINT
	| SELECT
	| SET
	| TABLE
	| TEMP
	| TEMPORARY
	| THEN
	| TO
	| TRANSACTION
	| TRIGGER
	| UNION
	| UNIQUE
	| UPDATE
	| USING
	| VACUUM
	| VALUES
	| VIEW
	| VIRTUAL
	| WHEN
	| WHERE
	| WITH
	| WITHOUT;

// TODO check all names below

name: any_name;

function_name: any_name;

database_name: any_name;

table_name: any_name;

table_or_index_name: any_name;

new_table_name: any_name;

column_name: any_name;

collation_name: any_name;

foreign_table: any_name;

index_name: any_name;

trigger_name: any_name;

view_name: any_name;

module_name: any_name;

pragma_name: any_name;

savepoint_name: any_name;

table_alias: any_name;

transaction_name: any_name;

any_name:
	IDENTIFIER
	| keyword
	| STRING_LITERAL
	| '(' any_name ')';
