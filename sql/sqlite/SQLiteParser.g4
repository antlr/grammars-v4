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

sql_stmt_list: ';'* sql_stmt (';'+ sql_stmt)* ';'*;

sql_stmt: (EXPLAIN (QUERY PLAN)?)? (
		alter_table_stmt
		| analyze_stmt
		| attach_stmt
		| begin_stmt
		| commit_stmt
		| create_index_stmt
		| create_table_stmt
		| create_trigger_stmt
		| create_view_stmt
		| create_virtual_table_stmt
		| delete_stmt
		| delete_stmt_limited
		| detach_stmt
		| drop_stmt
		| insert_stmt
		| pragma_stmt
		| reindex_stmt
		| release_stmt
		| rollback_stmt
		| savepoint_stmt
		| select_stmt
		| update_stmt
		| update_stmt_limited
		| vacuum_stmt
	);

alter_table_stmt:
	ALTER TABLE (schema_name '.')? table_name (
		RENAME (
			(TO new_table_name)
			| (
				COLUMN? old_column_name = column_name TO new_column_name = column_name
			)
		)
		| ADD COLUMN? column_def
	);

analyze_stmt:
	ANALYZE (
		schema_name
		| (schema_name '.')? table_or_index_name
	)?;

attach_stmt: ATTACH DATABASE? expr AS schema_name;

begin_stmt:
	BEGIN (DEFERRED | IMMEDIATE | EXCLUSIVE)? (
		TRANSACTION transaction_name?
	)?;

commit_stmt: ( COMMIT | END) TRANSACTION?;

rollback_stmt:
	ROLLBACK TRANSACTION? (TO SAVEPOINT? savepoint_name)?;

savepoint_stmt: SAVEPOINT savepoint_name;

release_stmt: RELEASE SAVEPOINT? savepoint_name;

create_index_stmt:
	CREATE UNIQUE? INDEX (IF NOT EXISTS)? (schema_name '.')? index_name ON table_name '('
		indexed_column (',' indexed_column)* ')' (WHERE expr)?;

indexed_column:
	(column_name | expr) (COLLATE collation_name)? asc_desc?;

create_table_stmt:
	CREATE (TEMP | TEMPORARY)? TABLE (IF NOT EXISTS)? (
		schema_name '.'
	)? table_name (
		(
			'(' column_def (',' column_def)* (
				',' table_constraint
			)* ')' (WITHOUT rowID = IDENTIFIER)?
		)
		| (AS select_stmt)
	);

column_def: column_name type_name? column_constraint*;

type_name:
	name+ (
		'(' signed_number ')'
		| '(' signed_number ',' signed_number ')'
	)?;

column_constraint: (CONSTRAINT name)? (
		(PRIMARY KEY asc_desc? conflict_clause? AUTOINCREMENT?)
		| ((NOT NULL_) | UNIQUE) conflict_clause?
		| CHECK '(' expr ')'
		| DEFAULT (
			signed_number
			| literal_value
			| ('(' expr ')')
		)
		| COLLATE collation_name
		| foreign_key_clause
		| (GENERATED ALWAYS)? AS '(' expr ')' (STORED | VIRTUAL)?
	);

signed_number: ( '+' | '-')? NUMERIC_LITERAL;

table_constraint: (CONSTRAINT name)? (
		(
			(PRIMARY KEY | UNIQUE) '(' indexed_column (
				',' indexed_column
			)* ')' conflict_clause?
		)
		| (CHECK '(' expr ')')
		| (
			FOREIGN KEY '(' column_name (',' column_name)* ')' foreign_key_clause
		)
	);

foreign_key_clause:
	REFERENCES foreign_table (
		'(' column_name ( ',' column_name)* ')'
	)? (
		(
			ON (DELETE | UPDATE) (
				(SET (NULL_ | DEFAULT))
				| CASCADE
				| RESTRICT
				| (NO ACTION)
			)
		)
		| (MATCH name)
	)* (NOT? DEFERRABLE ( INITIALLY (DEFERRED | IMMEDIATE))?)?;

conflict_clause:
	ON CONFLICT (ROLLBACK | ABORT | FAIL | IGNORE | REPLACE);
create_trigger_stmt:
	CREATE (TEMP | TEMPORARY)? TRIGGER (IF NOT EXISTS)? (
		schema_name '.'
	)? trigger_name (BEFORE | AFTER | (INSTEAD OF))? (
		DELETE
		| INSERT
		| (UPDATE ( OF column_name ( ',' column_name)*)?)
	) ON table_name (FOR EACH ROW)? (WHEN expr)? BEGIN (
		(update_stmt | insert_stmt | delete_stmt | select_stmt) ';'
	)+ END;

create_view_stmt:
	CREATE (TEMP | TEMPORARY)? VIEW (IF NOT EXISTS)? (
		schema_name '.'
	)? view_name ('(' column_name (',' column_name)* ')')? AS select_stmt;

create_virtual_table_stmt:
	CREATE VIRTUAL TABLE (IF NOT EXISTS)? (schema_name '.')? table_name USING module_name (
		'(' module_argument (',' module_argument)* ')'
	)?;

with_clause:
	WITH RECURSIVE? cte_table_name AS '(' select_stmt ')' (
		',' cte_table_name AS '(' select_stmt ')'
	)*;

cte_table_name:
	table_name ('(' column_name ( ',' column_name)* ')')?;

recursive_cte:
	cte_table_name AS '(' initial_select UNION ALL? recursive_select ')';

common_table_expression:
	table_name ('(' column_name ( ',' column_name)* ')')? AS '(' select_stmt ')';

delete_stmt:
	with_clause? DELETE FROM qualified_table_name (WHERE expr)?;

delete_stmt_limited:
	with_clause? DELETE FROM qualified_table_name (WHERE expr)? (
		order_by_stmt? limit_stmt
	)?;

detach_stmt: DETACH DATABASE? schema_name;

drop_stmt:
	DROP object = (INDEX | TABLE | TRIGGER | VIEW) (IF EXISTS)? (
		schema_name '.'
	)? any_name;

/*
 SQLite understands the following binary operators, in order from highest to lowest precedence: || /
 % + - << >> & | < <= > >= = == != <> IS IS NOT IN LIKE GLOB MATCH REGEXP AND OR
 */
expr:
	literal_value
	| BIND_PARAMETER
	| ( ( schema_name '.')? table_name '.')? column_name
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
	| function_name '(' ((DISTINCT? expr ( ',' expr)*) | '*')? ')' filter_clause? over_clause?
	| '(' expr (',' expr)* ')'
	| CAST '(' expr AS type_name ')'
	| expr COLLATE collation_name
	| expr NOT? (LIKE | GLOB | REGEXP | MATCH) expr (ESCAPE expr)?
	| expr ( ISNULL | NOTNULL | (NOT NULL_))
	| expr IS NOT? expr
	| expr NOT? BETWEEN expr AND expr
	| expr NOT? IN (
		('(' (select_stmt | expr ( ',' expr)*)? ')')
		| (( schema_name '.')? table_name)
		| (
			(schema_name '.')? table_function_name '(' (
				expr (',' expr)*
			)? ')'
		)
	)
	| ( ( NOT)? EXISTS)? '(' select_stmt ')'
	| CASE expr? ( WHEN expr THEN expr)+ ( ELSE expr)? END
	| raise_function;

raise_function:
	RAISE '(' (
		IGNORE
		| (( ROLLBACK | ABORT | FAIL) ',' error_message)
	) ')';

literal_value:
	NUMERIC_LITERAL
	| STRING_LITERAL
	| BLOB_LITERAL
	| NULL_
	| TRUE_
	| FALSE_
	| CURRENT_TIME
	| CURRENT_DATE
	| CURRENT_TIMESTAMP;

insert_stmt:
	with_clause? (
		INSERT
		| REPLACE
		| (
			INSERT OR (
				REPLACE
				| ROLLBACK
				| ABORT
				| FAIL
				| IGNORE
			)
		)
	) INTO (schema_name '.')? table_name (AS table_alias)? (
		'(' column_name ( ',' column_name)* ')'
	)? (
		(
			(
				VALUES '(' expr (',' expr)* ')' (
					',' '(' expr ( ',' expr)* ')'
				)*
			)
			| select_stmt
		) upsert_clause?
	)
	| (DEFAULT VALUES);

upsert_clause:
	ON CONFLICT (
		'(' indexed_column (',' indexed_column)* ')' (WHERE expr)?
	)? DO (
		NOTHING
		| (
			UPDATE SET (
				(column_name | column_name_list) EQ expr (
					',' (column_name | column_name_list) EQ expr
				)* (WHERE expr)?
			)
		)
	);

pragma_stmt:
	PRAGMA (schema_name '.')? pragma_name (
		'=' pragma_value
		| '(' pragma_value ')'
	)?;

pragma_value: signed_number | name | STRING_LITERAL;

reindex_stmt:
	REINDEX (
		collation_name
		| (( schema_name '.')? ( table_name | index_name))
	)?;

select_stmt:
	common_table_stmt? select_core (
		compound_operator select_core
	)* order_by_stmt? limit_stmt?;

join_clause:
	table_or_subquery (
		join_operator table_or_subquery join_constraint?
	)*;

select_core:
	(
		SELECT (DISTINCT | ALL)? result_column (
			',' result_column
		)* (
			FROM (
				table_or_subquery (',' table_or_subquery)*
				| join_clause
			)
		)? (WHERE expr)? (
			GROUP BY expr (',' expr)* (HAVING expr)?
		)? (
			WINDOW window_name AS window_defn (
				',' window_name AS window_defn
			)*
		)?
	)
	| VALUES '(' expr (',' expr)* ')' (
		',' '(' expr ( ',' expr)* ')'
	)*;

factored_select_stmt: select_stmt;

simple_select_stmt:
	common_table_stmt? select_core order_by_stmt? limit_stmt?;

compound_select_stmt:
	common_table_stmt? select_core (
		((UNION ALL?) | INTERSECT | EXCEPT) select_core
	)+ order_by_stmt? limit_stmt?;

table_or_subquery: (
		(schema_name '.')? table_name (AS? table_alias)? (
			(INDEXED BY index_name)
			| (NOT INDEXED)
		)?
	)
	| (
		(schema_name '.')? table_function_name '(' expr (
			',' expr
		)* ')' (AS? table_alias)?
	)
	| '(' (
		table_or_subquery (',' table_or_subquery)*
		| join_clause
	) ')'
	| ('(' select_stmt ')' ( AS? table_alias)?);

result_column:
	'*'
	| table_name '.' '*'
	| expr ( AS? column_alias)?;

join_operator:
	','
	| (NATURAL? ( (LEFT OUTER?) | INNER | CROSS)? JOIN);

join_constraint:
	(ON expr)
	| (USING '(' column_name ( ',' column_name)* ')');

compound_operator: (UNION ALL?) | INTERSECT | EXCEPT;

update_stmt:
	with_clause? UPDATE (
		OR (ROLLBACK | ABORT | REPLACE | FAIL | IGNORE)
	)? qualified_table_name SET (column_name | column_name_list) '=' expr (
		',' (column_name | column_name_list) '=' expr
	)* (WHERE expr)?;

column_name_list: '(' column_name (',' column_name)* ')';

update_stmt_limited:
	with_clause? UPDATE (
		OR (ROLLBACK | ABORT | REPLACE | FAIL | IGNORE)
	)? qualified_table_name SET (column_name | column_name_list) '=' expr (
		',' (column_name | column_name_list) '=' expr
	)* (WHERE expr)? (order_by_stmt? limit_stmt)?;

qualified_table_name: (schema_name '.')? table_name (AS alias)? (
		(INDEXED BY index_name)
		| (NOT INDEXED)
	)?;

vacuum_stmt: VACUUM schema_name? (INTO filename)?;

filter_clause: FILTER '(' WHERE expr ')';

window_defn:
	'(' base_window_name? (PARTITION BY expr (',' expr)*)? (
		ORDER BY ordering_term (',' ordering_term)*
	) frame_spec? ')';

over_clause:
	OVER (
		window_name
		| (
			'(' base_window_name? (PARTITION BY expr (',' expr)*)? (
				ORDER BY ordering_term (',' ordering_term)*
			)? frame_spec? ')'
		)
	);

frame_spec:
	frame_clause (
		EXCLUDE (NO OTHERS)
		| (CURRENT ROW)
		| GROUP
		| TIES
	)?;

frame_clause: (RANGE | ROWS | GROUPS) (
		frame_single
		| BETWEEN frame_left AND frame_right
	);
simple_function_invocation:
	simple_func '(' ((expr (',' expr)*) | '*') ')';

aggregate_function_invocation:
	aggregate_func '(' ((DISTINCT? expr (',' expr)*) | '*')? ')' filter_clause?;

window_function_invocation:
	window_function '(' ((expr (',' expr)*) | '*')? ')' filter_clause? OVER (
		window_defn
		| window_name
	);

common_table_stmt: //additional structures
	WITH RECURSIVE? common_table_expression (
		',' common_table_expression
	)*;

order_by_stmt: ORDER BY ordering_term ( ',' ordering_term)*;

limit_stmt: LIMIT expr ( (OFFSET | ',') expr)?;

ordering_term:
	expr (COLLATE collation_name)? asc_desc? (
		NULLS (FIRST | LAST)
	)?;
asc_desc: ASC | DESC;

frame_left:
	(expr PRECEDING)
	| (expr FOLLOWING)
	| (CURRENT ROW)
	| (UNBOUNDED PRECEDING);

frame_right:
	(expr PRECEDING)
	| (expr FOLLOWING)
	| (CURRENT ROW)
	| (UNBOUNDED FOLLOWING);

frame_single:
	(expr PRECEDING)
	| (UNBOUNDED PRECEDING)
	| (CURRENT ROW);

// unknown

window_function:
	(FIRST_VALUE | LAST_VALUE) '(' expr ')' OVER '(' partition_by? order_by_expr_asc_desc
		frame_clause? ')'
	| (CUME_DIST | PERCENT_RANK) '(' ')' OVER '(' partition_by? order_by_expr? ')'
	| (DENSE_RANK | RANK | ROW_NUMBER) '(' ')' OVER '(' partition_by? order_by_expr_asc_desc ')'
	| (LAG | LEAD) '(' expr offset? default_value? ')' OVER '(' partition_by? order_by_expr_asc_desc
		')'
	| NTH_VALUE '(' expr ',' signed_number ')' OVER '(' partition_by? order_by_expr_asc_desc
		frame_clause? ')'
	| NTILE '(' expr ')' OVER '(' partition_by? order_by_expr_asc_desc ')';

offset: ',' signed_number;

default_value: ',' signed_number;

partition_by: PARTITION BY expr+;

order_by_expr: ORDER BY expr+;

order_by_expr_asc_desc: ORDER BY order_by_expr_asc_desc;

expr_asc_desc: expr asc_desc? (',' expr asc_desc?)*;

//TODO BOTH OF THESE HAVE TO BE REWORKED TO FOLLOW THE SPEC
initial_select: select_stmt;

recursive_select: select_stmt;

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
	| NULL_
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
	| ROWS
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
	| WITHOUT
	| FIRST_VALUE
	| OVER
	| PARTITION
	| RANGE
	| PRECEDING
	| UNBOUNDED
	| CURRENT
	| FOLLOWING
	| CUME_DIST
	| DENSE_RANK
	| LAG
	| LAST_VALUE
	| LEAD
	| NTH_VALUE
	| NTILE
	| PERCENT_RANK
	| RANK
	| ROW_NUMBER
	| GENERATED
	| ALWAYS
	| STORED
	| TRUE_
	| FALSE_
	| WINDOW
	| NULLS
	| FIRST
	| LAST
	| FILTER
	| GROUPS
	| EXCLUDE;

// TODO check all names below

name: any_name;

function_name: any_name;

schema_name: any_name;

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

window_name: any_name;

alias: any_name;

filename: any_name;

base_window_name: any_name;

simple_func: any_name;

aggregate_func: any_name;

table_function_name: any_name;

any_name:
	IDENTIFIER
	| keyword
	| STRING_LITERAL
	| '(' any_name ')';
