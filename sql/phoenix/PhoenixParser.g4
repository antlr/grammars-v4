/*
Apache Phoenix grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar PhoenixParser;

options { tokenVocab=PhoenixLexer; }

phoenix_file
    : batch* EOF
    ;

batch
    : sql_command SEMI?
    ;

sql_command
    : ddl_command
    | dml_command
    | other_command
    ;

ddl_command
    : alter_command
    | alter_index_command
    | create_function_command
    | create_index_command
    | create_schema_command
    | create_sequence_command
    | create_table_command
    | create_view_command
    | drop_function_command
    | drop_index_command
    | drop_schema_command
    | drop_sequence_command
    | drop_table_command
    | drop_view_command
    ;

dml_command
    : select_command
    | upsert_values_command
    | upsert_select_command
    | delete_command
    ;

other_command
    : use_command
    | explain_command
    | update_statistics_command
    | declare_cursor_command
    | fetch_next_command
    | close_command
    | open_cursor_command
    | grant_command
    | revoke_command
    ;

alter_command
    : ALTER (TABLE | VIEW)? table_ref
      (
          ADD if_not_exists? column_def_list options_?
        | DROP COLUMN if_exists? column_ref_list
        | SET options_
      )
    ;

alter_index_command
    : ALTER INDEX if_exists? index_name ON table_ref (DISABLE | REBUILD | UNUSABLE | USABLE)
    ;

create_function_command
    : CREATE TEMPORARY? FUNCTION func_name '(' func_argument_list ')' RETURNS data_type AS class_name (USING JAR jar_path)?
    ;

create_index_command
    : CREATE LOCAL? INDEX if_not_exists? index_name ON table_ref '(' expression_asc_desc_list ')'
        (INCLUDE '(' column_ref_list ')')?
        ASYNC?
        index_options?
        (SPLIT ON split_point_list)?
    ;

create_schema_command
    : CREATE SCHEMA if_not_exists? schema_name
    ;

create_sequence_command
    : CREATE SEQUENCE if_not_exists? sequence_ref
        (START WITH? bind_parameter_number)?
        (INCREMENT BY? bind_parameter_number)?
        (MINVALUE bind_parameter_number)?
        (MAXVALUE bind_parameter_number)?
        CYCLE?
        (CACHE bind_parameter_number)?
    ;

create_table_command
    : CREATE TABLE if_not_exists? table_ref '(' column_def_list constraint? ')'
        table_options?
        (SPLIT ON '(' split_point_list ')')?
    ;

create_view_command
    : CREATE VIEW if_not_exists? new_table_ref ('(' column_def_list ')')?
        (AS SELECT '*' FROM existing_table_ref (WHERE expression)? )?
        table_options?
    ;

constraint
    : CONSTRAINT constraint_name PRIMARY KEY '(' constraint_column_list ')'
    ;

constraint_column_list
    : constraint_column (',' constraint_column)*
    ;

constraint_column
    : column_name asc_desc? ROW_TIMESTAMP?
    ;

constraint_name
    : name
    ;

new_table_ref
    : table_ref
    ;

table_options
    : options_
    ;

existing_table_ref
    : table_ref
    ;

expression_asc_desc_list
    : expression asc_desc? (',' expression asc_desc?)*
    ;

split_point_list
    : split_point (',' split_point)*
    ;

split_point
    : value
    | bind_parameter
    ;

index_options
    : options_list
    ;

options_list
    : options_+
    ;

options_
    : option (',' option)*
    ;

option
    : (family_name '.')? name '=' value
    ;

func_argument_list
    : func_argument (',' func_argument)*
    ;

func_argument
    : data_type CONSTANT? (DEFAULTVALUE '=' string)? (MINVALUE '=' string)? (MAXVALUE '=' string)?
    ;

class_name
    : string
    ;

jar_path
    : string
    ;

drop_table_command
    : DROP TABLE if_exists? table_ref CASCADE?
    ;

drop_view_command
    : DROP VIEW if_exists? table_ref CASCADE?
    ;

drop_schema_command
    : DROP SCHEMA if_exists? schema_name
    ;

drop_sequence_command
    : DROP SEQUENCE if_exists? sequence_ref
    ;

drop_index_command
    : DROP INDEX if_exists? index_name ON table_ref
    ;

drop_function_command
    : DROP FUNCTION if_exists? func_name
    ;

index_name
    : name
    ;

func_name
    : name
    ;

if_exists
    : IF EXISTS
    ;

if_not_exists
    : IF NOT EXISTS
    ;

table_ref
    : (schema_name '.')? table_name
    ;

use_command
    : USE (schema_name | DEFAULT)
    ;

explain_command
    : EXPLAIN (select_command | upsert_select_command | delete_command)
    ;

update_statistics_command
    : UPDATE STATISTICS table_ref (ALL | INDEX | COLUMN)? (SET guide_post_options)?
    ;

declare_cursor_command
    : DECLARE CURSOR cursor_name FOR select_statement
    ;

open_cursor_command
    : OPEN CURSOR cursor_name
    ;

fetch_next_command
    : FETCH NEXT (number ROWS)? FROM cursor_name
    ;

close_command
    : CLOSE cursor_name
    ;

grant_command
    : GRANT permission_string on_schema_table? TO GROUP? user_string
    ;

revoke_command
    : REVOKE on_schema_table? FROM GROUP? user_string
    ;

on_schema_table
    : ON (SCHEMA schema_name)? table_name
    ;

permission_string
    : string
    ;

user_string
    : string
    ;

cursor_name
    : name
    ;

guide_post_options
    : (family_name '.')? name '=' value (',' name '=' value)*
    ;

upsert_values_command
    : UPSERT INTO table_name ('(' column_ref_list | column_def_list ')')?
        VALUES '(' constant_term (',' constant_term)? ')'
        (ON DUPLICATE KEY (IGNORE | UPDATE column_ref '=' operand))?
    ;

column_ref_list
    : column_ref (',' column_ref)*
    ;

column_def_list
    : column_def (',' column_def)*
    ;

upsert_select_command
    : UPSERT hint? INTO table_name ('(' column_ref_list | column_def_list ')')? select_command //TODO hint
    ;

constant_term
    : term
    ;

delete_command
    : DELETE hint? FROM table_name //TODO hint
        where_clause?
        order_by_clause?
        limit_clause?
    ;

order_by_clause
    : ORDER BY order_list
    ;

limit_clause
    : LIMIT bind_parameter_number
    ;

order_list
    : order (',' order)*
    ;

where_clause
    : WHERE expression
    ;

select_command
    : select_statement union_list?
      order_by_clause?
      limit_clause?
      (OFFSET bind_parameter_number row_rows?)?
      (FETCH first_next bind_parameter_number row_rows? ONLY)?
    ;

select_statement
    : SELECT hint? (DISTINCT | ALL)? select_expression (',' select_expression)* //TODO hint
      FROM table_spec join_list?
      where_clause?
      (GROUP BY expression (',' expression)* )?
      (HAVING expression)?
    ;

union_list
    : union+
    ;

union
    : UNION ALL select_statement
    ;

join_list
    : join_item+
    ;

join_item
    : join_type? JOIN table_spec ON expression
    ;

row_rows
    : ROW
    | ROWS
    ;

bind_parameter_number
    : bind_parameter
    | number
    ;

order
    : expression asc_desc? (NULLS first_last)?
    ;

first_last
    : FIRST
    | LAST
    ;

first_next
    : FIRST
    | NEXT
    ;

bind_parameter
    : '?'
    | ':' number
    ;

number
    : DECIMAL_LITERAL
    ;

hint
    : scan_hint
    | index_hint
    | cache_hint
    | small_hint
    | join_hint
    | seek_to_column_hint
    | serial_hint
    ;

scan_hint
    : SKIP_SCAN
    | RANGE_SCAN
    ;

index_hint
    : INDEX
    | NO_INDEX
    | USE_INDEX_OVER_DATA_TABLE
    | USE_DATA_OVER_INDEX_TABLE
    ;

cache_hint
    : NO_CACHE
    ;

small_hint
    : SMALL
    ;

join_hint
    : USE_SORT_MERGE_JOIN
    | NO_STAR_JOIN
    | NO_CHILD_PARENT_JOIN_OPTIMIZATION
    ;

seek_to_column_hint
    : SEEK_TO_COLUMN
    | NO_SEEK_TO_COLUMN
    ;

serial_hint
    : SERIAL
    ;

select_expression
    : '*'
    | '(' family_name '.' '*' ')'
    | expression (AS? column_alias)?
    ;

family_name
    : name
    ;

quoted_name
    : DOUBLE_QUOTE_ID
    ;

column_alias
    : alias
    ;

alias
    : name
    ;

name
    : ID
    | quoted_name
    ;

table_spec
    : aliased_table_ref
    | '(' select_command')' (AS? table_alias)?
    ;

aliased_table_ref
    : table_ref (AS? table_alias)? ('(' column_def (',' column_def)? ')')? (TABLESAMPLE '(' positive_decimal ')')?
    ;

table_alias
    : alias
    ;

positive_decimal
    : number
    ;

schema_name
    : name
    ;

table_name
    : name
    ;

column_def
    : column_ref data_type (NOT? NULL)? (DEFAULT constant_operand)? (PRIMARY KEY asc_desc? ROW_TIMESTAMP? )?
    ;

column_ref
    : (family_name '.')? column_name
    ;

column_name
    : name
    ;

data_type
    : (sql_data_type | hbase_data_type) (ARRAY ('[' dimension_int ']')?)?
    ;

sql_data_type
    : char_type
    | varchar_type
    | decimal_type
    | tinyint_type
    | smallint_type
    | integer_type
    | bigint_type
    | float_type
    | double_type
    | timestamp_type
    | date_type
    | time_type
    | binary_type
    | varbinary_type
    ;

hbase_data_type
    : unsigned_timestamp_type
    | unsigned_date_type
    | unsigned_time_type
    | unsigned_tinyint_type
    | unsigned_smallint_type
    | unsigned_int_type
    | unsigned_long_type
    | unsigned_float_type
    | unsigned_double_type
    ;

asc_desc
    : ASC
    | DESC
    ;

join_type
    : INNER
    | (LEFT | RIGHT) OUTER?
    ;

constant_operand
    : operand
    ;

operand
    : summand ('||' summand)*
    ;

summand
    : factor ( ('+'|'-') factor )*
    ;

factor
    : term ( ('*'|'/'|'%') term )*
    ;

term
    : (
        value
        | '(' expression ')'
        | bind_parameter
        | function_
        | case
        | case_when
        | (table_alias '.')? column_ref
        | row_value_constructor
        | cast
        | sequence
        | array_constructor
    ) ('[' expression ']')?
    ;

expression
    : and_condition (OR and_condition)* // FIXME
    ;

and_condition
    : boolean_condition (AND boolean_condition)*
    ;

boolean_condition
    : NOT? condition
    ;

condition
    : operand ('='|'<'|'>'|'<='|'>='|'<>'|'!=') rhs_operand
    | (LIKE | ILIKE) operand
    | ID NOT? NULL
    | NOT? (
          IN '(' (select_command | (constant_operand (',' constant_operand)?) ) ')'
        | EXISTS '(' select_command ')'
        | BETWEEN operand AND operand
    )
    ;

rhs_operand
    : operand
    | any_all '(' operand | select_command ')'
    ;

any_all
    : ANY
    | ALL
    ;

value
    : string
    | numeric
    | boolean
    | null_
    | ID
    ;

string
    : STRING_LITERAL
    ;

numeric
    : int
    | long
    | decimal
    ;

int
    : '-'? number
    ;

long
    : '-'? number
    ;

decimal
    : '-'? number ('.' number)?
    ;

boolean
    : TRUE
    | FALSE
    ;

null_
    : NULL
    ;

case
    : CASE term
        WHEN expression THEN term
        (WHEN expression THEN term)*
        (ELSE expression)?
      END
    ;

case_when
    : CASE WHEN expression THEN term
        (WHEN expression THEN term)?
        (ELSE term)?
      END
    ;

row_value_constructor
    : '(' expression (',' expression)* ')'
    ;

cast
    : CAST '(' expression AS data_type ')'
    ;

sequence
    : (NEXT | CURRENT) (VALUE | number VALUES) FOR sequence_ref
    ;

sequence_ref
    : (schema_name '.')? sequence_name
    ;

sequence_name
    : name
    ;

array_constructor
    : ARRAY '[' expression (',' expression)* ']'
    ;

function_
    : aggregate_functions
    ;

aggregate_functions
    : AVG
    | COUNT
    | APPROX_COUNT_DISTINCT
    | MAX
    | MIN
    | SUM
    | PERCENTILE_CONT
    | PERCENTILE_DISC
    | PERCENT_RANK
    | FIRST_VALUE
    | LAST_VALUE
    | FIRST_VALUES
    | LAST_VALUES
    | NTH_VALUE
    | STDDEV_POP
    | STDDEV_SAMP
    ;

dimension_int
    : int
    ;

precision_int
    : int
    ;

scale_int
    : int
    ;

char_type
    : CHAR '(' precision_int ')'
    ;

varchar_type
    : VARCHAR '(' precision_int ')'
    ;

decimal_type
    : DECIMAL (precision_int ',' scale_int)?
    ;

tinyint_type
    : TINYINT
    ;

smallint_type
    : SMALLINT
    ;

integer_type
    : INT
    ;

bigint_type
    : BIGINT
    ;

float_type
    : FLOAT
    ;

double_type
    : DOUBLE
    ;

timestamp_type
    : TIMESTAMP
    ;

date_type
    : DATE
    ;

time_type
    : TIME
    ;

binary_type
    : BINARY '(' precision_int ')'
    ;

varbinary_type
    :  VARBINARY '(' precision_int ')'
    ;

unsigned_timestamp_type
    : UNSIGNED_TIMESTAMP
    ;

unsigned_date_type
    : UNSIGNED_DATE
    ;

unsigned_time_type
    : UNSIGNED_TIME
    ;

unsigned_tinyint_type
    : UNSIGNED_TINYINT
    ;

unsigned_smallint_type
    : UNSIGNED_SMALLINT
    ;

unsigned_int_type
    : UNSIGNED_INT
    ;

unsigned_long_type
    : UNSIGNED_LONG
    ;

unsigned_float_type
    : UNSIGNED_FLOAT
    ;

unsigned_double_type
    : UNSIGNED_DOUBLE
    ;
