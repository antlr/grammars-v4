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
    : literal
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
    : (family_name '.')? name '=' literal
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
    : ON (SCHEMA schema_name | table_ref )
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
    : (family_name '.')? name '=' literal (',' name '=' literal)*
    ;

upsert_values_command
    : UPSERT INTO table_name ('(' (column_ref_list | column_def_list) ')')?
        VALUES '(' literal (',' literal)* ')'
        (ON DUPLICATE KEY (IGNORE | UPDATE column_ref '=' expression))?
    ;

column_ref_list
    : column_ref (',' column_ref)*
    ;

column_def_list
    : column_def (',' column_def)*
    ;

upsert_select_command
    : UPSERT hint? INTO table_name ('(' (column_ref_list | column_def_list) ')')? select_command //TODO hint
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
    : '/*+' hint_name '*/'
    ;

hint_name
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
    | family_name '.' '*'
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
    : column_ref data_type (NOT? NULL_)? (DEFAULT literal)? (PRIMARY KEY asc_desc? ROW_TIMESTAMP? )?
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

asc_desc
    : ASC
    | DESC
    ;

any_all
    : ANY
    | ALL
    ;

join_type
    : INNER
    | (LEFT | RIGHT) OUTER?
    ;

expression
    : literal
    | bind_parameter
    | ((schema_name DOT)? table_name DOT)? column_name
    | MINUS expression
    | expression PIPEPIPE expression
    | expression (STAR | DIV | MOD) expression
    | expression (PLUS | MINUS) expression
    | expression comp_op expression
    | expression comp_op any_all LP (select_command) RP
    | expression (LIKE | ILIKE) expression
    | expression IS NOT? NULL_
    | expression NOT? IN LP (select_command | expression_list ) RP
    | expression NOT? BETWEEN expression AND expression
    | NOT? EXISTS LP select_command RP
    | ID LP expression_list RP
    | NOT expression
    | expression AND expression
    | expression OR expression
    | row_value_constructor
    | case
    | case_when
    | cast
    | sequence
    | array_constructor
    ;

comp_op
    : EQ | GT | GE | LT | LE | NE | NE2
    ;

expression_list
    : expression (COMMA expression)*
    ;

literal
    : string
    | numeric
    | true_false
    | NULL_
    ;

string
    : STRING_LITERAL
    ;

numeric
    : integer
    | decimal
    ;

integer
    : '-'? DECIMAL_LITERAL
    ;

decimal
    : '-'? number ('.' number)?
    ;

true_false
    : TRUE
    | FALSE
    ;

case
    : CASE expression
        WHEN expression THEN expression
        (WHEN expression THEN expression)*
        (ELSE expression)?
      END
    ;

case_when
    : CASE WHEN expression THEN expression
        (WHEN expression THEN expression)?
        (ELSE expression)?
      END
    ;

row_value_constructor
    : '(' expression_list ')'
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

dimension_int
    : integer
    ;

precision_int
    : integer
    ;

scale_int
    : integer
    ;

sql_data_type
    : CHAR '(' precision_int ')'
    | VARCHAR ('(' precision_int ')')?
    | DECIMAL (precision_int ',' scale_int)?
    | TINYINT
    | SMALLINT
    | INTEGER
    | BIGINT
    | FLOAT
    | DOUBLE
    | TIMESTAMP
    | DATE
    | TIME
    | BINARY '(' precision_int ')'
    | VARBINARY '(' precision_int ')'
    ;

hbase_data_type
    : UNSIGNED_TIMESTAMP
    | UNSIGNED_DATE
    | UNSIGNED_TIME
    | UNSIGNED_TINYINT
    | UNSIGNED_SMALLINT
    | UNSIGNED_INT
    | UNSIGNED_LONG
    | UNSIGNED_FLOAT
    | UNSIGNED_DOUBLE
    ;
