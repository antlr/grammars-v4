/*
AWS Athena grammar.
The MIT License (MIT).

Copyright (c) 2023, Michał Lorek.

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

parser grammar AthenaParser;

options { tokenVocab=AthenaLexer; }

stmt
    : command SEMI? EOF
    ;

command
    : ddl_command
    | dml_command
    ;

ddl_command
    : alter_database
    | alter_table_add_cols
    | alter_table_add_part
    | alter_table_drop_part
    | alter_table_rename_part
    | alter_table_replace_part
    | alter_table_set_location
    | alter_table_set_props
    | create_database
    | create_table
    | create_table_as
    | create_view
    | drop_database
    | drop_table
    | drop_view
    | describe
    | describe_view
    | msck
    | show_columns
    | show_create_table
    | show_create_view
    | show_databases
    | show_partitions
    | show_tables
    | show_tblproperties
    | show_views
    ;

dml_command
    : select
    | insert_into
    | delete_stmt
    | update
    | merge_into
    | optimize_stmt
    | vacuum
    | explain
    | prepare
    | execute
    | deallocate
    | unload
    ;

/**
DML
*/
select
    : (WITH with_query (',' with_query)*)?
        select_statement
    ;

select_statement
    : SELECT all_distinct? select_list
        (FROM from_item (',' from_item)*)?
        (WHERE condition)?
        (GROUP BY all_distinct? grouping_element (','grouping_element)*)?
        (HAVING condition)?
        ((UNION | INTERSECT | EXCEPT) all_distinct? select_statement)?
        (ORDER BY order_item (',' order_item)*)?
        (OFFSET count (ROW | ROWS)?)?
        (LIMIT (count | ALL))?
    ;

all_distinct
    : ALL
    | DISTINCT
    ;

order_item
    : expression (ASC|DESC)? (NULLS (FIRST|LAST))?
    ;

from_item
    : table_name
    ;

count
    : int_number
    ;

with_query
    : id_ AS '(' select_statement ')'
    ;

grouping_element
    : expression
    ;

condition
    : boolean_expression
    ;

insert_into
    : INSERT INTO destination_table ('(' column_list ')')?
        ( select_statement
        | VALUES value_list (',' value_list)*
        )
    ;

value_list
    : '(' value (',' value)* ')'
    ;

select_list
    : select_item (',' select_item)*
    ;

select_item
    : expression (AS? alias)?
    | (table_name '.')?'*'
    ;

delete_stmt
    : DELETE FROM (db_name'.')? table_name (WHERE predicate)?
    ;

update
    : UPDATE (db_name'.')? table_name
        SET col_name '=' expression (',' col_name '=' expression)*
        (WHERE predicate)?
    ;

merge_into
    : MERGE INTO target_table (AS? target_alias)?
        USING (source_table | query) (AS? source_alias)?
        ON search_condition
        when_clauses
    ;

search_condition
    : predicate
    ;

when_clauses
    : (when_matched_and_clause | when_matched_then_clause)* when_not_matched_clause?
    ;

when_not_matched_clause
    : WHEN NOT MATCHED (AND expression)? THEN INSERT column_list? VALUES '(' expression_list ')'
    ;

expression_list
    : expression (',' expression)*
    ;

column_list
    : col_name (',' col_name)*
    ;

when_matched_and_clause
    : WHEN MATCHED AND expression THEN update_delete
    ;

when_matched_then_clause
    : WHEN MATCHED THEN update_delete
    ;

update_delete
    : UPDATE SET '(' col_name '=' expression (',' col_name '=' expression)* ')'
    | DELETE
    ;

optimize_stmt
    : OPTIMIZE (db_name '.')? table_name REWRITE DATA USING BIN_PACK (WHERE predicate)?
    ;

vacuum
    : VACUUM target_table
    ;

target_table
    : table_name
    ;

source_table
    : table_name
    ;

explain
    : EXPLAIN statement ('(' explain_option (',' explain_option)* ')')?
    | EXPLAIN ANALYZE ('(' FORMAT (TEXT | JSON) ')')? statement
    ;

explain_option
    : FORMAT (TEXT | GRAPHVIZ | JSON)
    | TYPE (LOGICAL | DISTRIBUTED | VALIDATE | IO)
    ;

prepare
    : PREPARE statement_name FROM statement
    ;

statement
    : select
    | create_table_as
    | insert_into
    | unload
    ;

execute
    : EXECUTE statement_name (USING parameter (',' parameter)*)?
    ;

parameter
    : value
    ;

value
    : int_number
    | string
    | true_false
    ;

deallocate
    : DEALLOCATE PREPARE statement_name
    ;

unload
    : UNLOAD '(' select ')'
        TO string
        WITH '(' property_list ')'
    ;

property_list
    : property_name '=' property_value (',' property_name '=' property_value)*
    ;

property_value
    : value
    ;

predicate
    : true_false
    | boolean_expression
    ;

/**
DDL
*/
alter_database
    : ALTER db_schema database_name SET DBPROPERTIES '(' kv_pair (',' kv_pair)* ')'
    ;

db_schema
    : DATABASE
    | SCHEMA
    ;

kv_pair
    : string '=' (string | DQ_STRING_LITERAL)
    ;

alter_table_add_cols
    : ALTER TABLE table_name (PARTITION '(' part_col_name_value (',' part_col_name_value)* ')')?
        ADD COLUMNS (col_name data_type)
    ;

part_col_name_value
    : partition_col_name '=' partition_col_value
    ;

partition_col_name
    : col_name
    ;

partition_col_value
    : value
    ;

alter_table_add_part
    : ALTER TABLE table_name ADD if_not_exists?
        (PARTITION '(' part_col_name_value (',' part_col_name_value)* ')' (LOCATION string)?)+
    ;

alter_table_drop_part
    : ALTER TABLE table_name DROP if_exists? PARTITION '('partition_spec')' (',' PARTITION '('partition_spec')')*
    ;

partition_spec
    : part_col_name_value (',' part_col_name_value)*
    ;

alter_table_rename_part
    : ALTER TABLE table_name PARTITION (partition_spec) RENAME TO PARTITION (np=partition_spec)
    ;

alter_table_replace_part
    : ALTER TABLE table_name
        (PARTITION '('part_col_name_value (',' part_col_name_value)* ')')?
        REPLACE COLUMNS '('col_name data_type (',' col_name data_type)* ')'
    ;

alter_table_set_location
    : ALTER TABLE table_name (PARTITION '('partition_spec')')? SET LOCATION string
    ;

alter_table_set_props
    : ALTER TABLE table_name SET TBLPROPERTIES '(' kv_pair (',' kv_pair)* ')'
    ;

create_database
    : CREATE db_schema if_not_exists? database_name
      (COMMENT string)?
      (LOCATION string)?
      (WITH DBPROPERTIES '(' kv_pair (',' kv_pair)* ')')?
    ;

create_table
    : CREATE EXTERNAL TABLE if_not_exists?
     (db_name'.')? table_name ('(' col_def_with_comment (',' col_def_with_comment)* ')')?
     (COMMENT table_comment)?
     (PARTITIONED BY '('col_def_with_comment (',' col_def_with_comment)* ')')?
     (CLUSTERED BY '(' col_name (',' col_name)* ')' INTO num_buckets BUCKETS)?
     (ROW FORMAT row_format)?
     (STORED AS file_format)?
     LOCATION string
     (TBLPROPERTIES '(' property_list ')')?
    ;

table_comment
    : string
    ;

row_format
    : DELIMITED table_row_format_field_identifier? table_row_format_coll_items_identifier?
              table_row_format_map_keys_identifier? table_row_format_lines_identifier? table_row_null_format?
    | SERDE string (WITH SERDEPROPERTIES '(' property_list ')')?
    ;

table_row_format_field_identifier
    : FIELDS TERMINATED BY string (ESCAPED BY string)?
    ;

table_row_format_coll_items_identifier
    : COLLECTION ITEMS TERMINATED BY string
    ;

table_row_format_map_keys_identifier
    : MAP KEYS TERMINATED BY string
    ;

table_row_format_lines_identifier
    : LINES TERMINATED BY string
    ;

table_row_null_format
    : NULL_ DEFINED AS string
    ;

file_format
    : SEQUENCEFILE
    | TEXTFILE
    | RCFILE
    | ORC
    | PARQUET
    | AVRO
    | ION
    | INPUTFORMAT string OUTPUTFORMAT string
    ;

num_buckets
    : int_number
    ;

col_def_with_comment
    : col_name data_type (COMMENT col_comment)?
    ;

col_comment
    : string
    ;

create_table_as
    : CREATE TABLE table_name
    ( WITH '(' prop_exp (',' prop_exp)* ')')?
    AS query
    (WITH NO? DATA)?
    ;

property_name
    : id_
    ;

prop_exp
    : property_name '=' expression
    ;

create_view
    : CREATE or_replace? VIEW view_name AS query
    ;

describe
    : DESCRIBE (EXTENDED | FORMATTED)? (db_name '.')? table_name (PARTITION partition_spec)?
        //(col_name ( [.field_name] | [.'$elem$'] | [.'$key$'] | [.'$value$'] ) )? //TODO - poor documentation vs actual functionality
    ;

field_name
    : id_
    ;

describe_view
    : DESCRIBE view_name?
    ;

drop_database
    : DROP db_schema if_exists? database_name (RESTRICT | CASCADE)?
    ;

drop_table
    : DROP TABLE if_exists? table_name
    ;

drop_view
    : DROP VIEW if_exists? view_name
    ;

msck
    : MSCK REPAIR TABLE table_name
    ;

show_columns
    : SHOW COLUMNS from_in database_name '.' table_name
    | SHOW COLUMNS from_in table_name (from_in database_name)?
    ;

show_create_table
    : SHOW CREATE TABLE (db_name '.')? table_name
    ;

show_create_view
    : SHOW CREATE VIEW view_name
    ;

show_databases
    : SHOW (DATABASES | SCHEMAS) (LIKE reg_ex)?
    ;

show_partitions
    : SHOW PARTITIONS table_name
    ;

show_tables
    : SHOW TABLES (IN database_name)? reg_ex?
    ;

show_tblproperties
    : SHOW TBLPROPERTIES table_name ('(' string ')')?
    ;

show_views
    : SHOW VIEWS (IN database_name)? (LIKE reg_ex)?
    ;

query
    : select
    ;

true_false
    : TRUE
    | FALSE
    ;

boolean_expression
    : boolean_expression AND boolean_expression
    | boolean_expression OR boolean_expression
    | NOT* ('(' boolean_expression ')' | pred)
    ;

pred
    : expression comparison_operator expression
    | expression IS NOT? NULL_
    | id_ NOT? LIKE string
    | expression NOT? BETWEEN expression AND expression
    | expression NOT? IN table_subquery
    | expression NOT? IN '(' expression_list ')'
    | NOT? EXISTS table_subquery
    | expression comparison_operator (ALL | ANY | SOME) table_subquery
    ;

table_subquery
    : '(' select_statement ')'
    ;

comparison_operator
    : '<' | '=' | '>' | '<=' | '>=' | '<>' | '!='
    ;

expression
    : primitive_expression
    | '(' expression ')'
    | table_subquery
    | id_ '(' expression_list ')'
    | case_expression
    | when_expression
    | op=(PLUS | MINUS) expression
    | expression op=(STAR | DIVIDE | MODULE) expression
    | expression op=(PLUS | MINUS) expression
    | expression DOT expression
    | CAST '(' expression AS data_type ')'
    ;

case_expression
    : CASE expression
        (WHEN expression THEN expression)+
        (ELSE expression)?
        END
    ;

when_expression
    : CASE
        (WHEN expression THEN expression)+
        (ELSE expression)?
        END
    ;


primitive_expression
    : literal
    | id_
    ;

literal
    : number
    | string
    | true_false
    | NULL_
    ;

int_number
    : INTEGRAL_LITERAL
    ;

number
    : int_number
    | REAL_LITERAL
    | FLOAT_LITERAL
    ;

data_type
    : primitive_type
    | ARRAY '<' data_type '>'
    | MAP '<' primitive_type ',' data_type '>'
    | STRUCT '<' struct_col_def (',' struct_col_def)* '>'
    ;

primitive_type
    : BOOLEAN
    | TINYINT
    | SMALLINT
    | INT
    | INTEGER
    | BIGINT
    | DOUBLE
    | FLOAT
    | DECIMAL '(' precision ',' scale ')'
    | (CHAR | VARCHAR) '(' int_number ')'
    | STRING
    | BINARY
    | DATE
    | TIMESTAMP
    ;

precision
    : int_number
    ;

scale
    : int_number
    ;

struct_col_def
    : col_name ':' data_type (COMMENT col_comment)?
    ;

col_name
    : id_
    ;

db_name
    : id_
    ;

database_name
    : id_
    ;

statement_name
    : id_
    ;

table_name
    : id_
    ;

view_name
    : id_
    ;

destination_table
    : id_
    ;

string
    : SQ_STRING_LITERAL
    ;

reg_ex
    : string
    ;

alias
    : id_
    ;

target_alias
    : id_
    ;

source_alias
    : id_
    ;

id_
    : IDENTIFIER
    | DQ_STRING_LITERAL
    ;

if_not_exists
    : IF NOT EXISTS
    ;

if_exists
    : IF EXISTS
    ;

or_replace
    : OR REPLACE
    ;

from_in
    : FROM
    | IN
    ;
