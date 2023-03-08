/*
Apache Drill grammar.
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

parser grammar DrillParser;

options { tokenVocab=DrillLexer; }

drill_file
    : batch* EOF
    ;

batch
    : sql_command SEMI?
    ;

sql_command
    : ddl_command
    | other_command
    ;

ddl_command
    : alter_command
    | create_command
    | drop_command
    ;

create_command
    : create_schema
    | create_table
    | create_temp_table
    | create_function
    | create_view
    ;

create_schema
    : CREATE or_replace? SCHEMA
        (LOAD string)?
        ( '(' column_name data_type nullability? format? default? props? ')')?
        (FOR TABLE id_ | PATH string)?
        (PROPERTIES '(' string ASSIGN string (',' string ASSIGN string)* ')')?
    ;

create_table
    : CREATE TABLE name column_list_paren? AS query
    ;

column_list_paren
    : '(' column_list ')'
    ;

column_list
    : column_name (',' column_name)*
    ;

create_temp_table
    : CREATE TEMPORARY TABLE name column_list_paren? partition_by_clause? AS query
    ;

partition_by_clause
    : PARTITION BY column_list_paren
    ;

create_function
    : CREATE FUNCTION USING JAR string
    ;

create_view
    : CREATE or_replace? VIEW (workspace '.')? view_name column_list_paren? AS query
    ;

alter_command
    : alter_system
    ;

alter_system
    : ALTER SYSTEM ( SET id_ ASSIGN value
                   | RESET (id_ | ALL)
                   )
    ;

id_
    : BS_STRING_LITERAL
    ;

string
    : SQ_STRING_LITERAL
    ;

drop_command
    : drop_table
    | drop_view
    | drop_function
    ;

drop_table
    : DROP TABLE if_exists? (workspace '.')? name
    ;

drop_view
    : DROP VIEW if_exists? (workspace '.')? view_name
    ;

drop_function
    : DROP FUNCTION USING JAR string
    ;

other_command
    : set_command
    | reset_command
    | refresh_table_metadata
    | describe_command
    | show_command
    | use_command
    | select_stmt
    ;

set_command
    : (ALTER SESSION)? SET id_ ASSIGN value
    ;

reset_command
    : (ALTER SESSION)? RESET id_
    ;

refresh_table_metadata
    : REFRESH TABLE METADATA (COLUMNS column_list_paren | NONE )? table_path
    ;

describe_command
    : DESCRIBE (workspace '.')? (table_name | view_name) column_name?
    | DESCRIBE (SCHEMA | DATABASE) name ('.' workspace)?
    | DESCRIBE '(' query ')'
    ;

show_command
    : SHOW (TABLES | DATABASES | SCHEMAS)
    | SHOW FILES (FROM | IN )?
    ;

use_command
    : USE schema_name
    ;

select_stmt
    : with_clause
        select_list
        from_clause
        where_clause
        group_by_clause
        having_clause
        order_by_clause?
        limit_clause
        offset_clause
    ;

with_clause
    : WITH
    ;

select_list
    : SELECT DISTINCT? select_item (',' select_item)*
    ;

select_item
    : ( COLUMNS '[' number ']'
      | '*'
      | expression ) (AS column_alias)?
    ;

from_clause
    : FROM table_reference
    ;

table_reference
    : todo
    ;

where_clause
    : WHERE boolean_expression
    ;

boolean_expression
    : todo
    ;

expression
    : literal
    ;

literal
    : id_
    ;

expression_list
    : expression (',' expression)*
    ;

group_by_clause
    : GROUP BY expression_list
    ;

having_clause
    : HAVING
    ;

order_by_clause
    : ORDER BY expression (ASC | DESC)? (NULLS (FIRST | LAST))?
    ;

limit_clause
    : LIMIT
    | FETCH NEXT
    ;

offset_clause
    : OFFSET start (ROW | ROWS)
    ;

start
    : number
    ;

number
    : DECIMAL_LITERAL
    ;

query
    : SELECT
    ;

data_type
    : BIGINT
    | BINARY
    | BOOLEAN
    | DATE
    | DECIMAL
    | DEC
    | NUMERIC
    | FLOAT
    | DOUBLE PRECISION?
    | INTEGER
    | INT
    | INTERVAL
    | SMALLINT
    | TIME
    | TIMESTAMP
    | CHARACTER VARYING
    | CHAR
    | VARCHAR
    ;

default
    : todo
    ;

nullability
    : todo
    ;

format
    : todo
    ;

props
    : todo
    ;

or_replace
    : OR REPLACE
    ;

if_exists
    : IF EXISTS
    ;

workspace
    : name
    ;

name
    : ID
    ;

schema_name
    : name
    ;

table_name
    : name
    ;

view_name
    : name
    ;

column_name
    : name
    ;

column_alias
    : name
    ;

table_path
    : todo
    ;

value
    : todo
    ;

todo
    : ';'
    ;