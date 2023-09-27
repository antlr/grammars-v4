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
        ('(' column_definition (',' column_definition)* ')')?
        (FOR TABLE table_name | PATH string)?
        (PROPERTIES '(' kv_list ')')?
    ;

column_definition
    : column_name data_type nullability? format_clause? default_clause? properties_clause?
    ;

kv_list
    : kv_pair (',' kv_pair)*
    ;

kv_pair
    : string '=' string
    ;

create_table
    : CREATE TABLE table_name column_list_paren? AS query
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
    : ALTER SYSTEM ( SET option_name '=' value
                   | RESET (option_name | ALL)
                   )
    ;

option_name
    : BS_STRING_LITERAL
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
    | analyze_command
    | reset_command
    | refresh_table_metadata
    | describe_command
    | show_command
    | use_command
    | select_stmt
    ;

set_command
    : (ALTER SESSION)? SET option_name '=' value
    ;

reset_command
    : (ALTER SESSION)? RESET id_
    ;

refresh_table_metadata
    : REFRESH TABLE METADATA (COLUMNS column_list_paren | NONE)? table_name
    ;

analyze_command
    : ANALYZE TABLE (table_name | TABLE '(' id_ '(' param_list ')' ')')?
      (COLUMNS (column_list_paren | NONE))?
      (REFRESH METADATA (string LEVEL)?)?
      ((COMPUTE | ESTIMATE) STATISTICS (SAMPLE number PERCENT)?)?
    ;

param_list
    : expr_list
    ;

describe_command
    : DESCRIBE (workspace '.')? (table_name | view_name) column_name?
    | DESCRIBE (SCHEMA | DATABASE) name ('.' workspace)?
    | DESCRIBE '(' query ')'
    ;

show_command
    : SHOW (TABLES | DATABASES | SCHEMAS)
    | SHOW FILES ((FROM | IN ) fs=id_ '.' dir=id_)?
    ;

use_command
    : USE schema_name
    ;

select_stmt
    : with_clause?
        select_clause
        from_clause?
        where_clause?
        group_by_clause?
        having_clause?
        order_by_clause?
        limit_clause?
        offset_clause?
    ;

with_clause
    : WITH with_item (',' with_item)*
    ;

with_item
    : name column_list_paren? AS query
    ;

select_clause
    : SELECT DISTINCT? select_item (',' select_item)*
    ;

select_item
    : ( COLUMNS '[' number ']'
      | ((table_name | column_alias) '.')? '*'
      | expression
      ) (AS? column_alias)?
    ;

from_clause
    : FROM table_expression (',' table_expression)*
    ;

table_expression
    : with_clause correlation_clause?
    | table_name correlation_clause?
    | table_subquery correlation_clause?
    | join_clause
    | LATERAL? lateral_join_type? lateral_subquery (ON TRUE)?
    ;

lateral_join_type
    : (INNER? | LEFT OUTER?) JOIN
    ;

lateral_subquery
    : unnest_table_expr
    | '(' select_clause FROM unnest_table_expr (',' select_clause FROM unnest_table_expr)* ')'
    ;

join_clause
    : table_reference join_type table_reference (ON boolean_expression)?
    ;

join_type
    : ( INNER?
      | (LEFT | RIGHT | FULL) OUTER?
      | CROSS
      ) JOIN
    ;

table_reference
    : table_name correlation_clause?
    ;

unnest_table_expr
    : UNNEST '(' expression ')' correlation_clause
    ;

correlation_clause
    : AS? correlation_name column_list_paren?
    ;

where_clause
    : WHERE boolean_expression
    ;

boolean_expression
    : expression AND expression
    | expression OR expression
    | NOT+ expression
    | expression comparison_operator expression
    | expression IS NOT? NULL_
    | string NOT? LIKE string (ESCAPE string)?
    | expression NOT? BETWEEN expression AND expression
    | expression NOT? IN table_subquery
    | expression NOT? IN '(' expr_list ')'
    | NOT? EXISTS table_subquery
    | expression comparison_operator (ALL | ANY | SOME) table_subquery
    ;

table_subquery
    : '(' query
          order_by_clause?
          offset_clause?
      ')'
    ;

expression
    : primitive_expression
    | '(' expression ')'
    | table_subquery
    | function_call
//    | case_expression
    | op=('+' | '-') expression
    | expression op=(STAR | DIVIDE | MODULE) expression
    | expression op=(PLUS | MINUS ) expression
    | expression DOT expression
    | expression comparison_operator expression
    | expression AND expression
    | expression OR expression
    | NOT expression
    | expression NOT? IN '(' expr_list ')'
    | CAST '(' expression AS data_type ')'
    ;

primitive_expression
    : literal
    | id_
    ;

literal
    : TRUE
    | FALSE
    | NULL_
    | SQ_STRING_LITERAL
    | DECIMAL_LITERAL
    | FLOAT_LITERAL
    | REAL_LITERAL
    ;

function_call
    : function_name '(' expr_list? ')'
//    | standard_built_in_function '(' expr_list? ')'
//    | aggreagate_built_in_function '(' expr_list? ')'
    ;

comparison_operator
    : '<' | '=' | '>' | '<=' | '>=' | '<>'
    ;

expr_list
    : expression (',' expression)*
    ;

group_by_clause
    : GROUP BY expr_list
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
    : OFFSET number (ROW | ROWS)
    ;

number
    : DECIMAL_LITERAL
    ;

query
    : '(' query
            order_by_clause?
            offset_clause?
        ')'
    | query UNION ALL? query
    | select_stmt
    ;

select_expression
    : select_clause
        from_clause?
        where_clause?
        group_by_clause?
        having_clause?
        order_by_clause?
        offset_clause?
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
    | INTERVAL (YEAR | MONTH | DAY | HOUR | MINUTE | SECOND)
    | SMALLINT
    | TIME
    | TIMESTAMP
    | CHARACTER VARYING
    | CHAR
    | VARCHAR
    ;

default_clause
    : DEFAULT string
    ;

nullability
    : NOT? NULL_
    ;

format_clause
    : FORMAT string
    ;

properties_clause
    : PROPERTIES '{' kv_list '}'
    ;

or_replace
    : OR REPLACE
    ;

if_exists
    : IF EXISTS
    ;

id_
    : IDENTIFIER
    ;

string
    : SQ_STRING_LITERAL
    ;

workspace
    : name
    ;

name
    : IDENTIFIER
    ;

schema_name
    : (id_ '.')? name
    ;

table_name
    : ((id_ '.')? id_ '.')? (name | table_path)
    ;

view_name
    : name
    ;

correlation_name
    : name
    ;

column_name
    : name
    ;

function_name
    : name
    ;

column_alias
    : name
    | BS_STRING_LITERAL
    ;

table_path
    : BS_STRING_LITERAL
    ;

value
    : literal
    ;
