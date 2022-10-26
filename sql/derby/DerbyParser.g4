/*
Apache Derby grammar.
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

parser grammar DerbyParser;

options { tokenVocab=DerbyLexer; }

derby_file
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
    | create_command
    | drop_command
    ;

dml_command
    : select_statement
    | insert_statement
    | update_statement
    | delete_statement
    | merge_statement
    | truncate_table
    ;

keyword
    : ADD
    | ALL
    | ALLOCATE
    | ALTER
    | AND
    | ANY
    | ARE
    | AS
    | ASC
    | ASSERTION
    | AT
    | AUTHORIZATION
    | AVG
    | BEGIN
    | BETWEEN
    | BIGINT
    | BIT
    | BOOLEAN
    | BOTH
    | BY
    | CALL
    | CASCADE
    | CASCADED
    | CASE
    | CAST
    | CHAR
    | CHARACTER
    | CHARACTER_LENGTH
    | CHECK
    | CLOSE
    | COALESCE
    | COLLATE
    | COLLATION
    | COLUMN
    | COMMIT
    | CONNECT
    | CONNECTION
    | CONSTRAINT
    | CONSTRAINTS
    | CONTINUE
    | CONVERT
    | CORRESPONDING
    | CREATE
    | CROSS
    | CURRENT
    | CURRENT_DATE
    | CURRENT_ROLE
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | CURRENT_USER
    | CURSOR
    | DEALLOCATE
    | DEC
    | DECIMAL
    | DECLARE
    | DEFAULT
    | DEFERRABLE
    | DEFERRED
    | DELETE
    | DESC
    | DESCRIBE
    | DIAGNOSTICS
    | DISCONNECT
    | DISTINCT
    | DOUBLE
    | DROP
    | ELSE
    | END
    | END_EXEC
    | ESCAPE
    | EXCEPT
    | EXCEPTION
    | EXEC
    | EXECUTE
    | EXISTS
    | EXPLAIN
    | EXTERNAL
    | FALSE
    | FETCH
    | FIRST
    | FLOAT
    | FOR
    | FOREIGN
    | FOUND
    | FROM
    | FULL
    | FUNCTION
    | GET
    | GETCURRENTCONNECTION
    | GLOBAL
    | GO
    | GOTO
    | GRANT
    | GROUP
    | HAVING
    | HOUR
    | IDENTITY
    | IMMEDIATE
    | IN
    | INDICATOR
    | INITIALLY
    | INNER
    | INOUT
    | INPUT
    | INSENSITIVE
    | INSERT
    | INT
    | INTEGER
    | INTERSECT
    | INTO
    | IS
    | ISOLATION
    | JOIN
    | KEY
    | LAST
    | LEADING
    | LEFT
    | LIKE
    | LOWER
    | LTRIM
    | MATCH
    | MAX
    | MIN
    | MINUTE
    | NATIONAL
    | NATURAL
    | NCHAR
    | NVARCHAR
    | NEXT
    | NO
    | NONE
    | NOT
    | NULL_
    | NULLIF
    | NUMERIC
    | OF
    | ON
    | ONLY
    | OPEN
    | OPTION
    | OR
    | ORDER
    | OUTER
    | OUTPUT
    | OVERLAPS
    | PAD
    | PARTIAL
    | PREPARE
    | PRESERVE
    | PRIMARY
    | PRIOR
    | PRIVILEGES
    | PROCEDURE
    | PUBLIC
    | READ
    | REAL
    | REFERENCES
    | RELATIVE
    | RESTRICT
    | REVOKE
    | RIGHT
    | ROLLBACK
    | ROWS
    | RTRIM
    | SCHEMA
    | SCROLL
    | SECOND
    | SELECT
    | SESSION_USER
    | SET
    | SMALLINT
    | SOME
    | SPACE
    | SQL
    | SQLCODE
    | SQLERROR
    | SQLSTATE
    | SUBSTR
    | SUBSTRING
    | SUM
    | SYSTEM_USER
    | TABLE
    | TEMPORARY
    | TIMEZONE_HOUR
    | TIMEZONE_MINUTE
    | TO
    | TRANSACTION
    | TRANSLATE
    | TRANSLATION
    | TRIM
    | TRUE
    | UNION
    | UNIQUE
    | UNKNOWN
    | UPDATE
    | UPPER
    | USER
    | USING
    | VALUES
    | VARCHAR
    | VARYING
    | VIEW
    | WHENEVER
    | WHERE
    | WINDOW
    | WITH
    | WORK
    | WRITE
    | XML
    | XMLEXISTS
    | XMLPARSE
    | XMLQUERY
    | XMLSERIALIZE
    | YEAR
    ;

insert_statement
    : INSERT INTO table_name
          ( '(' simple_column_name (COMMA simple_column_name )* ')' )*
                query order_by_clause?
                offset_clause?
                fetch_clause?
    ;

offset_clause
    : OFFSET ( integer_literal | '?' ) row_rows
    ;

fetch_clause
    : FETCH first_next ( integer_literal | '?' ) row_rows ONLY
    ;

first_next
    : FIRST | NEXT
    ;

row_rows
    : ROW | ROWS
    ;

integer_literal
    : num
    ;

merge_statement
    : MERGE INTO table_name ( AS? correlation_name )?
      USING table_name ( AS? correlation_name )?
      ON search_condition merge_when_clause*
    ;

merge_when_clause
    : merge_when_matched
    | merge_when_not_matched
    ;

merge_when_matched
    : WHEN MATCHED ( AND match_refinement )? THEN ( merge_update | DELETE )
    ;

merge_when_not_matched
    : WHEN NOT MATCHED ( AND match_refinement ) THEN merge_insert
    ;

merge_update
    : UPDATE SET column_name EQ value (COMMA column_name EQ value )*
    ;

merge_insert
    : INSERT ( '(' simple_column_name (COMMA simple_column_name )*  ')' )? VALUES '(' value (COMMA value )* ')'
    ;

search_condition
    : boolean_expression
    ;

match_refinement
    : boolean_expression
    ;

update_statement
    : UPDATE table_name ( AS? correlation_name )?
              SET column_name EQ value ( COMMA column_name EQ value )*
              where_clause?
    | UPDATE table_name
              SET column_name EQ value ( COMMA column_name EQ value )*
              WHERE CURRENT OF cursor_nName
    ;

where_clause
    : WHERE boolean_expression
    ;

boolean_expression
    : expression AND expression
    | expression OR expression
    | NOT expression
    | expression comparison_operator expression
    | expression IS NOT? NULL_
    | character_expression NOT? LIKE character_expression ( ESCAPE string )?
    | expression NOT? BETWEEN expression AND expression
    | expression NOT? IN table_subquery
    | expression NOT? IN '(' expression (COMMA expression )* ')'
    | NOT? EXISTS table_subquery
    | expression comparison_operator ( ALL | ANY | SOME ) table_subquery
    ;

table_subquery
    : '(' query
          order_by_clause?
          offset_clause?
          fetch_clause?
      ')'
    ;

character_expression
    : string
    ;

comparison_operator
    : '<' | '=' | '>' | '<=' | '>=' | '<>'
    ;

value
    : expression | DEFAULT
    ;

delete_statement
    : DELETE FROM table_name ( AS? correlation_name )?
              where_clause?
    | DELETE FROM table_name WHERE CURRENT OF cursor_nName
    ;

// other commands
other_command
    : CALL procedure_name '(' expr_list ')'
    | DECLARE GLOBAL TEMPORARY TABLE table_name
                     column_definition (COMMA column_definition )*
                     ( ON COMMIT ( DELETE | PRESERVE ) ROWS )?
                     NOT LOGGED ( ON ROLLBACK DELETE ROWS )?
    | GRANT privilege_type ON TABLE? ( table_name | view_name ) TO grantees
    | GRANT EXECUTE ON FUNCTION function_name TO grantees
    | GRANT EXECUTE ON PROCEDURE procedure_name TO grantees
    | GRANT USAGE ON SEQUENCE sequence_name TO grantees
    | GRANT USAGE ON TYPE type_name TO grantees
    | GRANT USAGE ON DERBY AGGREGATE aggregate_name TO grantees
    | GRANT role_name (COMMA role_name )* TO grantees
    | LOCK TABLE table_name IN (SHARE | EXCLUSIVE) MODE
    | RENAME COLUMN table_name DOT simple_column_name TO simple_column_name
    | RENAME INDEX index_name TO index_name
    | RENAME TABLE RENAME TABLE table_name TO table_name
    | REVOKE privilege_type ON TABLE? ( table_name | view_name ) FROM revokees
    | REVOKE EXECUTE ON FUNCTION function_name FROM revokees RESTRICT
    | REVOKE EXECUTE ON PROCEDURE procedure_name FROM revokees RESTRICT
    | REVOKE USAGE ON SEQUENCE sequence_name FROM revokees RESTRICT
    | REVOKE USAGE ON TYPE type_name FROM revokees RESTRICT
    | REVOKE USAGE ON DERBY AGGREGATE aggregate_name FROM revokees RESTRICT
    | REVOKE role_name (COMMA role_name )* FROM revokees
    | SET CONSTRAINTS constraint_name_list ( DEFERRED | IMMEDIATE )
    | SET CURRENT? ISOLATION EQ? isolation_level
    | SET ROLE ( role_name | string | '?' | NONE )
    | SET CURRENT? SCHEMA EQ? ( schema_name | USER | '?' | string )
    | SET CURRENT SQLID EQ? ( schema_name | USER | '?' | string )
    ;

grantees
    : grantee (COMMA grantee)*
    ;

grantee
    : authorization_identifier | role_name | PUBLIC
    ;

isolation_level
    : UR
    | DIRTY READ
    | READ UNCOMMITTED
    | CS
    | READ COMMITTED
    | CURSOR STABILITY
    | RS
    | RR
    | REPEATABLE READ
    | SERIALIZABLE
    | RESET
    ;

constraint_name_list
    : ALL
    | constraint_name (COMMA constraint_name )*
    ;

column_definition
    : simple_column_name data_type?
          //columnLevelConstraint*
        with_default*
//         columnLevelConstraint*
    ;

with_default
    : WITH? DEFAULT default_constant_expression
    | generated_column_spec
    | generation_clause
    ;

default_constant_expression
    : NULL_
    | CURRENT ( SCHEMA | SQLID )
    | USER | CURRENT_USER | SESSION_USER | CURRENT_ROLE
    | DATE
    | TIME
    | TIMESTAMP
    | CURRENT DATE | CURRENT_DATE
    | CURRENT TIME | CURRENT_TIME
    | CURRENT TIMESTAMP | CURRENT_TIMESTAMP
    | literal
    ;

generated_column_spec
    : GENERATED always_by_default AS IDENTITY
        ( '(' START WITH integer_constant
            | INCREMENT BY integer_constant
            | NO? CYCLE ')' )?
    ;

generation_clause
    : GENERATED ALWAYS AS '(' values_expression ')'
    ;

column_level_constraint
    : ( CONSTRAINT constraint_name )?
        (
           NOT NULL_
           | CHECK '(' search_condition ')'
           | PRIMARY KEY
           | UNIQUE
           | references_clause
        )
        constraint_characteristics?
    ;

table_level_constraint
    : ( CONSTRAINT constraint_name )?
        (
            CHECK '(' search_condition ')' |
            (
                PRIMARY KEY '(' simple_column_name_list ')' |
                UNIQUE '(' simple_column_name_list ')' |
                FOREIGN KEY '(' simple_column_name_list ')'
                    references_clause
            )
        ) constraint_characteristics?
    ;

references_clause
    : REFERENCES table_name ( '(' simple_column_name_list ')' )?
        ( ON DELETE ( NO ACTION | RESTRICT | CASCADE | SET NULL_ ) )?
        ( ON UPDATE no_action_restrict )?
    | ( ON UPDATE no_action_restrict )?
        ( ON DELETE ( NO ACTION | RESTRICT | CASCADE | SET NULL_ ) )?
    ;

no_action_restrict
    : NO ACTION | RESTRICT
    ;

constraint_characteristics
    : constraint_check_time ( NOT? DEFERRABLE )?
    | NOT? DEFERRABLE constraint_check_time?
    ;

constraint_check_time
    : INITIALLY DEFERRED | INITIALLY IMMEDIATE
    ;

simple_column_name_list
    : simple_column_name (COMMA simple_column_name )*
    ;

truncate_table
    : TRUNCATE TABLE? table_name
    ;

privilege_type
    : ALL PRIVILEGES
    | privilege_list
    ;

privilege_list
    : table_privilege (COMMA table_privilege )*
    ;

table_privilege
    : DELETE
    | INSERT
    | REFERENCES column_list?
    | SELECT column_list?
    | TRIGGER
    | UPDATE column_list?
    ;

column_list
    : '(' column_identifier (COMMA column_identifier )* ')'
    ;

column_identifier
    : id_
    ;

revokees
    : revokee (COMMA revokee)*
    ;

revokee
    : authorization_identifier | role_name | PUBLIC
    ;

authorization_identifier
    : id_
    ;

// alter commands
alter_command
    : alter_table
    ;

alter_table
    : ALTER TABLE table_name
        ADD COLUMN column_definition
        | ADD constraint_clause
        | DROP COLUMN? column_name cascade_restrict?
        | DROP ( PRIMARY KEY |
               FOREIGN KEY constraint_name |
               UNIQUE constraint_name |
               CHECK constraint_name |
               CONSTRAINT constraint_name )
        | ALTER COLUMN? column_alteration
        | LOCKSIZE row_table
    ;

constraint_clause
    : column_level_constraint
    | table_level_constraint
    ;

cascade_restrict
    : CASCADE | RESTRICT
    ;

row_table
    : ROW | TABLE
    ;

column_alteration
    : column_name SET DATA TYPE BLOB( integer )
    | column_name SET DATA TYPE CLOB( integer )
    | column_name SET DATA TYPE VARCHAR( integer )
    | column_name SET DATA TYPE VARCHAR( integer ) FOR BIT DATA
    | column_name SET INCREMENT BY integer_constant
    | column_name RESTART WITH integer_constant
    | column_name SET GENERATED always_by_default
    | column_name set_drop NOT NULL_
    | column_name NOT? NULL_
    | column_name with_set? DEFAULT defaultValue
    | column_name SET NO? CYCLE
    | column_name DROP DEFAULT
    ;

integer
    : num
    ;

integer_constant
    : num
    ;

defaultValue
    : constant_expression_null
    ;

constant_expression_null
    : num | string | NULL_
    ;

always_by_default
    : ALWAYS | BY DEFAULT
    ;

set_drop
    : SET | DROP
    ;

with_set
    : WITH | SET
    ;

// create commands
create_command
    : create_derby_aggregate
    | create_function
    | create_index
    | create_procedure
    | create_role
    | create_schema
    | create_sequence
    | create_synonym
    | create_table
    | create_trigger
    | create_type
    | create_view
    ;

create_derby_aggregate
    : CREATE DERBY AGGREGATE aggregate_name FOR value_data_type (RETURNS value_data_type) EXTERNAL NAME string
    ;

value_data_type
    : data_type
    ;

create_function
    : CREATE FUNCTION function_name '(' function_parameter (COMMA function_parameter)* ')' RETURNS return_data_type
        function_element*
    ;

function_parameter
    : parameter_name? data_type
    ;

return_data_type
    : table_type
    | data_type
    ;

table_type
    : TABLE '(' column_element (COMMA column_element ) ')'
    ;

column_element
    : id_ data_type
    ;

function_element
    : LANGUAGE JAVA
    | (DETERMINISTIC | NOT DETERMINISTIC)
    | EXTERNAL NAME single_quoted_string
    | PARAMETER STYLE ( JAVA | DERBY_JDBC_RESULT_SET | DERBY )
    | EXTERNAL SECURITY ( DEFINER | INVOKER )
    | ( NO SQL | CONTAINS SQL | READS SQL DATA )
    | ( RETURNS NULL_ ON NULL_ INPUT | CALLED ON NULL_ INPUT )
    ;

create_index
    : CREATE UNIQUE? INDEX index_name ON table_name '(' simple_column_name asc_desc? (',' simple_column_name asc_desc? )* ')'
    ;

create_procedure
    : CREATE PROCEDURE procedure_name '(' procedure_parameter (COMMA procedure_parameter)* ')'
        procedure_element*
    ;

procedure_parameter
    : ( IN | OUT | INOUT )? parameter_name? data_type
    ;

data_type
    : BIGINT
    | (BLOB | BINARY LARGE OBJECT) ('(' num (KILO | MEGA | GIGA)? ')')?
    | BOOLEAN
    | (CHAR | CHARACTER) ('(' num ')')?
    //| (CHAR | CHARACTER) //('(' num ')')? FOR BIT DATA
    | (CLOB | CHARACTER LARGE OBJECT) ('(' num (KILO | MEGA | GIGA)? ')')?
    | DATE
    | (DECIMAL | DEC | NUMERIC) ('(' num (COMMA num)? ')')?
    | DOUBLE
    | DOUBLE PRECISION
    | FLOAT ('(' num ')')?
    | (INT | INTEGER)
    | LONG VARCHAR
    | LONG VARCHAR FOR BIT DATA
    | REAL
    | SMALLINT
    | TIME
    | TIMESTAMP
    | (VARCHAR | CHAR VARYING | CHARACTER VARYING ) ('(' num ')')?
    //| (VARCHAR | CHAR VARYING | CHARACTER VARYING ) //('(' num ')')? FOR BIT DATA
    | XML
    ;

procedure_element
    : DYNAMIC? RESULT SETS integer
    | LANGUAGE JAVA
    | ( DETERMINISTIC | NOT DETERMINISTIC )
    | EXTERNAL NAME single_quoted_string
    | PARAMETER STYLE ( JAVA | DERBY )
    | EXTERNAL SECURITY ( DEFINER | INVOKER )
    | ( NO SQL
        | MODIFIES SQL DATA
        | CONTAINS SQL
        | READS SQL DATA )
    ;

create_role
    : CREATE ROLE role_name
    ;

create_schema
    : CREATE SCHEMA
      (
          schema_name AUTHORIZATION user_name
          | schema_name
          | AUTHORIZATION user_name
      )
    ;

create_sequence
    : CREATE SEQUENCE sequence_name (COMMA sequence_element )*
    ;

sequence_element
    : AS data_type
    | START WITH signedInteger
    | INCREMENT BY signedInteger
    | MAXVALUE signedInteger | NO MAXVALUE
    | MINVALUE signedInteger | NO MINVALUE
    | CYCLE | NO CYCLE
    ;

signedInteger
    : num
    ;

create_synonym
    : CREATE SYNONYM synonym_name FOR ( view_name | table_name )
    ;

create_table
    : CREATE TABLE table_name
//      {
//          ( { columnDefinition | tableLevelConstraint }
//          [ , { columnDefinition | tableLevelConstraint } ] * )
//        |
//          [ ( simpleColumnName [ , simpleColumnName ] * ) ]
//          AS queryExpression
          WITH NO DATA
//      }
    ;

create_trigger
    : CREATE TRIGGER trigger_name
//      { AFTER | NO CASCADE BEFORE }
//      { INSERT | DELETE | UPDATE [ OF columnName [ , columnName ]* ] }
//      ON tableName
//      [ referencingClause ]
//      [ FOR EACH { ROW | STATEMENT } ] [ MODE DB2SQL ]
//      [ WHEN ( booleanExpression ) ]
//      triggeredSQLStatement
    ;

create_type
    : CREATE TYPE type_name
        EXTERNAL NAME single_quoted_string
        LANGUAGE JAVA
    ;

single_quoted_string
    : string
    ;

create_view
    : CREATE VIEW view_name
          ( '(' simple_column_name (COMMA simple_column_name )* ')' )?
      AS query order_by_clause?
               offset_clause?
               fetch_clause?
    ;

query
    : '(' query
            order_by_clause?
            offset_clause?
            fetch_clause?
        ')'
    | query INTERSECT all_distinct? query
    | query EXCEPT all_distinct? query
    | query UNION all_distinct? query
    | select_expression
    | values_expression
    ;

select_expression
    : SELECT all_distinct? select_item (COMMA select_item )*
        from_clause
        where_clause?
        group_by_clause?
        having_clause?
//      [ WINDOW clause ]
        order_by_clause?
        offset_clause?
        fetch_clause?
    ;

select_item
    : '*'
    | ( table_name | correlation_name ) DOT '*'
    | expression (AS simple_column_name )?
    ;

simple_column_name
    : id_
    ;

// drop commands
drop_command
    : drop_derby_aggregate
    | drop_function
    | drop_index
    | drop_procedure
    | drop_role
    | drop_schema
    | drop_sequence
    | drop_synonym
    | drop_table
    | drop_trigger
    | drop_type
    | drop_view
    ;

drop_derby_aggregate
    : DROP DERBY AGGREGATE aggregate_name RESTRICT
    ;

drop_function
    : DROP FUNCTION function_name
    ;

drop_index
    : DROP INDEX index_name
    ;

drop_procedure
    : DROP PROCEDURE procedure_name
    ;

drop_role
    : DROP ROLE role_name
    ;

drop_schema
    : DROP SCHEMA schema_name RESTRICT
    ;

drop_sequence
    : DROP SEQUENCE sequence_name RESTRICT
    ;

drop_synonym
    : DROP SYNONYM synonym_name
    ;

drop_table
    : DROP TABLE table_name
    ;

drop_trigger
    : DROP TRIGGER trigger_name
    ;

drop_type
    : DROP TYPE type_name RESTRICT
    ;

drop_view
    : DROP VIEW view_name
    ;

// names
string
    : STRING_LITERAL
    ;

table_name
    : (schema_name DOT)? id_
    ;

schema_name
    : id_
    ;

role_name
    : id_
    ;

aggregate_name
    : (schema_name DOT)? id_
    ;

constraint_name
    : (schema_name DOT)? id_
    ;

column_name
    : ( ( table_name | correlation_name ) DOT )? id_
    ;

correlation_name
    : id_
    ;

function_name
    : (schema_name DOT)? id_
    ;

index_name
    : (schema_name DOT)? id_
    ;

procedure_name
    : (schema_name DOT)? id_
    ;

parameter_name
    : id_
    ;

sequence_name
    : (schema_name DOT)? id_
    ;

synonym_name
    : (schema_name DOT)? id_
    ;

trigger_name
    : (schema_name DOT)? id_
    ;

type_name
    : (schema_name DOT)? id_
    ;

user_name
    : id_
    ;

view_name
    : (schema_name DOT)? id_
    ;

cursor_nName
    : id_
    ;

id_
    : ID
    | DOUBLE_QUOTE_ID
//    | DOUBLE_QUOTE_BLANK
//    | keyword
    ;

num
    : DECIMAL_LITERAL
    ;

// expressions
expr_list
    : expression (COMMA expression)*
    ;

expression
    : primitive_expression
    | '(' expression ')'
    | table_subquery
    | function_call
    | case_expression
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
//    | over_clause
    ;

primitive_expression
    : DEFAULT //?
    | literal
    | id_
    ;

function_call
    : function_name '(' expr_list? ')'
    | standard_built_in_function '(' expr_list? ')'
    | aggreagate_built_in_function '(' expr_list? ')'
    ;

literal
    : string // string, date, time, timestamp
    | sign? DECIMAL_LITERAL
    | sign? (REAL_LITERAL | FLOAT_LITERAL)
    | TRUE | FALSE
    | NULL_
    ;

sign
    : PLUS
    | MINUS
    ;

case_expression
    : searched_case
    | simple_case
    | extended_case
    ;

searched_case
    : CASE
        WHEN boolean_expression THEN expression
        ( WHEN boolean_expression THEN expression )*
        ( ELSE expression )?
      END
    ;

simple_case
    : CASE value_expression
        WHEN value_expression (COMMA value_expression )* THEN expression
        ( WHEN value_expression (COMMA value_expression )* THEN expression )*
        ( ELSE expression )?
      END
    ;

extended_case
    : CASE value_expression
        WHEN when_operand (COMMA when_operand )* THEN expression
        ( WHEN when_operand (COMMA when_operand )* THEN expression )*
        ( ELSE expression )?
      END
    ;

when_operand
    : value_expression |
      comparison_operator expression |
      IS NOT? NULL_ |
      NOT? LIKE string ( ESCAPE string )? |
      NOT? BETWEEN expression AND expression |
      NOT? IN table_subquery |
      NOT? IN '(' expr_list ')' |
      comparison_operator ( ALL | ANY | SOME ) table_subquery
    ;

value_expression
    : expression
    ;

standard_built_in_function
    : ABS | ABSVAL
    | ACOS
    | ASIN
    | ATAN
    | ATAN2
    | BIGINT
    | CAST
    | CEIL | CEILING
    | CHAR
    | PIPE_PIPE
    | COS
    | NULLIF
    | CURRENT_DATE
    | CURRENT ISOLATION
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | CURRENT_USER
    | DATE
    | DAY
    | DEGREES
    | DOUBLE
    | EXP
    | FLOOR
    | HOUR
    | IDENTITY_VAL_LOCAL
    | INT | INTEGER
    | LENGTH
    | LN | LOG
    | LOG10
    | LOCATE
    | LCASE | LOWER
    | LTRIM
    | MINUTE
    | MOD
    | MONTH
    | PI
    | RADIANS
    | RTRIM
    | SECOND
    | SESSION_USER
    | SIN
    | SMALLINT
    | SQRT
    | SUBSTR
    | TAN
    | TIME
    | TIMESTAMP
    | TRIM
    | UCASE | UPPER
    | USER
    | VARCHAR
    | YEAR
    ;

aggreagate_built_in_function
    : AVG
    | COUNT
    | MAX
    | MIN
    | STDDEV_POP
    | STDDEV_SAMP
    | SUM
    | VAR_POP
    | VAR_SAMP
    ;

// select
select_statement
    : query
        order_by_clause?
        offset_clause?
        fetch_clause?
        for_update_clause?
        ( WITH ( RR | RS | CS | UR ) )?
    ;

for_update_clause
    : FOR
      (
          READ ONLY
         | FETCH ONLY
         | UPDATE ( OF simple_column_name (COMMA simple_column_name )* )?
      )
    ;

all_distinct
    : ALL | DISTINCT
    ;

from_clause
    : FROM table_expression (COMMA table_expression )*
    ;

table_expression
    : table_view_or_function_expression
    | table_expression join_operation
    ;

join_operation
    : INNER? JOIN table_expression ( ON boolean_expression | using_clause )
    | LEFT OUTER? JOIN table_expression ( ON boolean_expression | using_clause )
    | RIGHT OUTER? JOIN table_expression ( ON boolean_expression | using_clause )
    | CROSS JOIN ( table_view_or_function_expression | table_expression )
    | NATURAL ( ( LEFT | RIGHT ) OUTER? | INNER )? JOIN ( table_view_or_function_expression | table_expression )
    ;

table_view_or_function_expression
    : ( table_name | view_name ) correlation_clause?
    | ( table_subquery | table_function_invocation ) correlation_clause
    ;

using_clause
    : USING '(' simple_column_name (COMMA simple_column_name )* ')'
    ;

correlation_clause
    : AS? correlation_name ( '(' simple_column_name (COMMA simple_column_name )* ')' )?
    ;

table_function_invocation
    : TABLE function_name '(' ( function_arg (COMMA function_arg )* )? ')'
    ;

function_arg
    : expression
    ;

group_by_clause
    : GROUP BY (
        column_name (COMMA column_name )*
        | ROLLUP '(' column_name (COMMA column_name )* ')'
    )
    ;

having_clause
    : HAVING boolean_expression
    ;

order_by_clause
    : ORDER BY order_by_item (COMMA order_by_item )*
    ;

order_by_item
    : ( column_name | columnPosition | expression ) asc_desc? ( NULLS FIRST | NULLS LAST )?
    ;

asc_desc
    : ASC | DESC
    ;

columnPosition
    : num
    ;

values_expression
    : (
        VALUES '(' value (COMMA value )* ')' (COMMA '(' value (COMMA value )* ')' )*
        | VALUES value (COMMA value )*
      ) order_by_clause?
        offset_clause?
        fetch_clause?
    ;
