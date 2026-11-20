/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2025 by
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
 */

parser grammar H2Parser;

options {
    tokenVocab = H2Lexer;
}

h2_file
    : batch? EOF
    ;

batch
    : command (';' command)* ';'?
    ;

command
    : select
    | insert
    | update
    | delete
    | backup
    | call
    | execute_immediate
    | explain
    | merge_into
    | merge_using
    | runscript
    | script
    | show
    | with
    | alter_domain
    | alter_domain_add_constraint
    | alter_domain_drop_constraint
    | alter_domain_rename
    | alter_domain_rename_constraint
    | alter_index_rename
    | alter_schema_rename
    | alter_sequence
    | alter_table_add
    | alter_table_add_constraint
    | alter_table_rename_constraint
    | alter_table_alter_column
    | alter_table_drop_column
    | alter_table_drop_constraint
    | alter_table_set
    | alter_table_rename
    | alter_user_admin
    | alter_user_rename
    | alter_user_set_password
    | alter_view_recompile
    | alter_view_rename
    | analyze
    | comment_on
    | create_aggregate
    | create_alias
    | create_constant
    | create_domain
    | create_index
    | create_linked_table
    | create_role
    | create_schema
    | create_sequence
    | create_table
    | create_trigger
    | create_user
    | create_view
    | create_materialized_view
    | drop_aggregate
    | drop_alias
    | drop_all_objects
    | drop_constant
    | drop_domain
    | drop_index
    | drop_role
    | drop_schema
    | drop_sequence
    | drop_table
    | drop_trigger
    | drop_user
    | drop_view
    | drop_materialized_view
    | refresh_materialized_view
    | truncate_table
    | alter_type
    | checkpoint
    | checkpoint_sync
    | commit
    | commit_transaction
    | grant_right
    | grant_alter_any_schema
    | grant_role
    | help
    | prepare_commit
    | revoke_right
    | revoke_alter_any_schema
    | revoke_role
    | rollback
    | rollback_transaction
    | savepoint
    | set_allow_literals
    | set_autocommit
    | set_cache_size
    | set_cluster
    | set_builtin_alias_override
    | set_catalog
    | set_collation
    | set_database_event_listener
    | set_db_close_delay
    | set_default_lock_timeout
    | set_default_null_ordering
    | set_default_table_type
    | set_exclusive
    | set_ignorecase
    | set_ignore_catalogs
    | set_java_object_serializer
    | set_lazy_query_execution
    | set_lock_mode
    | set_lock_timeout
    | set_max_length_inplace_lob
    | set_max_log_size
    | set_max_memory_rows
    | set_max_memory_undo
    | set_max_operation_memory
    | set_mode
    | set_non_keywords
    | set_optimize_reuse_results
    | set_password
    | set_query_statistics
    | set_query_statistics_max_entries
    | set_query_timeout
    | set_referential_integrity
    | set_retention_time
    | set_salt_hash
    | set_schema
    | set_schema_search_path
    | set_session_characteristics
    | set_throttle
    | set_time_zone
    | set_trace_level
    | set_trace_max_file_size
    | set_truncate_large_length
    | set_variable_binary
    | set_write_delay
    | shutdown
    ;

select
    : SELECT (ALL | DISTINCT (ON '(' expression_list ')')?)? select_expression (',' select_expression)*
        (FROM table_expression (',' table_expression)*)?
        (WHERE expression)?
    ;

select_expression
    : wildcard_expression
    | expression (AS? alias)?
    ;

wildcard_expression
    : (schema_name_dot? t=alias '.')? '*'
        (EXCEPT '(' column (',' column)* ')' )?
    ;

table_expression
    : (schema_name_dot? table_name
    | '(' query ')'
    | unnest
    | table
    | data_change_delta_table)
        (AS alias column_name_list_in_parentheses? )?
        (USE INDEX '(' index_name_list? ')')?
        (join_type? JOIN table_expression join_specification?)?
    ;

join_type
    : (LEFT | RIGHT) OUTER?
    | INNER
    | CROSS
    | NATURAL
    ;

join_specification
    : ON expression
    | USING '(' column_name_list ')'
    ;

index_name_list
    : index_name (',' index_name)*
    ;

column_name_list_in_parentheses
    : '(' column_name_list ')'
    ;

column_name_list
    : column_name (',' column_name)*
    ;

table
    : (TABLE | TABLE DISTINCT) '(' table_item (',' table_item)* ')'
    ;

table_item
    : id_ data_type_or_domain '=' (array_ | row_value_expression)
    ;

unnest
    : UNNEST '(' expression (',' expression)* ')' (WITH ORDINALITY)?
    ;

data_change_delta_table
    : (OLD | NEW | FINAL) TABLE
        '(' (insert | update | delete | merge_into | merge_using) ')'
    ;

insert
    : INSERT INTO
    ;

update
    : UPDATE
    ;

delete
    : DELETE FROM
    ;

backup
    : BACKUP TO string
    ;

call
    : CALL
    ;

execute_immediate
    : EXECUTE IMMEDIATE string
    ;

explain
    : EXPLAIN
    ;

merge_into
    : MERGE INTO
    ;

merge_using
    : MERGE USING
    ;

runscript
    : RUNSCRIPT FROM
    ;

script
    : SCRIPT
    ;

show
    : SHOW
    ;

with
    : WITH RECURSIVE?
    ;

alter_domain
    : ALTER DOMAIN_ if_exists?
    ;

if_exists
    : IF EXISTS
    ;

if_not_exists
    : IF NOT EXISTS
    ;

alter_domain_add_constraint
    : ALTER DOMAIN_ if_exists? schema_name_dot? domain_name
        ADD
    ;

alter_domain_drop_constraint
    : ALTER DOMAIN_ if_exists? schema_name_dot? domain_name
        DROP CONSTRAINT if_exists? schema_name_dot? constraint_name
    ;

alter_domain_rename
    : ALTER DOMAIN_ if_exists? schema_name_dot? o=domain_name RENAME TO n=domain_name
    ;

alter_domain_rename_constraint
    : ALTER DOMAIN_ if_exists? schema_name_dot? domain_name
        RENAME CONSTRAINT schema_name_dot? o=constraint_name? TO n=constraint_name
    ;

alter_index_rename
    : ALTER INDEX if_exists? schema_name_dot? o=index_name RENAME TO n=index_name
    ;

alter_schema_rename
    : ALTER SCHEMA if_exists? o=schema_name RENAME TO n=schema_name
    ;

alter_sequence
    : ALTER SEQUENCE if_exists? schema_name_dot?
    ;

alter_table_add
    : ALTER TABLE if_exists? schema_name_dot? table_name
        ADD COLUMN? if_not_exists?
    ;

alter_table_add_constraint
    : ALTER TABLE if_exists table_name ADD
    ;

alter_table_rename_constraint
    : ALTER TABLE if_exists? schema_name_dot? table_name
        RENAME CONSTRAINT schema_name_dot? o=constraint_name
        TO n=constraint_name
    ;

alter_table_alter_column
    : ALTER TABLE if_exists? schema_name_dot? table_name
        ALTER COLUMN if_exists? column_name
    ;

alter_table_drop_column
    : ALTER TABLE if_exists? schema_name_dot? table_name
        DROP COLUMN? if_exists?
    ;

alter_table_drop_constraint
    : ALTER TABLE if_exists? schema_name_dot? table_name
        DROP (CONSTRAINT if_exists? schema_name_dot? constraint_name restrict_cascade? | PRIMARY KEY)
    ;

alter_table_set
    : ALTER TABLE if_exists? schema_name_dot? table_name
        SET REFERENTIAL_INTEGRITY true_false check_nocheck?
    ;

check_nocheck
    : CHECK
    | NOCHECK
    ;

alter_table_rename
    : ALTER TABLE if_exists? schema_name_dot? o=table_name RENAME TO n=table_name
    ;

alter_user_admin
    : ALTER USER user_name ADMIN true_false
    ;

alter_user_rename
    : ALTER USER o=user_name RENAME TO n=user_name
    ;

alter_user_set_password
    : ALTER USER user_name SET (PASSWORD string | SALT bytes HASH bytes)
    ;

alter_view_recompile
    : ALTER VIEW if_exists? schema_name_dot? view_name RECOMPILE
    ;

alter_view_rename
    : ALTER VIEW if_exists? schema_name_dot? o=view_name RENAME TO n=view_name
    ;

analyze
    : ANALYZE (TABLE schema_name_dot? table_name)? (SAMPLE_SIZE int_)?
    ;

comment_on
    : COMMENT ON
    ;

create_aggregate
    : CREATE AGGREGATE if_not_exists?
    ;

create_alias
    : CREATE ALIAS if_not_exists?
    ;

create_constant
    : CREATE CONSTANT_ if_not_exists? schema_name_dot? constant_name VALUE expression
    ;

create_domain
    : CREATE DOMAIN_ if_not_exists? schema_name_dot? domain_name AS?
    ;

create_index
    : CREATE (UNIQUE nulls_distinct | SPATIAL)? INDEX
        if_not_exists? schema_name_dot? index_name?
    ;

nulls_distinct
    : NULLS ( (NOT | ALL) DISTINCT)
    ;

create_linked_table
    : CREATE FORCE? (global_local? TEMPORARY)?
        LINKED TABLE if_not_exists?
    ;

global_local
    : GLOBAL
    | LOCAL
    ;

create_role
    : CREATE ROLE if_not_exists? role_name
    ;

create_schema
    : CREATE SCHEMA if_not_exists?
    ;

create_sequence
    : CREATE SEQUENCE if_not_exists? schema_name_dot? sequence_name
    ;

create_table
    : CREATE (CACHED | MEMORY)? (TEMP | global_local? TEMPORARY)?
        TABLE if_not_exists? schema_name_dot? table_name
    ;

create_trigger
    : CREATE TRIGGER if_not_exists? schema_name_dot? trigger_name
    ;

create_user
    : CREATE USER if_not_exists? user_name
    ;

create_view
    : CREATE or_replace? FORCE? VIEW if_not_exists? schema_name_dot? view_name
    ;

or_replace
    : OR REPLACE
    ;

create_materialized_view
    : CREATE or_replace? MATERIALIZED VIEW if_not_exists? schema_name_dot? view_name
    ;

drop_aggregate
    : DROP AGGREGATE if_exists? aggregate_name
    ;

drop_alias
    : DROP ALIAS if_exists? alias_name
    ;

drop_all_objects
    : DROP ALL OBJECTS (DELETE FILES)?
    ;

drop_constant
    : DROP CONSTANT_ if_exists? schema_name_dot? constant_name
    ;

drop_domain
    : DROP DOMAIN_ if_exists? schema_name_dot? domain_name restrict_cascade?
    ;

restrict_cascade
    : RESTRICT
    | CASCADE
    ;

drop_index
    : DROP INDEX if_exists? schema_name_dot? index_name
    ;

drop_role
    : DROP ROLE if_exists? role_name
    ;

drop_schema
    : DROP SCHEMA if_exists? schema_name restrict_cascade?
    ;

drop_sequence
    : DROP SEQUENCE if_exists? schema_name_dot? sequence_name
    ;

drop_table
    : DROP TABLE if_exists? schema_name_dot? table_name (',' schema_name_dot? table_name)*
        restrict_cascade?
    ;

drop_trigger
    : DROP TRIGGER if_exists? schema_name_dot? trigger_name
    ;

drop_user
    : DROP USER if_exists? user_name
    ;

drop_view
    : DROP VIEW if_exists? schema_name_dot? view_name restrict_cascade?
    ;

drop_materialized_view
    : DROP MATERIALIZED VIEW if_exists? schema_name_dot? view_name
    ;

refresh_materialized_view
    : REFRESH MATERIALIZED VIEW if_exists? schema_name_dot? view_name
    ;

truncate_table
    : TRUNCATE TABLE schema_name_dot? table_name ( (CONTINUE | RESTART) IDENTITY)?
    ;

alter_type
    : ALTER TYPE schema_name_dot? enum_name ADD VALUE string
    ;

checkpoint
    : CHECKPOINT
    ;

checkpoint_sync
    : CHECKPOINT SYNC
    ;

commit
    : COMMIT WORK?
    ;

commit_transaction
    : COMMIT TRANSACTION transaction_name
    ;

grant_right
    : GRANT
    ;

grant_alter_any_schema
    : GRANT ALTER ANY SCHEMA TO user_name
    ;

grant_role
    : GRANT role_name (',' role_name)* TO (PUBLIC | user_name | role_name)
    ;

help
    : HELP
    ;

prepare_commit
    : PREPARE COMMIT transaction_name
    ;

revoke_right
    : REVOKE
    ;

revoke_alter_any_schema
    : REVOKE ALTER ANY SCHEMA FROM user_name
    ;

revoke_role
    : REVOKE role_name (',' role_name)* FROM (PUBLIC | user_name | role_name)
    ;

rollback
    : ROLLBACK WORK? (TO SAVEPOINT savepoint_name)?
    ;

rollback_transaction
    : ROLLBACK TRANSACTION transaction_name
    ;

savepoint
    : SAVEPOINT savepoint_name
    ;

set_allow_literals
    : SET ALLOW_LITERALS (NONE | ALL | NUMBERS)
    ;

set_autocommit
    : SET AUTOCOMMIT (true_false | ON | OFF)
    ;

set_cache_size
    : SET CACHE_SIZE int_
    ;

set_cluster
    : SET CLUSTER
    ;

set_builtin_alias_override
    : SET BUILTINT_ALIAS_OVERRIDE true_false
    ;

set_catalog
    : SET CATALOG
    ;

set_collation
    : SET DATABASE? COLLATION (OFF | collation_name (STRENGTH (PRIMARY | SECONDARY | TERTIARY | IDENTICAL))?)
    ;

set_database_event_listener
    : SET DATABASE_EVENT_LISTENER class_name_string
    ;

set_db_close_delay
    : SET DB_CLOSE_DELAY int_
    ;

set_default_lock_timeout
    : SET DEFAULT_LOCK_TIMEOUT int_
    ;

set_default_null_ordering
    : SET DEFAULT_NULL_ORDERING (LOW | HIGH | FIRST | LAST)
    ;

set_default_table_type
    : SET DEFAULT_TABLE_TYPE (MEMORY | CACHED)
    ;

set_exclusive
    : SET EXCLUSIVE
    ;

set_ignorecase
    : SET IGNORECASE true_false
    ;

set_ignore_catalogs
    : SET IGNORE_CATALOGS true_false
    ;

set_java_object_serializer
    : SET JAVA_OBJECT_SERIALIZER (NULL_ | class_name)
    ;

set_lazy_query_execution
    : SET LAZE_QUERY_EXECUTION int_
    ;

set_lock_mode
    : SET LOCK_MODE int_
    ;

set_lock_timeout
    : SET LOCK_TIMEOUT int_
    ;

set_max_length_inplace_lob
    : SET_MAX_LENGTH_INPLACE_LOB int_
    ;

set_max_log_size
    : SET MAX_LOG_SIZE int_
    ;

set_max_memory_rows
    : SET MAX_MEMORY_ROWS int_
    ;

set_max_memory_undo
    : SET MAX_MEMORY_UNDO int_
    ;

set_max_operation_memory
    : SET MAX_OPERATION_MEMORY int_
    ;

set_mode
    : SET MODE
        ( REGULAR
        | STRICT
        | LEGACY
        | DB2
        | DERBY
        | HSQLDB
        | MSSQLSERVER
        | MYSQL
        | ORACLE
        | POSTGRESQL
        )
    ;

set_non_keywords
    : SET NON_KEYWORDS
    ;

set_optimize_reuse_results
    : SET OPTIMZE_REUSE_RESULTS
    ;

set_password
    : SET PASSWORD string
    ;

set_query_statistics
    : SET QUERY_STATISTICS true_false
    ;

set_query_statistics_max_entries
    : SET QUERY_STATISTICS_MAX_ENTRIES int_
    ;

set_query_timeout
    : SET QUERY_TIMEOUT int_
    ;

set_referential_integrity
    : SET REFERENTIAL_INTEGRIRY true_false
    ;

set_retention_time
    : SET RETENTION_TIME int_
    ;

set_salt_hash
    : SET SALT bytes HASH bytes
    ;

set_schema
    : SET SCHEMA (string | schema_name)
    ;

set_schema_search_path
    : SET SCHEMA_SEARCH_PATH schema_name (',' schema_name)*
    ;

set_session_characteristics
    : SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL
        (READ (UNCOMMITED | COMMITED) | REPEATABLE READ | SERIALIZABLE)
    ;

set_throttle
    : SET THROTTLE int_
    ;

set_time_zone
    : SET TIME ZONE
    ;

set_trace_level
    : SET (TRACE_LEVEL_FILE | TRACE_LEVEL_SYSTEM_OUT) int_
    ;

set_trace_max_file_size
    : SET TRACE_MAX_FILE_SIZE int_
    ;

set_truncate_large_length
    : SET TRUNCATE_LARGE_LENGTH true_false
    ;

set_variable_binary
    : SET VARIABLE_BINARY true_false
    ;

set_write_delay
    : SET WRITE_DELAY int_
    ;

shutdown
    : SHUTDOWN (IMMEDIATELY | COMPACT | DEFRAG)
    ;

//
true_false
    : TRUE
    | FALSE
    ;

int_
    : ('+' | '-')? NUMBER
    ;

bytes
    : HEXSTRING
    ;

class_name_string
    :
    ;

string
    : STRING
    ;

id_
    : IDENTIFIER
    ;

class_name
    : id_
    ;

collation_name
    : id_
    ;

column_name
    : id_
    ;

constraint_name
    : id_
    ;

enum_name
    : id_
    ;

role_name
    : id_
    ;

savepoint_name
    : id_
    ;

schema_name
    : id_
    ;

schema_name_dot
    : schema_name '.'
    ;

aggregate_name
    : id_
    ;

alias_name
    : id_
    ;

constant_name
    : id_
    ;

domain_name
    : id_
    ;

field_name
    : id_
    ;

index_name
    : id_
    ;

sequence_name
    : id_
    ;

table_name
    : id_
    ;

transaction_name
    : id_
    ;

trigger_name
    : id_
    ;

user_name
    : id_
    ;

view_name
    : id_
    ;

//
expression
    : and_condition (OR and_condition)*
    ;

and_condition
    : condition (AND condition)*
    ;

condition
    : operand condition_rhs?
    | NOT condition
    | EXISTS '(' query ')'
    | UNIQUE nulls_distinct? '(' query ')'
    | INTERSECTS '(' operand ',' operand ')'
    ;

condition_rhs
    : comparison_rhs
    | quantified_comparison_rhs
    | null_predicate_rhs
    | distinct_predicate_rhs
    | quantified_distinct_predicate_rhs
    | boolean_test_rhs
    | type_predicate_rhs
    | json_predicate_rhs
    | between_predicate_rhs
    | in_predicate_rhs
    | like_predicate_rhs
    | regexp_predicate_rhs
    ;

compare
    : '<>'
    | '<='
    | '>='
    | '='
    | '<'
    | '>'
    | '!='
    | '&&'
    ;

operand
    : summand ('||' summand)*
    ;

summand
    : factor (plus_minus factor)*
    ;

plus_minus
    : '+'
    | '-'
    ;

factor
    : term (mul_div_mod term)*
    ;

mul_div_mod
    : '*'
    | '/'
    | '%'
    ;

term
    : (value
    | column
    | '?' int_?
    | sequence_value_expression
    | function
    | plus_minus term
    | '(' expression ')'
    | array_element_reference
    | field_reference
    | '(' query ')'
    | case_expression
    | cast_specification
    | user_defined_function_name) (time_zone | interval_qualifier)?
    ;

user_defined_function_name
    : id_
    ;

interval_qualifier
    : YEAR
    | MONTH
    | DAY
    | HOUR
    | MINUTE
    | SECONDS
    ;

precision_int
    : int_
    ;

scale_int
    : int_
    ;

query
    : select
    | explicit_table
    | table_value
    ;

explicit_table
    : TABLE schema_name_dot? table_name
        order_by_clause?
        offset_clause?
        fetch_clause?
    ;

table_value
    : VALUES row_value_expression (',' row_value_expression)*
        order_by_clause?
        offset_clause?
        fetch_clause?
    ;

order_by_clause
    : ORDER BY select_order (',' select_order)*
    ;

offset_clause
    : OFFSET expression row_rows
    ;

fetch_clause
    : FETCH first_next (expression PERCENT?)? row_rows only_with_ties
    ;

row_value_expression
    : ROW? '(' expression_list? ')'
    | expression
    ;

select_order
    : (expression | int_) asc_desc? nulls_first_last?
    ;

asc_desc
    : ASC
    | DESC
    ;

nulls_first_last
    : NULLS (FIRST | LAST)
    ;

first_next
    : FIRST
    | NEXT
    ;

row_rows
    : ROW
    | ROWS
    ;

only_with_ties
    : ONLY
    | WITH TIES
    ;

case_expression
    : simple_case
    | searched_case
    ;

simple_case
    : CASE expression
        (WHEN (expression | condition_rhs) (',' (expression | condition_rhs))* THEN expression)+
        (ELSE expression)?
        END
    ;

searched_case
    : (CASE WHEN expression THEN expression)+
        (ELSE expression)?
        END
    ;

cast_specification
    : CAST '(' value AS data_type_or_domain (FORMAT string)? ')'
    ;

data_type_or_domain
    : data_type
    | schema_name_dot? domain_name
    ;

data_type
    : predefined_type
    | array_type
    | row_type
    ;

predefined_type
    : CHAR
    ;

array_type
    : predefined_type ARRAY ('[' int_ ']')?
    ;

row_type
    : ROW '(' field_name data_type (',' field_name data_type)* ')'
    ;

function
    : id_ '(' expression_list ')'
    ;

sequence_value_expression
    : (NEXT | CURRENT) VALUE FOR schema_name_dot? sequence_name
    ;

time_zone
    : AT (TIME ZONE (interval_hour_to_minute | interval_hour_to_second | string) | LOCAL)
    ;

interval_hour_to_minute
    : INTERVAL plus_minus? string HOUR precision_in_parentheses? TO MINUTE
    ;

precision_in_parentheses
    : '(' precision_int ')'
    ;

interval_hour_to_second
    : INTERVAL plus_minus? string HOUR precision_in_parentheses? TO SECOND fp=precision_in_parentheses?
    ;

column
    : (schema_name_dot? t=alias '.')? (column_name | ROW_ID)
    ;

alias
    : id_
    ;

array_element_reference
    : (array_ | json) '[' int_ ']'
    ;

array_
    : ARRAY '[' expression_list? ']'
    ;

expression_list
    : expression (',' expression)*
    ;

json
    : JSON (bytes | string)
    ;

field_reference
    : '(' expression ')' '.' field_name
    ;

comparison_rhs
    : compare operand
    ;

quantified_comparison_rhs
    : compare all_any_some '(' (query | array_) ')'
    ;

null_predicate_rhs
    : IS NOT? NULL_
    ;

distinct_predicate_rhs
    : IS NOT? distinct_from? operand
    ;

quantified_distinct_predicate_rhs
    : IS NOT? distinct_from? all_any_some '(' (query | array_) ')'
    ;

distinct_from
    : DISTINCT FROM
    ;

all_any_some
    : ALL
    | ANY
    | SOME
    ;

boolean_test_rhs
    : IS NOT? (true_false | UNKNOWN)
    ;

type_predicate_rhs
    : IS NOT? OF '(' data_type (',' data_type)* ')'
    ;

json_predicate_rhs
    : IS NOT? JSON (VALUE | ARRAY | OBJECT | SCALAR)
        (with_without? UNIQUE KEYS?)?
    ;

with_without
    : WITH
    | WITHOUT
    ;

between_predicate_rhs
    : NOT? BETWEEN (ASYMETRIC | SYMETRIC)? operand AND operand
    ;

in_predicate_rhs
    : NOT? IN '(' (query | expression_list) ')'
    ;

like_predicate_rhs
    : NOT? (LIKE | ILIKE) operand (ESCAPE string)?
    ;

regexp_predicate_rhs
    : NOT? REGEXP operand
    ;


value
    : string
    | DOLLAR_QUOTED_STRING
    | numeric
    | date_and_time
    | boolean
    | bytes
    | interval
    | array_
    | geometry
    | json
    | uuid
    | NULL_
    ;

numeric
    : int_
    ;

date_and_time
    : DATE string
    | TIME without_timezone? string
    | TIME with_timezone string
    | TIMESTAMP without_timezone? string
    | TIMESTAMP with_timezone string
    ;

boolean
    : true_false
    | UNKNOWN
    ;

interval
    : INTERVAL plus_minus string
        (YEAR | MONTH | DAY | HOUR | MINUTE)
        precision_in_parentheses?
    ;

geometry
    : GEOMETRU (bytes | string)
    ;

uuid
    : UUID string
    ;

without_timezone
    : WITHOUT TIME ZONE
    ;

with_timezone
    : WITH TIME ZONE
    ;

todo
    : '.'
    ;