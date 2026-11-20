/*
Redshift SQL grammar.
The MIT License (MIT).

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

parser grammar RedshiftParser;

options {
    tokenVocab = RedshiftLexer;
}

redshift_file
    : batch? EOF
    ;

batch
    : sql_command (SEMI sql_command)* SEMI?
    ;

sql_command
    : abort_command
    | alter_database_command
    | alter_datashare_command
    | alter_default_privileges_command
    | alter_external_schema_command
    | alter_external_view_command
    | alter_function_command
    | alter_group_command
    | alter_identity_provider_command
    | alter_masking_policy_command
    | alter_materialized_view_command
    | alter_rls_policy_command
    | alter_role_command
    | alter_procedure_command
    | alter_schema_command
    | alter_system_command
    | alter_table_command
    | alter_table_append_command
    | alter_user_command
    | analyze_command
    | analyze_compression_command
    | attach_masking_policy_command
    | attach_rls_policy_command
    | begin_command
    | call_command
    | cancel_command
    | close_command
    | comment_command
    | commit_command
    | copy_command
    | create_database_command
    | create_datashare_command
    | create_external_function_command
    | create_external_model_command
    | create_external_schema_command
    | create_external_table_command
    | create_external_view_command
    | create_function_command
    | create_group_command
    | create_identity_provider_command
    | create_library_command
    | create_masking_policy_command
    | create_materialized_view_command
    | create_model_command
    | create_procedure_command
    | create_rls_policy_command
    | create_role_command
    | create_schema_command
    | create_table_command
    | create_table_as_command
    | create_user_command
    | create_view_command
    | deallocate_command
    | declare_command
    | delete_command
    | desc_datashare_command
    | desc_identity_provider_command
    | detach_masking_policy_command
    | detach_rls_policy_command
    | drop_database_command
    | drop_datashare_command
    | drop_external_view_command
    | drop_function_command
    | drop_group_command
    | drop_identity_provider_command
    | drop_library_command
    | drop_masking_policy_command
    | drop_model_command
    | drop_materialized_view_command
    | drop_procedure_command
    | drop_rls_policy_command
    | drop_role_command
    | drop_schema_command
    | drop_table_command
    | drop_user_command
    | drop_view_command
    | end_command
    | execute_command
    | explain_command
    | fetch_command
    | grant_command
    | insert_command
    | lock_command
    | merge_command
    | prepare_command
    | refresh_materialized_view_command
    | reset_command
    | revoke_command
    | rollback_command
    | select_command
    | select_into_command
    | set_command
    | set_session_authorization_command
    //| set_session_characteristics_command //deprecated
    | show_command
    | show_column_grants_command
    | show_columns_command
    | show_constraints_command
    | show_external_table_command
    | show_databases_command
    | show_functions_command
    | show_grants_command
    | show_model_command
    | show_datashares_command
    | show_parameters_command
    | show_policies_command
    | show_procedure_command
    | show_procedures_command
    | show_schemas_command
    | show_table_command
    | show_tables_command
    | show_view_command
    | start_transaction_command
    | truncate_command
    | unload_command
    | update_command
    | use_command
    | vacuum_command
    ;

abort_command
    : ABORT work_transaction?
    ;

work_transaction
    : WORK
    | TRANSACTION
    ;

alter_database_command
    : ALTER DATABASE database_name
        ( RENAME TO n=database_name
        | owner_to_clause
        | ( CONNECTION LIMIT limit_unlimited
          | COLLATE (CASE_SENSITIVE | CS | CASE_INSENSITIVE | CI)
          | ISOLATION LEVEL (SNAPSHOT | SERIALIZABLE)
          )?
        | INTEGRATION
            ( REFRESH (all_inerror TABLES (IN SCHEMA schema_name_list)? | TABLE schema_table_list)
            | SET
                (QUERY_ALL_STATES '=' true_false)?
                (ACCEPTINVCHARS '=' true_false)?
                (REFRESH_INTERVAL interval)?
                (TRUNCATECOLUMNS '=' true_false)?
                (HISTORY_MODE '=' true_false (FOR (ALL TABLES (IN SCHEMA schema_name_list)? | TABLE schema_table_list) )? )?
            )
        )
    ;

owner_to_clause
    : OWNER TO new_owner
    ;

limit_unlimited
    : limit
    | UNLIMITED
    ;

all_inerror
    : ALL
    | INERROR
    ;

schema_name_list
    : schema_name (',' schema_name)*
    ;

schema_sql_udf_list
    : schema_name '.' function_name argtype_list_in_parentheses (',' schema_name '.' function_name argtype_list_in_parentheses)*
    ;

argtype_list_in_parentheses
    : '(' argtype_list ')'
    ;

argtype_list
    : argtype (',' argtype)*
    ;

argtype
    : todo
    ;

schema_table_list
    : schema_name '.' table_name (',' schema_name '.' table_name)*
    ;

true_false
    : TRUE
    | FALSE
    ;

alter_datashare_command
    : ALTER DATASHARE datashare_name (ADD | REMOVE)
        ( TABLE schema_table_list
        | SCHEMA schema_name_list
        | FUNCTION schema_sql_udf_list
        | ALL TABLES IN SCHEMA schema_name_list
        | ALL FUNCTIONS IN SCHEMA schema_name_list
        )
    | ALTER DATASHARE datashare_name
        SET ( PUBLICACCESSIBLE '=' true_false
            | INCLUDENEW '=' true_false FOR SCHEMA schema_name
            )
    ;

alter_default_privileges_command
    : ALTER DEFAULT PRIVILEGES
        (FOR USER target_user_list)?
        (IN SCHEMA schema_name_list)?
        grant_or_revoke_clause
    ;

target_user_list
    : target_user (',' target_user)*
    ;

grant_or_revoke_clause
    : GRANT ( (privilege_list | all_privileges) ON TABLES | (EXECUTE | all_privileges) ON (FUNCTIONS | PROCEDURES) )
        TO grant_target_list
    | REVOKE grant_option_for? ( (privilege_list | all_privileges) ON TABLES | (EXECUTE | all_privileges) ON (FUNCTIONS | PROCEDURES) )
        FROM revoke_target RESTRICT?
    ;

privilege_list
    : privilege (',' privilege)*
    ;

privilege
    : SELECT
    | INSERT
    | UPDATE
    | DELETE
    | DROP
    | REFERENCES
    | TRUNCATE
    ;

all_privileges
    : ALL PRIVILEGES?
    ;

grant_target_list
    : grant_target (',' grant_target)*
    ;

grant_target
    : user_name (WITH GRANT OPTION)?
    | ROLE role_name
    | GROUP group_name
    | PUBLIC
    ;

grant_option_for
    : GRANT OPTION FOR
    ;

revoke_target
    : ROLE role_name
    | GROUP group_name
    | PUBLIC
    | user_name
    ;

alter_external_schema_command
    : ALTER EXTERNAL SCHEMA schema_name
        (IAM_ROLE (DEFAULT | i=string)?)?
        (AUTHENTICATION (NONE | IAM | MTLS)?)?
        (AUTHENTICATION_ARN c=string | SECRET_ARN s=string)?
        (URI k=string)?
    ;

alter_external_view_command
    : ALTER EXTERNAL VIEW schema_name '.' view_name
        ( catalog_name '.' schema_name '.' view_name
        | awsdatacatalog_name '.' database_name '.' view_name
        | e=schema_name '.' view_name
        )
        FORCE? (AS '(' query ')' | REMOVE DEFINITION)
    ;

alter_function_command
    : ALTER FUNCTION o=function_name '(' function_arg_list? ')'
        ( RENAME TO n=function_name
        | OWNER TO (u=user_name | CURRENT_USER | SESSION_USER)
        )
    ;

function_arg_list
    : py_arg_name py_arg_data_type (',' py_arg_name py_arg_data_type)*
    | sql_arg_data_type (',' sql_arg_data_type)*
    ;

alter_group_command
    : ALTER GROUP o=group_name
        ( (ADD | DROP) USER user_name (',' user_name)*
        | RENAME TO n=group_name
        )
    ;

alter_identity_provider_command
    : ALTER IDENTITY PROVIDER identity_provider_name
        (PARAMETERS p=string)?
        (NAMESPACE ns=string)?
        (IAM_ROLE ir=string)?
        (AUTO_CREATE_ROLES (TRUE (include_exclude GROUPS LIKE fp=string)? | FALSE)? )?
        disable_enable?
    ;

disable_enable
    : DISABLE
    | ENABLE
    ;

include_exclude
    : INCLUDE
    | EXCLUDE
    ;

alter_masking_policy_command
    : ALTER MASKING POLICY ( (database_name '.')? policy_name)
        USING '(' me=expression ')'
    ;

alter_materialized_view_command
    : ALTER MATERIALIZED VIEW mv=view_name
        ( AUTO REFRESH yes_no
        | ALTER ( DISTKEY column_name
                | DISTSTYLE (ALL | EVEN | KEY DISTKEY column_name | AUTO)
                | COMPOUND? SORTKEY column_name_list_in_parentheses
                | SORTKEY (AUTO | NONE)
                )
        | ROW LEVEL SECURITY on_off (CONJUNCTION TYPE (AND | OR) )? (FOR DATASHARES)?
        )
    ;

column_name_list_in_parentheses
    : '(' column_name_list ')'
    ;

column_name_list
    : column_name (',' column_name)*
    ;

yes_no
    : YES
    | NO
    ;

on_off
    : ON
    | OFF
    ;

alter_rls_policy_command
    : ALTER RLS POLICY (database_name '.')? policy_name
        USING '(' predicate_expression ')'
    ;

alter_role_command
    : ALTER ROLE o=role_name WITH?
        alter_role_opt (',' alter_role_opt)*
        (EXTERNALID TO external_id)?
    ;

alter_role_opt
    : RENAME TO n=role_name
    | owner_to_clause
    ;

alter_procedure_command
    : ALTER PROCEDURE o=sp_name sp_arg_list_in_parentheses?
        ( RENAME TO n=sp_name
        | OWNER TO (owner | CURRENT_USER | SESSION_USER)
        )
    ;

sp_arg_list_in_parentheses
    : '(' sp_arg_list ')'
    ;

sp_arg_list
    : sp_arg (',' sp_arg)*
    ;

sp_arg
    : argname? argmode? argtype
    ;

argname
    : id_
    ;

argmode
    : INOUT
    | IN
    | OUT
    ;

alter_schema_command
    : ALTER SCHEMA s=schema_name
        ( RENAME TO n=schema_name
        | owner_to_clause
        | QUOTA (quota (MB | GB | TB)? | UNLIMITED)
        )
    ;

quota
    : DECIMAL
    ;

alter_system_command
    : ALTER SYSTEM SET id_ '=' (TRUE | T | ON | FALSE | F | OFF)
    ;

alter_table_command
    : ALTER TABLE o=table_name
        ( ADD table_constraint
        | DROP CONSTRAINT constraint_name restrict_cascade?
        | owner_to_clause
        | RENAME TO n=table_name
        | RENAME COLUMN oc=column_name TO nc=column_name
        | ALTER COLUMN column_name TYPE
        | ALTER COLUMN column_name ENCODE encode_type (',' encode_type)*
        | ALTER DISTKEY column_name
        | ALTER DISTSTYLE (ALL | EVEN | KEY DISTKEY column_name | AUTO)
        | ALTER COMPOUND? SORTKEY '(' column_name_list ')'
        | ALTER SORTKEY (AUTO | NONE)
        | ALTER ENCODE AUTO
        | ADD COLUMN? column_name column_type (DEFAULT d=expression)? (ENCODE encode_type)? (NOT? NULL_)? collate_clause?
        | DROP COLUMN? column_name restrict_cascade
        | ROW LEVEL SECURITY
        | MASKING on_off FOR DATASHARES
        )
    ;

collate_clause
    : COLLATE (CASE_SENSITIVE | CS | CASE_INSENSITIVE | CI)
    ;

table_constraint
    : (CONSTRAINT constraint_name)?
        ( UNIQUE
        | PRIMARY KEY
        | FOREIGN KEY
        )
    ;

restrict_cascade
    : RESTRICT
    | CASCADE
    ;

alter_table_append_command
    : ALTER TABLE t=table_name APPEND FROM s=table_name
        (IGNOREEXTRA | FILLTARGET)?
    ;

alter_user_command
    : ALTER USER user_name WITH? option (',' option)*
    ;

option
    : CREATEDB | NOCREATEDB
    | CREATEUSER | NOCREATEUSER
    | SYSLOG ACCESS (RESTRICTED | UNSRESTRICTED)
    | PASSWORD (string | DISABLE) (VALID UNTIL  ed=string)?
    | RENAME TO nu=user_name
    | CONNECTION LIMIT limit_unlimited
    | SESSION TIMEOUT limit
    | RESET SESSION TIMEOUT
    | SET p=id_ (TO | '=') (value | DEFAULT)
    | RESET rp=id_
    | EXTERNALID external_id
    ;

analyze_command
    : ANALYZE VERBOSE?
        (table_name column_name_list_in_parentheses?)?
        (PREDICT COLUMNS | ALL COLUMNS)?
    ;

analyze_compression_command
    : ANALYZE COMPRESSION
        (table_name column_name_list_in_parentheses?)
        (COMPROWS numrows)
    ;

attach_masking_policy_command
    : ATTACH MASKING POLICY (policy_name ON relation_name | database_name '.' policy_name ON database_name '.' schema_name '.' relation_name)
        '(' ')'
        (USING '(' ')' )?
        TO user_role_public
        (PRIORITY priority)?
    ;

attach_rls_policy_command
    : ATTACH RLS POLICY
        ( policy_name ON TABLE? table_name_list
        | database_name '.' policy_name ON TABLE? db_schema_table_name_list
        )
        TO user_role_public (',' user_role_public)*
    ;

user_role_public
    : user_name
    | ROLE role_name
    | PUBLIC
    ;

table_name_list
    : table_name (',' table_name)*
    ;

db_schema_table_name_list
    : db_schema_table_name (',' db_schema_table_name)*
    ;

db_schema_table_name
    : database_name '.' schema_name '.' table_name
    ;

begin_command
    : BEGIN work_transaction? (ISOLATION LEVEL isolation_level)?
        (READ (WRITE | ONLY))?
    ;

isolation_level
    : SERIALIZABLE
    | READ UNCOMMITTED
    | READ COMMITTED
    | REPEATABLE READ
    ;

call_command
    : CALL sp_name '(' argument_list? ')'
    ;

argument_list
    : expression (',' expression)*
    ;

cancel_command
    : CANCEL process_id m=string?
    ;

close_command
    : CLOSE cursor_name
    ;

comment_command
    : COMMENT ON
        ( TABLE table_name
        | COLUMN table_name '.' column_name
        | CONSTRAINT constraint_name ON table_name
        | DATABASE database_name
        | VIEW view_name
        ) IS (string | NULL_)
    ;

commit_command
    : COMMIT work_transaction?
    ;

copy_command
    : COPY table_name column_name_list_in_parentheses?
        FROM ds=string
        IAM_ROLE r=string
        (FORMAT? AS? data_format)?
    ;

data_format
    : todo
    ;

create_database_command
    : CREATE DATABASE database_name
    ;

create_datashare_command
    : CREATE DATASHARE datashare_name
    ;

create_external_function_command
    : CREATE or_replace? EXTERNAL FUNCTION
    ;

or_replace
    : OR REPLACE
    ;

create_external_model_command
    : CREATE EXTERNAL MODEL model_name
    ;

create_external_schema_command
    : CREATE EXTERNAL SCHEMA if_not_exists?
    ;

if_not_exists
    : IF NOT EXISTS
    ;

create_external_table_command
    : CREATE EXTERNAL TABLE
    ;

create_external_view_command
    : CREATE EXTERNAL VIEW schema_name '.' view_name if_not_exists?
    ;

create_function_command
    : CREATE or_replace? FUNCTION
    ;

create_group_command
    : CREATE GROUP group_name
    ;

create_identity_provider_command
    : CREATE IDENTITY PROVIDER identity_provider_name TYPE
    ;

create_library_command
    : CREATE or_replace? LIBRARY library_name LANGUAGE
    ;

create_masking_policy_command
    : CREATE MASKING POLICY
    ;

create_materialized_view_command
    : CREATE MATERIALIZED VIEW
    ;

create_model_command
    : CREATE MODEL model_name
    ;

create_procedure_command
    : CREATE or_replace? PROCEDURE
    ;

create_rls_policy_command
    : CREATE RLS POLICY
    ;

create_role_command
    : CREATE ROLE role_name
    ;

create_schema_command
    : CREATE SCHEMA if_not_exists? schema_name
    ;

create_table_command
    : CREATE (LOCAL? temporary)? TABLE if_not_exists? table_name
    ;

temporary
    : TEMPORARY
    | TEMP
    ;

create_table_as_command
    : CREATE (LOCAL? temporary)? TABLE if_not_exists? table_name
    ;

create_user_command
    : CREATE USER
    ;

create_view_command
    : CREATE or_replace? VIEW
    ;

deallocate_command
    : DEALLOCATE PREPARE? plan_name
    ;

declare_command
    : DECLARE cursor_name CURSOR FOR query
    ;

delete_command
    : todo DELETE
    ;

desc_datashare_command
    : DESC DATASHARE datashare_name
    ;

desc_identity_provider_command
    : DESC IDENTITY PROVIDER identity_provider_name
    ;

detach_masking_policy_command
    : DETACH MASKING POLICY
    ;

detach_rls_policy_command
    : DETACH RLS POLICY
    ;

drop_database_command
    : DROP DATABASE database_name FORCE?
    ;

drop_datashare_command
    : DROP DATASHARE datashare_name
    ;

drop_external_view_command
    : DROP EXTERNAL VIEW schema_name '.' view_name if_exists?
    ;

drop_function_command
    : DROP FUNCTION function_name '('  fn_arg_list? ')'
        restrict_cascade?
    ;

fn_arg_list
    : fn_arg (',' fn_arg)*
    ;

fn_arg
    : argname? argtype
    ;

drop_group_command
    : DROP GROUP group_name
    ;

drop_identity_provider_command
    : DROP IDENTITY PROVIDER identity_provider_name CASCADE?
    ;

drop_library_command
    : DROP LIBRARY library_name
    ;

drop_masking_policy_command
    : DROP MASKING POLICY (database_name '.')? policy_name
    ;

drop_model_command
    : DROP MODEL if_exists? model_name
    ;

if_exists
    : IF EXISTS
    ;

drop_materialized_view_command
    : DROP MATERIALIZED VIEW if_exists? view_name restrict_cascade?
    ;

drop_procedure_command
    : DROP PROCEDURE sp_name '(' sp_arg_list? ')'
    ;

drop_rls_policy_command
    : DROP RLS POLICY if_exists? (database_name '.')? policy_name restrict_cascade?
    ;

drop_role_command
    : DROP ROLE role_name (FORCE | RESTRICT)?
    ;

drop_schema_command
    : DROP SCHEMA if_exists? schema_name_list
        (DROP EXTERNAL DATABASE)?
        restrict_cascade?
    ;

drop_table_command
    : DROP TABLE if_exists? table_name_list restrict_cascade?
    ;

drop_user_command
    : DROP USER if_exists? user_name (',' user_name)*
    ;

drop_view_command
    : DROP VIEW if_exists? view_name (',' view_name)* restrict_cascade?
    ;

end_command
    : END work_transaction?
    ;

execute_command
    : EXECUTE plan_name ('(' parameter_name (',' parameter_name)* ')')?
    ;

explain_command
    : EXPLAIN VERBOSE? query
    ;

fetch_command
    : FETCH (NEXT | ALL | (FORWARD | count | ALL))? FROM cursor_name
    ;

grant_command
    : GRANT
    ;

insert_command
    : INSERT INTO table_name
    ;

lock_command
    : LOCK TABLE? table_name (',' table_name)*
    ;

merge_command
    : MERGE INTO
    ;

prepare_command
    : PREPARE plan_name
    ;

refresh_materialized_view_command
    : REFRESH MATERIALIZED VIEW
    ;

reset_command
    : RESET (parameter_name | ALL)
    ;

revoke_command
    : REVOKE
    ;

rollback_command
    : ROLLBACK work_transaction?
    ;

select_command
    : todo SELECT
    ;

select_into_command
    : todo SELECT
    ;

set_command
    : SET
    ;

set_session_authorization_command
    : SET LOCAL? SESSION AUTHORIZATION (user_name | DEFAULT)
    ;

show_command
    : SHOW (parameter_name | ALL)
    ;

show_column_grants_command
    : SHOW COLUMN GRANTS ON TABLE (database_name '.')? schema_name '.' table_name
        (FOR user_role_public)?
        limit_clause?
    ;

limit_clause
    : LIMIT limit
    ;

show_columns_command
    : SHOW COLUMNS FROM TABLE database_name '.' schema_name '.' table_name
        like_pattern?
        limit_clause?
    ;

like_pattern
    : LIKE p=string
    ;

show_constraints_command
    : SHOW CONSTRAINTS (PRIMARY KEYS | FOREIGN KEYS EXPORTED?)
        FROM TABLE
        (database_name '.')? schema_name '.' table_name
        limit_clause?
    ;

show_external_table_command
    : SHOW EXTERNAL TABLE (database_name '.')? schema_name '.' table_name PARTITION?
    ;

show_databases_command
    : SHOW DATABASES like_pattern? limit_clause?
    | SHOW DATABASES FROM DATA CATALOG
        (ACCOUNT string_list)?
        like_pattern?
        (IAM_ROLE (DEFAULT | string))?
        limit_clause?
    ;

show_functions_command
    : SHOW FUNCTIONS FROM SCHEMA (database_name '.')? schema_name
        like_pattern?
        limit_clause?
    ;

show_grants_command
    : SHOW GRANTS ON
    ;

show_model_command
    : SHOW MODEL (ALL | model_name)
    ;

show_datashares_command
    : SHOW DATASHARES like_pattern?
    ;

show_parameters_command
    : SHOW PARAMETERS OF (FUNCTION | PROCEDURE)
        (database_name '.')? schema_name '.' function_name '(' argtype_list ')'
        like_pattern?
    ;

show_policies_command
    : SHOW (RLS | MASKING) POLICIES
        ( ON (database_name '.')? schema_name '.' relation_name (FOR user_role_public)?
        | FROM DATABASE database_name
        )?
        limit_clause?
    ;

show_procedure_command
    : SHOW PROCEDURE sp_name sp_arg_list_in_parentheses?
    ;

show_procedures_command
    : SHOW PROCEDURES FROM SCHEMA (database_name '.')? schema_name
        like_pattern?
        limit_clause?
    ;

show_schemas_command
    : SHOW SCHEMAS FROM DATABASE database_name
        like_pattern?
        limit_clause?
    ;

show_table_command
    : SHOW TABLE (schema_name '.')? table_name
    ;

show_tables_command
    : SHOW TABLES FROM SCHEMA database_name '.' schema_name
        like_pattern?
        limit_clause?
    ;

show_view_command
    : SHOW VIEW (schema_name '.')? view_name
    ;

start_transaction_command
    : begin_command
    ;

truncate_command
    : TRUNCATE TABLE? table_name
    ;

unload_command
    : UNLOAD
    ;

update_command
    : todo UPDATE table_name
    ;

use_command
    : USE database_name
    ;

vacuum_command
    : VACUUM (FULL | SORT ONLY | DELETE ONLY | REINDEX | RECLUSTER)?
        vacuum_target?
    ;

vacuum_target
    : table_name? TO threshold PERCENT BOOST?
    | table_name (TO threshold PERCENT)? BOOST?
    ;

// Expression
expression
    : todo
    ;

predicate_expression
    : todo
    ;

// Basic elements
threshold
    : DECIMAL
    ;

interval
    : DECIMAL
    ;

limit
    : DECIMAL
    ;

numrows
    : DECIMAL
    ;

priority
    : DECIMAL
    ;

process_id
    : DECIMAL
    ;

string
    : STRING
    ;

string_list
    : string (',' string)*
    ;

value
    : string
    | DECIMAL
    ;

py_arg_name
    : id_
    ;

py_arg_data_type
    : id_
    ;

sql_arg_data_type
    : SMALLINT | INT2
    | INTEGER | INT | INT4
    | BIGINT | INT8
    | DECIMAL | NUMERIC
    | REAL | FLOAT4
    | DOUBLE PRECISION | FLOAT8 | FLOAT
    | CHAR | CHARACTER | NCHAR | BPCHAR
    | VARCHAR | CHARACTER VARYING | NVARCHAR | TEXT
    | DATE
    | TIME without_time_zone?
    | TIMETZ | TIME with_time_zone
    | TIMESTAMP without_time_zone?
    | TIMESTAMPTZ | TIMESTAMP with_time_zone
    | INTERVAL (YEAR TO MONTH | DAY TO SECOND)
    | BOOLEAN | BOOL
    | HLLSKETCH
    | SUPER
    | VARBYTE | VARBINARY | BINARY VARYING
    | GEOMETRY
    | GEOGRAPHY
    ;

without_time_zone
    : WITHOUT TIME ZONE
    ;

with_time_zone
    : WITH TIME ZONE
    ;

encode_type
    : RAW
    | AZ64
    | BYTEDICT
    | DELTA
    | DELTA32K
    | LZO
    | MOSTLY8
    | MOSTLY16
    | MOSTLY32
    | RUNLENGTH
    | TEXT255
    | TEXT32K
    | ZSTD
    ;

column_type
    : sql_arg_data_type
    ;

count
    : DECIMAL
    ;

//
query
    : todo
    ;

//IDs
id_
    : ID
    ;

awsdatacatalog_name
    : id_
    ;

catalog_name
    : id_
    ;

column_name
    : id_
    ;

constraint_name
    : id_
    ;

cursor_name
    : id_
    ;

database_name
    : id_
    ;

datashare_name
    : id_
    ;

external_id
    : id_
    ;

function_name
    : id_
    ;

group_name
    : id_
    ;

identity_provider_name
    : id_
    ;

library_name
    : id_
    ;

model_name
    : id_
    ;

owner
    : id_
    ;

new_owner
    : owner
    ;

parameter_name
    : id_
    ;

plan_name
    : id_
    ;

policy_name
    : id_
    ;

relation_name
    :
    ;

role_name
    : id_
    ;

schema_name
    : id_
    ;

sp_name
    : id_
    ;

table_name
    : id_
    ;

target_user
    : id_
    ;

user_name
    : id_
    ;

view_name
    : id_
    ;

//TODOs
todo
    : '.'
    ;