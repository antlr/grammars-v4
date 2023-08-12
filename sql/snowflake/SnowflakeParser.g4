/*
Snowflake Database grammar.
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

parser grammar SnowflakeParser;

options { tokenVocab=SnowflakeLexer; }

snowflake_file
    : batch? EOF
    ;

batch
    : sql_command (SEMI sql_command)* SEMI?
    ;

sql_command
    : ddl_command
    | dml_command
    | show_command
    | use_command
    | describe_command
    | other_command
    ;

ddl_command
    : alter_command
    | create_command
    | drop_command
    | undrop_command
    ;

dml_command
    : query_statement
    | insert_statement
    | insert_multi_table_statement
    | update_statement
    | delete_statement
    | merge_statement
    ;

insert_statement
    : INSERT OVERWRITE? INTO object_name ( '(' column_list ')' )?
        (values_builder | select_statement)
    ;

insert_multi_table_statement
    : INSERT OVERWRITE? ALL into_clause2
    | INSERT OVERWRITE? (FIRST | ALL)
        (WHEN predicate THEN into_clause2+)+
        (ELSE into_clause2)?
        subquery
    ;

into_clause2
    : INTO object_name ( '(' column_list ')')? values_list?
    ;

values_list
    : VALUES '(' value_item (COMMA value_item)* ')'
    ;

value_item
    : column_name
    | DEFAULT
    | NULL_
    ;

merge_statement
    : MERGE INTO object_name as_alias?
        USING table_source ON search_condition
        merge_matches
    ;

merge_matches
    : (WHEN MATCHED (AND search_condition )? THEN merge_update_delete)+
    (WHEN NOT MATCHED (AND search_condition )? THEN merge_insert)?
    ;

merge_update_delete
    : UPDATE SET column_name EQ expr (',' column_name EQ expr )*
    | DELETE
    ;

merge_insert
    : INSERT ( '(' column_list ')' )? VALUES '(' expr_list ')'
    ;

update_statement
    : UPDATE object_name as_alias?
        SET column_name EQ expr (COMMA column_name EQ expr)*
        (FROM table_sources)?
        (WHERE search_condition)?
    ;

table_or_query
    : object_name as_alias?
    | '(' subquery ')' as_alias?
    ;

delete_statement
    : DELETE FROM object_name as_alias?
        (USING table_or_query (COMMA table_or_query)? )?
        (WHERE search_condition)?
    ;

values_builder
    : VALUES '(' expr_list ')' (COMMA '(' expr_list ')' )?
    ;

other_command
    : copy_into_table
    | copy_into_location
    | comment
    | commit
    | execute_immediate
    | execute_task
    | explain
    | get_dml
    | grant_ownership
    | grant_to_role
    | grant_to_share
    | grant_role
    | list
    | put
    | remove
    | revoke_from_role
    | revoke_from_share
    | revoke_role
    | rollback
    | set
    | truncate_materialized_view
    | truncate_table
    | unset
    | call
    | begin_txn
    ;

begin_txn
    : BEGIN ( WORK | TRANSACTION )? ( NAME id_ )?
    | START TRANSACTION ( NAME id_ )?
    ;

copy_into_table
    : COPY INTO object_name
            FROM ( internal_stage | external_stage | external_location )
            files?
            pattern?
            file_format?
            copy_options*
            ( VALIDATION_MODE EQ (RETURN_N_ROWS | RETURN_ERRORS | RETURN_ALL_ERRORS) )?
//
    /* Data load with transformation */
    | COPY INTO object_name ( '(' column_list ')' )?
//           FROM '(' SELECT expr_list
//                    FROM ( internal_stage | external_stage ) ')'
            files?
            pattern?
            file_format?
            copy_options*
    ;

external_location
    //(for Amazon S3)
    : S3_PATH //'s3://<bucket>[/<path>]'
//        ( ( STORAGE_INTEGRATION EQ id_ )?
//        | ( CREDENTIALS EQ '(' ( AWS_KEY_ID EQ string AWS_SECRET_KEY EQ string ( AWS_TOKEN EQ string )? ) ')' )?
//        )?
//        [ ENCRYPTION = ( [ TYPE = 'AWS_CSE' ] [ MASTER_KEY = '<string>' ] |
//                   [ TYPE = 'AWS_SSE_S3' ] |
//                   [ TYPE = 'AWS_SSE_KMS' [ KMS_KEY_ID = '<string>' ] ] |
//                   [ TYPE = 'NONE' ] ) ]
    // (for Google Cloud Storage)
    | GCS_PATH //'gcs://<bucket>[/<path>]'
//        ( STORAGE_INTEGRATION EQ id_ )?
        //[ ENCRYPTION = ( [ TYPE = 'GCS_SSE_KMS' ] [ KMS_KEY_ID = '<string>' ] | [ TYPE = 'NONE' ] ) ]
    // (for Microsoft Azure)
    | AZURE_PATH //'azure://<account>.blob.core.windows.net/<container>[/<path>]'
//        (   ( STORAGE_INTEGRATION EQ id_ )?
//            | ( CREDENTIALS EQ '(' ( AZURE_SAS_TOKEN EQ string ) ')' )
//        )?
        //[ ENCRYPTION = ( [ TYPE = { 'AZURE_CSE' | 'NONE' } ] [ MASTER_KEY = '<string>' ] ) ]
    ;

files
    : FILES EQ '(' string (COMMA string)* ')'
    ;

file_format
    : FILE_FORMAT EQ '(' (format_name | format_type) ')'
    ;

format_name
    : FORMAT_NAME EQ string
    ;

format_type
    : TYPE EQ ( CSV | JSON | AVRO | ORC | PARQUET | XML ) format_type_options*
    ;

stage_file_format
    : STAGE_FILE_FORMAT EQ '(' ( FORMAT_NAME EQ string )
                         | (TYPE EQ ( CSV | JSON | AVRO | ORC | PARQUET | XML ) format_type_options+ )
                     ')'
    ;

copy_into_location
    : COPY INTO ( internal_stage | external_stage | external_location )
           FROM ( object_name | '(' query_statement ')' )
      partition_by?
      file_format?
      copy_options?
      ( VALIDATION_MODE EQ RETURN_ROWS )?
      HEADER?
    ;

comment
    : COMMENT if_exists? ON object_type_name object_name function_signature? IS string
    | COMMENT if_exists? ON COLUMN full_column_name IS string
    ;

function_signature
    : '(' data_type_list? ')'
    ;

commit
    : COMMIT WORK?
    ;

execute_immediate
    : EXECUTE IMMEDIATE (string | id_ | ID2) (USING '(' id_ (COMMA id_)* ')')?
    | EXECUTE IMMEDIATE DBL_DOLLAR
    ;

execute_task
    : EXECUTE TASK object_name
    ;

explain
    : EXPLAIN ( USING (TABULAR | JSON | TEXT) )? sql_command
    ;

parallel
    : PARALLEL EQ num
    ;

get_dml
    : GET internal_stage FILE_PATH
            parallel?
            pattern?
    ;

grant_ownership
    : GRANT OWNERSHIP
         ( ON ( object_type_name object_name | ALL object_type_plural IN ( DATABASE id_ | SCHEMA schema_name ) )
         | ON FUTURE object_type_plural IN ( DATABASE id_ | SCHEMA schema_name )
         )
         TO ROLE id_ ( ( REVOKE | COPY ) CURRENT GRANTS )?
    ;

grant_to_role
    : GRANT (
         ( global_privileges | ALL PRIVILEGES? ) ON ACCOUNT
         | ( account_object_privileges  | ALL PRIVILEGES? ) ON ( USER | RESOURCE MONITOR | WAREHOUSE | DATABASE | INTEGRATION ) object_name
         | ( schema_privileges          | ALL PRIVILEGES? ) ON ( SCHEMA schema_name | ALL SCHEMAS IN DATABASE id_ )
         | ( schema_privileges          | ALL PRIVILEGES? ) ON ( FUTURE SCHEMAS IN DATABASE id_ )
         | ( schema_object_privileges   | ALL PRIVILEGES? ) ON ( object_type object_name | ALL object_type_plural IN ( DATABASE id_ | SCHEMA schema_name ) )
         | ( schema_object_privileges   | ALL PRIVILEGES? ) ON FUTURE object_type_plural IN ( DATABASE id_ | SCHEMA schema_name )
        )
        TO ROLE? id_ (WITH GRANT OPTION)?
    ;

global_privileges
    : global_privilege (COMMA global_privilege)*
    ;

global_privilege
    : CREATE ( ACCOUNT
            | DATA EXCHANGE LISTING
            | DATABASE
            | INTEGRATION
            | NETWORK POLICY
            | ROLE
            | SHARE
            | USER
            | WAREHOUSE )
    | ( APPLY MASKING POLICY
      | APPLY ROW ACCESS POLICY
      | APPLY SESSION POLICY
      | APPLY TAG
      | ATTACH POLICY )
    | ( EXECUTE TASK
      | IMPORT SHARE
      | MANAGE GRANTS
      | MONITOR ( EXECUTION | USAGE )
      | OVERRIDE SHARE RESTRICTIONS )
    ;

account_object_privileges
    : account_object_privilege (COMMA account_object_privilege)*
    ;

account_object_privilege
    : MONITOR
    | MODIFY
    | USAGE
    | OPERATE
    | CREATE SCHEMA
    | IMPORTED PRIVILEGES
    | USE_ANY_ROLE
    ;

schema_privileges
    : schema_privilege (COMMA schema_privilege)*
    ;

schema_privilege
    : MODIFY
    | MONITOR
    | USAGE
    | CREATE ( TABLE
             | EXTERNAL TABLE
             | VIEW
             | MATERIALIZED VIEW
             | MASKING POLICY
             | ROW ACCESS POLICY
             | SESSION POLICY
             | TAG
             | SEQUENCE
             | FUNCTION
             | PROCEDURE
             | FILE FORMAT
             | STAGE
             | PIPE
             | STREAM
             | TASK )
    | ADD SEARCH OPTIMIZATION
    ;

schema_object_privileges
    : schema_object_privilege (COMMA schema_object_privilege)*
    ;

schema_object_privilege
    : SELECT
    | INSERT
    | UPDATE
    | DELETE
    | TRUNCATE
    | REFERENCES
    | USAGE
    | READ (COMMA WRITE)?
    | MONITOR
    | OPERATE
    | APPLY
    ;

grant_to_share
    : GRANT object_privilege ON
            ( DATABASE id_
            | SCHEMA id_
            | FUNCTION id_
            | ( TABLE object_name | ALL TABLES IN SCHEMA schema_name )
            | VIEW id_ )
        TO SHARE id_
    ;

object_privilege
    : USAGE
    | SELECT
    | REFERENCE_USAGE
    ;

grant_role
    : GRANT ROLE role_name TO (ROLE role_name | USER id_)
    ;

role_name
    : system_defined_role
    | id_
    ;

system_defined_role
    : ORGADMIN
    | ACCOUNTADMIN
    | SECURITYADMIN
    | USERADMIN
    | SYSADMIN
    | PUBLIC
    ;

list
    : LIST ( internal_stage | external_stage )
        pattern?
    ;

//    @[<namespace>.]<int_stage_name>[/<path>]
//  | @[<namespace>.]%<table_name>[/<path>]
//  | @~[/<path>]
internal_stage
    : AT id_ '/'?
    ;

//  @[<namespace>.]<ext_stage_name>[/<path>]
external_stage
    : AT id_ '/'?
    ;

put
    : PUT FILE_PATH internal_stage
         ( PARALLEL EQ num )?
         ( AUTO_COMPRESS EQ true_false )?
         ( SOURCE_COMPRESSION EQ (AUTO_DETECT | GZIP | BZ2 | BROTLI | ZSTD | DEFLATE | RAW_DEFLATE | NONE) )?
         ( OVERWRITE EQ true_false )?
    ;

remove
    : REMOVE ( internal_stage | external_stage )
         pattern?
    ;

revoke_from_role
    : REVOKE (GRANT OPTION FOR)?
          (
             ( global_privilege           | ALL  PRIVILEGES? ) ON ACCOUNT
           | ( account_object_privileges  | ALL  PRIVILEGES? ) ON ( RESOURCE MONITOR | WAREHOUSE | DATABASE | INTEGRATION ) object_name
           | ( schema_privileges          | ALL  PRIVILEGES? ) ON ( SCHEMA schema_name | ALL SCHEMAS IN DATABASE id_ )
           | ( schema_privileges          | ALL  PRIVILEGES? ) ON ( FUTURE SCHEMAS IN DATABASE <db_name> )
           | ( schema_object_privileges   | ALL  PRIVILEGES? ) ON ( object_type object_name | ALL object_type_plural IN SCHEMA schema_name )
           | ( schema_object_privileges   | ALL  PRIVILEGES? ) ON FUTURE object_type_plural IN ( DATABASE id_ | SCHEMA schema_name )
          )
        FROM ROLE? id_ cascade_restrict?
    ;

revoke_from_share
    : REVOKE object_privilege ON
           ( DATABASE id_
           | SCHEMA schema_name
           | ( TABLE object_name | ALL TABLES IN SCHEMA schema_name )
           | ( VIEW object_name | ALL VIEWS IN SCHEMA schema_name )  )
        FROM SHARE id_
    ;

revoke_role
    : REVOKE ROLE role_name FROM (ROLE role_name | USER id_)
    ;

rollback
    : ROLLBACK WORK?
    ;

set
    : SET id_ EQ expr
    | SET LR_BRACKET id_ ( COMMA id_ )* RR_BRACKET EQ LR_BRACKET expr ( COMMA expr )* RR_BRACKET
    ;

truncate_materialized_view
    : TRUNCATE MATERIALIZED VIEW object_name
    ;

truncate_table
    : TRUNCATE TABLE? if_exists? object_name
    ;

unset
    : UNSET id_
    | UNSET '(' id_ (COMMA id_)* ')'
    ;

// alter commands
alter_command
    : alter_account
    | alter_alert
    | alter_api_integration
    | alter_connection
    | alter_database
    | alter_dynamic_table
    //| alter_event_table // uses ALTER TABLE stmt
    | alter_external_table
    | alter_failover_group
    | alter_file_format
    | alter_function
    | alter_masking_policy
    | alter_materialized_view
    | alter_network_policy
    | alter_notification_integration
    | alter_pipe
    | alter_procedure
    | alter_replication_group
    | alter_resource_monitor
    | alter_role
    | alter_row_access_policy
    | alter_schema
    | alter_security_integration
    | alter_security_integration_external_oauth
    | alter_security_integration_snowflake_oauth
    | alter_security_integration_saml2
    | alter_security_integration_scim
    | alter_sequence
    | alter_session
    | alter_session_policy
    | alter_share
    | alter_stage
    | alter_storage_integration
    | alter_stream
    | alter_table
    | alter_table_alter_column
    | alter_tag
    | alter_task
    | alter_user
    | alter_view
    | alter_warehouse
    ;

account_params
    : ALLOW_ID_TOKEN EQ true_false
    | CLIENT_ENCRYPTION_KEY_SIZE EQ num
    | ENFORCE_SESSION_POLICY EQ true_false
    | EXTERNAL_OAUTH_ADD_PRIVILEGED_ROLES_TO_BLOCKED_LIST EQ true_false
    | INITIAL_REPLICATION_SIZE_LIMIT_IN_TB EQ num
    | NETWORK_POLICY EQ string
    | PERIODIC_DATA_REKEYING EQ true_false
    | PREVENT_UNLOAD_TO_INLINE_URL EQ true_false
    | PREVENT_UNLOAD_TO_INTERNAL_STAGES EQ true_false
    | REQUIRE_STORAGE_INTEGRATION_FOR_STAGE_CREATION EQ true_false
    | REQUIRE_STORAGE_INTEGRATION_FOR_STAGE_OPERATION EQ true_false
    | SAML_IDENTITY_PROVIDER EQ json_literal
    | SESSION_POLICY EQ string
    | SSO_LOGIN_PAGE EQ true_false
    ;

object_params
    : DATA_RETENTION_TIME_IN_DAYS EQ num
    | MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num
    | default_ddl_collation
    | MAX_CONCURRENCY_LEVEL EQ num
    | NETWORK_POLICY EQ string
    | PIPE_EXECUTION_PAUSED EQ true_false
    | SESSION_POLICY EQ string
    | STATEMENT_QUEUED_TIMEOUT_IN_SECONDS EQ num
    | STATEMENT_TIMEOUT_IN_SECONDS EQ num
    ;

default_ddl_collation
    : DEFAULT_DDL_COLLATION_ EQ string
    ;

object_properties
    : PASSWORD EQ string
    | LOGIN_NAME EQ string
    | DISPLAY_NAME EQ string
    | FIRST_NAME EQ string
    | MIDDLE_NAME EQ string
    | LAST_NAME EQ string
    | EMAIL EQ string
    | MUST_CHANGE_PASSWORD EQ true_false
    | DISABLED EQ true_false
    | DAYS_TO_EXPIRY EQ num
    | MINS_TO_UNLOCK EQ num
    | DEFAULT_WAREHOUSE EQ string
    | DEFAULT_NAMESPACE EQ string
    | DEFAULT_ROLE EQ string
    //| DEFAULT_SECONDARY_ROLES EQ '(' 'ALL' ')'
    | MINS_TO_BYPASS_MFA EQ num
    | RSA_PUBLIC_KEY EQ string
    | RSA_PUBLIC_KEY_2 EQ string
    | comment_clause
    ;

session_params
    : ABORT_DETACHED_QUERY EQ true_false
    | AUTOCOMMIT EQ true_false
    | BINARY_INPUT_FORMAT EQ string
    | BINARY_OUTPUT_FORMAT EQ string
    | DATE_INPUT_FORMAT EQ string
    | DATE_OUTPUT_FORMAT EQ string
    | ERROR_ON_NONDETERMINISTIC_MERGE EQ true_false
    | ERROR_ON_NONDETERMINISTIC_UPDATE EQ true_false
    | JSON_INDENT EQ num
    | LOCK_TIMEOUT EQ num
    | QUERY_TAG EQ string
    | ROWS_PER_RESULTSET EQ num
    | SIMULATED_DATA_SHARING_CONSUMER EQ string
    | STATEMENT_TIMEOUT_IN_SECONDS EQ num
    | STRICT_JSON_OUTPUT EQ true_false
    | TIMESTAMP_DAY_IS_ALWAYS_24H EQ true_false
    | TIMESTAMP_INPUT_FORMAT EQ string
    | TIMESTAMP_LTZ_OUTPUT_FORMAT EQ string
    | TIMESTAMP_NTZ_OUTPUT_FORMAT EQ string
    | TIMESTAMP_OUTPUT_FORMAT EQ string
    | TIMESTAMP_TYPE_MAPPING EQ string
    | TIMESTAMP_TZ_OUTPUT_FORMAT EQ string
    | TIMEZONE EQ string
    | TIME_INPUT_FORMAT EQ string
    | TIME_OUTPUT_FORMAT EQ string
    | TRANSACTION_DEFAULT_ISOLATION_LEVEL EQ string
    | TWO_DIGIT_CENTURY_START EQ num
    | UNSUPPORTED_DDL_ACTION EQ string
    | USE_CACHED_RESULT EQ true_false
    | WEEK_OF_YEAR_POLICY EQ num
    | WEEK_START EQ num
    ;

alter_account
    : ALTER ACCOUNT alter_account_opts
    ;

enabled_true_false
    : ENABLED EQ true_false
    ;

alter_alert
    : ALTER ALERT if_exists? id_ ( resume_suspend
                                 | SET alert_set_clause+
                                 | UNSET alert_unset_clause+
                                 | MODIFY CONDITION EXISTS '(' alert_condition ')'
                                 | MODIFY ACTION alert_action
                                 )
    ;

resume_suspend
    : RESUME
    | SUSPEND
    ;

alert_set_clause
    : WAREHOUSE EQ id_
    | SCHEDULE EQ string
    | comment_clause
    ;

alert_unset_clause
    : WAREHOUSE
    | SCHEDULE
    | COMMENT
    ;

alter_api_integration
    : ALTER API? INTEGRATION if_exists? id_ SET
        ( API_AWS_ROLE_ARN EQ string )?
        ( AZURE_AD_APPLICATION_ID EQ string )?
        ( API_KEY EQ string )?
        enabled_true_false?
        ( API_ALLOWED_PREFIXES EQ '(' string ')' )?
        ( API_BLOCKED_PREFIXES EQ '(' string ')' )?
        comment_clause?

    |  ALTER API? INTEGRATION id_ set_tags
    |  ALTER API? INTEGRATION id_ unset_tags
    |  ALTER API? INTEGRATION if_exists? id_ UNSET api_integration_property (COMMA api_integration_property)*
    ;

api_integration_property
    : API_KEY
    | ENABLED
    | API_BLOCKED_PREFIXES
    | COMMENT
    ;

alter_connection
    : ALTER CONNECTION alter_connection_opts
    ;

alter_database
    : ALTER DATABASE if_exists? id_ RENAME TO id_
    | ALTER DATABASE if_exists? id_ SWAP WITH id_
    | ALTER DATABASE if_exists? id_ SET ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
                                        ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
                                        default_ddl_collation?
                                        comment_clause?
    | ALTER DATABASE id_ set_tags
    | ALTER DATABASE id_ unset_tags
    | ALTER DATABASE if_exists? id_ UNSET database_property (COMMA database_property)*
    | ALTER DATABASE id_ ENABLE REPLICATION TO ACCOUNTS account_id_list (IGNORE EDITION CHECK)?
    | ALTER DATABASE id_ DISABLE REPLICATION ( TO ACCOUNTS account_id_list )?
    | ALTER DATABASE id_ REFRESH
    // Database Failover
    | ALTER DATABASE id_ ENABLE FAILOVER TO ACCOUNTS account_id_list
    | ALTER DATABASE id_ DISABLE FAILOVER ( TO ACCOUNTS account_id_list )?
    | ALTER DATABASE id_ PRIMARY
    ;

database_property
    : DATA_RETENTION_TIME_IN_DAYS
    | MAX_DATA_EXTENSION_TIME_IN_DAYS
    | DEFAULT_DDL_COLLATION_
    | COMMENT
    ;

account_id_list
    : account_identifier (COMMA account_identifier)*
    ;

alter_dynamic_table
    : ALTER DYNAMIC TABLE id_ ( resume_suspend
                              | REFRESH
                              | SET WAREHOUSE EQ id_
                              )
    ;

alter_external_table
    : ALTER EXTERNAL TABLE if_exists? object_name REFRESH string?
    | ALTER EXTERNAL TABLE if_exists? object_name ADD FILES '(' string_list ')'
    | ALTER EXTERNAL TABLE if_exists? object_name REMOVE FILES '(' string_list ')'
    | ALTER EXTERNAL TABLE if_exists? object_name SET
        ( AUTO_REFRESH EQ true_false )?
        tag_decl_list?
    | ALTER EXTERNAL TABLE if_exists? object_name unset_tags
      //Partitions added and removed manually
    | ALTER EXTERNAL TABLE object_name if_exists? ADD PARTITION '(' column_name EQ string (COMMA column_name EQ string)* ')' LOCATION string
    | ALTER EXTERNAL TABLE object_name if_exists? DROP PARTITION LOCATION string
    ;

ignore_edition_check
    : IGNORE EDITION CHECK
    ;

replication_schedule
    : REPLICATION_SCHEDULE EQ string
    ;

db_name_list
    : id_ (COMMA id_)*
    ;

share_name_list
    : id_ (COMMA id_)*
    ;

full_acct_list
    : full_acct (COMMA full_acct)*
    ;

alter_failover_group
    //Source Account
    : ALTER FAILOVER GROUP if_exists? id_ RENAME TO id_
    | ALTER FAILOVER GROUP if_exists? id_ SET ( OBJECT_TYPES EQ object_type_list )? replication_schedule?
    | ALTER FAILOVER GROUP if_exists? id_ SET
        OBJECT_TYPES EQ object_type_list
//        ALLOWED_INTEGRATION_TYPES EQ <integration_type_name> [ , <integration_type_name> ... ] ]
        replication_schedule?
    | ALTER FAILOVER GROUP if_exists? id_ ADD db_name_list TO ALLOWED_DATABASES
    | ALTER FAILOVER GROUP if_exists? id_ MOVE DATABASES db_name_list TO FAILOVER GROUP id_
    | ALTER FAILOVER GROUP if_exists? id_ REMOVE db_name_list FROM ALLOWED_DATABASES
    | ALTER FAILOVER GROUP if_exists? id_ ADD share_name_list TO ALLOWED_SHARES
    | ALTER FAILOVER GROUP if_exists? id_ MOVE SHARES share_name_list TO FAILOVER GROUP id_
    | ALTER FAILOVER GROUP if_exists? id_ REMOVE share_name_list FROM ALLOWED_SHARES
    | ALTER FAILOVER GROUP if_exists? id_ ADD full_acct_list TO ALLOWED_ACCOUNTS ignore_edition_check?
    | ALTER FAILOVER GROUP if_exists? id_ REMOVE full_acct_list FROM ALLOWED_ACCOUNTS
      //Target Account
    | ALTER FAILOVER GROUP if_exists? id_ ( REFRESH | PRIMARY | SUSPEND | RESUME )
    ;

alter_file_format
    : ALTER FILE FORMAT if_exists? id_ RENAME TO id_
    | ALTER FILE FORMAT if_exists? id_ SET ( format_type_options* comment_clause? )
    ;

alter_function
    : alter_function_signature RENAME TO id_
    | alter_function_signature SET comment_clause
    | alter_function_signature SET SECURE
    | alter_function_signature UNSET ( SECURE | COMMENT )
    // External Functions
    | alter_function_signature SET API_INTEGRATION EQ id_
    | alter_function_signature SET HEADERS EQ '(' header_decl* ')'
    | alter_function_signature SET CONTEXT_HEADERS EQ '(' id_* ')'
    | alter_function_signature SET MAX_BATCH_ROWS EQ num
    | alter_function_signature SET COMPRESSION EQ compression_type
    | alter_function_signature SET ( REQUEST_TRANSLATOR | RESPONSE_TRANSLATOR ) EQ id_
    | alter_function_signature UNSET ( COMMENT | HEADERS | CONTEXT_HEADERS | MAX_BATCH_ROWS | COMPRESSION | SECURE | REQUEST_TRANSLATOR | RESPONSE_TRANSLATOR )
    ;

alter_function_signature
    : ALTER FUNCTION if_exists? id_ '(' data_type_list? ')'
    ;

data_type_list
    : data_type (COMMA data_type)*
    ;

alter_masking_policy
    : ALTER MASKING POLICY if_exists? id_ SET BODY ARROW expr
    | ALTER MASKING POLICY if_exists? id_ RENAME TO id_
    | ALTER MASKING POLICY if_exists? id_ SET comment_clause
    ;

alter_materialized_view
    : ALTER MATERIALIZED VIEW id_ (
        RENAME TO id_
        | CLUSTER BY '(' expr_list ')'
        | DROP CLUSTERING KEY
        | resume_suspend RECLUSTER?
        | SET (
            SECURE?
            comment_clause? )
        | UNSET (
            SECURE
            | COMMENT )
        )
    ;

alter_network_policy
    : ALTER NETWORK POLICY alter_network_policy_opts
    ;

alter_notification_integration
    : ALTER NOTIFICATION? INTEGRATION if_exists? id_ SET
        enabled_true_false?
        cloud_provider_params_auto
        comment_clause?
    // Push notifications
    | ALTER NOTIFICATION? INTEGRATION if_exists? id_ SET
        enabled_true_false?
        cloud_provider_params_push
        comment_clause?
    | ALTER NOTIFICATION? INTEGRATION id_ set_tags
    | ALTER NOTIFICATION? INTEGRATION id_ unset_tags
    | ALTER NOTIFICATION? INTEGRATION if_exists id_ UNSET (ENABLED | COMMENT)
    ;

alter_pipe
    : ALTER PIPE if_exists? id_ SET ( object_properties? comment_clause? )
    | ALTER PIPE id_ set_tags
    | ALTER PIPE id_ unset_tags
    | ALTER PIPE if_exists? id_ UNSET PIPE_EXECUTION_PAUSED EQ true_false
    | ALTER PIPE if_exists? id_ UNSET COMMENT
    | ALTER PIPE if_exists? id_ REFRESH  ( PREFIX EQ string )? ( MODIFIED_AFTER EQ string )?
    ;

alter_procedure
    : ALTER PROCEDURE if_exists? id_ '(' data_type_list? ')' RENAME TO id_
    | ALTER PROCEDURE if_exists? id_ '(' data_type_list? ')' SET comment_clause
    | ALTER PROCEDURE if_exists? id_ '(' data_type_list? ')' UNSET COMMENT
    | ALTER PROCEDURE if_exists? id_ '(' data_type_list? ')' EXECUTE AS caller_owner
    ;

alter_replication_group
    //Source Account
    : ALTER REPLICATION GROUP if_exists? id_ RENAME TO id_
    | ALTER REPLICATION GROUP if_exists? id_ SET
        ( OBJECT_TYPES EQ object_type_list )?
        ( REPLICATION_SCHEDULE EQ string )?
    | ALTER REPLICATION GROUP if_exists? id_ SET
        OBJECT_TYPES EQ object_type_list
        ALLOWED_INTEGRATION_TYPES EQ integration_type_name (COMMA integration_type_name)*
        ( REPLICATION_SCHEDULE EQ string )?
    | ALTER REPLICATION GROUP if_exists? id_ ADD db_name_list TO ALLOWED_DATABASES
    | ALTER REPLICATION GROUP if_exists? id_ MOVE DATABASES db_name_list TO REPLICATION GROUP id_
    | ALTER REPLICATION GROUP if_exists? id_ REMOVE db_name_list FROM ALLOWED_DATABASES
    | ALTER REPLICATION GROUP if_exists? id_ ADD share_name_list TO ALLOWED_SHARES
    | ALTER REPLICATION GROUP if_exists? id_ MOVE SHARES share_name_list TO REPLICATION GROUP id_
    | ALTER REPLICATION GROUP if_exists? id_ REMOVE share_name_list FROM ALLOWED_SHARES
    | ALTER REPLICATION GROUP if_exists? id_ ADD account_id_list TO ALLOWED_ACCOUNTS
        ignore_edition_check?
    | ALTER REPLICATION GROUP if_exists? id_ REMOVE account_id_list FROM ALLOWED_ACCOUNTS
    //Target Account
    | ALTER REPLICATION GROUP if_exists? id_ REFRESH
    | ALTER REPLICATION GROUP if_exists? id_ SUSPEND
    | ALTER REPLICATION GROUP if_exists? id_ RESUME
    ;

credit_quota
    : CREDIT_QUOTA EQ num
    ;

frequency
    : FREQUENCY EQ ( MONTHLY | DAILY | WEEKLY | YEARLY | NEVER )
    ;

notify_users
    : NOTIFY_USERS EQ LR_BRACKET id_ (COMMA id_)* RR_BRACKET
    ;

triggerDefinition
    : ON num PERCENT DO ( SUSPEND | SUSPEND_IMMEDIATE | NOTIFY )
    ;

alter_resource_monitor
    : ALTER RESOURCE MONITOR if_exists? id_
        (
            SET
                credit_quota?
                frequency?
                ( START_TIMESTAMP EQ LR_BRACKET string | IMMEDIATELY RR_BRACKET )?
                ( END_TIMESTAMP EQ string )?
        )?
         (
            notify_users
            ( TRIGGERS triggerDefinition (COMMA triggerDefinition)* )?
         )?
    ;

alter_role
    : ALTER ROLE if_exists? id_ RENAME TO id_
    | ALTER ROLE if_exists? id_ SET comment_clause
    | ALTER ROLE if_exists? id_ UNSET COMMENT
    | ALTER ROLE if_exists? id_ set_tags
    | ALTER ROLE if_exists? id_ unset_tags
    ;

alter_row_access_policy
    : ALTER ROW ACCESS POLICY if_exists? id_ SET BODY ARROW expr
    | ALTER ROW ACCESS POLICY if_exists? id_ RENAME TO id_
    | ALTER ROW ACCESS POLICY if_exists? id_ SET comment_clause
    ;

alter_schema
    : ALTER SCHEMA if_exists? schema_name RENAME TO schema_name
    | ALTER SCHEMA if_exists? schema_name SWAP WITH schema_name
    | ALTER SCHEMA if_exists? schema_name SET (
                                            ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
                                            ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
                                            default_ddl_collation?
                                            comment_clause?
                                         )
    | ALTER SCHEMA if_exists? schema_name set_tags
    | ALTER SCHEMA if_exists? schema_name unset_tags
    | ALTER SCHEMA if_exists? schema_name UNSET schema_property (COMMA schema_property)*
    | ALTER SCHEMA if_exists? schema_name ( ENABLE | DISABLE) MANAGED ACCESS
    ;

schema_property
    : DATA_RETENTION_TIME_IN_DAYS
    | MAX_DATA_EXTENSION_TIME_IN_DAYS
    | DEFAULT_DDL_COLLATION_
    | COMMENT
    ;

alter_security_integration
    : ALTER SEQUENCE if_exists? object_name RENAME TO object_name
    | ALTER SEQUENCE if_exists? object_name SET? ( INCREMENT BY? EQ? num )?
    | ALTER SEQUENCE if_exists? object_name SET comment_clause
    | ALTER SEQUENCE if_exists? object_name UNSET COMMENT
    ;

alter_security_integration_external_oauth
    : ALTER SECURITY? INTEGRATION if_exists id_ SET
        ( TYPE EQ EXTERNAL_OAUTH )?
        ( ENABLED EQ true_false )?
        ( EXTERNAL_OAUTH_TYPE EQ ( OKTA | AZURE | PING_FEDERATE | CUSTOM ) )?
        ( EXTERNAL_OAUTH_ISSUER EQ string )?
        ( EXTERNAL_OAUTH_TOKEN_USER_MAPPING_CLAIM EQ (string | '(' string_list ')') )?
        ( EXTERNAL_OAUTH_SNOWFLAKE_USER_MAPPING_ATTRIBUTE EQ string )?
        ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ string )? // For OKTA | PING_FEDERATE | CUSTOM
        ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ (string | '(' string_list ')') )? // For Azure
        ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY EQ string )?
        ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY_2 EQ string )?
        ( EXTERNAL_OAUTH_BLOCKED_ROLES_LIST EQ '(' string_list ')' )?
        ( EXTERNAL_OAUTH_ALLOWED_ROLES_LIST EQ '(' string_list ')' )?
        ( EXTERNAL_OAUTH_AUDIENCE_LIST EQ '(' string ')' )?
        ( EXTERNAL_OAUTH_ANY_ROLE_MODE EQ (DISABLE | ENABLE | ENABLE_FOR_PRIVILEGE) )?
        ( EXTERNAL_OAUTH_ANY_ROLE_MODE EQ string )? // Only for EXTERNAL_OAUTH_TYPE EQ CUSTOM
    | ALTER SECURITY? INTEGRATION if_exists? id_ UNSET security_integration_external_oauth_property (COMMA security_integration_external_oauth_property)*
    | ALTER SECURITY? INTEGRATION id_ set_tags
    | ALTER SECURITY? INTEGRATION id_ unset_tags
    ;

security_integration_external_oauth_property
    : ENABLED
    | NETWORK_POLICY
    | OAUTH_CLIENT_RSA_PUBLIC_KEY
    | OAUTH_CLIENT_RSA_PUBLIC_KEY_2
    | OAUTH_USE_SECONDARY_ROLES EQ (IMPLICIT | NONE)
    | COMMENT
    ;

alter_security_integration_snowflake_oauth
    : ALTER SECURITY? INTEGRATION if_exists? id_ SET
        ( TYPE EQ EXTERNAL_OAUTH )?
        enabled_true_false?
        ( EXTERNAL_OAUTH_TYPE EQ ( OKTA | AZURE | PING_FEDERATE | CUSTOM ) )?
        ( EXTERNAL_OAUTH_ISSUER EQ string )?
        ( EXTERNAL_OAUTH_TOKEN_USER_MAPPING_CLAIM EQ (string | '(' string_list ')') )?
        ( EXTERNAL_OAUTH_SNOWFLAKE_USER_MAPPING_ATTRIBUTE EQ string )?
        ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ string )? // For OKTA | PING_FEDERATE | CUSTOM
        ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ ( string | '(' string_list ')' ) )? // For Azure
        ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY EQ string )?
        ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY_2 EQ string )?
        ( EXTERNAL_OAUTH_BLOCKED_ROLES_LIST EQ '(' string_list ')' )?
        ( EXTERNAL_OAUTH_ALLOWED_ROLES_LIST EQ '(' string_list ')' )?
        ( EXTERNAL_OAUTH_AUDIENCE_LIST EQ '(' string ')' )?
        ( EXTERNAL_OAUTH_ANY_ROLE_MODE EQ DISABLE | ENABLE | ENABLE_FOR_PRIVILEGE )?
        ( EXTERNAL_OAUTH_SCOPE_DELIMITER EQ string ) // Only for EXTERNAL_OAUTH_TYPE EQ CUSTOM
    | ALTER SECURITY? INTEGRATION if_exists? id_ UNSET security_integration_snowflake_oauth_property (COMMA security_integration_snowflake_oauth_property)*
    | ALTER SECURITY? INTEGRATION id_ set_tags
    | ALTER SECURITY? INTEGRATION id_ unset_tags
    ;

security_integration_snowflake_oauth_property
    : ENABLED
    | EXTERNAL_OAUTH_AUDIENCE_LIST
    ;

alter_security_integration_saml2
    : ALTER SECURITY? INTEGRATION if_exists? id_ SET
          ( TYPE EQ SAML2 )?
          enabled_true_false?
          ( SAML2_ISSUER EQ string )?
          ( SAML2_SSO_URL EQ string )?
          ( SAML2_PROVIDER EQ string )?
          ( SAML2_X509_CERT EQ string )?
          ( SAML2_SP_INITIATED_LOGIN_PAGE_LABEL EQ string )?
          ( SAML2_ENABLE_SP_INITIATED EQ true_false )?
          ( SAML2_SNOWFLAKE_X509_CERT EQ string )?
          ( SAML2_SIGN_REQUEST EQ true_false )?
          ( SAML2_REQUESTED_NAMEID_FORMAT EQ string )?
          ( SAML2_POST_LOGOUT_REDIRECT_URL EQ string )?
          ( SAML2_FORCE_AUTHN EQ true_false )?
          ( SAML2_SNOWFLAKE_ISSUER_URL EQ string )?
          ( SAML2_SNOWFLAKE_ACS_URL EQ string )?
    | ALTER SECURITY? INTEGRATION if_exists? id_ UNSET ENABLED
    | ALTER SECURITY? INTEGRATION id_ set_tags
    | ALTER SECURITY? INTEGRATION id_ unset_tags
    ;

alter_security_integration_scim
    : ALTER SECURITY? INTEGRATION if_exists? id_ SET
          ( NETWORK_POLICY EQ string )?
          ( SYNC_PASSWORD EQ true_false )?
          comment_clause?
    | ALTER SECURITY? INTEGRATION if_exists? id_ UNSET security_integration_scim_property (COMMA security_integration_scim_property)*
    | ALTER SECURITY? INTEGRATION id_ set_tags
    | ALTER SECURITY? INTEGRATION id_ unset_tags
    ;

security_integration_scim_property
    : NETWORK_POLICY
    | SYNC_PASSWORD
    | COMMENT
    ;

alter_sequence
    : ALTER SEQUENCE if_exists? id_ RENAME TO id_
    | ALTER SEQUENCE if_exists? id_ SET? ( INCREMENT BY? EQ? num )?
    | ALTER SEQUENCE if_exists? id_ SET comment_clause
    | ALTER SEQUENCE if_exists? id_ UNSET COMMENT
    ;

alter_session
    : ALTER SESSION SET session_params
    | ALTER SESSION UNSET param_name (COMMA param_name)*
    ;

alter_session_policy
    : ALTER SESSION POLICY if_exists? id_ (UNSET | SET) ( SESSION_IDLE_TIMEOUT_MINS EQ num )?
                                                        ( SESSION_UI_IDLE_TIMEOUT_MINS EQ num )?
                                                        comment_clause?
    | ALTER SESSION POLICY if_exists? id_ RENAME TO id_
    ;

alter_share
    : ALTER SHARE if_exists? id_ ( ADD | REMOVE ) ACCOUNTS EQ id_ (COMMA id_)*     ( SHARE_RESTRICTIONS EQ true_false )?
    | ALTER SHARE if_exists? id_ ADD ACCOUNTS EQ id_ (COMMA id_)*                  ( SHARE_RESTRICTIONS EQ true_false )?
    | ALTER SHARE if_exists? id_ SET (ACCOUNTS EQ id_ (COMMA id_)* )? comment_clause?
    | ALTER SHARE if_exists? id_ set_tags
    | ALTER SHARE id_ unset_tags
    | ALTER SHARE if_exists? id_ UNSET COMMENT
    ;

alter_storage_integration
    : ALTER STORAGE? INTEGRATION if_exists? id_ SET
        cloud_provider_params2?
        enabled_true_false?
        ( STORAGE_ALLOWED_LOCATIONS EQ '(' string_list ')' )?
        ( STORAGE_BLOCKED_LOCATIONS EQ '(' string_list ')' )?
        comment_clause?
    | ALTER STORAGE? INTEGRATION if_exists? id_ set_tags
    | ALTER STORAGE? INTEGRATION id_ unset_tags
    | ALTER STORAGE? INTEGRATION if_exists? id_ UNSET (
                                                                ENABLED                   |
                                                                STORAGE_BLOCKED_LOCATIONS |
                                                                COMMENT
                                                                )
                                                                //[ , ... ]
    ;

alter_stream
    : ALTER STREAM if_exists? id_ SET
        tag_decl_list?
        comment_clause?
    | ALTER STREAM if_exists? id_ set_tags
    | ALTER STREAM id_ unset_tags
    | ALTER STREAM if_exists? id_ UNSET COMMENT
    ;

alter_table
    : ALTER TABLE if_exists? object_name RENAME TO object_name
    | ALTER TABLE if_exists? object_name SWAP WITH object_name
    | ALTER TABLE if_exists? object_name ( clustering_action | table_column_action | constraint_action )
    | ALTER TABLE if_exists? object_name ext_table_column_action
    | ALTER TABLE if_exists? object_name search_optimization_action
    | ALTER TABLE if_exists? object_name SET
        stage_file_format?
        ( STAGE_COPY_OPTIONS EQ '(' copy_options ')' )?
        ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
        ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
        ( CHANGE_TRACKING EQ true_false )?
        default_ddl_collation?
        comment_clause?
    | ALTER TABLE if_exists? object_name set_tags
    | ALTER TABLE if_exists? object_name unset_tags
    | ALTER TABLE if_exists? object_name UNSET (
                                             DATA_RETENTION_TIME_IN_DAYS         |
                                             MAX_DATA_EXTENSION_TIME_IN_DAYS     |
                                             CHANGE_TRACKING                     |
                                             DEFAULT_DDL_COLLATION_              |
                                             COMMENT                             |
                                             )
                                             //[ , ... ]
    | ALTER TABLE if_exists? object_name ADD ROW ACCESS POLICY id_ ON column_list_in_parentheses
    | ALTER TABLE if_exists? object_name DROP ROW ACCESS POLICY id_
    | ALTER TABLE if_exists? object_name DROP ROW ACCESS POLICY id_ COMMA ADD ROW ACCESS POLICY id_ ON column_list_in_parentheses
    | ALTER TABLE if_exists? object_name DROP ALL ROW ACCESS POLICIES
    ;

clustering_action
    : CLUSTER BY '(' expr_list ')'
    | RECLUSTER ( MAX_SIZE EQ num )? ( WHERE expr )?
    | resume_suspend RECLUSTER
    | DROP CLUSTERING KEY
    ;

table_column_action
    : ADD COLUMN? column_name data_type
        ( DEFAULT expr | ( AUTOINCREMENT | IDENTITY ) (  '(' num COMMA num ')' | START num INCREMENT num  )? )?
        inline_constraint?
        ( WITH? MASKING POLICY id_ ( USING '(' column_name COMMA column_list ')' )? )?
    | RENAME COLUMN column_name TO column_name
    | alter_modify '('?
                                COLUMN? column_name DROP DEFAULT
                          COMMA COLUMN? column_name SET DEFAULT object_name DOT NEXTVAL
                          COMMA COLUMN? column_name ( SET? NOT NULL_ | DROP NOT NULL_ )
                          COMMA COLUMN? column_name ( ( SET DATA )? TYPE )? data_type
                          COMMA COLUMN? column_name comment_clause
                          COMMA COLUMN? column_name UNSET COMMENT
                      //  [ COMMA COLUMN? column_name ... ]
                      //  [ , ... ]
                   ')'?
    | alter_modify COLUMN column_name SET MASKING POLICY id_ ( USING '(' column_name COMMA column_list ')' )?
                                                                        FORCE?
    | alter_modify COLUMN column_name UNSET MASKING POLICY
    | alter_modify column_set_tags (COMMA column_set_tags)*
    | alter_modify column_unset_tags (COMMA column_unset_tags)*
    | DROP COLUMN? column_list
    ;

inline_constraint
    : null_not_null? (CONSTRAINT id_)?
    (
        ( UNIQUE | primary_key ) common_constraint_properties*
        | foreign_key REFERENCES object_name ( LR_BRACKET column_name RR_BRACKET )? constraint_properties
    )
    ;

enforced_not_enforced
    : NOT? ENFORCED
    ;

deferrable_not_deferrable
    : NOT? DEFERRABLE
    ;

initially_deferred_or_immediate
    : INITIALLY ( DEFERRED | IMMEDIATE )
    ;

//TODO : Some properties are mutualy exclusive ie INITIALLY DEFERRED is not compatible with NOT DEFERRABLE
// also VALIDATE | NOVALIDATE need to be after ENABLE or ENFORCED. Lot of case to handle :)
common_constraint_properties
    : enforced_not_enforced ( VALIDATE | NOVALIDATE )?
    | deferrable_not_deferrable
    | initially_deferred_or_immediate
    | ( ENABLE | DISABLE ) ( VALIDATE | NOVALIDATE )?
    | RELY
    | NORELY
    ;

on_update
    : ON UPDATE on_action
    ;

on_delete
    : ON DELETE on_action
    ;

foreign_key_match
    : MATCH match_type=( FULL | PARTIAL | SIMPLE )
    ;

on_action
    : CASCADE
    | SET ( NULL_ | DEFAULT )
    | RESTRICT
    | NO ACTION
    ;

constraint_properties
    : common_constraint_properties*
    | foreign_key_match
    | foreign_key_match? ( on_update on_delete? | on_delete on_update? )
    ;

ext_table_column_action
    : ADD COLUMN? column_name data_type AS '(' expr ')'
    | RENAME COLUMN column_name TO column_name
    | DROP COLUMN? column_list
    ;

constraint_action
    : ADD out_of_line_constraint
    | RENAME CONSTRAINT id_ TO id_
    | alter_modify ( CONSTRAINT id_ | primary_key | UNIQUE | foreign_key ) column_list_in_parentheses
                         enforced_not_enforced? ( VALIDATE | NOVALIDATE ) ( RELY | NORELY )
    | DROP ( CONSTRAINT id_ | primary_key | UNIQUE | foreign_key ) column_list_in_parentheses
                         cascade_restrict?
    | DROP PRIMARY KEY

    ;

search_optimization_action
    : ADD SEARCH OPTIMIZATION (
            ON search_method_with_target (COMMA search_method_with_target )*
     )?
    | DROP SEARCH OPTIMIZATION (
            ON search_method_with_target (COMMA search_method_with_target )*
     )?
    ;

search_method_with_target
    : (EQUALITY | SUBSTRING | GEO) '(' (STAR | expr) ')'
    ;

alter_table_alter_column
    : ALTER TABLE object_name alter_modify ('(' alter_column_decl_list ')' | alter_column_decl_list )
    | ALTER TABLE object_name alter_modify COLUMN column_name SET MASKING POLICY id_ ( USING '(' column_name COMMA column_list ')' )? FORCE?
    | ALTER TABLE object_name alter_modify COLUMN column_name UNSET MASKING POLICY
    | ALTER TABLE object_name alter_modify column_set_tags (COMMA column_set_tags)*
    | ALTER TABLE object_name alter_modify column_unset_tags (COMMA column_unset_tags)*
    ;

alter_column_decl_list
    : alter_column_decl (COMMA alter_column_decl)*
    ;

alter_column_decl
    : COLUMN? column_name alter_column_opts
    ;

alter_column_opts
    : DROP DEFAULT
    | SET DEFAULT object_name DOT NEXTVAL
    | ( SET? NOT NULL_ | DROP NOT NULL_ )
    | ( (SET DATA)? TYPE )? data_type
    | comment_clause
    | UNSET COMMENT
    ;

column_set_tags
    : COLUMN? column_name set_tags
    ;

column_unset_tags
    : COLUMN column_name unset_tags
    ;

alter_tag
    : ALTER TAG if_exists? object_name alter_tag_opts
    ;

alter_task
    : ALTER TASK if_exists? object_name resume_suspend
    | ALTER TASK if_exists? object_name REMOVE AFTER string_list | ADD AFTER string_list
    | ALTER TASK if_exists? object_name SET
        ( WAREHOUSE EQ string )?
        ( SCHEDULE EQ string )?
        ( ALLOW_OVERLAPPING_EXECUTION EQ true_false )?
        ( USER_TASK_TIMEOUT_MS EQ num )?
        ( SUSPEND_TASK_AFTER_NUM_FAILURES EQ num )?
        comment_clause?
        session_parameter_init_list?
    | ALTER TASK if_exists? object_name UNSET
        WAREHOUSE?
        SCHEDULE?
        ALLOW_OVERLAPPING_EXECUTION?
        USER_TASK_TIMEOUT_MS?
        SUSPEND_TASK_AFTER_NUM_FAILURES?
        COMMENT?
        session_parameter_list?
            //[ , ... ]
    | ALTER TASK if_exists? object_name set_tags
    | ALTER TASK if_exists? object_name unset_tags
    | ALTER TASK if_exists? object_name MODIFY AS sql
    | ALTER TASK if_exists? object_name MODIFY WHEN expr
    ;

alter_user
    : ALTER USER if_exists? id_ alter_user_opts
    ;

alter_view
    : ALTER VIEW if_exists? id_ RENAME TO id_
    | ALTER VIEW if_exists? id_ SET comment_clause
    | ALTER VIEW if_exists? id_ UNSET COMMENT
    | ALTER VIEW id_ SET SECURE
    | ALTER VIEW id_ UNSET SECURE
    | ALTER VIEW if_exists? id_ set_tags
    | ALTER VIEW if_exists? id_ unset_tags
    | ALTER VIEW if_exists? id_ ADD ROW ACCESS POLICY id_ ON column_list_in_parentheses
    | ALTER VIEW if_exists? id_ DROP ROW ACCESS POLICY id_
    | ALTER VIEW if_exists? id_ ADD ROW ACCESS POLICY id_ ON column_list_in_parentheses COMMA DROP ROW ACCESS POLICY id_
    | ALTER VIEW if_exists? id_ DROP ALL ROW ACCESS POLICIES
    | ALTER VIEW id_ alter_modify COLUMN? id_ SET MASKING POLICY id_ ( USING '(' column_name COMMA column_list ')' )?
                                                                                                  FORCE?
    | ALTER VIEW id_ alter_modify COLUMN? id_ UNSET MASKING POLICY
    | ALTER VIEW id_ alter_modify COLUMN? id_ set_tags
    | ALTER VIEW id_ alter_modify COLUMN id_ unset_tags
    ;

alter_modify
    : ALTER | MODIFY
    ;

alter_warehouse
    : ALTER WAREHOUSE if_exists? alter_warehouse_opts
    ;

alter_connection_opts
    : id_ ENABLE FAILOVER TO ACCOUNTS id_ DOT id_  ( COMMA id_ DOT id_ )* ignore_edition_check?
    | id_ DISABLE FAILOVER ( TO ACCOUNTS  id_ DOT id_  (COMMA id_ DOT id_) )?
    | id_ PRIMARY
    | if_exists? id_ SET comment_clause
    | if_exists? id_ UNSET COMMENT
    ;

alter_user_opts
    : RENAME TO id_
    | RESET PASSWORD
    | ABORT ALL QUERIES
    | ADD DELEGATED AUTHORIZATION OF ROLE id_ TO SECURITY INTEGRATION id_
    | REMOVE DELEGATED ( AUTHORIZATION OF ROLE id_ | AUTHORIZATIONS ) FROM SECURITY INTEGRATION id_
    | set_tags
    | unset_tags
//    | SET object_properties? object_params? session_params?
//    | UNSET (object_property_name | object_param_name | session_param_name) //[ , ... ]
    ;

alter_tag_opts
    : RENAME TO object_name
    | ( ADD | DROP ) tag_allowed_values
    | UNSET ALLOWED_VALUES
    | SET MASKING POLICY id_ (COMMA MASKING POLICY id_)*
    | UNSET MASKING POLICY id_ (COMMA MASKING POLICY id_)*
    | SET comment_clause
    | UNSET COMMENT
    ;

alter_network_policy_opts
    : if_exists? id_ SET
            (ALLOWED_IP_LIST EQ '(' string_list ')' )?
            (BLOCKED_IP_LIST EQ '(' string_list ')' )?
            comment_clause?
    | if_exists? id_ UNSET COMMENT
    | id_ RENAME TO id_
    ;

alter_warehouse_opts
    : id_fn? ( SUSPEND | RESUME if_suspended? )
    | id_fn? ABORT ALL QUERIES
    | id_fn RENAME TO id_
//    | id_ SET [ objectProperties ]
    | id_fn set_tags
    | id_fn unset_tags
    | id_fn UNSET id_ (COMMA id_)*
    ;

alter_account_opts
    : SET account_params? object_params? session_params?
    | UNSET param_name (COMMA param_name)?
    | SET RESOURCE_MONITOR EQ id_
    | set_tags
    | unset_tags
    | id_ RENAME TO id_ ( SAVE_OLD_URL EQ true_false )?
    | id_ DROP OLD URL
    ;

set_tags
    : SET tag_decl_list
    ;

tag_decl_list
    : TAG object_name EQ tag_value (COMMA object_name EQ tag_value )*
    ;

unset_tags
    : UNSET TAG object_name (COMMA object_name)*
    ;

// create commands
create_command
    : create_account
    | create_alert
    | create_api_integration
    | create_object_clone
    | create_connection
    | create_database
    | create_dynamic_table
    | create_event_table
    | create_external_function
    | create_external_table
    | create_failover_group
    | create_file_format
    | create_function
    //| create_integration
    | create_managed_account
    | create_masking_policy
    | create_materialized_view
    | create_network_policy
    | create_notification_integration
    | create_pipe
    | create_procedure
    | create_replication_group
    | create_resource_monitor
    | create_role
    | create_row_access_policy
    | create_schema
    | create_security_integration_external_oauth
    | create_security_integration_snowflake_oauth
    | create_security_integration_saml2
    | create_security_integration_scim
    | create_sequence
    | create_session_policy
    | create_share
    | create_stage
    | create_storage_integration
    | create_stream
    | create_table
    | create_table_as_select
    | create_table_like
    //    | create_|_alter_table_…_constraint
    | create_tag
    | create_task
    | create_user
    | create_view
    | create_warehouse
    ;

create_account
    : CREATE ACCOUNT id_
            ADMIN_NAME EQ id_
            ADMIN_PASSWORD EQ string
          ( FIRST_NAME EQ id_ )?
          ( LAST_NAME EQ id_ )?
            EMAIL EQ string
          ( MUST_CHANGE_PASSWORD EQ true_false )?
            EDITION EQ ( STANDARD | ENTERPRISE | BUSINESS_CRITICAL )
          ( REGION_GROUP EQ region_group_id )?
          ( REGION EQ snowflake_region_id )?
          comment_clause?
    ;

create_alert
    : CREATE or_replace? ALERT if_not_exists? id_
        WAREHOUSE EQ id_
        SCHEDULE EQ string
        IF '(' EXISTS '(' alert_condition ')' ')'
        THEN alert_action
    ;

alert_condition
    : select_statement
    | show_command
    | call
    ;

alert_action
    : sql_command
    ;

create_api_integration
    : CREATE or_replace? API INTEGRATION if_not_exists? id_
          API_PROVIDER EQ ( id_ )
          API_AWS_ROLE_ARN EQ string
          ( API_KEY EQ string )?
          API_ALLOWED_PREFIXES EQ LR_BRACKET string RR_BRACKET
          ( API_BLOCKED_PREFIXES EQ LR_BRACKET string RR_BRACKET )?
          ENABLED EQ true_false
          comment_clause?
    | CREATE or_replace? API INTEGRATION if_not_exists? id_
          API_PROVIDER EQ id_
          AZURE_TENANT_ID EQ string
          AZURE_AD_APPLICATION_ID EQ string
          ( API_KEY EQ string )?
          API_ALLOWED_PREFIXES EQ '(' string ')'
          ( API_BLOCKED_PREFIXES EQ '(' string ')' )?
          ENABLED EQ true_false
          comment_clause?
    | CREATE or_replace API INTEGRATION if_not_exists id_
        API_PROVIDER EQ id_
        GOOGLE_AUDIENCE EQ string
        API_ALLOWED_PREFIXES EQ '(' string ')'
        ( API_BLOCKED_PREFIXES EQ '(' string ')' )?
        ENABLED EQ true_false
        comment_clause?
    ;

create_object_clone
    : CREATE or_replace? ( DATABASE | SCHEMA | TABLE ) if_not_exists? id_
        CLONE object_name
              ( at_before1 LR_BRACKET ( TIMESTAMP ASSOC string | OFFSET ASSOC string | STATEMENT ASSOC id_ ) RR_BRACKET )?
    | CREATE or_replace? ( STAGE | FILE FORMAT | SEQUENCE | STREAM | TASK ) if_not_exists? object_name
        CLONE object_name
    ;

create_connection
    : CREATE CONNECTION if_not_exists? id_ ( comment_clause? | (AS REPLICA OF id_ DOT id_ DOT id_ comment_clause?) )
    ;

create_database
    : CREATE or_replace? TRANSIENT? DATABASE if_not_exists? id_
            clone_at_before?
            ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
            ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
            default_ddl_collation?
            with_tags?
            comment_clause?
    ;

clone_at_before
    : CLONE id_ ( at_before1 LR_BRACKET ( TIMESTAMP ASSOC string | OFFSET ASSOC string | STATEMENT ASSOC id_ ) RR_BRACKET )?
    ;

at_before1
    : AT_KEYWORD | BEFORE
    ;

header_decl
    : string EQ string
    ;

compression_type
    : NONE
    | GZIP
    | DEFLATE
    | AUTO
    ;

compression
    : COMPRESSION EQ compression_type
    ;

create_dynamic_table
    : CREATE or_replace? DYNAMIC TABLE id_
        TARGET_LAG EQ (string | DOWNSTREAM)
        WAREHOUSE EQ wh=id_
        AS query_statement
    ;

create_event_table
    : CREATE or_replace? EVENT TABLE if_not_exists? id_
        cluster_by?
        (DATA_RETENTION_TIME_IN_DAYS EQ num)?
        (MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num)?
        change_tracking?
        (DEFAULT_DDL_COLLATION_ EQ string)?
        copy_grants?
        with_row_access_policy?
        with_tags?
        (WITH? comment_clause)?
    ;

create_external_function
    : CREATE or_replace? SECURE? EXTERNAL FUNCTION object_name LR_BRACKET ( arg_name arg_data_type (COMMA arg_name arg_data_type)* )? RR_BRACKET
        RETURNS data_type
        null_not_null?
        ( ( CALLED ON NULL_ INPUT) | ((RETURNS NULL_ ON NULL_ INPUT) | STRICT) )?
        ( VOLATILE | IMMUTABLE )?
        comment_clause?
        API_INTEGRATION EQ id_
        ( HEADERS EQ LR_BRACKET header_decl (COMMA header_decl)* RR_BRACKET )?
        ( CONTEXT_HEADERS EQ LR_BRACKET id_ (COMMA id_)* RR_BRACKET )?
        ( MAX_BATCH_ROWS EQ num )?
        compression?
        ( REQUEST_TRANSLATOR EQ id_ )?
        ( RESPONSE_TRANSLATOR EQ id_ )?
        AS string
    ;

create_external_table
    // Partitions computed from expressions
    : CREATE or_replace? EXTERNAL TABLE if_not_exists?
        object_name '(' external_table_column_decl_list ')'
        cloud_provider_params3?
        partition_by?
        WITH? LOCATION EQ external_stage
        ( REFRESH_ON_CREATE EQ true_false )?
        ( AUTO_REFRESH EQ true_false )?
        pattern?
        file_format
        ( AWS_SNS_TOPIC EQ string )?
        copy_grants?
        with_row_access_policy?
        with_tags?
        comment_clause?
    // Partitions added and removed manually
    | CREATE or_replace? EXTERNAL TABLE if_not_exists?
        object_name '(' external_table_column_decl_list ')'
        cloud_provider_params3?
        partition_by?
        WITH? LOCATION EQ external_stage
        PARTITION_TYPE EQ USER_SPECIFIED
        file_format
        copy_grants?
        with_row_access_policy?
        with_tags?
        comment_clause?
    // Delta Lake
    | CREATE or_replace? EXTERNAL TABLE if_not_exists?
        object_name '(' external_table_column_decl_list ')'
        cloud_provider_params3?
        partition_by?
        WITH? LOCATION EQ external_stage
        PARTITION_TYPE EQ USER_SPECIFIED
        file_format
        ( TABLE_FORMAT EQ DELTA )?
        copy_grants?
        with_row_access_policy?
        with_tags?
        comment_clause?
    ;

external_table_column_decl
    : column_name data_type AS (expr | id_) inline_constraint?
    ;

external_table_column_decl_list
    : external_table_column_decl (COMMA external_table_column_decl)*
    ;

full_acct
    : id_ DOT id_
    ;

integration_type_name
    : SECURITY INTEGRATIONS | API INTEGRATIONS
    ;

create_failover_group
    : CREATE FAILOVER GROUP if_not_exists? id_
          OBJECT_TYPES EQ object_type (COMMA object_type )*
          ( ALLOWED_DATABASES EQ id_ (COMMA id_ )* )?
          ( ALLOWED_SHARES EQ id_ (COMMA id_)* )?
          ( ALLOWED_INTEGRATION_TYPES EQ integration_type_name (COMMA integration_type_name)* )?
          ALLOWED_ACCOUNTS EQ full_acct (COMMA full_acct)*
          ( IGNORE EDITION CHECK )?
          ( REPLICATION_SCHEDULE EQ string )?
//      Secondary Replication Group
    | CREATE FAILOVER GROUP if_not_exists? id_
          AS REPLICA OF id_ DOT id_ DOT id_
    ;

type_fileformat
    : CSV
    | JSON
    | AVRO
    | ORC
    | PARQUET
    | XML
    | CSV_Q
    | JSON_Q
    | AVRO_Q
    | ORC_Q
    | PARQUET_Q
    | XML_Q
    ;

create_file_format
    : CREATE or_replace? FILE FORMAT if_not_exists? object_name
            (TYPE EQ type_fileformat)? format_type_options*
            comment_clause?
    ;

arg_decl
    : arg_name arg_data_type
    ;

col_decl
    : column_name data_type
    ;

function_definition
    : string
    | DBL_DOLLAR
    ;

create_function
    : CREATE or_replace? SECURE? FUNCTION object_name LR_BRACKET ( arg_decl (COMMA  arg_decl)* )? RR_BRACKET
        RETURNS ( data_type | TABLE LR_BRACKET (col_decl (COMMA col_decl)* )? RR_BRACKET )
        null_not_null?
        LANGUAGE JAVASCRIPT
        ( CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT )?
        ( VOLATILE | IMMUTABLE )?
        comment_clause?
        AS function_definition
    | CREATE or_replace? SECURE? FUNCTION object_name LR_BRACKET ( arg_decl (COMMA  arg_decl)* )? RR_BRACKET
        RETURNS ( data_type | TABLE LR_BRACKET (col_decl (COMMA col_decl)* )? RR_BRACKET )
        null_not_null?
        ( CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT )?
        ( VOLATILE | IMMUTABLE )?
        MEMOIZABLE?
        comment_clause?
        AS function_definition
    ;

create_managed_account
    : CREATE MANAGED ACCOUNT id_ ADMIN_NAME EQ id_ COMMA ADMIN_PASSWORD EQ string COMMA TYPE EQ READER (COMMA comment_clause)?
    ;

create_masking_policy
    : CREATE or_replace? MASKING POLICY if_not_exists? object_name AS
      '(' arg_name arg_data_type (COMMA arg_name arg_data_type)? ')'
      RETURNS arg_data_type ARROW expr
      comment_clause?
    ;

tag_decl
    : object_name EQ string
    ;

column_list_in_parentheses
    : LR_BRACKET column_list RR_BRACKET
    ;

create_materialized_view
    : CREATE or_replace? SECURE? MATERIALIZED VIEW if_not_exists? object_name
        ( LR_BRACKET column_list_with_comment RR_BRACKET )?
        view_col*
        with_row_access_policy?
        with_tags?
        copy_grants?
        comment_clause?
        cluster_by?
        AS select_statement //NOTA MATERIALIZED VIEW accept only simple select statement at this time
    ;

create_network_policy
    : CREATE or_replace? NETWORK POLICY id_
         ALLOWED_IP_LIST EQ '(' string_list? ')'
         ( BLOCKED_IP_LIST EQ '(' string_list? ')' )?
         comment_clause?
    ;

cloud_provider_params_auto
    //(for Google Cloud Storage)
    : NOTIFICATION_PROVIDER EQ GCP_PUBSUB GCP_PUBSUB_SUBSCRIPTION_NAME EQ string
    //(for Microsoft Azure Storage)
    | NOTIFICATION_PROVIDER EQ AZURE_EVENT_GRID AZURE_STORAGE_QUEUE_PRIMARY_URI EQ string AZURE_TENANT_ID EQ string
    ;

cloud_provider_params_push
    //(for Amazon SNS)
    : NOTIFICATION_PROVIDER EQ AWS_SNS
        AWS_SNS_TOPIC_ARN EQ string
        AWS_SNS_ROLE_ARN EQ string
    //(for Google Pub/Sub)
    | NOTIFICATION_PROVIDER EQ GCP_PUBSUB
        GCP_PUBSUB_TOPIC_NAME EQ string
    //(for Microsoft Azure Event Grid)
    | NOTIFICATION_PROVIDER EQ AZURE_EVENT_GRID
        AZURE_EVENT_GRID_TOPIC_ENDPOINT EQ string
        AZURE_TENANT_ID EQ string
    ;

create_notification_integration
    : CREATE or_replace? NOTIFICATION INTEGRATION if_not_exists? id_
        ENABLED EQ true_false
        TYPE EQ QUEUE
        cloud_provider_params_auto
        comment_clause?
    | CREATE or_replace? NOTIFICATION INTEGRATION if_not_exists? id_
        ENABLED EQ true_false
        DIRECTION EQ OUTBOUND
        TYPE EQ QUEUE
        cloud_provider_params_push
        comment_clause?
    ;

create_pipe
    : CREATE or_replace? PIPE if_not_exists? object_name
        ( AUTO_INGEST EQ true_false )?
        ( ERROR_INTEGRATION EQ id_ )?
        ( AWS_SNS_TOPIC EQ string )?
        ( INTEGRATION EQ string )?
        comment_clause?
        AS copy_into_table
    ;

caller_owner
    : CALLER | OWNER
    ;

executa_as
    : EXECUTE AS caller_owner
    ;

procedure_definition
    : string
    | DBL_DOLLAR
    ;

create_procedure
    : CREATE or_replace? PROCEDURE object_name LR_BRACKET ( arg_decl (COMMA arg_decl)* )? RR_BRACKET
        RETURNS ( data_type | TABLE LR_BRACKET ( col_decl (COMMA col_decl)* )? RR_BRACKET )
        ( NOT NULL_ )?
        LANGUAGE SQL
        ( CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT )?
        ( VOLATILE | IMMUTABLE )? // Note: VOLATILE and IMMUTABLE are deprecated.
        comment_clause?
        executa_as?
        AS procedure_definition
    | CREATE or_replace? SECURE? PROCEDURE object_name LR_BRACKET ( arg_decl (COMMA arg_decl)* )? RR_BRACKET
        RETURNS data_type ( NOT NULL_ )?
        LANGUAGE JAVASCRIPT
        ( CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT )?
        ( VOLATILE | IMMUTABLE )? // Note: VOLATILE and IMMUTABLE are deprecated.
        comment_clause?
        executa_as?
        AS procedure_definition
    ;

create_replication_group
    : CREATE REPLICATION GROUP if_not_exists? id_
          OBJECT_TYPES EQ object_type ( COMMA object_type )*
          ( ALLOWED_DATABASES EQ id_ (COMMA id_)* )?
          ( ALLOWED_SHARES EQ id_ (COMMA id_ )* )?
          ( ALLOWED_INTEGRATION_TYPES EQ integration_type_name (COMMA integration_type_name )* )?
          ALLOWED_ACCOUNTS EQ full_acct (COMMA full_acct)*
          ( IGNORE EDITION CHECK )?
          ( REPLICATION_SCHEDULE EQ string )?
    //Secondary Replication Group
    | CREATE REPLICATION GROUP if_not_exists? id_ AS REPLICA OF id_ DOT id_ DOT id_
    ;

create_resource_monitor
    : CREATE or_replace? RESOURCE MONITOR id_ WITH
        credit_quota?
        frequency?
        ( START_TIMESTAMP EQ ( string | IMMEDIATELY ) )?
        ( END_TIMESTAMP EQ string )?
        notify_users?
        ( TRIGGERS trigger_definition+ )?
    ;

create_role
    : CREATE or_replace? ROLE if_not_exists? id_ with_tags? comment_clause?
    ;

create_row_access_policy
    : CREATE or_replace? ROW ACCESS POLICY if_not_exists? id_ AS
        LR_BRACKET arg_decl (COMMA arg_decl)* RR_BRACKET
        RETURNS BOOLEAN ARROW expr
        comment_clause?
    ;

create_schema
    : CREATE or_replace? TRANSIENT? SCHEMA if_not_exists? schema_name
        clone_at_before?
        ( WITH MANAGED ACCESS )?
        ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
        ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
        default_ddl_collation?
        with_tags?
        comment_clause?
    ;

create_security_integration_external_oauth
    : CREATE or_replace? SECURITY INTEGRATION if_not_exists? id_
      TYPE EQ EXTERNAL_OAUTH
      ENABLED EQ true_false
      EXTERNAL_OAUTH_TYPE EQ ( OKTA | AZURE | PING_FEDERATE | CUSTOM )
      EXTERNAL_OAUTH_ISSUER EQ string
      EXTERNAL_OAUTH_TOKEN_USER_MAPPING_CLAIM EQ (string | '(' string_list ')' )
      EXTERNAL_OAUTH_SNOWFLAKE_USER_MAPPING_ATTRIBUTE EQ string
      ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ string )? // For OKTA | PING_FEDERATE | CUSTOM
      ( EXTERNAL_OAUTH_JWS_KEYS_URL EQ (string | '(' string_list ')') )? // For Azure
      ( EXTERNAL_OAUTH_BLOCKED_ROLES_LIST EQ '(' string_list ')' )?
      ( EXTERNAL_OAUTH_ALLOWED_ROLES_LIST EQ '(' string_list ')' )?
      ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY EQ string )?
      ( EXTERNAL_OAUTH_RSA_PUBLIC_KEY_2 EQ string )?
      ( EXTERNAL_OAUTH_AUDIENCE_LIST EQ '(' string ')' )?
      ( EXTERNAL_OAUTH_ANY_ROLE_MODE EQ (DISABLE | ENABLE | ENABLE_FOR_PRIVILEGE) )?
      ( EXTERNAL_OAUTH_SCOPE_DELIMITER EQ string )? // Only for EXTERNAL_OAUTH_TYPE EQ CUSTOM
    ;

implicit_none
    : IMPLICIT
    | NONE
    ;

create_security_integration_snowflake_oauth
    : CREATE or_replace? SECURITY INTEGRATION if_not_exists? id_
        TYPE EQ OAUTH
        OAUTH_CLIENT EQ partner_application
        OAUTH_REDIRECT_URI EQ string  //Required when OAUTH_CLIENTEQLOOKER
        enabled_true_false?
        ( OAUTH_ISSUE_REFRESH_TOKENS EQ true_false )?
        ( OAUTH_REFRESH_TOKEN_VALIDITY EQ num )?
        ( OAUTH_USE_SECONDARY_ROLES EQ implicit_none )?
        ( BLOCKED_ROLES_LIST EQ '(' string_list ')' )?
        comment_clause?
    // Snowflake OAuth for custom clients
    | CREATE or_replace? SECURITY INTEGRATION if_not_exists? id_
        TYPE EQ OAUTH
        OAUTH_CLIENT EQ CUSTOM
        //OAUTH_CLIENT_TYPE EQ 'CONFIDENTIAL' | 'PUBLIC'
        OAUTH_REDIRECT_URI EQ string
        enabled_true_false?
        ( OAUTH_ALLOW_NON_TLS_REDIRECT_URI EQ true_false )?
        ( OAUTH_ENFORCE_PKCE EQ true_false )?
        ( OAUTH_USE_SECONDARY_ROLES EQ implicit_none )?
        ( PRE_AUTHORIZED_ROLES_LIST EQ '(' string_list ')' )?
        ( BLOCKED_ROLES_LIST EQ '(' string_list ')' )?
        ( OAUTH_ISSUE_REFRESH_TOKENS EQ true_false )?
        ( OAUTH_REFRESH_TOKEN_VALIDITY EQ num )?
        network_policy?
        ( OAUTH_CLIENT_RSA_PUBLIC_KEY EQ string )?
        ( OAUTH_CLIENT_RSA_PUBLIC_KEY_2 EQ string )?
        comment_clause?
    ;

create_security_integration_saml2
    : CREATE or_replace? SECURITY INTEGRATION if_not_exists?
          TYPE EQ SAML2
          enabled_true_false
          SAML2_ISSUER EQ string
          SAML2_SSO_URL EQ string
          SAML2_PROVIDER EQ string
          SAML2_X509_CERT EQ string
          ( SAML2_SP_INITIATED_LOGIN_PAGE_LABEL EQ string )?
          ( SAML2_ENABLE_SP_INITIATED EQ true_false )?
          ( SAML2_SNOWFLAKE_X509_CERT EQ string )?
          ( SAML2_SIGN_REQUEST EQ true_false )?
          ( SAML2_REQUESTED_NAMEID_FORMAT EQ string )?
          ( SAML2_POST_LOGOUT_REDIRECT_URL EQ string )?
          ( SAML2_FORCE_AUTHN EQ true_false )?
          ( SAML2_SNOWFLAKE_ISSUER_URL EQ string )?
          ( SAML2_SNOWFLAKE_ACS_URL EQ string )?
    ;

create_security_integration_scim
    : CREATE or_replace? SECURITY INTEGRATION if_not_exists?
          id_
          TYPE EQ SCIM
          SCIM_CLIENT EQ (OKTA_Q | AZURE_Q | GENERIC_Q)
          RUN_AS_ROLE EQ (OKTA_PROVISIONER_Q | AAD_PROVISIONER_Q | GENERIC_SCIM_PROVISIONER_Q)
          network_policy?
          ( SYNC_PASSWORD EQ true_false )?
          comment_clause?
    ;

network_policy
    : NETWORK_POLICY EQ string
    ;

partner_application
    : TABLEAU_DESKTOP
    | TABLEAU_SERVER
    | LOOKER
    ;

start_with
    : START WITH? EQ? num
    ;

increment_by
    : INCREMENT BY? EQ? num
    ;

create_sequence
    : CREATE or_replace? SEQUENCE if_not_exists? object_name
        WITH?
        start_with?
        increment_by?
        comment_clause?
    ;

create_session_policy
    : CREATE or_replace? SESSION POLICY if_exists? id_
        (SESSION_IDLE_TIMEOUT_MINS EQ num)?
        (SESSION_UI_IDLE_TIMEOUT_MINS EQ num)?
        comment_clause?
    ;

create_share
    : CREATE or_replace? SHARE id_
        comment_clause?
    ;

character
    : CHAR_LITERAL
    ;

format_type_options
    //-- If TYPE EQ CSV
    : COMPRESSION EQ (AUTO | GZIP | BZ2 | BROTLI | ZSTD | DEFLATE | RAW_DEFLATE | NONE | AUTO_Q )
    | RECORD_DELIMITER EQ ( string | NONE)
    | FIELD_DELIMITER EQ ( string | NONE)
    | FILE_EXTENSION EQ string
    | SKIP_HEADER EQ num
    | SKIP_BLANK_LINES EQ true_false
    | DATE_FORMAT EQ (string | AUTO)
    | TIME_FORMAT EQ (string | AUTO)
    | TIMESTAMP_FORMAT EQ (string | AUTO)
    | BINARY_FORMAT EQ (HEX | BASE64 | UTF8)
    | ESCAPE EQ (character | NONE | NONE_Q )
    | ESCAPE_UNENCLOSED_FIELD EQ (string | NONE | NONE_Q )
    | TRIM_SPACE EQ true_false
    | FIELD_OPTIONALLY_ENCLOSED_BY EQ (string | NONE | NONE_Q | SINGLE_QUOTE )
    | NULL_IF EQ LR_BRACKET string_list RR_BRACKET
    | ERROR_ON_COLUMN_COUNT_MISMATCH EQ true_false
    | REPLACE_INVALID_CHARACTERS EQ true_false
    | EMPTY_FIELD_AS_NULL EQ true_false
    | SKIP_BYTE_ORDER_MARK EQ true_false
    | ENCODING EQ (string | UTF8) //by the way other encoding keyword are valid ie WINDOWS1252
    //-- If TYPE EQ JSON
    //| COMPRESSION EQ (AUTO | GZIP | BZ2 | BROTLI | ZSTD | DEFLATE | RAW_DEFLATE | NONE)
//    | DATE_FORMAT EQ string | AUTO
//    | TIME_FORMAT EQ string | AUTO
//    | TIMESTAMP_FORMAT EQ string | AUTO
//    | BINARY_FORMAT EQ HEX | BASE64 | UTF8
//    | TRIM_SPACE EQ true_false
//    | NULL_IF EQ LR_BRACKET string_list RR_BRACKET
//    | FILE_EXTENSION EQ string
    | ENABLE_OCTAL EQ true_false
    | ALLOW_DUPLICATE EQ true_false
    | STRIP_OUTER_ARRAY EQ true_false
    | STRIP_NULL_VALUES EQ true_false
//    | REPLACE_INVALID_CHARACTERS EQ true_false
    | IGNORE_UTF8_ERRORS EQ true_false
//    | SKIP_BYTE_ORDER_MARK EQ true_false
    //-- If TYPE EQ AVRO
//    | COMPRESSION EQ AUTO | GZIP | BROTLI | ZSTD | DEFLATE | RAW_DEFLATE | NONE
//    | TRIM_SPACE EQ true_false
//    | NULL_IF EQ LR_BRACKET string_list RR_BRACKET
    //-- If TYPE EQ ORC
//    | TRIM_SPACE EQ true_false
//    | NULL_IF EQ LR_BRACKET string_list RR_BRACKET
    //-- If TYPE EQ PARQUET
    | COMPRESSION EQ AUTO | LZO | SNAPPY | NONE
    | SNAPPY_COMPRESSION EQ true_false
    | BINARY_AS_TEXT EQ true_false
//    | TRIM_SPACE EQ true_false
//    | NULL_IF EQ LR_BRACKET string_list RR_BRACKET
    //-- If TYPE EQ XML
    | COMPRESSION EQ AUTO | GZIP | BZ2 | BROTLI | ZSTD | DEFLATE | RAW_DEFLATE | NONE
//    | IGNORE_UTF8_ERRORS EQ true_false
    | PRESERVE_SPACE EQ true_false
    | STRIP_OUTER_ELEMENT EQ true_false
    | DISABLE_SNOWFLAKE_DATA EQ true_false
    | DISABLE_AUTO_CONVERT EQ true_false
//    | SKIP_BYTE_ORDER_MARK EQ true_false
    ;

copy_options
    : ON_ERROR EQ ( CONTINUE | SKIP_FILE | SKIP_FILE_N | SKIP_FILE_N ABORT_STATEMENT )
    | SIZE_LIMIT EQ num
    | PURGE EQ true_false
    | RETURN_FAILED_ONLY EQ true_false
    | MATCH_BY_COLUMN_NAME EQ CASE_SENSITIVE | CASE_INSENSITIVE | NONE
    | ENFORCE_LENGTH EQ true_false
    | TRUNCATECOLUMNS EQ true_false
    | FORCE EQ true_false
    ;

stage_encryption_opts_internal
    : ENCRYPTION EQ LR_BRACKET TYPE EQ ( SNOWFLAKE_FULL | SNOWFLAKE_SSE ) RR_BRACKET
    ;

stage_type
    : TYPE EQ string
    ;

stage_master_key
    : MASTER_KEY EQ string
    ;

stage_kms_key
    : KMS_KEY_ID EQ string
    ;

stage_encryption_opts_aws
    : ENCRYPTION EQ LR_BRACKET
        ( stage_type? stage_master_key
        | stage_type stage_kms_key?
        )
        RR_BRACKET
    ;

aws_token
    : AWS_TOKEN EQ string
    ;

aws_key_id
    : AWS_KEY_ID EQ string
    ;

aws_secret_key
    : AWS_SECRET_KEY EQ string
    ;

aws_role
    : AWS_ROLE EQ string
    ;

azure_encryption_value
    : ( TYPE EQ AZURE_CSE_Q )? MASTER_KEY EQ string
    | MASTER_KEY EQ string TYPE EQ AZURE_CSE_Q
    | TYPE EQ NONE_Q
    ;
stage_encryption_opts_az
    : ENCRYPTION EQ LR_BRACKET azure_encryption_value RR_BRACKET
    ;
storage_integration_eq_id
    : STORAGE_INTEGRATION EQ id_
    ;

az_credential_or_storage_integration:
    storage_integration_eq_id
    |  CREDENTIALS EQ LR_BRACKET AZURE_SAS_TOKEN EQ string RR_BRACKET
    ;

gcp_encryption_value
    : ( TYPE EQ GCS_SSE_KMS_Q )? KMS_KEY_ID EQ string
    | KMS_KEY_ID EQ string TYPE EQ GCS_SSE_KMS_Q
    | TYPE EQ NONE_Q
    ;

stage_encryption_opts_gcp
    : ENCRYPTION EQ LR_BRACKET gcp_encryption_value RR_BRACKET
    ;

aws_credential_or_storage_integration:
    storage_integration_eq_id
    |  CREDENTIALS EQ LR_BRACKET ( aws_key_id aws_secret_key aws_token? | aws_role ) RR_BRACKET
    ;

external_stage_params
    //(for Amazon S3)
    : URL EQ s3_url=( S3_PATH | S3GOV_PATH )
      ( aws_credential_or_storage_integration? stage_encryption_opts_aws | stage_encryption_opts_aws? aws_credential_or_storage_integration )?
    //(for Google Cloud Storage)
    | URL EQ gc_url=GCS_PATH
      ( storage_integration_eq_id? stage_encryption_opts_gcp | stage_encryption_opts_gcp? storage_integration_eq_id )?
    //(for Microsoft Azure)
    | URL EQ azure_url=AZURE_PATH
      ( az_credential_or_storage_integration? stage_encryption_opts_az  | stage_encryption_opts_az? az_credential_or_storage_integration )?
    ;

true_false
    : TRUE
    | FALSE
    ;

enable
    : ENABLE EQ true_false
    ;

refresh_on_create
    : REFRESH_ON_CREATE EQ true_false
    ;

auto_refresh
    : AUTO_REFRESH EQ true_false
    ;

notification_integration
    : NOTIFICATION_INTEGRATION EQ string
    ;

directory_table_internal_params
    : DIRECTORY EQ LR_BRACKET
        (
            enable refresh_on_create?
            | REFRESH_ON_CREATE EQ FALSE
            | refresh_on_create enable
        )
        RR_BRACKET
    ;

directory_table_external_params
// (for Amazon S3)
    :  DIRECTORY EQ LR_BRACKET enable
          refresh_on_create?
          auto_refresh? RR_BRACKET
// (for Google Cloud Storage)
    |  DIRECTORY EQ LR_BRACKET enable
          auto_refresh?
          refresh_on_create?
          notification_integration? RR_BRACKET
// (for Microsoft Azure)
    |  DIRECTORY EQ LR_BRACKET enable
          refresh_on_create?
          auto_refresh?
          notification_integration? RR_BRACKET
    ;

/* ===========  Stage DDL section =========== */
create_stage
    : CREATE or_replace? temporary? STAGE if_not_exists? object_name_or_identifier
        stage_encryption_opts_internal?
        directory_table_internal_params?
        ( FILE_FORMAT EQ LR_BRACKET ( FORMAT_NAME EQ string | TYPE EQ ( CSV | JSON | AVRO | ORC | PARQUET | XML ) format_type_options* ) RR_BRACKET )?
        ( COPY_OPTIONS_ EQ LR_BRACKET copy_options RR_BRACKET )?
        with_tags?
        comment_clause?
    | CREATE or_replace? temporary? STAGE if_not_exists? object_name_or_identifier
        external_stage_params
        directory_table_external_params?
        ( FILE_FORMAT EQ LR_BRACKET ( FORMAT_NAME EQ string | TYPE EQ ( CSV | JSON | AVRO | ORC | PARQUET | XML ) format_type_options* ) RR_BRACKET )?
        ( COPY_OPTIONS_ EQ LR_BRACKET copy_options RR_BRACKET )?
        with_tags?
        comment_clause?
    ;

alter_stage
    : ALTER STAGE if_exists? object_name_or_identifier RENAME TO object_name_or_identifier
    | ALTER STAGE if_exists? object_name_or_identifier set_tags
    | ALTER STAGE if_exists? object_name_or_identifier unset_tags
    | ALTER STAGE if_exists? object_name_or_identifier SET
          external_stage_params?
          file_format?
          ( COPY_OPTIONS_ EQ LR_BRACKET copy_options RR_BRACKET )?
          comment_clause?
    ;

drop_stage
    : DROP STAGE if_exists? object_name_or_identifier
    ;

describe_stage
    : describe STAGE object_name_or_identifier
    ;

show_stages
    : SHOW STAGES like_pattern? in_obj?
    ;

/* ===========  End of stage DDL section =========== */

cloud_provider_params
    //(for Amazon S3)
    : STORAGE_PROVIDER EQ S3 STORAGE_AWS_ROLE_ARN EQ string ( STORAGE_AWS_OBJECT_ACL EQ string )?
    //(for Google Cloud Storage)
    | STORAGE_PROVIDER EQ GCS
    //(for Microsoft Azure)
    | STORAGE_PROVIDER EQ AZURE AZURE_TENANT_ID EQ string
    ;

cloud_provider_params2
    //(for Amazon S3)
    : STORAGE_AWS_ROLE_ARN EQ string ( STORAGE_AWS_OBJECT_ACL EQ string )?
    //(for Microsoft Azure)
    | AZURE_TENANT_ID EQ string
    ;

cloud_provider_params3
    : INTEGRATION EQ string
    ;

create_storage_integration
    : CREATE or_replace? STORAGE INTEGRATION if_not_exists? id_
        TYPE EQ EXTERNAL_STAGE
        cloud_provider_params
        ENABLED EQ true_false
        STORAGE_ALLOWED_LOCATIONS EQ LR_BRACKET string_list RR_BRACKET
        ( STORAGE_BLOCKED_LOCATIONS EQ LR_BRACKET string_list RR_BRACKET )?
        comment_clause?
    ;

copy_grants
    : COPY GRANTS
    ;

append_only
    : APPEND_ONLY EQ true_false
    ;

insert_only
    : INSERT_ONLY EQ TRUE
    ;

show_initial_rows
    : SHOW_INITIAL_ROWS EQ true_false
    ;

stream_time
    : at_before1 LR_BRACKET ( TIMESTAMP ASSOC string | OFFSET ASSOC string | STATEMENT ASSOC id_ | STREAM ASSOC string ) RR_BRACKET
    ;

create_stream
    //-- table
    : CREATE or_replace? STREAM if_not_exists?
        object_name
        copy_grants?
        ON TABLE object_name
        stream_time?
        append_only?
        show_initial_rows?
        comment_clause?
    //-- External table
    | CREATE or_replace? STREAM if_not_exists?
        object_name
        copy_grants?
        ON EXTERNAL TABLE object_name
        stream_time?
        insert_only?
        comment_clause?
    //-- Directory table
    |  CREATE or_replace? STREAM if_not_exists?
        object_name
        copy_grants?
        ON STAGE object_name
        comment_clause?
    //-- View
    |  CREATE or_replace? STREAM if_not_exists?
        object_name
        copy_grants?
        ON VIEW object_name
        stream_time?
        append_only?
        show_initial_rows?
        comment_clause?
    ;

temporary
    : TEMP | TEMPORARY
    ;

table_type
    : ( ( LOCAL | GLOBAL )? temporary | VOLATILE ) | TRANSIENT
    ;

with_tags
    : WITH? TAG LR_BRACKET tag_decl (COMMA tag_decl)* RR_BRACKET
    ;

with_row_access_policy
    : WITH? ROW ACCESS POLICY id_ ON LR_BRACKET column_name (COMMA column_name)* RR_BRACKET
    ;

cluster_by
    : CLUSTER BY expr_list_in_parentheses
    ;

change_tracking
    : CHANGE_TRACKING EQ true_false
    ;

with_masking_policy
    : WITH? MASKING POLICY id_ ( USING column_list_in_parentheses )?
    ;

collate
    : COLLATE string
    ;

default_value
    : DEFAULT expr | (AUTOINCREMENT | IDENTITY) (  LR_BRACKET num COMMA num RR_BRACKET | start_with | increment_by | start_with increment_by  )?
    ;

foreign_key
    : FOREIGN KEY
    ;

primary_key
    : PRIMARY KEY
    ;

out_of_line_constraint
    : (CONSTRAINT id_ )?
        (
           (UNIQUE | primary_key) column_list_in_parentheses common_constraint_properties*
           | foreign_key column_list_in_parentheses REFERENCES object_name column_list_in_parentheses constraint_properties
        )
    ;


full_col_decl
    : col_decl
        (
            collate
            | inline_constraint
            | default_value
            | null_not_null
        )*
        with_masking_policy?
        with_tags?
        (COMMENT string)?
    ;

column_decl_item
    : full_col_decl
    | out_of_line_constraint
    ;

column_decl_item_list
    : column_decl_item (COMMA column_decl_item)*
    ;

create_table
    : CREATE or_replace? table_type? TABLE (if_not_exists? object_name | object_name if_not_exists? )
        ((comment_clause? create_table_clause) | (create_table_clause comment_clause?))
    ;

create_table_clause
    : '(' column_decl_item_list ')'
        cluster_by?
        stage_file_format?
        ( STAGE_COPY_OPTIONS EQ LR_BRACKET copy_options RR_BRACKET )?
        ( DATA_RETENTION_TIME_IN_DAYS EQ num )?
        ( MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num )?
        change_tracking?
        default_ddl_collation?
        copy_grants?
        with_row_access_policy?
        with_tags?
    ;

create_table_as_select
    : CREATE or_replace? table_type? TABLE (if_not_exists? object_name | object_name if_not_exists? )
        ('(' column_decl_item_list ')')?
        cluster_by?
        copy_grants?
        with_row_access_policy?
        with_tags?
        comment_clause?
        AS query_statement
    ;

create_table_like
    : CREATE or_replace? TRANSIENT? TABLE object_name LIKE object_name
        cluster_by?
        copy_grants?
    ;

create_tag
    : CREATE or_replace? TAG if_not_exists? object_name tag_allowed_values? comment_clause?
    ;

tag_allowed_values
    : ALLOWED_VALUES tag_value (COMMA tag_value)*
    ;

session_parameter
    : ABORT_DETACHED_QUERY
    | ALLOW_CLIENT_MFA_CACHING
    | ALLOW_ID_TOKEN
    | AUTOCOMMIT
    | AUTOCOMMIT_API_SUPPORTED
    | BINARY_INPUT_FORMAT
    | BINARY_OUTPUT_FORMAT
    | CLIENT_ENABLE_LOG_INFO_STATEMENT_PARAMETERS
    | CLIENT_ENCRYPTION_KEY_SIZE
    | CLIENT_MEMORY_LIMIT
    | CLIENT_METADATA_REQUEST_USE_CONNECTION_CTX
    | CLIENT_METADATA_USE_SESSION_DATABASE
    | CLIENT_PREFETCH_THREADS
    | CLIENT_RESULT_CHUNK_SIZE
    | CLIENT_RESULT_COLUMN_CASE_INSENSITIVE
    | CLIENT_SESSION_KEEP_ALIVE
    | CLIENT_SESSION_KEEP_ALIVE_HEARTBEAT_FREQUENCY
    | CLIENT_TIMESTAMP_TYPE_MAPPING
    | DATA_RETENTION_TIME_IN_DAYS
    | DATE_INPUT_FORMAT
    | DATE_OUTPUT_FORMAT
    | DEFAULT_DDL_COLLATION_
    | ENABLE_INTERNAL_STAGES_PRIVATELINK
    | ENABLE_UNLOAD_PHYSICAL_TYPE_OPTIMIZATION
    | ENFORCE_SESSION_POLICY
    | ERROR_ON_NONDETERMINISTIC_MERGE
    | ERROR_ON_NONDETERMINISTIC_UPDATE
    | EXTERNAL_OAUTH_ADD_PRIVILEGED_ROLES_TO_BLOCKED_LIST
    | GEOGRAPHY_OUTPUT_FORMAT
    | GEOMETRY_OUTPUT_FORMAT
    | INITIAL_REPLICATION_SIZE_LIMIT_IN_TB
    | JDBC_TREAT_DECIMAL_AS_INT
    | JDBC_TREAT_TIMESTAMP_NTZ_AS_UTC
    | JDBC_USE_SESSION_TIMEZONE
    | JSON_INDENT
    | JS_TREAT_INTEGER_AS_BIGINT
    | LOCK_TIMEOUT
    | MAX_CONCURRENCY_LEVEL
    | MAX_DATA_EXTENSION_TIME_IN_DAYS
    | MULTI_STATEMENT_COUNT
    | MIN_DATA_RETENTION_TIME_IN_DAYS
    | NETWORK_POLICY
    | SHARE_RESTRICTIONS
    | PERIODIC_DATA_REKEYING
    | PIPE_EXECUTION_PAUSED
    | PREVENT_UNLOAD_TO_INLINE_URL
    | PREVENT_UNLOAD_TO_INTERNAL_STAGES
    | QUERY_TAG
    | QUOTED_IDENTIFIERS_IGNORE_CASE
    | REQUIRE_STORAGE_INTEGRATION_FOR_STAGE_CREATION
    | REQUIRE_STORAGE_INTEGRATION_FOR_STAGE_OPERATION
    | ROWS_PER_RESULTSET
    | SAML_IDENTITY_PROVIDER
    | SIMULATED_DATA_SHARING_CONSUMER
    | SSO_LOGIN_PAGE
    | STATEMENT_QUEUED_TIMEOUT_IN_SECONDS
    | STATEMENT_TIMEOUT_IN_SECONDS
    | STRICT_JSON_OUTPUT
    | SUSPEND_TASK_AFTER_NUM_FAILURES
    | TIMESTAMP_DAY_IS_ALWAYS_24H
    | TIMESTAMP_INPUT_FORMAT
    | TIMESTAMP_LTZ_OUTPUT_FORMAT
    | TIMESTAMP_NTZ_OUTPUT_FORMAT
    | TIMESTAMP_OUTPUT_FORMAT
    | TIMESTAMP_TYPE_MAPPING
    | TIMESTAMP_TZ_OUTPUT_FORMAT
    | TIMEZONE
    | TIME_INPUT_FORMAT
    | TIME_OUTPUT_FORMAT
    | TRANSACTION_ABORT_ON_ERROR
    | TRANSACTION_DEFAULT_ISOLATION_LEVEL
    | TWO_DIGIT_CENTURY_START
    | UNSUPPORTED_DDL_ACTION
    | USE_CACHED_RESULT
    | USER_TASK_MANAGED_INITIAL_WAREHOUSE_SIZE
    | USER_TASK_TIMEOUT_MS
    | WEEK_OF_YEAR_POLICY
    | WEEK_START
    ;

session_parameter_list
    : session_parameter (COMMA session_parameter)*
    ;

session_parameter_init_list
    : session_parameter_init (COMMA session_parameter_init)*
    ;

session_parameter_init
    : session_parameter EQ true_false
    ;

create_task
    : CREATE or_replace? TASK if_not_exists? object_name
        ( (WAREHOUSE EQ string) | (USER_TASK_MANAGED_INITIAL_WAREHOUSE_SIZE EQ string ) )?
        ( SCHEDULE EQ string )?
        ( ALLOW_OVERLAPPING_EXECUTION EQ true_false )?
        session_parameter_init_list?
        ( USER_TASK_TIMEOUT_MS EQ num )?
        ( SUSPEND_TASK_AFTER_NUM_FAILURES EQ num )?
        ( ERROR_INTEGRATION EQ id_ )?
        copy_grants?
        comment_clause?
        ( AFTER object_name (COMMA object_name)* )?
        ( WHEN search_condition )?
      AS
        sql
    ;

sql
    : EXECUTE IMMEDIATE DBL_DOLLAR
    | sql_command
    | call
    ;

call
    : CALL object_name '(' expr_list? ')'
    ;

create_user
    : CREATE or_replace? USER if_not_exists? id_ object_properties? object_params? session_params?
    ;

view_col
    : column_name with_masking_policy with_tags
    ;

create_view
    : CREATE or_replace? SECURE? RECURSIVE? VIEW if_not_exists? object_name
        ( LR_BRACKET column_list_with_comment RR_BRACKET )?
        view_col*
        with_row_access_policy?
        with_tags?
        copy_grants?
        comment_clause?
        AS query_statement
    ;

create_warehouse
    : CREATE or_replace? WAREHOUSE if_not_exists? id_fn
              ( WITH? wh_properties+ )?
              wh_params*
    ;

wh_properties
    : WAREHOUSE_SIZE EQ (XSMALL | SMALL | MEDIUM | LARGE | XLARGE | XXLARGE | XXXLARGE | X4LARGE | X5LARGE | X6LARGE | ID2)
    | MAX_CLUSTER_COUNT EQ num
    | MIN_CLUSTER_COUNT EQ num
    | SCALING_POLICY EQ (STANDARD | ECONOMY)
    | AUTO_SUSPEND (EQ num | NULL_)
    | AUTO_RESUME EQ true_false
    | INITIALLY_SUSPENDED EQ true_false
    | RESOURCE_MONITOR EQ id_
    | comment_clause
    | ENABLE_QUERY_ACCELERATION EQ true_false
    | QUERY_ACCELERATION_MAX_SCALE_FACTOR EQ num
    ;

wh_params
    : MAX_CONCURRENCY_LEVEL EQ num
    | STATEMENT_QUEUED_TIMEOUT_IN_SECONDS EQ num
    | STATEMENT_TIMEOUT_IN_SECONDS EQ num
      with_tags?
    ;

trigger_definition
    : ON num PERCENT DO ( SUSPEND | SUSPEND_IMMEDIATE | NOTIFY )
    ;

object_type_name
    : ROLE
    | USER
    | WAREHOUSE
    | INTEGRATION
    | NETWORK POLICY
    | SESSION POLICY
    | DATABASE
    | SCHEMA
    | TABLE
    | VIEW
    | STAGE
    | FILE FORMAT
    | STREAM
    | TASK
    | MASKING POLICY
    | ROW ACCESS POLICY
    | TAG
    | PIPE
    | FUNCTION
    | PROCEDURE
    | SEQUENCE
    ;

object_type_plural
    : ROLES
    | USERS
    | WAREHOUSES
    | INTEGRATIONS
    | DATABASES
    | SCHEMAS
    | TABLES
    | VIEWS
    | STAGES
    | STREAMS
    | TASKS
    ;

// drop commands
drop_command
    : drop_object
    | drop_alert
    | drop_connection
    | drop_database
    | drop_dynamic_table
    //| drop_event_table //uses DROP TABLE stmt
    | drop_external_table
    | drop_failover_group
    | drop_file_format
    | drop_function
    | drop_integration
    | drop_managed_account
    | drop_masking_policy
    | drop_materialized_view
    | drop_network_policy
    | drop_pipe
    | drop_procedure
    | drop_replication_group
    | drop_resource_monitor
    | drop_role
    | drop_row_access_policy
    | drop_schema
    | drop_sequence
    | drop_session_policy
    | drop_share
    | drop_stage
    | drop_stream
    | drop_table
    | drop_tag
    | drop_task
    | drop_user
    | drop_view
    | drop_warehouse
    ;

drop_object
    : DROP object_type if_exists id_ cascade_restrict?
    ;

drop_alert
    : DROP ALERT id_
    ;

drop_connection
    : DROP CONNECTION if_exists? id_
    ;

drop_database
    : DROP DATABASE if_exists? id_ cascade_restrict?
    ;

drop_dynamic_table
    : DROP DYNAMIC TABLE id_
    ;

drop_external_table
    : DROP EXTERNAL TABLE if_exists? object_name cascade_restrict?
    ;

drop_failover_group
    : DROP FAILOVER GROUP if_exists? id_
    ;

drop_file_format
    : DROP FILE FORMAT if_exists? id_
    ;

drop_function
    : DROP FUNCTION if_exists? object_name arg_types
    ;

drop_integration
    : DROP ( API | NOTIFICATION | SECURITY | STORAGE )? INTEGRATION if_exists? id_
    ;

drop_managed_account
    : DROP MANAGED ACCOUNT id_
    ;

drop_masking_policy
    : DROP MASKING POLICY id_
    ;

drop_materialized_view
    : DROP MATERIALIZED VIEW if_exists? object_name
    ;

drop_network_policy
    : DROP NETWORK POLICY if_exists? id_
    ;

drop_pipe
    : DROP PIPE if_exists? object_name
    ;

drop_procedure
    : DROP PROCEDURE if_exists? object_name arg_types
    ;

drop_replication_group
    : DROP REPLICATION GROUP if_exists? id_
    ;

drop_resource_monitor
    : DROP RESOURCE MONITOR id_
    ;

drop_role
    : DROP ROLE if_exists? id_
    ;

drop_row_access_policy
    : DROP ROW ACCESS POLICY if_exists? id_
    ;

drop_schema
    : DROP SCHEMA if_exists? schema_name cascade_restrict?
    ;

drop_sequence
    : DROP SEQUENCE if_exists? object_name cascade_restrict?
    ;

drop_session_policy
    : DROP SESSION POLICY if_exists? id_
    ;

drop_share
    : DROP SHARE id_
    ;

drop_stream
    : DROP STREAM if_exists? object_name
    ;

drop_table
    : DROP TABLE if_exists? object_name cascade_restrict?
    ;

drop_tag
    : DROP TAG if_exists? object_name
    ;

drop_task
    : DROP TASK if_exists? object_name
    ;

drop_user
    : DROP USER if_exists? id_
    ;

drop_view
    : DROP VIEW if_exists? object_name
    ;

drop_warehouse
    : DROP WAREHOUSE if_exists? id_fn
    ;

cascade_restrict
    : CASCADE | RESTRICT
    ;

arg_types
    : LR_BRACKET data_type_list? RR_BRACKET
    ;

// undrop commands
undrop_command
    //: undrop_object
    : undrop_database
    | undrop_schema
    | undrop_table
    | undrop_tag
    ;

undrop_database
    : UNDROP DATABASE id_
    ;

undrop_schema
    : UNDROP SCHEMA schema_name
    ;

undrop_table
    : UNDROP TABLE object_name
    ;

undrop_tag
    : UNDROP TAG object_name
    ;

// use commands
use_command
    : use_database
    | use_role
    | use_schema
    | use_secondary_roles
    | use_warehouse
    ;

use_database
    : USE DATABASE id_
    ;

use_role
    : USE ROLE id_
    ;

use_schema
    : USE SCHEMA? (id_ DOT)? id_
    ;

use_secondary_roles
    : USE SECONDARY ROLES ( ALL | NONE )
    ;

use_warehouse
    : USE WAREHOUSE id_fn
    ;

/* */
comment_clause
    : COMMENT EQ string
    ;

if_suspended
    : IF SUSPENDED
    ;

if_exists
    : IF EXISTS
    ;

if_not_exists
    : IF NOT EXISTS
    ;

or_replace
    : OR REPLACE
    ;

describe
    : DESC
    | DESCRIBE
    ;

// describe command
describe_command
    : describe_alert
    | describe_database
    | describe_dynamic_table
    | describe_event_table
    | describe_external_table
    | describe_file_format
    | describe_function
    | describe_integration
    | describe_masking_policy
    | describe_materialized_view
    | describe_network_policy
    | describe_pipe
    | describe_procedure
    | describe_result
    | describe_row_access_policy
    | describe_schema
    | describe_search_optimization
    | describe_sequence
    | describe_session_policy
    | describe_share
    | describe_stage
    | describe_stream
    | describe_table
    | describe_task
    | describe_transaction
    | describe_user
    | describe_view
    | describe_warehouse
    ;

describe_alert
    : describe ALERT id_
    ;

describe_database
    : describe DATABASE id_
    ;

describe_dynamic_table
    : describe DYNAMIC TABLE id_
    ;

describe_event_table
    : describe EVENT TABLE id_
    ;

describe_external_table
    : describe EXTERNAL? TABLE object_name ( TYPE EQ (COLUMNS | STAGE) )?
    ;

describe_file_format
    : describe FILE FORMAT id_
    ;

describe_function
    : describe FUNCTION object_name arg_types
    ;

describe_integration
    : describe ( API | NOTIFICATION | SECURITY | STORAGE )? INTEGRATION id_
    ;

describe_masking_policy
    : describe MASKING POLICY id_
    ;

describe_materialized_view
    : describe MATERIALIZED VIEW object_name
    ;

describe_network_policy
    : describe NETWORK POLICY id_
    ;

describe_pipe
    : describe PIPE object_name
    ;

describe_procedure
    : describe PROCEDURE object_name arg_types
    ;

describe_result
    : describe RESULT ( STRING | LAST_QUERY_ID LR_BRACKET RR_BRACKET )
    ;

describe_row_access_policy
    : describe ROW ACCESS POLICY id_
    ;

describe_schema
    : describe SCHEMA schema_name
    ;

describe_search_optimization
    : describe SEARCH OPTIMIZATION ON object_name
    ;

describe_sequence
    : describe SEQUENCE object_name
    ;

describe_session_policy
    : describe SESSION POLICY id_
    ;

describe_share
    : describe SHARE id_
    ;

describe_stream
    : describe STREAM object_name
    ;

describe_table
    : describe TABLE object_name ( TYPE EQ (COLUMNS | STAGE ) )?
    ;

describe_task
    : describe TASK object_name
    ;

describe_transaction
    : describe TRANSACTION num
    ;

describe_user
    : describe USER id_
    ;

describe_view
    : describe VIEW object_name
    ;

describe_warehouse
    : describe WAREHOUSE id_
    ;

// show commands
show_command
    : show_alerts
    | show_channels
    | show_columns
    | show_connections
    | show_databases
    | show_databases_in_failover_group
    | show_databases_in_replication_group
    | show_delegated_authorizations
    | show_dynamic_tables
    | show_event_tables
    | show_external_functions
    | show_external_tables
    | show_failover_groups
    | show_file_formats
    | show_functions
    | show_global_accounts
    | show_grants
    | show_integrations
    | show_locks
    | show_managed_accounts
    | show_masking_policies
    | show_materialized_views
    | show_network_policies
    | show_objects
    | show_organization_accounts
    | show_parameters
    | show_pipes
    | show_primary_keys
    | show_procedures
    | show_regions
    | show_replication_accounts
    | show_replication_databases
    | show_replication_groups
    | show_resource_monitors
    | show_roles
    | show_row_access_policies
    | show_schemas
    | show_sequences
    | show_session_policies
    | show_shares
    | show_shares_in_failover_group
    | show_shares_in_replication_group
    | show_stages
    | show_streams
    | show_tables
    | show_tags
    | show_tasks
    | show_transactions
    | show_user_functions
    | show_users
    | show_variables
    | show_views
    | show_warehouses
    ;

show_alerts
    : SHOW TERSE? ALERTS like_pattern?
        ( IN ( ACCOUNT | DATABASE id_? | SCHEMA schema_name? ) )?
        starts_with?
        limit_rows?
    ;

show_channels
    : SHOW CHANNELS like_pattern? ( IN ( ACCOUNT | DATABASE id_? | SCHEMA schema_name? | TABLE | TABLE? object_name) )?
    ;

show_columns
    : SHOW COLUMNS like_pattern? ( IN ( ACCOUNT | DATABASE id_? | SCHEMA schema_name? | TABLE | TABLE? object_name | VIEW |  VIEW? object_name ) )?
    ;

show_connections
    : SHOW CONNECTIONS like_pattern?
    ;

starts_with
    : STARTS WITH string
    ;

limit_rows
    : LIMIT num ( FROM string )?
    ;

show_databases
    : SHOW TERSE? DATABASES HISTORY? like_pattern?
       starts_with?
       limit_rows?
    ;

show_databases_in_failover_group
    : SHOW DATABASES IN FAILOVER GROUP id_
    ;

show_databases_in_replication_group
    : SHOW DATABASES IN REPLICATION GROUP id_
    ;

show_delegated_authorizations
    : SHOW DELEGATED AUTHORIZATIONS
    | SHOW DELEGATED AUTHORIZATIONS BY USER id_
    | SHOW DELEGATED AUTHORIZATIONS TO SECURITY INTEGRATION id_
    ;

show_dynamic_tables
    : SHOW DYNAMIC TABLES like_pattern?
        (IN ( ACCOUNT | DATABASE id_? | SCHEMA? schema_name?))?
        starts_with?
        limit_rows?
    ;

show_event_tables
    : SHOW TERSE? EVENT TABLES like_pattern?
        ( IN ( ACCOUNT | DATABASE id_? | SCHEMA? schema_name? ) )?
        starts_with?
        limit_rows?
    ;

show_external_functions
    : SHOW EXTERNAL FUNCTIONS like_pattern?
    ;

show_external_tables
    : SHOW TERSE? EXTERNAL TABLES like_pattern?
         ( IN ( ACCOUNT | DATABASE id_? | SCHEMA? schema_name? ) )?
         starts_with?
         limit_rows?
    ;

show_failover_groups
    : SHOW FAILOVER GROUPS ( IN ACCOUNT id_ )?
    ;

show_file_formats
    : SHOW FILE FORMATS like_pattern?
                        ( IN
                             (
                                ACCOUNT  |
                                DATABASE   |
                                DATABASE id_ |
                                SCHEMA   |
                                SCHEMA schema_name  |
                                schema_name
                             )
                        )?
    ;

show_functions
    : SHOW FUNCTIONS like_pattern? ( IN ( ACCOUNT | DATABASE | DATABASE id_ | SCHEMA | SCHEMA id_ | id_ ) )?
    ;

show_global_accounts
    : SHOW GLOBAL ACCOUNTS like_pattern?
    ;

show_grants
    : SHOW GRANTS show_grants_opts?
    | SHOW FUTURE GRANTS IN SCHEMA schema_name
    | SHOW FUTURE GRANTS IN DATABASE id_
    ;

show_grants_opts
    : ON ACCOUNT
    | ON object_type object_name
    | TO (ROLE id_ | USER id_ | SHARE id_ )
    | OF ROLE id_
    | OF SHARE id_
    ;

show_integrations
    : SHOW ( API | NOTIFICATION | SECURITY | STORAGE )? INTEGRATIONS like_pattern?
    ;

show_locks
    : SHOW LOCKS ( IN ACCOUNT )?
    ;

show_managed_accounts
    : SHOW MANAGED ACCOUNTS like_pattern?
    ;

show_masking_policies
    : SHOW MASKING POLICIES  like_pattern? in_obj?
    ;

in_obj
    : IN ( ACCOUNT
         | DATABASE
         | DATABASE id_
         | SCHEMA
         | SCHEMA schema_name
         | schema_name
         )
    ;

in_obj_2
    : IN ( ACCOUNT
         | DATABASE id_?
         | SCHEMA schema_name?
         | TABLE
         | TABLE object_name
         )
    ;

show_materialized_views
    : SHOW MATERIALIZED VIEWS like_pattern? in_obj?
    ;

show_network_policies
    : SHOW NETWORK POLICIES
    ;

show_objects
    : SHOW OBJECTS like_pattern? in_obj?
    ;

show_organization_accounts
    : SHOW ORGANIZATION ACCOUNTS like_pattern?
    ;

in_for
    : IN | FOR
    ;

show_parameters
    : SHOW PARAMETERS like_pattern?
                      ( in_for ( SESSION | ACCOUNT | USER id_? | ( WAREHOUSE | DATABASE | SCHEMA | TASK ) id_? | TABLE object_name ) )?
    ;

show_pipes
    : SHOW PIPES like_pattern? in_obj?
    ;

show_primary_keys
    : SHOW TERSE? PRIMARY KEYS in_obj_2?
    ;

show_procedures
    : SHOW PROCEDURES like_pattern? in_obj?
    ;

show_regions
    : SHOW REGIONS like_pattern?
    ;

show_replication_accounts
    : SHOW REPLICATION ACCOUNTS like_pattern?
    ;

show_replication_databases
    : SHOW REPLICATION DATABASES like_pattern? ( WITH PRIMARY account_identifier DOT id_ )?
    ;

show_replication_groups
    : SHOW REPLICATION GROUPS (IN ACCOUNT id_ )?
    ;

show_resource_monitors
    : SHOW RESOURCE MONITORS like_pattern?
    ;

show_roles
    : SHOW ROLES like_pattern?
    ;

show_row_access_policies
    : SHOW ROW ACCESS POLICIES like_pattern? in_obj?
    ;

show_schemas
    : SHOW TERSE? SCHEMAS HISTORY? like_pattern?
         ( IN ( ACCOUNT | DATABASE id_? ) )?
         starts_with?
         limit_rows?
    ;

show_sequences
    : SHOW SEQUENCES like_pattern? in_obj?
    ;

show_session_policies
    : SHOW SESSION POLICIES
    ;

show_shares
    : SHOW SHARES like_pattern?
    ;

show_shares_in_failover_group
    : SHOW SHARES IN FAILOVER GROUP id_
    ;

show_shares_in_replication_group
    : SHOW SHARES IN REPLICATION GROUP id_
    ;

show_streams
    : SHOW STREAMS like_pattern? in_obj?
    ;

show_tables
    : SHOW TABLES like_pattern? in_obj?
    ;

show_tags
    : SHOW TAGS like_pattern? ( IN ACCOUNT | DATABASE | DATABASE id_ | SCHEMA | SCHEMA schema_name | schema_name )?
    ;

show_tasks
    : SHOW TERSE? TASKS like_pattern? ( IN ( ACCOUNT | DATABASE id_? | SCHEMA? schema_name? ) )?
       starts_with?
       limit_rows?
    ;

show_transactions
    : SHOW TRANSACTIONS ( IN ACCOUNT )?
    ;

show_user_functions
    : SHOW USER FUNCTIONS like_pattern? in_obj?
    ;

show_users
    : SHOW TERSE? USERS like_pattern?
      ( STARTS WITH string )?
      ( LIMIT num )?
      ( FROM string )?
    ;

show_variables
    : SHOW VARIABLES like_pattern?
    ;

show_views
    : SHOW TERSE? VIEWS like_pattern?
       ( IN ( ACCOUNT | DATABASE id_? |  SCHEMA? schema_name? ) )?
       starts_with?
       limit_rows?
    ;

show_warehouses
    : SHOW WAREHOUSES like_pattern?
    ;

like_pattern
    : LIKE string
    ;

//names
account_identifier
    : id_
    ;

schema_name
    : id_ DOT id_
    | id_
    ;

object_type
    : ACCOUNT PARAMETERS
    | DATABASES
    | INTEGRATIONS
    | NETWORK POLICIES
    | RESOURCE MONITORS
    | ROLES
    | SHARES
    | USERS
    | WAREHOUSES
    ;

object_type_list
    : object_type (COMMA object_type)*
    ;

tag_value
    : string
    ;

arg_data_type
    : id_
    ;

arg_name
    : id_
    ;

param_name
    : id_
    ;

region_group_id
    : id_
    ;

snowflake_region_id
    : id_
    ;

string
    : STRING
    ;

string_list
    : string (COMMA string)*
    ;

id_fn
    : id_
    | IDENTIFIER '(' id_ ')'
    ;

id_
    : ID
    | ID2
    | DOUBLE_QUOTE_ID
    | DOUBLE_QUOTE_BLANK
    | keyword
    | non_reserved_words
    | data_type
    | builtin_function
    | ALERT
    | ALERTS
    | CONDITION
    | binary_builtin_function
    ;

keyword
    : INT
    | BIGINT
    | STAGE
    | USER
    | TYPE
    | CLUSTER
    | TEMP
    | FUNCTION
    | REGION
    | ROLLUP
    | AT_KEYWORD
    | TIMESTAMP
    | IF
    | COPY_OPTIONS_
    // etc
    ;

non_reserved_words
    : ORGADMIN
    | ACCOUNTADMIN
    | SECURITYADMIN
    | USERADMIN
    | SYSADMIN
    | PUBLIC
    | JAVASCRIPT
    | RESULT
    | INDEX
    | SOURCE
    | PROCEDURE_NAME
    | STATE
    | ROLE
    | DEFINITION
    | TIMEZONE
    | LOCAL
    | ROW_NUMBER
    | VALUE
    | NAME
    | TAG
    | WAREHOUSE
    | VERSION
    | OPTION
    | NVL2
    | FIRST_VALUE
    | RESPECT
    | NVL
    | RESTRICT
    | VALUES
    | EVENT
    | DOWNSTREAM
    | DYNAMIC
    | TARGET_LAG
    ;

builtin_function
    // If there is a lexer entry for a function we also need to add the token here
    // as it otherwise will not be picked up by the id_ rule
    : IFF
    | SUM
    | AVG
    | MIN
    | COUNT
    | CURRENT_TIMESTAMP
    | CURRENT_DATE
    | UPPER
    | LOWER
    | TO_BOOLEAN
    | IDENTIFIER
    | FLATTEN
    | SPLIT_TO_TABLE
    ;

list_operator
    // lexer entry which admit a list of comma separated expr
    : CONCAT
    | CONCAT_WS
    | COALESCE
    // To complete as needed
    ;

binary_builtin_function
    : ifnull=( IFNULL | NVL )
    | GET
    | LEFT
    | RIGHT
    | DATE_PART
    | to_date=( TO_DATE | DATE )
    | SPLIT
    | NULLIF
    | EQUAL_NULL
    | CONTAINS
    ;

binary_or_ternary_builtin_function
    : CHARINDEX
    | REPLACE
    | substring=( SUBSTRING | SUBSTR )
    | LIKE | ILIKE
    ;

ternary_builtin_function
    : dateadd=( DATEADD | TIMEADD | TIMESTAMPADD )
    | datefiff=( DATEDIFF | TIMEDIFF | TIMESTAMPDIFF )
    | SPLIT_PART
    | NVL2
    ;

pattern
    : PATTERN EQ string
    ;

//pattern_assoc
//    : PATTERN ASSOC string
//    ;

column_name
    : (id_ '.')? id_
    ;

column_list
    : column_name (COMMA column_name)*
    ;

column_list_with_comment
    : column_name (COMMENT string)? (COMMA column_name (COMMENT string)?)*
    ;

object_name
    : d=id_ DOT s=id_ DOT o=id_
    | s=id_ DOT o=id_
    | o=id_
    ;

object_name_or_identifier
    : object_name
    | IDENTIFIER LR_BRACKET string RR_BRACKET
    ;

num
    : DECIMAL
    ;

/*** expressions ***/
expr_list
    : expr (COMMA expr)*
    ;

expr_list_sorted
    : expr asc_desc? (COMMA expr asc_desc?)*
    ;

expr
    : primitive_expression
    | function_call
    | expr COLLATE string
    | case_expression
    | iff_expr
    | full_column_name
    | bracket_expression
    | op=( PLUS | MINUS ) expr
    | op=NOT expr
    | expr op=(STAR | DIVIDE | MODULE) expr
    | expr op=(PLUS | MINUS | PIPE_PIPE) expr
    | expr op=( AND | OR | NOT ) expr //bool operation
    | expr LSB expr RSB //array access
    | arr_literal
//    | expr time_zone
    | expr COLON expr //json access
    | expr DOT VALUE
    | expr DOT expr
    | expr COLON_COLON data_type //cast
    | expr over_clause
    | CAST LR_BRACKET expr AS data_type RR_BRACKET
    | json_literal
    | binary_builtin_function LR_BRACKET expr COMMA expr RR_BRACKET
    | binary_or_ternary_builtin_function LR_BRACKET expr COMMA expr (COMMA expr)* RR_BRACKET
    | ternary_builtin_function LR_BRACKET expr COMMA expr COMMA expr RR_BRACKET
    | subquery
    | try_cast_expr
    | object_name DOT NEXTVAL
    | trim_expression
    | expr comparison_operator expr
    | expr IS null_not_null
    | expr NOT? IN LR_BRACKET (subquery | expr_list) RR_BRACKET
    | expr NOT? ( LIKE | ILIKE ) expr (ESCAPE expr)?
    | expr NOT? RLIKE expr
    | expr NOT? ( LIKE | ILIKE ) ANY LR_BRACKET expr (COMMA expr)* RR_BRACKET (ESCAPE expr)?
    ;

iff_expr
    : IFF '(' search_condition ',' expr ',' expr ')'
    ;

trim_expression
    : ( TRIM | LTRIM | RTRIM ) LR_BRACKET expr (COMMA string)* RR_BRACKET
    ;

try_cast_expr
    : TRY_CAST LR_BRACKET expr AS data_type RR_BRACKET
    ;

json_literal
    : LCB kv_pair (COMMA kv_pair)* RCB
    | LCB RCB
    ;

kv_pair
    : key=STRING COLON value
    ;

value
    : expr
    ;

arr_literal
    : LSB value (',' value)* RSB
    | LSB RSB
    ;

data_type_size
    : LR_BRACKET num RR_BRACKET
    ;

data_type
    : int_alias = ( INT | INTEGER | SMALLINT | TINYINT | BYTEINT | BIGINT )
    | number_alias = ( NUMBER | NUMERIC | DECIMAL_ ) ( LR_BRACKET num (COMMA num)? RR_BRACKET )?
    | float_alias = ( FLOAT_ | FLOAT4 | FLOAT8 | DOUBLE | DOUBLE_PRECISION | REAL_ )
    | BOOLEAN
    | DATE
    | DATETIME data_type_size?
    | TIME data_type_size?
    | TIMESTAMP data_type_size?
    | TIMESTAMP_LTZ data_type_size?
    | TIMESTAMP_NTZ data_type_size?
    | TIMESTAMP_TZ data_type_size?
    | char_alias = ( CHAR | NCHAR | CHARACTER ) data_type_size?
    | varchar_alias = ( CHAR_VARYING | NCHAR_VARYING | NVARCHAR2 | NVARCHAR | STRING_ | TEXT | VARCHAR ) data_type_size?
    | binary_alias = ( BINARY | VARBINARY ) data_type_size?
    | VARIANT
    | OBJECT
    | ARRAY
    | GEOGRAPHY
    | GEOMETRY
    ;

primitive_expression
    : DEFAULT //?
    | NULL_
    | id_
    | literal
    //| json_literal
    //| arr_literal
    ;

order_by_expr
    : ORDER BY expr_list_sorted
    ;

//order_by_expr_list
//    : ORDER BY expr_list
//    ;

//over_clause_window
//    : OVER '(' partition_by? order_by_expr (cumulative_frame | sliding_frame)? ')'
//    ;

asc_desc
    : ASC | DESC
    ;

over_clause
    : OVER '(' partition_by order_by_expr? ')'
    | OVER '(' order_by_expr ')'
    ;

function_call
    : ranking_windowed_function
    | aggregate_function
//    | aggregate_windowed_function
    | object_name '(' expr_list? ')'
    | list_operator LR_BRACKET expr_list RR_BRACKET
    | to_date=( TO_DATE | DATE ) LR_BRACKET expr RR_BRACKET
    | length= ( LENGTH | LEN ) LR_BRACKET expr RR_BRACKET
    | TO_BOOLEAN LR_BRACKET expr RR_BRACKET
    ;

ignore_or_repect_nulls
    : ( IGNORE | RESPECT ) NULLS
    ;

ranking_windowed_function
    : (RANK | DENSE_RANK | ROW_NUMBER) '(' ')' over_clause
    | NTILE '(' expr ')' over_clause
    | ( LEAD | LAG ) LR_BRACKET expr ( COMMA expr COMMA expr)? RR_BRACKET ignore_or_repect_nulls? over_clause
    | ( FIRST_VALUE | LAST_VALUE ) LR_BRACKET expr RR_BRACKET ignore_or_repect_nulls? over_clause
    ;

aggregate_function
    : id_ '(' DISTINCT? expr_list ')'
    | id_ '(' STAR ')'
    | (LISTAGG | ARRAY_AGG) '(' DISTINCT? expr (COMMA string)? ')' (WITHIN GROUP '(' order_by_clause ')' )?
    ;

//rows_range
//    : ROWS | RANGE
//    ;

//cumulative_frame
//    : rows_range BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
//    | rows_range BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING
//    ;

//preceding_following
//    : PRECEDING | FOLLOWING
//    ;

//sliding_frame
//    : ROWS BETWEEN num preceding_following AND num preceding_following
//    | ROWS BETWEEN UNBOUNDED PRECEDING AND num preceding_following
//    | ROWS BETWEEN num preceding_following AND UNBOUNDED FOLLOWING
//    ;

literal
    : STRING // string, date, time, timestamp
    | sign? DECIMAL
    | sign? (REAL | FLOAT)
    | true_false
    | NULL_
    | AT_Q
    ;

sign
    : PLUS
    | MINUS
    ;

full_column_name
    : db_name=id_? DOT schema=id_? DOT tab_name=id_? DOT col_name=id_
    | schema=id_? DOT tab_name=id_? DOT col_name=id_
    | tab_name=id_? DOT col_name=id_
    | col_name=id_
    ;

bracket_expression
    : LR_BRACKET expr RR_BRACKET
    | LR_BRACKET subquery RR_BRACKET
    ;

case_expression
    : CASE expr switch_section+ (ELSE expr)? END
    | CASE switch_search_condition_section+ (ELSE expr)? END
    ;

switch_search_condition_section
    : WHEN search_condition THEN expr
    ;

switch_section
    : WHEN expr THEN expr
    ;

// select
query_statement
    : with_expression?
      select_statement set_operators*
    ;

with_expression
    : WITH common_table_expression (COMMA common_table_expression)*
    ;

common_table_expression
    : id_ ('(' columns=column_list ')')? AS '(' select_statement set_operators* ')'
    ;

select_statement
    : select_clause select_optional_clauses limit_clause?
    | select_top_clause select_optional_clauses //TOP and LIMIT are not allowed together
    ;

set_operators
    : (UNION ALL? | (EXCEPT | MINUS_) | INTERSECT) select_statement
    | ('(' select_statement ')')
    ;

select_optional_clauses
    : into_clause?
      from_clause?
      where_clause?
      group_by_clause?
      qualify_clause?
      order_by_clause?
    ;

select_clause
    : SELECT select_list_no_top
    ;

select_top_clause
    : SELECT select_list_top
    ;

select_list_no_top
    : all_distinct? select_list
    ;

select_list_top
    : all_distinct? top_clause? select_list
    ;

select_list
    : select_list_elem (COMMA select_list_elem)*
    ;

select_list_elem
    : column_elem
//    | udt_elem
    | expression_elem
    ;

column_elem
    : (object_name | alias DOT)? STAR
    | (object_name | alias DOT)? column_name as_alias?
    | (object_name | alias DOT)? DOLLAR column_position as_alias?
    ;

as_alias
    : AS? alias
    ;

expression_elem
    : ( expr | predicate ) as_alias?
    ;

column_position
    : num
    ;

all_distinct
    : ALL | DISTINCT
    ;

top_clause
    : TOP num
    ;

into_clause
    : INTO var_list
    ;

var_list
    : var (COMMA var);

var
    : COLON id_
    ;

from_clause
    : FROM table_sources // object_ref join_clause*
    ;

table_sources
    : table_source (',' table_source)*
    ;

table_source
    : table_source_item_joined
    //| '(' table_source ')'
    ;

table_source_item_joined
    : object_ref join_clause*
    | '(' table_source_item_joined ')' join_clause*
    ;

object_ref
    : object_name
        at_before?
        changes?
        match_recognize?
        pivot_unpivot?
        as_alias?
        sample?
    | object_name
        START WITH predicate
        CONNECT BY prior_list?
    | TABLE '(' function_call ')'
        pivot_unpivot?
        as_alias?
        sample?
    | values_table
        sample?
    | LATERAL? '(' subquery ')'
        pivot_unpivot?
        as_alias?
    | LATERAL ( flatten_table | splited_table )
        as_alias?
    //| AT id_ PATH?
    //    ('(' FILE_FORMAT ASSOC id_ COMMA pattern_assoc ')')?
    //    as_alias?
    ;

flatten_table_option
    : PATH_ ASSOC string
    | OUTER ASSOC true_false
    | RECURSIVE ASSOC true_false
    | MODE ASSOC (ARRAY_Q | OBJECT_Q | BOTH_Q)
    ;

flatten_table
    : FLATTEN LR_BRACKET ( INPUT ASSOC )? expr ( COMMA flatten_table_option )* RR_BRACKET
    ;

splited_table
    : SPLIT_TO_TABLE LR_BRACKET expr COMMA expr RR_BRACKET
    ;

prior_list
    : prior_item (COMMA prior_item)*
    ;

prior_item
    : PRIOR? id_ EQ PRIOR? id_
    ;

outer_join
    : (LEFT | RIGHT | FULL) OUTER?
    ;

join_type
    : INNER
    | outer_join
    ;

join_clause
    : join_type? JOIN object_ref ( (ON search_condition)? | (USING '(' column_list ')')? )
    //| join_type? JOIN object_ref (USING '(' column_list ')')?
    | NATURAL outer_join? JOIN object_ref
    | CROSS JOIN object_ref
    ;

at_before
    : AT_KEYWORD
      LR_BRACKET (
        TIMESTAMP ASSOC expr
        | OFFSET ASSOC expr
        | STATEMENT ASSOC string
        | STREAM ASSOC string
      ) RR_BRACKET
    | BEFORE LR_BRACKET STATEMENT ASSOC string RR_BRACKET
    ;

end
    : END '('
        TIMESTAMP ARROW string
        | OFFSET ARROW string
        | STATEMENT ARROW id_
     ')'
    ;

changes
    : CHANGES '(' INFORMATION ASSOC default_append_only ')'
        at_before end?
    ;

default_append_only
    : DEFAULT
    | APPEND ONLY
    ;

partition_by
    : PARTITION BY expr_list
    ;

alias
    : id_
    ;

expr_alias_list
    : expr AS? alias (COMMA expr AS? alias)*
    ;

measures
    : MEASURES expr_alias_list
    ;

match_opts
    : SHOW EMPTY_ MATCHES | OMIT EMPTY_ MATCHES | WITH UNMATCHED ROWS
    ;

row_match
    : (ONE ROW PER MATCH | ALL ROWS PER MATCH) match_opts?
    ;

first_last
    : FIRST |LAST
    ;

symbol
    : DUMMY
    ;

after_match
    : AFTER MATCH SKIP_ (PAST LAST ROW | TO NEXT ROW | TO first_last? symbol)
    ;

symbol_list
    : symbol AS expr (COMMA symbol AS expr)*
    ;

define
    : DEFINE symbol_list
    ;

match_recognize
    : MATCH_RECOGNIZE LR_BRACKET
        partition_by?
        order_by_clause?
        measures?
        row_match?
        after_match?
        pattern?
        define?
    RR_BRACKET
    ;

pivot_unpivot
    : PIVOT LR_BRACKET id_ LR_BRACKET id_ RR_BRACKET FOR id_ IN LR_BRACKET literal (COMMA literal)* RR_BRACKET RR_BRACKET ( as_alias column_alias_list_in_brackets? )?
    | UNPIVOT LR_BRACKET id_ FOR column_name IN LR_BRACKET column_list RR_BRACKET RR_BRACKET
    ;

column_alias_list_in_brackets
    : LR_BRACKET id_ (COMMA id_)* RR_BRACKET
    ;

expr_list_in_parentheses
    : LR_BRACKET expr_list RR_BRACKET
    ;

values_table
    : LR_BRACKET values_table_body RR_BRACKET (as_alias column_alias_list_in_brackets?)?
    | values_table_body (as_alias column_alias_list_in_brackets?)?
    ;

values_table_body
    : VALUES expr_list_in_parentheses (COMMA expr_list_in_parentheses)*
    ;

sample_method
    : row_sampling =  ( BERNOULLI | ROW )
    | block_sampling = ( SYSTEM | BLOCK )
    ;

repeatable_seed
    : (REPEATABLE | SEED) LR_BRACKET num RR_BRACKET
    ;

sample_opts
    : LR_BRACKET num ROWS? RR_BRACKET repeatable_seed?
    ;

sample
    : (SAMPLE | TABLESAMPLE) sample_method? sample_opts
    ;

search_condition
    : NOT* (predicate | '(' search_condition ')')
    | search_condition AND search_condition
    | search_condition OR search_condition
    ;

comparison_operator
    : EQ | GT | LT | LE | GE | LTGT | NE
    ;

null_not_null
    : NOT? NULL_
    ;

subquery
    : query_statement
    ;

predicate
    : EXISTS LR_BRACKET subquery RR_BRACKET
    | expr comparison_operator expr
    | expr comparison_operator (ALL | SOME | ANY) '(' subquery ')'
    | expr NOT? BETWEEN expr AND expr
    | expr NOT? IN '(' (subquery | expr_list) ')'
    | expr NOT? ( LIKE | ILIKE ) expr (ESCAPE expr)?
    | expr NOT? RLIKE expr
    | expr NOT? ( LIKE | ILIKE ) ANY LR_BRACKET expr (COMMA expr)* RR_BRACKET (ESCAPE expr)?
    | expr IS null_not_null
    | expr
    ;

where_clause
    : WHERE search_condition
    ;

group_item
    : id_ | num | expr
    ;

group_by_clause
    : GROUP BY group_item (COMMA group_item)* having_clause?
    | GROUP BY (CUBE | GROUPING SETS | ROLLUP) LR_BRACKET group_item (COMMA group_item)* RR_BRACKET
    | GROUP BY ALL
    ;

having_clause
    : HAVING search_condition
    ;

qualify_clause
    : QUALIFY expr
    ;

order_item
    : ( id_ | num | expr ) (ASC | DESC)? ( NULLS ( FIRST | LAST ) )?
    ;

order_by_clause
    : ORDER BY order_item (COMMA order_item)*
    ;

row_rows
    : ROW | ROWS
    ;

first_next
    : FIRST | NEXT
    ;

limit_clause
    : LIMIT num (OFFSET num)?
    | (OFFSET num )? row_rows? FETCH first_next? num row_rows? ONLY?
    ;
