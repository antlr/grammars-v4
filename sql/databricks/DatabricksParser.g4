/*
Databricks Database grammar.
The MIT License (MIT).

Copyright (c) 2025, Michał Lorek.

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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar DatabricksParser;

options {
    tokenVocab = DatabricksLexer;
}

databricks_file
    : statement_list? EOF
    ;

statement_list
    : statement (SEMI statement)* SEMI?
    ;

statement
    : ddl_statement
    | dml_statement
    | data_retrieval_statement
    | show_statement
    | describe_command
    | misc_statement
    | resource_management_statement
    | security_statement
    ;

ddl_statement
    : alter_statement
    | create_statement
    | drop_statement
    | comment_on
    | declare_variable
    | msck_repair_table
    | refresh_statement
    | set_tag
    | sync
    | truncate_table
    | undrop_table
    | unset_tag
    ;

dml_statement
    : copy_into_statement
    | delete_statement
    | insert_statement
    | insert_overwrite_directory_statement
    | insert_overwrite_directory_hive_format_statement
    | load_data_statement
    | merge_into_statement
    | update_statement
    ;

data_retrieval_statement
    : query_statement
    | select_statement
    | values_statement
    | sql_pipeline
    | explain_statement
    ;

insert_statement
    : INSERT OVERWRITE? INTO object_name column_list_in_parentheses? (
        values_builder
        | query_statement
    )
    ;

insert_overwrite_directory_statement
    :
    ;

insert_overwrite_directory_hive_format_statement
    :
    ;

load_data_statement
    :
    ;

merge_into_statement
    :
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
    : MERGE INTO object_name as_alias? USING table_source ON search_condition merge_matches
    ;

merge_matches
    : merge_cond+
    ;

merge_cond
    : (WHEN MATCHED (AND search_condition)? THEN merge_update_delete)+
    | WHEN NOT MATCHED (AND search_condition)? THEN merge_insert
    ;

merge_update_delete
    : UPDATE SET column_name EQ expr (',' column_name EQ expr)*
    | DELETE
    ;

merge_insert
    : INSERT ('(' column_list ')')? VALUES '(' expr_list ')'
    ;

update_statement
    : UPDATE object_name as_alias? SET column_name EQ expr (COMMA column_name EQ expr)* (
        FROM table_sources
    )? (WHERE search_condition)?
    ;

table_or_query
    : object_name as_alias?
    | '(' subquery ')' as_alias?
    ;

copy_into_statement
    :
    ;

delete_statement
    : DELETE FROM object_name as_alias? (USING table_or_query (COMMA table_or_query)?)? (
        WHERE search_condition
    )?
    ;

values_builder
    : VALUES '(' expr_list ')' (COMMA '(' expr_list ')')?
    ;

misc_statement
    : call
    | execute_immediate
    | reset
    | set
    | set_recipient
    | set_timezone
    | set_variable
    | use_catalog
    | use_database
    | use_schema
    ;

resource_management_statement
    : add_archive
    | add_file
    | add_jar
    | list_archive
    | list_file
    | list_jar
    | get
    | put_into
    | remove
    ;

security_statement
    : alter_group
    | create_group
    | deny
    | drop_group
    | grant
    | grant_share
    | repair_privileges
    | revoke
    | revoke_share
    | show_grants
    | show_grants_on_share
    | show_grants_to_recipient
    ;


comment_on
    : COMMENT ON (
        CATALOG catalog_name |
        COLUMN relation_name DOT column_name |
        CONNECTION connection_name |
        PROVIDER provider_name |
        (SCHEMA | DATABASE) schema_name |
        SHARE share_name |
        TABLE relation_name |
        VOLUME volume_name
        ) IS string
    ;

declare_variable
    : DECLARE or_replace? VARIABLE? variable_name (COMMA variable_name)* data_type?
        ((DEFAULT | EQ) expr)?
    ;

msck_repair_table
    : MSCK? REPAIR TABLE table_name
        (
        ( (ADD | DROP | SYNC) PARTITIONS)?
        | SYNC METADATA
        )
    ;

refresh_statement
    : REFRESH FOREIGN (
        CATALOG catalog_name |
        SCHEMA schema_name (RESOLVE DBFS LOCATION)? |
        TABLE table_name (RESOLVE DBFS LOCATION)?
    )
    | REFRESH (MATERIALIZED VIEW | STREAMING? TABLE) table_name (FULL | SYNC | ASYNC)?
    ;

set_tag
    : SET TAG ON (
        CATALOG catalog_name |
        COLUMN relation_name DOT column_name |
        (SCHEMA | DATABASE) schema_name |
        TABLE relation_name |
        VIEW relation_name |
        VOLUME volume_name
    ) k=id_ (EQ v=id_)?
    ;

sync
    : SYNC (
        SCHEMA ts=schema_name (AS EXTERNAL)? FROM ss=schema_name |
        TABLE tt=table_name (AS EXTERNAL)? FROM st=table_name
    )
    (SET OWNER principal)?
    (DRY RUN)?
    ;

undrop_table
    : UNDROP (MATERIALIZED VIEW | TABLE) (relation_name | WITH ID relation_id)
    ;

unset_tag
    : UNSET TAG ON (
        CATALOG catalog_name |
        COLUMN relation_name DOT column_name |
        (SCHEMA | DATABASE) schema_name |
        TABLE relation_name |
        VIEW relation_name |
        VOLUME volume_name
    ) k=id_
    ;

execute_immediate
    : EXECUTE IMMEDIATE string (INTO variable_name_list)? (USING )? //TODO
    ;

variable_name_list
    : variable_name (COMMA variable_name)*
    ;

reset
    : RESET id_?
    ;

set_recipient
    : SET RECIPIENT recipient_name
    ;

set_timezone
    : SET TIME ZONE (LOCAL ) //TODO
    ;

set_variable
    : SET (VAR | VARIABLE) variable_name EQ (expr | DEFAULT) //TODO
    | SET (VAR | VARIABLE) '(' variable_name_list ')' EQ '(' query_statement ')'
    ;

use_catalog
    : (USE | SET) CATALOG (catalog_name | string)?
    ;

set
    : SET id_ EQ expr
    | SET LR_BRACKET id_ (COMMA id_)* RR_BRACKET EQ LR_BRACKET expr (COMMA expr)* RR_BRACKET
    ;

truncate_table
    : TRUNCATE TABLE table_name partition_clause?
    ;

partition_clause
    : PARTITION '(' partition_column_list ')'
    ;

partition_column_list
    : partition_column (EQ partition_value | LIKE string)
    ;

partition_value
    : string
    | num
    ;


alter_statement
    : alter_catalog
    | alter_connection
    | alter_credential
    | alter_database
    | alter_location
    | alter_materialized_view
    | alter_provider
    | alter_recipient
    | alter_schema
    | alter_share
    | alter_streaming_table
    | alter_table
    | alter_view
    | alter_volume
    ;


default_ddl_collation
    : DEFAULT_DDL_COLLATION_ EQ string
    ;

resume_suspend
    : RESUME
    | SUSPEND
    ;

catalog_name
    : id_
    ;

default_collation_name
    :
    ;

location_name
    :
    ;

principal
    : id_
    ;

connection_name
    :
    ;

clean_room_name
    :
    ;

credential_name
    :
    ;

file_name
    : string
    ;

partition_column
    : id_
    ;

resource_name
    : string
    ;

function_name
    :
    ;

metadata_name
    :
    ;

procedure_name
    :
    ;

provider_name
    :
    ;

recipient_name
    :
    ;

relation_id
    : string //UUID
    ;

relation_name
    :
    ;

share_name
    :
    ;

table_name
    :
    ;

variable_name
    :
    ;

view_name
    :
    ;

volume_name
    :
    ;

alter_catalog
    : ALTER CATALOG catalog_name?
        DEFAULT COLLATION default_collation_name
        | SET? OWNER TO principal
        SET TAGS '(' tag_list ')' | UNSET TAGS '(' tag_list ')'
        | (ENABLE | DISABLE | INHERIT) PREDICTIVE OPTIMIZATION
        | OPTIONS '(' ')'
    ;

alter_connection
    : ALTER CONNECTION connection_name
    (SET? OWNER TO principal | RENAME TO n=connection_name | OPTIONS '(' option_list ')')
    ;

option_list
    :
    ;

alter_credential
    : ALTER storage_service CREDENTIAL credential_name
    (RENAME TO n=credential_name | SET? OWNER TO principal)
    ;

alter_database
    : ALTER DATABASE if_exists? id_ RENAME TO id_
    | ALTER DATABASE if_exists? id_ SWAP WITH id_
    | ALTER DATABASE if_exists? id_ SET (DATA_RETENTION_TIME_IN_DAYS EQ num)? (
        MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num
    )? default_ddl_collation? comment_clause?
    | ALTER DATABASE id_ set_tags
    | ALTER DATABASE id_ unset_tags
    | ALTER DATABASE if_exists? id_ UNSET database_property (COMMA database_property)*
    | ALTER DATABASE id_ ENABLE REPLICATION TO ACCOUNTS account_id_list (IGNORE EDITION CHECK)?
    | ALTER DATABASE id_ DISABLE REPLICATION ( TO ACCOUNTS account_id_list)?
    | ALTER DATABASE id_ REFRESH
    // Database Failover
    | ALTER DATABASE id_ ENABLE FAILOVER TO ACCOUNTS account_id_list
    | ALTER DATABASE id_ DISABLE FAILOVER ( TO ACCOUNTS account_id_list)?
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



data_type_list
    : data_type (COMMA data_type)*
    ;

alter_location
    : ALTER EXTERNAL LOCATION location_name
    ;

alter_materialized_view
    : ALTER MATERIALIZED VIEW id_ (
        RENAME TO id_
        | CLUSTER BY '(' expr_list ')'
        | DROP CLUSTERING KEY
        | resume_suspend RECLUSTER?
        | SET ( SECURE? comment_clause?)
        | UNSET ( SECURE | COMMENT)
    )
    ;

alter_provider
    : ALTER PROVIDER provider_name (RENAME TO n=provider_name | SET? OWNER TO principal)
    ;

alter_recipient
    : ALTER RECIPIENT recipient_name (
        RENAME TO n=recipient_name |
        SET? OWNER TO principal |
        SET PROPERTIES '(' property_key_value_list ')' |
        UNSET PROPERTIES '(' property_key_list ')'
    )
    ;

property_key_list
    : property_key (COMMA property_key)*
    ;

property_key_value_list
    : property_key_value (COMMA property_key_value)*
    ;

property_key_value
    : property_key EQ? property_value
    ;

property_key
    : id_ (DOT id_)*
    | string
    ;

property_value
    : string
    ;

alter_schema
    : ALTER SCHEMA if_exists? schema_name RENAME TO schema_name
    | ALTER SCHEMA if_exists? schema_name SWAP WITH schema_name
    | ALTER SCHEMA if_exists? schema_name SET (
        (DATA_RETENTION_TIME_IN_DAYS EQ num)? (MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num)? default_ddl_collation? comment_clause?
    )
    | ALTER SCHEMA if_exists? schema_name set_tags
    | ALTER SCHEMA if_exists? schema_name unset_tags
    | ALTER SCHEMA if_exists? schema_name UNSET schema_property (COMMA schema_property)*
    | ALTER SCHEMA if_exists? schema_name ( ENABLE | DISABLE) MANAGED
    ;

schema_property
    : DATA_RETENTION_TIME_IN_DAYS
    | MAX_DATA_EXTENSION_TIME_IN_DAYS
    | DEFAULT_DDL_COLLATION_
    | COMMENT
    ;


alter_share
    : ALTER SHARE if_exists? id_ (ADD | REMOVE) ACCOUNTS EQ id_ (COMMA id_)* (
        SHARE_RESTRICTIONS EQ true_false
    )?
    | ALTER SHARE if_exists? id_ ADD ACCOUNTS EQ id_ (COMMA id_)* (
        SHARE_RESTRICTIONS EQ true_false
    )?
    | ALTER SHARE if_exists? id_ SET (ACCOUNTS EQ id_ (COMMA id_)*)? comment_clause?
    | ALTER SHARE if_exists? id_ set_tags
    | ALTER SHARE id_ unset_tags
    | ALTER SHARE if_exists? id_ UNSET COMMENT
    ;

alter_streaming_table
    :
    ;

alter_table
    : ALTER TABLE if_exists? object_name RENAME TO object_name
    | ALTER TABLE if_exists? object_name SWAP WITH object_name
    | ALTER TABLE if_exists? object_name (
        clustering_action
        | table_column_action
        | constraint_action
    )
    | ALTER TABLE if_exists? object_name ext_table_column_action
    | ALTER TABLE if_exists? object_name search_optimization_action
    | ALTER TABLE if_exists? object_name? (
        STAGE_COPY_OPTIONS EQ '(' copy_options ')'
    )? (DATA_RETENTION_TIME_IN_DAYS EQ num)? (MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num)? (
        CHANGE_TRACKING EQ true_false
    )? default_ddl_collation? comment_clause?
    | ALTER TABLE if_exists? object_name set_tags
    | ALTER TABLE if_exists? object_name unset_tags
    | ALTER TABLE if_exists? object_name UNSET (
        DATA_RETENTION_TIME_IN_DAYS
        | MAX_DATA_EXTENSION_TIME_IN_DAYS
        | CHANGE_TRACKING
        | DEFAULT_DDL_COLLATION_
        | COMMENT
        |
    )
    ;


clustering_action
    : CLUSTER BY '(' expr_list ')'
    | RECLUSTER ( MAX_SIZE EQ num)? ( WHERE expr)?
    | resume_suspend RECLUSTER
    | DROP CLUSTERING KEY
    ;

table_column_action
    : ADD COLUMN? if_not_exists? full_col_decl (COMMA full_col_decl)*
    | RENAME COLUMN column_name TO column_name
    | alter_modify (
        '(' alter_column_clause (',' alter_column_clause)* ')'
        | alter_column_clause (',' alter_column_clause)*
    )
    | alter_modify COLUMN column_name SET MASKING POLICY id_ (
        USING '(' column_name COMMA column_list ')'
    )? FORCE?
    | alter_modify COLUMN column_name UNSET MASKING POLICY
    | alter_modify column_set_tags (COMMA column_set_tags)*
    | alter_modify column_unset_tags (COMMA column_unset_tags)*
    | DROP COLUMN? if_exists? column_list
    //| DROP DEFAULT
    ;

alter_column_clause
    : COLUMN? column_name (
        DROP DEFAULT
        | SET DEFAULT object_name DOT NEXTVAL
        | ( SET? NOT NULL_ | DROP NOT NULL_)
        | ( (SET DATA)? TYPE)? data_type
        | COMMENT string
        | UNSET COMMENT
    )
    ;

inline_constraint
    : (CONSTRAINT id_)? (
        (UNIQUE | primary_key) common_constraint_properties*
        | foreign_key REFERENCES object_name (LR_BRACKET column_name RR_BRACKET)? constraint_properties
    )
    ;

enforced_not_enforced
    : NOT? ENFORCED
    ;

deferrable_not_deferrable
    : NOT? DEFERRABLE
    ;

initially_deferred_or_immediate
    : INITIALLY (DEFERRED | IMMEDIATE)
    ;

//TODO : Some properties are mutualy exclusive ie INITIALLY DEFERRED is not compatible with NOT DEFERRABLE
// also VALIDATE | NOVALIDATE need to be after ENABLE or ENFORCED. Lot of case to handle :)
common_constraint_properties
    : enforced_not_enforced (VALIDATE | NOVALIDATE)?
    | deferrable_not_deferrable
    | initially_deferred_or_immediate
    | ( ENABLE | DISABLE) ( VALIDATE | NOVALIDATE)?
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
    : MATCH match_type = (FULL | PARTIAL | SIMPLE)
    ;

on_action
    : CASCADE
    | SET ( NULL_ | DEFAULT)
    | RESTRICT
    | NO ACTION
    ;

constraint_properties
    : common_constraint_properties*
    | foreign_key_match
    | foreign_key_match? ( on_update on_delete? | on_delete on_update?)
    ;

ext_table_column_action
    : ADD COLUMN? column_name data_type AS '(' expr ')'
    | RENAME COLUMN column_name TO column_name
    | DROP COLUMN? column_list
    ;

constraint_action
    : ADD out_of_line_constraint
    | RENAME CONSTRAINT id_ TO id_
    | alter_modify (CONSTRAINT id_ | primary_key | UNIQUE | foreign_key) column_list_in_parentheses enforced_not_enforced? (
        VALIDATE
        | NOVALIDATE
    ) (RELY | NORELY)
    | DROP (CONSTRAINT id_ | primary_key | UNIQUE | foreign_key) column_list_in_parentheses? cascade_restrict?
    | DROP PRIMARY KEY
    ;

search_optimization_action
    : ADD SEARCH OPTIMIZATION (ON search_method_with_target (COMMA search_method_with_target)*)?
    | DROP SEARCH OPTIMIZATION (ON search_method_with_target (COMMA search_method_with_target)*)?
    ;

search_method_with_target
    : (EQUALITY | SUBSTRING | GEO) '(' (STAR | expr) ')'
    ;


column_set_tags
    : COLUMN? column_name set_tags
    ;

column_unset_tags
    : COLUMN column_name unset_tags
    ;


alter_view
    : ALTER VIEW if_exists? object_name RENAME TO object_name
    | ALTER VIEW if_exists? object_name SET comment_clause
    | ALTER VIEW if_exists? object_name UNSET COMMENT
    | ALTER VIEW object_name SET SECURE
    | ALTER VIEW object_name UNSET SECURE
    | ALTER VIEW if_exists? object_name set_tags
    | ALTER VIEW if_exists? object_name unset_tags
    | ALTER VIEW if_exists? object_name ADD ROW  POLICY id_ ON column_list_in_parentheses
    | ALTER VIEW if_exists? object_name DROP ROW  POLICY id_
    | ALTER VIEW if_exists? object_name ADD ROW  POLICY id_ ON column_list_in_parentheses COMMA DROP ROW  POLICY id_
    | ALTER VIEW if_exists? object_name DROP ALL ROW  POLICIES
    | ALTER VIEW object_name alter_modify COLUMN? id_ SET MASKING POLICY id_ (
        USING '(' column_name COMMA column_list ')'
    )? FORCE?
    | ALTER VIEW object_name alter_modify COLUMN? id_ UNSET MASKING POLICY
    | ALTER VIEW object_name alter_modify COLUMN? id_ set_tags
    | ALTER VIEW object_name alter_modify COLUMN id_ unset_tags
    ;

alter_modify
    : ALTER
    | MODIFY
    ;


alter_volume
    : ALTER VOLUME volume_name (
        SET? OWNER TO principal |
        SET TAGS |
        UNSET TAGS
    )
    ;

set_tags
    : SET tag_decl_list
    ;

tag_decl_list
    : TAG object_name EQ tag_value (COMMA object_name EQ tag_value)*
    ;

unset_tags
    : UNSET tag_list
    ;

tag_list
    : TAG object_name (COMMA object_name)*
    ;


// create commands
create_statement
    : create_bloomfilter_index
    | create_catalog
    | create_connection
    | create_database
    | create_function
    | create_location
    | create_materialized_view
    | create_procedure
    | create_recipient
    | create_schema
    | create_server
    | create_share
    | create_streaming_table
    | create_table
    | create_view
    | create_volume
    ;

create_bloomfilter_index
    : CREATE BLOOMFILTER INDEX ON TABLE? table_name
        FOR COLUMNS '(' ')'
        (OPTIONS)?
    ;

create_catalog
    : CREATE CATALOG if_not_exists? catalog_name
    (USING SHARE  provider_name DOT share_name |
    MANAGED LOCATION location_path |
    COMMENT comment |
    DEFAULT COLLATION default_collation_name |
    OPTIONS '(' ')'
    )?
    ;

location_path
    : string
    ;

comment
    : string
    ;

create_connection
    : CREATE (CONNECTION | SERVER) if_not_exists? id_ (
        comment_clause?
        | (AS REPLICA OF id_ DOT id_ DOT id_ comment_clause?)
    )
    ;

create_database
    : CREATE or_replace? TRANSIENT? DATABASE if_not_exists? id_ clone_at_before? (
        DATA_RETENTION_TIME_IN_DAYS EQ num
    )? (MAX_DATA_EXTENSION_TIME_IN_DAYS EQ num)? default_ddl_collation? with_tags? comment_clause?
    ;

clone_at_before
    : CLONE id_ (
        at_before1 LR_BRACKET (TIMESTAMP ASSOC string | OFFSET ASSOC string | STATEMENT ASSOC id_) RR_BRACKET
    )?
    ;

at_before1
    : AT_KEYWORD
    | BEFORE
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


arg_decl
    : arg_name arg_data_type arg_default_value_clause?
    ;

arg_default_value_clause
    : DEFAULT expr
    ;

col_decl
    : column_name data_type virtual_column_decl?
    ;

virtual_column_decl
    : AS LR_BRACKET function_call RR_BRACKET
    | AS function_call
    ;

function_definition
    : string
    | DBL_DOLLAR
    ;

create_function
    : CREATE or_replace? SECURE? FUNCTION if_not_exists? object_name LR_BRACKET (
        arg_decl (COMMA arg_decl)*
    )? RR_BRACKET RETURNS (data_type | TABLE LR_BRACKET (col_decl (COMMA col_decl)*)? RR_BRACKET) null_not_null? (
        LANGUAGE (JAVA | PYTHON | JAVASCRIPT | SQL)
    )? (CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT)? (VOLATILE | IMMUTABLE)? (
        PACKAGES EQ '(' string_list ')'
    )? (RUNTIME_VERSION EQ (string | FLOAT))? (IMPORTS EQ '(' string_list ')')? (
        PACKAGES EQ '(' string_list ')'
    )? (HANDLER EQ string)? comment_clause? AS function_definition
    | CREATE or_replace? SECURE? FUNCTION object_name LR_BRACKET (arg_decl (COMMA arg_decl)*)? RR_BRACKET RETURNS (
        data_type
        | TABLE LR_BRACKET (col_decl (COMMA col_decl)*)? RR_BRACKET
    ) null_not_null? (CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT)? (
        VOLATILE
        | IMMUTABLE
    )? MEMOIZABLE? comment_clause? AS function_definition
    ;

create_location
    : CREATE EXTERNAL LOCATION if_not_exists? location_name
        URL string
        WITH '(' STORAGE CREDENTIAL credential_name ')'
        inline_comment_clause?
    ;

tag_decl
    : object_name EQ string
    ;

column_list_in_parentheses
    : LR_BRACKET column_list RR_BRACKET
    ;

create_materialized_view
    : CREATE or_replace? SECURE? MATERIALIZED VIEW if_not_exists? object_name (
        LR_BRACKET column_list_with_comment RR_BRACKET
    )? view_col* with_row_access_policy? with_tags? copy_grants? comment_clause? cluster_by? AS select_statement
    //NOTA MATERIALIZED VIEW accept only simple select statement at this time
    ;




caller_owner
    : CALLER
    | OWNER
    ;

executa_as
    : EXECUTE AS caller_owner
    ;

procedure_definition
    : string
    | DBL_DOLLAR
    ;

not_null
    : NOT NULL_
    ;

create_procedure
    : CREATE or_replace? PROCEDURE object_name LR_BRACKET (arg_decl (COMMA arg_decl)*)? RR_BRACKET RETURNS (
        data_type
        | TABLE LR_BRACKET (col_decl (COMMA col_decl)*)? RR_BRACKET
    ) not_null? LANGUAGE SQL (CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT)? (
        VOLATILE
        | IMMUTABLE
    )? // Note: VOLATILE and IMMUTABLE are deprecated.
    comment_clause? executa_as? AS procedure_definition
    | CREATE or_replace? SECURE? PROCEDURE object_name LR_BRACKET (arg_decl (COMMA arg_decl)*)? RR_BRACKET RETURNS data_type not_null? LANGUAGE
        JAVASCRIPT (CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT)? (
        VOLATILE
        | IMMUTABLE
    )? // Note: VOLATILE and IMMUTABLE are deprecated.
    comment_clause? executa_as? AS procedure_definition
    | CREATE or_replace? SECURE? PROCEDURE object_name LR_BRACKET (arg_decl (COMMA arg_decl)*)? RR_BRACKET RETURNS (
        data_type not_null?
        | TABLE LR_BRACKET (col_decl (COMMA col_decl)*)? RR_BRACKET
    ) LANGUAGE PYTHON RUNTIME_VERSION EQ string (IMPORTS EQ '(' string_list ')')? PACKAGES EQ '(' string_list ')' HANDLER EQ string
    //            ( CALLED ON NULL_ INPUT | RETURNS NULL_ ON NULL_ INPUT | STRICT )?
    //            ( VOLATILE | IMMUTABLE )? // Note: VOLATILE and IMMUTABLE are deprecated.
    comment_clause? executa_as? AS procedure_definition
    ;

create_recipient
    :
    ;

create_schema
    : CREATE SCHEMA if_not_exists? schema_name //TODO
    ;


create_server
    :
    ;


start_with
    : START WITH? EQ? num
    ;

increment_by
    : INCREMENT BY? EQ? num
    ;



create_share
    : CREATE or_replace? SHARE id_ comment_clause?
    ;

character
    : CHAR_LITERAL
    | AAD_PROVISIONER_Q
    | AUTO_Q
    | AVRO_Q
    | AZURE_CSE_Q
    | AZURE_Q
    | BOTH_Q
    | CSV_Q
    | GCS_SSE_KMS_Q
    | GENERIC_Q
    | GENERIC_SCIM_PROVISIONER_Q
    | JSON_Q
    | NONE_Q
    | OBJECT_Q
    | OKTA_PROVISIONER_Q
    | OKTA_Q
    | ORC_Q
    | PARQUET_Q
    | S3
    | SNOWPARK_OPTIMIZED
    | XML_Q
    ;


copy_options
    : ON_ERROR EQ (CONTINUE | SKIP_FILE | SKIP_FILE_N | SKIP_FILE_N ABORT_STATEMENT)
    | SIZE_LIMIT EQ num
    | PURGE EQ true_false
    | RETURN_FAILED_ONLY EQ true_false
    | MATCH_BY_COLUMN_NAME EQ CASE_SENSITIVE
    | CASE_INSENSITIVE
    | NONE
    | ENFORCE_LENGTH EQ true_false
    | TRUNCATECOLUMNS EQ true_false
    | FORCE EQ true_false
    ;



true_false
    : TRUE
    | FALSE
    ;

enable
    : ENABLE EQ true_false
    ;


copy_grants
    : COPY GRANTS
    ;



with_tags
    : WITH? TAG LR_BRACKET tag_decl (COMMA tag_decl)* RR_BRACKET
    ;

with_row_access_policy
    : WITH? ROW  POLICY id_ ON LR_BRACKET column_name (COMMA column_name)* RR_BRACKET
    ;

cluster_by
    : CLUSTER BY LINEAR? expr_list_in_parentheses
    ;

with_masking_policy
    : WITH? MASKING POLICY id_ (USING column_list_in_parentheses)?
    ;

collate
    : COLLATE string
    ;

order_noorder
    : ORDER
    | NOORDER
    ;

default_value
    : DEFAULT expr
    | (AUTOINCREMENT | IDENTITY) (
        LR_BRACKET num COMMA num RR_BRACKET
        | start_with
        | increment_by
        | start_with increment_by
    )? order_noorder?
    ;

foreign_key
    : FOREIGN KEY
    ;

primary_key
    : PRIMARY KEY
    ;

out_of_line_constraint
    : (CONSTRAINT id_)? (
        (UNIQUE | primary_key) column_list_in_parentheses common_constraint_properties*
        | foreign_key column_list_in_parentheses REFERENCES object_name column_list_in_parentheses constraint_properties
    ) inline_comment_clause?
    ;

//For classic table
full_col_decl
    : col_decl (collate | inline_constraint | null_not_null | default_value)* with_masking_policy? with_tags? inline_comment_clause?
    ;


create_streaming_table
    :
    ;

create_table
    : create_table_using
    | create_table_like
    | create_table_clone
    | create_table_hive_format
    ;

create_table_using
    :
    ;

create_table_like
    :
    ;

create_table_clone
    :
    ;

create_table_hive_format
    :
    ;


sql
    : EXECUTE IMMEDIATE DBL_DOLLAR
    | statement
    | call
    ;

call
    : CALL procedure_name '(' (argument_list | named_argument_list)? ')'
    ;

argument_list
    : expr_list
    ;

named_argument_list
    : id_ '=>' expr (COMMA id_ '=>' expr)*
    ;

view_col
    : column_name with_masking_policy with_tags
    ;

create_view
    : CREATE or_replace? SECURE? RECURSIVE? VIEW if_not_exists? object_name (
        LR_BRACKET column_list_with_comment RR_BRACKET
    )? view_col* with_row_access_policy? with_tags? copy_grants? comment_clause? AS query_statement
    ;

create_volume
    : CREATE EXTERNAL? VOLUME if_not_exists? volume_name
        (LOCATION location_path)?
        inline_comment_clause?
    ;

object_type_plural
    : ALERTS
    | DATABASES
    | INTEGRATIONS
    | POLICIES
    | ROLES
    | SCHEMAS
    | STAGES
    | STREAMS
    | TABLES
    | TAGS
    | TASKS
    | USERS
    | VIEWS
    ;

// drop commands
drop_statement
    : drop_bloomfilter_index
    | drop_catalog
    | drop_connection
    | drop_credential
    | drop_database
    | drop_function
    | drop_location
    | drop_procedure
    | drop_provider
    | drop_recipient
    | drop_schema
    | drop_share
    | drop_table
    | drop_variable
    | drop_view
    | drop_volume
    ;

drop_bloomfilter_index
    :
    ;

drop_catalog
    :
    ;

drop_connection
    : DROP CONNECTION if_exists? id_
    ;

drop_credential
    :
    ;

drop_database
    : DROP DATABASE if_exists? id_ cascade_restrict?
    ;

drop_function
    : DROP FUNCTION if_exists? object_name arg_types
    ;

drop_location
    :
    ;

drop_procedure
    : DROP PROCEDURE if_exists? object_name arg_types
    ;

drop_provider
    :
    ;

drop_recipient
    :
    ;

drop_schema
    : DROP SCHEMA if_exists? schema_name cascade_restrict?
    ;


drop_share
    : DROP SHARE id_
    ;



drop_table
    : DROP TABLE if_exists? object_name cascade_restrict?
    ;

drop_variable
    :
    ;

drop_view
    : DROP VIEW if_exists? object_name
    ;

drop_volume
    :
    ;

cascade_restrict
    : CASCADE
    | RESTRICT
    ;

arg_types
    : LR_BRACKET data_type_list? RR_BRACKET
    ;


use_database
    : USE DATABASE schema_name
    ;

use_schema
    : USE SCHEMA schema_name
    ;

add_archive
    : (ADD (ARCHIVE | ARCHIVES) file_name)+
    ;

add_file
    : ADD (FILE | FILES) resource_name+
    ;

add_jar
    : ADD (JAR | JARS) file_name+
    ;

list_archive
    : LIST (ARCHIVE | ARCHIVES) file_name*
    ;

list_file
    : LIST (FILE | FILES) resource_name*
    ;

list_jar
    : LIST (JAR | JARS) file_name*
    ;

get
    : GET vp=string TO lp=string
    ;

put_into
    : PUT lp=string INTO vp=string OVERWRITE?
    ;

remove
    : REMOVE vp=string
    ;

alter_group
    : ALTER GROUP p=principal (ADD | DROP)
        (
        user_group principal_list
        )
    ;

user_group
    : USER
    | GROUP
    ;

principal_list
    : principal (COMMA principal)*
    ;

create_group
    : CREATE GROUP g=principal (WITH user_group principal_list)?
    ;

deny
    : DENY ON TO principal
    ;

drop_group
    : DROP GROUP g=principal
    ;

grant
    : GRANT privilege_types ON securable_object TO principal
    ;

privilege_types
    : ALL PRIVILEGES
    | privilege_type_list
    ;

privilege_type_list
    : privilege_type (COMMA privilege_type)*
    ;

privilege_type
    : ACCESS
    | APPLY TAG
    | BROWSE
    | CREATE (
        CATALOG
        | CLEAN ROOM
        | CONNECTION
        | EXTERNAL (LOCATION | METADATA | TABLE | VOLUME)
        | FOREIGN (CATALOG | SECURABLE)
        | FUNCTION
        | MODEL VERSION?
        | MANAGED STORAGE
        | PROVIDER
        | RECIPIENT
        | SCHEMA
        | SHARE
        | storage_service CREDENTIAL
        | TABLE
        | MATERIALIZED VIEW
        | VOLUME
    )
    | EXECUTE (CLEAN ROOM TASK)?
    | EXTERNAL USE (LOCATION | SCHEMA)
    | MANAGE ALLOWLIST?
    | MODIFY (CLEAN ROOM)?
    | READ (FILES | VOLUME)
    | REFRESH
    | SELECT
    | SET SHARE PERMISSION
    | USE (
        CATALOG
        | CONNECTION
        | SCHEMA
        | MARKETPLACE ASSETS
        | PROVIDER
        | RECIPIENT
        | SHARE
    )
    | WRITE (FILES | VOLUME)
    ;

grant_share
    : GRANT SELECT ON SHARE share_name TO RECIPIENT recipient_name
    ;

repair_privileges
    : MSCK REPAIR object PRIVILEGES
    ;

schema_database
    : SCHEMA
    | DATABASE
    ;

object
    : schema_database schema_name
    | FUNCTION function_name
    | TABLE table_name
    | VIEW view_name
    | ANONYMOUS FUNCTION
    | ANY FILE
    ;

revoke
    : REVOKE privilege_types ON securable_object FROM principal
    ;

revoke_share
    : REVOKE SELECT ON SHARE share_name FROM RECIPIENT recipient_name
    ;

show_grants
    : SHOW GRANTS principal? ON securable_object
    ;

securable_object
    : CATALOG catalog_name?
    | CONNECTION connection_name
    | CLEAN ROOM clean_room_name
    | EXTERNAL LOCATION location_name
    | EXTERNAL METADATA metadata_name
    | FUNCTION function_name
    | METASTORE
    | PROCEDURE procedure_name
    | SCHEMA schema_name
    | SHARE share_name
    | storage_service? CREDENTIAL credential_name
    | TABLE? table_name
    | MATERIALIZED VIEW view_name
    | VIEW view_name
    | VOLUME volume_name
    ;

storage_service
    : STORAGE
    | SERVICE
    ;

show_grants_on_share
    : SHOW GRANTS ON SHARE
    ;

show_grants_to_recipient
    : SHOW GRANTS TO RECIPIENT recipient_name
    ;


/* */
comment_clause
    : COMMENT EQ string
    ;

inline_comment_clause
    : COMMENT string
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

describe_command
    : describe_catalog
    | describe_connection
    | describe_credential
    | describe_database
    | describe_function
    | describe_location
    | describe_procedure
    | describe_provider
    | describe_query
    | describe_recipient
    | describe_schema
    | describe_share
    | describe_table
    | describe_volume
    ;

describe_catalog
    : describe CATALOG EXTENDED? catalog_name
    ;

describe_connection
    : describe CONNECTION EXTENDED? id_ credential_name
    ;

describe_credential
    : DESCRIBE storage_service CREDENTIAL
    ;

describe_database
    : describe DATABASE EXTENDED? schema_name
    ;

describe_function
    : describe FUNCTION EXTENDED? function_name
    ;

describe_location
    : DESCRIBE EXTERNAL LOCATION location_name
    ;

describe_procedure
    : describe PROCEDURE EXTENDED? procedure_name
    ;

describe_provider
    : describe PROVIDER provider_name
    ;

describe_query
    : describe QUERY? query_statement
    ;

describe_recipient
    : describe RECIPIENT recipient_name
    ;

describe_schema
    : describe SCHEMA EXTENDED? schema_name
    ;

describe_share
    : describe SHARE share_name
    ;

describe_table
    : describe TABLE? EXTENDED? table_name (partition_clause | column_name)? (AS JSON)?
    ;

describe_volume
    : DESCRIBE VOLUME volume_name
    ;

show_statement
    : list
    | show_all_in_share
    | show_catalogs
    | show_columns
    | show_connections
    | show_create_table
    | show_credentials
    | show_databases
    | show_functions
    | show_groups
    | show_locations
    | show_partitions
    | show_procedures
    | show_providers
    | show_recipients
    | show_schemas
    | show_shares
    | show_shares_in_provider
    | show_table
    | show_tables
    | show_tables_dropped
    | show_tblproperties
    | show_users
    | show_views
    | show_volumes
    ;

list
    : LIST url=string (WITH '(' CREDENTIAL credential_name ')')? (LIMIT num)?
    ;

show_all_in_share
    : SHOW ALL IN SHARE id_
    ;

show_catalogs
    : SHOW CATALOGS like_pattern
    ;

show_columns
    : SHOW COLUMNS in_from object_name in_from id_
    ;

in_from
    : IN
    | FROM
    ;

show_connections
    : SHOW CONNECTIONS
    ;

show_create_table
    : SHOW CREATE TABLE object_name
    ;

show_credentials
    : SHOW storage_service? CREDENTIALS
    ;

show_databases
    : SHOW DATABASES (in_from catalog_name)? like_pattern?
    ;

show_functions
    : SHOW function_kind? FUNCTIONS (in_from schema_name)? like_pattern? //TODO
    ;

function_kind
    : USER
    | SYSTEM
    | ALL
    ;

in_obj
    : IN (ACCOUNT | DATABASE | DATABASE id_ | SCHEMA | SCHEMA schema_name | schema_name)
    ;

show_groups
    : SHOW GROUPS (WITH user_group principal)? like_pattern?
    ;

show_locations
    : SHOW EXTERNAL LOCATIONS
    ;

show_partitions
    : SHOW PARTITIONS table_name partition_clause?
    ;

show_procedures
    : SHOW PROCEDURES (in_from schema_name)?
    ;

show_providers
    : SHOW PROVIDERS like_pattern?
    ;

show_recipients
    : SHOW RECIPIENTS like_pattern?
    ;

show_schemas
    : SHOW SCHEMAS (in_from catalog_name)? like_pattern?
    ;

show_shares
    : SHOW SHARES like_pattern?
    ;

show_shares_in_provider
    : SHOW SHARES IN PROVIDER provider_name like_pattern?
    ;

show_table
    : SHOW TABLE EXTENDED (in_from schema_name)? LIKE string partition_clause?
    ;

show_tables
    : SHOW TABLES like_pattern? in_obj?
    ;

show_tables_dropped
    : SHOW TABLES DROPPED (in_from schema_name)? (LIMIT num)?
    ;

show_tblproperties
    : SHOW TBLPROPERTIES table_name //TODO
    ;

show_users
    : SHOW USERS like_pattern?
    ;

show_views
    : SHOW VIEWS (in_from schema_name)? like_pattern?
    ;

show_volumes
    : SHOW VOLUMES (in_from schema_name)? like_pattern?
    ;

like_pattern
    : LIKE? string
    ;

//names
account_identifier
    : id_
    ;

schema_name
    : (catalog_name DOT)? s = id_
    | id_clause
    ;

id_clause
    : IDENTIFIER '(' string ')' // TODO
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


string
    : STRING
    ;

string_list
    : string (COMMA string)*
    ;


id_
    //id_ is used for object name. Snowflake is very permissive
    //so we could use nearly all keyword as object name (table, column etc..)
    : ID
    | DOUBLE_QUOTE_BLANK
    | keyword
    | non_reserved_words
    | object_type_plural
    | data_type
    ;

keyword
    //List here keyword (SnowSQL meaning) allowed as object name
    // Name of builtin function should be included in specifique section (ie builtin_function)
    // please add in alphabetic order for easy reading
    // https://docs.snowflake.com/en/sql-reference/reserved-keywords
    : ACCOUNT
    | ACTION
    | ALERT
    | AT_KEYWORD
    | CLUSTER
    | COMMENT
    | CONDITION
    | COPY_OPTIONS_
    | DIRECTION
    | EMAIL
    | FIRST_VALUE
    | FLATTEN
    | FUNCTION
    | IF
    | JOIN
    | KEY
    | LAG
    | LANGUAGE
    | LENGTH
    | MAX_CONCURRENCY_LEVEL
    | MODE
    | NOORDER
    | ORDER
    | OUTER
    | POLICY
    | RECURSIVE
    | REGION
    | ROLE
    | ROLLUP
    | ROW_NUMBER
    | SEQUENCE
    | SESSION
    | STAGE
    | TAG
    | TARGET_LAG
    | TEMP
    | TIMESTAMP
    | TYPE
    | USER
    | VALUE
    | VALUES
    // etc
    ;

non_reserved_words
    //List here lexer token referenced by rules which is not a keyword (SnowSQL Meaning) and allowed has object name
    // please add in alphabetic order for easy reading
    : ACCOUNTADMIN
    | AES
    | ARRAY_AGG
    | CHECKSUM
    | COLLECTION
    | COMMENT
    | CONFIGURATION
    | DATA
    | DAYS
    | DEFINITION
    | DELTA
    | DISPLAY_NAME
    | DOWNSTREAM
    | DYNAMIC
    | EDITION
    | EMAIL
    | EMPTY_
    | ENABLED
    | ERROR_INTEGRATION
    | EVENT
    | EXCHANGE
    | EXPIRY_DATE
    | EXPR
    | FILE
    | FILES
    | FIRST_NAME
    | FIRST_VALUE
    | FREQUENCY
    | GLOBAL
    | HIGH
    | HOURS
    | IDENTIFIER
    | IDENTITY
    | INCREMENTAL
    | IMPORTED
    | INDEX
    | INITIALIZE
    | INPUT
    | INTERVAL
    | JAVASCRIPT
    | LAST_NAME
    | LAST_QUERY_ID
    | LEAD
    | LOCAL
    | LOW
    | MAX_CONCURRENCY_LEVEL
    | MEDIUM
    | MODE
    | NAME
    | NETWORK
    | NULLIF
    | NVL
    | OFFSET
    | OLD
    | ON_CREATE
    | ON_ERROR
    | ON_SCHEDULE
    | OPTION
    | ORGADMIN
    | OUTBOUND
    | OUTER
    | OWNER
    | PARTITION
    | PASSWORD
    | PATH_
    | PATTERN
    | PORT
    | PRIORITY
    | PROCEDURE_NAME
    | PROPERTY
    | PROVIDER
    | PUBLIC
    | QUARTER
    | QUERY
    | QUERY_TAG
    | RANK
    | RECURSIVE
    | REFERENCES
    | REFRESH_MODE
    | RESOURCE
    | RESOURCES
    | RESPECT
    | RESTRICT
    | RESULT
    | ROLE
    | ROUNDING_MODE
    | ROW_NUMBER
    | SCALE
    | SCHEDULE
    | SECURITYADMIN
    | SOURCE
    | START_DATE
    | STATE
    | STATS
    | STATUS
    | SYSADMIN
    | TAG
    | TAGS
    | TARGET_LAG
    | TIMEZONE
    | URL
    | USERADMIN
    | VALUE
    | VALUES
    | VECTOR
    | VERSION
    | VISIBILITY
    | YEAR
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
    : s = id_ DOT o = id_
    | o = id_
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
    : object_name DOT NEXTVAL
    | expr LSB expr RSB //array access
    | expr COLON expr   //json access
    | expr DOT (VALUE | expr)
    | expr COLLATE string
    | case_expression
    | iff_expr
    | bracket_expression
    | op = ( PLUS | MINUS) expr
    | expr op = (STAR | DIVIDE | MODULE) expr
    | expr op = (PLUS | MINUS | PIPE_PIPE) expr
    | l = expr comparison_operator r = expr
    | op = NOT+ expr
    | expr AND expr //bool operation
    | expr OR expr  //bool operation
    | arr_literal
    //    | expr time_zone
    | expr over_clause
    | cast_expr
    | expr COLON_COLON data_type // Cast also
    | try_cast_expr
    | json_literal
    | trim_expression
    | function_call
    | subquery
    | expr IS (null_not_null | not_distinct_from expr)
    | expr NOT? IN LR_BRACKET (subquery | expr_list) RR_BRACKET
    | expr NOT? ( LIKE | ILIKE) expr (ESCAPE expr)?
    | expr NOT? RLIKE expr
    | expr NOT? (LIKE | ILIKE) ANY LR_BRACKET expr (COMMA expr)* RR_BRACKET (ESCAPE expr)?
    | primitive_expression //Should be latest rule as it's nearly a catch all
    ;

iff_expr
    : IFF '(' search_condition ',' expr ',' expr ')'
    ;

trim_expression
    : (TRIM | LTRIM | RTRIM) LR_BRACKET expr (COMMA string)* RR_BRACKET
    ;

try_cast_expr
    : TRY_CAST LR_BRACKET expr AS data_type RR_BRACKET
    ;

cast_expr
    : CAST LR_BRACKET expr AS data_type RR_BRACKET
    | (TIMESTAMP | DATE | TIME | INTERVAL) expr
    ;

json_literal
    : LCB kv_pair (COMMA kv_pair)* RCB
    | LCB RCB
    ;

kv_pair
    : key = STRING COLON value
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
    : int_alias = (INT | INTEGER | SMALLINT | TINYINT | BYTEINT | BIGINT)
    | number_alias = (NUMBER | NUMERIC | DECIMAL_) (LR_BRACKET num (COMMA num)? RR_BRACKET)?
    | float_alias = (FLOAT_ | FLOAT4 | FLOAT8 | DOUBLE | DOUBLE_PRECISION | REAL_)
    | BOOLEAN
    | DATE
    | DATETIME data_type_size?
    | TIME data_type_size?
    | TIMESTAMP data_type_size?
    | TIMESTAMP_LTZ data_type_size?
    | TIMESTAMP_NTZ data_type_size?
    | TIMESTAMP_TZ data_type_size?
    | char_alias = ( CHAR | NCHAR | CHARACTER) data_type_size?
    | varchar_alias = (
        CHAR_VARYING
        | NCHAR_VARYING
        | NVARCHAR2
        | NVARCHAR
        | STRING_
        | TEXT
        | VARCHAR
    ) data_type_size?
    | binary_alias = ( BINARY | VARBINARY) data_type_size?
    | VARIANT
    | OBJECT
    | ARRAY
    | GEOGRAPHY
    | GEOMETRY
    | VECTOR '(' vector_element_type COMMA num ')'
    ;

vector_element_type
    : INT
    | INTEGER
    | FLOAT_
    | FLOAT4
    | FLOAT8
    ;

primitive_expression
    : DEFAULT //?
    | NULL_
    | id_ ('.' id_)* // json field access
    | id_ '.' STAR
    | full_column_name
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
    : ASC
    | DESC
    ;

over_clause
    : OVER '(' partition_by order_by_expr? ')'
    | OVER '(' order_by_expr ')'
    | OVER '(' ')'
    ;

function_call
    : round_expr
    | ranking_windowed_function
    | aggregate_function
    //    | aggregate_windowed_function
    | object_name '(' expr_list? ')'
    | object_name '(' param_assoc_list ')'
    | to_date = ( TO_DATE | DATE) LR_BRACKET expr RR_BRACKET
    | length = ( LENGTH | LEN) LR_BRACKET expr RR_BRACKET
    | TO_BOOLEAN LR_BRACKET expr RR_BRACKET
    ;

param_assoc_list
    : param_assoc (',' param_assoc)*
    ;

param_assoc
    : id_ ASSOC expr
    ;

ignore_or_repect_nulls
    : (IGNORE | RESPECT) NULLS
    ;

ranking_windowed_function
    : (RANK | DENSE_RANK | ROW_NUMBER) '(' ')' over_clause
    | NTILE '(' expr ')' over_clause
    | (LEAD | LAG) LR_BRACKET expr (COMMA expr COMMA expr)? RR_BRACKET ignore_or_repect_nulls? over_clause
    | (FIRST_VALUE | LAST_VALUE) LR_BRACKET expr RR_BRACKET ignore_or_repect_nulls? over_clause
    ;

aggregate_function
    : id_ '(' DISTINCT? expr_list ')'
    | id_ '(' STAR ')'
    | (LISTAGG | ARRAY_AGG) '(' DISTINCT? expr (COMMA string)? ')' (
        WITHIN GROUP '(' order_by_clause ')'
    )?
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
    : db_name = id_? DOT schema = id_? DOT tab_name = id_? DOT col_name = id_
    | schema = id_? DOT tab_name = id_? DOT col_name = id_
    | tab_name = id_? DOT col_name = id_
    | col_name = id_
    ;

bracket_expression
    : LR_BRACKET expr_list RR_BRACKET
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
    : with_expression? select_statement_in_parentheses set_operators*
    ;

with_expression
    : WITH RECURSIVE? common_table_expression (COMMA common_table_expression)*
    ;

common_table_expression
    : id_ (LR_BRACKET columns = column_list RR_BRACKET)? AS select_statement_in_parentheses
    ;

select_statement
    : select_clause select_optional_clauses limit_clause?
    | select_top_clause select_optional_clauses //TOP and LIMIT are not allowed together
    ;

values_statement
    :
    ;

sql_pipeline
    :
    ;

explain_statement
    : EXPLAIN (EXTENDED | CODEGEN | COST | FORMATTED)? statement
    ;

set_operators
    : (UNION ALL? by_name? | EXCEPT | MINUS_ | INTERSECT) select_statement_in_parentheses //EXCEPT and MINUS have same SQL meaning
    | select_statement_in_parentheses
    ;

by_name
    : BY NAME
    ;

select_statement_in_parentheses
    : LR_BRACKET select_statement_in_parentheses RR_BRACKET
    | select_statement_in_parentheses set_operators
    | select_statement
    | with_expression
    ;

select_optional_clauses
    : into_clause? from_clause? where_clause? (group_by_clause having_clause? | having_clause)? qualify_clause? order_by_clause?
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
    : select_list_elem (COMMA select_list_elem)* COMMA?
    ;

select_list_elem
    : column_elem as_alias?
    | column_elem_star exclude_clause?
    //    | udt_elem
    | expression_elem as_alias?
    ;

column_elem_star
    : object_name_or_alias? STAR
    ;

column_elem
    : object_name_or_alias? column_name
    | object_name_or_alias? DOLLAR column_position
    ;

object_name_or_alias
    : object_name
    | alias DOT
    ;

exclude_clause
    : EXCLUDE (column_name | column_list_in_parentheses)
    ;

as_alias
    : AS? alias
    ;

expression_elem
    : expr
    | predicate
    ;

column_position
    : num
    ;

all_distinct
    : ALL
    | DISTINCT
    ;

top_clause
    : TOP num
    ;

into_clause
    : INTO var_list
    ;

var_list
    : var (COMMA var)*
    ;

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
    : object_name at_before? changes? match_recognize? pivot_unpivot? as_alias? column_list_in_parentheses? sample?
    | object_name START WITH predicate CONNECT BY prior_list?
    | TABLE '(' function_call ')' pivot_unpivot? as_alias? sample?
    | values_table sample?
    | LATERAL? '(' subquery ')' pivot_unpivot? as_alias? column_list_in_parentheses?
    | LATERAL (flatten_table | splited_table) as_alias?
    //| AT id_ PATH?
    //    ('(' FILE_FORMAT ASSOC id_ COMMA pattern_assoc ')')?
    //    as_alias?
    ;

flatten_table_option
    : PATH_ ASSOC string
    | OUTER ASSOC true_false
    | RECURSIVE ASSOC true_false
    ;

flatten_table
    : FLATTEN LR_BRACKET (INPUT ASSOC)? expr (COMMA flatten_table_option)* RR_BRACKET
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
    : join_type? DIRECTED? JOIN object_ref on_using_clause?
    | NATURAL join_type? DIRECTED? JOIN object_ref
    | CROSS DIRECTED? JOIN object_ref
    | ASOF JOIN object_ref MATCH_CONDITION '(' expr ')' on_using_clause?
    ;

on_using_clause
    : ON search_condition | USING column_list_in_parentheses
    ;

at_before
    : AT_KEYWORD LR_BRACKET (
        TIMESTAMP ASSOC expr
        | OFFSET ASSOC expr
        | STATEMENT ASSOC string
        | STREAM ASSOC string
    ) RR_BRACKET
    | BEFORE LR_BRACKET STATEMENT ASSOC string RR_BRACKET
    ;

end
    : END LR_BRACKET (TIMESTAMP ASSOC expr | OFFSET ASSOC expr | STATEMENT ASSOC string) RR_BRACKET
    ;

changes
    : CHANGES LR_BRACKET INFORMATION ASSOC default_append_only RR_BRACKET at_before end?
    ;

default_append_only
    : DEFAULT
    | APPEND_ONLY
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
    : SHOW EMPTY_ MATCHES
    | OMIT EMPTY_ MATCHES
    | WITH UNMATCHED ROWS
    ;

row_match
    : (ONE ROW PER MATCH | ALL ROWS PER MATCH) match_opts?
    ;

first_last
    : FIRST
    | LAST
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
    : MATCH_RECOGNIZE LR_BRACKET partition_by? order_by_clause? measures? row_match? after_match? pattern? define? RR_BRACKET
    ;

pivot_unpivot
    : PIVOT LR_BRACKET id_ LR_BRACKET id_ RR_BRACKET FOR id_ IN LR_BRACKET literal (COMMA literal)* RR_BRACKET RR_BRACKET (
        as_alias column_alias_list_in_brackets?
    )?
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
    : row_sampling = (BERNOULLI | ROW)
    | block_sampling = ( SYSTEM | BLOCK)
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
    : EQ
    | GT
    | LT
    | LE
    | GE
    | LTGT
    | NE
    ;

null_not_null
    : NOT? NULL_
    ;

not_distinct_from
    : NOT? DISTINCT FROM
    ;

subquery
    : query_statement
    ;

predicate
    : EXISTS LR_BRACKET subquery RR_BRACKET
    | expr comparison_operator (ALL | SOME | ANY) '(' subquery ')'
    | expr NOT? BETWEEN expr AND expr
    | expr NOT? IN '(' (subquery | expr_list) ')'
    | expr NOT? (LIKE | ILIKE) expr (ESCAPE expr)?
    | expr NOT? RLIKE expr
    | expr NOT? (LIKE | ILIKE) ANY LR_BRACKET expr (COMMA expr)* RR_BRACKET (ESCAPE expr)?
    | expr IS null_not_null
    | expr
    ;

where_clause
    : WHERE search_condition
    ;

group_by_elem
    : column_elem
    | num
    | expression_elem
    ;

group_by_list
    : group_by_elem (COMMA group_by_elem)*
    ;

group_by_clause
    : GROUP BY group_by_list having_clause?
    | GROUP BY (CUBE | GROUPING SETS | ROLLUP) LR_BRACKET group_by_list RR_BRACKET
    | GROUP BY ALL
    ;

having_clause
    : HAVING search_condition
    ;

qualify_clause
    : QUALIFY expr
    ;

order_item
    : (id_ | num | expr) (ASC | DESC)? (NULLS ( FIRST | LAST))?
    ;

order_by_clause
    : ORDER BY order_item (COMMA order_item)*
    ;

row_rows
    : ROW
    | ROWS
    ;

first_next
    : FIRST
    | NEXT
    ;

limit_clause
    : LIMIT num (OFFSET num)?
    | (OFFSET num)? row_rows? FETCH first_next? num row_rows? ONLY?
    ;

round_mode
    : HALF_AWAY_FROM_ZERO_Q
    | HALF_TO_EVEN_Q
    ;

round_expr
    : ROUND LR_BRACKET EXPR ASSOC expr COMMA SCALE ASSOC expr (
        COMMA ROUNDING_MODE ASSOC round_mode
    )* RR_BRACKET
    | ROUND LR_BRACKET expr COMMA expr (COMMA round_mode)* RR_BRACKET
    ;