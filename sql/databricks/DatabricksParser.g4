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
    | describe_statement
    | misc_statement
    | resource_management_statement
    | security_statement
    ;

ddl_statement
    : alter_statement
    | create_statement
    | drop_statement
    | comment_on_statement
    | declare_variable
    | msck_repair_table_statement
    | refresh_statement
    | set_tag_statement
    | sync_statement
    | truncate_table_statement
    | undrop_table_statement
    | unset_tag_statement
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

describe_statement
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

misc_statement
    : call
    | execute_immediate
    | reset
    | set
    | set_recipient
    | set_timezone
    | set_variable
    | use_catalog
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

insert_statement
    : INSERT (OVERWRITE | INTO) table_name TODO
    ;

insert_overwrite_directory_statement
    : TODO
    ;

insert_overwrite_directory_hive_format_statement
    : TODO
    ;

load_data_statement
    : TODO
    ;

merge_into_statement
    : TODO
    ;

update_statement
    : UPDATE table_name as_alias? SET column_name EQ expr (COMMA column_name EQ expr)* (
        FROM TODO
    )? (WHERE expr)?
    ;

copy_into_statement
    : COPY INTO TODO
    ;

delete_statement
    : DELETE FROM object_name as_alias? (WHERE expr)?
    ;

comment_on_statement
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

msck_repair_table_statement
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

set_tag_statement
    : SET TAG ON (
        CATALOG catalog_name |
        COLUMN relation_name DOT column_name |
        (SCHEMA | DATABASE) schema_name |
        TABLE relation_name |
        VIEW relation_name |
        VOLUME volume_name
    ) k=id_ (EQ v=id_)?
    ;

sync_statement
    : SYNC (
        SCHEMA ts=schema_name (AS EXTERNAL)? FROM ss=schema_name |
        TABLE tt=table_name (AS EXTERNAL)? FROM st=table_name
    )
    (SET OWNER principal)?
    (DRY RUN)?
    ;

undrop_table_statement
    : UNDROP (MATERIALIZED VIEW | TABLE) (relation_name | WITH ID relation_id)
    ;

unset_tag_statement
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
    : EXECUTE IMMEDIATE string (INTO variable_name_list)? (USING )? TODO
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
    : SET TIME ZONE (LOCAL ) TODO
    ;

set_variable
    : SET (VAR | VARIABLE) variable_name EQ (expr | DEFAULT) TODO
    | SET (VAR | VARIABLE) '(' variable_name_list ')' EQ '(' query_statement ')'
    ;

use_catalog
    : (USE | SET) CATALOG (catalog_name | string)?
    ;

set
    : SET id_ EQ expr
    | SET LR_BRACKET id_ (COMMA id_)* RR_BRACKET EQ LR_BRACKET expr (COMMA expr)* RR_BRACKET
    ;

truncate_table_statement
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

catalog_name
    : id_
    ;

default_collation_name
    : id_
    ;

location_name
    : id_
    ;

principal
    : id_
    ;

connection_name
    : id_
    ;

clean_room_name
    : id_
    ;

credential_name
    : id_
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
    : id_
    ;

metadata_name
    : id_
    ;

procedure_name
    : id_
    ;

provider_name
    : id_
    ;

recipient_name
    : id_
    ;

relation_id
    : string //UUID
    ;

relation_name
    : id_
    ;

share_name
    : id_
    ;

table_name
    : id_
    ;

variable_name
    : id_
    ;

view_name
    : id_
    ;

volume_name
    : id_
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
    : TODO
    ;

alter_credential
    : ALTER storage_service CREDENTIAL credential_name
    (RENAME TO n=credential_name | SET? OWNER TO principal)
    ;

data_type_list
    : data_type (COMMA data_type)*
    ;

alter_location
    : ALTER EXTERNAL LOCATION location_name
    ;

alter_materialized_view
    : ALTER MATERIALIZED VIEW view_name TODO
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
    : ALTER (DATABASE | SCHEMA) schema_name TODO
    ;

alter_share
    : ALTER SHARE share_name TODO
    ;

alter_streaming_table
    : ALTER STREAMING TABLE table_name TODO
    ;

alter_table
    : ALTER TABLE TODO
    ;

alter_view
    : ALTER VIEW TODO
    ;

alter_volume
    : ALTER VOLUME volume_name (
        SET? OWNER TO principal |
        SET TAGS |
        UNSET TAGS
    )
    ;

tag_list
    : TAG object_name (COMMA object_name)*
    ;

create_statement
    : create_bloomfilter_index
    | create_catalog
    | create_connection
    | create_function
    | create_location
    | create_materialized_view
    | create_procedure
    | create_recipient
    | create_schema
    | create_share
    | create_streaming_table
    | create_table
    | create_view
    | create_volume
    ;

create_bloomfilter_index
    : CREATE BLOOMFILTER INDEX ON TABLE? table_name
        FOR COLUMNS '(' TODO ')'
        (OPTIONS TODO)?
    ;

create_catalog
    : CREATE CATALOG if_not_exists? catalog_name
    (USING SHARE provider_name DOT share_name |
    MANAGED LOCATION location_path |
    COMMENT comment |
    DEFAULT COLLATION default_collation_name |
    OPTIONS '(' TODO ')'
    )?
    ;

location_path
    : string
    ;

comment
    : string
    ;

create_connection
    : CREATE (SERVER | CONNECTION) if_not_exists? connection_name (
        TYPE (DATABRICKS | HTTP | MYSQL | POSTGRESQL | REDSHIFT | SNOWFLAKE | SQLDW | SQLSERVER)
        OPTIONS '(' TODO ')'
        inline_comment_clause?
    )
    ;

create_function
    : CREATE or_replace? TEMPORARY? FUNCTION if_not_exists? function_name TODO
    ;

create_location
    : CREATE EXTERNAL LOCATION if_not_exists? location_name
        URL string
        WITH '(' STORAGE CREDENTIAL credential_name ')'
        inline_comment_clause?
    ;

column_list_in_parentheses
    : LR_BRACKET column_list RR_BRACKET
    ;

create_materialized_view
    : CREATE or_replace? MATERIALIZED VIEW if_not_exists? view_name TODO
    ;

create_procedure
    : CREATE or_replace? PROCEDURE if_not_exists procedure_name TODO
    ;

create_recipient
    : TODO
    ;

create_schema
    : CREATE (DATABASE | SCHEMA) if_not_exists? schema_name TODO
    ;

create_share
    : CREATE or_replace? SHARE id_ comment_clause?
    ;

true_false
    : TRUE
    | FALSE
    ;

create_streaming_table
    : TODO
    ;

create_table
    : create_table_using
    | create_table_like
    | create_table_clone
    | create_table_hive_format
    ;

create_table_using
    : TODO
    ;

create_table_like
    : TODO
    ;

create_table_clone
    : TODO
    ;

create_table_hive_format
    : TODO
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

create_view
    : CREATE or_replace? TEMPORARY? VIEW if_not_exists? view_name TODO
    ;

create_volume
    : CREATE EXTERNAL? VOLUME if_not_exists? volume_name
        (LOCATION location_path)?
        inline_comment_clause?
    ;

object_type_plural
    : DATABASES
    | ROLES
    | SCHEMAS
    | STREAMS
    | TABLES
    | TAGS
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
    : DROP BLOOMFILTER INDEX ON TABLE? table_name (FOR COLUMNS column_list_in_parentheses)?
    ;

drop_catalog
    : DROP CATALOG if_exists? catalog_name cascade_restrict?
    ;

drop_connection
    : DROP CONNECTION if_exists? id_
    ;

drop_credential
    : DROP storage_service? CREDENTIAL if_exists? credential_name
    ;

drop_database
    : DROP DATABASE if_exists? id_ cascade_restrict?
    ;

drop_function
    : DROP FUNCTION if_exists? object_name arg_types
    ;

drop_location
    : DROP EXTERNAL LOCATION if_exists? location_name
    ;

drop_procedure
    : DROP PROCEDURE if_exists? object_name arg_types
    ;

drop_provider
    : DROP PROVIDER if_exists? provider_name
    ;

drop_recipient
    : DROP RECIPIENT if_exists? recipient_name
    ;

drop_schema
    : DROP SCHEMA if_exists? schema_name cascade_restrict?
    ;


drop_share
    : DROP SHARE if_exists? share_name
    ;

drop_table
    : DROP TABLE if_exists? object_name cascade_restrict?
    ;

drop_variable
    : DROP TEMPORARY VARIABLE if_exists? variable_name
    ;

drop_view
    : DROP MATERIALIZED? VIEW if_exists? view_name
    ;

drop_volume
    : DROP VOLUME if_exists? volume_name
    ;

cascade_restrict
    : CASCADE
    | RESTRICT
    ;

arg_types
    : LR_BRACKET data_type_list? RR_BRACKET
    ;

use_schema
    : USE (DATABASE | SCHEMA) schema_name
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
    : SHOW function_kind? FUNCTIONS (in_from schema_name)? like_pattern? TODO
    ;

function_kind
    : USER
    | SYSTEM
    | ALL
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
    : SHOW TABLES (in_from schema_name)? like_pattern?
    ;

show_tables_dropped
    : SHOW TABLES DROPPED (in_from schema_name)? (LIMIT num)?
    ;

show_tblproperties
    : SHOW TBLPROPERTIES table_name TODO
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

schema_name
    : (catalog_name DOT)? s = id_
    | id_clause
    ;

id_clause
    : IDENTIFIER '(' string ')' TODO
    ;

string
    : STRING
    ;

id_
    : ID
    | DOUBLE_QUOTE_BLANK
    | keyword
    | non_reserved_words
    | object_type_plural
    | data_type
    ;

keyword
    : COMMENT
    | FUNCTION
    | IF
    | JOIN
    | KEY
    | LANGUAGE
    | ORDER
    | OUTER
    | RECURSIVE
    | ROLE
    | TAG
    | TEMP
    | TIMESTAMP
    | TYPE
    | USER
    | VALUE
    | VALUES
    // etc
    ;

non_reserved_words
    : COMMENT
    | TIMEZONE
    ;

column_name
    : (id_ '.')? id_
    ;

column_list
    : column_name (COMMA column_name)*
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

expr
    : expr '[' expr ']'
    | expr COLON expr
    | expr DOT (VALUE | expr)
    | COLLATE string
    | case_expression
    | bracket_expression
    | op = ( PLUS | MINUS) expr
    | expr op = (STAR | DIVIDE | MODULE) expr
    | expr op = (PLUS | MINUS | PIPE_PIPE) expr
    | l = expr comparison_operator r = expr
    | op = NOT+ expr
    | expr AND expr //bool operation
    | expr OR expr  //bool operation
    | cast_expr
    | expr COLON_COLON data_type // Cast operator
    | function_call
    | subquery
    | expr IS (NOT NULL_ expr)
    | expr NOT? IN LR_BRACKET (subquery | expr_list) RR_BRACKET
    | expr NOT? LIKE expr (ESCAPE expr)?
    | expr NOT? LIKE ANY LR_BRACKET expr (COMMA expr)* RR_BRACKET (ESCAPE expr)?
    | primitive_expression
    ;

cast_expr
    : CAST LR_BRACKET expr AS data_type RR_BRACKET
    ;

data_type_size
    : LR_BRACKET num RR_BRACKET
    ;

data_type
    : int_alias = (INT | SMALLINT | TINYINT | BIGINT)
    | number_alias = (NUMERIC | DECIMAL_ | DEC) (LR_BRACKET num (COMMA num)? RR_BRACKET)?
    | float_alias = (FLOAT_ | DOUBLE | REAL_)
    | BOOLEAN
    | DATE
    | INTERVAL TODO
    | TIMESTAMP data_type_size?
    | TIMESTAMP_NTZ data_type_size?
    | STRING_
    | BINARY data_type_size?
    | VARIANT
    | OBJECT
    | ARRAY
    | GEOGRAPHY
    | GEOMETRY
    | VOID
    | STRUCT TODO
    | MAP TODO
    ;

primitive_expression
    : DEFAULT //?
    | NULL_
    | id_ ('.' id_)*
    | id_ '.' STAR
    | full_column_name
    | literal
    ;

function_call
    : object_name '(' expr_list? ')'
    | object_name '(' param_assoc_list ')'
    ;

param_assoc_list
    : param_assoc (',' param_assoc)*
    ;

param_assoc
    : id_ ASSOC expr
    ;

literal
    : STRING // string, date, time, timestamp
    | sign? DECIMAL
    | sign? (REAL | FLOAT)
    | true_false
    | NULL_
    ;

sign
    : PLUS
    | MINUS
    ;

full_column_name
    : schema = id_? DOT tab_name = id_? DOT col_name = id_
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
    : WHEN expr THEN expr
    ;

switch_section
    : WHEN expr THEN expr
    ;

// select
query_statement
    : with_expression? SELECT TODO
    ;

with_expression
    : WITH RECURSIVE? common_table_expression (COMMA common_table_expression)*
    ;

common_table_expression
    : id_ (LR_BRACKET columns = column_list RR_BRACKET)? AS TODO
    ;

select_statement
    : SELECT TODO
    ;

values_statement
    : TODO
    ;

sql_pipeline
    : TODO
    ;

explain_statement
    : EXPLAIN (EXTENDED | CODEGEN | COST | FORMATTED)? statement
    ;

as_alias
    : AS? alias
    ;

alias
    : id_
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

subquery
    : query_statement
    ;
