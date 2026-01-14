/*
CockroachDB grammar.
The MIT License (MIT).

Copyright (c) 2026, Michał Lorek.

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

parser grammar CockroachDBParser;

options {
    tokenVocab = CockroachDBLexer;
}

cockroachdb_file
    : stmt_list? EOF
    ;

stmt_list
    : stmt (SEMI stmt)* SEMI?
    ;

stmt
    : stmt_without_legacy_transaction
    | legacy_transaction_stmt
    ;

stmt_without_legacy_transaction
    : preparable_stmt
    | analyze_stmt
    | call_stmt
    | copy_stmt
    | comment_stmt
    | execute_stmt
    | deallocate_stmt
    | discard_stmt
    | do_stmt
    | grant_stmt
    | prepare_stmt
    | revoke_stmt
    | savepoint_stmt
    | reassign_owned_by_stmt
    | drop_owned_by_stmt
    | release_stmt
    | refresh_stmt
    | nonpreparable_set_stmt
    | transaction_stmt
    | close_cursor_stmt
    | declare_cursor_stmt
    | fetch_cursor_stmt
    | move_cursor_stmt
    | unlisten_stmt
    | show_commit_timestamp_stmt
    ;

legacy_transaction_stmt
    : legacy_begin_stmt
    | legacy_end_stmt
    ;

preparable_stmt
    : alter_stmt
    | backup_stmt
    | cancel_stmt
    | create_stmt
    | check_stmt
    | delete_stmt
    | drop_stmt
    | explain_stmt
    | import_stmt
    | insert_stmt
    | inspect_stmt
    | pause_stmt
    | reset_stmt
    | restore_stmt
    | resume_stmt
    | export_stmt
    | scrub_stmt
    | select_stmt
    | preparable_set_stmt
    | show_stmt
    | truncate_stmt
    | update_stmt
    | upsert_stmt
    ;

analyze_stmt
    : (ANALYZE | ANALYSE) analyze_target
    ;

analyze_target
    : table_name
    ;

call_stmt
    : CALL func_application
    ;

func_application
    : func_application_name '('
        ( ALL? expr_list opt_sort_clause_no_index
        | DISTINCT expr_list
        | '*'
        ) ')'
    ;

opt_sort_clause_no_index
    : sort_clause_no_index?
    ;

sort_clause_no_index
    : ORDER BY sortby_no_index_list
    ;

sortby_no_index_list
    : sortby (',' (sortby | sortby_index))*
    ;

sortby
    : a_expr opt_asc_desc? opt_nulls_order?
    ;

opt_asc_desc
    : ASC
    | DESC
    ;

opt_nulls_order
    : NULLS (FIRST | LAST)
    ;

sortby_index
    : (PRIMARY KEY table_name | INDEX table_name '@' index_name) opt_asc_desc?
    ;

copy_stmt
    : COPY (
        table_name opt_column_list? (
            FROM STDIN opt_with_copy_options? opt_where_clause?
            | TO STDOUT opt_with_copy_options?
          )
        | '(' copy_to_stmt ')' TO STDOUT opt_with_copy_options?
    )
    ;

opt_column_list
    : '(' name_list ')'
    ;

opt_where_clause
    : where_clause
    ;

where_clause
    : WHERE a_expr
    ;

opt_with_copy_options
    : opt_with? (copy_options_list | '(' copy_generic_options_list ')')
    ;

opt_with
    : WITH
    ;

copy_options_list
    : copy_options+
    ;

copy_options
    : (DESTINATION '=' | DELIMITER | NULL_) string_or_placeholder
    | BINARY
    | CSV
    | HEADER
    | (QUOTE | ESCAPE | ENCODING) sconst
    ;

string_or_placeholder
    : non_reserved_word_or_sconst
    | PLACEHOLDER
    ;

copy_generic_options_list
    : copy_generic_options (',' copy_generic_options)*
    ;

copy_generic_options
    : (DESTINATION '=' | DELIMITER | NULL_) string_or_placeholder
    | FORMAT (BINARY | CSV | TEXT | sconst)
    | HEADER (TRUE | FALSE)?
    | (QUOTE | ESCAPE | ENCODING) sconst
    ;

copy_to_stmt
    : delete_stmt
    | insert_stmt
    | select_stmt
    | update_stmt
    | upsert_stmt
    ;

comment_stmt
    : COMMENT ON (
        DATABASE database_name
        | SCHEMA qualifiable_schema_name
        | TYPE type_name
        | TABLE table_name
        | CONSTRAINT constraint_name ON table_name
        | COLUMN column_path
        | INDEX table_index_name
    ) IS comment_text
    ;

column_path
    : name
    | prefixed_column_path
    ;

prefixed_column_path
    : db_object_name_component '.' unrestricted_name ('.' )
    ;

comment_text
    : sconst
    | NULL_
    ;

execute_stmt
    : EXECUTE table_alias_name execute_param_clause?
    ;

table_alias_name
    : name
    ;

execute_param_clause
    : '(' expr_list ')'
    ;

deallocate_stmt
    : DEALLOCATE PREPARE? (name | ALL)
    ;

discard_stmt
    : DISCARD (ALL | SEQUENCES | TEMP | TEMPORARY)
    ;

do_stmt
    : DO do_stmt_opt_list
    ;

do_stmt_opt_list
    : do_stmt_opt_item+
    ;

do_stmt_opt_item
    : sconst
    | LANGUAGE non_reserved_word_or_sconst
    ;

non_reserved_word_or_sconst
    : non_reserved_word
    | sconst
    ;

grant_stmt
    : GRANT (
     privileges ON (
        grant_targets
        | TYPE target_types
        | (ALL (SEQUENCES | TABLES | FUNCTIONS | PROCEDURES | ROUTINES) IN)? SCHEMA schema_name_list
     ) TO role_spec_list opt_with_grant_option?
     | SYSTEM privileges
     | privilege_list TO role_spec_list (WITH ADMIN OPITON)?
    )
    ;

privileges
    : ALL opt_privilege_clause?
    | privilege_list
    ;

opt_privilege_clause
    : PRIVILEGES
    ;

grant_targets
    : identifier_
    | col_name_keyword
    | unreserved_keyword
    | complex_table_pattern
    | (SEQUENCE | table_pattern ',' | TABLE) table_pattern_list
    | (DATABASE | EXTERNAL CONNECTION) name_list
    | (FUNCTION | PROCEDURE) function_with_paramtypes_list
    ;

table_pattern
    : simple_db_object_name
    | complex_table_pattern
    ;

table_pattern_list
    : table_pattern (',' table_pattern)*
    ;

name_list
    : name (',' name)*
    ;

function_with_paramtypes_list
    : function_with_paramtypes (',' function_with_paramtypes)*
    ;

function_with_paramtypes
    : db_object_name func_params?
    ;

func_params
    : '(' func_params_list? ')'
    ;

func_params_list
    : routine_param (',' routine_param)*
    ;

routine_param
    : ()? routine_param_type
    | param_name routine_param_class?
    ;

routine_param_class
    : IN OUT?
    | OUT
    | INOUT
    ;

routine_param_type
    : typename
    ;

typename
    : simple_typename (opt_array_bounds | ARRAY)
    ;

simple_typename
    : general_type_name
    | '@' iconst
    | complex_type_name
    | const_typename
    | interval_type
    ;

general_type_name
    : type_function_name_no_crdb_extra
    ;

type_function_name_no_crdb_extra
    : identifier_
    | unreserved_keyword
    | type_func_name_no_crdb_extra_keyword
    ;

type_func_name_no_crdb_extra_keyword
    : AUTHORIZATION
    | COLLATION
    | CROSS
    | FULL
    | INNER
    | ILIKE
    | IS
    | ISNULL
    | JOIN
    | LEFT
    | LIKE
    | NATURAL
    | NONE
    | NOTNULL
    | OUTER
    | OVERLAPS
    | RIGHT
    | SIMILAR
    ;

complex_type_name
    : general_type_name '.' unrestricted_name ('.' unrestricted_name)?
    ;

const_typename
    : numeric
    | bit_without_length
    | bit_with_length
    | character_without_length
    | character_with_length
    | const_datetime
    | const_geo
    | const_vector
    ;

numeric
    : INT
    | INTEGER
    | SMALLINT
    | BIGINT
    | REAL
    | FLOAT opt_float?
    | DOUBLE PRECISION
    | (DECIMAL | DEC | NUMERIC) opt_numeric_modifiers?
    | BOOLEAN
    ;

opt_float
    : iconst_in_parentheses
    ;

opt_numeric_modifiers
    : '(' iconst (',' iconst)? ')'
    ;

bit_without_length
    : BIT VARYING?
    | VARBIT
    ;

bit_with_length
    : (BIT opt_varying? | VARBIT) iconst_in_parentheses
    ;

opt_varying
    : VARYING
    ;

character_without_length
    : character_base
    ;

character_base
    : char_aliases VARYING?
    | VARCHAR
    | STRING
    ;

char_aliases
    : CHAR
    | CHARACTER
    ;

character_with_length
    : character_base iconst_in_parentheses
    ;

const_datetime
    : DATE
    | (TIME | TIMESTAMP) iconst_in_parentheses? opt_timezone?
    | (TIMETZ | TIMESTAMPTZ) iconst_in_parentheses?
    ;

opt_timezone
    : (WITH | WITHOUT) TIME ZONE
    ;

iconst_in_parentheses
    : '(' iconst ')'
    ;

const_geo
    : (GEOGRAPGHY | GEOMETRY) ('(' geo_shape_type (',' signed_iconst)? ')')?
    | BOX2D
    ;

geo_shape_type
    : POINT
    | POINTM
    | POINTZ
    | POINTZM
    | LINESTRING
    | LINESTRINGM
    | LINESTRINGZ
    | LINESTRINGZM
    | POLYGON
    | POLYGONM
    | POLYGONZ
    | POLYGONZM
    | MULTIPOINT
    | MULTIPOINTM
    | MULTIPOINTZ
    | MULTIPOINTZM
    | MULTILINESTRING
    | MULTILINESTRINGM
    | MULTILINESTRINGZ
    | MULTILINESTRINGZM
    | MULTIPOLYGON
    | MULTIPOLYGONM
    | MULTIPOLYGONZ
    | MULTIPOLYGONZM
    | GEOMETRYCOLLECTION
    | GEOMETRYCOLLECTIONM
    | GEOMETRYCOLLECTIONZ
    | GEOMETRYCOLLECTIONZM
    | GEOMETRY
    | GEOMETRYM
    | GEOMETRYZ
    | GEOMETRYZM
    ;

signed_iconst
    : iconst
    | only_signed_iconst
    ;

only_signed_iconst
    : ('+' | '-') iconst
    ;

only_signed_fconst
    : ('+' | '-') fconst
    ;

signed_iconst64
    : signed_iconst
    ;

const_vector
    : VECTOR iconst_in_parentheses?
    ;

interval_type
    : INTERVAL (interval_qualifier | iconst_in_parentheses)?
    ;

interval_qualifier
    : YEAR (TO MONTH)?
    | MONTH
    | DAY (TO (HOUR | MINUTE | interval_second))?
    | HOUR (TO (MINUTE | interval_second))?
    | MINUTE (TO interval_second)?
    | interval_second
    ;

interval_second
    : SECOND iconst_in_parentheses?
    ;

opt_array_bounds
    : '[' ']'
    ;

param_name
    : type_function_name
    ;

type_function_name
    : identifier_
    | unreserved_keyword
    | type_func_name_keyword
    ;

complex_table_pattern
    : complex_db_object_name
    | (db_object_name_component '.' (unrestricted_name '.')? )? '*'
    ;

target_types
    : type_name_list
    ;

type_name_list
    : type_name (',' type_name)*
    ;

schema_name_list
    : qualifiable_schema_name (',' qualifiable_schema_name)*
    ;

role_spec_list
    : role_spec (',' role_spec)*
    ;

role_spec
    : identifier_
    | unreserved_keyword
    | CURRENT_USER
    | SESSION_USER
    ;

opt_with_grant_option
    : WITH GRANT OPTION
    ;

privilege_list
    : privilege (',' privilege)*
    ;

privilege
    : name
    | CREATE
    | GRANT
    | SELECT
    ;

prepare_stmt
    : PREPARE table_alias_name prep_type_clause? AS preparable_stmt
    ;

prep_type_clause
    : '(' type_list ')'
    ;

type_list
    : typename (',' typename)*
    ;

revoke_stmt
    : REVOKE (
        privileges ON (
            grant_targets
            | TYPE target_types
            | (ALL (TABLES | SEQUENCES | FUNCTIONS | PROCEDURES | ROUTINES) IN)? SCHEMA schema_name_list
            )
        | (ADMIN OPTION FOR)? privilege_list
        | SYSTEM privileges
        | GRANT OPTION FOR (
            privileges ON (
                grant_targets
                | TYPE target_types
                | (ALL (TABLES | FUNCTIONS | PROCEDURES | ROUTINES)? IN)? SCHEMA schema_name_list
                )
            | SYSTEM privileges
            )
    )
    FROM role_spec_list
    ;

savepoint_stmt
    : SAVEPOINT name
    ;

reassign_owned_by_stmt
    : REASSIGN OWNED BY role_spec_list TO role_spec
    ;

drop_owned_by_stmt
    : DROP OWNED BY role_spec_list opt_drop_behavior
    ;

opt_drop_behavior
    : CASCADE
    | RESTRICT
    ;

release_stmt
    : RELEASE savepoint_name
    ;

refresh_stmt
    : REFRESH MATERIALIZED VIEW opt_concurrently? view_name opt_as_of_clause? opt_clear_data?
    ;

opt_concurrently
    : CONCURRENTLY
    ;

opt_as_of_clause
    : as_of_clause
    ;

as_of_clause
    : AS OF SYSTEM TIME a_expr
    ;

opt_clear_data
    : WITH NO? DATA
    ;

nonpreparable_set_stmt
    : set_transaction_stmt
    ;

set_transaction_stmt
    : SET SESSION? TRANSACTION transaction_mode_list
    ;

transaction_mode_list
    : transaction_mode (','? transaction_mode)*
    ;

transaction_mode
    : transaction_ise_level
    | transaction_user_priority
    | transaction_read_mode
    | as_of_clause
    | transaction_deferrable_mode
    ;

transaction_ise_level
    : ISOLATION LEVEL iso_level
    ;

iso_level
    : READ (UNCOMMITTED | COMMITTED)
    | SNAPSHOT
    | REPEATABLE READ
    | SERIALIZABLE
    ;

transaction_user_priority
    : PRIORITY user_priority
    ;

user_priority
    : LOW
    | NORMAL
    | HIGH
    ;

transaction_read_mode
    : READ (ONLY | WRITE)
    ;

transaction_deferrable_mode
    : NOT? DEFERRABLE
    ;

transaction_stmt
    : begin_stmt
    | commit_stmt
    | rollback_stmt
    | abort_stmt
    | prepare_transaction_stmt
    | commit_prepared_stmt
    | rollback_prepared_stmt
    ;

begin_stmt
    : COMMIT TRANSACTION begin_transaction?
    ;

begin_transaction
    : transaction_mode_list
    ;

commit_stmt
    : COMMIT opt_transaction?
    ;

opt_transaction
    : TRANSACTION
    ;

rollback_stmt
    : ROLLBACK opt_transaction? (TO savepoint_name)?
    ;

abort_stmt
    : ABORT opt_abort_mod?
    ;

opt_abort_mod
    : TRANSACTION
    | WORK
    ;

prepare_transaction_stmt
    : PREPARE TRANSACTION sconst
    ;

commit_prepared_stmt
    : COMMIT PREPARED sconst
    ;

rollback_prepared_stmt
    : ROLLBACK PREPARRED sconst
    ;

close_cursor_stmt
    : CLOSE (ALL | cursor_name)
    ;

declare_cursor_stmt
    : DECLARE cursor_name opt_binary? opt_sensitivity?
        opt_scroll? CURSOR opt_hold?
        for_with_lookahead_variants
        select_stmt
    ;

opt_binary
    : BINARY
    ;

opt_sensitivity
    : INSENSITIVE
    | ASENSITIVE
    ;

opt_scroll
    : NO? SCROLL
    ;

opt_hold
    : (WITH | WITHOUT) HOLD
    ;

for_with_lookahead_variants
    : FOR
    | FOR_TABLE
    ;

fetch_cursor_stmt
    : FETCH cursor_movement_specifier
    ;

cursor_movement_specifier
    : (from_or_in
    |   (next_prior
        | forward_backward
        | (ABSOLUTE | RELATIVE) signed_iconst64
        | FIRST
        | LAST
        | opt_forward_backward (signed_iconst64 | ALL)
        ) opt_from_or_in
    ) cursor_name
    ;

from_or_in
    : FROM
    | IN
    ;

next_prior
    : NEXT
    | PRIOR
    ;

opt_from_or_in
    : from_or_in?
    ;

forward_backward
    : FORWARD
    | BACKWARD
    ;

opt_forward_backward
    : forward_backward?
    ;

move_cursor_stmt
    : MOVE cursor_movement_specifier
    ;

unlisten_stmt
    : UNLISTEN (type_name | '*')
    ;

show_commit_timestamp_stmt
    : SHOW COMMIT TIMESTAMP
    ;

legacy_begin_stmt
    : BEGIN opt_transaction? begin_transaction
    ;

legacy_end_stmt
    : END opt_transaction?
    ;

alter_stmt
    : alter_ddl_stmt
    | alter_external_connection_stmt
    | alter_role_stmt
    | alter_virtual_cluster_stmt
    ;

backup_stmt
    : BACKUP opt_backup_targets INTO ( (sconst_or_placeholder | LATEST) IN)?
        string_or_placeholder_opt_list
        opt_as_of_clause
        opt_with_backup_options
    ;

sconst_or_placeholder
    : sconst
    | PLACEHOLDER
    ;

opt_backup_targets
    : backup_targets
    ;

backup_targets
    : identifier_
    | col_name_keyword
    | unreserved_keyword
    | complex_table_pattern
    | (table_pattern ',' | TABLE) table_pattern_list
    | DATABASE name_list
    ;

string_or_placeholder_opt_list
    : string_or_placeholder
    | '(' string_or_placeholder_list ')'
    ;

string_or_placeholder_list
    : string_or_placeholder (',' string_or_placeholder)*
    ;

opt_with_backup_options
    : WITH (backup_options_list | OPTIONS '(' backup_options_list ')')
    ;

backup_options
    : (ENCRYPTION_PASSPHRASE | EXECUTION LOCALITY ) '=' string_or_placeholder
    | (REVISION_HISTORY | UPDATES_CLUSTER_MONITORING_METRICS) ('=' a_expr)?
    | DETACHED ('=' (TRUE | FALSE))?
    | (KMS | INCREMENTAL_LOCATION) '=' string_or_placeholder_opt_list
    | INCLUDE_ALL_VIRTUAL_CLUSTERS '=' a_expr
    ;

backup_options_list
    : backup_options (',' backup_options)*
    ;

cancel_stmt
    : cancel_jobs_stmt
    | cancel_queries_stmt
    | cancel_sessions_stmt
    | cancel_all_jobs_stmt
    ;

create_stmt
    : create_role_stmt
    | create_ddl_stmt
    | create_stats_stmt
    | create_changefeed_stmt
    | create_extension_stmt
    | create_external_connection_stmt
    | create_logical_replication_stream_stmt
    | create_schedule_stmt
    ;

check_stmt
    : check_external_connection_stmt
    ;

delete_stmt
    : opt_with_clause? DELETE opt_batch_clause? FROM table_expr_opt_alias_idx
        opt_using_clause? opt_where_clause? sort_clause? limit_clause?
            returning_clause
    ;

opt_with_clause
    : with_clause?
    ;

with_clause
    : WITH RECURSIVE? cte_list
    ;

opt_batch_clause
    : BATCH  ('(' batch_param_list ')')?
    ;

batch_param_list
    : batch_param (',' batch_param)*
    ;

batch_param
    : SIZE a_expr
    ;

opt_using_clause
    : USING from_list
    ;

cte_list
    : common_table_expr (',' common_table_expr)*
    ;

common_table_expr
    : table_alias_name opt_col_def_list_no_types? AS
        materialized_clause? '(' preparable_stmt ')'
    ;

opt_col_def_list_no_types
    : '(' col_def_list_no_types ')'
    ;

col_def_list_no_types
    : name (',' name)*
    ;

materialized_clause
    : NOT? MATERIALIZED
    ;

drop_stmt
    : drop_ddl_stmt
    | drop_role_stmt
    | drop_schedule_stmt
    | drop_external_connection_stmt
    ;

explain_stmt
    : EXPLAIN (ANALYZE | ANALYSE)? ('(' explain_option_list ')')? explainable_stmt
    ;

explain_option_list
    : explain_option_name (',' explain_option_name)*
    ;

explain_option_name
    : non_reserved_word
    ;

explainable_stmt
    : preparable_stmt
    | comment_stmt
    | execute_stmt
    | call_stmt
    | do_stmt
    ;

import_stmt
    : IMPORT INTO table_name ('(' insert_column_list ')')? import_format
        DATA '(' string_or_placeholder_list ')' opt_with_options?
    ;

insert_column_list
    : insert_column_item (',' insert_column_item)*
    ;

insert_column_item
    : column_name
    ;

import_format
    : name
    ;

opt_with_options
    : WITH (kv_option_list | OPTIONS '(' kv_option_list ')')
    ;

kv_option_list
    : kv_option (',' kv_option)*
    ;

kv_option
    : (name | sconst) ('=' string_or_placeholder)?
    ;

insert_stmt
    : opt_with_clause INSERT INTO insert_target insert_rest on_conflict? returning_clause?
    ;

insert_target
    : table_name_opt_idx (AS table_alias_name)?
    ;

table_name_opt_idx
    : opt_only table_name opt_index_flags? opt_descendant
    ;

opt_only
    : ONLY?
    ;

opt_index_flags
    : '@' (index_name | '[' iconst ']' | '{' index_flags_param_list '}')
    ;

index_flags_param_list
    : index_flags_param (',' index_flags_param)*
    ;

index_flags_param
    : FORCE_INDEX '=' index_name
    | NO_INDEX_JOIN
    | NO_ZIGZAG_JOIN
    | NO_FULL_SCAN
    | AVOID_FULL_SCAN
    | FORCE_ZIGZAG ('=' index_name)?
    ;

opt_descendant
    : '*'?
    ;

insert_rest
    : ('(' insert_column_list ')')? select_stmt
    | DEFAULT VALUES
    ;

on_conflict
    : ON CONFLICT
        ( DO NOTHING
        | ('(' name_list ')' opt_where_clause? | ON CONSTRAINT constraint_name) DO (NOTHING | UPDATE SET set_clause_list opt_where_clause?)
        )
    ;

set_clause_list
    : set_clause (',' set_clause)*
    ;

set_clause
    : single_set_clause
    | multiple_set_clause
    ;

single_set_clause
    : column_name '=' a_expr
    ;

multiple_set_clause
    : '(' insert_column_list ')' '=' in_expr
    ;

returning_clause
    : RETURNING (target_list | NOTHING)
    ;

target_list
    : target_elem (',' target_elem)*
    ;

target_elem
    : a_expr (AS target_name | bare_col_label)?
    | '*'
    ;

target_name
    : unrestricted_name
    ;

bare_col_label
    : identifier_
    | bare_label_keywords
    ;

bare_label_keywords
    : ABORT
    | ABSOLUTE
    | ACCESS
    | ACTION
    | ADD
    | ADMIN
    | AFTER
    | AGGREGATE
    | ALL
    | ALTER
    | ALWAYS
    | ANALYSE
    | ANALYZE
    | AND
    | ANNOTATE_TYPE
    | ANY
    | ASC
    | ASENSITIVE
    | ASYMMETRIC
    | AS_JSON
    | AT
    | ATOMIC
    | ATTRIBUTE
    | AUTHORIZATION
    | AUTOMATIC
    | AVAILABILITY
    | AVOID_FULL_SCAN
    | BACKUP
    | BACKUPS
    | BACKWARD
    | BATCH
    | BEFORE
    | BEGIN
    | BETWEEN
    | BIDIRECTIONAL
    | BIGINT
    | BINARY
    | BIT
    | BOOLEAN
    | BOTH
    | BOX2D
    | BUCKET_COUNT
    | BY
    | BYPASSRLS
    | CACHE
    | CALL
    | CALLED
    | CANCEL
    | CANCELQUERY
    | CAPABILITIES
    | CAPABILITY
    | CASCADE
    | CASE
    | CAST
    | CHANGEFEED
    | CHARACTERISTICS
    | CHECK
    | CHECK_FILES
    | CLOSE
    | CLUSTER
    | CLUSTERS
    | COALESCE
    | COLLATION
    | COLUMN
    | COLUMNS
    | COMMENT
    | COMMENTS
    | COMMIT
    | COMMITTED
    | COMPACT
    | COMPLETE
    | COMPLETIONS
    | CONCURRENTLY
    | CONFIGURATION
    | CONFIGURATIONS
    | CONFIGURE
    | CONFLICT
    | CONNECTION
    | CONNECTIONS
    | CONSTRAINT
    | CONSTRAINTS
    | CONTROLCHANGEFEED
    | CONTROLJOB
    | CONVERSION
    | CONVERT
    | COPY
    | COST
    | COVERING
    | CREATEDB
    | CREATELOGIN
    | CREATEROLE
    | CROSS
    | CSV
    | CUBE
    | CURRENT
    | CURRENT_CATALOG
    | CURRENT_DATE
    | CURRENT_ROLE
    | CURRENT_SCHEMA
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | CURRENT_USER
    | CURSOR
    | CYCLE
    | DATA
    | DATABASE
    | DATABASES
    | DEALLOCATE
    | DEBUG_IDS
    | DEC
    | DECIMAL
    | DECLARE
    | DEFAULT
    | DEFAULTS
    | DEFERRABLE
    | DEFERRED
    | DEFINER
    | DELETE
    | DELIMITER
    | DEPENDS
    | DESC
    | DESTINATION
    | DETACHED
    | DETAILS
    | DISABLE
    | DISCARD
    | DISTINCT
    | DO
    | DOMAIN
    | DOUBLE
    | DROP
    | EACH
    | ELSE
    | ENABLE
    | ENCODING
    | ENCRYPTED
    | ENCRYPTION_INFO_DIR
    | ENCRYPTION_PASSPHRASE
    | END
    | ENUM
    | ENUMS
    | ERRORS
    | ESCAPE
    | EXCLUDE
    | EXCLUDING
    | EXECUTE
    | EXECUTION
    | EXISTS
    | EXPERIMENTAL
    | EXPERIMENTAL_AUDIT
    | EXPERIMENTAL_FINGERPRINTS
    | EXPERIMENTAL_RELOCATE
    | EXPERIMENTAL_REPLICA
    | EXPIRATION
    | EXPLAIN
    | EXPORT
    | EXTENSION
    | EXTERNAL
    | EXTRACT
    | EXTRACT_DURATION
    | EXTREMES
    | FAILURE
    | FALSE
    | FAMILY
    | FILES
    | FIRST
    | FLOAT
    | FOLLOWING
    | FORCE
    | FORCE_NOT_NULL
    | FORCE_NULL
    | FORCE_QUOTE
    | FORCE_INDEX
    | FORCE_INVERTED_INDEX
    | FORCE_ZIGZAG
    | FOREIGN
    | FORMAT
    | FORWARD
    | FREEZE
    | FULL
    | FUNCTION
    | FUNCTIONS
    | GENERATED
    | GEOGRAPHY
    | GEOMETRY
    | GEOMETRYCOLLECTION
    | GEOMETRYCOLLECTIONM
    | GEOMETRYCOLLECTIONZ
    | GEOMETRYCOLLECTIONZM
    | GEOMETRYM
    | GEOMETRYZ
    | GEOMETRYZM
    | GLOBAL
    | GOAL
    | GRANTEE
    | GRANTS
    | GREATEST
    | GROUPING
    | GROUPS
    | HASH
    | HEADER
    | HIGH
    | HISTOGRAM
    | HOLD
    | IDENTITY
    | IF
    | IFERROR
    | IFNULL
    | IGNORE_FOREIGN_KEYS
    | ILIKE
    | IMMEDIATE
    | IMMEDIATELY
    | IMMUTABLE
    | IMPORT
    | IN
    | INCLUDE
    | INCLUDE_ALL_SECONDARY_TENANTS
    | INCLUDE_ALL_VIRTUAL_CLUSTERS
    | INCLUDING
    | INCREMENT
    | INCREMENTAL
    | INCREMENTAL_LOCATION
    | INDEX
    | INDEXES
    | INDEX
    | INDEX
    | INDEX
    | INHERITS
    | INITIALLY
    | INJECT
    | INNER
    | INOUT
    | INPUT
    | INSENSITIVE
    | INSERT
    | INSPECT
    | INSTEAD
    | INT
    | INTEGER
    | INTERVAL
    | INTO_DB
    | INVERTED
    | INVISIBLE
    | INVOKER
    | IS
    | ISERROR
    | ISOLATION
    | JOB
    | JOBS
    | JOIN
    | JSON
    | KEY
    | KEYS
    | KMS
    | KV
    | LABEL
    | LANGUAGE
    | LAST
    | LATERAL
    | LATEST
    | LC_COLLATE
    | LC_CTYPE
    | LEADING
    | LEAKPROOF
    | LEASE
    | LEAST
    | LEFT
    | LESS
    | LEVEL
    | LIKE
    | LINESTRING
    | LINESTRINGM
    | LINESTRINGZ
    | LINESTRINGZM
    | LIST
    | LOCAL
    | LOCALITY
    | LOCALTIME
    | LOCALTIMESTAMP
    | LOCKED
    | LOGGED
    | LOGICAL
    | LOGICALLY
    | LOGIN
    | LOOKUP
    | LOW
    | MATCH
    | MATERIALIZED
    | MAXVALUE
    | MERGE
    | METHOD
    | MINVALUE
    | MODE
    | MODIFYCLUSTERSETTING
    | MOVE
    | MULTILINESTRING
    | MULTILINESTRINGM
    | MULTILINESTRINGZ
    | MULTILINESTRINGZM
    | MULTIPOINT
    | MULTIPOINTM
    | MULTIPOINTZ
    | MULTIPOINTZM
    | MULTIPOLYGON
    | MULTIPOLYGONM
    | MULTIPOLYGONZ
    | MULTIPOLYGONZM
    | NAMES
    | NAN
    | NATURAL
    | NEVER
    | NEW
    | NEW_DB_NAME
    | NEW_KMS
    | NEXT
    | NO
    | NOBYPASSRLS
    | NOCANCELQUERY
    | NOCONTROLCHANGEFEED
    | NOCONTROLJOB
    | NOCREATEDB
    | NOCREATELOGIN
    | NOCREATEROLE
    | NODE
    | NOLOGIN
    | NOMODIFYCLUSTERSETTING
    | NONE
    | NONVOTERS
    | NORMAL
    | NOREPLICATION
    | NOSQLLOGIN
    | NOT
    | NOTHING
    | NOTHING
    | NOVIEWACTIVITY
    | NOVIEWACTIVITYREDACTED
    | NOVIEWCLUSTERSETTING
    | NOWAIT
    | NO_FULL_SCAN
    | NO_INDEX_JOIN
    | NO_ZIGZAG_JOIN
    | NULL
    | NULLIF
    | NULLS
    | NUMERIC
    | OF
    | OFF
    | OIDS
    | OLD
    | OLD_KMS
    | ONLY
    | OPERATOR
    | OPT
    | OPTION
    | OPTIONS
    | OR
    | ORDINALITY
    | OTHERS
    | OUT
    | OUTER
    | OVERLAY
    | OWNED
    | OWNER
    | PARALLEL
    | PARENT
    | PARTIAL
    | PARTITION
    | PARTITIONS
    | PASSWORD
    | PAUSE
    | PAUSED
    | PER
    | PERMISSIVE
    | PHYSICAL
    | PLACEMENT
    | PLACING
    | PLAN
    | PLANS
    | POINT
    | POINTM
    | POINTZ
    | POINTZM
    | POLICIES
    | POLICY
    | POLYGON
    | POLYGONM
    | POLYGONZ
    | POLYGONZM
    | POSITION
    | PRECEDING
    | PREPARE
    | PREPARED
    | PRESERVE
    | PRIMARY
    | PRIOR
    | PRIORITY
    | PRIVILEGES
    | PROCEDURE
    | PROCEDURES
    | PROVISIONSRC
    | PUBLIC
    | PUBLICATION
    | QUERIES
    | QUERY
    | QUOTE
    | RANGE
    | RANGES
    | READ
    | REAL
    | REASON
    | REASSIGN
    | RECURRING
    | RECURSIVE
    | REDACT
    | REF
    | REFERENCES
    | REFERENCING
    | REFRESH
    | REGION
    | REGIONAL
    | REGIONS
    | REINDEX
    | RELATIVE
    | RELEASE
    | RELOCATE
    | REMOVE_REGIONS
    | RENAME
    | REPEATABLE
    | REPLACE
    | REPLICATED
    | REPLICATION
    | RESET
    | RESTART
    | RESTORE
    | RESTRICT
    | RESTRICTED
    | RESTRICTIVE
    | RESUME
    | RETENTION
    | RETURN
    | RETURNS
    | REVISION_HISTORY
    | REVOKE
    | RIGHT
    | ROLE
    | ROLES
    | ROLLBACK
    | ROLLUP
    | ROUTINES
    | ROW
    | ROWS
    | RULE
    | RUN
    | RUNNING
    | SAVEPOINT
    | SCANS
    | SCATTER
    | SCHEDULE
    | SCHEDULES
    | SCHEMA
    | SCHEMAS
    | SCHEMA_ONLY
    | SCROLL
    | SCRUB
    | SEARCH
    | SECONDARY
    | SECURITY
    | SECURITY_INVOKER
    | SELECT
    | SEQUENCE
    | SEQUENCES
    | SERIALIZABLE
    | SERVER
    | SERVICE
    | SESSION
    | SESSIONS
    | SESSION_USER
    | SET
    | SETOF
    | SETS
    | SETTING
    | SETTINGS
    | SHARE
    | SHARED
    | SHOW
    | SIMILAR
    | SIMPLE
    | SIZE
    | SKIP
    | SKIP_LOCALITIES_CHECK
    | SKIP_MISSING_FOREIGN_KEYS
    | SKIP_MISSING_SEQUENCES
    | SKIP_MISSING_SEQUENCE_OWNERS
    | SKIP_MISSING_UDFS
    | SKIP_MISSING_VIEWS
    | SMALLINT
    | SNAPSHOT
    | SOME
    | SOURCE
    | SPLIT
    | SQL
    | SQLLOGIN
    | STABLE
    | START
    | STATE
    | STATEMENT
    | STATEMENTS
    | STATISTICS
    | STATUS
    | STDIN
    | STDOUT
    | STOP
    | STORAGE
    | STORE
    | STORED
    | STORING
    | STRAIGHT
    | STREAM
    | STRICT
    | STRING
    | SUBSCRIPTION
    | SUBSTRING
    | SUBJECT
    | SUPER
    | SUPPORT
    | SURVIVAL
    | SURVIVE
    | SYMMETRIC
    | SYNTAX
    | SYSTEM
    | TABLE
    | TABLES
    | TABLESPACE
    | TEMP
    | TEMPLATE
    | TEMPORARY
    | TENANT
    | TENANTS
    | TENANT_NAME
    | TESTING_RELOCATE
    | TEXT
    | THEN
    | THROTTLING
    | TIES
    | TIME
    | TIMESTAMP
    | TIMESTAMPTZ
    | TIMETZ
    | TRACE
    | TRACING
    | TRAILING
    | TRANSACTION
    | TRANSACTIONS
    | TRANSFER
    | TRANSFORM
    | TREAT
    | TRIGGER
    | TRIGGERS
    | TRIM
    | TRUE
    | TRUNCATE
    | TRUSTED
    | TYPE
    | TYPES
    | UNBOUNDED
    | UNCOMMITTED
    | UNIDIRECTIONAL
    | UNIQUE
    | UNKNOWN
    | UNLISTEN
    | UNLOGGED
    | UNSAFE_RESTORE_INCOMPATIBLE_VERSION
    | UNSET
    | UNSPLIT
    | UNTIL
    | UPDATE
    | UPDATES_CLUSTER_MONITORING_METRICS
    | UPSERT
    | USE
    | USER
    | USERS
    | USING
    | VALID
    | VALIDATE
    | VALUE
    | VALUES
    | VARBIT
    | VARCHAR
    | VARIABLES
    | VARIADIC
    | VECTOR
    | VERIFY_BACKUP_TABLE_DATA
    | VIEW
    | VIEWACTIVITY
    | VIEWACTIVITYREDACTED
    | VIEWCLUSTERSETTING
    | VIRTUAL
    | VIRTUAL_CLUSTER_NAME
    | VIRTUAL_CLUSTER
    | VISIBLE
    | VISIBILITY
    | VOLATILE
    | VOTERS
    | WHEN
    | WORK
    | WRITE
    | ZONE
    ;

inspect_stmt
    : inspect_table_stmt
    | inspect_database_stmt
    ;

inspect_table_stmt
    : INSPECT TABLE table_name opt_as_of_clause? opt_inspect_options_clause?
    ;

opt_inspect_options_clause
    : WITH OPTIONS inspect_option_list
    ;

inspect_option_list
    : inspect_option (',' inspect_option)*
    ;

inspect_option
    : INDEX (ALL | '(' table_index_name_list ')')
    ;

inspect_database_stmt
    : INSPECT DATABASE db_name opt_as_of_clause? opt_inspect_options_clause?
    ;

pause_stmt
    : pause_jobs_stmt
    | pause_schedules_stmt
    | pause_all_jobs_stmt
    ;

pause_jobs_stmt
    : PAUSE (
        JOB a_expr with_reason?
        | JOBS (select_stmt with_reason? | for_schedules_clause)
    )
    ;

with_reason
    : WITH REASON '=' string_or_placeholder
    ;

pause_schedules_stmt
    : PAUSE (SCHEDULE a_expr | SCHEDULES select_stmt)
    ;

pause_all_jobs_stmt
    : PAUSE ALL name JOBS
    ;

reset_stmt
    : reset_session_stmt
    | reset_csetting_stmt
    ;

reset_session_stmt
    : RESET SESSION? session_var
    | RESET_ALL ALL
    ;

session_var
    : identifier_ session_var_parts?
    | ALL
    | DATABASE
    | NAMES
    | ROLE
    | SESSION_USER
    | LC_COLLATE
    | TIME ZONE
    | VIRTUAL_CLUSTER_NAME
    ;

session_var_parts
    : ('.' identifier_)+
    ;

reset_csetting_stmt
    : RESET CLUSTER SETTING var_name
    ;

var_name
    : name attrs?
    ;

attrs
    : ('.' unrestricted_name)+
    ;

restore_stmt
    : RESTORE (backup_targets | SYSTEM USERS)? FROM string_or_placeholder IN
        string_or_placeholder_opt_list opt_as_of_clause? opt_with_restore_options?
    ;

opt_with_restore_options
    : WITH (restore_options_list | OPTIONS '(' restore_options_list ')')
    ;

restore_options_list
    : restore_options (',' restore_options)*
    ;

restore_options
    : (ENCRYPTION_PASSPHRASE | INTO_DB | NEW_DB_NAME | VIRTUAL_CLUSTER_NAME | VIRTUAL_CLUSTER | EXECUTION LOCALITY) '=' string_or_placeholder
    | (KMS | INCREMENTAL_LOCATION) '=' string_or_placeholder_opt_list
    | SKIP_MISSING_FOREIGN_KEYS
    | SKIP_MISSING_SEQUENCES
    | SKIP_MISSING_SEQUENCE_OWNERS
    | SKIP_MISSING_VIEWS
    | SKIP_MISSING_UDFS
    | DETACHED
    | SKIP_LOCALITIES_CHECK
    | SCHEMA_ONLY
    | VERIFY_BACKUP_TABLE_DATA
    | UNSAFE_RESTORE_INCOMPATIBLE_VERSION
    | EXPERIMENTAL DEFERRED? COPY
    | REMOVE_REGIONS
    ;

resume_stmt
    : resume_jobs_stmt
    | resume_schedules_stmt
    | resume_all_jobs_stmt
    ;

resume_jobs_stmt
    : RESUME (JOB a_expr | JOBS (select_stmt | for_schedules_clause))
    ;

for_schedules_clause
    : FOR (SCHEDULES select_stmt | SCHEDULE a_expr)
    ;

resume_schedules_stmt
    : RESUME (SCHEDULES select_stmt | SCHEDULE a_expr)
    ;

resume_all_jobs_stmt
    : RESUME ALL name JOBS
    ;

export_stmt
    : EXPORT INTO import_format string_or_placeholder opt_with_options? FROM select_stmt
    ;

scrub_stmt
    : scrub_table_stmt
    |  scrub_database_stmt
    ;

drop_ddl_stmt
    : drop_database_stmt
    | drop_index_stmt
    | drop_table_stmt
    | drop_view_stmt
    | drop_sequence_stmt
    | drop_schema_stmt
    | drop_type_stmt
    | drop_func_stmt
    | drop_proc_stmt
    | drop_trigger_stmt
    | drop_policy_stmt
    ;

drop_role_stmt
    : DROP role_or_group_or_user if_exists? role_spec_list
    ;

if_exists
    : IF EXISTS
    ;

role_or_group_or_user
    : ROLE
    | USER
    ;

drop_schedule_stmt
    : DROP (SCHEDULE a_expr | SCHEDULES select_stmt)
    ;

drop_external_connection_stmt
    : DROP EXTERNAL CONNECTION string_or_placeholder
    ;

drop_database_stmt
    : DROP DATABASE if_exists? database_name opt_drop_behavior?
    ;

drop_index_stmt
    : DROP INDEX opt_concurrently? if_exists? table_index_name_list opt_drop_behavior?
    ;

table_index_name_list
    : table_index_name (',' table_index_name)*
    ;

drop_table_stmt
    : DROP TABLE if_exists? table_name_list opt_drop_behavior?
    ;

table_name_list
    : db_object_name_list
    ;

db_object_name_list
    : db_object_name (',' db_object_name)*
    ;

drop_view_stmt
    : DROP MATERIALIZED? VIEW if_exists? view_name_list opt_drop_behavior?
    ;

view_name_list
    : db_object_name_list
    ;

drop_sequence_stmt
    : DROP SEQUENCE if_exists? sequence_name_list opt_drop_behavior?
    ;

sequence_name_list
    : db_object_name_list
    ;

drop_schema_stmt
    : DROP SCHEMA if_exists? schema_name_list opt_drop_behavior?
    ;

drop_type_stmt
    : DROP TYPE if_exists? type_name_list opt_drop_behavior?
    ;

drop_func_stmt
    : DROP FUNCTION if_exists? function_with_paramtypes_list opt_drop_behavior?
    ;

drop_proc_stmt
    : DROP PROCEDURE if_exists? function_with_paramtypes_list? opt_drop_behavior?
    ;

drop_trigger_stmt
    : DROP TRIGGER if_exists? name ON table_name opt_drop_behavior?
    ;

drop_policy_stmt
    : DROP POLICY if_exists? name ON table_name opt_drop_behavior?
    ;
















select_stmt
    : select_no_parens
    | select_with_parens
    ;

select_no_parens
    : simple_select
    | select_clause (
        sort_clause
        | sort_clause? (for_locking_clause select_limit? | select_limit for_locking_clause?)
      )
    | with_clause select_clause (
            sort_clause
            | sort_clause? (for_locking_clause select_limit? | select_limit for_locking_clause?)
        )?
    ;

simple_select
    : simple_select_clause
    | values_clause
    | table_clause
    | set_operation
    ;

simple_select_clause
    : SELECT (opt_all_clause? opt_target_list? | (DISTINCT | distinct_on_clause) target_list)
        from_clause? opt_where_clause?
        group_clause? having_clause? window_clause?
    ;

opt_all_clause
    : ALL
    ;

opt_target_list
    : target_list
    ;

distinct_on_clause
    : DISTINCT ON expr_list_in_parens
    ;

from_clause
    : FROM from_list opt_as_of_clause?
    ;

from_list
    : table_ref (',' table_ref)*
    ;

group_clause
    : GROUP BY group_by_list
    ;

group_by_list
    : group_by_item (',' group_by_item)*
    ;

group_by_item
    : a_expr
    ;

having_clause
    : HAVING a_expr
    ;

window_clause
    : WINDOW window_definition_list
    ;

window_definition_list
    : window_definition (',' window_definition)*
    ;

window_definition
    : window_name AS window_specification
    ;

window_name
    : name
    ;

window_specification
    : '(' opt_existing_window_name? opt_partition_clause? opt_sort_clause_no_index opt_frame_clause? ')'
    ;

opt_existing_window_name
    : name
    ;

opt_partition_clause
    : PARTITION BY expr_list
    ;

opt_frame_clause
    : (RANGE | ROWS | GROUPS) frame_extent opt_frame_exclusion?
    ;

frame_extent
    : (BETWEEN frame_bound AND) frame_bound
    ;

frame_bound
    : (UNBOUNDED | a_expr) (PRECEDING | FOLLOWING)
    | CURRENT ROW
    ;

opt_frame_exclusion
    : EXCLUDE (CURRENT ROW | GROUP | TIES | NO OTHERS)
    ;

values_clause
    : VALUES expr_list_in_parens (',' expr_list_in_parens)*
    ;

table_clause
    : TABLE table_ref
    ;

table_ref
    : relation_expr opt_index_flags? todo
    | todo
    ;

set_operation
    : select_clause (UNION | INTERSECT | EXCEPT) all_or_distinct? select_clause
    ;

all_or_distinct
    : ALL
    | DISTINCT
    ;

select_clause
    : simple_select
    | select_with_parens
    ;

select_limit
    : limit_clause offset_clause?
    | offset_clause limit_clause?
    ;

limit_clause
    : LIMIT (ALL | a_expr)
    | FETCH first_or_next select_fetch_first_value? row_or_rows ONLY
    ;

first_or_next
    : FIRST
    | NEXT
    ;

select_fetch_first_value
    : c_expr
    | only_signed_iconst
    | only_signed_fconst
    ;

row_or_rows
    : ROW
    | ROWS
    ;

offset_clause
    : OFFSET (a_expr | select_fetch_first_value row_or_rows )
    ;

sort_clause
    : ORDER BY sortby_list
    ;

sortby_list
    : (sortby | sortby_index) (',' (sortby | sortby_index))*
    ;

select_with_parens
    : '(' (select_no_parens | select_with_parens) ')'
    ;

for_locking_clause
    : for_locking_items
    | FOR READ ONLY
    ;

for_locking_items
    : for_locking_item+
    ;

for_locking_item
    : for_locking_strength opt_locked_rels opt_nowait_or_skip
    ;

for_locking_strength
    : FOR ( (NO KEY)? UPDATE | KEY? SHARE)
    ;

opt_locked_rels
    : OF table_name_list
    ;

opt_nowait_or_skip
    : SKIP LOCKED
    | NOWAIT
    ;

truncate_stmt
    : TRUNCATE TABLE? relation_expr_list opt_drop_behavior?
    ;

relation_expr_list
    : relation_expr (',' relation_expr)*
    ;

relation_expr
    : table_name '*'?
    | ONLY (table_name | '(' table_name ')')
    ;

update_stmt
    : opt_with_clause? UPDATE table_expr_opt_alias_idx SET set_clause_list
        opt_from_list? where_clause? sort_clause? limit_clause? returning_clause
    ;

table_expr_opt_alias_idx
    : table_name_opt_idx (AS? table_alias_name)?
    ;

opt_from_list
    : FROM from_list
    ;

upsert_stmt
    : opt_with_clause? UPSERT INTO insert_target insert_rest returning_clause
    ;

//

iconst
    : DECIMAL
    ;

fconst
    : FLOAT
    ;

sconst
    : STRING_LITERAL
    ;

//

expr_list_in_parens
    : '(' expr_list ')'
    ;

expr_list
    :
    ;

a_expr
    :
    ;

c_expr
    : d_expr array_subscripts?
    | case_expr
    | EXISTS select_with_parens
    ;

array_subscripts
    : array_subscript+
    ;

array_subscript
    : '[' (a_expr | opt_slice_bound? ':' opt_slice_bound?) ']'
    ;

opt_slice_bound
    : a_expr
    ;

in_expr
    : select_with_parens
    | expr_tuple1_ambiguous
    ;

expr_tuple1_ambiguous
    : '(' tuple1_ambiguous_values ')'
    ;

tuple1_ambiguous_values
    : a_expr (',' expr_list?)?
    ;

//

identifier_
    : ID
    ;

name
    : identifier_
    | unreserved_keyword
    | col_name_keyword
    ;

non_reserved_word
    : identifier_
    | unreserved_keyword
    | col_name_keyword
    | type_func_name_keyword
    ;

unreserved_keyword
    : ABORT
    | ABSOLUTE
    | ACTION
    | ACCESS
    | ADD
    | ADMIN
    | AFTER
    | AGGREGATE
    | ALTER
    | ALWAYS
    | ASENSITIVE
    | AS_JSON
    | AT
    | ATOMIC
    | ATTRIBUTE
    | AUTOMATIC
    | AVAILABILITY
    | AVOID_FULL_SCAN
    | BACKUP
    | BACKUPS
    | BACKWARD
    | BATCH
    | BEFORE
    | BEGIN
    | BIDIRECTIONAL
    | BINARY
    | BUCKET_COUNT
    | BY
    | BYPASSRLS
    | CACHE
    | CALL
    | CALLED
    | CANCEL
    | CANCELQUERY
    | CAPABILITIES
    | CAPABILITY
    | CASCADE
    | CHANGEFEED
    | CHECK_FILES
    | CLOSE
    | CLUSTER
    | CLUSTERS
    | COLUMNS
    | COMMENT
    | COMMENTS
    | COMMIT
    | COMMITTED
    | COMPACT
    | COMPLETE
    | COMPLETIONS
    | CONFLICT
    | CONFIGURATION
    | CONFIGURATIONS
    | CONFIGURE
    | CONNECTION
    | CONNECTIONS
    | CONSTRAINTS
    | CONTROLCHANGEFEED
    | CONTROLJOB
    | CONVERSION
    | CONVERT
    | COPY
    | COST
    | COVERING
    | CREATEDB
    | CREATELOGIN
    | CREATEROLE
    | CSV
    | CUBE
    | CURRENT
    | CURSOR
    | CYCLE
    | DATA
    | DATABASE
    | DATABASES
    | DAY
    | DEALLOCATE
    | DEBUG_IDS
    | DECLARE
    | DELETE
    | DEFAULTS
    | DEFERRED
    | DEFINER
    | DELIMITER
    | DEPENDS
    | DESTINATION
    | DETACHED
    | DETAILS
    | DISABLE
    | DISCARD
    | DOMAIN
    | DOUBLE
    | DROP
    | EACH
    | ENABLE
    | ENCODING
    | ENCRYPTED
    | ENCRYPTION_PASSPHRASE
    | ENCRYPTION_INFO_DIR
    | ENUM
    | ENUMS
    | ERRORS
    | ESCAPE
    | EXCLUDE
    | EXCLUDING
    | EXECUTE
    | EXECUTION
    | EXPERIMENTAL
    | EXPERIMENTAL_AUDIT
    | EXPERIMENTAL_FINGERPRINTS
    | EXPERIMENTAL_RELOCATE
    | EXPERIMENTAL_REPLICA
    | EXPIRATION
    | EXPLAIN
    | EXPORT
    | EXTENSION
    | EXTERNAL
    | EXTREMES
    | FAILURE
    | FILES
    | FILTER
    | FIRST
    | FOLLOWING
    | FORMAT
    | FORCE
    | FORCE_NOT_NULL
    | FORCE_NULL
    | FORCE_QUOTE
    | FORCE_INDEX
    | FORCE_INVERTED_INDEX
    | FORCE_ZIGZAG
    | FORWARD
    | FREEZE
    | FUNCTION
    | FUNCTIONS
    | GENERATED
    | GEOMETRYM
    | GEOMETRYZ
    | GEOMETRYZM
    | GEOMETRYCOLLECTION
    | GEOMETRYCOLLECTIONM
    | GEOMETRYCOLLECTIONZ
    | GEOMETRYCOLLECTIONZM
    | GLOBAL
    | GOAL
    | GRANTEE
    | GRANTS
    | GROUPS
    | HASH
    | HEADER
    | HIGH
    | HISTOGRAM
    | HOLD
    | HOUR
    | IDENTITY
    | IMMEDIATE
    | IMMEDIATELY
    | IMMUTABLE
    | IMPORT
    | INCLUDE
    | INCLUDING
    | INCLUDE_ALL_SECONDARY_TENANTS
    | INCLUDE_ALL_VIRTUAL_CLUSTERS
    | INCREMENT
    | INCREMENTAL
    | INCREMENTAL_LOCATION
    | INDEX
    | INDEXES
    | INHERITS
    | INJECT
    | INPUT
    | INSERT
    | INSPECT
    | INSTEAD
    | INTO_DB
    | INVERTED
    | INVISIBLE
    | ISOLATION
    | INVOKER
    | JOB
    | JOBS
    | JSON
    | KEY
    | KEYS
    | KMS
    | KV
    | LABEL
    | LANGUAGE
    | LAST
    | LATEST
    | LC_COLLATE
    | LC_CTYPE
    | LEAKPROOF
    | LEASE
    | LESS
    | LEVEL
    | LINESTRING
    | LINESTRINGM
    | LINESTRINGZ
    | LINESTRINGZM
    | LIST
    | LOCAL
    | LOCKED
    | LOGICAL
    | LOGICALLY
    | LOGIN
    | LOCALITY
    | LOGGED
    | LOOKUP
    | LOW
    | MATCH
    | MATERIALIZED
    | MAXVALUE
    | MERGE
    | METHOD
    | MINUTE
    | MINVALUE
    | MODIFYCLUSTERSETTING
    | MULTILINESTRING
    | MULTILINESTRINGM
    | MULTILINESTRINGZ
    | MULTILINESTRINGZM
    | MULTIPOINT
    | MULTIPOINTM
    | MULTIPOINTZ
    | MULTIPOINTZM
    | MULTIPOLYGON
    | MULTIPOLYGONM
    | MULTIPOLYGONZ
    | MULTIPOLYGONZM
    | MODE
    | MONTH
    | MOVE
    | NAMES
    | NAN
    | NEVER
    | NEW
    | NEW_DB_NAME
    | NEW_KMS
    | NEXT
    | NO
    | NORMAL
    | NOTHING
    | NO_INDEX_JOIN
    | NO_ZIGZAG_JOIN
    | NO_FULL_SCAN
    | NOBYPASSRLS
    | NOCREATEDB
    | NOCREATELOGIN
    | NOCANCELQUERY
    | NOCREATEROLE
    | NOCONTROLCHANGEFEED
    | NOCONTROLJOB
    | NODE
    | NOLOGIN
    | NOMODIFYCLUSTERSETTING
    | NONVOTERS
    | NOREPLICATION
    | NOSQLLOGIN
    | NOVIEWACTIVITY
    | NOVIEWACTIVITYREDACTED
    | NOVIEWCLUSTERSETTING
    | NOWAIT
    | NULLS
    | IGNORE_FOREIGN_KEYS
    | INSENSITIVE
    | OF
    | OFF
    | OIDS
    | OLD
    | OLD_KMS
    | OPERATOR
    | OPT
    | OPTION
    | OPTIONS
    | ORDINALITY
    | OTHERS
    | OVER
    | OWNED
    | OWNER
    | PARALLEL
    | PARENT
    | PARTIAL
    | PARTITION
    | PARTITIONS
    | PASSWORD
    | PAUSE
    | PAUSED
    | PER
    | PERMISSIVE
    | PHYSICAL
    | PLACEMENT
    | PLAN
    | PLANS
    | POINTM
    | POINTZ
    | POINTZM
    | POLICIES
    | POLICY
    | POLYGONM
    | POLYGONZ
    | POLYGONZM
    | PRECEDING
    | PREPARE
    | PREPARED
    | PRESERVE
    | PRIOR
    | PRIORITY
    | PRIVILEGES
    | PROCEDURE
    | PROCEDURES
    | PROVISIONSRC
    | PUBLIC
    | PUBLICATION
    | QUERIES
    | QUERY
    | QUOTE
    | RANGE
    | RANGES
    | READ
    | REASON
    | REASSIGN
    | RECURRING
    | RECURSIVE
    | REDACT
    | REF
    | REFERENCING
    | REFRESH
    | REGION
    | REGIONAL
    | REGIONS
    | REINDEX
    | RELATIVE
    | RELEASE
    | RELOCATE
    | REMOVE_REGIONS
    | RENAME
    | REPEATABLE
    | REPLACE
    | REPLICATED
    | REPLICATION
    | RESET
    | RESTART
    | RESTORE
    | RESTRICT
    | RESTRICTED
    | RESTRICTIVE
    | RESUME
    | RETENTION
    | RETURN
    | RETURNS
    | REVISION_HISTORY
    | REVOKE
    | ROLE
    | ROLES
    | ROLLBACK
    | ROLLUP
    | ROUTINES
    | ROWS
    | RULE
    | RUN
    | RUNNING
    | SCHEDULE
    | SCHEDULES
    | SCHEMA_ONLY
    | SCROLL
    | SETTING
    | SETTINGS
    | STATUS
    | SAVEPOINT
    | SCANS
    | SCATTER
    | SCHEMA
    | SCHEMAS
    | SCRUB
    | SEARCH
    | SECOND
    | SECURITY
    | SECURITY_INVOKER
    | SECONDARY
    | SERIALIZABLE
    | SEQUENCE
    | SEQUENCES
    | SERVER
    | SERVICE
    | SESSION
    | SESSIONS
    | SET
    | SETS
    | SHARE
    | SHARED
    | SHOW
    | SIMPLE
    | SIZE
    | SKIP
    | SKIP_LOCALITIES_CHECK
    | SKIP_MISSING_FOREIGN_KEYS
    | SKIP_MISSING_SEQUENCES
    | SKIP_MISSING_SEQUENCE_OWNERS
    | SKIP_MISSING_VIEWS
    | SKIP_MISSING_UDFS
    | SOURCE
    | SNAPSHOT
    | SPLIT
    | SQL
    | SQLLOGIN
    | STABLE
    | START
    | STATE
    | STATEMENT
    | STATEMENTS
    | STATISTICS
    | STDIN
    | STDOUT
    | STOP
    | STORAGE
    | STORE
    | STORED
    | STORING
    | STRAIGHT
    | STREAM
    | STRICT
    | SUBSCRIPTION
    | SUBJECT
    | SUPER
    | SUPPORT
    | SURVIVE
    | SURVIVAL
    | SYNTAX
    | SYSTEM
    | TABLES
    | TABLESPACE
    | TEMP
    | TEMPLATE
    | TEMPORARY
    | TENANT
    | TENANT_NAME
    | TENANTS
    | TESTING_RELOCATE
    | TEXT
    | TIES
    | TRACE
    | TRACING
    | TRANSACTION
    | TRANSACTIONS
    | TRANSFER
    | TRANSFORM
    | TRIGGER
    | TRIGGERS
    | TRUNCATE
    | TRUSTED
    | TYPE
    | TYPES
    | THROTTLING
    | UNIDIRECTIONAL
    | UNBOUNDED
    | UNCOMMITTED
    | UNKNOWN
    | UNLISTEN
    | UNLOGGED
    | UNSAFE_RESTORE_INCOMPATIBLE_VERSION
    | UNSET
    | UNSPLIT
    | UNTIL
    | UPDATE
    | UPDATES_CLUSTER_MONITORING_METRICS
    | UPSERT
    | USE
    | USERS
    | VALID
    | VALIDATE
    | VALUE
    | VARIABLES
    | VARYING
    | VERIFY_BACKUP_TABLE_DATA
    | VIEW
    | VIEWACTIVITY
    | VIEWACTIVITYREDACTED
    | VIEWCLUSTERSETTING
    | VIRTUAL_CLUSTER_NAME
    | VIRTUAL_CLUSTER
    | VISIBLE
    | VISIBILITY
    | VOLATILE
    | VOTERS
    | WITHIN
    | WITHOUT
    | WRITE
    | YEAR
    | ZONE
    ;

col_name_keyword
    : ANNOTATE_TYPE
    | BETWEEN
    | BIGINT
    | BIT
    | BOOLEAN
    | BOX2D
    | CHAR
    | CHARACTER
    | CHARACTERISTICS
    | COALESCE
    | DEC
    | DECIMAL
    | EXISTS
    | EXTRACT
    | EXTRACT_DURATION
    | FLOAT
    | GEOGRAPHY
    | GEOMETRY
    | GREATEST
    | GROUPING
    | IF
    | IFERROR
    | IFNULL
    | INOUT
    | INT
    | INTEGER
    | INTERVAL
    | ISERROR
    | LEAST
    | NULLIF
    | NUMERIC
    | OUT
    | OVERLAY
    | POINT
    | POLYGON
    | POSITION
    | PRECISION
    | REAL
    | ROW
    | SETOF
    | SMALLINT
    | STRING
    | SUBSTRING
    | TIME
    | TIMETZ
    | TIMESTAMP
    | TIMESTAMPTZ
    | TREAT
    | TRIM
    | VALUES
    | VARBIT
    | VARCHAR
    | VECTOR
    | VIRTUAL
    | WORK
    ;

column_name
    : name
    ;

constraint_name
    : name
    ;

cursor_name
    : name
    ;

database_name
    : name
    ;

func_application_name
    : func_name
    | '[' FUNCTION iconst ']'
    ;

func_name
    : type_function_name
    | prefixed_column_path
    | INDEX
    ;

index_name
    : name
    ;

view_name
    : table_name
    ;

savepoint_name
    : SAVEPOINT? name
    ;

qualifiable_schema_name
    : name ('.' name)?
    ;

table_index_name
    : table_name '@' index_name
    | standalone_index_name
    ;

standalone_index_name
    : db_object_name
    ;

db_object_name
    : simple_db_object_name
    | complex_db_object_name
    ;

simple_db_object_name
    : db_object_name_component
    ;

db_object_name_component
    : name
    | FAMILY
    | cockroachdb_extra_reserved_keyword
    ;

complex_db_object_name
    : db_object_name_component '.' unrestricted_name ('.' unrestricted_name)?
    ;

db_name
    : db_object_name
    ;

sequence_name
    : db_object_name
    ;

table_name
    : db_object_name
    ;

type_name
    : db_object_name
    ;

unrestricted_name
    : identifier_
    | unreserved_keyword
    | col_name_keyword
    | type_func_name_keyword
    | reserved_keyword
    ;

type_func_name_keyword
    : type_func_name_no_crdb_extra_keyword
    | FAMILY
    ;

reserved_keyword
    : ALL
    | ANALYSE
    | ANALYZE
    | AND
    | ANY
    | ARRAY
    | AS
    | ASC
    | ASYMMETRIC
    | BOTH
    | CASE
    | CAST
    | CHECK
    | COLLATE
    | COLUMN
    | CONCURRENTLY
    | CONSTRAINT
    | CREATE
    | CURRENT_CATALOG
    | CURRENT_DATE
    | CURRENT_ROLE
    | CURRENT_SCHEMA
    | CURRENT_TIME
    | CURRENT_TIMESTAMP
    | CURRENT_USER
    | DEFAULT
    | DEFERRABLE
    | DESC
    | DISTINCT
    | DO
    | ELSE
    | END
    | EXCEPT
    | FALSE
    | FETCH
    | FOR
    | FOREIGN
    | FROM
    | GRANT
    | GROUP
    | HAVING
    | IN
    | INITIALLY
    | INTERSECT
    | INTO
    | LATERAL
    | LEADING
    | LIMIT
    | LOCALTIME
    | LOCALTIMESTAMP
    | NOT
    | NULL
    | OFFSET
    | ON
    | ONLY
    | OR
    | ORDER
    | PLACING
    | PRIMARY
    | REFERENCES
    | RETURNING
    | SELECT
    | SESSION_USER
    | SOME
    | SYMMETRIC
    | TABLE
    | THEN
    | TO
    | TRAILING
    | TRUE
    | UNION
    | UNIQUE
    | USER
    | USING
    | VARIADIC
    | WHEN
    | WHERE
    | WINDOW
    | WITH
    | cockroachdb_extra_reserved_keyword
    ;

cockroachdb_extra_reserved_keyword
    : INDEX
    | NOTHING
    ;
