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
    : ((routine_param_class param_name?)? | param_name routine_param_class?) routine_param_type
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
    : simple_typename (opt_array_bounds | ARRAY)?
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
    : INT_
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

alter_external_connection_stmt
    : ALTER EXTERNAL CONNECTION if_exists? label_spec AS string_or_placeholder
    ;

label_spec
    : if_not_exists? string_or_placeholder
    ;

alter_role_stmt
    : ALTER (
        role_or_group_or_user if_exists? role_spec (opt_role_options | opt_in_database? set_or_reset_clause)
        | (ROLE_ALL | USER_ALL) ALL opt_in_database? set_or_reset_clause
    )
    ;

opt_in_database
    : IN DATABASE database_name
    ;

set_or_reset_clause
    : SET set_rest
    | RESET_ALL ALL
    | RESET session_var
    ;

alter_virtual_cluster_stmt
    : alter_virtual_cluster_replication_stmt
    | alter_virtual_cluster_capability_stmt
    | alter_virtual_cluster_rename_stmt
    | alter_virtual_cluster_service_stmt
    ;

alter_virtual_cluster_replication_stmt
    : ALTER virtual_cluster virtual_cluster_spec
        ( (PAUSE | RESUME) REPLICATION
        | COMPLETE REPLICATION TO (SYSTEM TIME a_expr | LATEST)
        | SET REPLICATION (replication_options_list | SOUCE source_replication_options_list)
        | START REPLICATION OF d_expr ON d_expr opt_with_replication_options?
        )
    ;

virtual_cluster
    : VIRTUAL CLUSTER
    ;

virtual_cluster_spec
    : d_expr
    | '[' a_expr ']'
    ;

replication_options_list
    : replication_options (',' replication_options)*
    ;

replication_options
    : RETENTION '=' d_expr
    | READ VIRTUAL CLUSTER
    ;

source_replication_options_list
    : source_replication_options (',' source_replication_options)*
    ;

source_replication_options
    : EXPIRATION WINDOW '=' d_expr
    ;

opt_with_replication_options
    : WITH (replication_options_list | OPTIONS '(' replication_options_list ')')
    ;

alter_virtual_cluster_capability_stmt
    : ALTER virtual_cluster virtual_cluster_spec (GRANT | REVOKE) ALL CAPABILITIES
    ;

alter_virtual_cluster_rename_stmt
    : ALTER virtual_cluster virtual_cluster_spec RENAME TO d_expr
    ;

alter_virtual_cluster_service_stmt
    : ALTER virtual_cluster virtual_cluster_spec (START SERVICE SHARED | STOP SERVICE)
    ;

alter_ddl_stmt
    : alter_table_stmt
    | alter_index_stmt
    | alter_view_stmt
    | alter_sequence_stmt
    | alter_database_stmt
    | alter_range_stmt
    | alter_partition_stmt
    | alter_schema_stmt
    | alter_type_stmt
    | alter_default_privileges_stmt
    | alter_changefeed_stmt
    | alter_backup_stmt
    | alter_func_stmt
    | alter_proc_stmt
    | alter_backup_schedule
    | alter_policy_stmt
    | alter_job_stmt
    ;

alter_table_stmt
    : alter_onetable_stmt
    | alter_split_stmt
    | alter_unsplit_stmt
    | alter_scatter_stmt
    | alter_zone_table_stmt
    | alter_rename_table_stmt
    | alter_table_set_schema_stmt
    | alter_table_locality_stmt
    | alter_table_logged_stmt
    | alter_table_owner_stmt
    ;

alter_onetable_stmt
    : ALTER TABLE if_exists? relation_expr alter_table_cmds
    ;

alter_table_cmds
    : alter_table_cmd (',' alter_table_cmd)*
    ;

alter_table_cmd
    : RENAME (opt_column? | CONSTRAINT) column_name TO column_name
    | ADD
        ( COLUMN? if_not_exists? column_table_def
        | (table_constraint | CONSTRAINT if_not_exists? constraint_name constraint_elem) opt_validate_behavior?
        )
    | ALTER
        ( opt_column? column_name
            ( alter_column_default
            | alter_column_on_update
            | alter_column_visible
            | DROP (NOT NULL_ | IDENTITY if_exists? | STORED)
            | ADD (generated_always_as | generated_by_default_as) IDENTITY ('(' opt_sequence_option_list ')')?
            | set_generated_always
            | set_generated_default
            | identity_option_list
            | SET NOT NULL_
            | opt_set_data? TYPE typename opt_collate? opt_alter_column_using?
            )
        | PRIMARY KEY USING COLUMNS '(' index_params ')' opt_hash_sharded? opt_with_storage_parameter_list
        )
    | DROP ( opt_column? if_exists? column_name
           | CONSTRAINT if_exists? constraint_name
           ) opt_drop_behavior?
    | VALIDATE CONSTRAINT constraint_name
    | EXPERIMENTAL_AUDIT SET audit_mode
    | partition_by_table
    | SET '(' storage_parameter_list ')'
    | RESET '(' storage_parameter_key_list ')'
    | table_rls_mode ROW LEVEL SECURITY
    ;

opt_column
    : COLUMN
    ;

column_table_def
    : column_name typename col_qual_list
    ;

table_constraint
    : (CONSTRAINT constraint_name)? constraint_elem
    ;

constraint_elem
    : CHECK '(' a_expr ')'
    | UNIQUE '(' index_params ')' opt_storing? opt_partition_by_index? opt_where_clause?
    | PRIMARY
    | FOREIGN
    ;

opt_storing
    : storing '(' name_list ')'
    ;

storing
    : COVERING
    | STORING
    | INCLUDE
    ;

opt_partition_by_index
    : partition_by
    ;

opt_validate_behavior
    : NOT VALID
    ;

alter_column_default
    : SET DEFAULT a_expr
    | DROP DEFAULT
    ;

alter_column_on_update
    : SET ON UPDATE a_expr
    | DROP ON UPDATE
    ;

alter_column_visible
    : SET NOT? VISIBLE
    ;

identity_option_list
    : identity_option_elem+
    ;

identity_option_elem
    : SET ( NO (CYCLE | MINVALUE | MAXVALUE)
          | ( (PER (NODE | SESSION)) CACHE
            | MINVALUE
            | MAXVALUE
            | INCREMENT BY?
            | START WITH?
            ) signed_iconst64
          )
    | RESTART (WITH? signed_iconst64)?
    ;

opt_set_data
    : SET DATA
    ;

opt_collate
    : COLLATE collation_name
    ;

opt_alter_column_using
    : USING a_expr
    ;

col_qual_list
    : col_qualification*
    ;

col_qualification
    : (CONSTRAINT constraint_name)? col_qualification_elem
    | COLLATE collation_name
    | FAMILTY family_name
    | CREATE (FAMILTY family_name? | if_not_exists? FAMILY family_name)
    ;

col_qualification_elem
    : NOT (NULL_ | VISIBLE)
    | NULL_
    | UNIQUE
    | PRIMARY KEY (USING HASH opt_hash_sharded_bucket_count)? opt_with_storage_parameter_list
    | CHECK '(' a_expr ')'
    | (DEFAULT | ON UPDATE) b_expr
    | REFERENCES table_name opt_name_parens? key_match? reference_actions
    | generated_as '(' a_expr ')' (STORED | VIRTUAL)
    | (generated_always_as | generated_by_default_as) IDENTITY ('(' opt_sequence_option_list? ')')?
    ;

opt_name_parens
    : '(' name ')'
    ;

key_match
    : MATCH (SIMPLE | FULL)
    ;

reference_actions
    : reference_on_update reference_on_delete?
    | reference_on_delete reference_on_update?
    ;

reference_on_update
    : ON UPDATE reference_action
    ;

reference_on_delete
    : ON DELETE reference_action
    ;

reference_action
    : NO ACTION
    | RESTRICT
    | CASCADE
    | SET (NULL_ | DEFAULT)
    ;

generated_as
    : AS
    | generated_always_as
    ;

generated_always_as
    : GENERATED_ALWAYS ALWAYS AS
    ;

generated_by_default_as
    : GENERATED_BY_DEFAULT BY DEFAULT AS
    ;

opt_sequence_option_list
    : sequence_option_list
    ;

sequence_option_list
    : sequence_option_elem+
    ;

sequence_option_elem
    : AS typename
    | NO (CYCLE | MINVALUE | MAXVALUE)
    | OWNED BY (NONE | column_path)
    | ( (PER (NODE | SESSION))? CACHE
      | MINVALUE
      | MAXVALUE
      | INCREMENT BY?
      | START WITH?
      )  signed_iconst64
    | RESTART (WITH? signed_iconst64)?
    | VIRTUAL
    ;

set_generated_always
    : SET GENERATED_ALWAYS ALWAYS
    ;

set_generated_default
    : SET GENERATED_BY_DEFAULT BY DEFAULT
    ;

index_params
    : index_elem (',' index_elem)*
    ;

index_elem
    : (func_expr_windowless | '(' a_expr ')' | name) index_elem_options
    ;

index_elem_options
    : opt_class? opt_asc_desc? opt_nulls_order?
    ;

opt_class
    : name
    ;

opt_hash_sharded
    : USING HASH opt_hash_sharded_bucket_count?
    ;

opt_hash_sharded_bucket_count
    : WITH BUCKET_COUNT '=' a_expr
    ;

opt_with_storage_parameter_list
    : WITH '(' storage_parameter_list ')'
    ;

audit_mode
    : READ WRITE
    | OFF
    ;

partition_by_table
    : partition_by
    | PARTITION ALL BY partition_by_inner
    ;

partition_by
    : PARTITION BY partition_by_inner
    ;

partition_by_inner
    : LIST '(' name_list ')' '(' list_partitions ')'
    | RANGE '(' name_list ')' '(' range_partitions ')'
    | NOTHING
    ;

list_partitions
    : list_partition (',' list_partition)*
    ;

list_partition
    : partition VALUES IN '(' expr_list ')' opt_partition_by?
    ;

opt_partition_by
    : partition_by
    ;

range_partitions
    : range_partition (',' range_partition)*
    ;

range_partition
    : partition VALUES FROM '(' expr_list ')' TO '(' expr_list ')' opt_partition_by?
    ;

storage_parameter_list
    : storage_parameter (',' storage_parameter)*
    ;

storage_parameter
    : storage_parameter_key '=' var_value
    ;

storage_parameter_key_list
    : storage_parameter_key (',' storage_parameter_key)*
    ;

storage_parameter_key
    : name
    | sconst
    ;

table_rls_mode
    : ENABLE
    | DISABLE
    | NO? FORCE
    ;

alter_split_stmt
    : ALTER TABLE table_name SPLIT AT select_stmt (WITH EXPIRATION a_expr)?
    ;

alter_unsplit_stmt
    : ALTER TABLE table_name UNSPLIT (AT select_stmt | ALL)
    ;

alter_scatter_stmt
    : ALTER TABLE table_name SCATTER (FROM expr_list_in_parens TO expr_list_in_parens)?
    ;

alter_zone_table_stmt
    : ALTER TABLE table_name set_zone_config
    ;

alter_rename_table_stmt
    : ALTER TABLE if_exists? relation_expr RENAME TO table_name
    ;

alter_table_set_schema_stmt
    : ALTER TABLE if_exists relation_expr SET SCHEMA schema_name
    ;

alter_table_locality_stmt
    : ALTER TABLE if_exists? relation_expr SET locality
    ;

locality
    : LOCALITY
        ( GLOBAL
        | REGIONAL ( BY ( TABLE (IN (region_name | PRIMARY REGION))?
                        | ROW (AS name)?
                        )
                   | IN (region_name | PRIMARY REGION)
                   )?
        )
    ;

region_name
    : name
    | sconst
    ;

alter_table_logged_stmt
    : ALTER TABLE if_exists? relation_expr SET (LOGGED | UNLOGGED)
    ;

alter_table_owner_stmt
    : ALTER TABLE if_exists? relation_expr OWNER TO role_spec
    ;

alter_index_stmt
    : alter_oneindex_stmt
    | alter_split_index_stmt
    | alter_unsplit_index_stmt
    | alter_scatter_index_stmt
    | alter_rename_index_stmt
    | alter_zone_index_stmt
    | alter_index_visible_stmt
    ;

alter_oneindex_stmt
    : ALTER INDEX if_exists? table_index_name alter_index_cmds
    ;

alter_index_cmds
    : alter_index_cmd (',' alter_index_cmd)*
    ;

alter_index_cmd
    : partition_by_index
    ;

partition_by_index
    : partition_by
    ;

alter_split_index_stmt
    : ALTER INDEX table_index_name SPLIT AT select_stmt (WITH EXPIRATION a_expr)?
    ;

alter_unsplit_index_stmt
    : ALTER INDEX table_index_name UNSPLIT (AT select_stmt | ALL)
    ;

alter_scatter_index_stmt
    : ALTER INDEX table_index_name SCATTER (FROM expr_list_in_parens TO expr_list_in_parens)?
    ;

alter_rename_index_stmt
    : ALTER INDEX if_exists? table_index_name RENAME TO index_name
    ;

alter_zone_index_stmt
    : ALTER INDEX table_index_name set_zone_config
    ;

alter_index_visible_stmt
    : ALTER INDEX if_exists? table_index_name alter_index_visible
    ;

alter_index_visible
    : NOT? VISIBLE
    | INVISIBLE
    | VISIBILITY fconst
    ;

alter_view_stmt
    : alter_rename_view_stmt
    | alter_view_set_schema_stmt
    | alter_view_owner_stmt
    ;

alter_rename_view_stmt
    : ALTER MATERIALIZED? VIEW if_exists? relation_expr RENAME TO view_name
    ;

alter_view_set_schema_stmt
    : ALTER MATERIALIZED? VIEW if_exists? relation_expr SET SCHEMA schema_name
    ;

alter_view_owner_stmt
    : ALTER MATERIALIZED? VIEW if_exists? relation_expr OWNER TO role_spec
    ;

alter_sequence_stmt
    : alter_rename_sequence_stmt
    | alter_sequence_options_stmt
    | alter_sequence_set_schema_stmt
    | alter_sequence_owner_stmt
    ;

alter_rename_sequence_stmt
    : ALTER SEQUENCE if_exists? relation_expr RENAME TO sequence_name
    ;

alter_sequence_options_stmt
    : ALTER SEQUENCE if_exists? sequence_name sequence_option_list
    ;

alter_sequence_set_schema_stmt
    : ALTER SEQUENCE if_exists? relation_expr SET SCHEMA schema_name
    ;

alter_sequence_owner_stmt
    : ALTER SEQUENCE if_exists relation_expr OWNER TO role_spec
    ;

alter_database_stmt
    : alter_rename_database_stmt
    | alter_zone_database_stmt
    | alter_database_owner
    | alter_database_to_schema_stmt
    | alter_database_add_region_stmt
    | alter_database_drop_region_stmt
    | alter_database_survival_goal_stmt
    | alter_database_primary_region_stmt
    | alter_database_placement_stmt
    | alter_database_set_stmt
    | alter_database_add_super_region
    | alter_database_alter_super_region
    | alter_database_drop_super_region
    | alter_database_set_secondary_region_stmt
    | alter_database_drop_secondary_region
    | alter_database_set_zone_config_extension_stmt
    ;

alter_rename_database_stmt
    : ALTER DATABASE database_name RENAME TO database_name
    ;

alter_zone_database_stmt
    : ALTER DATABASE database_name set_zone_config
    ;

alter_database_owner
    : ALTER DATABASE database_name OWNER TO role_spec
    ;

alter_database_to_schema_stmt
    : ALTER DATABASE database_name CONVERT TO SCHEMA WITH PARENT database_name
    ;

alter_database_add_region_stmt
    : ALTER DATABASE database_name ADD REGION if_not_exists? region_name
    ;

alter_database_drop_region_stmt
    : ALTER DATABASE database_name DROP REGION if_exists? region_name
    ;

alter_database_survival_goal_stmt
    : ALTER DATABASE database_name survival_goal_clause
    ;

survival_goal_clause
    : SURVIVE opt_equal? (REGION | ZONE) FAILURE
    ;

opt_equal
    : '='
    ;

alter_database_primary_region_stmt
    : ALTER DATABASE database_name SET? primary_region_clause
    ;

primary_region_clause
    : PRIMARY REGION opt_equal? region_name
    ;

alter_database_placement_stmt
    : ALTER DATABASE database_name placement_clause
    ;

placement_clause
    : PLACEMENT (RESTRICTED | DEFAULT)
    ;

alter_database_set_stmt
    : ALTER DATABASE database_name set_or_reset_clause
    ;

alter_database_add_super_region
    : ALTER DATABASE database_name ADD SUPER REGION region_name VALUES region_name_list
    ;

region_name_list
    : region_name (',' region_name)*
    ;

alter_database_alter_super_region
    : ALTER DATABASE database_name ALTER SUPER REGION region_name VALUES region_name_list
    ;

alter_database_drop_super_region
    : ALTER DATABASE database_name DROP SUPER REGION region_name
    ;

alter_database_set_secondary_region_stmt
    : ALTER DATABASE database_name SET secondary_region_clause
    ;

secondary_region_clause
    : SECONDARY REGION opt_equal? region_name
    ;

alter_database_drop_secondary_region
    : ALTER DATABASE database_name DROP SECONDARY REGION if_exists?
    ;

alter_database_set_zone_config_extension_stmt
    : ALTER DATABASE database_name ALTER LOCALITY
        (GLOBAL | REGIONAL (IN region_name)?)? set_zone_config
    ;

alter_range_stmt
    : alter_zone_range_stmt
    | alter_range_relocate_stmt
    ;

alter_zone_range_stmt
    : ALTER RANGE a_expr set_zone_config
    ;

alter_range_relocate_stmt
    : ALTER RANGE
        ( relocate_kw lease_or_relocate_subject_nonlease TO a_expr FOR select_stmt
        | a_expr relocate_kw lease_or_relocate_subject_nonlease TO a_expr
        )
    ;

relocate_kw
    : TESTING_RELOCATE
    | EXPERIMENTAL_RELOCATE
    | RELOCATE
    ;

lease_or_relocate_subject_nonlease
    : LEASE
    | relocate_subject_nonlease? FROM a_expr
    ;

relocate_subject_nonlease
    : VOTERS
    | NONVOTERS
    ;

alter_partition_stmt
    : alter_zone_partition_stmt
    ;

alter_zone_partition_stmt
    : ALTER PARTITION partition_name OF
        ( TABLE table_name
        | INDEX (table_index_name | table_name '@' '*')
        ) set_zone_config
    ;

set_zone_config
    : CONFIGURE ZONE (USING var_set_list | DISCARD)
    ;

var_set_list
    : var_set_item (',' var_set_item)*
    ;

var_set_item
    : var_name '=' (COPY FROM PARENT | var_value)
    ;

alter_schema_stmt
    : ALTER SCHEMA qualifiable_schema_name (RENAME TO schema_name | OWNER TO role_spec)
    ;

alter_type_stmt
    : ALTER TYPE type_name (
        | ADD VALUE if_not_exists? sconst opt_add_val_placement?
        | DROP VALUE sconst
        | RENAME (VALUE sconst TO sconst | TO name)
        | SET SCHEMA schema_name
        | OWNER TO role_spec
        )
    ;

opt_add_val_placement
    : (BEFORE | AFTER) sconst
    ;

alter_default_privileges_stmt
    : ALTER DEFAULT PRIVILEGES (opt_for_roles | FOR ALL ROLES) opt_in_schemas?
        (abbreviated_grant_stmt | abbreviated_revoke_stmt)
    ;

abbreviated_grant_stmt
    : GRANT privileges ON target_object_type TO role_spec_list opt_with_grant_option?
    ;

target_object_type
    : TABLES
    | SEQUENCES
    | TYPES
    | SCHEMAS
    | FUNCTIONS
    | ROUTINES
    ;

abbreviated_revoke_stmt
    : REVOKE (GRANT OPTION FOR)? privileges ON target_object_type FROM role_spec_list opt_drop_behavior?
    ;

opt_in_schemas
    : IN SCHEMA schema_name_list
    ;

alter_changefeed_stmt
    : ALTER CHANGEFEED a_expr alter_changefeed_cmds
    ;

alter_changefeed_cmds
    : alter_changefeed_cmd+
    ;

alter_changefeed_cmd
    : ADD changefeed_table_targets opt_with_options?
    | DROP changefeed_table_targets
    | SET kv_option_list
    | UNSET name_list
    ;

changefeed_table_targets
    : changefeed_table_target (',' changefeed_table_target)*
    ;

changefeed_table_target
    : opt_table_prefix? table_name opt_chagefeed_family?
    ;

opt_table_prefix
    : TABLE
    ;

opt_chagefeed_family
    : FAMILY family_name
    ;

alter_backup_stmt
    : ALTER BACKUP string_or_placeholder (IN string_or_placeholder)? alter_backup_cmds
    ;

alter_backup_cmds
    : alter_backup_cmd+
    ;

alter_backup_cmd
    : ADD backup_kms
    ;

backup_kms
    : NEW_KMS '=' string_or_placeholder_opt_list WITH OLD_KMS '=' string_or_placeholder_opt_list
    ;

alter_func_stmt
    : alter_func_options_stmt
    | alter_func_rename_stmt
    | alter_func_owner_stmt
    | alter_func_set_schema_stmt
    | alter_func_dep_extension_stmt
    ;

alter_func_options_stmt
    : ALTER FUNCTION function_with_paramtypes alter_func_opt_list ops_restrict?
    ;

alter_func_opt_list
    : common_routine_opt_item+
    ;

common_routine_opt_item
    : (CALLED | RETURNS NULL_) ON NULL_ INPUT
    | STRICT
    | IMMUTABLE
    | STABLE
    | VOLATILE
    | EXTERNAL? SECURITY (DEFINER | INVOKER)
    | NOT? LEAKPROOF
    ;

ops_restrict
    : RESTRICT
    ;

alter_func_rename_stmt
    : ALTER FUNCTION function_with_paramtypes RENAME TO name
    ;

alter_func_owner_stmt
    : ALTER FUNCTION function_with_paramtypes OWNER TO role_spec
    ;

alter_func_set_schema_stmt
    : ALTER FUNCTION function_with_paramtypes SET SCHEMA schema_name
    ;

alter_func_dep_extension_stmt
    : ALTER FUNCTION function_with_paramtypes opt_no? DEPENDS ON EXTENSION name
    ;

opt_no
    : NO
    ;

alter_proc_stmt
    : alter_proc_rename_stmt
    | alter_proc_owner_stmt
    | alter_proc_set_schema_stmt
    ;

alter_proc_rename_stmt
    : ALTER PROCEDURE function_with_paramtypes RENAME TO name
    ;

alter_proc_owner_stmt
    : ALTER PROCEDURE function_with_paramtypes OWNER TO role_spec
    ;

alter_proc_set_schema_stmt
    : ALTER PROCEDURE function_with_paramtypes SET SCHEMA schema_name
    ;

alter_backup_schedule
    : ALTER BACKUP SCHEDULE iconst alter_backup_schedule_cmds
    ;

alter_backup_schedule_cmds
    : alter_backup_schedule_cmd (',' alter_backup_schedule_cmd)*
    ;

alter_backup_schedule_cmd
    : SET
        ( LABEL sconst_or_placeholder
        | INTO string_or_placeholder_opt_list
        | WITH backup_options
        | cron_expr
        | FULL BACKUP (ALWAYS | sconst_or_placeholder)
        | SCHEDULE OPTION kv_option
        )
    | EXECUTE FULL? IMMEDIATELY
    ;

cron_expr
    : (name | sconst) ('=' string_or_placeholder)?
    ;

alter_policy_stmt
    : ALTER POLICY name ON table_name
        ( RENAME TO name
        | opt_policy_roles? opt_policy_exprs?
        )
    ;

opt_policy_roles
    : TO role_spec_list
    ;

opt_policy_exprs
    : ( USING ('(' a_expr ')' WITH CHECK)?
      | WITH CHECK ('(' a_expr ')' USING)?
      ) '(' a_expr ')'
    ;

alter_job_stmt
    : ALTER JOB a_expr OWNER TO role_spec
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

cancel_jobs_stmt
    : CANCEL (JOB a_expr | JOBS (select_stmt | for_schedules_clause))
    ;

cancel_queries_stmt
    : CANCEL (QUERY if_exists? a_expr | QUERIES if_exists? select_stmt)
    ;

cancel_sessions_stmt
    : CANCEL (SESSION if_exists a_expr | SESSIONS if_exists? select_stmt)
    ;

cancel_all_jobs_stmt
    : CANCEL ALL name JOBS
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

create_stats_stmt
    : CREATE STATISTICS statistics_name opt_stats_columns?
        FROM create_stats_target opt_create_stats_options?
    ;

opt_stats_columns
    : ON name_list
    ;

create_stats_target
    : table_name
    ;

opt_create_stats_options
    : (WITH OPTIONS)? create_stats_option_list
    ;

create_stats_option_list
    : create_stats_option+
    ;

create_stats_option
    : as_of_clause
    | USING EXTREMES
    | where_clause
    ;

create_changefeed_stmt
    : CREATE CHANGEFEED
        ( for_with_lookahead_variants changefeed_table_targets opt_changefeed_sink opt_with_options?
        | opt_changefeed_sink opt_with_options? AS SELECT target_list FROM changefeed_target_expr opt_where_clause?
        )
    ;

opt_changefeed_sink
    : INTO string_or_placeholder
    ;

changefeed_target_expr
    : insert_target
    ;

create_extension_stmt
    : CREATE EXTENSION if_not_exists? name
    ;

create_external_connection_stmt
    : CREATE EXTERNAL CONNECTION label_spec AS string_or_placeholder
    ;

create_logical_replication_stream_stmt
    : CREATE LOGICALLY REPLICATED logical_replication_resources FROM logical_replication_resources
        ON string_or_placeholder opt_logical_replication_create_table_options?
    ;

logical_replication_resources
    : TABLE db_object_name
    | TABLES '(' logical_replication_resources_list ')'
    | DATABASE database_name
    ;

logical_replication_resources_list
    : db_object_name (',' db_object_name)*
    ;

opt_logical_replication_create_table_options
    : WITH ( logical_replication_create_table_options_list
           | OPTIONS '(' logical_replication_create_table_options_list ')'
           )
    ;

logical_replication_create_table_options_list
    : logical_replication_create_table_options (',' logical_replication_create_table_options)*
    ;

logical_replication_create_table_options
    : ( (MODE | DISCARD | LABEL) '='
        | BIDIRECTIONAL ON
      ) string_or_placeholder
    | UNIDIRECTIONAL
    ;

create_schedule_stmt
    : create_schedule_for_changefeed_stmt
    | create_schedule_for_backup_stm
    ;

create_schedule_for_changefeed_stmt
    : CREATE SCHEDULE schedule_label_spec FOR CHANGEFEED
        ( changefeed_table_targets changefeed_sink opt_with_options?
        | changefeed_sink opt_with_options? AS SELECT target_list
            FROM changefeed_target_expr opt_where_clause? cron_expr opt_with_schedule_options?
        )
    ;

schedule_label_spec
    : label_spec
    ;

changefeed_sink
    : INTO string_or_placeholder
    ;

opt_with_schedule_options
    : WITH SCHEDULE OPTIONS (kv_option_list | '(' kv_option_list ')')
    ;

create_schedule_for_backup_stm
    : CREATE SCHEDULE schedule_label_spec FOR BACKUP opt_backup_targets
        INTO string_or_placeholder_opt_list opt_with_backup_options? cron_expr
            opt_full_backup_clause? opt_with_schedule_options?
    ;

opt_full_backup_clause
    : FULL BACKUP (sconst_or_placeholder | ALWAYS)
    ;

create_ddl_stmt
    : create_database_stmt
    | create_index_stmt
    | create_schema_stmt
    | create_table_stmt
    | create_table_as_stmt
    | create_type_stmt
    | create_view_stmt
    | create_sequence_stmt
    | create_func_stmt
    | create_proc_stmt
    | create_trigger_stmt
    | create_policy_stmt
    ;

create_database_stmt
    : CREATE DATABASE if_not_exists? database_name opt_with?
        opt_template_clause? opt_encoding_clause? opt_lc_collate_clause? opt_lc_ctype_clause?
        opt_connection_limit? opt_primary_region_clause? opt_regions_list? opt_survival_goal_clause?
        opt_placement_clause? opt_owner_clause? opt_super_region_clause? opt_secondary_region_clause?
    ;

opt_template_clause
    : TEMPLATE opt_equal? non_reserved_word_or_sconst
    ;

opt_encoding_clause
    : ENCODING opt_equal? non_reserved_word_or_sconst
    ;

opt_lc_collate_clause
    : LC_COLLATE_ opt_equal? non_reserved_word_or_sconst
    ;

opt_lc_ctype_clause
    : LC_CTYPE_ opt_equal? non_reserved_word_or_sconst
    ;

opt_connection_limit
    : CONNECTION LIMIT opt_equal? signed_iconst
    ;

opt_primary_region_clause
    : primary_region_clause
    ;

opt_regions_list
    : REGIONS opt_equal? region_name_list
    ;

opt_survival_goal_clause
    : survival_goal_clause
    ;

opt_placement_clause
    : placement_clause
    ;

opt_owner_clause
    : OWNER opt_equal? role_spec
    ;

opt_super_region_clause
    : super_region_clause
    ;

super_region_clause
    : SUPER REGION region_name VALUES region_name_list
    ;

opt_secondary_region_clause
    : secondary_region_clause
    ;

create_index_stmt
    : CREATE UNIQUE?
        ( INDEX opt_concurrently? (index_name | if_not_exists? index_name)? ON table_name opt_index_access_method? '(' index_params ')' opt_hash_sharded
        | (INVERTED | VECTOR) INDEX opt_concurrently? (index_name | if_not_exists? index_name)? ON table_name '(' index_params ')'
        ) opt_storing? opt_partition_by_index? opt_with_storage_parameter_list? opt_where_clause? opt_index_visible?
    ;

opt_index_access_method
    : USING name
    ;

create_schema_stmt
    : CREATE SCHEMA if_not_exists? (qualifiable_schema_name | opt_schema_name? AUTHORIZATION role_spec)
    ;

opt_schema_name
    : qualifiable_schema_name
    ;

create_table_stmt
    : CREATE opt_persistence_temp_table? TABLE if_not_exists? table_name
        '(' opt_table_elem_list? ')' partition_by_table? opt_table_with?
        opt_create_table_on_commit? locality?
    ;

opt_table_with
    : opt_with_storage_parameter_list
    ;

opt_create_table_on_commit
    : ON COMMIT PRESERVE ROWS
    ;

opt_persistence_temp_table
    : (LOCAL | GLOBAL)? (TEMPORARY | TEMP)
    | UNLOGGED
    ;

opt_table_elem_list
    : table_elem_list
    ;

table_elem_list
    : table_elem (',' table_elem)*
    ;

table_elem
    : column_table_def
    | index_def
    | family_def
    | table_constraint opt_validate_behavior
    | LIKE table_name like_table_option_list
    ;

index_def
    : ( INDEX name '(' index_params ')' opt_hash_sharded opt_storing
      | UNIQUE INDEX index_name?
      | (INVERTED | VECTOR) INDEX name? '(' index_params ')'
      ) opt_partition_by_index? opt_with_storage_parameter_list? opt_where_clause? opt_index_visible?
    ;

opt_index_visible
    : NOT? VISIBLE
    | INVISIBLE
    | VISIBILTY fconst
    ;

family_def
    : FAMILY family_name? '(' name_list ')'
    ;

like_table_option_list
    : ( (INCLUDING | EXCLUDING) like_table_option)+
    ;

like_table_option
    : CONSTRAINTS
    | DEFAULTS
    | GENERATED
    | INDEXES
    | ALL
    ;

create_table_as_stmt
    : CREATE opt_persistence_temp_table? TABLE if_not_exists?
        table_name create_as_opt_col_list? opt_table_with? AS select_stmt
            opt_create_table_on_commit
    ;

create_as_opt_col_list
    : '(' create_as_table_defs ')'
    ;

create_as_table_defs
    : column_name create_as_col_qual_list? (',' (column_name create_as_col_qual_list? | family_def | create_as_constraint_def) )*
    ;

create_as_col_qual_list
    : create_as_col_qualification
    ;

create_as_col_qualification
    : create_as_col_qualification_elem
    | FAMILY family_name
    ;

create_as_col_qualification_elem
    : PRIMARY KEY opt_with_storage_parameter_list
    ;

create_as_constraint_def
    : create_as_constraint_elem
    ;

create_as_constraint_elem
    : PRIMARY KEY '(' create_as_params ')' opt_with_storage_parameter_list
    ;

create_as_params
    : create_as_param (',' create_as_param)*
    ;

create_as_param
    : column_name
    ;


create_type_stmt
    : CREATE TYPE if_not_exists? type_name AS
        ( ENUM '(' opt_enum_val_list? ')'
        | '(' opt_composite_type_list? ')'
        )
    ;

opt_enum_val_list
    : enum_val_list
    ;

enum_val_list
    : sconst (',' sconst)*
    ;

opt_composite_type_list
    : composite_type_list
    ;

composite_type_list
    : name simple_typename (',' name simple_typename)*
    ;

create_view_stmt
    : CREATE
        ( (opt_temp? VIEW if_not_exists? | or_replace opt_temp? VIEW) view_name opt_column_list? AS select_stmt
        | MATERIALIZED VIEW if_not_exists? view_name opt_column_list? AS select_stmt opt_with_data?
        )
    ;

opt_with_data
    : WITH DATA
    ;

create_sequence_stmt
    : CREATE opt_temp? SEQUENCE if_not_exists? sequence_name opt_sequence_option_list?
    ;

opt_temp
    : TEMPORARY
    | TEMP
    ;

create_func_stmt
    : CREATE or_replace? FUNCTION routine_create_name '(' opt_routine_param_with_defaults_list? ')'
        ( RETURNS ( opt_return_set? routine_return_type
                  | TABLE '(' table_func_column_list ')'
                  )
        )?
        opt_create_routine_opt_list? opt_routine_body?
    ;

opt_return_set
    : SETOF
    ;

routine_return_type
    : routine_param_type
    ;

table_func_column_list
    : table_func_column (',' table_func_column)*
    ;

table_func_column
    : param_name routine_param_type
    ;

create_proc_stmt
    : CREATE or_replace? PROCEDURE routine_create_name '(' opt_routine_param_with_defaults_list? ')'
        opt_create_routine_opt_list? opt_routine_body?
    ;

opt_routine_param_with_defaults_list
    : routine_param_with_defaults_list
    ;

routine_param_with_defaults_list
    : routine_param_with_default (',' routine_param_with_default)*
    ;

routine_param_with_default
    : routine_param ( (DEFAULT | '=') a_expr)?
    ;

opt_create_routine_opt_list
    : routine_return_stmt
    | BEGIN ATOMIC routine_body_stmt_list? END
    ;

routine_return_stmt
    : RETURN a_expr
    ;

routine_body_stmt_list
    : (routine_body_stmt ';')+
    ;

routine_body_stmt
    : stmt_without_legacy_transaction
    | routine_return_stmt
    ;

opt_routine_body
    : routine_return_stmt
    | BEGIN ATOMIC routine_body_stmt_list END
    ;

or_replace
    : OR REPLACE
    ;

routine_create_name
    : db_object_name
    ;

create_trigger_stmt
    : CREATE or_replace? TRIGGER name trigger_action_time trigger_event_list
        ON table_name opt_trigger_transition_list trigger_for_each? trigger_when?
        EXECUTE function_or_procedure func_name '(' trigger_func_args? ')'
    ;

trigger_action_time
    : BEFORE
    | AFTER
    | INSTEAD OF
    ;

trigger_event_list
    : OR
    | trigger_event
    ;

trigger_event
    : INSERT
    | DELETE
    | UPDATE (OF name_list)?
    | TRUNCATE
    ;

opt_trigger_transition_list
    : trigger_transition_list
    ;

trigger_transition_list
    : trigger_transition+
    ;

trigger_transition
    : transition_is_new transition_is_row AS? table_alias_name
    ;

transition_is_new
    : NEW
    | OLD
    ;

transition_is_row
    : ROW
    | TABLE
    ;

trigger_for_each
    : FOR EACH? trigger_for_type
    ;

trigger_for_type
    : ROW
    | STATEMENT
    ;

trigger_when
    : WHEN a_expr
    ;

function_or_procedure
    : FUNCTION
    | PROCEDURE
    ;

trigger_func_args
    : trigger_func_arg (',' trigger_func_arg)*
    ;

trigger_func_arg
    : iconst
    | fconst
    | sconst
    | unrestricted_name
    ;

create_policy_stmt
    : CREATE POLICY if_not_exists? name ON table_name
        opt_policy_type? opt_policy_command? opt_policy_roles opt_policy_exprs
    ;

opt_policy_type
    : AS (PERMISSIVE | RESTRICTIVE)
    ;

opt_policy_command
    : FOR (ALL | SELECT | INSERT | UPDATE | DELETE)
    ;

create_role_stmt
    : CREATE role_or_group_or_user if_not_exists? role_spec opt_role_options?
    ;

if_not_exists
    : IF NOT EXISTS
    ;

opt_role_options
    : opt_with? role_options
    ;

role_options
    : role_option+
    ;

role_option
    : CREATEROLE
    | NOCREATEROLE
    | LOGIN
    | NOLOGIN
    | CONTROLJOB
    | NOCONTROLJOB
    | CONTROLCHANGEFEED
    | NOCONTROLCHANGEFEED
    | CREATEDB
    | NOCREATEDB
    | CREATELOGIN
    | NOCREATELOGIN
    | VIEWACTIVITY
    | NOVIEWACTIVITY
    | VIEWACTIVITYREDACTED
    | NOVIEWACTIVITYREDACTED
    | CANCELQUERY
    | NOCANCELQUERY
    | MODIFYCLUSTERSETTING
    | NOMODIFYCLUSTERSETTING
    | SQLLOGIN
    | NOSQLLOGIN
    | VIEWCLUSTERSETTING
    | NOVIEWCLUSTERSETTING
    | password_clause
    | valid_until_clause
    | provisionsrc_clause
    | REPLICATION
    | NOREPLICATION
    | BYPASSRLS
    | NOBYPASSRLS
    ;

password_clause
    : PASSWORD (sconst_or_placeholder | NULL_)
    ;

valid_until_clause
    : VALID UNTIL (string_or_placeholder | NULL_)
    ;

provisionsrc_clause
    : PROVISIONSRC (string_or_placeholder | NULL_)
    ;

check_stmt
    : check_external_connection_stmt
    ;

check_external_connection_stmt
    : CHECK EXTERNAL CONNECTION string_or_placeholder opt_with_check_external_connection_options_list?
    ;

opt_with_check_external_connection_options_list
    : WITH (check_external_connection_options_list | OPTIONS '(' check_external_connection_options_list ')')
    ;

check_external_connection_options_list
    : check_external_connection_options (',' check_external_connection_options)*
    ;

check_external_connection_options
    : (TRANSFER | TIME) '=' string_or_placeholder
    | CONCURRENTLY '=' a_expr
    ;

delete_stmt
    : opt_with_clause? DELETE opt_batch_clause? FROM table_expr_opt_alias_idx
        opt_using_clause? opt_where_clause? sort_clause? limit_clause?
            returning_clause
    ;

opt_with_clause
    : with_clause
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
    | DOMAIN_
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
    | INT_
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
    | LC_COLLATE_
    | LC_CTYPE_
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
    | NAN_
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
    | NULL_
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
    | SKIP_
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
    | LC_COLLATE_
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

scrub_table_stmt
    : EXPERIMENTAL SCRUB TABLE table_name opt_as_of_clause? opt_scrub_options_clause?
    ;

opt_scrub_options_clause
    : WITH OPTIONS scrub_option_list
    ;

scrub_option_list
    : scrub_option (',' scrub_option)*
    ;

scrub_option
    : (INDEX | CONSTRAINT) (ALL | '(' name_list ')')
    | PHYSICAL
    ;

scrub_database_stmt
    : EXPERIMENTAL SCRUB DATABASE database_name opt_as_of_clause?
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
    | simple_select (
        sort_clause
        | sort_clause? (for_locking_clause select_limit? | select_limit for_locking_clause?)
      )
    | with_clause simple_select (
            sort_clause
            | sort_clause? (for_locking_clause select_limit? | select_limit for_locking_clause?)
        )?
    ;

simple_select
    : simple_select_clause
    | values_clause
    | table_clause
    | select_with_parens
    | simple_select (UNION | INTERSECT | EXCEPT) all_or_distinct? simple_select
    ;

simple_select_clause
    : SELECT
        ( opt_all_clause? target_list?
        | (DISTINCT | distinct_on_clause) target_list
        )
        from_clause?
        where_clause?
        group_clause?
        having_clause?
        window_clause?
    ;

opt_all_clause
    : ALL
    ;

distinct_on_clause
    : DISTINCT ON expr_list_in_parens
    ;

from_clause
    : FROM from_list as_of_clause?
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
    : ( relation_expr opt_index_flags?
      | select_with_parens
      | '[' row_source_extension_stmt ']'
      | LATERAL (select_with_parens | func_table)
    ) opt_ordinality? opt_alias_clause?
    | func_table opt_ordinality opt_func_alias_clause
    | '(' table_ref ')' opt_ordinality? alias_clause?
    | table_ref (
              (CROSS opt_join_hint? | NATURAL (join_type opt_join_hint?)?) JOIN table_ref
              | (join_type opt_join_hint?)? JOIN table_ref join_equal
              )
    ;

row_source_extension_stmt
    : delete_stmt
    | explain_stmt
    | insert_rest
    | select_stmt
    | show_stmt
    | update_stmt
    | upsert_stmt
    ;

func_table
    : (func_expr_windowless | ROWS FROM '(' rowsfrom_list ')')
    ;

func_expr_windowless
    : func_application
    | func_expr_common_subexpr
    ;

func_expr_common_subexpr
    : COLLATION FOR '(' a_expr ')'
    | IF '(' a_expr ',' a_expr ',' a_expr ')'
    | (NULLIF | IFNULL) '(' a_expr ',' a_expr ')'
    | IFERROR '(' a_expr ',' a_expr ',' a_expr ')'
    | ISERROR '(' a_expr ',' a_expr ')'
    | CAST '(' a_expr AS cast_target ')'
    | ANNOTATE_TYPE '(' a_expr ',' typename ')'
    | COALESCE '(' expr_list ')'
    | CURRENT_DATE
    | CURRENT_SCHEMA
    | CURRENT_CATALOG
    | CURRENT_TIMESTAMP
    | CURRENT_TIME
    | LOCALTIMESTAMP
    | LOCALTIME
    | CURRENT_USER
    | CURRENT_ROLE
    | SESSION_USER
    | USER
    | special_function
    ;

cast_target
    : typename
    ;

special_function
    : (CURRENT_DATE | CURRENT_SCHEMA | CURRENT_USER | SESSION_USER) '(' ')'
    | (EXTRACT | EXTRACT_DURATION) '(' extract_list ')'
    | OVERLAY '(' overlay_list ')'
    | POSITION '(' position_list? ')'
    | SUBSTRING '(' substr_list ')'
    | (GREATEST | LEAST) expr_list_in_parens
    | (CURRENT_TIMESTAMP | CURRENT_TIME | LOCALTIMESTAMP | LOCALTIME) '(' a_expr ')'
    | TRIM '(' (BOTH | LEADING | TRAILING)? trim_list ')'
    ;

extract_list
    : extract_arg FROM a_expr
    | expr_list
    ;

extract_arg
    : identifier_
    | YEAR
    | MONTH
    | DAY
    | HOUR
    | MINUTE
    | SECOND
    | sconst
    ;

overlay_list
    : a_expr overlay_placing substr_from substr_for?
    | expr_list
    ;

position_list
    : b_expr IN b_expr
    ;

substr_list
    : a_expr (substr_from substr_for? | substr_for substr_from?)
    | expr_list
    ;

trim_list
    : (a_expr? FROM)? expr_list
    ;

overlay_placing
    : PLACING a_expr
    ;

substr_from
    : FROM a_expr
    ;

substr_for
    : FOR a_expr
    ;

rowsfrom_list
    : rowsfrom_item (',' rowsfrom_item)*
    ;

rowsfrom_item
    : func_expr_windowless opt_func_alias_clause?
    ;

join_type
    : (FULL | LEFT | RIGHT) join_outer?
    | INNER
    ;

join_outer
    : OUTER
    ;

opt_join_hint
    : HASH
    | MERGE
    | LOOKUP
    | INVERTED
    | STRAIGHT
    ;

join_equal
    : USING '(' name_list ')'
    | ON a_expr
    ;

opt_ordinality
    : WITH ORDINALITY
    ;

opt_alias_clause
    : alias_clause
    ;

alias_clause
    : AS? table_alias_name opt_col_def_list_no_types?
    ;

opt_func_alias_clause
    : func_alias_clause
    ;

func_alias_clause
    : alias_clause
    | (AS table_alias_name? | table_alias_name) '(' col_def_list ')'
    ;

col_def_list
    : col_def (',' col_def)*
    ;

col_def
    : name typename
    ;

//set_operation
//    : select_clause (UNION | INTERSECT | EXCEPT) all_or_distinct? select_clause
//    ;

all_or_distinct
    : ALL
    | DISTINCT
    ;

//select_clause
//    : simple_select
//    | select_with_parens
//    ;

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
    : SKIP_ LOCKED
    | NOWAIT
    ;

preparable_set_stmt
    : set_session_stmt
    | set_local_stmt
    | set_csetting_stmt
    | use_stmt
    ;

set_session_stmt
    : SET (SESSION (set_rest_more | CHARACTERISTICS AS TRANSACTION transaction_mode_list) | set_rest_more)
    ;

set_rest_more
    : set_rest
    ;

set_rest
    : generic_set
    ;

generic_set
    : var_name to_or_eq var_list
    ;

var_list
    : var_value (',' var_value)*
    ;

var_value
    : a_expr
    | extra_var_value
    ;

extra_var_value
    : ON
    | NONE
    | cockroachdb_extra_reserved_keyword
    ;

to_or_eq
    : '='
    | TO
    ;

set_local_stmt
    : SET LOCAL set_rest
    ;

set_csetting_stmt
    : SET CLUSTER SETTING var_name to_or_eq var_value
    ;

use_stmt
    : USE var_name
    ;

show_stmt
    : show_backup_stmt
    | show_columns_stmt
    | show_constraints_stmt
    | show_triggers_stmt
    | show_create_stmt
    | show_create_schedules_stmt
    | show_create_external_connections_stmt
    | show_databases_stmt
    | show_enums_stmt
    | show_external_connections_stmt
    | show_types_stmt
    | show_functions_stmt
    | show_procedures_stmt
    | show_grants_stmt
    | show_indexes_stmt
    | show_partitions_stmt
    | show_jobs_stmt
    | show_locality_stmt
    | show_schedules_stmt
    | show_statements_stmt
    | show_ranges_stmt
    | show_range_for_row_stmt
    | show_regions_stmt
    | show_survival_goal_stmt
    | show_roles_stmt
    | show_savepoint_stmt
    | show_schemas_stmt
    | show_sequences_stmt
    | show_session_stmt
    | show_sessions_stmt
    | show_stats_stmt
    | show_tables_stmt
    | show_trace_stmt
    | show_transactions_stmt
    | show_transfer_stmt
    | show_users_stmt
    | show_default_session_variables_for_role_stmt
    | show_zone_stmt
    | show_full_scans_stmt
    | show_default_privileges_stmt
    | show_inspect_errors_stmt
    ;

show_backup_stmt
    : SHOW (
        BACKUPS IN string_or_placeholder opt_with_show_backup_options?
        | BACKUP (SCHEMAS FROM)? string_or_placeholder IN string_or_placeholder_opt_list opt_with_show_backup_options?
        )
    ;

opt_with_show_backup_options
    : WITH (
        show_backup_options_list
        | OPTIONS '(' show_backup_options_list ')'
    )
    ;

show_backup_options_list
    : show_backup_options (',' show_backup_options)*
    ;

show_backup_options
    : AS JSON
    | CHECK_FILES
    | SKIP_ SIZE
    | DEBUG_IDS
    | (INCREMENT_LOCATION | KMS) '=' string_or_placeholder_opt_list
    | (ENCRYPTION_PASSPHRASE | ENCRYPTION_INFO_DIR) '=' string_or_placeholder
    | PRIVILEGES
    ;

show_columns_stmt
    : SHOW COLUMNS FROM table_name with_comment?
    ;

with_comment
    : WITH COMMENT
    ;

show_constraints_stmt
    : SHOW (CONSTRAINT | CONSTRAINTS) FROM table_name with_comment?
    ;

show_triggers_stmt
    : SHOW TRIGGER FROM table_name
    ;

show_create_stmt
    : SHOW CREATE
        ( table_name opt_show_create_format_options
        | ALL (SCHEMAS | TABLES | TRIGGERS | TYPES | ROUTINES)
        )
    ;

opt_show_create_format_options
    : WITH (REDACT | IGNORE_FOREIGN_KEYS)
    ;

show_create_schedules_stmt
    : SHOW CREATE (ALL SCHEDULES | SCHEDULE a_expr)
    ;

show_create_external_connections_stmt
    : SHOW CREATE (ALL EXTERNAL CONNECTIONS | EXTERNAL CONNECTION string_or_placeholder)
    ;

show_databases_stmt
    : SHOW DATABASES with_comment?
    ;

show_enums_stmt
    : SHOW ENUMS (FROM name ('.' name)?)?
    ;

show_external_connections_stmt
    : SHOW EXTERNAL (CONNECTIONS | CONNECTION string_or_placeholder)
    ;

show_types_stmt
    : SHOW TYPES with_comment?
    ;

show_functions_stmt
    : SHOW FUNCTIONS (FROM name ('.' name)?)?
    ;

show_procedures_stmt
    : SHOW PROCEDURES (FROM name ('.' name)?)?
    ;

show_grants_stmt
    : SHOW (GRANTS opt_target_roles? | SYSTEM GRANTS) for_grantee_clause?
    ;

opt_target_roles
    : ON target_roles
    ;

target_roles
    : ROLE role_spec_list
    | SCHEMA (schema_name_list | schema_wildcard)
    | TYPE type_name_list
    | grant_targets
    ;

schema_wildcard
    : wildcard_pattern
    ;

wildcard_pattern
    : name '.' '*'
    ;

for_grantee_clause
    : FOR role_spec_list
    ;

show_indexes_stmt
    : SHOW (INDEX | INDEXES | KEYS) FROM (table_name | DATABASE database_name) with_comment?
    ;

show_partitions_stmt
    : SHOW PARTITIONS FROM (TABLE table_name | DATABASE database_name | INDEX (table_index_name | table_name '@' '*'))
    ;

show_jobs_stmt
    : SHOW (
        AUTOMATIC JOBS
        | JOBS (select_stmt? (WITH show_job_options_list)? | WHEN COMPLETE select_stmt | for_schedules_clause)
        | CHANGEFEED (JOBS select_stmt? | JOB a_expr)
        | JOB (a_expr (WITH show_job_options_list)? | WHEN COMPLETE a_expr)
    )
    ;

show_job_options_list
    : show_job_options (',' show_job_options)*
    ;

show_job_options
    : EXECUTION DETAILS
    ;

show_locality_stmt
    : SHOW LOCALITY
    ;

show_schedules_stmt
    : SHOW (schedule_state? SCHEDULE opt_schedule_executor_type | SCHEDULE a_expr)
    ;

schedule_state
    : RUNNING
    | PAUSED
    ;

opt_schedule_executor_type
    : FOR (BACKUP | SQL STATISTICS | CHANGEFEED)
    ;

show_statements_stmt
    : SHOW ALL? opt_cluster statements_or_queries
    ;

opt_cluster
    : CLUSTER
    | LOCAL
    ;

statements_or_queries
    : STATEMENTS
    | QUERIES
    ;

show_ranges_stmt
    : SHOW (
        RANGES (
                FROM (
                INDEX table_index_name
                | TABLE table_name
                | DATABASE database_name
                | CURRENT_CATALOG
                )
              )?
        | CLUSTER RANGES
        )
        opt_show_ranges_options
    ;

opt_show_ranges_options
    : WITH show_ranges_options
    ;

show_ranges_options
    : show_ranges_option (',' show_ranges_option)*
    ;

show_ranges_option
    : TABLES
    | INDEXES
    | DETAILS
    | KEYS
    | EXPLAIN
    ;

show_range_for_row_stmt
    : SHOW RANGE FROM (TABLE table_name | INDEX table_index_name) FOR ROW expr_list_in_parens
    ;

show_regions_stmt
    : SHOW (
        REGIONS (FROM (CLUSTER | DATABASE database_name? | ALL DATABASES))?
        | SUPER REGIONS FROM DATABASE database_name
        )
    ;

show_survival_goal_stmt
    : SHOW SURVIVAL GOAL FROM DATABASE database_name?
    ;

show_roles_stmt
    : SHOW ROLES
    ;

show_savepoint_stmt
    : SHOW SAVEPOINT STATUS
    ;

show_schemas_stmt
    : SHOW SCHEMAS (FROM name)? with_comment?
    ;

show_sequences_stmt
    : SHOW SEQUENCES (FROM name)?
    ;

show_session_stmt
    : SHOW SESSION? session_var
    ;

show_sessions_stmt
    : SHOW ALL opt_cluster SESSIONS
    ;

show_stats_stmt
    : SHOW STATISTICS FOR_TABLE TABLE table_name opt_with_options?
    ;

show_tables_stmt
    : SHOW TABLE (FROM name ('.' name)?)? with_comment?
    ;

show_trace_stmt
    : SHOW opt_compact? KV? TRACE FOR SESSION
    ;

opt_compact
    : COMPACT
    ;

show_transactions_stmt
    : SHOW ALL? opt_cluster TRANSACTIONS
    ;

show_transfer_stmt
    : SHOW TRANSFER STATE (WITH sconst)?
    ;

show_users_stmt
    : SHOW USERS
    ;

show_default_session_variables_for_role_stmt
    : SHOW DEFAULT SESSION VARIABLES FOR
        (role_or_group_or_user role_spec | (ROLE_ALL | USER_ALL) ALL)
    ;

show_zone_stmt
    : SHOW (
        ZONE (
            CONFIGURATION FROM (
                RANGE zone_name
                | DATABASE database_name
                | table_or_index opt_partition?
                | PARTITION partition_name OF table_or_index
                )
            | CONFIGURATIONS
        )
        | ALL ZONE CONFIGURATIONS
        )
    ;

zone_name
    : unrestricted_name
    ;

opt_partition
    : partition
    ;

partition
    : PARTITION partition_name
    ;

partition_name
    : unrestricted_name
    ;

table_or_index
    : TABLE table_name
    | INDEX table_index_name
    ;

show_full_scans_stmt
    : SHOW FULL TABLE SCANS
    ;

show_default_privileges_stmt
    : SHOW DEFAULT PRIVILEGES (opt_for_roles? | FOR (GRANTEE role_spec_list | ALL ROLES))
        opt_in_schema?
    ;

opt_for_roles
    : FOR role_or_group_or_user role_spec_list
    ;

opt_in_schema
    : IN SCHEMA schema_name
    ;

schema_name
    : name
    ;

show_inspect_errors_stmt
    : SHOW INSPECT ERRORS opt_for_table_clause? opt_for_job_clause? opt_with_details?
    ;

opt_for_table_clause
    : FOR_TABLE TABLE table_name
    ;

opt_for_job_clause
    : FOR_JOB JOB iconst
    ;

opt_with_details
    : WITH DETAILS
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

bconst
    : 'b' STRING_LITERAL
    ;


bitconst
    : BINARY_LITERAL
    ;
//

expr_list_in_parens
    : '(' expr_list ')'
    ;

expr_list
    : a_expr (',' a_expr)*
    ;

a_expr
    : (c_expr
    | ('+' | '-' | '~' | SQRT | CBRT | qual_op | NOPT) a_expr
    | row OVERLAPS row
    | DEFAULT)
        ( TYPECAST cast_target
        | TYPEANNOTATE typename
        | COLLATE collation_name
        | AT TIME ZONE a_expr
        //TODO https://www.cockroachlabs.com/docs/stable/sql-grammar#a_expr
        )+
    ;

b_expr
    : (c_expr | ('+' | '-' | '~' | qual_op) b_expr)
        ( TYPECAST cast_target
        | TYPEANNOTATE typename
        //TODO https://www.cockroachlabs.com/docs/stable/sql-grammar#b_expr
        ) +
    ;

qual_op
    : OPERATOR '(' operator_op ')'
    ;

operator_op
    : all_op
    ;

all_op
    : '+'
    | '-'
    | '*'
    | '/'
    | '%'
    | '^'
    | '<'
    | '>'
    | '='
    | '<='
    | '>='
    | '<>'
    | '?'
    | '&'
    | '|'
    | '#'
    //TODO https://www.cockroachlabs.com/docs/stable/sql-grammar#all_op
    ;

c_expr
    : d_expr array_subscripts?
    | case_expr
    | EXISTS select_with_parens
    ;

d_expr
    : '@' iconst
    | fconst
    | sconst
    | bconst
    | bitconst
    | typed_literal
    | interval_value
    | TRUE
    | FALSE
    | NULL_
    | column_path_with_star
    | PLACEHOLDER
    | '(' a_expr ')' ('.' ('*' | unrestricted_name | '@' iconst) )?
    | func_expr
    | select_with_parens
    | labeled_row
    | ARRAY (select_with_parens | row | array_expr)
    ;

typed_literal
    : (func_name_no_crdb_extra | const_typename) sconst
    ;

func_name_no_crdb_extra
    : type_function_name_no_crdb_extra
    | prefixed_column_path
    ;

interval_value
    : INTERVAL (sconst interval_qualifier? | '(' iconst ')' sconst )
    ;

column_path_with_star
    : column_path
    | db_object_name_component '.' (unrestricted_name '.' (unrestricted_name '.')?)? '*'
    ;

func_expr
    : func_application within_group_clause? filter_clause? over_clause?
    | func_expr_common_subexpr
    ;

array_expr
    : '[' (expr_list? | array_expr_list) ']'
    ;

array_expr_list
    : array_expr (',' array_expr)*
    ;

within_group_clause
    : WITHIN GROUP '(' single_sort_clause ')'
    ;

over_clause
    : OVER (window_specification | window_name)
    ;

filter_clause
    : FILTER '(' WHERE a_expr ')'
    ;

single_sort_clause
    : ORDER BY (sortby (',' sortby_list)? | sortby_index ',' sortby_list)
    ;

labeled_row
    : row
    | '(' row AS name_list ')'
    ;

row
    : ROW '(' expr_list? ')'
    | expr_tuple_unambiguous
    ;

expr_tuple_unambiguous
    : '(' tuple1_unambiguous_values ')'
    ;

tuple1_unambiguous_values
    : a_expr ',' expr_list?
    ;

case_expr
    : CASE case_arg? when_clause_list case_default? END
    ;

case_arg
    : a_expr
    ;

when_clause_list
    : when_clause+
    ;

when_clause
    : WHEN a_expr THEN a_expr
    ;

case_default
    : ELSE a_expr
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
    | DOMAIN_
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
    | LC_COLLATE_
    | LC_CTYPE_
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
    | NAN_
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
    | SKIP_
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
    | INT_
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

collation_name
    : unrestricted_name
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

statistics_name
    : name
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

family_name
    : name
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
    | NULL_
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
