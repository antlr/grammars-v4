parser grammar TeradataSQLDDLParser;

import TeradataSQLDMLParser
     , TeradataSQLDCLParser
     , TeradataSQLExpressionsParser
     , TeradataSQLLiteralsParser
     , TeradataSQLIdentifiersParser
     , TeradataSQLRequestModifiersParser
     , TeradataSQLNonReservedWordsParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

ddl_stat
    : //alter_constraint_stat //TODO
    /*|*/ alter_foreign_server_stat
    | alter_function_stat
    | alter_join_index_stat
    | alter_hash_index_stat
    //| alter_method_stat //TODO
    |  alter_table_stat
    //| alter_trigger_stat //TODO
    | alter_type_stat
    //| alter_zone_stat //TODO
    | begin_isolated_loading_stat
    | begin_logging_stat
    | begin_query_capture_stat
    | begin_query_logging_stat
    | checkpoint_isolated_loading_stat
    | collect_statistics_optimizer_form_stat
    | comment_placing_stat
    | create_replace_authorization_stat
    //| create_replace_cast_stat //TODO
    | create_replace_function_stat
    //| create_replace_function_mapping_stat //TODO
    | create_replace_macro_stat
    //| create_replace_method_stat //TODO
    //| create_replace_ordering_stat //TODO
    | create_replace_procedure_stat
    //| create_replace_replication_ruleset_stat //TODO
    //| create_replace_transform_stat //TODO
    //| create_replace_trigger_stat //TODO
    | create_replace_view_stat
    //| create_constraint_stat //TODO
    | create_database_stat
    | create_foreign_server_stat
    //| create_glop_set_stat //TODO
    | create_hash_index_stat
    | create_index_stat
    | create_join_index_stat
    //| create_map_stat //TODO
    | create_profile_stat
    //| create_replication_group_stat //TODO
    | create_role_stat
    | create_table_stat
    | create_type_stat
    | create_user_stat
    //| create_zone_stat //TODO
    | database_stat
    | delete_database_stat
    | delete_user_stat
    | drop_authorization_stat
    | drop_cast_stat
    | drop_constraint_stat
    | drop_database_stat
    | drop_error_table_stat
    | drop_foreign_server_stat
    | drop_function_stat
    | drop_function_mapping_stat
    | drop_glop_set_stat
    | drop_hash_index_stat
    | drop_index_stat
    | drop_join_index_stat
    | drop_macro_stat
    | drop_map_stat
    | drop_method_stat
    | drop_ordering_stat
    | drop_procedure_stat
    | drop_profile_stat
    | drop_replication_group_stat
    | drop_replication_ruleset_stat
    | drop_role_stat
    | drop_schema_stat
    | drop_statistics_optimizer_form_stat
    | drop_table_stat
    | drop_transform_stat
    | drop_trigger_stat
    | drop_type_stat
    | drop_user_stat
    | drop_view_stat
    | drop_zone_stat
    | end_isolated_loading_stat
    | end_logging_stat
    | end_query_capture_stat
    | end_query_logging_stat
    | flush_query_logging_stat
    | help_stat
    | help_statistics_optimimizer_form_stat
    | help_statistics_qcd_form_stat
    | incremental_restore_allow_write_stat
    | logging_incremental_archive_off_stat
    | logging_incremental_archive_on_stat
    | modify_database_stat
    | modify_profile_stat
    | modify_user_stat
    | rename_function_stat
    | rename_procedure_stat
    | rename_macro_stat
    | rename_table_stat
    | rename_trigger_stat
    | rename_view_stat
    | replace_query_logging_stat
    | set_session_stat
    | set_role_stat
    | set_query_band_stat
    | show_object_stat
    | show_query_logging_stat
    | show_request_stat
    | show_statistics_optimizer_form_stat
    | show_statistics_qcd_form_stat
    ;

/*********************************
    ALTER FOREIGN SERVER statement
*/
alter_foreign_server_stat
    : ALTER FOREIGN SERVER td_server_db? server_name=unqualified_name
        foreign_server_external_security_clause?
        (foreign_server_add_clause|foreign_server_drop_clause) (',' foreign_server_add_clause|foreign_server_drop_clause)*
    ;

foreign_server_add_clause
    : ADD ( foreign_server_using_option
          | (IMPORT|EXPORT) (foreign_server_using_clause|foreign_server_with_clause)
          )
    ;

foreign_server_drop_clause
    : DROP ( foreign_server_option_name
           | (IMPORT|EXPORT) (USING foreign_server_option_name+)?
           )
    ;

/***************************
    ALTER FUNCTION statement
*/
alter_function_stat
    : ALTER ( SPECIFIC FUNCTION function_name
            | FUNCTION function_name ( '(' (variable_data_type (',' variable_data_type)* )? ')' )?
            )
      (EXECUTE NOT? PROTECTED|COMPILE ONLY?)
    ;

/*****************************
    ALTER JOIN INDEX statement
*/
alter_join_index_stat : ALTER JOIN INDEX join_index_name=table_name map_spec ;

/*****************************
    ALTER HASH INDEX statement
*/
alter_hash_index_stat : ALTER HASH INDEX hash_index_name=table_name map_spec ;

/************************
    ALTER TABLE statement
*/
alter_table_stat
    : alter_table_basic_stat
    | alter_table_join_index_stat
    | alter_table_revalidation_stat
    | alter_table_release_rows_stat
    | alter_table_map_and_collocation_form_stat
    | alter_foreign_table_stat
    | alter_table_to_current_stat
    ;

alter_table_basic_stat
    : ALTER TABLE table_name
      ( alter_option (',' alter_option)*
      | table_option_alter_form
      | normalize_option
      | DROP NORMALIZE
      | modify_primary
      | MODIFY NO PRIMARY INDEX? alter_partitioning?
      | MODIFY alter_partitioning
      | FROM TIME ZONE '=' ('+'|'-')? quotestring=char_string_literal (',' TIMEDATEWZCONTROL '=' n=integer_literal)? (',' WITH TIME ZONE)?
      | (SET|RESET) DOWN
      )
    ;

alter_table_join_index_stat : ALTER TABLE join_index_name=table_name join_index_add_option (',' join_index_add_option)* ;

alter_table_revalidation_stat : NONTEMPORAL? ALTER TABLE table_name REVALIDATE (WITH (INSERT INTO? save_table_name=table_name|DELETE))?;

alter_table_release_rows_stat : ALTER TABLE table_name RELEASE DELETED? ROWS (AND RESET LOAD IDENTITY)? ;

alter_table_map_and_collocation_form_stat : ALTER TABLE table_name ',' map_spec ;

alter_foreign_table_stat
    : ALTER FOREIGN TABLE table_name
      (',' table_option)*
      (',' foreign_table_external_security_clause)?
      (alter_foreign_column_option (',' alter_foreign_column_option)* )?
      ( '('
            location_column
          ',' (payload_column| column_definition (',' column_definition)* )
         ')' )?
      (UPDATE (location|foreign_table_option)* )?
    ;

alter_foreign_column_option
    : ADD add_option
    | DROP name=unqualified_name IDENTITY?
    | RENAME old_name=unqualified_name (AS|TO) new_name=unqualified_name
    ;

alter_table_to_current_stat : ALTER TABLE table_name TO CURRENT (WITH (INSERT INTO? save_table=table_name|DELETE) )? ;


alter_option
    : ADD add_option
    | MODIFY (CONSTRAINT? constraint_name=unqualified_name)? CHECK '(' logical_expr ')'
    | RENAME old_name=unqualified_name (AS|TO) new_name=unqualified_name
    | DROP drop_option
    ;

table_option_alter_form
    : fallback_protection
    | WITH JOURNAL TABLE '=' journal_table_name=table_name
    | before_journal
    | ON COMMIT (DELETE|PRESERVE) ROWS
    | NO? LOG
    | (NO|DUAL|LOCAL|NOT LOCAL)? AFTER JOURNAL
    | CHECKSUM '=' (DEFAULT|ON|OFF)
    | FREESPACE '=' integer_literal PERCENT?
    | datablocksize IMMEDIATE
    | mergeblockratio
    | block_compression
    | with_isolated_loading_alter_form
    ;

add_option
    : (alter_column_spec| '(' alter_column_spec (',' alter_column_spec)* ')' ) (INTO into_name=unqualified_name)?
    | (COLUMN|ROW|SYSTEM)? '(' (name+=unqualified_name|alter_column_spec) (',' (name+=unqualified_name|alter_column_spec) )* ')' (NO? AUTO COMPRESS)?
    | '(' name+=unqualified_name ')' NO? AUTO COMPRESS
    | PERIOD FOR period_name=unqualified_name '(' period_begin_column=unqualified_name ',' period_end_column=unqualified_name ')'
    | table_constraint
    | row_level_security_constraint_column_name+=unqualified_name (',' row_level_security_constraint_column_name+=unqualified_name)* CONSTRAINT
    ;

drop_option
    : PERIOD FOR period_name=unqualified_name
    | name=unqualified_name IDENTITY?
    | CONSTRAINT name=unqualified_name
    | (CONSTRAINT name=unqualified_name)? FOREIGN KEY '(' fk_column_name+=unqualified_name (',' fk_column_name+=unqualified_name)* ')' references
    | (CONSTRAINT? name=unqualified_name)? CHECK
    | INCONSISTENT REFERENCES
    | row_level_security_constraint_column_name+=unqualified_name (',' row_level_security_constraint_column_name+=unqualified_name)* CONSTRAINT
    ;

modify_primary
    : MODIFY (NOT? UNIQUE)? PRIMARY AMP? INDEX?
      (index_name=unqualified_name|NOT NAMED)?
      ( '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')' )?
      alter_partitioning?
    ;

alter_partitioning
    : ( PARTITION BY (partitioning_level|'(' partitioning_level (',' partitioning_level)* ')' )
      | add_drop_range_option (',' add_drop_range_option)*
      ) (WITH (INSERT INTO? save_table_name=table_name|DELETE) )?
    | NOT PARTITIONED
    ;

add_drop_range_option
    : DROP alter_range_expr (ADD alter_range_expr)?
    | ADD alter_range_expr
    ;

alter_column_spec : name=unqualified_name ( data_type
                                          | data_type column_attribute+
                                          | column_attribute+ data_type column_attribute*
                                          | column_attribute+
                                          )
                  ;

alter_range_expr
    : (RANGE|RANGE_L) ( BETWEEN range_expr_3 (',' range_expr_3)*
                        ( ',' NO RANGE ((OR|',') UNKNOWN)?
                        | ',' UNKNOWN
                        )?
                      | NO RANGE ((OR|',') UNKNOWN)?
                      | UNKNOWN
                      | WHERE logical_expr
                      )
    ;

with_isolated_loading_alter_form : with_isolated_loading (FOR (ALL|INSERT|NONE))? (USING FAST MODE (ON|OFF))? ;

join_index_add_option
    : ADD ( (COLUMN|ROW|SYSTEM) '(' name=unqualified_name ')' (NO? AUTO COMPRESS)?
          | '(' name=unqualified_name ')' NO? AUTO COMPRESS
          )
    ;

/***********************
    ALTER TYPE statement
*/
alter_type_stat
    : ALTER TYPE sysudtlib? udt_name
        ( add_attribute_clause
        | add_method_clause
        | add_specific_method_clause
        | drop_attribute_clause
        | drop_method_clause
        | drop_specific_method_clause
        | COMPILE ONLY?
        )
    ;

add_attribute_clause : ADD ATTRIBUTE type_attribute_spec (',' type_attribute_spec)*;

add_method_clause : ADD (INSTANCE|CONSTRUCTOR)? METHOD add_method_spec (',' add_method_spec)* ;

add_specific_method_clause : ADD SPECIFIC METHOD add_specific_method_spec (',' add_specific_method_spec)* ;

drop_attribute_clause : DROP ATTRIBUTE attribute_name (',' attribute_name)* ;

drop_method_clause : DROP (INSTANCE|CONSTRUCTOR)? METHOD sysudtlib? method_name (',' sysudtlib? method_name)* ;

drop_specific_method_clause : DROP SPECIFIC METHOD sysudtlib? method_name (',' sysudtlib? method_name)* ;

add_method_spec
    : sysudtlib? method_name '(' ( data_type (',' data_type)* )? ')'
              RETURNS returns_parameter_spec
              (CAST FROM cast_from_data_type=data_type )?
              (SPECIFIC sysudtlib? specific_method_name=method_name)?
              (method_attr* method_language_spec method_attr+|method_attr+ method_language_spec method_attr*)
    ;

add_specific_method_spec
    : sysudtlib? specific_method_name=method_name ( '(' data_type (',' data_type)* ')' )? FOR sysudtlib? udt_name
    ;

/***********************************
    BEGIN ISOLATED LOADING statement
*/
begin_isolated_loading_stat
    : BEGIN CONCURRENT? ISOLATED LOADING ON table_name (',' table_name)*
      USING QUERY_BAND query_band=char_string_literal // 'LDILoadGroup=value;'
      (IN? (SINGLE|MULTIPLE) SESSION)?
    ;

/**************************
    BEGIN LOGGING statement
*/
begin_logging_stat
    : BEGIN LOGGING DENIALS? (WITH TEXT)?
      ON logging_frequency
      (FOR CONSTRAINT constraint_name=object_name)?
      (ALL|operation (',' operation)*)
      (FOR CONSTRAINT constraint_name=object_name)?
      (BY user_name (',' user_name)* )?
      (ON logging_item (',' logging_item)* )?
    ;

operation
    : ALTER EXTERNAL PROCEDURE
    | ALTER FUNCTION
    | ALTER PROCEDURE
    | CHECKPOINT
    | CREATE AUTHORIZATION
    | CREATE DATABASE
    | CREATE EXTERNAL PROCEDURE
    | CREATE FUNCTION
    | CREATE GLOP SET
    | CREATE MACRO
    | CREATE PROCEDURE
    | CREATE PROFILE
    | CREATE ROLE
    | CREATE TABLE
    | CREATE TRIGGER
    | CREATE USER
    | CREATE VIEW
    | DATABASE
    | DROP
    | DROP AUTHORIZATION
    | DROP DATABASE
    | DROP FUNCTION
    | DROP GLOP SET
    | DROP MACRO
    | DROP PROCEDURE
    | DROP PROFILE
    | DROP ROLE
    | DROP TABLE
    | DROP TRIGGER
    | DROP USER
    | DROP VIEW
    | DUMP
    | EXECUTE
    | EXECUTE FUNCTION
    | EXECUTE PROCEDURE
    | GRANT
    | INDEX
    | MACRO
    | PROCEDURE
    | REFERENCES
    | RESTORE
    | ROLLBACK DATABASE
    | ROLLFORWARD DATABASE
    | TABLE
    | TRIGGER
    | UDTMETHOD
    | UDTTYPE
    | UDTUSAGE
    | USER
    | VIEW
    | DELETE
    | INSERT
    | OVERRIDE DELETE
    | OVERRIDE DUMP
    | OVERRIDE INSERT
    | OVERRIDE RESTORE
    | OVERRIDE SELECT
    | OVERRIDE UPDATE
    | SELECT
    | UPDATE
    ;

logging_frequency : FIRST | LAST | FIRST AND LAST | EACH ;

logging_item
    : AUTHORIZATION authorization_name=object_name
    | DATABASE database_name
    | USER user_name
    | TABLE table_name
    | VIEW view_name=table_name
    | MACRO macro_name
    | PROCEDURE procedure_name
    | FUNCTION function_name
    | FUNCTION MAPPING function_mapping_name=object_name
    | TYPE udt_type
    ;

/********************************
    BEGIN QUERY CAPTURE statement
*/
begin_query_capture_stat
    : BEGIN QUERY CAPTURE
      (FOR INDEX ANALYSIS)?
      (WITH (VERBOSE|DETAILED? STATSUSAGE) (',' (VERBOSE|DETAILED? STATSUSAGE))? )?
      (INTO qcd_name=database_name)?
      AS WORKLOAD workload_name=unqualified_name
    ;

/********************************
    BEGIN QUERY LOGGING statement
*/
begin_query_logging_stat
    : BEGIN QUERY LOGGING
      (WITH query_logging_with_item (',' query_logging_with_item)* )?
      (MODE '=' m=integer_literal)? // 0, 1, 2, 3 (default)
      (LIMIT query_logging_limit_item (AND query_logging_limit_item)? )?
      ON query_logging_on_items
    ;

query_logging_with_item
    : ALL
    | EXPLAIN
    | LOCK '=' lock_duration=integer_literal
    | NONE
    | (NO COLUMNS)? OBJECTS
    | PARAMINFO
    | FEATUREINFO
    | SQL
    | DETAILED? STATSUSAGE
    | STEPINFO
    | USECOUNT
    | UTILITYINFO
    | VERBOSE? XMLPLAN
    ;

query_logging_limit_item
    : SQLTEXT ('=' sqltext_n=integer_literal)
    | ( SUMMARY '=' summary_n1=integer_literal ',' summary_n2=integer_literal ',' summary_n3=integer_literal
      | THRESHOLD ('=' threshold_n=integer_literal)
      ) (CPUTIME|CPUTIMENORM|ELAPSEDSEC|ELAPSEDTIME|IOCOUNT)?
    ;

query_logging_on_items
    : query_logging_on_all
    | query_logging_on_users
    | query_logging_on_application
    ;

query_logging_on_all : ALL account_spec? ;

query_logging_on_users : user_name (',' user_name)* account_spec? ;

query_logging_on_application
    : APPLNAME '=' (application_name+=char_string_literal|'('application_name+=char_string_literal (',' application_name+=char_string_literal)* ')')
    ;

account_spec
    : ACCOUNT '=' (account_string+=char_string_literal|'('account_string+=char_string_literal (',' account_string+=char_string_literal)* ')')
    ;

/****************************************
    CHECKPOINT ISOLATED LOADING statement
*/
checkpoint_isolated_loading_stat : CHECKPOINT CONCURRENT? ISOLATED LOADING FOR QUERY_BAND query_band=char_string_literal ; // 'LDILoadGroup=value;'

/**********************************************
    COLLECT STATISTICS OPTIMIZER FORM statement
*/
collect_statistics_optimizer_form_stat
    : COLLECT SUMMARY? (STATISTICS|STAT|STATS)
      (USING using_option (AND using_option)* )?
      ( ON? collection_source (stats_target_spec (',' stats_target_spec)* )?
      | (stats_target_spec (',' stats_target_spec)* )? ON collection_source
      )
      (FROM from_stats_option)?
    ;

using_option
    : ( SAMPLE
      | SYSTEM SAMPLE
      | SAMPLE n=integer_literal PERCENT
      | NO SAMPLE
      | (SYSTEM THRESHOLD|THRESHOLD n=integer_literal|NO THRESHOLD) (PERCENT|DAYS)?
      | MAXINTERVALS n=integer_literal
      | SYSTEM MAXINTERVALS
      | MAXVALUELENGTH n=integer_literal
      | SYSTEM MAXVALUELENGTH
      )
      (FOR CURRENT)?
    ;

stats_target_spec : UNIQUE? PRIMARY? INDEX stats_index_spec|COLUMN stats_column_spec ;

stats_index_spec
    : index_name=table_name
    | (index_name=table_name)? ALL? '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')'
      index_ordering?
    ;

stats_column_spec
    : scalar_expr (AS? statistics_name)?
    | '(' (scalar_expr|PARTITION) (',' (scalar_expr|PARTITION))* ')' (AS? statistics_name)?
    | PARTITION
    | statistics_name
    ;

collection_source : TEMPORARY? table_name ;

from_stats_option
    : ( TEMPORARY table_name
      | index_name=table_name
      )
      ( COLUMN ( (from_column_name=unqualified_name|partition_column=PARTITION)
               | statistics_name
               | '(' (from_column_name=unqualified_name|partition_column=PARTITION) (',' (unqualified_name|partition_column=PARTITION) )* ')'
               ) )?
    ;

/****************************
    COMMENT placing statement
*/
comment_placing_stat
    : COMMENT ON? (object_kind? object_name|COLUMN column_name|column_name)
      (AS|IS)? (comment=char_string_literal|unicode_comment=unicode_char_string_literal|hexstring_comment=hex_char_string_literal)
    ;

/*****************************************
    CREATE/REPLACE AUTHORIZATION statement
*/
create_replace_authorization_stat
    : (CREATE|REPLACE) AUTHORIZATION authorization_name=object_name
      (AS (DEFINER|INVOKER) TRUSTED)?
      USER authorization_user_name=char_string_literal
      PASSWORD authorization_password=char_string_literal
    ;

/************************************
    CREATE/REPLACE FUNCTION statement
*/
create_replace_function_stat
    : create_replace_sql_function_stat
    | create_replace_table_function_stat
    | create_replace_external_function_stat
    ;

create_replace_sql_function_stat
    : (CREATE|REPLACE) FUNCTION function_name '(' (sql_function_parameter_spec (',' sql_function_parameter_spec)* )? ')'
      RETURNS return_data_type=data_type (sql_function_language_spec|sql_function_access_spec)* sql_function_attr* (sql_function_language_spec|sql_function_access_spec)* sql_function_attr*
      (SQL SECURITY DEFINER)? COLLATION INVOKER INLINE TYPE UNSIGNED_INTEGER /* 1 */
      RETURN return_expr=scalar_expr
    ;

create_replace_table_function_stat
    : (CREATE|REPLACE) FUNCTION function_name '(' (table_function_parameter_spec (',' table_function_parameter_spec)* )? ')'
      RETURNS TABLE table_spec
        (table_function_language_spec|no_sql)* table_function_attr* (table_function_language_spec|no_sql)* table_function_attr*
      (USING (GLOP SET)? glob_set_name=object_name)?
      EXTERNAL (NAME ( external_function_name=function_name
                     | code_or_jar_spec=char_string_literal
                     ) )?
      table_function_parameter_style?
      (EXTERNAL SECURITY (DEFINER authorization_name=object_name?|INVOKER) )?
      (EXECUTE map_spec )?
    ;

create_replace_external_function_stat
    : (CREATE|REPLACE) FUNCTION function_name '(' (external_function_parameter_spec (',' external_function_parameter_spec)* )? ')'
      RETURNS return_data_type=external_function_data_type (CAST FROM from_data_type=external_function_data_type)?
        (external_function_language_spec|no_sql)* external_function_attr* (external_function_language_spec|no_sql)* external_function_attr*
      (USING GLOP SET glob_set_name=object_name)?
      EXTERNAL (NAME ( external_function_name=function_name
                     | code_or_jar_spec=char_string_literal
                     ) )?
         external_function_parameter_style?
         (EXTERNAL SECURITY (DEFINER authorization_name=object_name?|INVOKER) )?
    ;

// for SQL functions
sql_function_parameter_spec : parameter_name variable_data_type ;

sql_function_language_spec : LANGUAGE SQL ;

sql_function_access_spec : CONTAINS SQL ;

sql_function_attr
    : SPECIFIC function_name
    | NOT? DETERMINISTIC
    | CALLED ON NULL INPUT
    | RETURNS NULL ON NULL INPUT
    ;

// for table functions
table_spec
    : '(' column_spec (',' column_spec)* ')'
    | VARYING ( COLUMNS '(' maximum_output_columns=UNSIGNED_INTEGER ')'
              | USING FUNCTION function_name?
              )
    ;

table_function_parameter_spec : parameter_name? external_function_data_type ;

table_function_language_spec : LANGUAGE (C|CPP|JAVA|SAS) ;

table_function_attr
    : SPECIFIC function_name
    | NOT? DETERMINISTIC
    | CALLED ON NULL INPUT
    | table_function_parameter_style
    ;

table_function_parameter_style : PARAMETER STYLE (SQL|JAVA|SQLTABLE) ;

// for external functions
external_function_parameter_spec : parameter_name? external_function_data_type ;

external_function_language_spec : LANGUAGE (C|CPP|JAVA) ;

external_function_attr
    : SPECIFIC function_name
    | CLASS (AGGREGATE|AG) ( '(' interim_size=UNSIGNED_INTEGER ')' )?
    | external_function_parameter_style
    | NOT? DETERMINISTIC
    | CALLED ON NULL INPUT
    | RETURNS NULL ON NULL INPUT
    | FOR (COMPRESS|DECOMPRESS)
    ;

external_function_parameter_style : PARAMETER STYLE (SQL|JAVA|TD_GENERAL) ;

// shared
no_sql : NO SQL ;


/*********************************
    CREATE/REPLACE MACRO statement
*/
create_replace_macro_stat
    : (CREATE MACRO|CM|REPLACE MACRO) macro_name
      ( '(' macro_parameter (',' macro_parameter)* ')' )?
      AS '(' ( using_request_modifier? locking_request_modifier? (dml_stat|ddl_stat|dcl_stat) ';' )+ ')'
    ;

macro_parameter : parameter_name (data_type|'(' data_type (',' data_type_attribute)* ')') data_type_attribute* ;

/*************************************
    CREATE/REPLACE PROCEDURE statement
*/
create_replace_procedure_stat : create_replace_procedure_sql_form_stat ;

create_replace_procedure_sql_form_stat
    : (CREATE|REPLACE) PROCEDURE procedure_name '(' (parameter_spec (',' parameter_spec)*)? ')'
      (sql_data_access_option|dynamic_result_sets|sql_security_option)*
      procedure_body
    ;

parameter_spec : (IN|OUT|INOUT)? parameter_name variable_data_type data_type_attribute* ;

sql_data_access_option : CONTAINS SQL | MODIFIES SQL DATA | READS SQL DATA ;

dynamic_result_sets : DYNAMIC RESULT SETS number_of_sets=integer_literal ;

sql_security_option : SQL SECURITY (CREATOR|DEFINER|INVOKER|OWNER) ;

procedure_body
    : procedure_data_stat
    | compound_stat
    ;

procedure_stat
    : ( procedure_data_stat
      | compound_stat
      | procedure_cursor_control_stat
      | assignment_stat
      | condition_stat
      | (label_name ':')? iteration_stat label_name?
      | diagnostic_stat
      | print_stat
      | ITERATE label_name
      | LEAVE label_name
      | BEGIN REQUEST procedure_stat+ END REQUEST
      ) ';' //TODO: cases with no semicolon after last END
    ;

procedure_data_stat : procedure_dml_stat | procedure_ddl_stat |  procedure_dcl_stat ;

procedure_dml_stat
    : abort_stat
    | begin_transaction_stat
    | end_transaction_stat
    | call_stat
    | collect_statistics_qcd_form_stat
    | commit_stat
    | delete_stat
    | drop_statistics_qcd_form_stat
    | insert_stat
    | merge_stat
    | rollback_stat
    | select_stat // ONLY SELECT..INTO form is supported
    | select_and_consume_stat // ONLY SELECT AND CONSUME..INTO form is supported
    | update_stat
    | locking_stat
    ;

procedure_ddl_stat
    : alter_function_stat
    | alter_table_stat
    //| alter_trigger_stat //TODO
    | begin_logging_stat
    | collect_statistics_optimizer_form_stat
    | comment_placing_stat
    //| create_cast_stat //TODO
    | create_database_stat
    | create_error_table_stat
    | create_index_stat
    | create_foreign_table_stat
    | create_join_index_stat
    | create_hash_index_stat
    | create_replace_macro_stat
    //| create_ordering_stat //TODO
    | create_profile_stat
    | create_role_stat
    | create_table_as_stat
    | create_table_primary_form_stat
    //| create_transform_stat //TODO
    //| create_trigger_stat //TODO
    | create_user_stat
    | create_replace_view_stat
    | delete_database_stat
    | delete_user_stat
    | drop_cast_stat
    | drop_database_stat
    | drop_error_table_stat
    | drop_index_stat
    | drop_join_index_stat
    | drop_hash_index_stat
    | drop_macro_stat
    | drop_ordering_stat
    | drop_procedure_stat
    | drop_profile_stat
    | drop_role_stat
    | drop_statistics_optimizer_form_stat
    | drop_table_stat
    | drop_transform_stat
    | drop_trigger_stat
    | drop_user_stat
    | drop_view_stat
    | end_logging_stat
    | modify_database_stat
    | modify_profile_stat
    | modify_user_stat
    | rename_macro_stat
    | rename_procedure_stat
    | rename_table_stat
    | rename_trigger_stat
    | rename_view_stat
    //| replace_cast_stat //TODO
    //| replace_function_stat //TODO
    //| replace_ordering_stat //TODO
    //| replace_transform_stat //TODO
    //| replace_trigger_stat //TODO
    | set_query_band_stat
    ;

procedure_dcl_stat
    : give_stat
    | grant_stat
    | revoke_stat
    ;

compound_stat
    : (label_name ':')? BEGIN
      local_declaration*
      cursor_declaration*
      condition_handler*
      procedure_stat*
      END label_name?
    ;

// Only stored procedure forms, except DECLARE CURSOR
procedure_cursor_control_stat
    : allocate_stat
    | close_stat
    | deallocate_prepare_stat
    | positioned_delete_stat
    | positioned_update_stat
    | execute_statement_stat
    | execute_immediate_stat
    | fetch_stat
    | open_stat
    | prepare_stat
    ; //select into ??

assignment_stat : SET assignment_target=variable_name '=' assignment_source=scalar_expr ;

condition_stat : case_stat | if_stat ;

iteration_stat : while_stat | loop_stat | for_stat | repeat_stat ;

diagnostic_stat
    : (SIGNAL signal_spec|RESIGNAL signal_spec?)
      (SET condition_information_item '=' scalar_expr (',' condition_information_item '=' scalar_expr)* )?
    | GET DIAGNOSTICS diagnostic_statement_assignment (',' diagnostic_statement_assignment)*
    | GET DIAGNOSTICS EXCEPTION (condition_number=integer_literal|condition_variable=variable_name)
      diagnostic_condition_assignment (',' diagnostic_condition_assignment)*
    ;

print_stat : PRINT scalar_expr (',' scalar_expr)* ;

local_declaration
    : DECLARE ( variable_name (',' variable_name)* variable_data_type data_type_attribute*
              | condition_name CONDITION (FOR sqlstate_spec)?
              )
      ';'
    ;

cursor_declaration // Stored procedures form with PREPARE tail (as in DDL CREATE PROCEDURE documentation)
    : DECLARE cursor_name (NO? SCROLL)? CURSOR
      (WITHOUT RETURN|WITH RETURN ONLY? (TO (CALLER|CLIENT) )? )?
      FOR (locking_request_modifier? with_request_modifier? query_expr (FOR (READ ONLY|UPDATE))?|statement_name)
      (PREPARE prepared_statement_name=statement_name
         FROM (statement_string=char_string_literal|statement_string_variable=variable_name) )?
      ';'
    ;

condition_handler
    : DECLARE ((CONTINUE|EXIT) HANDLER|condition_name CONDITION)
      FOR ( sqlstate_spec (',' sqlstate_spec)*
          | (SQLEXCEPTION|SQLWARNING|NOT FOUND|condition_name) (',' (SQLEXCEPTION|SQLWARNING|NOT FOUND|condition_name))*
          )
      handler_action_statement=procedure_stat
    ;

allocate_stat : ALLOCATE cursor_name CURSOR FOR PROCEDURE procedure_name ;

close_stat : CLOSE cursor_name ;

deallocate_prepare_stat : DEALLOCATE PREPARE statement_name ;

positioned_delete_stat : (DELETE|DEL) FROM table_name WHERE CURRENT OF cursor_name ;

positioned_update_stat
    : (UPDATE|UPD) table_name (AS? alias_name)?
      SET set_spec (',' set_spec)*
      WHERE CURRENT OF cursor_name
    ;

execute_statement_stat : EXECUTE statement_name (USING variable_name (',' variable_name)* )? ;

execute_immediate_stat : EXECUTE IMMEDIATE statement_name ;

fetch_stat
    : FETCH ((NEXT|FIRST)? FROM)? cursor_name
      INTO variable_name (',' variable_name)*
    ;

open_stat : OPEN cursor_name (USING variable_name (',' variable_name)* )? ;

prepare_stat : PREPARE statement_name FROM (statement_string=char_string_literal|statement_string_variable=variable_name) ;


case_stat
    : CASE (operand_1=scalar_expr when_operand_clause+|when_condition_clause+)
      (ELSE procedure_stat+)?
      END CASE
    ;

when_operand_clause : WHEN operand=scalar_expr THEN procedure_stat+ ;

when_condition_clause : WHEN logical_expr THEN procedure_stat+ ;

if_stat
    : IF logical_expr THEN procedure_stat+
      (ELSEIF logical_expr THEN procedure_stat+ )*
      (ELSE procedure_stat+ )?
      END IF
    ;

while_stat : WHILE logical_expr DO procedure_stat+ END WHILE ;

loop_stat : LOOP procedure_stat+  END LOOP ;

for_stat
    : FOR variable_name AS (cursor_name CURSOR FOR)? locking_request_modifier? with_request_modifier? query_expr
      DO procedure_stat+
      END FOR
    ;

repeat_stat : REPEAT procedure_stat+ UNTIL logical_expr END REPEAT ;

diagnostic_statement_assignment : variable_name '=' statement_information_item ;

diagnostic_condition_assignment : variable_name '=' condition_information_item ;

condition_information_item
    : CLASS_ORIGIN
    | CONDITION_IDENTIFIER
    | CONDITION_NUMBER
    | MESSAGE_LENGTH
    | MESSAGE_TEXT
    | RETURNED_SQLSTATE
    | SUBCLASS_ORIGIN
    ;

statement_information_item
    : COMMAND_FUNCTION
    | COMMAND_FUNCTION_CODE
    | MORE_
    | NUMBER
    | ROW_COUNT
    | TRANSACTION_ACTIVE
    ;

signal_spec : condition_name | sqlstate_spec ;

sqlstate_spec : SQLSTATE VALUE? sqlstate_code=char_string_literal ;

/********************************
    CREATE/REPLACE VIEW statement
*/
create_replace_view_stat
    : (CREATE RECURSIVE? VIEW|CV|REPLACE RECURSIVE? VIEW)
      view_name=table_name
      column_list?
      AS
      locking_request_modifier?
      as_of_clause?
      with_request_modifier?
      query_expr
    ;

as_of_clause
    : AS OF calendar_function '(' (DATE (AT TIME ZONE)?|TIMESTAMP with_time_zone?) scalar_expr (',' system_calendar_name=object_name)? ')'
    ;

/****************************
    CREATE DATABASE statement
*/
create_database_stat
    : (CREATE DATABASE|CD) database_name (FROM from_database_name=database_name)?
      AS database_attribute (',' database_attribute)*
    ;

database_attribute
    : database_size_spec
    | ACCOUNT '=' account_name=char_string_literal
    | database_default_map
    | fallback_protection
    | before_journal
    | after_journal
    | default_journal_table
    ;

/*************************
    CREATE INDEX statement
*/
create_index_stat
    : CREATE index_spec (',' index_spec)* ON TEMPORARY? table_name
    ;

index_spec
    : UNIQUE? INDEX (index_name=unqualified_name)? ALL? '(' index_column_name=unqualified_name (',' index_column_name=unqualified_name)* ')'
      index_ordering? index_loading?
    ;

/*************************
    CREATE INDEX statement
*/
create_join_index_stat
    : CREATE JOIN INDEX join_index_name=table_name
      table_option_index_form (',' table_option_index_form)*
      AS join_index_select_clause
      (index_definition (','? index_definition)* )?
    ;

join_index_select_clause
    : (SEL|SELECT)
      ( ji_selection (',' ji_selection)*
      | '(' ji_selection (',' ji_selection)* ')' ',' '(' ji_selection (',' ji_selection)* ')'
      | (COLUMN|ROW)? '(' ji_selection (',' ji_selection)* ')' (NO? AUTO COMPRESS)?
      )
      FROM ji_source (',' ji_source)*
      where_clause?.
      GROUP BY grouping_spec=ji_grouping_or_ordering_spec
      ORDER BY ordering_spec=ji_grouping_or_ordering_spec
    ;

ji_selection
    : column_name
    | aggregation_clause
    ;

aggregation_clause
    : ( scalar_expr
      | SUM '(' scalar_expr ')'
      | (COUNT|MIN|MAX) '(' scalar_expr ')'
      | EXTRACT '(' (YEAR|MONTH) FROM scalar_expr ')'
      ) (AS? alias_name)?
    ;

ji_source
    : table_name (AS? alias_name)?
    | ji_joined_table
    ;

ji_joined_table
    : '(' ji_joined_table ')'
    | ji_joined_table (INNER|(LEFT|RIGHT) OUTER?)? JOIN ji_joined_table ON logical_expr
    | table_name (AS? alias_name)?
    ;

ji_grouping_or_ordering_spec : scalar_expr (',' scalar_expr)* ;

/***************************
    CREATE PROFILE statement
*/
create_profile_stat : CREATE PROFILE profile_name (AS profile_attribute (',' profile_attribute)* )? ;

profile_attribute
    : ACCOUNT '=' (account_string+=char_string_literal (',' account_string+=char_string_literal)*|NULL)
    | database_default_map
    | DEFAULT DATABASE '=' database_name
    | (SPOOL|TEMPORARY) '=' (size=scalar_expr BYTES?|NULL)
    | PASSWORD ATTRIBUTES? '=' ( '(' password_attribute (',' password_attribute)* ')'|NULL)
    | QUERY_BAND '=' band_spec=char_string_literal ('(' NOT? DEFAULT ')' )?
    | IGNORE QUERY_BAND VALUES '=' ignore_band_spec=char_string_literal
    | TRANSFORM '(' (transform_specification (',' transform_specification)* )? ')'
    | COST PROFILE '=' (cost_profile_name=unqualified_name|NULL)
    | CONSTRAINT '=' user_constraint (',' user_constraint)*
    ;

password_attribute
    : EXPIRE '=' (n=scalar_expr|NULL)
    | MINCHAR '=' (n=scalar_expr|NULL)
    | MAXCHAR '=' (n=scalar_expr|NULL)
    | DIGITS '=' (c=unqualified_name|NULL)
    | SPECCHAR '=' (c=unqualified_name|NULL)
    | MAXLOGONATTEMPTS '=' (n=scalar_expr|NULL)
    | LOCKEDUSEREXPIRE '=' (n=scalar_expr|NULL)
    | REUSE '=' (n=scalar_expr|NULL)
    | RESTRICTWORDS '=' (c=unqualified_name|NULL)
    ;

/**********************************
    CREATE FOREIGN SERVER statement
*/
create_foreign_server_stat
    : CREATE FOREIGN SERVER td_server_db? server_name=unqualified_name
      foreign_server_external_security_clause?
      foreign_server_using_clause?
      (do_import_with (',' do_export_with)?|do_export_with (',' do_import_with)? )?
    ;

foreign_server_external_security_clause : EXTERNAL SECURITY (INVOKER|DEFINER)? TRUSTED authorization_name=object_name ;

foreign_server_using_clause : USING foreign_server_using_option+ ;

foreign_server_using_option : foreign_server_option_name '(' foreign_option_value=scalar_expr ')' ;

foreign_server_operator_option : function_name foreign_server_using_clause? ;

do_import_with : DO IMPORT foreign_server_with_clause ;

do_export_with : DO EXPORT foreign_server_with_clause ;

foreign_server_with_clause : WITH foreign_server_operator_option ;

foreign_server_option_name : (LINK|VERSION) ;

/******************************
    CREATE HASH INDEX statement
*/
create_hash_index_stat
    : CREATE HASH INDEX hash_index_name=table_name (',' table_option_index_form)*
      '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')'
      ON table_name
      (BY '(' by_column_name+=unqualified_name (',' by_column_name+=unqualified_name)* )?
      (ORDER BY ( VALUES
                | (HASH|VALUES)? '(' order_column_name+=unqualified_name (',' order_column_name+=unqualified_name)* ')'
                ) )?
    ;

/************************
    CREATE ROLE statement
*/
create_role_stat : CREATE EXTERNAL? ROLE role_name ;

/*************************
    CREATE TABLE statement
*/
create_table_stat
    : create_table_primary_form_stat
    | create_table_as_stat
    | create_queue_table_stat
    | create_global_temporary_trace_table_stat
    | create_foreign_table_stat
    | create_error_table_stat
    ;

create_table_primary_form_stat
    : (CREATE table_kind? TABLE|CT) table_name (',' table_option)*
      '(' column_definition (',' column_definition)* ')'
      (index_definition (','? index_definition)* )?
      table_preservation?
    ;

create_table_as_stat
    : (CREATE table_kind? TABLE|CT) table_name (',' table_option)*
      ('(' ctas_column_definition (',' ctas_column_definition)* ')' )?
       AS (source_table_name=table_name|'(' with_request_modifier? query_expr ')') WITH NO? DATA (AND NO? (STATISTICS|STATS|STAT))?
      (index_definition (','? index_definition)* )?
      table_preservation?
   ;

create_queue_table_stat
    : (CREATE table_kind? TABLE|CT) table_name ','
      QUEUE (',' table_option)*
      '(' qits_definition (',' column_definition)* (',' column_constraint_attribute)*')'
      (index_definition (','? index_definition)* )?
    ;

create_global_temporary_trace_table_stat
    : CREATE (SET|MULTISET)? GLOBAL TEMPORARY TRACE TABLE table_name (',' table_option)*
      '(' PROC_ID BYTE type_precision? // proc_id BYTE(2)
          ',' SEQUENCE INTEGER // sequence INTEGER
         (',' column_definition)*
      ')'
      table_preservation?
    ;

create_foreign_table_stat
    : CREATE MULTISET? FOREIGN TABLE table_name
      (',' table_option)*
      (',' foreign_table_external_security_clause)?
      ( '('
            location_column
          ',' (payload_column| column_definition (',' column_definition)* )
         ')' )?
      USING '(' location foreign_table_option* ')'
      (','? NO PRIMARY INDEX)?
      (','? PARTITION BY COLUMN)?
    ;

create_error_table_stat : CREATE ERROR TABLE error_table_name=table_name? FOR data_table_name=table_name (NO RLS)? ;


table_kind
    : (SET|MULTISET) (GLOBAL TEMPORARY|VOLATILE)
    | (GLOBAL TEMPORARY|VOLATILE) (SET|MULTISET)
    | (SET|MULTISET)
    | (GLOBAL TEMPORARY|VOLATILE)
    ;

table_option
    : map_spec
    | fallback_protection
    | WITH JOURNAL TABLE '=' journal_table_name=table_name
    | NO? LOG
    | before_journal
    | after_journal
    | CHECKSUM '=' (DEFAULT|ON|OFF)
    | FREESPACE '=' integer_literal PERCENT?
    | mergeblockratio
    | datablocksize
    | block_compression
    | table_isolated_loading
    ;

column_definition
    : name=unqualified_name column_attribute* data_type column_attribute*
    | (COLUMN|ROW) '(' name=unqualified_name column_attribute* data_type column_attribute* ')' (NO? AUTO COMPRESS)?
    | PERIOD FOR period_name=unqualified_name '(' period_begin_column=unqualified_name ',' period_end_column=unqualified_name ')'
    | normalize_option
    | table_constraint
    ;

ctas_column_definition
    : name=unqualified_name column_attribute*
    | (COLUMN|ROW) '(' name=unqualified_name column_attribute* ')' (NO? AUTO COMPRESS)?
    | table_constraint
    ;

index_definition
    : UNIQUE? PRIMARY? INDEX index_name=unqualified_name? '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')'
    | NO PRIMARY INDEX
    | PRIMARY AMP INDEX? index_name=unqualified_name? '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')'
    | PARTITION BY (partitioning_level| '(' partitioning_level (',' partitioning_level)* ')')
    | UNIQUE INDEX index_name=unqualified_name? ('(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')' )? index_loading?
    | INDEX index_name=unqualified_name? ALL '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')' index_ordering? index_loading?
    ;

qits_definition
    : qits_column_name=column_name TIMESTAMP type_precision? /*(6)*/ with_time_zone? NOT NULL DEFAULT CURRENT_TIMESTAMP type_precision? /*(6)*/
    ;

foreign_table_external_security_clause : EXTERNAL SECURITY ( (INVOKER|DEFINER) TRUSTED)? authorization_name=object_name ;

location_column : LOCATION VARCHAR type_precision /*(2048)*/ CHARACTER SET UNICODE CASESPECIFIC ;

payload_column
    : PAYLOAD ( JSON type_precision /*(8388096|16776192)*/ inline_length /*32000|64000*/ CHARACTER SET (UNICODE|LATIN)
              | DATASET type_precision /*(2097088000)*/ inline_length /*64000*/ STORAGE FORMAT CSV CHARACTER SET (UNICODE|LATIN)
              )
    ;

foreign_table_option
    : PATHPATTERN '(' path_pattern=CHAR_STRING ')'
    | MANIFEST '(' manifest=CHAR_STRING ')' // 'TRUE'|'FALSE'
    | ROWFORMAT '(' encoding_format=CHAR_STRING ')'
    | STOREDAS '(' stored_as=CHAR_STRING ')' // 'TEXTFILE'|'PARQUET'
    | HEADER '(' header=CHAR_STRING ')' // 'TRUE'|'FALSE'
    | STRIP_EXTERIOR_SPACES '(' strip_exterior_spaces=CHAR_STRING ')' // 'TRUE'|'FALSE'
    | STRIP_ENCLOSING_CHAR '(' strip_enclosing_char=CHAR_STRING ')' // 'NONE'
    ;


table_preservation : ON COMMIT (DELETE|PRESERVE) ROWS ;

//table options
mergeblockratio
    : DEFAULT MERGEBLOCKRATIO
    | MERGEBLOCKRATIO '=' integer_literal PERCENT?
    | NO MERGEBLOCKRATIO
    ;

datablocksize
    : DATABLOCKSIZE '=' ( data_block_size=integer_literal (BYTES|KBYTES|KILOBYTES)?
                        | (MINIMUM|MAXIMUM|DEFAULT) DATABLOCKSIZE
                        )
    ;

block_compression
    : BLOCKCOMPRESSION '=' (AUTOTEMP|MANUAL|ALWAYS|NEVER)
      (',' BLOCKCOMPRESSIONALGORITHM '=' (ZLIB|ELZS_H|DEFAULT) )?
      (',' BLOCKCOMPRESSIONLEVEL '=' (value=integer_literal|DEFAULT) )?
    ;

table_isolated_loading : with_isolated_loading (FOR (ALL|INSERT|NONE) )? ;

//column definition
column_attribute
    : uppercase_phrase
    | casespecific_phrase
    | format_phrase
    | title_phrase
    | default_value_control_phrase
    | character_set_phrase
    | column_storage_attribute
    | column_constraint_attribute
    | auto_column_attribute
    | identity_column_attribute
    ;


column_storage_attribute
    : NO COMPRESS
    | COMPRESS
    | COMPRESS compressed_value
    | COMPRESS '(' compressed_value (',' compressed_value)* ')'
    | COMPRESS USING compress_udf=table_name DECOMPRESS USING decompress_udf=table_name
    ;

compressed_value : '-' integer_literal | '-' float_literal | literal | NULL ;

column_constraint_attribute
    : (CONSTRAINT constraint_name=unqualified_name)?
      ( UNIQUE
      | PRIMARY KEY
      | CHECK '(' logical_expr ')'
      | references
      | row_level_security_constraint_column_name+=unqualified_name
        (',' row_level_security_constraint_column_name+=unqualified_name)* CONSTRAINT
      )
    ;

auto_column_attribute : NOT? AUTO COLUMN ;

identity_column_attribute
    : GENERATED (ALWAYS|BY DEFAULT) AS IDENTITY '('
      ( START WITH start_with=id_column_value
      | INCREMENT BY increment_by=id_column_value
      | MINVALUE minvalue=id_column_value
      | NO MINVALUE
      | MAXVALUE maxvalue=id_column_value
      | NO MAXVALUE
      | NO? CYCLE
      )+ ')'
    ;

id_column_value : ('-'|'+')? integer_literal ;

normalize_option
    : NORMALIZE (ALL BUT '(' normalize_ignore_column_name+=unqualified_name (',' normalize_ignore_column_name+=unqualified_name)* ')' )?
      ON normalize_column=unqualified_name
      (ON (MEETS OR OVERLAPS|OVERLAPS (OR MEETS)? ) )?
    ;

table_constraint
    : (CONSTRAINT constraint_name=unqualified_name)? ( (UNIQUE|PRIMARY KEY) '(' constrained_column_name+=unqualified_name (',' constrained_column_name+=unqualified_name)* ')'
                                             | CHECK '(' logical_expr ')'
                                             | FOREIGN KEY '(' referencing_column+=unqualified_name (',' referencing_column+=unqualified_name)* ')' references
                                             )
   ;

references
    : REFERENCES (WITH NO? CHECK OPTION)? referenced_table_name=table_name
      ( '(' referenced_column_name+=unqualified_name (',' referenced_column_name+=unqualified_name)* ')' )?
    ;

//index definition
partitioning_level
    : ( partitioning_expr
      | COLUMN (NO? AUTO COMPRESS)?
      | COLUMN (NO? AUTO COMPRESS)? (ALL BUT)? column_partition (',' column_partition)*
      ) (ADD constant=literal)?
    ;

column_partition
    : '(' (COLUMN|ROW)? ( partitioning_column_name+=unqualified_name
                        | '(' partitioning_column_name+=unqualified_name (',' partitioning_column_name+=unqualified_name )* ')'
                        )
      (NO? AUTO COMPRESS)? ')'
    ;

/************************
    CREATE TYPE statement
*/
create_type_stat
    : create_type_structured_form_stat
    | create_type_distinct_form_stat
    | create_type_array_form_stat
    ;

create_type_structured_form_stat
    : CREATE TYPE sysudtlib? udt_name AS '(' type_attribute_spec (',' type_attribute_spec)* ')'
      INSTANTIABLE? NOT FINAL (structured_method_spec (',' structured_method_spec)* )?
    ;

create_type_distinct_form_stat : CREATE TYPE sysudtlib? udt_name AS data_type FINAL distinct_method_spec? ;

create_type_array_form_stat
    : create_type_one_dimensional_array_form_stat
    | create_type_one_dimensional_varray_form_stat
    | create_type_multidimensional_array_form_stat
    | create_type_multidimensional_varray_form_stat
    ;

create_type_one_dimensional_array_form_stat
    : CREATE TYPE sysudtlib? udt_name
        AS data_type ARRAY '[' number_of_elements=integer_literal ']' (DEFAULT NULL)?
    ;

create_type_one_dimensional_varray_form_stat
    : CREATE TYPE sysudtlib? udt_name
        AS (VARYING ARRAY|VARRAY) '(' number_of_elements=integer_literal ')' OF data_type (DEFAULT NULL)?
    ;

create_type_multidimensional_array_form_stat
    : CREATE TYPE sysudtlib? udt_name
        AS data_type ARRAY multidimensional_array_dimension multidimensional_array_dimension+ (DEFAULT NULL)?
    ;

create_type_multidimensional_varray_form_stat
    : CREATE TYPE sysudtlib? udt_name
             AS (VARYING ARRAY|VARRAY) multidimensional_varray_dimension multidimensional_varray_dimension+
             OF data_type (DEFAULT NULL)?
    ;

type_attribute_spec : attribute_name data_type ;

structured_method_spec
    : (INSTANCE|CONSTRUCTOR)? METHOD sysudtlib? method_name '(' ( method_parameter_spec (',' method_parameter_spec)* )? ')'
        RETURNS returns_parameter_spec
        (CAST FROM cast_from_data_type=data_type (AS LOCATOR)? )?
        (SPECIFIC sysudtlib? specific_method_name=method_name)?
        (SELF AS RESULT)?
        (method_attr* method_language_spec method_attr+|method_attr+ method_language_spec method_attr*)
    ;

distinct_method_spec
    : INSTANCE? METHOD sysudtlib? method_name '(' ( method_parameter_spec (',' method_parameter_spec)* )? ')'
        RETURNS returns_parameter_spec
        (CAST FROM cast_from_data_type=data_type (AS LOCATOR)? )?
        (SPECIFIC sysudtlib? specific_method_name=method_name)?
        (SELF AS RESULT)?
        (method_attr* method_language_spec method_attr+|method_attr+ method_language_spec method_attr*)
    ;

method_parameter_spec : parameter_name? data_type (AS LOCATOR)? ;

returns_parameter_spec : data_type (AS LOCATOR)? ;

method_language_spec : LANGUAGE (C|CPP) ;

method_attr
    : SPECIFIC sysudtlib? specific_method_name=method_name
    | PARAMETER STYLE (SQL|TD_GENERAL)
    | NOT? DETERMINISTIC
    | CALLED ON NULL INPUT
    | RETURNS NULL ON NULL INPUT
    | NO SQL
    ;

multidimensional_array_dimension : '[' (array_bounds|maximum_size+=integer_literal) ']' ;

multidimensional_varray_dimension : '(' (array_bounds|maximum_size+=integer_literal) ')' ;

array_bounds : lower_bound=bound ':' upper_bound=bound ;

bound : '-'? integer_literal ;

/************************
    CREATE USER statement
*/
create_user_stat
    : CREATE USER user_name (FROM from_database_name=database_name)?
      AS (user_attribute (',' user_attribute)* )?
      ','? PASSWORD '=' (password|'(' EXPIRE '=' expire=integer_literal')') ','? (user_attribute (',' user_attribute)* )?
    ;

user_attribute
    : STARTUP '=' startup_string=char_string_literal
    | database_size_spec
    | DEFAULT DATABASE '=' database_name
    | COLLATION '=' collation_sequence
    | ACCOUNT '=' (account_string+=char_string_literal (',' account_string+=char_string_literal)*|NULL)
    | database_default_map
    | fallback_protection
    | before_journal
    | after_journal
    | default_journal_table
    | TIME ZONE '=' (LOCAL|('+'|'-')? timezone_string=char_string_literal|NULL)
    | DATEFORM '=' (INTEGERDATE|ANSIDATE|NULL)
    | DEFAULT CHARACTER SET (LATIN|UNICODE|KANJISJIS)
    | DEFAULT ROLE '=' (role_name|NONE|NULL|ALL)
    | PROFILE '=' (profile_name|NULL)
    | TRANSFORM '(' transform_specification (',' transform_specification)* ')'
    | DBA
    | CONSTRAINT '=' user_constraint (',' user_constraint)*
    | EXPORTWIDTH '=' (char_string_literal|DEFAULT) // 'COMPATIBILITY', 'EXPECTED', 'MAXIMUM'
    ;

transform_specification
    : data_type '=' group_name=unqualified_name
    ;

user_constraint
    : row_level_security_constraint_column_name=unqualified_name
      ( '(' (level_name+=unqualified_name DEFAULT?|category_name+=unqualified_name)
            (',' (level_name+=unqualified_name DEFAULT?|category_name+=unqualified_name) )* ')'
      | '(' NULL ')'
      )
    ;

/*********************
    DATABASE statement
*/
database_stat : DATABASE database_name ;

/****************************
    DELETE DATABASE statement
*/
delete_database_stat
    : (DELETE|DEL) DATABASE database_name ALL?
    ;

/************************
    DELETE USER statement
*/
delete_user_stat : (DELETE|DEL) USER user_name ALL? ;

/*******************************
    DROP AUTHORIZATION statement
*/
drop_authorization_stat : DROP AUTHORIZATION authorization_name=object_name ;

/**********************
    DROP CAST statement
*/
drop_cast_stat : DROP CAST (database_name '.')? '(' source_data_type=data_type AS target_data_type=data_type')' ;

/****************************
    DROP CONSTRAINT statement
*/
drop_constraint_stat : DROP CONSTRAINT constraint_name=object_name ;

/**************************
    DROP DATABASE statement
*/
drop_database_stat : DROP DATABASE database_name ;

/*****************************
    DROP ERROR TABLE statement
*/
drop_error_table_stat : DROP ERROR TABLE (FOR data_table_name=table_name|error_table_name=table_name) ;

/********************************
    DROP FOREIGN SERVER statement
*/
drop_foreign_server_stat : DROP FOREIGN SERVER td_server_db? server_name=unqualified_name ;

/**************************
    DROP FUNCTION statement
*/
drop_function_stat
    : DROP SPECIFIC FUNCTION function_name
    | DROP FUNCTION function_name ('(' data_type (',' data_type)* ')')?
    ;

/**********************************
    DROP FUNCTION MAPPING statement
*/
drop_function_mapping_stat : DROP FUNCTION MAPPING functin_mapping_name=object_name ;

/***********************
    DROP INDEX statement
*/
drop_index_stat
    : DROP INDEX ( '(' index_column_name+=unqualified_name (',' index_column_name+=unqualified_name)* ')'
                 | index_name=table_name )
      index_ordering? ON TEMPORARY? table_name
    ;

/**************************
    DROP GLOP SET statement
*/
drop_glop_set_stat : DROP GLOP SET glop_set_name=object_name ;

/****************************
    DROP JOIN INDEX statement
*/
drop_join_index_stat : DROP JOIN INDEX join_index_name=table_name ;

/****************************
    DROP HASH INDEX statement
*/
drop_hash_index_stat : DROP HASH INDEX join_index_name=table_name ;

/***********************
    DROP MACRO statement
*/
drop_macro_stat : DROP MACRO macro_name ;

/*********************
    DROP MAP statement
*/
drop_map_stat : DROP MAP map_name=unqualified_name ;

/************************
    DROP METHOD statement
*/
drop_method_stat : DROP METHOD sysudtlib? method_name ;

/**************************
    DROP ORDERING statement
*/
drop_ordering_stat : DROP ORDERING FOR udt_type ;

/***************************
    DROP PROCEDURE statement
*/
drop_procedure_stat : DROP PROCEDURE procedure_name ;

/*************************
    DROP PROFILE statement
*/
drop_profile_stat : DROP PROFILE profile_name ;

/***********************************
    DROP REPLICATION GROUP statement
*/
drop_replication_group_stat : DROP REPLICATION GROUP name=unqualified_name ;

/*************************************
    DROP REPLICATION RULESET statement
*/
drop_replication_ruleset_stat : DROP REPLICATION RULESET name=unqualified_name FOR replication_group_name=unqualified_name ;

/**********************
    DROP ROLE statement
*/
drop_role_stat : DROP ROLE role_name ;

/************************
    DROP SCHEMA statement
*/
drop_schema_stat : DROP storage_format=unqualified_name SCHEMA sysudtlib? schema_name=unqualified_name ;

/*******************************************
    DROP STATISTICS OPTIMIZER FORM statement
*/
drop_statistics_optimizer_form_stat
    : DROP (STAT|STATS|STATISTICS)
      ( ON? collection_source (stats_target_spec (',' stats_target_spec)* )?
      | (stats_target_spec (',' stats_target_spec)* )? ON collection_source
      )
    ;

/***********************
    DROP TABLE statement
*/
drop_table_stat : DROP TEMPORARY? FOREIGN? TABLE table_name ALL? ;

/***************************
    DROP TRANSFORM statement
*/
drop_transform_stat
    : DROP TRANSFORM (database_name '.')? (transform_group_name=unqualified_name|ALL) FOR udt_type
    ;

/*************************
    DROP TRIGGER statement
*/
drop_trigger_stat : DROP TRIGGER trigger_name=object_name ;

/**********************
    DROP TYPE statement
*/
drop_type_stat : DROP TYPE udt_type ;

/**********************
    DROP USER statement
*/
drop_user_stat : DROP USER user_name ;

/**********************
    DROP VIEW statement
*/
drop_view_stat : DROP VIEW view_name=table_name ;

/**********************
    DROP ZONE statement
*/
drop_zone_stat : DROP ZONE zone_name=unqualified_name ;

/*********************************
    END ISOLATED LOADING statement
*/
end_isolated_loading_stat
    : END CONCURRENT? ISOLATED LOADING FOR QUERY_BAND query_band=char_string_literal // 'LDILoadGroup=value;'
      (OVERRIDE SESSION)?
    ;

/******************************
    END LOGGING statement
*/
end_logging_stat
    : END LOGGING DENIALS? (WITH TEXT)?
      ON (ALL|operation (',' operation)*)
      (FOR CONSTRAINT constraint_name=object_name)?
      (FOR CONSTRAINT constraint_name=object_name)?
      (BY user_name (',' user_name)* )?
      (ON logging_item (',' logging_item)* )?
    ;

/******************************
    END QUERY CAPTURE statement
*/
end_query_capture_stat : END QUERY CAPTURE ;

/******************************
    END QUERY LOGGING statement
*/
end_query_logging_stat : END QUERY LOGGING ON end_query_logging_on_items ;

end_query_logging_on_items
    : end_query_logging_all_rules
    | query_logging_on_all
    | query_logging_on_users
    | query_logging_on_application
    ;

end_query_logging_all_rules : ALL RULES ;

/********************************
    FLUSH QUERY LOGGING statement
*/
flush_query_logging_stat : FLUSH QUERY LOGGING WITH flush_option ;

flush_option
    : ALL
    | ALLDBQL
    | ALLTDWM
    | DEFAULT
    | EXPLAIN
    | LOCK
    | OBJECTS
    | PARAMINFO
    | SQL
    | STATSUSAGE
    | STEPINFO
    | SUMMARY
    | TDWMEVENT
    | TDWMEXCEPTION
    | TDWMHISTORY
    | USECOUNT
    | XMLPLAN
    ;

/*****************
    HELP statement
*/
help_stat
    : HELP request=CHAR_STRING # HelpOnlineStat
    | HELP COLUMN (scalar_expr|table_name_for_all_columns=table_name '.' '*') (',' (scalar_expr|table_name_for_all_columns=table_name '.' '*'))* # HelpColumnListStat
    | HELP COLUMN column_name (',' column_name)* FROM table_name (',' table_name)* # HelpColumnFromStat
    | HELP COLUMN '*' FROM table_name (',' table_name)* # HelpColumnAllFromStat
    | HELP COLUMN column_name FROM ERROR TABLE FOR data_table_name=table_name # HelpColumnFromErrorTableStat
    | HELP CONSTRAINT constraint_name=object_name # HelpConstraintStat
    | HELP TABLE table_name # HelpTableStat
    | HELP ERROR TABLE FOR data_table_name=table_name # HelpErrorTableStat
    | HELP VOLATILE TABLE table_name # HelpVolatileTableStat
    | HELP VIEW view_name=table_name # HelpViewStat
    | HELP INDEX TEMPORARY? indexed_object_name=object_name ( '(' column_name (',' column_name)* ')' )? # HelpIndexStat
    | HELP JOIN INDEX join_index_name=object_name # HelpJoinIndexStat
    | HELP HASH INDEX hash_index_name=object_name # HelpHashIndexStat
    | HELP PROCEDURE procedure_name (ATTRIBUTES|ATTRS|ATTR)? # HelpProcedureStat
    | HELP FUNCTION function_name ('(' (variable_data_type (',' variable_data_type)* )? ')')? # HelpFunctionStat
    | HELP SPECIFIC FUNCTION function_name # HelpSpecificFunctionStat
    | HELP method # HelpMethodStat
    | HELP SPECIFIC METHOD sysudtlib? method_name # HelpSpecificMethodStat
    | HELP TYPE udt_type (ATTRIBUTE|METHOD)? # HelpTypeStat
    | HELP dataset_storage_format SCHEMA schema_name=udt_type # HelpStorageFormatSchemaStat
    | HELP CAST udt_type (SOURCE|TARGET)? # HelpCastStat
    | HELP TRANSFORM udt_type # HelpTransformStat
    | HELP DATABASE database_name # HelpDatabaseStat
    | HELP USER user_name # HelpUserStat
    | HELP TRIGGER trigger_or_table_name=object_name # HelpTriggerStat
    | HELP FOREIGN SERVER td_server_db? server_name=unqualified_name # HelpForeignServer
    | HELP FOREIGN DATABASE database_name server_name_reference  # HelpForeignDatabase
    | HELP FOREIGN TABLE table_name server_name_reference  # HelpForeignTable
    | HELP FOREIGN FUNCTION  function_name server_name_reference # HelpForeignFunction
    ;

/*******************************************
    HELP STATISTICS optimizer form statement
*/
help_statistics_optimimizer_form_stat
    : HELP CURRENT? STATISTICS ON? TEMPORARY? object_name
    ;

/*************************************
    HELP STATISTICS QCD form statement
*/
help_statistics_qcd_form_stat
    : HELP CURRENT? STATISTICS ON? object_name FROM qcd_name=database_name
      (FOR QUERY query_id=integer_literal)?
      (SAMPLEID statistics_id=integer_literal)?
      (USING MODIFIED)?
    ;

/********************************************
    INCREMENTAL RESTORE ALLOW WRITE statement
*/
incremental_restore_allow_write_stat
    : INCREMENTAL RESTORE ALLOW WRITE FOR object_name (',' object_name)*
    ;

/*******************************************
    LOGGING INCREMENTAL ARCHIVE OFF statement
*/
logging_incremental_archive_off_stat
    : LOGGING INCREMENTAL ARCHIVE OFF FOR object_name (',' object_name)*
    ;

/*******************************************
    LOGGING INCREMENTAL ARCHIVE ON statement
*/
logging_incremental_archive_on_stat
    : LOGGING INCREMENTAL ARCHIVE ON FOR object_name (',' object_name)* (DELETE LOG ROWS)?
    ;

/****************************
    MODIFY DATABASE statement
*/
modify_database_stat
    : MODIFY DATABASE database_name
      AS modified_database_attribute (',' modified_database_attribute)*
    ;

modified_database_attribute
    : database_size_spec
    | ACCOUNT '=' account_name=char_string_literal
    | database_default_map
    | fallback_protection
    | before_journal
    | after_journal
    | default_journal_table
    | drop_default_journal_table
    ;

/***************************
    MODIFY PROFILE statement
*/
modify_profile_stat : MODIFY PROFILE profile_name AS profile_attribute (',' profile_attribute)* ;

/************************
    MODIFY USER statement
*/
modify_user_stat
    : MODIFY USER user_name AS modify_user_attribute (',' modify_user_attribute)*
    ;

modify_user_attribute
    : database_size_spec
    | STARTUP '=' (startup_string=char_string_literal|NULL)
    | PASSWORD '=' (password|'(' EXPIRE '=' expire=integer_literal')') (FOR USER)?
    | RELEASE PASSWORD LOCK
    | ACCOUNT '=' (account_string+=char_string_literal (',' account_string+=char_string_literal)*|NULL)
    | database_default_map
    | DEFAULT DATABASE '=' database_name
    | COLLATION '=' collation_sequence
    | fallback_protection
    | before_journal
    | after_journal
    | default_journal_table
    | drop_default_journal_table
    | TIME ZONE '=' (LOCAL|('+'|'-')? timezone_string=char_string_literal|NULL)
    | DATEFORM '=' (INTEGERDATE|ANSIDATE|NULL)
    | DEFAULT CHARACTER SET (LATIN|UNICODE|KANJISJIS)
    | DEFAULT ROLE '=' (role_name|NONE|NULL|ALL)
    | PROFILE '=' (profile_name|NULL)
    | TRANSFORM '(' (transform_specification (',' transform_specification)* )? ')'
    | NOT? DBA
    | EXPORTWIDTH '=' (char_string_literal|DEFAULT) // 'COMPATIBILITY', 'EXPECTED', 'MAXIMUM'
    | CONSTRAINT '=' user_constraint (',' user_constraint)*
    ;

/****************************
    RENAME FUNCTION statement
*/
rename_function_stat
    : RENAME ( SPECIFIC FUNCTION old_specific_function_name=function_name (TO|AS) new_specific_function_name=function_name
             | FUNCTION old_function_name=function_name ('(' data_type (',' data_type)* ')')? (TO|AS) new_function_name=function_name
             )
    ;

/*****************************
    RENAME PROCEDURE statement
*/
rename_procedure_stat : RENAME PROCEDURE old_procedure_name=procedure_name (TO|AS) new_procedure_name=procedure_name ;

/*************************
    RENAME MACRO statement
*/
rename_macro_stat : RENAME MACRO old_macro_name=macro_name (TO|AS) new_macro_name=macro_name ;

/*************************
    RENAME TABLE statement
*/
rename_table_stat : RENAME TABLE old_table_name=table_name (TO|AS) new_table_name=table_name ;

/***************************
    RENAME TRIGGER statement
*/
rename_trigger_stat : RENAME TRIGGER old_trigger_name=object_name (TO|AS) new_trigger_name=object_name ;

/************************
    RENAME VIEW statement
*/
rename_view_stat : RENAME VIEW old_view_name=table_name (TO|AS) new_view_name=table_name ;

/**********************************
    REPLACE QUERY LOGGING statement
*/
replace_query_logging_stat
    : REPLACE QUERY LOGGING
      (WITH query_logging_with_item (',' query_logging_with_item)* )?
      (MODE '=' m=integer_literal)? // 0, 1, 2, 3 (default)
      (LIMIT query_logging_limit_item (AND query_logging_limit_item)? )?
      ON query_logging_on_items
    ;

/************************
    SET SESSION statement
*/
set_session_stat
    : (SET SESSION|SS) ACCOUNT '=' account_string=char_string_literal FOR (SESSION|REQUEST)
    | (SET SESSION|SS) CALENDAR '=' calendar_name=unqualified_name
    | (SET SESSION|SS) CHARACTER SET UNICODE PASS THROUGH (ON|OFF)
    | (SET SESSION|SS) COLLATION collation_sequence
    | (SET SESSION|SS) session_constraint (',' session_constraint)*
    | (SET SESSION|SS) DATABASE database_name
    | (SET SESSION|SS) DATEFORM '=' (ANSIDATE|INTEGERDATE)
    | (SET SESSION|SS) DEBUG session_debug_spec (ON|OFF)
    | (SET SESSION|SS) DOT NOTATION (DEFAULT|LIST|NULL|ERROR) ON ERROR
    | (SET SESSION|SS) FOR NO? CONCURRENT? ISOLATED LOADING
    | (SET SESSION|SS) FUNCTION TRACE (trace_enabling_spec|OFF)
    | (SET SESSION|SS) JSON IGNORE ERRORS (ON|OFF)
    | (SET SESSION|SS) SEARCHUIFDBPATH '=' database_name (',' database_name)*
    | (SET SESSION|SS) CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL isolation_level
    | SET TIME ZONE (LOCAL|USER|scalar_expr)
    | (SET SESSION|SS) UDFSEARCHPATH '=' database_name (',' database_name)* FOR FUNCTION '=' udf_name+=table_name (',' udf_name+=table_name)*
    ;

collation_sequence : ASCII | CHARSET_COLL | EBCDIC | HOST | JIS_COLL | MULTINATIONAL ;

session_constraint
    : CONSTRAINT '=' row_level_security_constraint_name=unqualified_name
      '(' (level_name=unqualified_name|category_name+=unqualified_name (',' category_name+=unqualified_name)*|NULL) ')'
    ;

isolation_level : READ UNCOMMITTED | RU | SERIALIZABLE | SR ;

session_debug_spec
    : FUNCTION function_name
    | PROCEDURE procedure_name
    | METHOD sysudtlib? method_name
    ;

trace_enabling_spec
    : USING mask_string=char_string_literal FOR TRACE? TABLE table_name
    ;

/*********************
    SET ROLE statement
*/
set_role_stat : SET ROLE (role_name|EXTERNAL|NONE|NULL|ALL) ;

/***************************
    SET QUERY_BAND statement
*/
set_query_band_stat : SET QUERY_BAND '=' (band_spec=char_string_literal|NONE) UPDATE? FOR (SESSION VOLATILE?|TRANSACTION) ;

/************************
    SHOW object statement
*/
show_object_stat
    : SHOW (IN XML)? HASH INDEX hash_index_name=object_name # ShowHashIndexStat
    | SHOW (IN XML)? JOIN INDEX join_index_name=object_name # ShowJoinIndexStat
    | SHOW MACRO macro_name # ShowMacroStat
    | SHOW (IN XML)? TEMPORARY? TABLE table_name # ShowTableStat
    | SHOW ERROR TABLE FOR data_table_name=table_name # ShowErrorTableStat
    | SHOW TRIGGER trigger_name=object_name # ShowTriggerStat
    | SHOW (IN XML)? VIEW view_name=table_name # ShowViewStat
    | SHOW PROCEDURE procedure_name # ShowProcedureStat
    | SHOW SPECIFIC FUNCTION function_name # ShowSpecificFunctionStat
    | SHOW FUNCTION function_name ('(' variable_data_type (',' variable_data_type)* ')' )? # ShowFunctionStat
    | SHOW SPECIFIC METHOD sysudtlib? method_name # ShowSpecificMethodStat
    | SHOW method # ShowMethodStat
    | SHOW CAST udt_type # ShowCastStat
    | SHOW TYPE udt_type # ShowTypeStat
    | SHOW dataset_storage_format with_schema? SCHEMA schema_name=udt_type # ShowStorageFormatSchemaStat
    | SHOW FILE uif_name=object_name # ShowFileStat
    | SHOW CONSTRAINT constraint_name=object_name # ShowConstraintStat
    | SHOW AUTHORIZATION authorization_name=object_name # ShowAuthorizationStat
    | SHOW GLOP SET glop_set_name=object_name # ShowGlopSetStat
    | SHOW (IN XML)? FOREIGN SERVER td_server_db? server_name=unqualified_name # ShowForeignServer
    ;

/*******************************
    SHOW QUERY LOGGING statement
*/
show_query_logging_stat : SHOW QUERY LOGGING ON query_logging_on_items ;

/*************************
    SHOW request statement
*/
show_request_stat : SHOW (IN XML)? QUALIFIED? dml_stat ;

/*******************************************
    SHOW STATISTICS optimizer form statement
*/
show_statistics_optimizer_form_stat
    : SHOW (IN XML)? SUMMARY? CURRENT? (STATISTICS|STATS|STAT)
      (VALUES SEQUENCED? )?
      (show_stats_target_spec (',' show_stats_target_spec)* )?
      ON ( TEMPORARY table_name
         | index_name=table_name
         )
    ;

/*************************************
    SHOW STATISTICS QCD form statement
*/
show_statistics_qcd_form_stat
    : SHOW (IN XML)? (STATISTICS|STATS|STAT)
      (VALUES SEQUENCED? )?
      (show_stats_target_spec (',' show_stats_target_spec)* )?
      ON object_name
      FROM qcd_name=database_name
      (FOR QUERY query_id=integer_literal)?
      (SAMPLEID statistics_id=integer_literal)?
      (USING MODIFIED)?
    ;

show_stats_target_spec : UNIQUE? INDEX stats_index_spec|COLUMN stats_column_spec ;


/*
    Shared rules
*/
method
    : (INSTANCE|CONSTRUCTOR)? METHOD method_name ('(' (udt_name (',' udt_name)* )? ')' )?
      FOR for_udt_name=udt_name
    ;

index_loading : WITH NO? LOAD IDENTITY ;

index_ordering : ORDER BY (VALUES|HASH) ( '(' order_column_name=unqualified_name ')' )? ;

table_option_index_form
    : map_spec
    | fallback_protection
    | CHECKSUM '=' (DEFAULT|ON|OFF)
    | block_compression
    ;

map_spec : MAP '=' map_name=unqualified_name (COLOCATE USING colocation_name=unqualified_name)? ;

database_size_spec : (PERMANENT|PERM|SPOOL|TEMPORARY) '=' size=scalar_expr BYTES? skew_spec? ;

skew_spec : SKEW '=' (skew_pct=integer_literal|DEFAULT) PERCENT? ;

database_default_map : DEFAULT MAP '=' (map_name=unqualified_name|NULL) (OVERRIDE NOT? ON ERROR)? ;

fallback_protection : NO? FALLBACK PROTECTION? ;

before_journal : (NO|DUAL)? BEFORE? JOURNAL ;

after_journal : (NO|DUAL|NOT? LOCAL)? AFTER JOURNAL ;

default_journal_table : DEFAULT JOURNAL TABLE '=' journal_table_name=table_name ;

drop_default_journal_table : DROP DEFAULT JOURNAL TABLE ('=' journal_table_name=table_name)? ;

password : PASSWORD_STRING | OBJECT_NAME ;
