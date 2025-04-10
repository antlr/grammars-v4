parser grammar TeradataSQLDMLParser;

import TeradataSQLDataTypesParser
     , TeradataSQLIdentifiersParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

dml_stat
    : select_stat
    | select_and_consume_stat
    | delete_stat
    | insert_stat
    | update_stat
    | merge_stat
    | collect_demographics_stat
    | collect_statistics_qcd_form_stat
    | drop_statistics_qcd_form_stat
    | dump_explain_stat
    | initiate_index_analysis_stat
    | initiate_partition_analysis_stat
    | insert_explain_stat
    | restart_index_analysis_stat
    | call_stat
    | execute_stat
    | commit_stat
    | rollback_stat
    | abort_stat
    | begin_transaction_stat
    | end_transaction_stat
    | locking_stat
    | comment_retrieving_stat
    | checkpoint_stat
    | echo_stat
    | null_stat
    ;

/*******************
    SELECT statement
*/
select_stat
    : request_modifier?
      query_expr
    ;

/*******************
    SELECT AND CONSUME statement
*/
select_and_consume_stat
    : SELECT AND CONSUME TOP count=UNSIGNED_INTEGER /* 1 */ selected_columns
      into_clause?
      FROM queue_table_name=table_name
    ;

/*******************
    DELETE statement
*/
delete_stat
    : locking_request_modifier?
      (DELETE|DEL) with_isolated_loading?
      ( target_table_name=table_name
      | FROM delete_table_spec (',' delete_table_spec)*
      | target_table_name=table_name FROM delete_table_spec (',' delete_table_spec)*
      )
      (where_clause|ALL)?
    ;

delete_table_spec
    : table_name (AS? correlation_name=alias_name)?
    | subquery AS? correlation_name=alias_name
    ;

/*******************
    INSERT statement
*/
insert_stat
    : locking_request_modifier?
      (INSERT|INS) with_isolated_loading?
      INTO? target_table_name=table_name server_name_reference?
      ( VALUES? '(' scalar_expr? (',' scalar_expr? )* ')'
      | column_list VALUES scalar_expr_list_comma_separated
      | column_list? with_request_modifier? select_query=query_expr
        hash_by?
        local_order_by?
        logging_errors?
      | JSON (json_string=char_string_literal|parametrized_sql='?')
      | DEFAULT VALUES
      )
    ;

hash_by : HASH BY (scalar_expr (',' scalar_expr)*|RANDOM) ;

local_order_by : LOCAL ORDER BY order_by_spec_full (',' order_by_spec_full)* ;

/*******************
    UPDATE statement
*/
update_stat
    : update_basic_form_stat
    | update_with_from_stat
    | update_upsert_form_stat
    ;

update_basic_form_stat
    : locking_request_modifier?
      (UPDATE|UPD) with_isolated_loading?
      target_table_name=table_name (AS? correlation_name=alias_name)?
      SET set_spec (',' set_spec)*
      (where_clause|ALL)?
    ;

update_with_from_stat
    : locking_request_modifier?
      (UPDATE|UPD) with_isolated_loading?
      target_table_name=table_name
      FROM update_table_spec (',' update_table_spec)*
      SET set_spec (',' set_spec)*
      (where_clause|ALL)?
    ;

update_upsert_form_stat
    : locking_request_modifier?
      (UPDATE|UPD) with_isolated_loading?
      target_table_name=table_name (AS? correlation_name=alias_name)
      SET set_spec (',' set_spec)*
      WHERE logical_expr
      ELSE (INSERT|INS) INTO? insert_table_name=table_name
      ( VALUES? scalar_expr_list_comma_separated
      | column_list VALUES? scalar_expr_list_comma_separated
      | DEFAULT VALUES
      )
    ;

update_table_spec
    : table_name (AS? correlation_name=alias_name)?
    | subquery AS? correlation_name=alias_name
    ;

/*******************
    MERGE statement
*/
merge_stat
    : locking_request_modifier?
      MERGE with_isolated_loading?
      INTO? target_table_name=table_name (AS? target_correlation_name=alias_name)?
      USING ( VALUES scalar_expr_list_comma_separated
            | subquery
            | source_table_name=table_name
            ) AS? source_correlation_name=alias_name column_list?
      ON match_condition=logical_expr
      ( when_matched when_not_matched?
      | when_not_matched when_matched?
      )
      logging_errors?
    ;

when_matched
    : WHEN MATCHED THEN
      ( (UPDATE|UPD) SET set_spec (',' set_spec)*
      | DELETE
      )
    ;

when_not_matched
    : WHEN NOT MATCHED THEN (INSERT|INS)
      (VALUES|column_list VALUES)?
      scalar_expr_list_comma_separated
    ;


/*********************************
    COLLECT DEMOGRAPHICS statement
*/
collect_demographics_stat
    : COLLECT DEMOGRAPHICS FOR ( table_name | '(' table_name (',' table_name )* ')' )
      INTO qcd_name=database_name (ALL|WITH NO INDEX)?
    ;

/****************************************
    COLLECT STATISTICS QCD form statement
*/
collect_statistics_qcd_form_stat
    : COLLECT (STATISTICS|STATS|STAT)
      FOR SAMPLE sample_percentage=integer_literal PERCENT?
      INTO qcd_name=database_name
      (SET QUERY query_id=integer_literal)?
      (SAMPLEID statistics_id=integer_literal)?
      (UPDATE MODIFIED)?
      ON? table_name
      qcd_stats_target_spec
    ;

qcd_stats_target_spec
    : COLUMN (column_name|PARTITION|'(' (column_name|PARTITION) (',' (column_name|PARTITION))* ')')
    | INDEX (index_name=unqualified_name|'(' column_name (',' column_name)* ')' )
    ;

/*************************************
    DROP STATISTICS QCD form statement
*/
drop_statistics_qcd_form_stat
    : DROP (STATISTICS|STATS|STAT) FROM qcd_name=database_name
      ON? table_name
      qcd_stats_target_spec?
    ;

/*************************
    DUMP EXPLAIN statement
*/
dump_explain_stat
    : DUMP EXPLAIN INTO qcd_name=database_name (AS query_plan_name=unqualified_name)?
      limit_sql_clause?
      (CHECK STATISTICS)?
      explained_sql_request
    ;

/************************************
    INITIATE INDEX ANALYSIS statement
*/
initiate_index_analysis_stat
    : INITIATE INDEX ANALYSIS (ON table_name (',' table_name)* )?
      FOR workload_name=unqualified_name IN qcd_name=database_name AS index_name_tag=alias_name
      (SET index_analysis_set_spec (',' index_analysis_set_spec)* )?
      (KEEP INDEX)?
      (USE MODIFIED (STATISTICS|STATS|STAT) )?
      (WITH NO? INDEX TYPE index_type_number+=integer_literal (',' index_type_number+=integer_literal)* )? // index_type_number between 1 and 6
      (CHECKPOINT checkpoint_trigger=integer_literal)? // positive integer
      analysis_time_limit_clause?
    ;

index_analysis_set_spec : index_analysis_boundary_option '=' value=integer_literal ;

index_analysis_boundary_option
    : CHANGERATE
    | COLUMNSPERINDEX
    | COLUMNSPERJOININDEX
    | INDEXMAINTMODE
    | INDEXESPERTABLE
    | SEARCHSPACE
    ;

/****************************************
    INITIATE PARTITION ANALYSIS statement
*/
initiate_partition_analysis_stat
    : INITIATE PARTITION ANALYSIS (ON table_name (',' table_name)* )?
      FOR workload_name=unqualified_name IN qcd_name=database_name AS result_name_tag=alias_name
      analysis_time_limit_clause?
    ;

/***************************
    INSERT EXPLAIN statement
*/
insert_explain_stat
    : INSERT EXPLAIN
      (WITH NO? STATISTICS (USING SAMPLE sample_percentage=integer_literal PERCENT? )? )?
      (AND DEMOGRAPHICS)?
      (FOR table_name (',' table_name)* )?
      INTO qcd_name=database_name (AS query_plan_name=unqualified_name)?
      limit_sql_clause?
      (FOR frequency=integer_literal)? // frequency - any positive integer up to 4 bytes (int)
      (CHECK STATISTICS)?
      (IN XML NODDLTEXT? )?
      explained_sql_request
    ;

/***********************************
    RESTART INDEX ANALYSIS statement
*/
restart_index_analysis_stat
    : RESTART INDEX ANALYSIS
      FOR workload_name=unqualified_name IN qcd_name=database_name AS index_name_tag=alias_name
      (CHECKPOINT checkpoint_trigger=integer_literal)?
      analysis_time_limit_clause?
    ;

/*****************
    CALL statement
*/
call_stat : CALL procedure_name '(' (argument (',' argument)* )? ')' ;

argument
    : scalar_expr returns_clause?
    | '?'
    ;

/********************
    EXECUTE statement
*/
execute_stat
    : (EXECUTE|EXEC) macro_name ( '(' ( scalar_expr (',' scalar_expr)*
                                      | parameter_name '=' scalar_expr (',' parameter_name '=' scalar_expr)*
                                      )
                                ')' )?
    ;

/*******************
    COMMIT statement
*/
commit_stat : COMMIT (WORK RELEASE? )? ;

/*********************
    ROLLBACK statement
*/
rollback_stat : ROLLBACK WORK? abort_message=char_string_literal? from_clause? where_clause? ;

/******************
    ABORT statement
*/
abort_stat
    : ABORT (abort_message=char_string_literal)?
      from_clause?
      where_clause?
    ;

/******************************
    BEGIN TRANSACTION statement
*/
begin_transaction_stat : BEGIN TRANSACTION | BT ;

/******************************
    END TRANSACTION statement
*/
end_transaction_stat : END TRANSACTION | ET ;

/********************
    LOCKING statement
*/
/* There is no distinct "locking statement", but it is possible to use
    locking request modifier with empty SQL statement
*/
locking_stat : locking_request_modifier ;

/********************
    COMMENT retrieving statement
*/
comment_retrieving_stat : COMMENT ON? (object_kind? object_name|COLUMN column_name|column_name) ;

/***********************
    CHECKPOINT statement
*/
checkpoint_stat : CHECKPOINT journal_table=table_name (',' NAMED variable_name)? ;

/*****************
    ECHO statement
*/
echo_stat : ECHO string=char_string_literal? ;

/*****************
    null statement
*/
null_stat : ';' ;

/*
    Shared rules
*/
set_spec : column_name /* .mutator_method_name */ '=' scalar_expr ;

with_isolated_loading : WITH NO? CONCURRENT? ISOLATED LOADING ;

logging_errors
    : LOGGING ALL? ERRORS
      (WITH (NO LIMIT|LIMIT OF error_limit=integer_literal) )?
    ;

object_kind : FUNCTION|GLOP SET|GROUP|MACRO|METHOD|PROCEDURE|PROFILE|ROLE|TRIGGER|TYPE|VIEW|DATABASE|TABLE|USER|FILE ;

explained_sql_request : delete_stat|execute_stat|insert_stat|merge_stat|select_stat|update_stat ;

limit_sql_clause : LIMIT (SQL ('=' sql_length=integer_literal)? )? ;

analysis_time_limit_clause : TIME LIMIT '=' elapsed_time=integer_literal ; //elapsed_time between 1 and 2880
