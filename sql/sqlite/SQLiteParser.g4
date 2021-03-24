/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers
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
 *
 * Project : sqlite-parser; an ANTLR4 grammar for SQLite https://github.com/bkiers/sqlite-parser
 * Developed by:
 *     Bart Kiers, bart@big-o.nl
 *     Martin Mirchev, marti_2203@abv.bg
 *     Mike Lische, mike@lischke-online.de
 */

// $antlr-format alignTrailingComments on, columnLimit 130, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments off
// $antlr-format useTab off, allowShortRulesOnASingleLine off, allowShortBlocksOnASingleLine on, alignSemicolons ownLine

parser grammar SQLiteParser;

options {
    tokenVocab = SQLiteLexer;
}

parse: (sql_stmt_list)* EOF
;

sql_stmt_list:
    SCOL* sql_stmt (SCOL+ sql_stmt)* SCOL*
;

sql_stmt: (EXPLAIN_SYM (QUERY_SYM PLAN_SYM)?)? (
        alter_table_stmt
        | analyze_stmt
        | attach_stmt
        | begin_stmt
        | commit_stmt
        | create_index_stmt
        | create_table_stmt
        | create_trigger_stmt
        | create_view_stmt
        | create_virtual_SYMVIRTUAL_SYM_table_stmt
        | delete_stmt
        | delete_stmt_limited
        | detach_stmt
        | drop_stmt
        | insert_stmt
        | pragma_stmt
        | reindex_stmt
        | release_stmt
        | rollback_stmt
        | savepoint_stmt
        | select_stmt
        | update_stmt
        | update_stmt_limited
        | vacuum_stmt
    )
;

alter_table_stmt:
    ALTER_SYM TABLE_SYM (schema_name DOT)? table_name (
        RENAME_SYM (
            TO_SYM new_table_name
            | COLUMN_SYM? old_column_name = column_name TO_SYM new_column_name = column_name
        )
        | ADD_SYM COLUMN_SYM? column_def
    )
;

analyze_stmt:
    ANALYZE_SYM (schema_name | (schema_name DOT)? table_or_index_name)?
;

attach_stmt:
    ATTACH_SYM DATABASE_SYM? expr AS_SYM schema_name
;

begin_stmt:
    BEGIN_SYM (DEFERRED_SYM | IMMEDIATE_SYM | EXCLUSIVE_SYM)? (
        TRANSACTION_SYM transaction_name?
    )?
;

commit_stmt: (COMMIT_SYM | END_SYM) TRANSACTION_SYM?
;

rollback_stmt:
    ROLLBACK_SYM TRANSACTION_SYM? (TO_SYM SAVEPOINT_SYM? savepoint_name)?
;

savepoint_stmt:
    SAVEPOINT_SYM savepoint_name
;

release_stmt:
    RELEASE_SYM SAVEPOINT_SYM? savepoint_name
;

create_index_stmt:
    CREATE_SYM UNIQUE_SYM? INDEX_SYM (IF_SYM NOT_SYM EXISTS_SYM)? (schema_name DOT)? index_name ON_SYM table_name OPEN_PAR
        indexed_column (COMMA indexed_column)* CLOSE_PAR (WHERE_SYM expr)?
;

indexed_column: (column_name | expr) (COLLATE_SYM collation_name)? asc_desc?
;

create_table_stmt:
    CREATE_SYM (TEMP_SYM | TEMPORARY_SYM)? TABLE_SYM (IF_SYM NOT_SYM EXISTS_SYM)? (
        schema_name DOT
    )? table_name (
        OPEN_PAR column_def (COMMA column_def)*? (COMMA table_constraint)* CLOSE_PAR (
            WITHOUT_SYM row_SYMROW_SYMID = IDENTIFIER
        )?
        | AS_SYM select_stmt
    )
;

column_def:
    column_name type_name? column_constraint*
;

type_name:
    name+? (
        OPEN_PAR signed_number CLOSE_PAR
        | OPEN_PAR signed_number COMMA signed_number CLOSE_PAR
    )?
;

column_constraint: (CONSTRAINT_SYM name)? (
        (PRIMARY_SYM KEY_SYM asc_desc? conflict_clause? AUTOINCREMENT_SYM?)
        | (NOT_SYM NULL_SYM | UNIQUE_SYM) conflict_clause?
        | CHECK_SYM OPEN_PAR expr CLOSE_PAR
        | DEFAULT_SYM (signed_number | literal_value | OPEN_PAR expr CLOSE_PAR)
        | COLLATE_SYM collation_name
        | foreign_key_clause
        | (GENERATED_SYM ALWAYS_SYM)? AS_SYM OPEN_PAR expr CLOSE_PAR (
            STORED_SYM
            | VIRTUAL_SYM
        )?
    )
;

signed_number: (PLUS | MINUS)? NUMERIC_LITERAL
;

table_constraint: (CONSTRAINT_SYM name)? (
        (PRIMARY_SYM KEY_SYM | UNIQUE_SYM) OPEN_PAR indexed_column (
            COMMA indexed_column
        )* CLOSE_PAR conflict_clause?
        | CHECK_SYM OPEN_PAR expr CLOSE_PAR
        | FOREIGN_SYM KEY_SYM OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR foreign_key_clause
    )
;

foreign_key_clause:
    REFERENCES_SYM foreign_table (
        OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
    )? (
        ON_SYM (DELETE_SYM | UPDATE_SYM) (
            SET_SYM (NULL_SYM | DEFAULT_SYM)
            | CASCADE_SYM
            | RESTRICT_SYM
            | NO_SYM ACTION_SYM
        )
        | MATCH_SYM name
    )* (NOT_SYM? DEFERRABLE_SYM (INITIALLY_SYM (DEFERRED_SYM | IMMEDIATE_SYM))?)?
;

conflict_clause:
    ON_SYM CONFLICT_SYM (
        ROLLBACK_SYM
        | ABORT_SYM
        | FAIL_SYM
        | IGNORE_SYM
        | REPLACE_SYM
    )
;

create_trigger_stmt:
    CREATE_SYM (TEMP_SYM | TEMPORARY_SYM)? TRIGGER_SYM (IF_SYM NOT_SYM EXISTS_SYM)? (
        schema_name DOT
    )? trigger_name (BEFORE_SYM | AFTER_SYM | INSTEAD_SYM OF_SYM)? (
        DELETE_SYM
        | INSERT_SYM
        | UPDATE_SYM (OF_SYM column_name ( COMMA column_name)*)?
    ) ON_SYM table_name (FOR_SYM EACH_SYM ROW_SYM)? (WHEN_SYM expr)? BEGIN_SYM (
        (update_stmt | insert_stmt | delete_stmt | select_stmt) SCOL
    )+ END_SYM
;

create_view_stmt:
    CREATE_SYM (TEMP_SYM | TEMPORARY_SYM)? VIEW_SYM (IF_SYM NOT_SYM EXISTS_SYM)? (
        schema_name DOT
    )? view_name (OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR)? AS_SYM select_stmt
;

create_virtual_SYMVIRTUAL_SYM_table_stmt:
    CREATE_SYM VIRTUAL_SYM TABLE_SYM (IF_SYM NOT_SYM EXISTS_SYM)? (schema_name DOT)? table_name USING_SYM module_name (
        OPEN_PAR module_argument (COMMA module_argument)* CLOSE_PAR
    )?
;

with_clause:
    WITH_SYM RECURSIVE_SYM? cte_table_name AS_SYM OPEN_PAR select_stmt CLOSE_PAR (
        COMMA cte_table_name AS_SYM OPEN_PAR select_stmt CLOSE_PAR
    )*
;

cte_table_name:
    table_name (OPEN_PAR column_name ( COMMA column_name)* CLOSE_PAR)?
;

recursive_cte:
    cte_table_name AS_SYM OPEN_PAR initial_select UNION_SYM ALL_SYM? recursive_SYM_select CLOSE_PAR
;

common_table_expression:
    table_name (OPEN_PAR column_name ( COMMA column_name)* CLOSE_PAR)? AS_SYM OPEN_PAR select_stmt CLOSE_PAR
;

delete_stmt:
    with_clause? DELETE_SYM FROM_SYM qualified_table_name (WHERE_SYM expr)?
;

delete_stmt_limited:
    with_clause? DELETE_SYM FROM_SYM qualified_table_name (WHERE_SYM expr)? (
        order_by_stmt? limit_stmt
    )?
;

detach_stmt:
    DETACH_SYM DATABASE_SYM? schema_name
;

drop_stmt:
    DROP_SYM object = (INDEX_SYM | TABLE_SYM | TRIGGER_SYM | VIEW_SYM) (
        IF_SYM EXISTS_SYM
    )? (schema_name DOT)? any_name
;

/*
 SQLite understands the following binary operators, in order from highest to lowest precedence:
    ||
    * / %
    + -
    << >> & |
    < <= > >=
    = == != <> IS IS NOT IN LIKE GLOB MATCH REGEXP
    AND
    OR
 */
expr:
    literal_value
    | BIND_PARAMETER
    | ((schema_name DOT)? table_name DOT)? column_name
    | unary_operator expr
    | expr PIPE2 expr
    | expr ( STAR | DIV | MOD) expr
    | expr ( PLUS | MINUS) expr
    | expr ( LT2 | GT2 | AMP | PIPE) expr
    | expr ( LT | LT_EQ | GT | GT_EQ) expr
    | expr (
        ASSIGN
        | EQ
        | NOT_EQ1
        | NOT_EQ2
        | IS_SYM
        | IS_SYM NOT_SYM
        | IN_SYM
        | LIKE_SYM
        | GLOB_SYM
        | MATCH_SYM
        | REGEXP_SYM
    ) expr
    | expr AND_SYM expr
    | expr OR_SYM expr
    | function_name OPEN_PAR ((DISTINCT_SYM? expr ( COMMA expr)*) | STAR)? CLOSE_PAR filter_clause? over_clause?
    | OPEN_PAR expr (COMMA expr)* CLOSE_PAR
    | CAST_SYM OPEN_PAR expr AS_SYM type_name CLOSE_PAR
    | expr COLLATE_SYM collation_name
    | expr NOT_SYM? (LIKE_SYM | GLOB_SYM | REGEXP_SYM | MATCH_SYM) expr (
        ESCAPE_SYM expr
    )?
    | expr ( ISNULL_SYM | NOTNULL_SYM | NOT_SYM NULL_SYM)
    | expr IS_SYM NOT_SYM? expr
    | expr NOT_SYM? BETWEEN_SYM expr AND_SYM expr
    | expr NOT_SYM? IN_SYM (
        OPEN_PAR (select_stmt | expr ( COMMA expr)*)? CLOSE_PAR
        | ( schema_name DOT)? table_name
        | (schema_name DOT)? table_function_name OPEN_PAR (expr (COMMA expr)*)? CLOSE_PAR
    )
    | ((NOT_SYM)? EXISTS_SYM)? OPEN_PAR select_stmt CLOSE_PAR
    | CASE_SYM expr? (WHEN_SYM expr THEN_SYM expr)+ (ELSE_SYM expr)? END_SYM
    | raise_function
;

raise_function:
    RAISE_SYM OPEN_PAR (
        IGNORE_SYM
        | (ROLLBACK_SYM | ABORT_SYM | FAIL_SYM) COMMA error_message
    ) CLOSE_PAR
;

literal_value:
    NUMERIC_LITERAL
    | STRING_LITERAL
    | BLOB_LITERAL
    | NULL_SYM
    | TRUE_SYM
    | FALSE_SYM
    | CURRENT_TIME_SYM
    | CURRENT_DATE_SYM
    | CURRENT_TIMESTAMP_SYM
;

insert_stmt:
    with_clause? (
        INSERT_SYM
        | REPLACE_SYM
        | INSERT_SYM OR_SYM (
            REPLACE_SYM
            | ROLLBACK_SYM
            | ABORT_SYM
            | FAIL_SYM
            | IGNORE_SYM
        )
    ) INTO_SYM (schema_name DOT)? table_name (AS_SYM table_alias)? (
        OPEN_PAR column_name ( COMMA column_name)* CLOSE_PAR
    )? (
        (
            VALUES_SYM OPEN_PAR expr (COMMA expr)* CLOSE_PAR (
                COMMA OPEN_PAR expr ( COMMA expr)* CLOSE_PAR
            )*
            | select_stmt
        ) upsert_clause?
    )
    | DEFAULT_SYM VALUES_SYM
;

upsert_clause:
    ON_SYM CONFLICT_SYM (
        OPEN_PAR indexed_column (COMMA indexed_column)* CLOSE_PAR (WHERE_SYM expr)?
    )? DO_SYM (
        NOTHING_SYM
        | UPDATE_SYM SET_SYM (
            (column_name | column_name_list) EQ expr (
                COMMA (column_name | column_name_list) EQ expr
            )* (WHERE_SYM expr)?
        )
    )
;

pragma_stmt:
    PRAGMA_SYM (schema_name DOT)? pragma_name (
        ASSIGN pragma_value
        | OPEN_PAR pragma_value CLOSE_PAR
    )?
;

pragma_value:
    signed_number
    | name
    | STRING_LITERAL
;

reindex_stmt:
    REINDEX_SYM (collation_name | (schema_name DOT)? (table_name | index_name))?
;

select_stmt:
    common_table_stmt? select_core (compound_operator select_core)* order_by_stmt? limit_stmt?
;

join_clause:
    table_or_subquery (join_operator table_or_subquery join_constraint?)*
;

select_core:
    (
        SELECT_SYM (DISTINCT_SYM | ALL_SYM)? result_column (COMMA result_column)* (
            FROM_SYM (table_or_subquery (COMMA table_or_subquery)* | join_clause)
        )? (WHERE_SYM expr)? (GROUP_SYM BY_SYM expr (COMMA expr)* (HAVING_SYM expr)?)? (
            WINDOW_SYM window_name AS_SYM window_defn (
                COMMA window_name AS_SYM window_defn
            )*
        )?
    )
    | VALUES_SYM OPEN_PAR expr (COMMA expr)* CLOSE_PAR (
        COMMA OPEN_PAR expr ( COMMA expr)* CLOSE_PAR
    )*
;

factored_select_stmt:
    select_stmt
;

simple_select_stmt:
    common_table_stmt? select_core order_by_stmt? limit_stmt?
;

compound_select_stmt:
    common_table_stmt? select_core (
        (UNION_SYM ALL_SYM? | INTERSECT_SYM | EXCEPT_SYM) select_core
    )+ order_by_stmt? limit_stmt?
;

table_or_subquery: (
        (schema_name DOT)? table_name (AS_SYM? table_alias)? (
            INDEXED_SYM BY_SYM index_name
            | NOT_SYM INDEXED_SYM
        )?
    )
    | (schema_name DOT)? table_function_name OPEN_PAR expr (COMMA expr)* CLOSE_PAR (
        AS_SYM? table_alias
    )?
    | OPEN_PAR (table_or_subquery (COMMA table_or_subquery)* | join_clause) CLOSE_PAR
    | OPEN_PAR select_stmt CLOSE_PAR (AS_SYM? table_alias)?
;

result_column:
    STAR
    | table_name DOT STAR
    | expr ( AS_SYM? column_alias)?
;

join_operator:
    COMMA
    | NATURAL_SYM? (LEFT_SYM OUTER_SYM? | INNER_SYM | CROSS_SYM)? JOIN_SYM
;

join_constraint:
    ON_SYM expr
    | USING_SYM OPEN_PAR column_name ( COMMA column_name)* CLOSE_PAR
;

compound_operator:
    UNION_SYM ALL_SYM?
    | INTERSECT_SYM
    | EXCEPT_SYM
;

update_stmt:
    with_clause? UPDATE_SYM (
        OR_SYM (ROLLBACK_SYM | ABORT_SYM | REPLACE_SYM | FAIL_SYM | IGNORE_SYM)
    )? qualified_table_name SET_SYM (column_name | column_name_list) ASSIGN expr (
        COMMA (column_name | column_name_list) ASSIGN expr
    )* (WHERE_SYM expr)?
;

column_name_list:
    OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
;

update_stmt_limited:
    with_clause? UPDATE_SYM (
        OR_SYM (ROLLBACK_SYM | ABORT_SYM | REPLACE_SYM | FAIL_SYM | IGNORE_SYM)
    )? qualified_table_name SET_SYM (column_name | column_name_list) ASSIGN expr (
        COMMA (column_name | column_name_list) ASSIGN expr
    )* (WHERE_SYM expr)? (order_by_stmt? limit_stmt)?
;

qualified_table_name: (schema_name DOT)? table_name (AS_SYM alias)? (
        INDEXED_SYM BY_SYM index_name
        | NOT_SYM INDEXED_SYM
    )?
;

vacuum_stmt:
    VACUUM_SYM schema_name? (INTO_SYM filename)?
;

filter_clause:
    FILTER_SYM OPEN_PAR WHERE_SYM expr CLOSE_PAR
;

window_defn:
    OPEN_PAR base_window_name? (PARTITION_SYM BY_SYM expr (COMMA expr)*)? (
        ORDER_SYM BY_SYM ordering_term (COMMA ordering_term)*
    ) frame_spec? CLOSE_PAR
;

over_clause:
    OVER_SYM (
        window_name
        | OPEN_PAR base_window_name? (PARTITION_SYM BY_SYM expr (COMMA expr)*)? (
            ORDER_SYM BY_SYM ordering_term (COMMA ordering_term)*
        )? frame_spec? CLOSE_PAR
    )
;

frame_spec:
    frame_clause (
        EXCLUDE_SYM (NO_SYM OTHERS_SYM)
        | CURRENT_SYM ROW_SYM
        | GROUP_SYM
        | TIES_SYM
    )?
;

frame_clause: (RANGE_SYM | ROWS_SYM | GROUPS_SYM) (
        frame_single
        | BETWEEN_SYM frame_left AND_SYM frame_right
    )
;

simple_function_invocation:
    simple_func OPEN_PAR (expr (COMMA expr)* | STAR) CLOSE_PAR
;

aggregate_function_invocation:
    aggregate_func OPEN_PAR (DISTINCT_SYM? expr (COMMA expr)* | STAR)? CLOSE_PAR filter_clause?
;

window_function_invocation:
    window_function OPEN_PAR (expr (COMMA expr)* | STAR)? CLOSE_PAR filter_clause? OVER_SYM (
        window_defn
        | window_name
    )
;

common_table_stmt: //additional structures
    WITH_SYM RECURSIVE_SYM? common_table_expression (COMMA common_table_expression)*
;

order_by_stmt:
    ORDER_SYM BY_SYM ordering_term (COMMA ordering_term)*
;

limit_stmt:
    LIMIT_SYM expr ((OFFSET_SYM | COMMA) expr)?
;

ordering_term:
    expr (COLLATE_SYM collation_name)? asc_desc? (NULLS_SYM (FIRST_SYM | LAST_SYM))?
;

asc_desc:
    ASC_SYM
    | DESC_SYM
;

frame_left:
    expr PRECEDING_SYM
    | expr FOLLOWING_SYM
    | CURRENT_SYM ROW_SYM
    | UNBOUNDED_SYM PRECEDING_SYM
;

frame_right:
    expr PRECEDING_SYM
    | expr FOLLOWING_SYM
    | CURRENT_SYM ROW_SYM
    | UNBOUNDED_SYM FOLLOWING_SYM
;

frame_single:
    expr PRECEDING_SYM
    | UNBOUNDED_SYM PRECEDING_SYM
    | CURRENT_SYM ROW_SYM
;

// unknown

window_function:
    (FIRST_VALUE_SYM | LAST_VALUE_SYM) OPEN_PAR expr CLOSE_PAR OVER_SYM OPEN_PAR partition_by? order_by_expr_asc_desc frame_clause
        ? CLOSE_PAR
    | (CUME_DIST_SYM | PERCENT_RANK_SYM) OPEN_PAR CLOSE_PAR OVER_SYM OPEN_PAR partition_by? order_by_expr? CLOSE_PAR
    | (DENSE_RANK_SYM | RANK_SYM | ROW_NUMBER_SYM) OPEN_PAR CLOSE_PAR OVER_SYM OPEN_PAR partition_by? order_by_expr_asc_desc
        CLOSE_PAR
    | (LAG_SYM | LEAD_SYM) OPEN_PAR expr of_SYMOF_SYMfset? default_SYMDEFAULT_SYM_value? CLOSE_PAR OVER_SYM OPEN_PAR partition_by?
        order_by_expr_asc_desc CLOSE_PAR
    | NTH_VALUE_SYM OPEN_PAR expr COMMA signed_number CLOSE_PAR OVER_SYM OPEN_PAR partition_by? order_by_expr_asc_desc
        frame_clause? CLOSE_PAR
    | NTILE_SYM OPEN_PAR expr CLOSE_PAR OVER_SYM OPEN_PAR partition_by? order_by_expr_asc_desc CLOSE_PAR
;

of_SYMOF_SYMfset:
    COMMA signed_number
;

default_SYMDEFAULT_SYM_value:
    COMMA signed_number
;

partition_by:
    PARTITION_SYM BY_SYM expr+
;

order_by_expr:
    ORDER_SYM BY_SYM expr+
;

order_by_expr_asc_desc:
    ORDER_SYM BY_SYM order_by_expr_asc_desc
;

expr_asc_desc:
    expr asc_desc? (COMMA expr asc_desc?)*
;

//TODO BOTH OF_SYM THESE HAVE TO BE REWORKED TO FOLLOW THE SPEC
initial_select:
    select_stmt
;

recursive_SYM_select:
    select_stmt
;

unary_operator:
    MINUS
    | PLUS
    | TILDE
    | NOT_SYM
;

error_message:
    STRING_LITERAL
;

module_argument: // TODO check_SYMCHECK_SYM what exactly is permitted here
    expr
    | column_def
;

column_alias:
    IDENTIFIER
    | STRING_LITERAL
;

keyword:
    ABORT_SYM
    | ACTION_SYM
    | ADD_SYM
    | AFTER_SYM
    | ALL_SYM
    | ALTER_SYM
    | ANALYZE_SYM
    | AND_SYM
    | AS_SYM
    | ASC_SYM
    | ATTACH_SYM
    | AUTOINCREMENT_SYM
    | BEFORE_SYM
    | BEGIN_SYM
    | BETWEEN_SYM
    | BY_SYM
    | CASCADE_SYM
    | CASE_SYM
    | CAST_SYM
    | CHECK_SYM
    | COLLATE_SYM
    | COLUMN_SYM
    | COMMIT_SYM
    | CONFLICT_SYM
    | CONSTRAINT_SYM
    | CREATE_SYM
    | CROSS_SYM
    | CURRENT_DATE_SYM
    | CURRENT_TIME_SYM
    | CURRENT_TIMESTAMP_SYM
    | DATABASE_SYM
    | DEFAULT_SYM
    | DEFERRABLE_SYM
    | DEFERRED_SYM
    | DELETE_SYM
    | DESC_SYM
    | DETACH_SYM
    | DISTINCT_SYM
    | DROP_SYM
    | EACH_SYM
    | ELSE_SYM
    | END_SYM
    | ESCAPE_SYM
    | EXCEPT_SYM
    | EXCLUSIVE_SYM
    | EXISTS_SYM
    | EXPLAIN_SYM
    | FAIL_SYM
    | FOR_SYM
    | FOREIGN_SYM
    | FROM_SYM
    | FULL_SYM
    | GLOB_SYM
    | GROUP_SYM
    | HAVING_SYM
    | IF_SYM
    | IGNORE_SYM
    | IMMEDIATE_SYM
    | IN_SYM
    | INDEX_SYM
    | INDEXED_SYM
    | INITIALLY_SYM
    | INNER_SYM
    | INSERT_SYM
    | INSTEAD_SYM
    | INTERSECT_SYM
    | INTO_SYM
    | IS_SYM
    | ISNULL_SYM
    | JOIN_SYM
    | KEY_SYM
    | LEFT_SYM
    | LIKE_SYM
    | LIMIT_SYM
    | MATCH_SYM
    | NATURAL_SYM
    | NO_SYM
    | NOT_SYM
    | NOTNULL_SYM
    | NULL_SYM
    | OF_SYM
    | OFFSET_SYM
    | ON_SYM
    | OR_SYM
    | ORDER_SYM
    | OUTER_SYM
    | PLAN_SYM
    | PRAGMA_SYM
    | PRIMARY_SYM
    | QUERY_SYM
    | RAISE_SYM
    | RECURSIVE_SYM
    | REFERENCES_SYM
    | REGEXP_SYM
    | REINDEX_SYM
    | RELEASE_SYM
    | RENAME_SYM
    | REPLACE_SYM
    | RESTRICT_SYM
    | RIGHT_SYM
    | ROLLBACK_SYM
    | ROW_SYM
    | ROWS_SYM
    | SAVEPOINT_SYM
    | SELECT_SYM
    | SET_SYM
    | TABLE_SYM
    | TEMP_SYM
    | TEMPORARY_SYM
    | THEN_SYM
    | TO_SYM
    | TRANSACTION_SYM
    | TRIGGER_SYM
    | UNION_SYM
    | UNIQUE_SYM
    | UPDATE_SYM
    | USING_SYM
    | VACUUM_SYM
    | VALUES_SYM
    | VIEW_SYM
    | VIRTUAL_SYM
    | WHEN_SYM
    | WHERE_SYM
    | WITH_SYM
    | WITHOUT_SYM
    | FIRST_VALUE_SYM
    | OVER_SYM
    | PARTITION_SYM
    | RANGE_SYM
    | PRECEDING_SYM
    | UNBOUNDED_SYM
    | CURRENT_SYM
    | FOLLOWING_SYM
    | CUME_DIST_SYM
    | DENSE_RANK_SYM
    | LAG_SYM
    | LAST_VALUE_SYM
    | LEAD_SYM
    | NTH_VALUE_SYM
    | NTILE_SYM
    | PERCENT_RANK_SYM
    | RANK_SYM
    | ROW_NUMBER_SYM
    | GENERATED_SYM
    | ALWAYS_SYM
    | STORED_SYM
    | TRUE_SYM
    | FALSE_SYM
    | WINDOW_SYM
    | NULLS_SYM
    | FIRST_SYM
    | LAST_SYM
    | FILTER_SYM
    | GROUPS_SYM
    | EXCLUDE_SYM
;

// TODO check_SYMCHECK_SYM all names below

name:
    any_name
;

function_name:
    any_name
;

schema_name:
    any_name
;

table_name:
    any_name
;

table_or_index_name:
    any_name
;

new_table_name:
    any_name
;

column_name:
    any_name
;

collation_name:
    any_name
;

foreign_table:
    any_name
;

index_name:
    any_name
;

trigger_name:
    any_name
;

view_name:
    any_name
;

module_name:
    any_name
;

pragma_name:
    any_name
;

savepoint_name:
    any_name
;

table_alias:
    any_name
;

transaction_name:
    any_name
;

window_name:
    any_name
;

alias:
    any_name
;

filename:
    any_name
;

base_window_name:
    any_name
;

simple_func:
    any_name
;

aggregate_func:
    any_name
;

table_function_name:
    any_name
;

any_name:
    IDENTIFIER
    | keyword
    | STRING_LITERAL
    | OPEN_PAR any_name CLOSE_PAR
;
