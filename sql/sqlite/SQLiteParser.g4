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
// $antlr-format alignColons hanging

parser grammar SQLiteParser;

options {
    tokenVocab = SQLiteLexer;
}

parse
    : sql_stmt_list EOF
;

sql_stmt_list
    : sql_stmt? (SCOL sql_stmt?)*
;

sql_stmt
    : (EXPLAIN_ (QUERY_ PLAN_)?)? (
        alter_table_stmt
        | analyze_stmt
        | attach_stmt
        | begin_stmt
        | commit_stmt
        | create_index_stmt
        | create_table_stmt
        | create_trigger_stmt
        | create_view_stmt
        | create_virtual_table_stmt
        | delete_stmt
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
        | vacuum_stmt
    )
;

alter_table_stmt
    : ALTER_ TABLE_ (schema_name DOT)? table_name (
        RENAME_ (
            TO_ new_table_name = table_name
            | COLUMN_? old_column_name = column_name TO_ new_column_name = column_name
        )
        | ADD_ COLUMN_? column_def
        | DROP_ COLUMN_? column_name
    )
;

analyze_stmt
    : ANALYZE_ (schema_name | (schema_name DOT)? table_or_index_name)?
;

attach_stmt
    : ATTACH_ DATABASE_? expr AS_ schema_name
;

begin_stmt
    : BEGIN_ (DEFERRED_ | IMMEDIATE_ | EXCLUSIVE_)? TRANSACTION_?
;

commit_stmt
    : (COMMIT_ | END_) TRANSACTION_?
;

rollback_stmt
    : ROLLBACK_ TRANSACTION_? (TO_ SAVEPOINT_? savepoint_name)?
;

savepoint_stmt
    : SAVEPOINT_ savepoint_name
;

release_stmt
    : RELEASE_ SAVEPOINT_? savepoint_name
;

create_index_stmt
    : CREATE_ UNIQUE_? INDEX_ (IF_ NOT_ EXISTS_)? (schema_name DOT)? index_name ON_ table_name OPEN_PAR indexed_column (
        COMMA indexed_column
    )* CLOSE_PAR (WHERE_ expr)?
;

// Differs from syntax diagram because column_name is already a subset of expr
indexed_column
    : expr (COLLATE_ collation_name)? asc_desc?
;

create_table_stmt
    : CREATE_ (TEMP_ | TEMPORARY_)? TABLE_ (IF_ NOT_ EXISTS_)? (schema_name DOT)? table_name (
        OPEN_PAR column_def (COMMA column_def)*? (COMMA table_constraint)* CLOSE_PAR table_options?
        | AS_ select_stmt
    )
;

table_options
    : (WITHOUT_ ROWID_ | STRICT_) (COMMA (WITHOUT_ ROWID_ | STRICT_))*
;

column_def
    : column_name type_name? column_constraint*
;

type_name
    : name+? (
        OPEN_PAR signed_number CLOSE_PAR
        | OPEN_PAR signed_number COMMA signed_number CLOSE_PAR
    )?
;

column_constraint
    : (CONSTRAINT_ name)? (
        PRIMARY_ KEY_ asc_desc? conflict_clause? AUTOINCREMENT_?
        | (NOT_? NULL_ | UNIQUE_) conflict_clause?
        | CHECK_ OPEN_PAR expr CLOSE_PAR
        | DEFAULT_ (signed_number | literal_value | OPEN_PAR expr CLOSE_PAR)
        | COLLATE_ collation_name
        | foreign_key_clause
        | (GENERATED_ ALWAYS_)? AS_ OPEN_PAR expr CLOSE_PAR (STORED_ | VIRTUAL_)?
    )
;

signed_number
    : (PLUS | MINUS)? NUMERIC_LITERAL
;

table_constraint
    : (CONSTRAINT_ name)? (
        (PRIMARY_ KEY_ | UNIQUE_) OPEN_PAR indexed_column (COMMA indexed_column)* CLOSE_PAR conflict_clause?
        | CHECK_ OPEN_PAR expr CLOSE_PAR
        | FOREIGN_ KEY_ OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR foreign_key_clause
    )
;

foreign_key_clause
    : REFERENCES_ foreign_table (OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR)? (
        ON_ (DELETE_ | UPDATE_) (
            SET_ (NULL_ | DEFAULT_)
            | CASCADE_
            | RESTRICT_
            | NO_ ACTION_
        )
        | MATCH_ name
    )* (NOT_? DEFERRABLE_ (INITIALLY_ (DEFERRED_ | IMMEDIATE_))?)?
;

conflict_clause
    : ON_ CONFLICT_ (ROLLBACK_ | ABORT_ | FAIL_ | IGNORE_ | REPLACE_)
;

create_trigger_stmt
    : CREATE_ (TEMP_ | TEMPORARY_)? TRIGGER_ (IF_ NOT_ EXISTS_)? (schema_name DOT)? trigger_name (
        BEFORE_
        | AFTER_
        | INSTEAD_ OF_
    )? (DELETE_ | INSERT_ | UPDATE_ (OF_ column_name (COMMA column_name)*)?) ON_ table_name (
        FOR_ EACH_ ROW_
    )? (WHEN_ expr)? BEGIN_ (
        (update_stmt | insert_stmt | delete_stmt | select_stmt) SCOL
    )+ END_
;

create_view_stmt
    : CREATE_ (TEMP_ | TEMPORARY_)? VIEW_ (IF_ NOT_ EXISTS_)? (schema_name DOT)? view_name (
        OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
    )? AS_ select_stmt
;

create_virtual_table_stmt
    : CREATE_ VIRTUAL_ TABLE_ (IF_ NOT_ EXISTS_)? (schema_name DOT)? table_name USING_ module_name (
        OPEN_PAR module_argument (COMMA module_argument)* CLOSE_PAR
    )?
;

with_clause
    : WITH_ RECURSIVE_? common_table_expression (COMMA common_table_expression)*
;

common_table_expression
    : cte_table_name AS_ (NOT_? MATERIALIZED_)? OPEN_PAR select_stmt CLOSE_PAR
;

cte_table_name
    : table_name (OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR)?
;

// Merged with delete_stmt_limited, which is an optional extension of delete_stmt
delete_stmt
    : with_clause? DELETE_ FROM_ qualified_table_name (WHERE_ expr)? returning_clause? order_clause? limit_clause?
;

detach_stmt
    : DETACH_ DATABASE_? schema_name
;

drop_stmt
    : DROP_ object = (INDEX_ | TABLE_ | TRIGGER_ | VIEW_) (IF_ EXISTS_)? (
        schema_name DOT
    )? any_name
;

// expr has been split into multiple levels to ensure precedence
/*
expr
    : literal_value
    | BIND_PARAMETER
    | ((schema_name DOT)? table_name DOT)? column_name
    | (MINUS | PLUS | TILDE) expr
    | expr COLLATE_ collation_name
    | expr (PIPE2 | JPTR | JPTR2) expr
    | expr (STAR | DIV | MOD) expr
    | expr (PLUS | MINUS) expr
    | expr (LT2 | GT2 | AMP | PIPE) expr
    | expr (LT | LT_EQ | GT | GT_EQ) expr
    | expr (ASSIGN | EQ | NOT_EQ1 | NOT_EQ2) expr
    | expr IS_ NOT_? (DISTINCT_ FROM_)? expr
    | expr NOT_? BETWEEN_ expr AND_ expr
    | expr NOT_? IN_ (
        OPEN_PAR (select_stmt | expr (COMMA expr)*)? CLOSE_PAR
        | (schema_name DOT)? table_name
        | (schema_name DOT)? table_function_name OPEN_PAR (expr (COMMA expr)*)? CLOSE_PAR
    )
    | expr NOT_? ((LIKE_ expr (ESCAPE_ expr)?) | (GLOB_ | REGEXP_ | MATCH_) expr)
    | expr (ISNULL_ | NOTNULL_ | NOT_ NULL_)
    | NOT_ expr
    | expr AND_ expr
    | expr OR_ expr
    | function_name OPEN_PAR ((DISTINCT_? expr (COMMA expr)* order_clause?) | STAR)? CLOSE_PAR filter_clause? over_clause?
    | OPEN_PAR expr (COMMA expr)* CLOSE_PAR
    | CAST_ OPEN_PAR expr AS_ type_name CLOSE_PAR
    | ((NOT_)? EXISTS_)? OPEN_PAR select_stmt CLOSE_PAR
    | CASE_ expr? (WHEN_ expr THEN_ expr)+ (ELSE_ expr)? END_
    | raise_function
;
*/

expr
    : expr_recursive
;

expr_recursive
    : function_name OPEN_PAR (DISTINCT_? expr (COMMA expr)* order_clause? | STAR)? CLOSE_PAR percentile_clause? filter_clause?
        over_clause?
    | OPEN_PAR expr (COMMA expr)* CLOSE_PAR
    | CAST_ OPEN_PAR expr AS_ type_name CLOSE_PAR
    | CASE_ expr? (WHEN_ expr THEN_ expr)+ (ELSE_ expr)? END_
    | expr_or
;

expr_or
    : expr_and (OR_ expr_and)*
;

expr_and
    : expr_not (AND_ expr_not)*
;

expr_not
    : NOT_* expr_binary
;

expr_binary
    : expr_comparison (
        (ASSIGN | EQ | NOT_EQ1 | NOT_EQ2) expr_comparison
        | IS_ NOT_? (DISTINCT_ FROM_)? expr_comparison
        | NOT_? BETWEEN_ expr_comparison AND_ expr_comparison
        | NOT_? IN_ (
            OPEN_PAR (select_stmt | expr_comparison (COMMA expr_comparison)*)? CLOSE_PAR
            | (schema_name DOT)? table_name
            | (schema_name DOT)? table_function_name OPEN_PAR (
                expr_comparison (COMMA expr_comparison)*
            )? CLOSE_PAR
        )
        | NOT_? (
            LIKE_ expr_comparison (ESCAPE_ expr_comparison)?
            | (GLOB_ | REGEXP_ | MATCH_) expr_comparison
        )
        | ISNULL_
        | NOTNULL_
        | NOT_ NULL_
    )*
;

expr_comparison
    : expr_bitwise ((LT | LT_EQ | GT | GT_EQ) expr_bitwise)*
;

expr_bitwise
    : expr_addition ((LT2 | GT2 | AMP | PIPE) expr_addition)*
;

expr_addition
    : expr_multiplication ((PLUS | MINUS) expr_multiplication)*
;

expr_multiplication
    : expr_string ((STAR | DIV | MOD) expr_string)*
;

expr_string
    : expr_collate ((PIPE2 | JPTR | JPTR2) expr_collate)*
;

expr_collate
    : expr_unary (COLLATE_ collation_name)*
;

expr_unary
    : (MINUS | PLUS | TILDE)* expr_base
;

expr_base
    : literal_value
    | BIND_PARAMETER
    | (schema_name DOT)? table_name DOT column_name
    | column_name_excluding_string
    | (NOT_? EXISTS_)? OPEN_PAR select_stmt CLOSE_PAR
    | raise_function
;

raise_function
    : RAISE_ OPEN_PAR (IGNORE_ | (ROLLBACK_ | ABORT_ | FAIL_) COMMA error_message) CLOSE_PAR
;

literal_value
    : NUMERIC_LITERAL
    | STRING_LITERAL
    | BLOB_LITERAL
    | NULL_
    | TRUE_
    | FALSE_
    | CURRENT_TIME_
    | CURRENT_DATE_
    | CURRENT_TIMESTAMP_
;

percentile_clause
    : WITHIN_ GROUP_ OPEN_PAR ORDER_ BY_ expr CLOSE_PAR
;

value_row
    : OPEN_PAR expr (COMMA expr)* CLOSE_PAR
;

values_clause
    : VALUES_ value_row (COMMA value_row)*
;

// Differs from syntax diagram because values_clause is already a subset of select_stmt
insert_stmt
    : with_clause? (
        INSERT_
        | REPLACE_
        | INSERT_ OR_ (REPLACE_ | ROLLBACK_ | ABORT_ | FAIL_ | IGNORE_)
    ) INTO_ (schema_name DOT)? table_name (AS_ table_alias)? (
        OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
    )? (select_stmt upsert_clause* | DEFAULT_ VALUES_) returning_clause?
;

returning_clause
    : RETURNING_ (STAR | expr (AS_? column_alias)?) (
        COMMA (STAR | expr (AS_? column_alias)?)
    )*
;

upsert_clause
    : ON_ CONFLICT_ (
        OPEN_PAR indexed_column (COMMA indexed_column)* CLOSE_PAR (WHERE_ expr)?
    )? DO_ (
        NOTHING_
        | UPDATE_ SET_ (column_name | column_name_list) ASSIGN expr (
            COMMA (column_name | column_name_list) ASSIGN expr
        )* (WHERE_ expr)?
    )
;

pragma_stmt
    : PRAGMA_ (schema_name DOT)? pragma_name (
        ASSIGN pragma_value
        | OPEN_PAR pragma_value CLOSE_PAR
    )?
;

pragma_value
    : signed_number
    | name
    | STRING_LITERAL
;

reindex_stmt
    : REINDEX_ (collation_name | (schema_name DOT)? (table_name | index_name))?
;

select_stmt
    : with_clause? select_core (compound_operator select_core)* order_clause? limit_clause?
;

join_clause
    : table_or_subquery (join_operator table_or_subquery join_constraint?)*
;

// Differs from syntax diagram because comma-separated table_or_subquery is already a subset of join_clause
select_core
    : SELECT_ (DISTINCT_ | ALL_)? result_column (COMMA result_column)* (
        FROM_ join_clause
    )? (WHERE_ where_expr = expr)? (
        GROUP_ BY_ group_by_expr += expr (COMMA group_by_expr += expr)* (
            HAVING_ having_expr = expr
        )?
    )? (WINDOW_ window_name AS_ window_defn (COMMA window_name AS_ window_defn)*)?
    | values_clause
;

// Differs from syntax diagram because comma-separated table_or_subquery is already a subset of join_clause
table_or_subquery
    : (schema_name DOT)? table_name (AS_ table_alias | table_alias_excluding_joins)? (
        INDEXED_ BY_ index_name
        | NOT_ INDEXED_
    )?
    | (schema_name DOT)? table_function_name OPEN_PAR expr (COMMA expr)* CLOSE_PAR (
        AS_? table_alias
    )?
    | OPEN_PAR join_clause CLOSE_PAR
    | OPEN_PAR select_stmt CLOSE_PAR (AS_? table_alias)?
;

result_column
    : STAR
    | table_name DOT STAR
    | expr (AS_? column_alias)?
;

join_operator
    : COMMA
    | NATURAL_? ((LEFT_ | RIGHT_ | FULL_) OUTER_? | INNER_ | CROSS_)? JOIN_
;

join_constraint
    : ON_ expr
    | USING_ OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
;

compound_operator
    : UNION_ ALL_?
    | INTERSECT_
    | EXCEPT_
;

// Differs from syntax diagram because comma-separated table_or_subquery is already a subset of join_clause
// Merged with update_stmt_limited, which is an optional extension of update_stmt
update_stmt
    : with_clause? UPDATE_ (OR_ (ROLLBACK_ | ABORT_ | REPLACE_ | FAIL_ | IGNORE_))? qualified_table_name SET_ (
        column_name
        | column_name_list
    ) ASSIGN expr (COMMA (column_name | column_name_list) ASSIGN expr)* (
        FROM_ join_clause
    )? (WHERE_ expr)? returning_clause? order_clause? limit_clause?
;

column_name_list
    : OPEN_PAR column_name (COMMA column_name)* CLOSE_PAR
;

qualified_table_name
    : (schema_name DOT)? table_name (AS_ alias)? (
        INDEXED_ BY_ index_name
        | NOT_ INDEXED_
    )?
;

vacuum_stmt
    : VACUUM_ schema_name? (INTO_ filename)?
;

filter_clause
    : FILTER_ OPEN_PAR WHERE_ expr CLOSE_PAR
;

window_defn
    : OPEN_PAR base_window_name? (PARTITION_ BY_ expr (COMMA expr)*)? order_clause? frame_spec? CLOSE_PAR
;

over_clause
    : OVER_ (
        window_name
        | OPEN_PAR base_window_name? (PARTITION_ BY_ expr (COMMA expr)*)? order_clause? frame_spec? CLOSE_PAR
    )
;

frame_spec
    : frame_clause (EXCLUDE_ (NO_ OTHERS_ | CURRENT_ ROW_ | GROUP_ | TIES_))?
;

frame_clause
    : (RANGE_ | ROWS_ | GROUPS_) (
        frame_single
        | BETWEEN_ frame_left AND_ frame_right
    )
;

order_clause
    : ORDER_ BY_ ordering_term (COMMA ordering_term)*
;

limit_clause
    : LIMIT_ expr ((OFFSET_ | COMMA) expr)?
;

ordering_term
    : expr (COLLATE_ collation_name)? asc_desc? (NULLS_ (FIRST_ | LAST_))?
;

asc_desc
    : ASC_
    | DESC_
;

frame_left
    : expr PRECEDING_
    | expr FOLLOWING_
    | CURRENT_ ROW_
    // | UNBOUNDED_ PRECEDING_ // Special case of expr PRECEDING_
;

frame_right
    : expr PRECEDING_
    | expr FOLLOWING_
    | CURRENT_ ROW_
    // | UNBOUNDED_ FOLLOWING_ // Special case of expr FOLLOWING_
;

frame_single
    : expr PRECEDING_
    | CURRENT_ ROW_
    // | UNBOUNDED_ PRECEDING_ // Special case of expr PRECEDING_
;

error_message
    : expr
;

filename
    : expr
;

module_argument
    : module_argument_outer*
;

module_argument_outer
    : ~(OPEN_PAR | CLOSE_PAR | UNEXPECTED_CHAR | COMMA)
    | OPEN_PAR module_argument_inner* CLOSE_PAR
;

module_argument_inner
    : ~(OPEN_PAR | CLOSE_PAR | UNEXPECTED_CHAR)
    | OPEN_PAR module_argument_inner* CLOSE_PAR
;

// Only some keywords are allowed as identifiers.
fallback_excluding_conflicts
    : ABORT_
    | ACTION_
    // | ADD_
    | AFTER_
    // | ALL_
    // | ALTER_
    | ALWAYS_
    | ANALYZE_
    // | AND_
    // | AS_
    | ASC_
    | ATTACH_
    // | AUTOINCREMENT_
    | BEFORE_
    | BEGIN_
    // | BETWEEN_
    | BY_
    | CASCADE_
    // | CASE_
    | CAST_
    // | CHECK_
    // | COLLATE_
    | COLUMN_
    // | COMMIT_
    | CONFLICT_
    // | CONSTRAINT_
    // | CREATE_
    // | CROSS_
    | CURRENT_
    | CURRENT_DATE_
    | CURRENT_TIME_
    | CURRENT_TIMESTAMP_
    | DATABASE_
    // | DEFAULT_
    // | DEFERRABLE_
    | DEFERRED_
    // | DELETE_
    | DESC_
    | DETACH_
    // | DISTINCT_
    | DO_
    // | DROP_
    | EACH_
    // | ELSE_
    | END_
    // | ESCAPE_
    | EXCEPT_
    | EXCLUDE_
    | EXCLUSIVE_
    // | EXISTS_
    | EXPLAIN_
    | FAIL_
    | FALSE_ // FALSE is handled as a special-case indentifier
    // | FILTER_
    | FIRST_
    | FOLLOWING_
    | FOR_
    // | FOREIGN_
    // | FROM_
    // | FULL_
    | GENERATED_
    | GLOB_
    // | GROUP_
    | GROUPS_
    // | HAVING_
    | IF_
    | IGNORE_
    | IMMEDIATE_
    // | IN_
    // | INDEX_
    // | INDEXED_
    | INITIALLY_
    // | INNER_
    // | INSERT_
    | INSTEAD_
    | INTERSECT_
    // | INTO_
    // | IS_
    // | ISNULL_
    // | JOIN_
    | KEY_
    | LAST_
    // | LEFT_
    | LIKE_
    // | LIMIT_
    | MATCH_
    | MATERIALIZED_
    // | NATURAL_
    | NO_
    // | NOT_
    // | NOTHING_
    // | NOTNULL_
    // | NULL_
    | NULLS_
    | OF_
    | OFFSET_
    // | ON_
    // | OR_
    // | ORDER_
    | OTHERS_
    // | OUTER_
    // | OVER_
    | PARTITION_
    | PLAN_
    | PRAGMA_
    | PRECEDING_
    // | PRIMARY_
    | QUERY_
    // | RAISE_
    | RANGE_
    | RECURSIVE_
    // | REFERENCES_
    | REGEXP_
    | REINDEX_
    | RELEASE_
    | RENAME_
    | REPLACE_
    | RESTRICT_
    // | RETURNING_
    // | RIGHT_
    | ROLLBACK_
    | ROW_
    | ROWID_ // ROWID is handled as a special-case indentifier
    | ROWS_
    | SAVEPOINT_
    // | SELECT_
    // | SET_
    | STORED_ // STORED is handled as a special-case indentifier
    | STRICT_ // STRICT is handled as a special-case indentifier
    // | TABLE_
    | TEMP_
    | TEMPORARY_
    // | THEN_
    | TIES_
    // | TO_
    // | TRANSACTION_
    | TRIGGER_
    | TRUE_ // TRUE is handled as a special-case indentifier
    | UNBOUNDED_
    | UNION_
    // | UNIQUE_
    // | UPDATE_
    // | USING_
    | VACUUM_
    // | VALUES_
    | VIEW_
    | VIRTUAL_
    // | WHEN_
    // | WHERE_
    // | WINDOW_
    | WITH_
    | WITHIN_
    | WITHOUT_
;

join_keyword
    : CROSS_
    | FULL_
    | INDEXED_
    | INNER_
    | LEFT_
    | NATURAL_
    | OUTER_
    | RIGHT_
;

fallback
    : fallback_excluding_conflicts
    | join_keyword
    | RAISE_
;

name
    : any_name
;

function_name
    : any_name_excluding_raise
;

schema_name
    : any_name
;

table_name
    : any_name
;

table_or_index_name
    : any_name
;

column_name
    : any_name
;

column_name_excluding_string
    : any_name_excluding_string
;

column_alias
    : any_name
;

collation_name
    : any_name
;

foreign_table
    : any_name
;

index_name
    : any_name
;

trigger_name
    : any_name
;

view_name
    : any_name
;

module_name
    : any_name
;

pragma_name
    : any_name
;

savepoint_name
    : any_name
;

table_alias
    : any_name
;

table_alias_excluding_joins
    : any_name_excluding_joins
;

window_name
    : any_name
;

alias
    : any_name
;

base_window_name
    : any_name
;

table_function_name
    : any_name
;

any_name_excluding_raise
    : IDENTIFIER
    | fallback_excluding_conflicts
    | join_keyword
    | STRING_LITERAL
;

any_name_excluding_joins
    : IDENTIFIER
    | fallback_excluding_conflicts
    | RAISE_
    | STRING_LITERAL
;

any_name_excluding_string
    : IDENTIFIER
    | fallback
;

any_name
    : IDENTIFIER
    | fallback
    | STRING_LITERAL
;

// Orphans (Not ever parsed, merely provided by https://sqlite.org/syntaxdiagrams.html as partial descriptions of other rules)

/*
factored_select_stmt
    : select_stmt
;

simple_select_stmt
    : with_clause? select_core order_clause? limit_clause?
;

compound_select_stmt
    : with_clause? select_core ((UNION_ ALL_? | INTERSECT_ | EXCEPT_) select_core)+ order_clause? limit_clause?
;

recursive_cte
    : cte_table_name AS_ OPEN_PAR initial_select UNION_ ALL_? recursive_select CLOSE_PAR
;

initial_select
    : select_stmt
;

recursive_select
    : select_stmt
;

simple_function_invocation
    : simple_func OPEN_PAR (expr (COMMA expr)* | STAR) CLOSE_PAR
;

aggregate_function_invocation
    : aggregate_func OPEN_PAR (DISTINCT_? expr (COMMA expr)* order_clause? | STAR)? CLOSE_PAR filter_clause?
;

window_function_invocation
    : window_func OPEN_PAR (expr (COMMA expr)* | STAR)? CLOSE_PAR filter_clause? OVER_ (
        window_defn
        | window_name
    )
;

simple_func
    : any_name
;

aggregate_func
    : any_name
;

window_func
    : any_name
;
*/
