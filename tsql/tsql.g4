/*
T-SQL (Transact-SQL, MSSQL) grammar.
The MIT License (MIT).
Copyright (c) 2015-2016, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2016, Scott Ure (scott@redstormsoftware.com).
Copyright (c) 2016, Rui Zhang (ruizhang.ccs@gmail.com).
Copyright (c) 2016, Marcus Henriksson (kuseman80@gmail.com).

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

grammar tsql;

tsql_file
    : batch* EOF
    ;

batch
    : sql_clauses go_statement?
    ;

sql_clauses
    : (sql_clause SEMI?)+
    ;

sql_clause
    : dml_clause

    | ddl_clause

    | cfl_statement

    | another_statement
    ;

// Data Manipulation Language: https://msdn.microsoft.com/en-us/library/ff848766(v=sql.120).aspx
dml_clause
    : delete_statement
    | insert_statement
    | select_statement
    | update_statement
    ;

// Data Definition Language: https://msdn.microsoft.com/en-us/library/ff848799.aspx)
ddl_clause
    : //create_function
    create_database
    | create_index
    | create_procedure
    | create_statistics
    | create_table
    | create_type
    | create_view

    | alter_table
    | alter_database

    | drop_index
    | drop_procedure
    | drop_statistics
    | drop_table
	| drop_type
    | drop_view
    ;

// Control-of-Flow Language: https://msdn.microsoft.com/en-us/library/ms174290.aspx
// Labels for better AST traverse.
cfl_statement
    // https://msdn.microsoft.com/en-us/library/ms190487.aspx
    : BEGIN ';'? sql_clauses? END ';'?               #block_statement
    // https://msdn.microsoft.com/en-us/library/ms181271.aspx
    | BREAK ';'?                                     #break_statement
    // https://msdn.microsoft.com/en-us/library/ms174366.aspx
    | CONTINUE ';'?                                  #continue_statement
    // https://msdn.microsoft.com/en-us/library/ms180188.aspx
    | GOTO id ';'?                                   #goto_statement
    | id ':' ';'?                                    #goto_statement
    // https://msdn.microsoft.com/en-us/library/ms182717.aspx
    | IF search_condition sql_clause (ELSE sql_clause)? ';'?  #if_statement
    // https://msdn.microsoft.com/en-us/library/ms174998.aspx
    | RETURN expression? ';'?                        #return_statement
    // https://msdn.microsoft.com/en-us/library/ee677615.aspx
    | THROW (
      error_number=(DECIMAL | LOCAL_ID) ',' message=(STRING | LOCAL_ID) ','
      state=(DECIMAL | LOCAL_ID))? ';'?              #throw_statement
    // https://msdn.microsoft.com/en-us/library/ms175976.aspx
    | BEGIN TRY ';'? try_clauses=sql_clauses? END TRY ';'?
      BEGIN CATCH ';'? catch_clauses=sql_clauses? END CATCH ';'?                    #try_catch_statement
    // https://msdn.microsoft.com/en-us/library/ms187331.aspx
    | WAITFOR (DELAY | TIME)  expression ';'?                                       #waitfor_statement
    // https://msdn.microsoft.com/en-us/library/ms178642.aspx
    | WHILE search_condition (sql_clause | BREAK ';'? | CONTINUE ';'?)              #while_statement
    // https://msdn.microsoft.com/en-us/library/ms176047.aspx.
    | PRINT expression ';'?                                                         #print_statement
    // https://msdn.microsoft.com/en-us/library/ms178592.aspx
    | RAISERROR '(' msg=(DECIMAL | STRING | LOCAL_ID) ',' severity=constant_LOCAL_ID ','
        state=constant_LOCAL_ID (',' constant_LOCAL_ID)* ')' ';'?                   #raiseerror_statement
    ;

another_statement
    : declare_statement
    | cursor_statement
    | execute_statement
    | security_statement
    | set_statement
    | transaction_statement
    | use_statement
    ;

// DML

// https://msdn.microsoft.com/en-us/library/ms189835.aspx
delete_statement
    : with_expression?
      DELETE (TOP '(' expression ')' PERCENT?)?
      FROM? delete_statement_from
      insert_with_table_hints?
      output_clause?
      (FROM table_sources)?
      (WHERE (search_condition | CURRENT OF (GLOBAL? cursor_name | cursor_var=LOCAL_ID)))?
      for_clause? option_clause? ';'?
    ;

delete_statement_from
    : table_alias
    | ddl_object
    | rowset_function_limited
    | table_var=LOCAL_ID
    ;

// https://msdn.microsoft.com/en-us/library/ms174335.aspx
insert_statement
    : with_expression?
      INSERT (TOP '(' expression ')' PERCENT?)?
      INTO? (ddl_object | rowset_function_limited)
      insert_with_table_hints?
      ('(' column_name_list ')')?
      output_clause?
      insert_statement_value
      for_clause? option_clause? ';'?
    ;

insert_statement_value
    : table_value_constructor
    | derived_table
    | execute_statement
    | DEFAULT VALUES
    ;

// https://msdn.microsoft.com/en-us/library/ms189499.aspx
select_statement
    : with_expression? query_expression order_by_clause? for_clause? option_clause? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms177523.aspx
update_statement
    : with_expression?
      UPDATE (TOP '(' expression ')' PERCENT?)?
      (ddl_object | rowset_function_limited)
      with_table_hints?
      SET update_elem (',' update_elem)*
      output_clause?
      (FROM table_sources)?
      (WHERE (search_condition_list | CURRENT OF (GLOBAL? cursor_name | cursor_var=LOCAL_ID)))?
      for_clause? option_clause? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms177564.aspx
output_clause
    : OUTPUT output_dml_list_elem (',' output_dml_list_elem)*
      (INTO (LOCAL_ID | table_name) ('(' column_name_list ')')? )?
    ;

output_dml_list_elem
    : (output_column_name | expression) (AS? column_alias)?  // TODO: scalar_expression
    ;

output_column_name
    : (DELETED | INSERTED | table_name) '.' ('*' | id)
    | DOLLAR_ACTION
    ;

// DDL

// https://msdn.microsoft.com/en-ie/library/ms176061.aspx
create_database
    : CREATE DATABASE (database=id)
    ( CONTAINMENT '=' ( NONE | PARTIAL ) )?
    ( ON PRIMARY? database_file_spec ( ',' database_file_spec )* )?
    ( LOG ON database_file_spec ( ',' database_file_spec )* )?
    ( COLLATE collation_name = id )?
    ( WITH  create_database_option ( ',' create_database_option )* )?
    ;

// https://msdn.microsoft.com/en-us/library/ms188783.aspx
create_index
    : CREATE UNIQUE? clustered? INDEX id ON table_name_with_hint '(' column_name_list (ASC | DESC)? ')'
    (index_options)?
    (ON id)?
    ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms187926(v=sql.120).aspx
create_procedure
    : CREATE proc=(PROC | PROCEDURE) func_proc_name (';' DECIMAL)?
      ('('? procedure_param (',' procedure_param)* ')'?)?
      (WITH procedure_option (',' procedure_option)*)?
      (FOR REPLICATION)? AS sql_clauses
    ;

procedure_param
    : LOCAL_ID (id '.')? AS? data_type VARYING? ('=' default_val=default_value)? (OUT | OUTPUT | READONLY)?
    ;

procedure_option
    : ENCRYPTION
    | RECOMPILE
    | execute_clause
    ;

// https://msdn.microsoft.com/en-us/library/ms188038.aspx
create_statistics
    : CREATE STATISTICS id ON table_name_with_hint '(' column_name_list ')'
      (WITH (FULLSCAN | SAMPLE DECIMAL (PERCENT | ROWS) | STATS_STREAM)
            (',' NORECOMPUTE)? (',' INCREMENTAL EQUAL on_off)? )? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms174979.aspx
create_table
    : CREATE TABLE table_name '(' column_def_table_constraints ','? ')' (ON id | DEFAULT)? (TEXTIMAGE_ON id | DEFAULT)?';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms187956.aspx
create_view
    : CREATE VIEW simple_name ('(' column_name_list ')')?
      (WITH view_attribute (',' view_attribute)*)?
      AS select_statement (WITH CHECK OPTION)? ';'?
    ;

view_attribute
    : ENCRYPTION | SCHEMABINDING | VIEW_METADATA
    ;

// https://msdn.microsoft.com/en-us/library/ms190273.aspx
alter_table
    : ALTER TABLE table_name (SET '(' LOCK_ESCALATION '=' (AUTO | TABLE | DISABLE) ')'
                             | ADD column_def_table_constraint
                             | DROP CONSTRAINT constraint=id
                             | WITH CHECK ADD CONSTRAINT constraint=id FOREIGN KEY '(' fk = column_name_list ')' REFERENCES table_name '(' pk = column_name_list')'
                             | CHECK CONSTRAINT constraint=id
                             )
                             ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms174269.aspx
alter_database
    : ALTER DATABASE (database=id | CURRENT)
      (MODIFY NAME '=' new_name=id | COLLATE collation=id | SET database_optionspec (WITH termination)? ) ';'?
    ;

// https://msdn.microsoft.com/en-us/library/bb522682.aspx
// Runtime check.
database_optionspec
    :  auto_option
      | change_tracking_option
      | containment_option
      | cursor_option
//      | database_mirroring_option
      | date_correlation_optimization_option
      | db_encryption_option
      | db_state_option
      | db_update_option
      | db_user_access_option
      | delayed_durability_option
      | external_access_option
      | FILESTREAM database_filestream_option
      | HADR_options
      | mixed_page_allocation_option
      | parameterization_option
//      | query_store_options
      | recovery_option
//      | remote_data_archive_option
      | service_broker_option
      | snapshot_option
      | sql_option
      | target_recovery_time_option
      | termination
    ;

auto_option:
     AUTO_CLOSE on_off
      | AUTO_CREATE_STATISTICS  OFF | ON ( INCREMENTAL EQUAL  ON | OFF  )
      | AUTO_SHRINK  on_off
      | AUTO_UPDATE_STATISTICS on_off
      | AUTO_UPDATE_STATISTICS_ASYNC  (ON | OFF )
    ;

change_tracking_option:
    CHANGE_TRACKING  EQUAL ( OFF | ON (change_tracking_option_list (',' change_tracking_option_list)*)*  )    
    ;

change_tracking_option_list:
     AUTO_CLEANUP EQUAL on_off
     | CHANGE_RETENTION EQUAL ( DAYS | HOURS | MINUTES )
    ;

containment_option:
     CONTAINMENT EQUAL ( NONE | PARTIAL )
    ;

cursor_option:
    CURSOR_CLOSE_ON_COMMIT on_off
    | CURSOR_DEFAULT ( LOCAL | GLOBAL )
  ;

/* Will visit later
database_mirroring_option:
     ALTER DATABASE Database Mirroring
    ;
*/

date_correlation_optimization_option:
    DATE_CORRELATION_OPTIMIZATION on_off
    ;

db_encryption_option:
     ENCRYPTION on_off
    ;
db_state_option:
     ( ONLINE | OFFLINE | EMERGENCY )
    ;

db_update_option:
    READ_ONLY | READ_WRITE
    ;

db_user_access_option:
    ( SINGLE_USER | RESTRICTED_USER | MULTI_USER )
    ;
delayed_durability_option:
     DELAYED_DURABILITY EQUAL ( DISABLED | ALLOWED | FORCED )
    ;

external_access_option:
   DB_CHAINING on_off  
  | TRUSTWORTHY on_off  
  | DEFAULT_LANGUAGE EQUAL ( id | STRING )  
  | DEFAULT_FULLTEXT_LANGUAGE EQUAL ( id | STRING )  
  | NESTED_TRIGGERS EQUAL ( OFF | ON )  
  | TRANSFORM_NOISE_WORDS EQUAL ( OFF | ON )  
  | TWO_DIGIT_YEAR_CUTOFF EQUAL DECIMAL
  ;

HADR_options:
    ALTER DATABASE SET HADR
    ;

mixed_page_allocation_option:
     MIXED_PAGE_ALLOCATION ( OFF | ON )
    ;

parameterization_option:
     PARAMETERIZATION ( SIMPLE | FORCED )
    ;

/* Will visit later
query_store_options:
    ;
*/

recovery_option:
     RECOVERY ( FULL | BULK_LOGGED | SIMPLE )
     | TORN_PAGE_DETECTION on_off
     | PAGE_VERIFY ( CHECKSUM | TORN_PAGE_DETECTION | NONE )
    ;

/*Will visit later
remote_data_archive_option:
    ;
*/

service_broker_option:
    ENABLE_BROKER  
    | DISABLE_BROKER  
    | NEW_BROKER  
    | ERROR_BROKER_CONVERSATIONS  
    | HONOR_BROKER_PRIORITY on_off
  ;
snapshot_option:
   ALLOW_SNAPSHOT_ISOLATION on_off  
  | READ_COMMITTED_SNAPSHOT (ON | OFF )  
  | MEMORY_OPTIMIZED_ELEVATE_TO_SNAPSHOT = (ON | OFF )
  ;

sql_option:
  ANSI_NULL_DEFAULT on_off   
  | ANSI_NULLS on_off   
  | ANSI_PADDING on_off   
  | ANSI_WARNINGS on_off   
  | ARITHABORT on_off   
  | COMPATIBILITY_LEVEL EQUAL DECIMAL
  | CONCAT_NULL_YIELDS_NULL on_off   
  | NUMERIC_ROUNDABORT on_off   
  | QUOTED_IDENTIFIER on_off   
  | RECURSIVE_TRIGGERS on_off   
  ;

target_recovery_time_option:
     TARGET_RECOVERY_TIME EQUAL DECIMAL ( SECONDS | MINUTES )
    ;

termination:
    ROLLBACK AFTER seconds = DECIMAL
    | ROLLBACK IMMEDIATE   
    | NO_WAIT  
    ;

// https://msdn.microsoft.com/en-us/library/ms176118.aspx
drop_index
    : DROP INDEX (IF EXISTS)? name=id (ON table_name)? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms174969.aspx
drop_procedure
    : DROP PROCEDURE (IF EXISTS)? func_proc_name ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms175075.aspx
drop_statistics
    : DROP STATISTICS (table_name '.')? name=id ';'
    ;

// https://msdn.microsoft.com/en-us/library/ms173790.aspx
drop_table
    : DROP TABLE (IF EXISTS)? table_name ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms173492.aspx
drop_view
    : DROP VIEW (IF EXISTS)? simple_name (',' simple_name)* ';'?
    ;

create_type:
    CREATE TYPE name = simple_name FROM data_type default_value
    ;

drop_type:
    DROP TYPE ( IF EXISTS )? name = simple_name
    ;

rowset_function_limited
    : openquery
    | opendatasource
    ;

// https://msdn.microsoft.com/en-us/library/ms188427(v=sql.120).aspx
openquery
    : OPENQUERY '(' linked_server=id ',' query=STRING ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms179856.aspx
opendatasource
    : OPENDATASOURCE '(' provider=STRING ',' init=STRING ')'
     '.' (database=id)? '.' (scheme=id)? '.' (table=id)
    ;

// Other statements.

// https://msdn.microsoft.com/en-us/library/ms188927.aspx
declare_statement
    : DECLARE LOCAL_ID AS? table_type_definition ';'?
    | DECLARE declare_local (',' declare_local)* ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms181441(v=sql.120).aspx
cursor_statement
    // https://msdn.microsoft.com/en-us/library/ms175035(v=sql.120).aspx
    : CLOSE GLOBAL? cursor_name ';'?
    // https://msdn.microsoft.com/en-us/library/ms188782(v=sql.120).aspx
    | DEALLOCATE GLOBAL? cursor_name ';'?
    // https://msdn.microsoft.com/en-us/library/ms180169(v=sql.120).aspx
    | declare_cursor
    // https://msdn.microsoft.com/en-us/library/ms180152(v=sql.120).aspx
    | fetch_cursor
    // https://msdn.microsoft.com/en-us/library/ms190500(v=sql.120).aspx
    | OPEN GLOBAL? cursor_name ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms188332.aspx
execute_statement
    : EXECUTE (return_status=LOCAL_ID '=')? func_proc_name (execute_statement_arg (',' execute_statement_arg)*)? ';'?
    | EXECUTE '(' execute_var_string ('+' execute_var_string)* ')' (AS? (LOGIN | USER) '=' STRING)? ';'?
    ;

execute_statement_arg
    : (parameter=LOCAL_ID '=')? ((constant_LOCAL_ID | id) (OUTPUT | OUT)? | DEFAULT | NULL)
    ;

execute_var_string
    : LOCAL_ID
    | STRING
    ;

// https://msdn.microsoft.com/en-us/library/ff848791.aspx
security_statement
    // https://msdn.microsoft.com/en-us/library/ms188354.aspx
    : execute_clause ';'?
    // https://msdn.microsoft.com/en-us/library/ms187965.aspx
    | GRANT (ALL PRIVILEGES? | grant_permission ('(' column_name_list ')')?) (ON on_id=table_name)? TO (to_principal=id) (WITH GRANT OPTION)? (AS as_principal=id)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms178632.aspx
    | REVERT ('(' WITH COOKIE '=' LOCAL_ID ')')? ';'?
    ;

grant_permission
    : EXECUTE
    | VIEW id // DEFINITION
    | TAKE id // OWNERSHIP
    | CONTROL id? // SERVER
    | CREATE (TABLE | VIEW)
    | SHOWPLAN
    | IMPERSONATE
    | SELECT
    | REFERENCES
    | INSERT
    | ALTER (ANY? (id | DATABASE))?
    ;

// https://msdn.microsoft.com/en-us/library/ms190356.aspx
// https://msdn.microsoft.com/en-us/library/ms189484.aspx
set_statement
    : SET LOCAL_ID ('.' member_name=id)? '=' expression ';'?
    | SET LOCAL_ID assignment_operator expression ';'?
    | SET LOCAL_ID '='
      CURSOR declare_set_cursor_common (FOR (READ ONLY | UPDATE (OF column_name_list)?))? ';'?
    // https://msdn.microsoft.com/en-us/library/ms189837.aspx
    | set_special
    ;

// https://msdn.microsoft.com/en-us/library/ms174377.aspx
transaction_statement
    // https://msdn.microsoft.com/en-us/library/ms188386.aspx
    : BEGIN DISTRIBUTED (TRAN | TRANSACTION) (id | LOCAL_ID)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms188929.aspx
    | BEGIN (TRAN | TRANSACTION) ((id | LOCAL_ID) (WITH MARK STRING)?)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms190295.aspx
    | COMMIT (TRAN | TRANSACTION) ((id | LOCAL_ID) (WITH '(' DELAYED_DURABILITY EQUAL (OFF | ON) ')')?)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms178628.aspx
    | COMMIT WORK? ';'?
    // https://msdn.microsoft.com/en-us/library/ms181299.aspx
    | ROLLBACK (TRAN | TRANSACTION) (id | LOCAL_ID)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms174973.aspx
    | ROLLBACK WORK? ';'?
    // https://msdn.microsoft.com/en-us/library/ms188378.aspx
    | SAVE (TRAN | TRANSACTION) (id | LOCAL_ID)? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms188037.aspx
go_statement
    : GO (count=DECIMAL)?
    ;

// https://msdn.microsoft.com/en-us/library/ms188366.aspx
use_statement
    : USE database=id ';'?
    ;

execute_clause
    : EXECUTE AS clause=(CALLER | SELF | OWNER | STRING)
    ;

declare_local
    : LOCAL_ID AS? data_type ('=' expression)?
    ;

table_type_definition
    : TABLE '(' column_def_table_constraints ')'
    ;

column_def_table_constraints
    : column_def_table_constraint (','? column_def_table_constraint)*
    ;

column_def_table_constraint
    : column_definition
    | table_constraint
    ;

// https://msdn.microsoft.com/en-us/library/ms187742.aspx
column_definition
    : id (data_type | AS expression) (COLLATE id)? null_notnull?
      ((CONSTRAINT constraint=id)? DEFAULT constant_expression (WITH VALUES)?
       | IDENTITY ('(' seed=DECIMAL ',' increment=DECIMAL ')')? (NOT FOR REPLICATION)?)?
      ROWGUIDCOL?
      column_constraint*
    ;

// https://msdn.microsoft.com/en-us/library/ms186712.aspx
column_constraint
    :(CONSTRAINT id)? null_notnull?
      ((PRIMARY KEY | UNIQUE) clustered? index_options?
      | CHECK (NOT FOR REPLICATION)? '(' search_condition ')')
    ;

// https://msdn.microsoft.com/en-us/library/ms188066.aspx
table_constraint
    : (CONSTRAINT id)?
       ((PRIMARY KEY | UNIQUE) clustered? '(' column_name_list (ASC | DESC)? ')' index_options? (ON id)?
       | CHECK (NOT FOR REPLICATION)? '(' search_condition ')')
    ;

index_options
    : WITH '(' index_option (',' index_option)* ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms186869.aspx
// Id runtime checking. Id in (PAD_INDEX, FILLFACTOR, IGNORE_DUP_KEY, STATISTICS_NORECOMPUTE, ALLOW_ROW_LOCKS,
// ALLOW_PAGE_LOCKS, SORT_IN_TEMPDB, ONLINE, MAXDOP, DATA_COMPRESSION, ONLINE).
index_option
    : simple_id '=' (simple_id | on_off | DECIMAL)
    ;

// https://msdn.microsoft.com/en-us/library/ms180169.aspx
declare_cursor
    : DECLARE cursor_name
      (CURSOR (declare_set_cursor_common (FOR UPDATE (OF column_name_list)?)?)?
      | INSENSITIVE? SCROLL? CURSOR FOR select_statement (FOR (READ ONLY | UPDATE | (OF column_name_list)))?
      ) ';'?
    ;

declare_set_cursor_common
    : (LOCAL | GLOBAL)?
      (FORWARD_ONLY | SCROLL)? (STATIC | KEYSET | DYNAMIC | FAST_FORWARD)?
      (READ_ONLY | SCROLL_LOCKS | OPTIMISTIC)? TYPE_WARNING?
      FOR select_statement
    ;

fetch_cursor
    : FETCH ((NEXT | PRIOR | FIRST | LAST | (ABSOLUTE | RELATIVE) expression)? FROM)?
      GLOBAL? cursor_name (INTO LOCAL_ID (',' LOCAL_ID)*)? ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms190356.aspx
// Runtime check.
set_special
    : SET id (id | constant_LOCAL_ID | on_off) ';'?
    // https://msdn.microsoft.com/en-us/library/ms173763.aspx
    | SET TRANSACTION ISOLATION LEVEL
      (READ UNCOMMITTED | READ COMMITTED | REPEATABLE READ | SNAPSHOT | SERIALIZABLE) ';'?
    // https://msdn.microsoft.com/en-us/library/ms188059.aspx
    | SET IDENTITY_INSERT table_name on_off ';'?
    | SET ANSI_NULLS on_off
    | SET QUOTED_IDENTIFIER on_off
    | SET ANSI_PADDING on_off
    ;

constant_LOCAL_ID
    : constant
    | LOCAL_ID
    ;

// Expression.

// https://msdn.microsoft.com/en-us/library/ms190286.aspx
// Operator precendence: https://msdn.microsoft.com/en-us/library/ms190276.aspx
expression
    : DEFAULT                                                  #primitive_expression
    | NULL                                                     #primitive_expression
    | LOCAL_ID                                                 #primitive_expression
    | constant                                                 #primitive_expression
    | function_call                                            #function_call_expression
    | expression COLLATE id                                    #function_call_expression
    // https://msdn.microsoft.com/en-us/library/ms181765.aspx
    | CASE caseExpr=expression switch_section+ (ELSE elseExpr=expression)? END   #case_expression
    | CASE switch_search_condition_section+ (ELSE elseExpr=expression)? END      #case_expression

    | full_column_name                                         #column_ref_expression
    | '(' expression ')'                                       #bracket_expression
    | '(' subquery ')'                                         #subquery_expression
    | '~' expression                                           #unary_operator_expression

    | expression op=('*' | '/' | '%') expression               #binary_operator_expression
    | op=('+' | '-') expression                                #unary_operator_expression
    | expression op=('+' | '-' | '&' | '^' | '|') expression   #binary_operator_expression
    | expression comparison_operator expression                #binary_operator_expression

    | over_clause                                              #over_clause_expression
    ;

constant_expression
    : NULL
    | constant
    // system functions: https://msdn.microsoft.com/en-us/library/ms187786.aspx
    | function_call
    | LOCAL_ID         // TODO: remove.
    | '(' constant_expression ')'
    ;

subquery
    : select_statement
    ;

// https://msdn.microsoft.com/en-us/library/ms175972.aspx
with_expression
    : WITH (XMLNAMESPACES ',')? common_table_expression (',' common_table_expression)*
    ;

common_table_expression
    : expression_name=id ('(' column_name_list ')')? AS '(' select_statement ')'
    ;

update_elem
    : (full_column_name | LOCAL_ID) ('=' | assignment_operator) expression
    | udt_column_name=id '.' method_name=id '(' expression_list ')'
    //| full_column_name '.' WRITE (expression, )
    ;

// https://msdn.microsoft.com/en-us/library/ms173545.aspx
search_condition_list
    : search_condition (',' search_condition)*
    ;

search_condition
    : search_condition_and (OR search_condition_and)*
    ;

search_condition_and
    : search_condition_not (AND search_condition_not)*
    ;

search_condition_not
    : NOT? predicate
    ;

predicate
    : EXISTS '(' subquery ')'
    | expression comparison_operator expression
    | expression comparison_operator (ALL | SOME | ANY) '(' subquery ')'
    | expression NOT? BETWEEN expression AND expression
    | expression NOT? IN '(' (subquery | expression_list) ')'
    | expression NOT? LIKE expression (ESCAPE expression)?
    | expression IS null_notnull
    | '(' search_condition ')'
	| DECIMAL
    ;

query_expression
    : (query_specification | '(' query_expression ')') union*
    ;

union
    : (UNION ALL? | EXCEPT | INTERSECT) (query_specification | ('(' query_expression ')')+)
    ;

// https://msdn.microsoft.com/en-us/library/ms176104.aspx
query_specification
    : SELECT (ALL | DISTINCT)? (TOP expression PERCENT? (WITH TIES)?)?
      select_list
      // https://msdn.microsoft.com/en-us/library/ms188029.aspx
      (INTO table_name)?
      (FROM table_sources)?
      (WHERE where=search_condition)?
      // https://msdn.microsoft.com/en-us/library/ms177673.aspx
      (GROUP BY group_by_item (',' group_by_item)*)?
      (HAVING having=search_condition)?
    ;

// https://msdn.microsoft.com/en-us/library/ms188385.aspx
order_by_clause
    : ORDER BY order_by_expression (',' order_by_expression)*
      (OFFSET expression (ROW | ROWS) (FETCH (FIRST | NEXT) expression (ROW | ROWS) ONLY)?)?
    ;

// https://msdn.microsoft.com/en-us/library/ms173812.aspx
for_clause
    : FOR BROWSE
    | FOR XML AUTO xml_common_directives?
    | FOR XML PATH ('(' STRING ')')? xml_common_directives?
    ;

xml_common_directives
    : ',' (BINARY BASE64 | TYPE | ROOT)
    ;

order_by_expression
    : expression (ASC | DESC)?
    ;

group_by_item
    : expression
    /*| rollup_spec
    | cube_spec
    | grouping_sets_spec
    | grand_total*/
    ;

option_clause
    // https://msdn.microsoft.com/en-us/library/ms181714.aspx
    : OPTION '(' option (',' option)* ')'
    ;

option
    : FAST number_rows=DECIMAL
    | (HASH | ORDER) GROUP
    | (MERGE | HASH | CONCAT) UNION
    | (LOOP | MERGE | HASH) JOIN
    | EXPAND VIEWS
    | FORCE ORDER
    | IGNORE_NONCLUSTERED_COLUMNSTORE_INDEX
    | KEEP PLAN
    | KEEPFIXED PLAN
    | MAXDOP number_of_processors=DECIMAL
    | MAXRECURSION number_recursion=DECIMAL
    | OPTIMIZE FOR '(' optimize_for_arg (',' optimize_for_arg)* ')'
    | OPTIMIZE FOR UNKNOWN
    | PARAMETERIZATION (SIMPLE | FORCED)
    | RECOMPILE
    | ROBUST PLAN
    | USE PLAN STRING
    ;

optimize_for_arg
    : LOCAL_ID (UNKNOWN | '=' constant)
    ;

// https://msdn.microsoft.com/en-us/library/ms176104.aspx
select_list
    : select_list_elem (',' select_list_elem)*
    ;

select_list_elem
    : (table_name '.')? ('*' | '$' (IDENTITY | ROWGUID))
    | column_alias '=' expression
    | expression (AS? column_alias)?
    ;

table_sources
    : table_source (',' table_source)*
    ;

// https://msdn.microsoft.com/en-us/library/ms177634.aspx
table_source
    : table_source_item_joined
    | '(' table_source_item_joined ')'
    ;

table_source_item_joined
    : table_source_item join_part*
    ;

table_source_item
    : table_name_with_hint        as_table_alias?
    | rowset_function             as_table_alias?
    | derived_table              (as_table_alias column_alias_list?)?
    | change_table                as_table_alias
    | function_call               as_table_alias?
    | LOCAL_ID                    as_table_alias?
    | LOCAL_ID '.' function_call (as_table_alias column_alias_list?)?
    ;

change_table
    : CHANGETABLE '(' CHANGES table_name ',' (NULL | DECIMAL | LOCAL_ID) ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms191472.aspx
join_part
    // https://msdn.microsoft.com/en-us/library/ms173815(v=sql.120).aspx
    : (INNER? |
       join_type=(LEFT | RIGHT | FULL) OUTER?) (join_hint=(LOOP | HASH | MERGE | REMOTE))?
       JOIN table_source ON search_condition
    | CROSS JOIN table_source
    | CROSS APPLY table_source
    | OUTER APPLY table_source
    ;

table_name_with_hint
    : table_name with_table_hints?
    ;

// https://msdn.microsoft.com/en-us/library/ms190312.aspx
rowset_function
    :  (
	      OPENROWSET LR_BRACKET provider_name = STRING COMMA connectionString = STRING COMMA sql = STRING RR_BRACKET 
	   )
	   | ( OPENROWSET '(' BULK data_file=STRING ',' (bulk_option (',' bulk_option)* | id)')' )
    ;

// runtime check.
bulk_option
    : id '=' bulk_option_value=(DECIMAL | STRING)
    ;

derived_table
    : subquery
    | '(' subquery ')'
    ;

function_call
    : ranking_windowed_function
    | aggregate_windowed_function
    | scalar_function_name '(' expression_list? ')'
    // https://msdn.microsoft.com/en-us/library/ms173784.aspx
    | BINARY_CHECKSUM '(' '*' ')'
    // https://msdn.microsoft.com/en-us/library/hh231076.aspx
    // https://msdn.microsoft.com/en-us/library/ms187928.aspx
    | CAST '(' expression AS data_type ')'
    | CONVERT '(' data_type ',' expression (',' style=expression)? ')'
    // https://msdn.microsoft.com/en-us/library/ms189788.aspx
    | CHECKSUM '(' '*' ')'
    // https://msdn.microsoft.com/en-us/library/ms190349.aspx
    | COALESCE '(' expression_list ')'
    // https://msdn.microsoft.com/en-us/library/ms188751.aspx
    | CURRENT_TIMESTAMP
    // https://msdn.microsoft.com/en-us/library/ms176050.aspx
    | CURRENT_USER
    // https://msdn.microsoft.com/en-us/library/ms186819.aspx
    | DATEADD '(' ID ',' expression ',' expression ')'
    // https://msdn.microsoft.com/en-us/library/ms189794.aspx
    | DATEDIFF '(' ID ',' expression ',' expression ')'
    // https://msdn.microsoft.com/en-us/library/ms174395.aspx
    | DATENAME '(' ID ',' expression ')'
    // https://msdn.microsoft.com/en-us/library/ms174420.aspx
    | DATEPART '(' ID ',' expression ')'
    // https://msdn.microsoft.com/en-us/library/ms189838.aspx
    | IDENTITY '(' data_type (',' seed=DECIMAL)? (',' increment=DECIMAL)? ')'
    // https://msdn.microsoft.com/en-us/library/bb839514.aspx
    | MIN_ACTIVE_ROWVERSION
    // https://msdn.microsoft.com/en-us/library/ms177562.aspx
    | NULLIF '(' expression ',' expression ')'
    // https://msdn.microsoft.com/en-us/library/ms177587.aspx
    | SESSION_USER
    // https://msdn.microsoft.com/en-us/library/ms179930.aspx
    | SYSTEM_USER
    ;

switch_section
    : WHEN expression THEN expression
    ;

switch_search_condition_section
    : WHEN search_condition THEN expression
    ;

as_table_alias
    : AS? table_alias
    ;

table_alias
    : id with_table_hints?
    ;

// https://msdn.microsoft.com/en-us/library/ms187373.aspx
with_table_hints
    : WITH? '(' table_hint (',' table_hint)* ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms187373.aspx
insert_with_table_hints
    : WITH '(' table_hint (',' table_hint)* ')'
    ;

// Id runtime check. Id can be (FORCESCAN, HOLDLOCK, NOLOCK, NOWAIT, PAGLOCK, READCOMMITTED,
// READCOMMITTEDLOCK, READPAST, READUNCOMMITTED, REPEATABLEREAD, ROWLOCK, TABLOCK, TABLOCKX
// UPDLOCK, XLOCK)
table_hint
    : NOEXPAND? ( INDEX '(' index_value (',' index_value)* ')'
                | INDEX '=' index_value
                | FORCESEEK ('(' index_value '(' ID  (',' ID)* ')' ')')?
                | SERIALIZABLE
                | SNAPSHOT
                | SPATIAL_WINDOW_MAX_CELLS '=' DECIMAL
                | ID)?
    ;

index_value
    : id | DECIMAL
    ;

column_alias_list
    : '(' column_alias (',' column_alias)* ')'
    ;

column_alias
    : id
    | STRING
    ;

table_value_constructor
    : VALUES '(' expression_list ')' (',' '(' expression_list ')')*
    ;

expression_list
    : expression (',' expression)*
    ;

// https://msdn.microsoft.com/en-us/library/ms189798.aspx
ranking_windowed_function
    : (RANK | DENSE_RANK | ROW_NUMBER) '(' ')' over_clause
    | NTILE '(' expression ')' over_clause
    ;

// https://msdn.microsoft.com/en-us/library/ms173454.aspx
aggregate_windowed_function
    : (AVG | MAX | MIN | SUM | STDEV | STDEVP | VAR | VARP)
      '(' all_distinct_expression ')' over_clause?
    | (COUNT | COUNT_BIG)
      '(' ('*' | all_distinct_expression) ')' over_clause?
    | CHECKSUM_AGG '(' all_distinct_expression ')'
    | GROUPING '(' expression ')'
    | GROUPING_ID '(' expression_list ')'
    ;

all_distinct_expression
    : (ALL | DISTINCT)? expression
    ;

// https://msdn.microsoft.com/en-us/library/ms189461.aspx
over_clause
    : OVER '(' (PARTITION BY expression_list)? order_by_clause? row_or_range_clause? ')'
    ;

row_or_range_clause
    : (ROWS | RANGE) window_frame_extent
    ;

window_frame_extent
    : window_frame_preceding
    | BETWEEN window_frame_bound AND window_frame_bound
    ;

window_frame_bound
    : window_frame_preceding
    | window_frame_following
    ;

window_frame_preceding
    : UNBOUNDED PRECEDING
    | DECIMAL PRECEDING
    | CURRENT ROW
    ;

window_frame_following
    : UNBOUNDED FOLLOWING
    | DECIMAL FOLLOWING
    ;

create_database_option:
    FILESTREAM ( database_filestream_option (',' database_filestream_option)* )
    | DEFAULT_LANGUAGE EQUAL ( id | STRING )
    | DEFAULT_FULLTEXT_LANGUAGE EQUAL ( id | STRING )
    | NESTED_TRIGGERS EQUAL ( OFF | ON )
    | TRANSFORM_NOISE_WORDS EQUAL ( OFF | ON )
    | TWO_DIGIT_YEAR_CUTOFF EQUAL DECIMAL
    | DB_CHAINING ( OFF | ON )
    | TRUSTWORTHY ( OFF | ON )
    ;

database_filestream_option:
     LR_BRACKET
     (
         ( NON_TRANSACTED_ACCESS EQUAL ( OFF | READ_ONLY | FULL ) )
         |
         ( DIRECTORY_NAME EQUAL STRING )
     )
     RR_BRACKET
    ;

database_file_spec:
    file_group | file_spec;

file_group:
     FILEGROUP id
     ( CONTAINS FILESTREAM )?
     ( DEFAULT )?
     ( CONTAINS MEMORY_OPTIMIZED_DATA )?
     file_spec ( ',' file_spec )*
    ;
file_spec
    : LR_BRACKET
      NAME EQUAL ( id | STRING ) ','?
      FILENAME EQUAL file = STRING ','?
      ( SIZE EQUAL file_size ','? )?
      ( MAXSIZE EQUAL (file_size | UNLIMITED )','? )?
      ( FILEGROWTH EQUAL file_size ','? )?
      RR_BRACKET
    ;

// Primitive.

full_table_name
    : (server=id '.' database=id '.'  schema=id   '.'
      |              database=id '.' (schema=id)? '.'
      |                               schema=id   '.')? table=id
    ;

table_name
    : (database=id '.' (schema=id)? '.' | schema=id '.')? table=id
    ;

simple_name
    : (schema=id '.')? name=id
    ;

func_proc_name
    : (database=id '.' (schema=id)? '.' | (schema=id) '.')? procedure=id
    ;

ddl_object
    : full_table_name
    | LOCAL_ID
    ;

full_column_name
    : (table_name '.')? id
    ;

column_name_list
    : id (',' id)*
    ;

cursor_name
    : id
    | LOCAL_ID
    ;

on_off
    : ON
    | OFF
    ;

clustered
    : CLUSTERED
    | NONCLUSTERED
    ;

null_notnull
    : NOT? NULL
    ;

scalar_function_name
    : func_proc_name
    | RIGHT
    | LEFT
    | BINARY_CHECKSUM
    | CHECKSUM
    ;

// https://msdn.microsoft.com/en-us/library/ms187752.aspx
// TODO: implement runtime check or add new tokens.
data_type
    /*: BIGINT
    | BINARY '(' DECIMAL ')'
    | BIT
    | CHAR '(' DECIMAL ')'
    | DATE
    | DATETIME
    | DATETIME2
    | DATETIMEOFFSET '(' DECIMAL ')'
    | DECIMAL '(' DECIMAL ',' DECIMAL ')'
    | FLOAT
    | GEOGRAPHY
    | GEOMETRY
    | HIERARCHYID
    | IMAGE
    | INT
    | MONEY
    | NCHAR '(' DECIMAL ')'
    | NTEXT
    | NUMERIC '(' DECIMAL ',' DECIMAL ')'
    | NVARCHAR '(' DECIMAL | MAX ')'
    | REAL
    | SMALLDATETIME
    | SMALLINT
    | SMALLMONEY
    | SQL_VARIANT
    | TEXT
    | TIME '(' DECIMAL ')'
    | TIMESTAMP
    | TINYINT
    | UNIQUEIDENTIFIER
    | VARBINARY '(' DECIMAL | MAX ')'
    | VARCHAR '(' DECIMAL | MAX ')'
    | XML*/
    : id IDENTITY? ('(' (DECIMAL | MAX) (',' DECIMAL)? ')')?
    ;

default_value
    : NULL
    | constant
    ;

// https://msdn.microsoft.com/en-us/library/ms179899.aspx
constant
    : STRING // string, datetime or uniqueidentifier
    | BINARY
    | sign? DECIMAL
    | sign? (REAL | FLOAT)  // float or decimal
    | sign? dollar='$' (DECIMAL | FLOAT)       // money
    ;

sign
    : '+'
    | '-'
    ;

// https://msdn.microsoft.com/en-us/library/ms175874.aspx
id
    : simple_id
    | DOUBLE_QUOTE_ID
    | SQUARE_BRACKET_ID
    ;

simple_id
    : ID
    | ABSOLUTE
    | APPLY
    | AUTO
    | AVG
    | BASE64
    | CALLER
    | CAST
    | CATCH
    | CHECKSUM_AGG
    | COMMITTED
    | CONCAT
    | CONTROL
    | COOKIE
    | COUNT
    | COUNT_BIG
    | DELAY
    | DELETED
    | DENSE_RANK
    | DISABLE
    | DYNAMIC
    | ENCRYPTION
    | EXPAND
    | FAST
    | FAST_FORWARD
    | FIRST
    | FOLLOWING
    | FORCE
    | FORCESEEK
    | FORWARD_ONLY
    | FULLSCAN
    | GLOBAL
    | GO
    | GROUPING
    | GROUPING_ID
    | HASH
    | IMPERSONATE
    | INSENSITIVE
    | INSERTED
    | ISOLATION
    | KEEP
    | KEEPFIXED
    | FORCED
    | KEYSET
    | IGNORE_NONCLUSTERED_COLUMNSTORE_INDEX
    | LAST
    | LEVEL
    | LOCAL
    | LOCK_ESCALATION
    | LOGIN
    | LOOP
    | MARK
    | MAX
    | MAXDOP
    | MAXRECURSION
    | MIN
    | MODIFY
    | NAME
    | NEXT
    | NOCOUNT
    | NOEXPAND
    | NORECOMPUTE
    | NTILE
    | NUMBER
    | OFFSET
    | ONLINE
    | ONLY
    | OPTIMISTIC
    | OPTIMIZE
    | OUT
    | OUTPUT
    | OWNER
    | PARAMETERIZATION
    | PARTITION
    | PATH
    | PRECEDING
    | PRIOR
    | PRIVILEGES
    | RANGE
    | RANK
    | READONLY
    | READ_ONLY
    | RECOMPILE
    | RELATIVE
    | REMOTE
    | REPEATABLE
    | ROBUST
    | ROOT
    | ROW
    | ROWGUID
    | ROWS
    | ROW_NUMBER
    | SAMPLE
    | SCHEMABINDING
    | SCROLL
    | SCROLL_LOCKS
    | SELF
    | SERIALIZABLE
    | SIMPLE
    | SNAPSHOT
    | SPATIAL_WINDOW_MAX_CELLS
    | STATIC
    | STATS_STREAM
    | STDEV
    | STDEVP
    | SUM
    | TEXTIMAGE_ON
    | THROW
    | TIES
    | TIME
    | TRY
    | TYPE
    | TYPE_WARNING
    | UNBOUNDED
    | UNCOMMITTED
    | UNKNOWN
    | USING
    | VAR
    | VARP
    | VIEW_METADATA
    | VIEWS
    | WORK
    | XML
    | XMLNAMESPACES
    ;

// https://msdn.microsoft.com/en-us/library/ms188074.aspx
// Spaces are allowed for comparison operators.
comparison_operator
    : '=' | '>' | '<' | '<' '=' | '>' '=' | '<' '>' | '!' '=' | '!' '>' | '!' '<'
    ;

assignment_operator
    : '+=' | '-=' | '*=' | '/=' | '%=' | '&=' | '^=' | '|='
    ;

file_size:
    DECIMAL( KB | MB | GB | TB | '%' )?
    ;

// Lexer

// Basic keywords (from https://msdn.microsoft.com/en-us/library/ms189822.aspx)
ADD:                                   A D D;
ALL:                                   A L L;
ALTER:                                 A L T E R;
AND:                                   A N D;
ANY:                                   A N Y;
AS:                                    A S;
ASC:                                   A S C;
AUTHORIZATION:                         A U T H O R I Z A T I O N;
BACKUP:                                B A C K U P;
BEGIN:                                 B E G I N;
BETWEEN:                               B E T W E E N;
BREAK:                                 B R E A K;
BROWSE:                                B R O W S E;
BULK:                                  B U L K;
BY:                                    B Y;
CASCADE:                               C A S C A D E;
CASE:                                  C A S E;
CHANGETABLE:                           C H A N G E T A B L E;
CHANGES:                               C H A N G E S;
CHECK:                                 C H E C K;
CHECKPOINT:                            C H E C K P O I N T;
CLOSE:                                 C L O S E;
CLUSTERED:                             C L U S T E R E D;
COALESCE:                              C O A L E S C E;
COLLATE:                               C O L L A T E;
COLUMN:                                C O L U M N;
COMMIT:                                C O M M I T;
COMPUTE:                               C O M P U T E;
CONSTRAINT:                            C O N S T R A I N T;
CONTAINMENT:                           C O N T A I N M E N T;
CONTAINS:                              C O N T A I N S;
CONTAINSTABLE:                         C O N T A I N S T A B L E;
CONTINUE:                              C O N T I N U E;
CONVERT:                               C O N V E R T;
CREATE:                                C R E A T E;
CROSS:                                 C R O S S;
CURRENT:                               C U R R E N T;
CURRENT_DATE:                          C U R R E N T '_' D A T E;
CURRENT_TIME:                          C U R R E N T '_' T I M E;
CURRENT_TIMESTAMP:                     C U R R E N T '_' T I M E S T A M P;
CURRENT_USER:                          C U R R E N T '_' U S E R;
CURSOR:                                C U R S O R;
DATABASE:                              D A T A B A S E;
DBCC:                                  D B C C;
DEALLOCATE:                            D E A L L O C A T E;
DECLARE:                               D E C L A R E;
DEFAULT:                               D E F A U L T;
DELETE:                                D E L E T E;
DENY:                                  D E N Y;
DESC:                                  D E S C;
DISK:                                  D I S K;
DISTINCT:                              D I S T I N C T;
DISTRIBUTED:                           D I S T R I B U T E D;
DOUBLE:                                D O U B L E;
DROP:                                  D R O P;
DUMP:                                  D U M P;
ELSE:                                  E L S E;
END:                                   E N D;
ERRLVL:                                E R R L V L;
ESCAPE:                                E S C A P E;
EXCEPT:                                E X C E P T;
EXECUTE:                               E X E C (U T E)?;
EXISTS:                                E X I S T S;
EXIT:                                  E X I T;
EXTERNAL:                              E X T E R N A L;
FETCH:                                 F E T C H;
FILE:                                  F I L E;
FILENAME:                              F I L E N A M E;
FILLFACTOR:                            F I L L F A C T O R;
FOR:                                   F O R;
FORCESEEK:                             F O R C E S E E K;
FOREIGN:                               F O R E I G N;
FREETEXT:                              F R E E T E X T;
FREETEXTTABLE:                         F R E E T E X T T A B L E;
FROM:                                  F R O M;
FULL:                                  F U L L;
FUNCTION:                              F U N C T I O N;
GOTO:                                  G O T O;
GRANT:                                 G R A N T;
GROUP:                                 G R O U P;
HAVING:                                H A V I N G;
IDENTITY:                              I D E N T I T Y;
IDENTITYCOL:                           I D E N T I T Y C O L;
IDENTITY_INSERT:                       I D E N T I T Y '_' I N S E R T;
IF:                                    I F;
IN:                                    I N;
INDEX:                                 I N D E X;
INNER:                                 I N N E R;
INSERT:                                I N S E R T;
INTERSECT:                             I N T E R S E C T;
INTO:                                  I N T O;
IS:                                    I S;
JOIN:                                  J O I N;
KEY:                                   K E Y;
KILL:                                  K I L L;
LEFT:                                  L E F T;
LIKE:                                  L I K E;
LINENO:                                L I N E N O;
LOAD:                                  L O A D;
LOG:                                   L O G;
MERGE:                                 M E R G E;
NATIONAL:                              N A T I O N A L;
NOCHECK:                               N O C H E C K;
NONCLUSTERED:                          N O N C L U S T E R E D;
NONE:                                  N O N E;
NOT:                                   N O T;
NULL:                                  N U L L;
NULLIF:                                N U L L I F;
OF:                                    O F;
OFF:                                   O F F;
OFFSETS:                               O F F S E T S;
ON:                                    O N;
OPEN:                                  O P E N;
OPENDATASOURCE:                        O P E N D A T A S O U R C E;
OPENQUERY:                             O P E N Q U E R Y;
OPENROWSET:                            O P E N R O W S E T;
OPENXML:                               O P E N X M L;
OPTION:                                O P T I O N;
OR:                                    O R;
ORDER:                                 O R D E R;
OUTER:                                 O U T E R;
OVER:                                  O V E R;
PARTIAL:                               P A R T I A L;
PERCENT:                               P E R C E N T;
PIVOT:                                 P I V O T;
PLAN:                                  P L A N;
PRECISION:                             P R E C I S I O N;
PRIMARY:                               P R I M A R Y;
PRINT:                                 P R I N T;
PROC:                                  P R O C;
PROCEDURE:                             P R O C E D U R E;
PUBLIC:                                P U B L I C;
RAISERROR:                             R A I S E R R O R;
READ:                                  R E A D;
READTEXT:                              R E A D T E X T;
RECONFIGURE:                           R E C O N F I G U R E;
REFERENCES:                            R E F E R E N C E S;
REPLICATION:                           R E P L I C A T I O N;
RESTORE:                               R E S T O R E;
RESTRICT:                              R E S T R I C T;
RETURN:                                R E T U R N;
REVERT:                                R E V E R T;
REVOKE:                                R E V O K E;
RIGHT:                                 R I G H T;
ROLLBACK:                              R O L L B A C K;
ROWCOUNT:                              R O W C O U N T;
ROWGUIDCOL:                            R O W G U I D C O L;
RULE:                                  R U L E;
SAVE:                                  S A V E;
SCHEMA:                                S C H E M A;
SECURITYAUDIT:                         S E C U R I T Y A U D I T;
SELECT:                                S E L E C T;
SEMANTICKEYPHRASETABLE:                S E M A N T I C K E Y P H R A S E T A B L E;
SEMANTICSIMILARITYDETAILSTABLE:        S E M A N T I C S I M I L A R I T Y D E T A I L S T A B L E;
SEMANTICSIMILARITYTABLE:               S E M A N T I C S I M I L A R I T Y T A B L E;
SESSION_USER:                          S E S S I O N '_' U S E R;
SET:                                   S E T;
SETUSER:                               S E T U S E R;
SHUTDOWN:                              S H U T D O W N;
SOME:                                  S O M E;
STATISTICS:                            S T A T I S T I C S;
SYSTEM_USER:                           S Y S T E M '_' U S E R;
TABLE:                                 T A B L E;
TABLESAMPLE:                           T A B L E S A M P L E;
TEXTSIZE:                              T E X T S I Z E;
THEN:                                  T H E N;
TO:                                    T O;
TOP:                                   T O P;
TRAN:                                  T R A N;
TRANSACTION:                           T R A N S A C T I O N;
TRIGGER:                               T R I G G E R;
TRUNCATE:                              T R U N C A T E;
TRY_CONVERT:                           T R Y '_' C O N V E R T;
TSEQUAL:                               T S E Q U A L;
UNION:                                 U N I O N;
UNIQUE:                                U N I Q U E;
UNPIVOT:                               U N P I V O T;
UPDATE:                                U P D A T E;
UPDATETEXT:                            U P D A T E T E X T;
USE:                                   U S E;
USER:                                  U S E R;
VALUES:                                V A L U E S;
VARYING:                               V A R Y I N G;
VIEW:                                  V I E W;
WAITFOR:                               W A I T F O R;
WHEN:                                  W H E N;
WHERE:                                 W H E R E;
WHILE:                                 W H I L E;
WITH:                                  W I T H;
WITHIN:                                W I T H I N;
WRITETEXT:                             W R I T E T E X T;

// Additional keywords (they can be id).
ABSOLUTE:                              A B S O L U T E;
AFTER:                                 A F T E R;
ALLOWED:                               A L L O W E D; 
ALLOW_SNAPSHOT_ISOLATION:              A L L O W '_' S N A P S H O T '_' I S O L A T I O N;
ANSI_NULLS:                            A N S I '_' N U L L S;
ANSI_NULL_DEFAULT:                     A N S I '_' N U L L '_' D E F A U L T;
ANSI_PADDING:                          A N S I '_' P A D D I N G;
ANSI_WARNINGS:                         A N S I '_' W A R N I N G S;
APPLY:                                 A P P L Y;
ARITHABORT:                            A R I T H A B O R T;
AUTO:                                  A U T O;
AUTO_CLEANUP:                          A U T O '_' C L E A N U P; 
AUTO_CLOSE:                            A U T O '_' C L O S E;
AUTO_CREATE_STATISTICS:                A U T O '_' C R E A T E '_' S T A T I S T I C S;
AUTO_SHRINK:                           A U T O '_' S H R I N K;
AUTO_UPDATE_STATISTICS:                A U T O '_' U P D A T E '_' S T A T I S T I C S;
AUTO_UPDATE_STATISTICS_ASYNC:          A U T O '_' U P D A T E '_' S T A T I S T I C S '_' A S Y N C;
AVG:                                   A V G;
BASE64:                                B A S E '64';
BINARY_CHECKSUM:                       B I N A R Y '_' C H E C K S U M;
BULK_LOGGED:                           B U L K '_' L O G G E D; 
CALLER:                                C A L L E R;
CAST:                                  C A S T;
CATCH:                                 C A T C H;
CHANGE_RETENTION:                      C H A N G E '_' R E T E N T I O N; 
CHANGE_TRACKING:                       C H A N G E '_' T R A C K I N G; 
CHECKSUM:                              C H E C K S U M;
CHECKSUM_AGG:                          C H E C K S U M '_' A G G;
COMMITTED:                             C O M M I T T E D;
COMPATIBILITY_LEVEL:                   C O M P A T I B I L I T Y '_' L E V E L;                           
CONCAT:                                C O N C A T;
CONCAT_NULL_YIELDS_NULL:               C O N C A T '_' N U L L '_' Y I E L D S '_' N U L L;
CONTROL:                               C O N T R O L;
COOKIE:                                C O O K I E;
COUNT:                                 C O U N T;
COUNT_BIG:                             C O U N T '_' B I G;
CURSOR_CLOSE_ON_COMMIT:                C U R S O R '_' C L O S E '_' O N '_' C O M M I T;
CURSOR_DEFAULT:                        C U R S O R '_' D E F A U L T;
DATEADD:                               D A T E A D D;
DATEDIFF:                              D A T E D I F F;
DATENAME:                              D A T E N A M E;
DATEPART:                              D A T E P A R T;
DATE_CORRELATION_OPTIMIZATION:         D A T E '_' C O R R E L A T I O N '_' O P T I M I Z A T I O N;
DAYS:                                  D A Y S; 
DB_CHAINING:                           D B '_' C H A I N I N G;
DEFAULT_FULLTEXT_LANGUAGE:             D E F A U L T '_' F U L L T E X T '_' L A N G U A G E;
DEFAULT_LANGUAGE:                      D E F A U L T '_' L A N G U A G E;
DELAY:                                 D E L A Y;
DELAYED_DURABILITY:                    D E L A Y E D '_' D U R A B I L I T Y;
DELETED:                               D E L E T E D;
DENSE_RANK:                            D E N S E '_' R A N K;
DIRECTORY_NAME:                        D I R E C T O R Y '_' N A M E;
DISABLE:                               D I S A B L E;
DISABLED:                              D I S A B L E D; 
DISABLE_BROKER:                        D I S A B L E '_' B R O K E R;  
DYNAMIC:                               D Y N A M I C;
EMERGENCY:                             E M E R G E N C Y; 
ENABLE_BROKER:                         E N A B L E '_' B R O K E R;
ENCRYPTION:                            E N C R Y P T I O N;
ERROR_BROKER_CONVERSATIONS:            E R R O R '_' B R O K E R '_' C O N V E R S A T I O N S; 
EXPAND:                                E X P A N D;
FAST:                                  F A S T;
FAST_FORWARD:                          F A S T '_' F O R W A R D;
FILEGROUP:                             F I L E G R O U P;
FILEGROWTH:                            F I L E G R O W T H;
FILESTREAM:                            F I L E S T R E A M;
FIRST:                                 F I R S T;
FOLLOWING:                             F O L L O W I N G;
FORCE:                                 F O R C E;
FORCED:                                F O R C E D;
FORWARD_ONLY:                          F O R W A R D '_' O N L Y;
FULLSCAN:                              F U L L S C A N;
GB:                                    G B;
GLOBAL:                                G L O B A L;
GO:                                    G O;
GROUPING:                              G R O U P I N G;
GROUPING_ID:                           G R O U P I N G '_' I D;
HADR:                                  H A D R;
HASH:                                  H A S H;
HONOR_BROKER_PRIORITY:                 H O N O R '_' B R O K E R '_' P R I O R I T Y;
HOURS:                                 H O U R S; 
IGNORE_NONCLUSTERED_COLUMNSTORE_INDEX: I G N O R E '_' N O N C L U S T E R E D '_' C O L U M N S T O R E '_' I N D E X;
IMMEDIATE:                             I M M E D I A T E;
IMPERSONATE:                           I M P E R S O N A T E;
INCREMENTAL:                           I N C R E M E N T A L; 
INSENSITIVE:                           I N S E N S I T I V E;
INSERTED:                              I N S E R T E D;
ISOLATION:                             I S O L A T I O N;
KB:                                    K B;
KEEP:                                  K E E P;
KEEPFIXED:                             K E E P F I X E D;
KEYSET:                                K E Y S E T;
LAST:                                  L A S T;
LEVEL:                                 L E V E L;
LOCAL:                                 L O C A L;
LOCK_ESCALATION:                       L O C K '_' E S C A L A T I O N;
LOGIN:                                 L O G I N;
LOOP:                                  L O O P;
MARK:                                  M A R K;
MAX:                                   M A X;
MAXDOP:                                M A X D O P;
MAXRECURSION:                          M A X R E C U R S I O N;
MAXSIZE:                               M A X S I Z E;
MB:                                    M B;
MEMORY_OPTIMIZED_DATA:                 M E M O R Y '_' O P T I M I Z E D '_' D A T A;
MIN:                                   M I N;
MINUTES:                               M I N U T E S; 
MIN_ACTIVE_ROWVERSION:                 M I N '_' A C T I V E '_' R O W V E R S I O N;
MIXED_PAGE_ALLOCATION:                 M I X E D '_' P A G E '_' A L L O C A T I O N; 
MODIFY:                                M O D I F Y;
MULTI_USER:                            M U L T I '_' U S E R;
NAME:                                  N A M E;
NESTED_TRIGGERS:                       N E S T E D '_' T R I G G E R S;
NEW_BROKER:                            N E W '_' B R O K E R;  
NEXT:                                  N E X T;
NOCOUNT:                               N O C O U N T;
NOEXPAND:                              N O E X P A N D;
NON_TRANSACTED_ACCESS:                 N O N '_' T R A N S A C T E D '_' A C C E S S;
NORECOMPUTE:                           N O R E C O M P U T E;
NO_WAIT:                               N O '_' W A I T;
NTILE:                                 N T I L E;
NUMBER:                                N U M B E R;
NUMERIC_ROUNDABORT:                    N U M E R I C '_' R O U N D A B O R T;
OFFLINE:                               O F F L I N E; 
OFFSET:                                O F F S E T;
ONLINE:                                O N L I N E; 
ONLY:                                  O N L Y;
OPTIMISTIC:                            O P T I M I S T I C;
OPTIMIZE:                              O P T I M I Z E;
OUT:                                   O U T;
OUTPUT:                                O U T P U T;
OWNER:                                 O W N E R;
PAGE_VERIFY:                           P A G E '_' V E R I F Y;
PARAMETERIZATION:                      P A R A M E T E R I Z A T I O N;
PARTITION:                             P A R T I T I O N;
PATH:                                  P A T H;
PRECEDING:                             P R E C E D I N G;
PRIOR:                                 P R I O R;
PRIVILEGES:                            P R I V I L E G E S;
QUOTED_IDENTIFIER:                     Q U O T E D '_' I D E N T I F I E R;
RANGE:                                 R A N G E;
RANK:                                  R A N K;
READONLY:                              R E A D O N L Y;
READ_COMMITTED_SNAPSHOT:               R E A D '_' C O M M I T T E D '_' S N A P S H O T;
READ_ONLY:                             R E A D '_' O N L Y;
READ_WRITE:                            R E A D '_' W R I T E;
RECOMPILE:                             R E C O M P I L E;
RECOVERY:                              R E C O V E R Y;
RECURSIVE_TRIGGERS:                    R E C U R S I V E '_' T R I G G E R S;
RELATIVE:                              R E L A T I V E;
REMOTE:                                R E M O T E;
REPEATABLE:                            R E P E A T A B L E;
RESTRICTED_USER:                       R E S T R I C T E D '_' U S E R; 
ROBUST:                                R O B U S T;
ROOT:                                  R O O T;
ROW:                                   R O W;
ROWGUID:                               R O W G U I D;
ROWS:                                  R O W S;
ROW_NUMBER:                            R O W '_' N U M B E R;
SAMPLE:                                S A M P L E;
SCHEMABINDING:                         S C H E M A B I N D I N G;
SCROLL:                                S C R O L L;
SCROLL_LOCKS:                          S C R O L L '_' L O C K S;
SECONDS:                               S E C O N D S;
SELF:                                  S E L F;
SERIALIZABLE:                          S E R I A L I Z A B L E;
SHOWPLAN:                              S H O W P L A N;
SIMPLE:                                S I M P L E;
SINGLE_USER:                           S I N G L E '_' U S E R; 
SIZE:                                  S I Z E;
SNAPSHOT:                              S N A P S H O T;
SPATIAL_WINDOW_MAX_CELLS:              S P A T I A L '_' W I N D O W '_' M A X '_' C E L L S;
STATIC:                                S T A T I C;
STATS_STREAM:                          S T A T S '_' S T R E A M;
STDEV:                                 S T D E V;
STDEVP:                                S T D E V P;
SUM:                                   S U M;
TAKE:                                  T A K E;
TARGET_RECOVERY_TIME:                  T A R G E T '_' R E C O V E R Y '_' T I M E;
TB:                                    T B;
TEXTIMAGE_ON:                          T E X T I M A G E '_' O N;
THROW:                                 T H R O W;
TIES:                                  T I E S;
TIME:                                  T I M E;
TORN_PAGE_DETECTION:                   T O R N '_' P A G E '_' D E T E C T I O N; 
TRANSFORM_NOISE_WORDS:                 T R A N S F O R M '_' N O I S E '_' W O R D S;
TRUSTWORTHY:                           T R U S T W O R T H Y;
TRY:                                   T R Y;
TWO_DIGIT_YEAR_CUTOFF:                 T W O '_' D I G I T '_' Y E A R '_' C U T O F F;
TYPE:                                  T Y P E;
TYPE_WARNING:                          T Y P E '_' W A R N I N G;
UNBOUNDED:                             U N B O U N D E D;
UNCOMMITTED:                           U N C O M M I T T E D;
UNKNOWN:                               U N K N O W N;
UNLIMITED:                             U N L I M I T E D;
USING:                                 U S I N G;
VAR:                                   V A R;
VARP:                                  V A R P;
VIEWS:                                 V I E W S;
VIEW_METADATA:                         V I E W '_' M E T A D A T A;
WORK:                                  W O R K;
XML:                                   X M L;
XMLNAMESPACES:                         X M L N A M E S P A C E S;

DOLLAR_ACTION:                         '$' A C T I O N;

SPACE:              [ \t\r\n]+    -> skip;
COMMENT:            '/*' .*? '*/' -> channel(HIDDEN);
LINE_COMMENT:       '--' ~[\r\n]* -> channel(HIDDEN);

// TODO: ID can be not only Latin.
DOUBLE_QUOTE_ID:    '"' ~'"'+ '"';
SQUARE_BRACKET_ID:  '[' ~']'+ ']';
LOCAL_ID:           '@' ([a-zA-Z_$@#0-9] | FullWidthLetter)+;
DECIMAL:             DEC_DIGIT+;
ID:                  ( [a-zA-Z_#] | FullWidthLetter) ( [a-zA-Z_#$@0-9] | FullWidthLetter )*;
STRING:              N? '\'' (~'\'' | '\'\'')* '\'';
BINARY:              '0' X HEX_DIGIT*;
FLOAT:               DEC_DOT_DEC;
REAL:                DEC_DOT_DEC (E [+-]? DEC_DIGIT+)?;

EQUAL:               '=';

GREATER:             '>';
LESS:                '<';
EXCLAMATION:         '!';

PLUS_ASSIGN:         '+=';
MINUS_ASSIGN:        '-=';
MULT_ASSIGN:         '*=';
DIV_ASSIGN:          '/=';
MOD_ASSIGN:          '%=';
AND_ASSIGN:          '&=';
XOR_ASSIGN:          '^=';
OR_ASSIGN:           '|=';

DOT:                 '.';
UNDERLINE:           '_';
AT:                  '@';
SHARP:               '#';
DOLLAR:              '$';
LR_BRACKET:          '(';
RR_BRACKET:          ')';
COMMA:               ',';
SEMI:                ';';
COLON:               ':';
STAR:                '*';
DIVIDE:              '/';
MODULE:              '%';
PLUS:                '+';
MINUS:               '-';
BIT_NOT:             '~';
BIT_OR:              '|';
BIT_AND:             '&';
BIT_XOR:             '^';

fragment LETTER:       [a-zA-Z_];
fragment DEC_DOT_DEC:  (DEC_DIGIT+ '.' DEC_DIGIT+ |  DEC_DIGIT+ '.' | '.' DEC_DIGIT+);
fragment HEX_DIGIT:    [0-9A-Fa-f];
fragment DEC_DIGIT:    [0-9];

fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];
fragment FullWidthLetter
    : '\u00c0'..'\u00d6' 
    | '\u00d8'..'\u00f6' 
    | '\u00f8'..'\u00ff' 
    | '\u0100'..'\u1fff' 
    | '\u2c00'..'\u2fff' 
    | '\u3040'..'\u318f' 
    | '\u3300'..'\u337f' 
    | '\u3400'..'\u3fff' 
    | '\u4e00'..'\u9fff' 
    | '\ua000'..'\ud7ff' 
    | '\uf900'..'\ufaff' 
    | '\uff00'..'\ufff0'
    // | '\u10000'..'\u1F9FF'  //not support four bytes chars
    // | '\u20000'..'\u2FA1F'
    ;
