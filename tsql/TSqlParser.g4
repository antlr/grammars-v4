/*
T-SQL (Transact-SQL, MSSQL) grammar.
The MIT License (MIT).
Copyright (c) 2017, Mark Adams (madams51703@gmail.com)
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
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

parser grammar TSqlParser;

options { tokenVocab=TSqlLexer; }

tsql_file
    : batch* EOF
    ;

batch
    : sql_clauses go_statement*
    ;

sql_clauses
    : (sql_clause SEMI?)+
    ;

sql_clause
    : dml_clause

    | ddl_clause

    | cfl_statement

    | dbcc_clause

    | empty_statement

    | another_statement
    ;

// Data Manipulation Language: https://msdn.microsoft.com/en-us/library/ff848766(v=sql.120).aspx
dml_clause
    : merge_statement
    | delete_statement
    | insert_statement
    | select_statement
    | update_statement
    ;

// Data Definition Language: https://msdn.microsoft.com/en-us/library/ff848799.aspx)
ddl_clause
    : create_database
    | create_endpoint
    | create_index
    | create_or_alter_procedure
    | create_or_alter_trigger
    | create_or_alter_function
    | create_statistics
    | create_table
    | create_type
    | create_view
    | alter_table
    | alter_database
    | drop_index
    | drop_procedure
    | drop_trigger
    | drop_function
    | drop_statistics
    | drop_table
    | drop_type
    | drop_view
    ;

// Control-of-Flow Language: https://docs.microsoft.com/en-us/sql/t-sql/language-elements/control-of-flow
cfl_statement
    : block_statement
    | break_statement
    | continue_statement
    | goto_statement
    | if_statement
    | return_statement
    | throw_statement
    | try_catch_statement
    | waitfor_statement
    | while_statement
    | print_statement
    | raiseerror_statement
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/begin-end-transact-sql
block_statement
    : BEGIN ';'? sql_clauses? END ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/break-transact-sql
break_statement
    : BREAK ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/continue-transact-sql
continue_statement
    : CONTINUE ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/goto-transact-sql
goto_statement
    : GOTO id ';'?
    | id ':' ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/return-transact-sql
return_statement
    : RETURN expression? ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/if-else-transact-sql
if_statement
    : IF search_condition sql_clause (ELSE sql_clause)? ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/throw-transact-sql
throw_statement
    : THROW (throw_error_number ',' throw_message ',' throw_state)? ';'?
    ;

throw_error_number
    : DECIMAL | LOCAL_ID
    ;

throw_message
    : STRING | LOCAL_ID
    ;

throw_state
    : DECIMAL | LOCAL_ID
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/try-catch-transact-sql
try_catch_statement
    : BEGIN TRY ';'? try_clauses=sql_clauses? END TRY ';'? BEGIN CATCH ';'? catch_clauses=sql_clauses? END CATCH ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/waitfor-transact-sql
waitfor_statement
    : WAITFOR receive_statement? ','? ((DELAY | TIME | TIMEOUT) time)?  expression? ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/while-transact-sql
while_statement
    : WHILE search_condition (sql_clause | BREAK ';'? | CONTINUE ';'?)
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/print-transact-sql
print_statement
    : PRINT expression ';'?
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/raiserror-transact-sql
raiseerror_statement
    : RAISERROR '(' msg=(DECIMAL | STRING | LOCAL_ID) ',' severity=constant_LOCAL_ID ','
    state=constant_LOCAL_ID (',' constant_LOCAL_ID)* ')' (WITH (LOG | SETERROR))? ';'?
    ;

empty_statement
    : ';'
    ;

another_statement
    : declare_statement
    | cursor_statement
    | conversation_statement
    | create_contract
    | create_queue
    | alter_queue
    | execute_statement
    | message_statement
    | security_statement
    | set_statement
    | transaction_statement
    | use_statement
    ;

create_queue
    : CREATE QUEUE (full_table_name | queue_name=id)
      queue_settings?
      (ON filegroup=id | DEFAULT)?
    ;

queue_settings
    :WITH
       (STATUS EQUAL (ON | OFF) COMMA?)?
       (RETENTION EQUAL (ON | OFF) COMMA?)?
       (ACTIVATION
         LR_BRACKET
           (COMMA? STATUS EQUAL (ON | OFF))?
           (COMMA? PROCEDURE_NAME EQUAL func_proc_name)?
           (COMMA? MAX_QUEUE_READERS EQUAL max_readers=DECIMAL)?
           (COMMA? EXECUTE AS (SELF | user_name=STRING | OWNER))?
           (COMMA? POISON_MESSAGE_HANDLING STATUS EQUAL (ON | OFF))?
           (COMMA? DROP)?
         RR_BRACKET)?
    ;

alter_queue
    : ALTER QUEUE (full_table_name | queue_name=id)
      (queue_settings | queue_action)
    ;

queue_action
    : REBUILD ( WITH LR_BRACKET queue_rebuild_options RR_BRACKET)?
    | REORGANIZE (WITH LOB_COMPACTION EQUAL (ON | OFF))?
    | MOVE TO (id | DEFAULT)
    ;
queue_rebuild_options
    : MAXDOP EQUAL DECIMAL
    ;

create_contract
    : CREATE CONTRACT contract_name
      (AUTHORIZATION owner_name=id)?
      LR_BRACKET ((message_type_name=id | DEFAULT)
          SENT BY (INITIATOR | TARGET | ANY ) COMMA?)+
      RR_BRACKET
    ;

conversation_statement
    : begin_conversation_timer
    | begin_conversation_dialog
    | end_conversation
    | get_conversation
    | send_conversation
    | waitfor_conversation
    ;

message_statement
    : CREATE MESSAGE TYPE message_type_name=id
      (AUTHORIZATION owner_name=id)?
      (VALIDATION EQUAL (NONE
      | EMPTY
      | WELL_FORMED_XML
      | VALID_XML WITH SCHEMA COLLECTION schema_collection_name=id))
    ;

// DML

// https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql
merge_statement
    : with_expression?
      MERGE (TOP '(' expression ')' PERCENT?)?
      INTO? ddl_object insert_with_table_hints? as_table_alias?
      USING table_sources
      ON search_condition
      (WHEN MATCHED (AND search_condition)?
          THEN merge_matched)*
      (WHEN NOT MATCHED (BY TARGET)? (AND search_condition)?
          THEN merge_not_matched)?
      (WHEN NOT MATCHED BY SOURCE (AND search_condition)?
          THEN merge_matched)*
      output_clause?
      option_clause? ';'
    ;


merge_matched
    : UPDATE SET update_elem (',' update_elem)*
    | DELETE
    ;

merge_not_matched
    : INSERT ('(' column_name_list ')')?
      (table_value_constructor | DEFAULT VALUES)
    ;

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

receive_statement
    : '('? RECEIVE (ALL | DISTINCT | top_clause | '*')
      (LOCAL_ID '=' expression ','?)* FROM full_table_name
      (INTO table_variable=id (WHERE where=search_condition))? ')'?
    ;

// https://msdn.microsoft.com/en-us/library/ms189499.aspx
select_statement
    : with_expression? query_expression order_by_clause? for_clause? option_clause? ';'?
    ;

time
    : (LOCAL_ID | constant)
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
    : (output_column_name | expression) as_column_alias?  // TODO: scalar_expression
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
    : CREATE UNIQUE? clustered? INDEX id ON table_name_with_hint '(' column_name_list_with_order ')'
    (INCLUDE '(' column_name_list ')' )?
    (WHERE where=search_condition)?
    (index_options)?
    (ON id)?
    ';'?
    ;

// https://msdn.microsoft.com/en-us/library/ms187926(v=sql.120).aspx
create_or_alter_procedure
    : (CREATE | ALTER) proc=(PROC | PROCEDURE) func_proc_name (';' DECIMAL)?
      ('('? procedure_param (',' procedure_param)* ')'?)?
      (WITH procedure_option (',' procedure_option)*)?
      (FOR REPLICATION)? AS sql_clauses
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/statements/create-trigger-transact-sql
create_or_alter_trigger
    : dml_trigger
    | ddl_trigger
    ;

dml_trigger
    : (CREATE | ALTER) TRIGGER simple_name
      ON table_name
      (WITH dml_trigger_option (',' dml_trigger_option)* )?
      (FOR | AFTER | INSTEAD OF)
      dml_trigger_operation (',' dml_trigger_operation)*
      (WITH APPEND)?
      (NOT FOR REPLICATION)?
      AS sql_clauses
    ;

dml_trigger_option
    : ENCRYPTION
    | execute_clause
    ;

dml_trigger_operation
    : (INSERT | UPDATE | DELETE)
    ;

ddl_trigger
    : (CREATE | ALTER) TRIGGER simple_name
      ON (ALL SERVER | DATABASE)
      (WITH dml_trigger_option (',' dml_trigger_option)* )?
      (FOR | AFTER) ddl_trigger_operation (',' dml_trigger_operation)*
      AS sql_clauses
    ;

ddl_trigger_operation
    : simple_id
    ;

//https://msdn.microsoft.com/en-us/library/ms186755.aspx
create_or_alter_function
    : (CREATE | ALTER) FUNCTION func_proc_name
        (('(' procedure_param (',' procedure_param)* ')') | '(' ')') //must have (), but can be empty
        (func_body_returns_select | func_body_returns_table | func_body_returns_scalar) ';'?
    ;

func_body_returns_select
  :RETURNS TABLE
  (WITH function_option (',' function_option)*)?
  AS?
  RETURN select_statement
  ;

func_body_returns_table
  : RETURNS LOCAL_ID table_type_definition
        (WITH function_option (',' function_option)*)?
        AS?
        BEGIN
           sql_clause*
           RETURN
        END
  ;

func_body_returns_scalar
  :RETURNS data_type
       (WITH function_option (',' function_option)*)?
       AS?
       BEGIN
           sql_clause*
           RETURN ret=expression ';'?
       END
       ;

procedure_param
    : LOCAL_ID (id '.')? AS? data_type VARYING? ('=' default_val=default_value)? (OUT | OUTPUT | READONLY)?
    ;

procedure_option
    : ENCRYPTION
    | RECOMPILE
    | execute_clause
    ;

function_option
    : ENCRYPTION
    | SCHEMABINDING
    | RETURNS NULL ON NULL INPUT
    | CALLED ON NULL INPUT
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
    : CREATE TABLE table_name '(' column_def_table_constraints ','? ')' table_options* (ON id | DEFAULT)? (TEXTIMAGE_ON id | DEFAULT)?';'?
    ;

table_options
    : WITH '(' index_option (',' index_option)* ')'
    ;

table_option
    : simple_id '=' (simple_id | on_off | DECIMAL)
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
                             | ALTER COLUMN column_definition
                             | DROP COLUMN id
                             | DROP CONSTRAINT constraint=id
                             | WITH CHECK ADD CONSTRAINT constraint=id FOREIGN KEY '(' fk = column_name_list ')' REFERENCES table_name '(' pk = column_name_list')'
                             | CHECK CONSTRAINT constraint=id
                             | REBUILD table_options)
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
      | database_mirroring_option
      | date_correlation_optimization_option
      | db_encryption_option
      | db_state_option
      | db_update_option
      | db_user_access_option
      | delayed_durability_option
      | external_access_option
      | FILESTREAM database_filestream_option
      | hadr_options
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


// https://docs.microsoft.com/en-us/sql/t-sql/statements/create-endpoint-transact-sql

create_endpoint:
    CREATE ENDPOINT endpointname endpoint_authorization endpoint_state endpoint_as_clause endpoint_for_clause 
     ;

endpointname:
     endpoint_name=id 
     ;

endpoint_authorization:
     AUTHORIZATION login=id
     |
;

endpoint_state:
     STATE EQUAL ( state=STARTED | state=STOPPED | state=DISABLED )
     |
;

endpoint_as_clause:
AS TCP LR_BRACKET endpoint_tcp_protocol_specific_arguments RR_BRACKET
;

endpoint_tcp_protocol_specific_arguments:
   LISTENER_PORT EQUAL port=DECIMAL endpoint_listener_ip
   ;

endpoint_listener_ip:
     COMMA LISTENER_IP EQUAL (ALL | ipv4_address | ipv6_address )
    |
    ;

ipv4_address:
   IPV4_ADDR  
   ;


ipv6_address:
        IPV6_ADDR
	;

endpoint_for_clause:
     FOR SERVICE_BROKER LR_BRACKET endpoint_server_broker_clause RR_BRACKET
     |FOR DATABASE_MIRRORING LR_BRACKET endpoint_database_mirroring_clause RR_BRACKET
     |
;

endpoint_database_mirroring_clause:
	 endpoint_authentication_clause endpoint_encryption_clause endpoint_role_clause ;

endpoint_role_clause:
        endpoint_role_clause  COMMA
	|ROLE EQUAL ( WITNESS | PARTNER | ALL )
        ;


endpoint_server_broker_clause:
         endpoint_authentication_clause endpoint_encryption_clause endpoint_message_forwarding_clause endpoint_message_forward_size
	;

endpoint_authentication_clause:
        endpoint_authentication_clause COMMA
        |AUTHENTICATION EQUAL endpoint_authentication_clause_methods
	|
	;
endpoint_authentication_clause_methods:
	 WINDOWS windows_auth_methods
        | CERTIFICATE cert_name=id 
	| WINDOWS windows_auth_methods
        | CERTIFICATE cert_name=id WINDOWS windows_auth_methods
	;

windows_auth_methods:
	( NTLM |KERBEROS | NEGOTIATE )
        ;

endpoint_encryption_clause:
	endpoint_encryption_clause COMMA
	|ENCRYPTION EQUAL DISABLED
	|ENCRYPTION EQUAL ( SUPPORTED | REQUIRED )  endpoint_encryption_algorithm
	|
	;

endpoint_encryption_algorithm:
	ALGORITHM ( AES | RC4 | AES RC4 | RC4 AES )
 	|
	; 
	
endpoint_message_forwarding_clause:
	endpoint_message_forwarding_clause COMMA
	|MESSAGE_FORWARDING EQUAL ( ENABLED | DISABLED )
	|
	;

endpoint_message_forward_size:
	MESSAGE_FORWARD_SIZE EQUAL DECIMAL
	|
	;

/* Will visit later
*/
database_mirroring_option:
         mirroring_set_option
	;

mirroring_set_option:
	 PARTNER  partner_option 
	| WITNESS witness_option 
	;

partner_option:
	EQUAL partner_server
	| FAILOVER
	| FORCE_SERVICE_ALLOW_DATA_LOSS
	| OFF
	| RESUME
	| SAFETY (FULL | OFF )
	| SUSPEND
	| TIMEOUT DECIMAL
	;

witness_option:
	EQUAL witness_server
	| OFF
	;

witness_server:
	partner_server
	;

partner_server:
	TCP COLON DOUBLE_FORWARD_SLASH host COLON port_number
	;

port_number:
	port=DECIMAL
	;

host:
	id DOT host
	|(id DOT |id)
	;

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

hadr_options:
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
    : DROP INDEX (IF EXISTS)?
    ( drop_relational_or_xml_or_spatial_index (',' drop_relational_or_xml_or_spatial_index)*
    | drop_backward_compatible_index (',' drop_backward_compatible_index)*
    )
    ';'?
    ;

drop_relational_or_xml_or_spatial_index
    : index_name=id ON full_table_name
    ;

drop_backward_compatible_index
    : (owner_name=id '.')? table_or_view_name=id '.' index_name=id
    ;

// https://msdn.microsoft.com/en-us/library/ms174969.aspx
drop_procedure
    : DROP proc=(PROC | PROCEDURE) (IF EXISTS)? func_proc_name (',' func_proc_name)* ';'?
    ;

//https://docs.microsoft.com/en-us/sql/t-sql/statements/drop-trigger-transact-sql
drop_trigger
    : drop_dml_trigger
    | drop_ddl_trigger
    ;

drop_dml_trigger
    : DROP TRIGGER (IF EXISTS)? simple_name (',' simple_name)* ';'?
    ;

drop_ddl_trigger
    : DROP TRIGGER (IF EXISTS)? simple_name (',' simple_name)*
    ON (DATABASE | ALL SERVER) ';'?
    ;

//https://msdn.microsoft.com/en-us/library/ms190290.aspx
drop_function
    : DROP FUNCTION (IF EXISTS)? func_proc_name (',' func_proc_name)* ';'?
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

create_type
    : CREATE TYPE name = simple_name
      (FROM data_type default_value)?
      (AS TABLE LR_BRACKET column_def_table_constraints RR_BRACKET)?
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
    | DECLARE LOCAL_ID AS? xml_type_definition ';'?
    | WITH XMLNAMESPACES '(' xml_namespace_uri=STRING ','? AS id ')' ';'?
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
    : EXECUTE (return_status=LOCAL_ID '=')? (func_proc_name | expression) (execute_statement_arg (',' execute_statement_arg)*)? ';'?
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
    | GRANT (ALL PRIVILEGES? | grant_permission ('(' column_name_list ')')?) (ON on_id=table_name)? TO (to_principal+=id) (',' to_principal+=id)* (WITH GRANT OPTION)? (AS as_principal=id)? ';'?
    // https://msdn.microsoft.com/en-us/library/ms178632.aspx
    | REVERT ('(' WITH COOKIE '=' LOCAL_ID ')')? ';'?
    | open_key
    | close_key
    | create_key
    | create_certificate
    ;

create_certificate
    : CREATE CERTIFICATE certificate_name=id (AUTHORIZATION user_name=id)?
      (FROM existing_keys | generate_new_keys)
      (ACTIVE FOR BEGIN DIALOG '=' (ON | OFF))?
    ;

existing_keys
    : ASSEMBLY assembly_name=id
    | EXECUTABLE? FILE EQUAL path_to_file=STRING (WITH PRIVATE KEY '(' private_key_options ')')?
    ;

private_key_options
    : (FILE | BINARY) '=' path=STRING (',' (DECRYPTION | ENCRYPTION) BY PASSWORD '=' password=STRING)?
    ;

generate_new_keys
    : (ENCRYPTION BY PASSWORD '=' password=STRING)?
      WITH SUBJECT EQUAL certificate_subject_name=STRING (',' date_options)*
    ;

date_options
    : (START_DATE | EXPIRY_DATE) EQUAL STRING
    ;

open_key
    : OPEN SYMMETRIC KEY key_name=id DECRYPTION BY decryption_mechanism
    | OPEN MASTER_KEY DECRYPTION BY PASSWORD '=' password=STRING
    ;

close_key
    : CLOSE SYMMETRIC KEY key_name=id
    | CLOSE ALL SYMMETRIC KEYS
    | CLOSE MASTER_KEY
    ;

create_key
    : CREATE MASTER_KEY ENCRYPTION BY PASSWORD '=' password=STRING
    | CREATE SYMMETRIC KEY key_name=id
      (AUTHORIZATION user_name=id)?
      (FROM PROVIDER provider_name=id)?
      WITH ((key_options | ENCRYPTION BY encryption_mechanism)','?)+
    ;

key_options
    : KEY_SOURCE EQUAL pass_phrase=STRING
    | ALGORITHM EQUAL algorithm
    | IDENTITY_VALUE EQUAL identity_phrase=STRING
    | PROVIDER_KEY_NAME EQUAL key_name_in_provider=STRING
    | CREATION_DISPOSITION EQUAL (CREATE_NEW | OPEN_EXISTING)
    ;

algorithm
    : DES
    | TRIPLE_DES
    | TRIPLE_DES_3KEY
    | RC2
    | RC4
    | RC4_128
    | DESX
    | AES_128
    | AES_192
    | AES_256
    ;

encryption_mechanism
    : CERTIFICATE certificate_name=id
    | ASYMMETRIC KEY asym_key_name=id
    | SYMMETRIC KEY decrypting_Key_name=id
    | PASSWORD '=' STRING
    ;

decryption_mechanism
    : CERTIFICATE certificate_name=id (WITH PASSWORD EQUAL STRING)?
    | ASYMMETRIC KEY asym_key_name=id (WITH PASSWORD EQUAL STRING)?
    | SYMMETRIC KEY decrypting_Key_name=id
    | PASSWORD EQUAL STRING
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

dbcc_clause
    : DBCC name=simple_id ('(' expression_list ')')? (WITH dbcc_options)? ';'?
    ;

dbcc_options
    :  simple_id (',' simple_id)?
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

xml_type_definition
    : XML '(' ( CONTENT | DOCUMENT )? xml_schema_collection ')'
    ;

xml_schema_collection
    : ID '.' ID
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
      ((CONSTRAINT constraint=id)? null_or_default null_or_default?
       | IDENTITY ('(' seed=DECIMAL ',' increment=DECIMAL ')')? (NOT FOR REPLICATION)?)?
      ROWGUIDCOL?
      column_constraint*
    ;

// https://msdn.microsoft.com/en-us/library/ms186712.aspx
column_constraint
    :(CONSTRAINT constraint=id)?
      ((PRIMARY KEY | UNIQUE) clustered? index_options?
      | CHECK (NOT FOR REPLICATION)? '(' search_condition ')'
      | (FOREIGN KEY)? REFERENCES table_name '(' pk = column_name_list')' on_delete? on_update?
      | null_notnull)
    ;

// https://msdn.microsoft.com/en-us/library/ms188066.aspx
table_constraint
    : (CONSTRAINT constraint=id)?
       ((PRIMARY KEY | UNIQUE) clustered? '(' column_name_list_with_order ')' index_options? (ON id)?
         | CHECK (NOT FOR REPLICATION)? '(' search_condition ')'
         | DEFAULT '('?  (STRING | PLUS | function_call | DECIMAL)+ ')'? FOR id
         | FOREIGN KEY '(' fk = column_name_list ')' REFERENCES table_name '(' pk = column_name_list')' on_delete? on_update?)
    ;

on_delete
    : ON DELETE (NO ACTION | CASCADE | SET NULL | SET DEFAULT)
    ;

on_update
    : ON UPDATE (NO ACTION | CASCADE | SET NULL | SET DEFAULT)
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
    : declare_set_cursor_common_partial*
      FOR select_statement
    ;

declare_set_cursor_common_partial
    : (LOCAL | GLOBAL)
    | (FORWARD_ONLY | SCROLL)
    | (STATIC | KEYSET | DYNAMIC | FAST_FORWARD)
    | (READ_ONLY | SCROLL_LOCKS | OPTIMISTIC)
    | TYPE_WARNING
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
    | SET ANSI_WARNINGS on_off
    | SET modify_method
    ;

constant_LOCAL_ID
    : constant
    | LOCAL_ID
    ;

// Expression.

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/expressions-transact-sql
// Operator precendence: https://docs.microsoft.com/en-us/sql/t-sql/language-elements/operator-precedence-transact-sql
expression
    : primitive_expression
    | function_call
    | expression COLLATE id
    | case_expression
    | full_column_name
    | bracket_expression
    | unary_operator_expression
    | expression op=('*' | '/' | '%') expression
    | expression op=('+' | '-' | '&' | '^' | '|') expression
    | expression comparison_operator expression
    | expression assignment_operator expression
    | over_clause
    ;

primitive_expression
    : DEFAULT | NULL | LOCAL_ID | constant
    ;

// https://docs.microsoft.com/en-us/sql/t-sql/language-elements/case-transact-sql
case_expression
    : CASE caseExpr=expression switch_section+ (ELSE elseExpr=expression)? END
    | CASE switch_search_condition_section+ (ELSE elseExpr=expression)? END
    ;

unary_operator_expression
    : '~' expression
    | op=('+' | '-') expression
    ;

bracket_expression
    : '(' expression ')' | '(' subquery ')'
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
    | WITH BLOCKING_HIERARCHY ('(' full_column_name_list ')')? AS '(' select_statement ')'
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
    ;

query_expression
    : (query_specification | '(' query_expression ')') union*
    ;

union
    : (UNION ALL? | EXCEPT | INTERSECT) (query_specification | ('(' query_expression ')'))
    ;

// https://msdn.microsoft.com/en-us/library/ms176104.aspx
query_specification
    : SELECT (ALL | DISTINCT)? top_clause?
      select_list
      // https://msdn.microsoft.com/en-us/library/ms188029.aspx
      (INTO table_name)?
      (FROM table_sources)?
      (WHERE where=search_condition)?
      // https://msdn.microsoft.com/en-us/library/ms177673.aspx
      (GROUP BY (ALL)? group_by_item (',' group_by_item)*)?
      (HAVING having=search_condition)?
    ;

// https://msdn.microsoft.com/en-us/library/ms189463.aspx
top_clause
    : TOP (top_percent | top_count) (WITH TIES)?
    ;

top_percent
    : (REAL | FLOAT) PERCENT
    | '(' expression ')' PERCENT
    ;

top_count
    : DECIMAL
    | '(' expression ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms188385.aspx
order_by_clause
    : ORDER BY order_by_expression (',' order_by_expression)*
      (OFFSET expression (ROW | ROWS) (FETCH (FIRST | NEXT) expression (ROW | ROWS) ONLY)?)?
    ;

// https://msdn.microsoft.com/en-us/library/ms173812.aspx
for_clause
    : FOR BROWSE
    | FOR XML xml_common_directives?
    | FOR XML (AUTO | RAW | PATH | EXPLICIT) ','? ('(' STRING ')')? xml_common_directives*
    ;

xml_common_directives
    : ',' (BINARY_BASE64 | TYPE | ROOT)
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
    : LOCAL_ID (UNKNOWN | '=' (constant | NULL))
    ;

// https://msdn.microsoft.com/en-us/library/ms176104.aspx
select_list
    : select_list_elem (',' select_list_elem)*
    ;

udt_method_arguments
    : '(' execute_var_string (',' execute_var_string)* ')'
    ;

// https://docs.microsoft.com/ru-ru/sql/t-sql/queries/select-clause-transact-sql
asterisk
    : '*'
    | table_name '.' asterisk
    ;

column_elem
    : (table_name '.')? (column_name=id | '$' IDENTITY | '$' ROWGUID) as_column_alias?
    ;

udt_elem
    : udt_column_name=id '.' non_static_attr=id udt_method_arguments as_column_alias?
    | udt_column_name=id ':' ':' static_attr=id udt_method_arguments? as_column_alias?
    ;

expression_elem
    : expression as_column_alias?
    | column_alias eq='=' expression
    ;

select_list_elem
    : asterisk
    | column_elem
    | udt_elem
    | expression_elem
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
    | full_table_name             as_table_alias?
    | rowset_function             as_table_alias?
    | derived_table              (as_table_alias column_alias_list?)?
    | change_table                as_table_alias
    | function_call               as_table_alias?
    | LOCAL_ID                    as_table_alias?
    | LOCAL_ID '.' function_call (as_table_alias column_alias_list?)?
    | open_xml
    | ':' ':' function_call       as_table_alias? // Build-in function (old syntax)
    ;

//https://docs.microsoft.com/en-us/sql/t-sql/functions/openxml-transact-sql
open_xml
    : OPENXML '(' expression ',' expression (',' expression)? ')' 
    (WITH '(' schema_declaration ')' )?
    ;

schema_declaration
    : column_declaration (',' column_declaration)*
    ;

column_declaration
    : ID data_type (STRING)?
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
    | PIVOT pivot_clause as_table_alias
    | UNPIVOT unpivot_clause as_table_alias    
    ;

pivot_clause
    : '(' aggregate_windowed_function FOR full_column_name IN column_alias_list ')'
    ;

unpivot_clause
    : '(' expression FOR full_column_name IN '(' full_column_name_list ')' ')'
    ; 

full_column_name_list
    : full_column_name (',' full_column_name)*
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
    | table_value_constructor
    | '(' table_value_constructor ')'    
    ;

function_call
    : ranking_windowed_function				#RANKING_WINDOWED_FUNCTION
    | aggregate_windowed_function			#AGGREGATE_WINDOWED_FUNCTION
    | analytic_windowed_function			#ANALYTIC_WINDOWED_FUNCTION
    | scalar_function_name '(' expression_list? ')' 	#SCALAR_FUNCTION
    // https://msdn.microsoft.com/en-us/library/ms173784.aspx
    | BINARY_CHECKSUM '(' '*' ')'			#BINARY_CHECKSUM
    // https://msdn.microsoft.com/en-us/library/hh231076.aspx
    // https://msdn.microsoft.com/en-us/library/ms187928.aspx
    | CAST '(' expression AS data_type ')'		#CAST
    | CONVERT '(' convert_data_type=data_type ','convert_expression=expression (',' style=expression)? ')'				#CONVERT
    // https://msdn.microsoft.com/en-us/library/ms189788.aspx
    | CHECKSUM '(' '*' ')'				#CHECKSUM
    // https://msdn.microsoft.com/en-us/library/ms190349.aspx
    | COALESCE '(' expression_list ')'			#COALESCE
    // https://msdn.microsoft.com/en-us/library/ms188751.aspx
    | CURRENT_TIMESTAMP					#CURRENT_TIMESTAMP
    // https://msdn.microsoft.com/en-us/library/ms176050.aspx
    | CURRENT_USER					#CURRENT_USER
    // https://msdn.microsoft.com/en-us/library/ms186819.aspx
    | DATEADD '(' ID ',' expression ',' expression ')'	#DATEADD
    // https://msdn.microsoft.com/en-us/library/ms189794.aspx
    | DATEDIFF '(' ID ',' expression ',' expression ')'	#DATEDIFF
    // https://msdn.microsoft.com/en-us/library/ms174395.aspx
    | DATENAME '(' ID ',' expression ')'		#DATENAME
    // https://msdn.microsoft.com/en-us/library/ms174420.aspx
    | DATEPART '(' ID ',' expression ')'		#DATEPART
    // https://docs.microsoft.com/en-us/sql/t-sql/functions/getdate-transact-sql
    | GETDATE '(' ')'					#GETDATE
    // https://docs.microsoft.com/en-us/sql/t-sql/functions/getdate-transact-sql
    | GETUTCDATE '(' ')'				#GETUTCDATE
    // https://msdn.microsoft.com/en-us/library/ms189838.aspx
    | IDENTITY '(' data_type (',' seed=DECIMAL)? (',' increment=DECIMAL)? ')'								#IDENTITY
    // https://msdn.microsoft.com/en-us/library/bb839514.aspx
    | MIN_ACTIVE_ROWVERSION				#MIN_ACTIVE_ROWVERSION
    // https://msdn.microsoft.com/en-us/library/ms177562.aspx
    | NULLIF '(' expression ',' expression ')'		#NULLIF
    // https://msdn.microsoft.com/fr-fr/library/ms188043.aspx
    | STUFF '(' expression ',' DECIMAL ',' DECIMAL ',' expression ')'									#STUFF
    // https://msdn.microsoft.com/en-us/library/ms177587.aspx
    | SESSION_USER					#SESSION_USER
    // https://msdn.microsoft.com/en-us/library/ms179930.aspx
    | SYSTEM_USER					#SYSTEM_USER
    // https://msdn.microsoft.com/en-us/library/ms184325.aspx
    | ISNULL '(' expression ',' expression ')'		#ISNULL
    // https://docs.microsoft.com/en-us/sql/t-sql/xml/xml-data-type-methods
    | xml_data_type_methods				#XML_DATA_TYPE_METHODS
    ;

xml_data_type_methods
    : value_method
    | query_method
    | exist_method
    | modify_method
    | nodes_method
    ;

value_method
    : (LOCAL_ID | ID | EVENTDATA | query_method) '.' VALUE '(' xquery=STRING ',' sqltype=STRING ')'
    | (LOCAL_ID | ID | EVENTDATA | query_method) '.' ROW '.' VALUE '(' xquery=STRING ',' sqltype=STRING ')'
    | (LOCAL_ID | ID | EVENTDATA | query_method) '.' PARAM_NODE '.' VALUE '(' xquery=STRING ',' sqltype=STRING ')'
    ;

query_method
    : (LOCAL_ID | ID | full_table_name) '.' QUERY '(' xquery=STRING ')'
    | (LOCAL_ID | ID | full_table_name) '.' ROW '.' QUERY '(' xquery=STRING ')'
    ;

exist_method
    : (LOCAL_ID | ID) '.' EXIST '(' xquery=STRING ')'
    ;

modify_method
    : (LOCAL_ID | ID) '.' MODIFY '(' xml_dml=STRING ')'
    ;

nodes_method
    : (LOCAL_ID | ID) '.' NODES '(' xquery=STRING ')'
    ;


switch_section
    : WHEN expression THEN expression
    ;

switch_search_condition_section
    : WHEN search_condition THEN expression
    ;

as_column_alias
    : AS? column_alias
    ;

as_table_alias
    : AS? table_alias
    ;

table_alias
    : id with_table_hints?
    ;

// https://msdn.microsoft.com/en-us/library/ms187373.aspx
with_table_hints
    : WITH? '(' table_hint (','? table_hint)* ')'
    ;

// https://msdn.microsoft.com/en-us/library/ms187373.aspx
insert_with_table_hints
    : WITH '(' table_hint (','? table_hint)* ')'
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
                | ID)
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

// https://docs.microsoft.com/en-us/sql/t-sql/functions/analytic-functions-transact-sql
analytic_windowed_function
	: (FIRST_VALUE | LAST_VALUE) '(' expression ')' over_clause
	| (LAG | LEAD) '(' expression  (',' expression (',' expression)? )? ')' over_clause
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
    | (database=id '.' (schema=id)? '.' | schema=id '.')? BLOCKING_HIERARCHY
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
/*  There are some RESERVED WORDS that can be column names */
full_column_name
    : (table_name '.')? id
    | (table_name '.')? COMPATIBILITY_LEVEL
    | (table_name '.')? STATUS
    | (table_name '.')? QUOTED_IDENTIFIER
    | (table_name '.')? ARITHABORT
    | (table_name '.')? ANSI_WARNINGS
    | (table_name '.')? ANSI_PADDING
    | (table_name '.')? ANSI_NULLS

    ;

column_name_list_with_order
    : id (ASC | DESC)? (',' id (ASC | DESC)?)*
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

null_or_default
    :(null_notnull | DEFAULT constant_expression (WITH VALUES)?)
    ;

scalar_function_name
    : func_proc_name
    | RIGHT
    | LEFT
    | BINARY_CHECKSUM
    | CHECKSUM
    ;

begin_conversation_timer
    : BEGIN CONVERSATION TIMER '(' LOCAL_ID ')' TIMEOUT '=' time ';'?
    ;

begin_conversation_dialog
    : BEGIN DIALOG (CONVERSATION)? dialog_handle=LOCAL_ID
      FROM SERVICE initiator_service_name=service_name
      TO SERVICE target_service_name=service_name (',' service_broker_guid=STRING)?
      ON CONTRACT contract_name
      (WITH
        ((RELATED_CONVERSATION | RELATED_CONVERSATION_GROUP) '=' LOCAL_ID ','?)?
        (LIFETIME '=' (DECIMAL | LOCAL_ID) ','?)?
        (ENCRYPTION '=' (ON | OFF))? )?
      ';'?
    ;

contract_name
    : (id | expression)
    ;

service_name
    : (id | expression)
    ;

end_conversation
    : END CONVERSATION conversation_handle=LOCAL_ID ';'?
      (WITH (ERROR '=' faliure_code=(LOCAL_ID | STRING) DESCRIPTION '=' failure_text=(LOCAL_ID | STRING))? CLEANUP? )?
    ;

waitfor_conversation
    : WAITFOR? '(' get_conversation ')' (','? TIMEOUT timeout=time)? ';'?
    ;

get_conversation
    :GET CONVERSATION GROUP conversation_group_id=(STRING | LOCAL_ID) FROM queue=queue_id ';'?
    ;

queue_id
    : (database_name=id '.' schema_name=id '.' name=id)
    | id
    ;

send_conversation
    : SEND ON CONVERSATION conversation_handle=(STRING | LOCAL_ID)
      MESSAGE TYPE message_type_name=expression
      ('(' message_body_expression=(STRING | LOCAL_ID) ')' )?
      ';'?
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
    | DEFAULT
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
    | ACTIVE
    | APPLY
    | AUTO
    | AVG
    | CALLED
    | CALLER
    | CAST
    | CATCH
    | CHECKSUM_AGG
    | COMMITTED
    | CONCAT
    | CONCAT_NULL_YIELDS_NULL
    | CONTROL
    | COOKIE
    | COUNT
    | COUNT_BIG
    | DATA_COMPRESSION
    | DELAY
    | DELETED
    | DENSE_RANK
    | DISABLE
    | DYNAMIC
    | ENCRYPTION
    | EVENTDATA
    | EXPAND
    | FAST
    | FAST_FORWARD
    | FILLFACTOR
    | FIRST
    | FOLLOWING
    | FORCE
    | FORCESEEK
    | FORWARD_ONLY
    | FULLSCAN
    | GLOBAL
    | GROUPING
    | GROUPING_ID
    | HASH
    | IMPERSONATE
    | INSENSITIVE
    | INSERTED
    | ISOLATION
    | KEEP
    | KEEPFIXED
    | KEY
    | FORCED
    | KEYSET
    | IGNORE_NONCLUSTERED_COLUMNSTORE_INDEX
    | INPUT
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
    | OFFSETS
    | ONLINE
    | ONLY
    | OPTIMISTIC
    | OPTIMIZE
    | OUT
    | OUTPUT
    | OWNER
    | PAGE
    | PARAMETERIZATION
    | PARTITION
    | PATH
    | PRECEDING
    | PRIOR
    | PRIVILEGES
    | PUBLIC
    | RANGE
    | RANK
    | RAW
    | READONLY
    | READ_ONLY
    | RECOMPILE
    | RELATIVE
    | REMOTE
    | REPEATABLE
    | RETURN
    | RETURNS
    | ROBUST
    | ROOT
    | ROW
    | ROWCOUNT
    | ROWGUID
    | ROWS
    | ROW_NUMBER
    | SAFETY
    | SAMPLE
    | SIZE
    | SCHEMABINDING
    | SCROLL
    | SCROLL_LOCKS
    | SELF
    | SERIALIZABLE
    | SERVER
    | SIMPLE
    | SNAPSHOT
    | SOURCE
    | SPATIAL_WINDOW_MAX_CELLS
    | STATE
    | STATIC
    | STATS_STREAM
    | STDEV
    | STDEVP
    | SUM
    | TARGET
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
    | VALUE
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

file_size
    : DECIMAL( KB | MB | GB | TB | '%' )?
    ;
