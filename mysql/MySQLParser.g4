/*
MySQL (Positive Technologies) grammar
The MIT License (MIT).
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2017, Ivan Khudyashev (IHudyashov@ptsecurity.com)

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

parser grammar MySQLParser;

options { tokenVocab=MySQLLexer; }


// Top Level Description

root
    : sql_statements? (MINUS MINUS)? EOF
   ;

sql_statements
    : (sql_statement (MINUS MINUS)? SEMI | empty_statement)* 
   (sql_statement ((MINUS MINUS)? SEMI)? | empty_statement)
   ;
   
sql_statement
    : ddl_statement | dml_statement | transaction_statement
   | replication_statement | prepared_statement
   | administration_statement | utility_statement
   ;

empty_statement
    : SEMI
   ;

ddl_statement
    : create_database | create_event | create_index
   | create_logfile_group | create_procedure | create_function
   | create_server | create_table | create_tablespace_innodb
   | create_tablespace_ndb | create_trigger | create_view 
   | alter_database | alter_event | alter_function 
   | alter_instance | alter_logfile_group | alter_procedure 
   | alter_server | alter_table | alter_tablespace | alter_view
   | drop_database | drop_event | drop_index 
   | drop_logfile_group | drop_procedure | drop_function 
   | drop_server | drop_table | drop_tablespace 
   | drop_trigger | drop_view 
   | rename_table | truncate_table
   ;

dml_statement
    : select_statement | insert_statement | update_statement 
   | delete_statement | replace_statement | call_statement
   | load_data_statement | load_xml_statement | do_statement
   | handler_statement
   ;

transaction_statement
    : start_transaction 
   | begin_work | commit_work | rollback_work
   | savepoint_statement | rollback_statement 
   | release_statement | lock_tables | unlock_tables
   ;

replication_statement
    : change_master | change_repl_filter | purge_binary_logs
   | reset_master | reset_slave | start_slave | stop_slave
   | start_group_repl | stop_group_repl 
   | xa_start_transaction | xa_end_transaction | xa_prepare
   | xa_commit_work | xa_rollback_work | xa_recover_work
   ;

prepared_statement
    : prepare_statement | execute_statement | deallocate_prepare
   ;

// remark: NOT INCLUDED IN sql_statement, but include in body
//  of routine's statements
compound_statement
    : block_statement 
   | case_statement | if_statement | leave_statement 
   | loop_statement | repeat_statement | while_statement 
   | iterate_statement | return_statement | cursor_statement
   ;

administration_statement
    : alter_user | create_user | drop_user | grant_statement 
   | grant_proxy | rename_user | revoke_statement 
   | revoke_proxy | analyze_table | check_table 
   | checksum_table | optimize_table | repair_table 
   | create_udfunction | install_plugin | uninstall_plugin 
   | set_statement | show_statement | binlog_statement 
   | cache_index_statement | flush_statement | kill_statement 
   | load_index_into_cache | reset_statement 
   | shutdown_statement
   ;

utility_statement
    : simple_describe_statement | full_describe_statement 
   | help_statement | use_statement
   ;


// Data Definition Language

//    Create statements

create_database
    : CREATE (DATABASE | SCHEMA) 
   if_not_exists? id_ create_database_option*
   ;

create_event
    : CREATE owner_statement? EVENT if_not_exists? full_id
     ON SCHEDULE schedule_expression
     (ON COMPLETION NOT? PRESERVE)?
     (ENABLE | DISABLE | DISABLE ON SLAVE)?
     (COMMENT STRING_LITERAL)?
   DO routine_body
   ;

create_index
    : CREATE 
     (ONLINE | OFFLINE)? 
     index_category=(UNIQUE | FULLTEXT | SPATIAL)? 
     INDEX id_ index_type? 
     ON table_name index_colname_list
     index_option* 
     (
       ALGORITHM '='? alg_type=(DEFAULT | INPLACE | COPY) 
       | LOCK '='? 
         lock_type=(DEFAULT | NONE | SHARED | EXCLUSIVE)
     )?
   ;

create_logfile_group
    : CREATE LOGFILE GROUP id_
     ADD UNDOFILE undo_file=STRING_LITERAL
     (INITIAL_SIZE '='? init_size=filesize_literal)?
     (UNDO_BUFFER_SIZE '='? undo_size=filesize_literal)?
     (REDO_BUFFER_SIZE '='? redo_size=filesize_literal)?
     (NODEGROUP '='? id_)?
     WAIT?
     (COMMENT '='? comment=STRING_LITERAL)?
     ENGINE '='? engine_name
   ;

create_procedure
    : CREATE owner_statement?
   PROCEDURE full_id 
     '(' proc_param? (',' proc_param)* ')' 
     routine_characteristic* 
   routine_body
   ;

create_function
    : CREATE owner_statement?
   FUNCTION full_id
     '(' func_param? (',' func_param)* ')' 
     RETURNS data_type 
     routine_characteristic* 
   routine_body
   ;

create_server
    : CREATE SERVER id_
   FOREIGN DATA WRAPPER (MYSQL | STRING_LITERAL)
   OPTIONS '(' server_option (',' server_option)* ')'
   ;

create_table
    : CREATE TEMPORARY? TABLE if_not_exists? 
      table_name (LIKE table_name | '(' LIKE table_name ')' )     #copyCreateTable
   | CREATE TEMPORARY? TABLE if_not_exists? 
      table_name column_def_table_constraints?
      ( table_option (','? table_option)* )?
      partition_options? (IGNORE | REPLACE)?
      AS? select_statement                               #queryCreateTable
   | CREATE TEMPORARY? TABLE if_not_exists? 
      table_name column_def_table_constraints 
      ( table_option (','? table_option)* )?
      partition_options?                                 #colCreateTable
   ;

create_tablespace_innodb
    : CREATE TABLESPACE id_ 
     ADD DATAFILE datafile=STRING_LITERAL
     (FILE_BLOCK_SIZE '=' fb_size=filesize_literal)?
     (ENGINE '='? engine_name)?
   ;

create_tablespace_ndb
    : CREATE TABLESPACE id_ 
     ADD DATAFILE datafile=STRING_LITERAL
     USE LOGFILE GROUP id_
     (EXTENT_SIZE '='? extent_size=filesize_literal)?
     (INITIAL_SIZE '='? initial_size=filesize_literal)?
     (AUTOEXTEND_SIZE '='? autoextend_size=filesize_literal)?
     (MAX_SIZE '='? max_size=filesize_literal)?
     (NODEGROUP '='? id_)?
     WAIT?
     (COMMENT '='? comment=STRING_LITERAL)?
     ENGINE '='? engine_name
   ;

create_trigger
    : CREATE owner_statement? 
     TRIGGER this_trigger=full_id 
     trigger_time=(BEFORE | AFTER)
     trigger_event=(INSERT | UPDATE | DELETE)
     ON table_name FOR EACH ROW 
     ((FOLLOWS | PRECEDES) other_trigger=full_id)?
   routine_body
   ;

create_view
    : CREATE (OR REPLACE)? 
     (
       ALGORITHM '=' alg_type=(UNDEFINED | MERGE | TEMPTABLE)
     )? 
     owner_statement? 
     (SQL SECURITY sec_context=(DEFINER | INVOKER))? 
     VIEW full_id ('(' id_list ')')? AS select_statement
     (WITH check_option=(CASCADED | LOCAL)? CHECK OPTION)?
   ;

// details

create_database_option
    : DEFAULT? CHARACTER SET '='? charset_name
   | DEFAULT? COLLATE '='? collation_name
   ;

owner_statement
    : DEFINER '=' (user_name | CURRENT_USER ( '(' ')')?)
   ;

schedule_expression
    : AT timestamp_value interval_expr*                        #preciseSchedule
   | EVERY (decimal_literal | expression) interval_type
       (
         STARTS startts=timestamp_value 
         (start_intervals+=interval_expr)*
       )? 
       (
         ENDS endts=timestamp_value 
         (end_intervals+=interval_expr)*
       )?                                             #intervalSchedule
   ;

timestamp_value
    : CURRENT_TIMESTAMP
   | string_literal
   | decimal_literal
   | expression
   ;

interval_expr
    : '+' INTERVAL (decimal_literal | expression) interval_type
   ;

interval_type
    : interval_type_base
   | YEAR | YEAR_MONTH | DAY_HOUR | DAY_MINUTE
   | DAY_SECOND | HOUR_MINUTE | HOUR_SECOND | MINUTE_SECOND
   | SECOND_MICROSECOND | MINUTE_MICROSECOND
   | HOUR_MICROSECOND | DAY_MICROSECOND
   ;

index_type
    : USING (BTREE | HASH)
   ;

index_option
    : KEY_BLOCK_SIZE '='? filesize_literal
   | index_type
   | WITH PARSER id_
   | COMMENT STRING_LITERAL
   ;

proc_param
    : (IN | OUT | INOUT) id_ data_type
   ;

func_param
    : id_ data_type
   ;

routine_characteristic
    : COMMENT STRING_LITERAL                             #rcComment
   | LANGUAGE SQL                                     #rcSqllang
   | NOT? DETERMINISTIC                               #rcDeterm
   | (
       CONTAINS SQL | NO SQL | READS SQL DATA 
       | MODIFIES SQL DATA
     )                                                #rcSqldata
   | SQL SECURITY sec_context=(DEFINER | INVOKER)              #rcSecurestmt
   ;

server_option
    : HOST STRING_LITERAL
   | DATABASE STRING_LITERAL
   | USER STRING_LITERAL
   | PASSWORD STRING_LITERAL
   | SOCKET STRING_LITERAL
   | OWNER STRING_LITERAL
   | PORT decimal_literal
   ;

column_def_table_constraints
    : '(' 
     column_def_table_constraint
     (',' column_def_table_constraint)* 
   ')'
   ;

column_def_table_constraint
    : id_ column_definition                                 #columnDefinition
   | table_constraint                                    #constraintDefinition
   | index_column_definition                             #indexDefinition
   ;

column_definition
    : data_type separate_column_constraint*
   ;

separate_column_constraint
    : null_notnull                                       #colConstrNull
   | DEFAULT default_value                               #colConstrDflt
   | AUTO_INCREMENT                                   #colConstrAuInc
   | PRIMARY? KEY                                     #colConstrPK
   | UNIQUE KEY?                                      #colConstrUK
   | COMMENT STRING_LITERAL                              #colConstrComment
   | COLUMN_FORMAT colformat=(FIXED | DYNAMIC | DEFAULT)       #colConstrForm
   | STORAGE storageval=(DISK | MEMORY | DEFAULT)              #colConstrStorage
   | reference_definition                                #colConstrRefdef
   ;

table_constraint
    : (CONSTRAINT constr_name=id_?)? 
     PRIMARY KEY index_type? index_colname_list index_option*     #tblConstrPK
   | (CONSTRAINT constr_name=id_?)? 
       UNIQUE (INDEX | KEY)? index_name=id_? index_type? 
       index_colname_list index_option*                     #tblConstrUK
   | (CONSTRAINT constr_name=id_?)? 
       FOREIGN KEY index_name=id_? index_colname_list 
       reference_definition                              #tblConstrFK
   | CHECK '(' expression ')'                            #tblConstCheck
   ;

reference_definition
    : REFERENCES table_name index_colname_list 
     (MATCH ref_match_type=(FULL | PARTIAL | SIMPLE))? 
     (on_delete_action | on_update_action)?
   ;

on_delete_action
    : ON DELETE reference_action_control_type
     (
       ON UPDATE reference_action_control_type
     )?
   ;
on_update_action
    : ON UPDATE reference_action_control_type
     (
       ON DELETE reference_action_control_type
     )?
   ;

reference_action_control_type
    : RESTRICT | CASCADE | SET NULL_LITERAL | NO ACTION
   ;

index_column_definition
    : (INDEX | KEY) id_? index_type? 
     index_colname_list index_option*                       #simpleIndex
   | (FULLTEXT | SPATIAL) 
       (INDEX | KEY)? id_? 
       index_colname_list index_option*                     #specIndex
   ;

table_option
    : ENGINE '='? engine_name                            #tblOptEngine
   | AUTO_INCREMENT '='? decimal_literal                    #tblOptAuInc
   | AVG_ROW_LENGTH '='? decimal_literal                    #tblOptAvgRLen
   | DEFAULT? (CHARACTER SET | CHARSET) '='? charset_name         #tblOptDefCharSet
   | CHECKSUM '='? ('0' | '1')                              #tblOptChkSum
   | DEFAULT? COLLATE '='? collation_name                   #tblOptDefCollate
   | COMMENT '='? STRING_LITERAL                         #tblOptComment
   | COMPRESSION '='? STRING_LITERAL                        #tblOptCompr
   | CONNECTION '='? STRING_LITERAL                      #tblOptConn
   | DATA DIRECTORY '='? STRING_LITERAL                     #tblOptDataDir
   | DELAY_KEY_WRITE '='? ('0' | '1')                       #tblOptDelKW
   | ENCRYPTION '='? STRING_LITERAL                      #tblOptEncr
   | INDEX DIRECTORY '='? STRING_LITERAL                    #tblOptIndexDir
   | INSERT_METHOD '='? (NO | FIRST | LAST)                 #tblOptInsMeth
   | KEY_BLOCK_SIZE '='? filesize_literal                   #tblOptKeyBlockSz
   | MAX_ROWS '='? decimal_literal                          #tblOptMaxRows
   | MIN_ROWS '='? decimal_literal                          #tblOptMinRows
   | PACK_KEYS '='? ('0' | '1' | DEFAULT)                   #tblOptPackK
   | PASSWORD '='? STRING_LITERAL                           #tblOptPasswd
   | ROW_FORMAT '='? 
       (
         DEFAULT | DYNAMIC | FIXED | COMPRESSED
         | REDUNDANT | COMPACT
       )                                           #tblOptRowFormat
   | STATS_AUTO_RECALC '='? (DEFAULT | '0' | '1')              #tblOptStatAutoR
   | STATS_PERSISTENT '='? (DEFAULT | '0' | '1')               #tblOptStatPersist
   | STATS_SAMPLE_PAGES '='? decimal_literal                #tblOptStatSamplPg
   | TABLESPACE id_ (STORAGE (DISK | MEMORY | DEFAULT))?       #tblOptTablespace
   | UNION '='? '(' table_name (',' table_name)* ')'           #tblOptUnion
   ;

partition_options
    : PARTITION BY partition_function_definition 
     (PARTITIONS part_num=decimal_literal)? 
     (
       SUBPARTITION BY linear_partition_func_def
       (SUBPARTITIONS subpart_num=decimal_literal)? 
     )? 
   ('(' partition_def (',' partition_def)* ')')?
   ;

partition_function_definition
    : linear_partition_func_def
   | (RANGE | LIST) 
       (
         '(' expression ')' 
         | COLUMNS '(' id_list ')' 
       )
   ;

linear_partition_func_def
    : LINEAR? HASH '(' expression ')'
   | LINEAR? KEY (ALGORITHM '=' ('1' | '2'))? '(' id_list ')'
   ;

partition_def
    : PARTITION id_ 
     (
       VALUES 
       (
         LESS THAN 
           ( 
             '(' (expression | constant_list) ')' 
             | MAXVALUE
           )
         | IN  '(' constant_list ')' 
       ) 
     )?
     (STORAGE? ENGINE '='? engine_name)?
     (COMMENT '='? comment=STRING_LITERAL)?
     (DATA DIRECTORY '='? data_dir=STRING_LITERAL)?
     (INDEX DIRECTORY '='? index_dir=STRING_LITERAL)?
     (MAX_ROWS '='? max_row_num=decimal_literal)?
     (MIN_ROWS '='? min_row_num=decimal_literal)?
     (TABLESPACE '='? tblspace_id=id_)?
     (NODEGROUP '='? nodegroup_id=id_)?
     (subpartition_def (',' subpartition_def)*)?
   ;

subpartition_def
    : SUBPARTITION id_
     (STORAGE? ENGINE '='? engine_name)?
     (COMMENT '='? comment=STRING_LITERAL)?
     (DATA DIRECTORY '='? data_dir=STRING_LITERAL)?
     (INDEX DIRECTORY '='? index_dir=STRING_LITERAL)?
     (MAX_ROWS '='? max_row_num=decimal_literal)?
     (MIN_ROWS '='? min_row_num=decimal_literal)?
     (TABLESPACE '='? tblspace_id=id_)?
     (NODEGROUP '='? nodegroup_id=id_)?
   ;


//    Alter statements

alter_database
    : ALTER (DATABASE | SCHEMA) id_? create_database_option+      #alterDb
   | ALTER (DATABASE | SCHEMA) id_ 
       UPGRADE DATA DIRECTORY NAME                          #alterDbUpgradeName
   ;

alter_event
    : ALTER owner_statement? 
   EVENT full_id
     (ON SCHEDULE schedule_expression)?
     (ON COMPLETION NOT? PRESERVE)?
     (RENAME TO full_id)?
     (ENABLE | DISABLE | DISABLE ON SLAVE)?
     (COMMENT STRING_LITERAL)?
     (DO routine_body)?
   ;

alter_function
    : ALTER FUNCTION full_id routine_characteristic*
   ;

alter_instance
    : ALTER INSTANCE ROTATE INNODB MASTER KEY
   ;

alter_logfile_group
    : ALTER LOGFILE GROUP id_
   ADD UNDOFILE STRING_LITERAL
   (INITIAL_SIZE '='? filesize_literal)?
   WAIT? ENGINE '='? engine_name
   ;

alter_procedure
    : ALTER PROCEDURE full_id routine_characteristic*
   ;

alter_server
    : ALTER SERVER id_ OPTIONS 
   '(' server_option (',' server_option)* ')'
   ;

alter_table
    : ALTER (ONLINE | OFFLINE)? IGNORE? TABLE table_name
   alter_table_spec (',' alter_table_spec)* 
   partition_options*
   ;

alter_tablespace
    : ALTER TABLESPACE id_
   (ADD | DROP) DATAFILE STRING_LITERAL
   (INITIAL_SIZE '=' filesize_literal)?
   WAIT?
   ENGINE '='? engine_name
   ;

alter_view
    : ALTER 
     (
       ALGORITHM '=' alg_type=(UNDEFINED | MERGE | TEMPTABLE)
     )?
     owner_statement? 
     (SQL SECURITY sec_context=(DEFINER | INVOKER))?
     VIEW full_id ('(' id_list ')')? AS select_statement
     (WITH check_opt=(CASCADED | LOCAL)? CHECK OPTION)?
   ;

// details

alter_table_spec
    : table_option                                       #altblTableOpt
   | ADD COLUMN? id_ column_definition (FIRST | AFTER id_)?    #altblAddCol
   | ADD COLUMN? 
       '(' 
         id_ column_definition ( ',' id_ column_definition)*
       ')'                                            #altblAddCols
   | ADD (INDEX | KEY) id_? index_type? 
       index_colname_list index_option*                     #altblAddIndex
   | ADD (CONSTRAINT id_?)? PRIMARY KEY 
       index_type? index_colname_list index_option*            #altblAddPK
   | ADD (CONSTRAINT id_?)? UNIQUE (INDEX | KEY)? id_? 
       index_type? index_colname_list index_option*            #altblAddUK
   | ADD (FULLTEXT | SPATIAL) (INDEX | KEY)? id_? 
       index_colname_list index_option*                     #altblAddSpecIndex
   | ADD (CONSTRAINT id_?)? FOREIGN KEY id_? 
       index_colname_list reference_definition                 #altblAddFK
   | ALGORITHM '='? (DEFAULT | INPLACE | COPY)                 #altblAlg
   | ALTER COLUMN? id_ 
     (SET DEFAULT default_value | DROP DEFAULT)             #altblColDef
   | CHANGE COLUMN? id_ 
       id_ column_definition (FIRST | AFTER id_)?              #altblColChange
   | LOCK '='? (DEFAULT | NONE | SHARED | EXCLUSIVE)           #altblLock
   | MODIFY COLUMN? 
       id_ column_definition (FIRST | AFTER id_)?              #altblColMod
   | DROP COLUMN? id_                                    #altblColDrop
   | DROP PRIMARY KEY                                    #altblDropPK
   | DROP (INDEX | KEY) id_                              #altblDropIndex
   | DROP FOREIGN KEY id_                                #altblDropFK
   | DISABLE KEYS                                     #altblDisKey
   | ENABLE KEYS                                      #altblEnKey
   | RENAME (TO | AS)? id_                               #altblRenameTbl
   | ORDER BY id_list                                    #altblResort
   | CONVERT TO CHARACTER SET charset_name 
       (COLLATE collation_name)?                         #altblConvert
   | DEFAULT? CHARACTER SET '=' charset_name 
       (COLLATE '=' collation_name)?                        #altblDefCharset
   | DISCARD TABLESPACE                               #altblDisTblspace
   | IMPORT TABLESPACE                                   #altblImpTblSpace
   | FORCE                                            #altblForce
   | (WITHOUT | WITH) VALIDATION                         #altblValid
   | ADD PARTITION partition_def                         #altblAddPart
   | DROP PARTITION id_list                              #altblDropPart
   | DISCARD PARTITION (id_list | ALL) TABLESPACE              #altblDiscartPart
   | IMPORT PARTITION (id_list | ALL) TABLESPACE               #altblImportPart
   | TRUNCATE PARTITION (id_list | ALL)                     #altblTruncPart
   | COALESCE PARTITION decimal_literal                     #altblCoalPart
   | REORGANIZE PARTITION id_list 
       INTO '(' partition_def (',' partition_def)* ')'            #altblReorgPart
   | EXCHANGE PARTITION id_ WITH TABLE table_name 
       ((WITH | WITHOUT) VALIDATION)?                       #altblExchPart
   | ANALYZE PARTITION (id_list | ALL)                      #altblAnalPart
   | CHECK PARTITION (id_list | ALL)                        #altblCheckPart
   | OPTIMIZE PARTITION (id_list | ALL)                     #altblOptimPart
   | REBUILD PARTITION (id_list | ALL)                      #altblRebuildPart
   | REPAIR PARTITION (id_list | ALL)                       #altblRepairPart
   | REMOVE PARTITIONING                                 #altblRemovePart
   | UPGRADE PARTITIONING                                #altblUpgrPart
   ;


//    Drop statements

drop_database
    : DROP (DATABASE | SCHEMA) if_exists? id_
   ;

drop_event
    : DROP EVENT if_exists? full_id
   ;

drop_index
    : DROP INDEX (ONLINE | OFFLINE)? id_ ON table_name
     (
       ALGORITHM '='? (DEFAULT | INPLACE | COPY)
     )? 
     (
       LOCK '='? (DEFAULT | NONE | SHARED | EXCLUSIVE)
     )?
   ;

drop_logfile_group
    : DROP LOGFILE GROUP id_ ENGINE '=' engine_name
   ;

drop_procedure
    : DROP PROCEDURE if_exists? full_id
   ;

drop_function
    : DROP FUNCTION if_exists? full_id
   ;

drop_server
    : DROP SERVER if_exists? id_
   ;

drop_table
    : DROP TEMPORARY? TABLE if_exists? 
   table_list (RESTRICT | CASCADE)?
   ;

drop_tablespace
    : DROP TABLESPACE id_ (ENGINE '='? engine_name)?
   ;

drop_trigger
    : DROP TRIGGER if_exists? full_id
   ;

drop_view
    : DROP VIEW if_exists? 
   full_id (',' full_id)* (RESTRICT | CASCADE)?
   ;


//    Other DDL statements

rename_table
    : RENAME TABLE 
     table_name TO table_name 
     (',' table_name TO table_name)*
   ;

truncate_table
    : TRUNCATE TABLE? table_name
   ;


// Data Manipulation Language

//    Primary DML Statements


call_statement
    : CALL full_id
   (
     '(' (constant_list | expression_list)? ')'
   )?
   ;

delete_statement
    : single_delete_statement | multiple_delete_statement
   ;

do_statement
    : DO expression_list
   ;

handler_statement
    : handler_open_statement
   | handler_read_index_statement
   | handler_read_statement
   | handler_close_statement
   ;

insert_statement
    : INSERT 
     (LOW_PRIORITY | DELAYED | HIGH_PRIORITY)? IGNORE?
     INTO? table_name
     (PARTITION '(' id_list ')' )?
     (
       ('(' id_list ')')? insert_statement_value
       | SET 
           set_firstelem=update_elem 
           (',' set_elem+=update_elem)*
     )
     (
       ON DUPLICATE KEY UPDATE 
       duplicate_firstelem=update_elem 
       (',' duplicate_elem+=update_elem)*
     )?
   ;

load_data_statement
    : LOAD DATA 
     priority=(LOW_PRIORITY | CONCURRENT)? 
     LOCAL? INFILE filename=STRING_LITERAL
     replaceignore=(REPLACE | IGNORE)? 
   INTO TABLE table_name
     (PARTITION '(' id_list ')' )?
     (CHARACTER SET charset=charset_name)?
     (
       (FIELDS | COLUMNS)
       (TERMINATED BY terminatefieldsymb=STRING_LITERAL)?
       (OPTIONALLY? ENCLOSED BY enclosedsymb=STRING_LITERAL)?
       (ESCAPED BY escapesymb=STRING_LITERAL)?
     )?
     (
       LINES 
         (STARTING BY startingsymb=STRING_LITERAL)? 
         (TERMINATED BY terminatelinesymb=STRING_LITERAL)?
     )?
     ( 
       IGNORE decimal_literal (LINES | ROWS) 
     )?
   ( '(' col_or_uservar (',' col_or_uservar)* ')' )?
     (SET update_elem (',' update_elem)*)? 
   ;

load_xml_statement
    : LOAD XML 
     priority=(LOW_PRIORITY | CONCURRENT)? 
     LOCAL? INFILE STRING_LITERAL
     (REPLACE | IGNORE)? 
   INTO TABLE table_name
     (CHARACTER SET charset_name)?
     (ROWS IDENTIFIED BY '<' STRING_LITERAL '>')?
     ( IGNORE decimal_literal (LINES | ROWS) )?
   ( '(' col_or_uservar (',' col_or_uservar)* ')' )?
     (SET update_elem (',' update_elem)*)? 
   ;

replace_statement
    : REPLACE (LOW_PRIORITY | DELAYED)? 
     INTO? table_name
     (PARTITION '(' id_list ')' )?
     (
       ('(' id_list ')')? insert_statement_value
       | SET 
           set_firstelem=update_elem 
           (',' set_elem+=update_elem)*
     )
   ;

select_statement
    : query_specification (FOR UPDATE | LOCK IN SHARE MODE)?      #simpleSelect
   | query_expression (FOR UPDATE | LOCK IN SHARE MODE)?          #parenSelect
   | query_specification_nointo union_statement+ 
       (
         UNION (ALL | DISTINCT)? 
         (query_specification | query_expression)
       )?
       order_by_clause? limit_clause? 
       (FOR UPDATE | LOCK IN SHARE MODE)?                   #unionSelect
   | query_expression_nointo union_parenth+ 
       (
         UNION (ALL | DISTINCT)?
         query_expression
       )? 
       order_by_clause? limit_clause? 
       (FOR UPDATE | LOCK IN SHARE MODE)?                   #unionParenSelect
   ;

update_statement
    : single_update_statement | multiple_update_statement
   ;

// details

insert_statement_value
    : select_statement
   | (VALUES | VALUE) 
       '(' expression_list ')' 
       (',' '(' expression_list ')')*
   ;

update_elem
    : full_column_name '=' expression
   ;

col_or_uservar
    : id_ | LOCAL_ID
   ;


//    Detailed DML Statements

single_delete_statement
    : DELETE LOW_PRIORITY? QUICK? IGNORE? FROM table_name
   (PARTITION '(' id_list ')' )?
   (WHERE expression)? 
   order_by_clause? (LIMIT decimal_literal)?
   ;

multiple_delete_statement
    : DELETE LOW_PRIORITY? QUICK? IGNORE?
   (
     table_name ('.' '*')? ( ',' table_name ('.' '*')? )* 
       FROM table_sources
     | FROM 
         table_name ('.' '*')? ( ',' table_name ('.' '*')? )*
         USING table_sources
   )
   (WHERE expression)?
   ;

handler_open_statement
    : HANDLER table_name OPEN (AS? id_)?
   ;

handler_read_index_statement
    : HANDLER table_name READ index=full_id 
     (
       comparison_operator '(' constant_list ')'
       | move_order=(FIRST | NEXT | PREV | LAST)
     )
     (WHERE expression)? (LIMIT decimal_literal)?
   ;

handler_read_statement
    : HANDLER table_name READ (FIRST | NEXT)
   (WHERE expression)? (LIMIT decimal_literal)?
   ;

handler_close_statement
    : HANDLER table_name CLOSE
   ;

single_update_statement
    : UPDATE LOW_PRIORITY? IGNORE? table_name (AS? id_)?
   SET update_elem (',' update_elem)*
   (WHERE expression)? order_by_clause? limit_clause?
   ;

multiple_update_statement
    : UPDATE LOW_PRIORITY? IGNORE? table_sources
   SET update_elem (',' update_elem)*
   (WHERE expression)?
   ;

// details

order_by_clause
    : ORDER BY order_by_expression (',' order_by_expression)*
   ;

order_by_expression
    : expression (ASC | DESC)?
   ;

table_sources
    : table_source (',' table_source)*
   ;

table_source
    : table_source_item join_part*
   | '(' table_source_item join_part* ')'
   ;

table_source_item
    : table_name 
     (PARTITION '(' id_list ')' )? (AS? alias=id_)? 
     (index_hint (',' index_hint)* )?                       #atomTableItem
   | (subquery | '(' subquery ')') AS? alias=id_               #subqueryTableItem
   | '(' table_sources ')'                               #tableSourcesItem
   ;

index_hint
    : (USE | IGNORE | FORCE) (INDEX|KEY) 
   (FOR (JOIN | ORDER BY | GROUP BY))? '(' id_list ')'
   ;

join_part
    : (INNER | CROSS)? JOIN 
     table_source_item 
     (
       ON expression 
       | USING '(' id_list ')'
     )?                                            #innerJoin
   | STRAIGHT_JOIN table_source_item (ON expression)?          #straightJoin
   | (LEFT | RIGHT) OUTER? JOIN 
       table_source_item 
       (
         ON expression | 
         USING '(' id_list ')'
       )                                           #outerJoin
   | NATURAL ((LEFT | RIGHT) OUTER?)? JOIN table_source_item      #naturalJoin
   ;

subquery
    : select_statement;


//    Select Statement's Details

query_expression
    : '(' query_specification ')'
   | '(' query_expression ')'
   ;

query_expression_nointo
    : '(' query_specification_nointo ')'
   | '(' query_expression_nointo ')'
   ;

query_specification
    : SELECT select_spec* select_list select_into_expression? 
   from_clause? order_by_clause? limit_clause?
   ;

query_specification_nointo
    : SELECT select_spec* select_list 
   from_clause? order_by_clause? limit_clause?
   ;

union_parenth
    : UNION (ALL|DISTINCT)? query_expression_nointo
   ;
   
union_statement
    : UNION (ALL|DISTINCT)? 
   (query_specification_nointo | query_expression_nointo)
   ;

// details

select_spec
    : (ALL|DISTINCT|DISTINCTROW)
   | HIGH_PRIORITY | STRAIGHT_JOIN | SQL_SMALL_RESULT 
   | SQL_BIG_RESULT | SQL_BUFFER_RESULT 
   | (SQL_CACHE|SQL_NO_CACHE)
   | SQL_CALC_FOUND_ROWS
   ;

select_list
    : ('*' | select_list_elem ) (',' select_list_elem)*
   ;

select_list_elem
    : full_id '.' '*'                                    #sellistelAllCol
   | full_column_name (AS? id_)?                         #sellistelCol
   | function_call (AS? id_)?                            #sellistelFunc
   | (LOCAL_ID VAR_ASSIGN)? expression (AS? id_)?              #sellistelExpr
   ;

select_into_expression
    : INTO (LOCAL_ID | id_) (',' (LOCAL_ID | id_) )*           #selectIntoVars
   | INTO DUMPFILE STRING_LITERAL                           #selectIntoDump
   | (
      INTO OUTFILE filename=STRING_LITERAL 
      (CHARACTER SET charset=charset_name)?
      (
        (FIELDS | COLUMNS) 
          (TERMINATED BY terminatefieldsymb=STRING_LITERAL)?
          (
            OPTIONALLY? 
            ENCLOSED BY enclosedsymb=STRING_LITERAL
          )?
          (ESCAPED BY escapesymb=STRING_LITERAL)? 
      )? 
      (
        LINES (STARTING BY startingsymb=STRING_LITERAL)?
         (TERMINATED BY terminatelinesymb=STRING_LITERAL)?
      )?
     )                                                #selectIntoOutfile
   ;

from_clause
    : FROM table_sources 
   (WHERE expression)? 
   (
     GROUP BY 
     group_by_item (',' group_by_item)* 
     (WITH ROLLUP)? 
   )?
   (HAVING expression)?
   ;

group_by_item
    : expression (ASC | DESC)?
   ;

limit_clause
    : LIMIT 
   (
     (decimal_literal ',')? decimal_literal
     | decimal_literal OFFSET decimal_literal
   )
   ;


// Transaction's Statements

start_transaction
    : START TRANSACTION (transact_option (',' transact_option)* )?
   ;

begin_work
    : BEGIN WORK?
   ;

commit_work
    : COMMIT WORK? (AND NO? CHAIN)? (NO? RELEASE)?
   ;

rollback_work
    : ROLLBACK WORK? (AND NO? CHAIN)? (NO? RELEASE)?
   ;

savepoint_statement
    : SAVEPOINT id_
   ;

rollback_statement
    : ROLLBACK WORK? TO SAVEPOINT? id_
   ;

release_statement
    : RELEASE SAVEPOINT id_
   ;

lock_tables
    : LOCK TABLES lock_table_element (',' lock_table_element)*
   ;

unlock_tables
    : UNLOCK TABLES
   ;


// details

set_autocommit_statement
    : SET AUTOCOMMIT '=' ('0' | '1')
   ;

set_transaction_statement
    : SET (GLOBAL | SESSION)? TRANSACTION 
   trans_characteristic (',' trans_characteristic)*
   ;

transact_option
    : WITH CONSISTENT SNAPSHOT
   | READ WRITE
   | READ ONLY
   ;

lock_table_element
    : table_name (AS? id_)? (READ LOCAL? | LOW_PRIORITY? WRITE)
   ;

trans_characteristic
    : ISOLATION LEVEL transaction_level
   | READ WRITE
   | READ ONLY
   ;

transaction_level
    : REPEATABLE READ
   | READ COMMITTED
   | READ UNCOMMITTED
   | SERIALIZABLE
   ;


// Replication's Statements

//    Base Replication

change_master
    : CHANGE MASTER TO 
   master_option (',' master_option)* channel_option?
   ;

change_repl_filter
    : CHANGE REPLICATION FILTER repl_filter (',' repl_filter)*
   ;

purge_binary_logs
    : PURGE (BINARY | MASTER) LOGS (TO | BEFORE) STRING_LITERAL
   ;

reset_master
    : RESET MASTER
   ;

reset_slave
    : RESET SLAVE ALL? channel_option?
   ;

start_slave
    : START SLAVE (thread_type (',' thread_type)*)? 
   UNTIL until_option?  
   start_slave_connection_option* channel_option?
   ;

stop_slave
    : STOP SLAVE (thread_type (',' thread_type)*)?
   ;

start_group_repl
    : START GROUP_REPLICATION
   ;

stop_group_repl
    : START GROUP_REPLICATION
   ;

// details

master_option
    : string_master_option '=' STRING_LITERAL                  #masterOptString
   | decimal_master_option '=' decimal_literal                 #masterOptDecimal
   | bool_master_option '=' ('0' | '1')                     #masterOptBool
   | MASTER_HEARTBEAT_PERIOD '=' REAL_LITERAL                  #masterOptReal
   | IGNORE_SERVER_IDS '=' '(' (id_ (',' id_)*)? ')'           #masterOptIdList
   ;

string_master_option
    : MASTER_BIND | MASTER_HOST | MASTER_USER | MASTER_PASSWORD 
   | MASTER_LOG_FILE | RELAY_LOG_FILE | MASTER_SSL_CA 
   | MASTER_SSL_CAPATH | MASTER_SSL_CERT | MASTER_SSL_CRL 
   | MASTER_SSL_CRLPATH | MASTER_SSL_KEY | MASTER_SSL_CIPHER 
   | MASTER_TLS_VERSION
   ;
decimal_master_option
    : MASTER_PORT | MASTER_CONNECT_RETRY | MASTER_RETRY_COUNT 
   | MASTER_DELAY | MASTER_LOG_POS | RELAY_LOG_POS
   ;

bool_master_option
    : MASTER_AUTO_POSITION | MASTER_SSL 
   | MASTER_SSL_VERIFY_SERVER_CERT
   ;

channel_option
    : FOR CHANNEL STRING_LITERAL
   ;

repl_filter
    : REPLICATE_DO_DB '=' '(' id_list ')'                   #replfilterDbList
   | REPLICATE_IGNORE_DB '=' '(' id_list ')'                #replfilterDbList
   | REPLICATE_DO_TABLE '=' '(' table_list ')'                 #replfilterTableList
   | REPLICATE_IGNORE_TABLE '=' '(' table_list ')'             #replfilterTableList
   | REPLICATE_WILD_DO_TABLE '=' '(' simple_string_list ')'    #replfilterStableList
   | REPLICATE_WILD_IGNORE_TABLE 
      '=' '(' simple_string_list ')'                        #replfilterStableList
   | REPLICATE_REWRITE_DB '=' '(' table_pair_list ')'          #replfilterTablepairList
   ;

thread_type
    : IO_THREAD | SQL_THREAD
   ;

until_option
    : (SQL_BEFORE_GTIDS | SQL_AFTER_GTIDS) '=' gtid_set           #untilGtidSset
   | MASTER_LOG_FILE '=' STRING_LITERAL
     ',' MASTER_LOG_POS '=' decimal_literal                 #untilMasterLog
   | RELAY_LOG_FILE '=' STRING_LITERAL
     ',' RELAY_LOG_POS '=' decimal_literal                     #untilRelayLog
   | SQL_AFTER_MTS_GAPS                               #untilSqlGaps
   ;

start_slave_connection_option
    : USER '=' con_opt_user=STRING_LITERAL
   | PASSWORD '=' con_opt_password=STRING_LITERAL
   | DEFAULT_AUTH '=' con_opt_def_auth=STRING_LITERAL
   | PLUGIN_DIR '=' con_opt_plugin_dir=STRING_LITERAL
   ;

gtid_set
    : uuid_set (',' uuid_set)*
   | STRING_LITERAL
   ;


//    XA Transactions

xa_start_transaction
    : XA (START | BEGIN) xid (JOIN | RESUME)?
   ;

xa_end_transaction
    : XA END xid (SUSPEND (FOR MIGRATE)?)?
   ;

xa_prepare
    : XA PREPARE xid
   ;

xa_commit_work
    : XA COMMIT xid (ONE PHASE)?
   ;

xa_rollback_work
    : XA ROLLBACK xid
   ;

xa_recover_work
    : XA RECOVER (CONVERT xid)?
   ;


// Prepared Statements

prepare_statement
    : PREPARE id_ FROM (STRING_LITERAL | LOCAL_ID)
   ;

execute_statement
    : EXECUTE id_ (USING user_var_list)?
   ;

deallocate_prepare
    : (DEALLOCATE | DROP) PREPARE id_
   ;


// Compound Statements

routine_body
    : block_statement | sql_statement
   ;

// details

block_statement
    : (id_ ':')? BEGIN
   (
      (declare_variable SEMI)*
      (declare_condition SEMI)*
      (declare_cursor SEMI)*
      (declare_handler SEMI)*
      procedure_sql_statement+
   )?
   END id_?
   ;

case_statement
    : CASE (id_ | expression)?
   (
     WHEN (constant | expression) 
     THEN procedure_sql_statement+
   )+
   (ELSE procedure_sql_statement+)?
   END CASE
   ;

if_statement
    : IF expression THEN procedure_sql_statement+
   (ELSEIF expression THEN procedure_sql_statement+)*
   (ELSE procedure_sql_statement+ )?
   END IF
   ;

iterate_statement
    : ITERATE id_
   ;

leave_statement
    : LEAVE id_
   ;

loop_statement
    : (id_ ':')? 
   LOOP procedure_sql_statement+ 
   END LOOP id_?
   ;

repeat_statement
    : (id_ ':')? 
   REPEAT procedure_sql_statement+ 
   UNTIL expression 
   END REPEAT id_?
   ;

return_statement
    : RETURN expression
   ;

while_statement
    : (id_ ':')? 
   WHILE expression 
   DO procedure_sql_statement+ 
   END WHILE id_?
   ;

cursor_statement
    : CLOSE id_
   | FETCH (NEXT? FROM)? id_ INTO id_list
   | OPEN id_
   ;

// details

declare_variable
    : DECLARE id_list data_type (DEFAULT default_value)?
   ;

declare_condition
    : DECLARE id_ CONDITION FOR 
   (
     decimal_literal
     | SQLSTATE VALUE? STRING_LITERAL
   )
   ;

declare_cursor
    : DECLARE id_ CURSOR FOR select_statement
   ;

declare_handler
    : DECLARE (CONTINUE | EXIT | UNDO) HANDLER FOR 
   handler_condition_value (',' handler_condition_value)* 
   routine_body
   ;

handler_condition_value
    : decimal_literal
   | SQLSTATE VALUE? STRING_LITERAL
   | id_
   | SQLWARNING
   | NOT FOUND
   | SQLEXCEPTION
   ;

procedure_sql_statement
    : (compound_statement | sql_statement) SEMI
   ;

// Administration Statements

//    Account management statements

alter_user
    : ALTER USER 
     user_name user_password_option 
     ( ',' user_name user_password_option)*                 #alterUserMysql56
   | ALTER USER if_exists? 
       user_auth_option (',' user_auth_option)*
       (REQUIRE (NONE | tls_option (AND? tls_option)* ) )?
       (WITH user_resource_option+)?
       (user_password_option | user_lock_option)*              #alterUserMysql57
   ;

create_user
    : CREATE USER user_auth_option (',' user_auth_option)*        #createUserMysql56
   | CREATE USER if_not_exists? 
       user_auth_option (',' user_auth_option)*
       (REQUIRE (NONE | tls_option (AND? tls_option)* ) )?
       (WITH user_resource_option+)?
       (user_password_option | user_lock_option)*              #createUserMysql57
   ;

drop_user
    : DROP USER if_exists? user_name (',' user_name)*
   ;

grant_statement
    : GRANT privelege_clause (',' privelege_clause)*
   ON 
     priv_obj_type=(TABLE | FUNCTION | PROCEDURE)? 
     privilege_level
   TO user_auth_option (',' user_auth_option)*
   (REQUIRE (NONE | tls_option (AND? tls_option)* ) )?
   (WITH (GRANT OPTION | user_resource_option)* )?
   ;

grant_proxy
    : GRANT PROXY ON user_name
   TO user_name (',' user_name)*
   (WITH GRANT OPTION)?
   ;

rename_user
    : RENAME USER 
   user_name TO user_name 
   (',' user_name TO user_name)
   ;

revoke_statement
    : REVOKE privelege_clause (',' privelege_clause)*
   ON 
     priv_obj_type=(TABLE | FUNCTION | PROCEDURE)? 
     privilege_level
   FROM user_name (',' user_name)*                          #detailRevoke
   | REVOKE ALL PRIVILEGES? ',' GRANT OPTION
       FROM user_name (',' user_name)*                      #shortRevoke
   ;

revoke_proxy
    : REVOKE PROXY ON user_name FROM user_name (',' user_name)*
   ;

// details

set_password_statement
    : SET PASSWORD (FOR user_name)? '=' set_password_option
   ;

user_password_option
    : PASSWORD EXPIRE
   (DEFAULT | NEVER | INTERVAL decimal_literal DAY)?
   ;

user_auth_option
    : user_name IDENTIFIED BY PASSWORD hashedpwd=STRING_LITERAL      #authByPassword
   |  user_name
       IDENTIFIED (WITH auth_plugin)? BY STRING_LITERAL        #authByString
   | user_name 
       IDENTIFIED WITH auth_plugin 
       (AS STRING_LITERAL)?                              #authByHash
   ;

tls_option
    : SSL
   | X509
   | CIPHER STRING_LITERAL
   | ISSUER STRING_LITERAL
   | SUBJECT STRING_LITERAL
   ;

user_resource_option
    : MAX_QUERIES_PER_HOUR decimal_literal
   | MAX_UPDATES_PER_HOUR decimal_literal
   | MAX_CONNECTIONS_PER_HOUR decimal_literal
   | MAX_USER_CONNECTIONS decimal_literal
   ;

user_lock_option
    : ACCOUNT (LOCK | UNLOCK)
   ;

privelege_clause
    : privilege ( '(' id_list ')' )?
   ;

privilege
    : ALL PRIVILEGES?
   | ALTER ROUTINE?
   | CREATE 
      (TEMPORARY TABLES | ROUTINE | VIEW | USER | TABLESPACE)?
   | DELETE | DROP | EVENT | EXECUTE | FILE | GRANT OPTION
   | INDEX | INSERT | LOCK TABLES | PROCESS | PROXY
   | REFERENCES | RELOAD 
   | REPLICATION (CLIENT | SLAVE)
   | SELECT
   | SHOW (VIEW | DATABASES)
   | SHUTDOWN | SUPER | TRIGGER | UPDATE | USAGE
   ;

privilege_level
    : '*'
   | '*' '.' '*'
   | id_ '.' '*'
   | id_ '.' id_
   | id_
   ;

set_password_option
    : (PASSWORD | OLD_PASSWORD) '(' STRING_LITERAL ')'
   | STRING_LITERAL
   ;


//    Table maintenance statements

analyze_table
    : ANALYZE (NO_WRITE_TO_BINLOG | LOCAL)? TABLE table_list
   ;

check_table
    : CHECK TABLE table_list check_table_option*
   ;

checksum_table
    : CHECKSUM TABLE table_list (QUICK | EXTENDED)?
   ;
   
optimize_table
    : OPTIMIZE (NO_WRITE_TO_BINLOG | LOCAL)? TABLE table_list
   ;

repair_table
    : REPAIR (NO_WRITE_TO_BINLOG | LOCAL)? TABLE table_list
   QUICK? EXTENDED? USE_FRM?
   ;

// details

check_table_option
    : FOR UPGRADE | QUICK | FAST | MEDIUM | EXTENDED | CHANGED
   ;


//    Plugin and udf statements

create_udfunction
    : CREATE AGGREGATE? FUNCTION id_ 
   RETURNS (STRING | INTEGER | REAL | DECIMAL)
   SONAME STRING_LITERAL
   ;
   
install_plugin
    : INSTALL PLUGIN id_ SONAME STRING_LITERAL
   ;

 uninstall_plugin
    : UNINSTALL PLUGIN id_
   ;


//    Set and show statements

set_statement
    : SET variable_clause '=' expression 
     (',' variable_clause '=' expression)*                     #setVariableAssignment
   | SET (CHARACTER SET | CHARSET) (charset_name | DEFAULT)    #setCharset
   | SET NAMES 
       (charset_name (COLLATE collation_name)? | DEFAULT)         #setNames
   | set_password_statement                              #setPasswordStatement
   | set_transaction_statement                              #setTransaction
   | set_autocommit_statement                            #setAutocommit
   ;

show_statement
    : SHOW (BINARY | MASTER) LOGS                           #showMasterlogs
   | SHOW (BINLOG | RELAYLOG) EVENTS (IN STRING_LITERAL)?
       (FROM from_pos=decimal_literal)?
       (LIMIT 
         (offset=decimal_literal ',')? 
         row_count=decimal_literal
       )?                                             #showLogevents
   | SHOW 
       (
         CHARACTER SET | COLLATION | DATABASES | SCHEMAS
         | FUNCTION STATUS | PROCEDURE STATUS
         | (GLOBAL | SESSION)? (STATUS | VARIABLES)
       ) 
       show_filter?                                   #showObjWithFilter
   | SHOW FULL? (COLUMNS | FIELDS) (FROM | IN) table_name
       ((FROM | IN) id_)? show_filter?                      #showColumns
   | SHOW CREATE (DATABASE | SCHEMA) if_not_exists? id_        #showCreateDb
   | SHOW CREATE 
       (EVENT | FUNCTION | PROCEDURE | TABLE | TRIGGER | VIEW) 
       full_id                                        #showCreateFullidobj
   | SHOW CREATE USER user_name                          #showCreateUser
   | SHOW ENGINE engine_name (STATUS | MUTEX)                  #showEngine
   | SHOW 
       (
         STORAGE? ENGINES | MASTER STATUS | PLUGINS 
         | PRIVILEGES | FULL? PROCESSLIST | PROFILES
         | SLAVE HOSTS | AUTHORS | CONTRIBUTORS
       )                                           #showGlobalinfo
   | SHOW (ERRORS | WARNINGS)
       (LIMIT 
         (offset=decimal_literal ',')? 
         row_count=decimal_literal
       )                                           #showErrWarn
   | SHOW COUNT '(' '*' ')' (ERRORS | WARNINGS)             #showCountErrWarn
   | SHOW (EVENTS | TABLE STATUS | FULL? TABLES | TRIGGERS)
       ((FROM | IN) id_)? show_filter?                      #showFromschemaFilter
   | SHOW (FUNCTION | PROCEDURE) CODE full_id                  #showRoutinecode
   | SHOW GRANTS (FOR user_name)?                           #showGrants
   | SHOW (INDEX | INDEXES | KEYS) (FROM | IN) table_name
       ((FROM | IN) id_)? (WHERE expression)?                  #showIndexes
   | SHOW OPEN TABLES ( (FROM | IN) id_)? show_filter?            #showOpentables
   | SHOW PROFILE show_profile_type (',' show_profile_type)*
       (FOR QUERY decimal_literal)?
       (LIMIT 
         (offset=decimal_literal ',')? 
         row_count=decimal_literal
       )                                           #showProfile
   | SHOW SLAVE STATUS (FOR CHANNEL STRING_LITERAL)?           #showSlavestatus
   ;

// details

variable_clause
    : LOCAL_ID | GLOBAL_ID | ( ('@' '@')? (GLOBAL | SESSION)  )? id_
   ;

show_filter
    : LIKE STRING_LITERAL
   | WHERE expression
   ;

show_profile_type
    : ALL | BLOCK IO | CONTEXT SWITCHES | CPU | IPC | MEMORY 
   | PAGE FAULTS | SOURCE | SWAPS
   ;


//    Other administrative statements

binlog_statement
    : BINLOG STRING_LITERAL
   ;

cache_index_statement
    : CACHE INDEX tbl_index_list (',' tbl_index_list)*
   ( PARTITION '(' (id_list | ALL) ')' )?
   IN id_
   ;

flush_statement
    : FLUSH (NO_WRITE_TO_BINLOG | LOCAL)?
   flush_option (',' flush_option)*
   ;

kill_statement
    : KILL (CONNECTION | QUERY)? decimal_literal+
   ;

load_index_into_cache
    : LOAD INDEX INTO CACHE 
   load_tbl_index_list (',' load_tbl_index_list)*
   ;

// remark reser (maser | slave) describe in replication's
//  statements section
reset_statement
    : RESET QUERY CACHE
   ;

shutdown_statement
    : SHUTDOWN
   ;

// details

tbl_index_list
    : table_name ( (INDEX | KEY)? '(' id_list ')' )?
   ;

flush_option
    : DES_KEY_FILE | HOSTS
   | (BINARY | ENGINE | ERROR | GENERAL | RELAY | SLOW)? LOGS
   | RELAY LOGS channel_option?
   | OPTIMIZER_COSTS | PRIVILEGES | QUERY CACHE | STATUS 
   | USER_RESOURCES
   | TABLES (WITH READ LOCK)?
   | TABLES table_list (WITH READ LOCK | FOR EXPORT)?
   ;

load_tbl_index_list
    : table_name 
   ( PARTITION '(' (partition_list=id_list | ALL) ')' )?
   ( (INDEX | KEY)? '(' index_list=id_list ')' )?
   (IGNORE LEAVES)?
   ;


// Utility Statements


simple_describe_statement
    : (EXPLAIN | DESCRIBE | DESC) table_name
   (colname=id_ | col_wildcard=STRING_LITERAL)?
   ;

full_describe_statement
    : (EXPLAIN | DESCRIBE | DESC)
   (EXTENDED | PARTITIONS | FORMAT '=' (TRADITIONAL | JSON))?
   describe_object_clause
   ;

help_statement
    : HELP STRING_LITERAL
   ;

use_statement
    : USE id_
   ;

// details

describe_object_clause
    : (
     select_statement | delete_statement | insert_statement 
     | replace_statement | update_statement
   )                                               #descstmtDescObj
   | FOR CONNECTION id_                               #connectionDescObj
   ;


// Common Clauses

//    DB Objects

table_name
    : id_ (DOT_ID | '.' id_)?
   ;

full_id
    : id_ (DOT_ID | '.' id_)?
   ;

full_column_name
    : id_ (dot_ext_id dot_ext_id? )?
   ;

index_col_name
    : id_ ('(' decimal_literal ')')? (ASC | DESC)?
   ;

user_name
    : STRING_USER_NAME;

mysql_variable
    : LOCAL_ID
   | GLOBAL_ID
   ;

charset_name
    : BINARY
   | charset_name_base
   | STRING_LITERAL
   | CHARSET_REVERSE_QOUTE_STRING
   ;

collation_name
    : id_ | STRING_LITERAL;

engine_name
    : ARCHIVE | BLACKHOLE | CSV | FEDERATED | INNODB | MEMORY 
   | MRG_MYISAM | MYISAM | NDB | NDBCLUSTER | PERFOMANCE_SCHEMA
   ;

uuid_set
    : decimal_literal '-' decimal_literal '-' decimal_literal
   '-' decimal_literal '-' decimal_literal 
   (':' decimal_literal '-' decimal_literal)+
   ;

xid
    : xid_gtrid=xid_string_id 
   (
     ',' xid_bqual=xid_string_id 
     (',' xid_formatID=decimal_literal)?
   )?
   ;

xid_string_id
    : STRING_LITERAL
   | BIT_STRING
   | HEXADECIMAL_LITERAL+
   ;

auth_plugin
    : id_ | STRING_LITERAL
   ;

id_
    : simple_id
   //| DOUBLE_QUOTE_ID
   | REVERSE_QUOTE_ID
   | CHARSET_REVERSE_QOUTE_STRING
   ;
   
simple_id
    : ID
   | charset_name_base
   | transaction_level_base
   | engine_name
   | privileges_base
   | interval_type_base
   | data_type_base
   | keywords_can_be_id
   | function_name_base
   | spatial_data_type
   ;

dot_ext_id
    : DOT_ID
   | '.' id_
   ;


//    Literals

decimal_literal
    : DECIMAL_LITERAL | ZERO_DECIMAL | ONE_DECIMAL | TWO_DECIMAL
   ;

filesize_literal
    : FILESIZE_LITERAL | decimal_literal;

string_literal
    : (
     STRING_CHARSET_NAME? STRING_LITERAL 
     | START_NATIONAL_STRING_LITERAL
   ) STRING_LITERAL+
   | (
       STRING_CHARSET_NAME? STRING_LITERAL 
       | START_NATIONAL_STRING_LITERAL
     ) (COLLATE collation_name)?
   ;

boolean_literal
    : TRUE | FALSE;

hexadecimal_literal
    : STRING_CHARSET_NAME? HEXADECIMAL_LITERAL;

null_notnull
    : NOTNULL
   | (NULL_LITERAL | NULL_SPEC_LITERAL)
   ;

constant
    : string_literal | decimal_literal
   | hexadecimal_literal | boolean_literal
   | REAL_LITERAL | BIT_STRING | NOTNULL
   | (NULL_LITERAL | NULL_SPEC_LITERAL)
   ;


//    Data Types

data_type
    : (CHAR | VARCHAR | TINYTEXT | TEXT | MEDIUMTEXT | LONGTEXT) 
     length_one_dimension? BINARY? 
     (CHARACTER SET charset_name)? (COLLATE collation_name)?      #charDatatype
   | (TINYINT | SMALLINT | MEDIUMINT | INT | INTEGER | BIGINT) 
     length_one_dimension? UNSIGNED? ZEROFILL?                 #dimensionDatatype
   | (REAL | DOUBLE | FLOAT) 
     length_two_dimension? UNSIGNED? ZEROFILL?                 #dimensionDatatype
   | (DECIMAL | NUMERIC) 
     length_two_optional_dimension? UNSIGNED? ZEROFILL?        #dimensionDatatype
   | (DATE | YEAR | TINYBLOB | BLOB | MEDIUMBLOB | LONGBLOB)      #simpleDatatype
   | (BIT | TIME | TIMESTAMP | DATETIME | BINARY | VARBINARY) 
     length_one_dimension?                               #dimensionDatatype
   | (ENUM | SET) 
     '(' STRING_LITERAL (',' STRING_LITERAL)* ')' BINARY? 
     (CHARACTER SET charset_name)? (COLLATE collation_name)?      #collectCharDatatype
   | spatial_data_type                                   #spatialDatatype
   ;

data_type_to_convert
    : (BINARY| NCHAR) length_one_dimension?
   | CHAR length_one_dimension? (CHARACTER SET charset_name)?
   | DATE | DATETIME | TIME
   | DECIMAL length_two_dimension?
   | (SIGNED | UNSIGNED) INTEGER?
   ;

spatial_data_type
    : GEOMETRYCOLLECTION | LINESTRING | MULTILINESTRING 
   | MULTIPOINT | MULTIPOLYGON | POINT | POLYGON
   ;

length_one_dimension
    : '(' decimal_literal ')'
   ;

length_two_dimension
    : '(' decimal_literal ',' decimal_literal ')'
   ;

length_two_optional_dimension
    : '(' decimal_literal (',' decimal_literal)? ')'
   ;


//    Common Lists

id_list
    : id_ (',' id_)*
   ;

table_list
    : table_name (',' table_name)*
   ;

table_pair_list
    : '(' table_name ',' table_name ')' 
   (',' '(' table_name ',' table_name ')')*
   ;

index_colname_list
    : '(' index_col_name (',' index_col_name)* ')'
   ;

expression_list
    : expression (',' expression)*
   ;

constant_list
    : constant (',' constant)*
   ;

simple_string_list
    : STRING_LITERAL (',' STRING_LITERAL)*
   ;

user_var_list
    : LOCAL_ID (',' LOCAL_ID)*
   ;


//    Common Expressons

default_value
    : NULL_LITERAL
   | constant
   ;

if_exists
    : IF EXISTS;

if_not_exists
    : IF NOT EXISTS;


//    Functions

function_call
    : specific_function_call                             #specificFunctionCall
   | aggregate_windowed_function                         #aggregateFunctionCall
   | scalar_function_name '(' function_args? ')'               #scalarFunctionCall
   | id_ dot_ext_id? '(' function_args? ')'                 #udfFunctionCall
   ;

specific_function_call
    : (
     CURRENT_DATE | CURRENT_TIME | CURRENT_TIMESTAMP 
     | CURRENT_USER | LOCALTIME
   )                                               #simpleSpecificFCall
   | CONVERT '(' expression ',' data_type_to_convert ')'       #convertDataTypeFCall
   | CONVERT '(' expression USING charset_name ')'             #convertDataTypeFCall
   | CAST '(' expression AS data_type_to_convert ')'           #convertDataTypeFCall
   | VALUES '(' full_column_name ')'                        #valuesFCall
   | CASE expression 
     (WHEN condarg+=function_arg THEN resarg+=function_arg)+ 
     (ELSE function_arg)? END                            #caseFCall
   | CASE 
     (WHEN condarg+=function_arg THEN resarg+=function_arg )+ 
     (ELSE function_arg)? END                            #caseFCall
   | CHAR '(' function_args  (USING charset_name)? ')'            #charFCall
   | POSITION '(' (fstr=string_literal | fexpr=expression) 
     IN (sstr=string_literal | sexpr=expression) ')'           #positionFCall
   | (SUBSTR | SUBSTRING) '(' 
       (string_literal | fexpr=expression) FROM 
       (fdecimal=decimal_literal | sexpr=expression)
       (FOR (sdecimal=decimal_literal | texpr=expression) )? 
     ')'                                           #substrFCall
   | TRIM '(' 
       (BOTH | LEADING | TRAILING) 
       (fstr=string_literal | fexpr=expression)? 
       FROM (sstr=string_literal | sexpr=expression) 
     ')'                                           #trimFCall
   | TRIM '(' 
       (fstr=string_literal | fexpr=expression) 
       FROM (sstr=string_literal | sexpr=expression) 
     ')'                                           #trimFCall
   | WEIGHT_STRING '(' 
       (string_literal | expression) (AS (CHAR | BINARY) 
       '(' decimal_literal ')' )?  levels_in_weight_string?  
     ')'                                           #weightFCall
   | EXTRACT '(' 
       interval_type 
       FROM (fstr=string_literal | fexpr=expression) 
     ')'                                           #extractFCall
   | GET_FORMAT '(' (DATE|TIME|DATETIME) ',' string_literal ')'   #getFormatFCall
   ;

levels_in_weight_string
    : LEVEL 
     firstlevel=decimal_literal 
     firstord=(ASC | DESC | REVERSE)? 
     (
       ',' nextlevel+=decimal_literal 
       nextord+=(ASC | DESC | REVERSE)?
     )*                                            #levelWeightFList
   | LEVEL 
     firstlevel=decimal_literal '-' lastlevel=decimal_literal     #levelWeightFRange
   ;

aggregate_windowed_function
    : (AVG | MAX | MIN | SUM) 
     '(' (ALL | DISTINCT)? function_arg ')'
   | COUNT '(' ('*' | ALL? function_arg) ')'
   | COUNT '(' DISTINCT function_args ')'
   | (
       BIT_AND | BIT_OR | BIT_XOR | STD | STDDEV | STDDEV_POP 
       | STDDEV_SAMP | VAR_POP | VAR_SAMP | VARIANCE
     ) '(' ALL? function_arg ')'
   | GROUP_CONCAT '(' 
       DISTINCT? function_args 
       (ORDER BY 
         order_by_expression (',' order_by_expression)* 
       )? (SEPARATOR STRING_LITERAL)? 
     ')'
   ;

scalar_function_name
    : function_name_base
   | ASCII | CURDATE | CURRENT_DATE | CURRENT_TIME 
   | CURRENT_TIMESTAMP | CURTIME | DATE_ADD | DATE_SUB 
   | IF | LOCALTIME | LOCALTIMESTAMP | MID | NOW | REPLACE 
   | SUBSTR | SUBSTRING | SYSDATE | TRIM 
   | UTC_DATE | UTC_TIME | UTC_TIMESTAMP
   ;

function_args
    : (constant | full_column_name | function_call | expression) 
   (
     ',' 
     (constant | full_column_name | function_call | expression)
   )*
   ;

function_arg
    : constant | full_column_name | function_call | expression
   ;


//    Expressions, predicates

// Simplified approach for expression
expression
    : (NOT | '!') expression                             #notExpression
   | expression logical_operator expression                 #logicalExpression
   | predicate IS NOT? (TRUE | FALSE | UNKNOWN)             #isExpression
   | predicate                                        #predicateExpression
   ;

predicate
    : predicate NOT? IN '(' (subquery | expression_list) ')'      #inPredicate
   | predicate IS null_notnull                              #isNullPredicate
   | predicate comparison_operator predicate                #binaryComparasionPredicate
   | predicate 
     comparison_operator (ALL | ANY | SOME) '(' subquery ')'      #subqueryComparasionPredicate
   | predicate NOT? BETWEEN predicate AND predicate            #betweenPredicate
   | predicate SOUNDS LIKE predicate                        #soundsLikePredicate
   | predicate NOT? LIKE predicate (ESCAPE string_literal)?    #likePredicate
   | predicate NOT? (REGEXP|RLIKE) predicate                #regexpPredicate
   | (LOCAL_ID VAR_ASSIGN)? expression_atom                 #expressionAtomPredicate
   ;


// Add in ASTVisitor null_notnull in constant
expression_atom
    : DEFAULT                                         #defaultExpressionAtom
   | constant                                         #constantExpressionAtom
   | full_column_name                                    #fullColumnNameExpressionAtom
   | function_call                                    #functionCallExpressionAtom
   | mysql_variable                                   #mysqlVariableExpressionAtom
   | unary_operator expression_atom                      #unaryExpressionAtom
   | BINARY expression_atom                              #binaryExpressionAtom
   | '(' expression ')'                                  #nestedExpressionAtom
   | EXISTS? '(' subquery ')'                            #existsExpessionAtom
   | INTERVAL expression interval_type                      #intervalExpressionAtom
   | expression_atom bit_operator expression_atom              #bitExpressionAtom
   | expression_atom math_operator expression_atom             #mathExpressionAtom
   ;

unary_operator
    : '!' | '~' | '+' | '-' | NOT
   ;

comparison_operator
    : '=' | '>' | '<' | '<' '=' | '>' '=' 
   | '<' '>' | '!' '=' | '<' '=' '>'
   ;

logical_operator
    : AND | '&' '&' | XOR | OR | '|' '|'
   ;

bit_operator
    : '<' '<' | '>' '>' | '&' | '^' | '|'
   ;

math_operator
    : '*' | '/' | '%' | DIV | MOD | '+' | '-'
   ;


//    Simple id sets
//     (that keyword, which can be id)

charset_name_base
    : ARMSCII8 | ASCII | BIG5 | CP1250 | CP1251 | CP1256 | CP1257
   | CP850 | CP852 | CP866 | CP932 | DEC8 | EUCJPMS | EUCKR 
   | GB2312 | GBK | GEOSTD8 | GREEK | HEBREW | HP8 | KEYBCS2 
   | KOI8R | KOI8U | LATIN1 | LATIN2 | LATIN5 | LATIN7 | MACCE
   | MACROMAN | SJIS | SWE7 | TIS620 | UCS2 | UJIS | UTF16 
   | UTF16LE | UTF32 | UTF8 | UTF8MB3 | UTF8MB4
   ;

transaction_level_base
    : REPEATABLE | COMMITTED | UNCOMMITTED | SERIALIZABLE
   ;

privileges_base
    : TABLES | ROUTINE | EXECUTE | FILE | PROCESS 
   | RELOAD | SHUTDOWN | SUPER | PRIVILEGES
   ;

interval_type_base
    : QUARTER | MONTH | DAY | HOUR 
   | MINUTE | WEEK | SECOND | MICROSECOND
   ;

data_type_base
    : DATE | TIME | TIMESTAMP | DATETIME | YEAR | ENUM | TEXT
   ;
 
keywords_can_be_id
    : ACTION | AFTER | ALGORITHM | ANY | AT | AUTHORS | AUTOCOMMIT
   | AUTOEXTEND_SIZE | AUTO_INCREMENT | AVG_ROW_LENGTH | BEGIN 
   | BINLOG | BIT | BTREE | CASCADED | CHAIN | CHECKSUM 
   | CIPHER | CLIENT | COALESCE | CODE | COLUMNS 
   | COLUMN_FORMAT | COMMENT | COMMIT | COMPACT | COMPLETION 
   | COMPRESSED | CONCURRENT | CONNECTION | CONSISTENT 
   | CONTAINS | CONTRIBUTORS | COPY | DATA | DATAFILE 
   | DEFINER | DELAY_KEY_WRITE | DIRECTORY | DISABLE | DISCARD 
   | DISK | DO | DUMPFILE| DUPLICATE | DYNAMIC | ENABLE | ENDS 
   | ENGINE | ENGINES | ERRORS | ESCAPE | EVEN | EVENT | EVENTS 
   | EVERY | EXCHANGE | EXCLUSIVE | EXPIRE | EXTENT_SIZE 
   | FIELDS | FIRST | FIXED | FULL | FUNCTION | GLOBAL | GRANTS 
   | HASH | HOST | IDENTIFIED | IMPORT | INITIAL_SIZE | INPLACE 
   | INSERT_METHOD | INVOKER | ISOLATION | ISSUER 
   | KEY_BLOCK_SIZE | LANGUAGE | LAST | LESS | LEVEL | LIST
   | LOCAL | LOGS | LOGFILE | MASTER | MAX_CONNECTIONS_PER_HOUR 
   | MAX_QUERIES_PER_HOUR | MAX_ROWS | MAX_SIZE 
   | MAX_UPDATES_PER_HOUR | MAX_USER_CONNECTIONS | MEMORY 
   | MERGE | MID | MIN_ROWS | MUTEX | SHARE | MODIFY | MYSQL 
   | NAME | NAMES | NCHAR | NO | NODEGROUP | NONE | OFFLINE 
   | OFFSET | OJ | OLD_PASSWORD | ONLINE | ONLY | OPTIONS 
   | OWNER | PACK_KEYS | PARSER | PARTIAL | PARTITIONING 
   | PARTITIONS | PASSWORD | PLUGINS | PORT | PRESERVE 
   | PROCESSLIST | PROFILE | PROFILES | PROXY | QUERY | QUICK 
   | REBUILD | REDO_BUFFER_SIZE | REDUNDANT | RELAYLOG | REMOVE 
   | REORGANIZE | REPAIR | REPLICATION | RETURNS | ROLLBACK 
   | ROLLUP | ROW | ROWS | ROW_FORMAT | SAVEPOINT | SCHEDULE 
   | SECURITY | SERVER | SESSION | SHARE | SHARED | SIGNED 
   | SIMPLE | SLAVE | SNAPSHOT | SOCKET | SOME | SOUNDS 
   | SQL_BUFFER_RESULT | SQL_CACHE | SQL_NO_CACHE | START 
   | STARTS | STATS_AUTO_RECALC | STATS_PERSISTENT 
   | STATS_SAMPLE_PAGES | STATUS | STORAGE | SUBJECT 
   | SUBPARTITION | SUBPARTITIONS | TABLESPACE | TEMPORARY 
   | TEMPTABLE | THAN | TRANSACTION | TRUNCATE | UNDEFINED 
   | UNDOFILE | UNDO_BUFFER_SIZE | UNKNOWN | UPGRADE | USER 
   | VALUE | VARIABLES | VIEW | WAIT | WARNINGS | WORK 
   | WRAPPER | X509 | XML
   ;

function_name_base
    : ABS | ACOS | ADDDATE | ADDTIME | AES_DECRYPT | AES_ENCRYPT 
   | AREA | ASBINARY | ASIN | ASTEXT | ASWKB | ASWKT 
   | ASYMMETRIC_DECRYPT | ASYMMETRIC_DERIVE 
   | ASYMMETRIC_ENCRYPT | ASYMMETRIC_SIGN | ASYMMETRIC_VERIFY 
   | ATAN | ATAN2 | BENCHMARK | BIN | BIT_COUNT | BIT_LENGTH 
   | BUFFER | CEIL | CEILING | CENTROID | CHARACTER_LENGTH 
   | CHARSET | CHAR_LENGTH | COERCIBILITY | COLLATION 
   | COMPRESS | CONCAT | CONCAT_WS | CONNECTION_ID | CONV 
   | CONVERT_TZ | COS | COT | COUNT | CRC32 
   | CREATE_ASYMMETRIC_PRIV_KEY | CREATE_ASYMMETRIC_PUB_KEY 
   | CREATE_DH_PARAMETERS | CREATE_DIGEST | CROSSES | DATE 
   | DATEDIFF | DATE_FORMAT | DAY | DAYNAME | DAYOFMONTH 
   | DAYOFWEEK | DAYOFYEAR | DECODE | DEGREES | DES_DECRYPT 
   | DES_ENCRYPT | DIMENSION | DISJOINT | ELT | ENCODE 
   | ENCRYPT | ENDPOINT | ENVELOPE | EQUALS | EXP | EXPORT_SET 
   | EXTERIORRING | EXTRACTVALUE | FIELD | FIND_IN_SET | FLOOR 
   | FORMAT | FOUND_ROWS | FROM_BASE64 | FROM_DAYS 
   | FROM_UNIXTIME | GEOMCOLLFROMTEXT | GEOMCOLLFROMWKB 
   | GEOMETRYCOLLECTION | GEOMETRYCOLLECTIONFROMTEXT 
   | GEOMETRYCOLLECTIONFROMWKB | GEOMETRYFROMTEXT 
   | GEOMETRYFROMWKB | GEOMETRYN | GEOMETRYTYPE | GEOMFROMTEXT 
   | GEOMFROMWKB | GET_FORMAT | GET_LOCK | GLENGTH | GREATEST 
   | GTID_SUBSET | GTID_SUBTRACT | HEX | HOUR | IFNULL 
   | INET6_ATON | INET6_NTOA | INET_ATON | INET_NTOA | INSTR 
   | INTERIORRINGN | INTERSECTS | ISCLOSED | ISEMPTY | ISNULL 
   | ISSIMPLE | IS_FREE_LOCK | IS_IPV4 | IS_IPV4_COMPAT 
   | IS_IPV4_MAPPED | IS_IPV6 | IS_USED_LOCK | LAST_INSERT_ID 
   | LCASE | LEAST | LEFT | LENGTH | LINEFROMTEXT | LINEFROMWKB
   | LINESTRING | LINESTRINGFROMTEXT | LINESTRINGFROMWKB | LN 
   | LOAD_FILE | LOCATE | LOG | LOG10 | LOG2 | LOWER | LPAD 
   | LTRIM | MAKEDATE | MAKETIME | MAKE_SET | MASTER_POS_WAIT 
   | MBRCONTAINS | MBRDISJOINT | MBREQUAL | MBRINTERSECTS 
   | MBROVERLAPS | MBRTOUCHES | MBRWITHIN | MD5 | MICROSECOND 
   | MINUTE | MLINEFROMTEXT | MLINEFROMWKB | MONTH | MONTHNAME 
   | MPOINTFROMTEXT | MPOINTFROMWKB | MPOLYFROMTEXT 
   | MPOLYFROMWKB | MULTILINESTRING | MULTILINESTRINGFROMTEXT 
   | MULTILINESTRINGFROMWKB | MULTIPOINT | MULTIPOINTFROMTEXT 
   | MULTIPOINTFROMWKB | MULTIPOLYGON | MULTIPOLYGONFROMTEXT 
   | MULTIPOLYGONFROMWKB | NAME_CONST | NULLIF | NUMGEOMETRIES 
   | NUMINTERIORRINGS | NUMPOINTS | OCT | OCTET_LENGTH | ORD 
   | OVERLAPS | PERIOD_ADD | PERIOD_DIFF | PI | POINT 
   | POINTFROMTEXT | POINTFROMWKB | POINTN | POLYFROMTEXT 
   | POLYFROMWKB | POLYGON | POLYGONFROMTEXT | POLYGONFROMWKB 
   | POSITION| POW | POWER | QUARTER | QUOTE | RADIANS | RAND 
   | RANDOM_BYTES | RELEASE_LOCK | REVERSE | RIGHT | ROUND 
   | ROW_COUNT | RPAD | RTRIM | SECOND | SEC_TO_TIME 
   | SESSION_USER | SHA | SHA1 | SHA2 | SIGN | SIN | SLEEP 
   | SOUNDEX | SQL_THREAD_WAIT_AFTER_GTIDS | SQRT | SRID 
   | STARTPOINT | STRCMP | STR_TO_DATE | ST_AREA | ST_ASBINARY 
   | ST_ASTEXT | ST_ASWKB | ST_ASWKT | ST_BUFFER | ST_CENTROID 
   | ST_CONTAINS | ST_CROSSES | ST_DIFFERENCE | ST_DIMENSION 
   | ST_DISJOINT | ST_DISTANCE | ST_ENDPOINT | ST_ENVELOPE 
   | ST_EQUALS | ST_EXTERIORRING | ST_GEOMCOLLFROMTEXT 
   | ST_GEOMCOLLFROMTXT | ST_GEOMCOLLFROMWKB 
   | ST_GEOMETRYCOLLECTIONFROMTEXT 
   | ST_GEOMETRYCOLLECTIONFROMWKB | ST_GEOMETRYFROMTEXT 
   | ST_GEOMETRYFROMWKB | ST_GEOMETRYN | ST_GEOMETRYTYPE 
   | ST_GEOMFROMTEXT | ST_GEOMFROMWKB | ST_INTERIORRINGN 
   | ST_INTERSECTION | ST_INTERSECTS | ST_ISCLOSED | ST_ISEMPTY
   | ST_ISSIMPLE | ST_LINEFROMTEXT | ST_LINEFROMWKB 
   | ST_LINESTRINGFROMTEXT | ST_LINESTRINGFROMWKB 
   | ST_NUMGEOMETRIES | ST_NUMINTERIORRING 
   | ST_NUMINTERIORRINGS | ST_NUMPOINTS | ST_OVERLAPS 
   | ST_POINTFROMTEXT | ST_POINTFROMWKB | ST_POINTN 
   | ST_POLYFROMTEXT | ST_POLYFROMWKB | ST_POLYGONFROMTEXT 
   | ST_POLYGONFROMWKB | ST_SRID | ST_STARTPOINT 
   | ST_SYMDIFFERENCE | ST_TOUCHES | ST_UNION | ST_WITHIN 
   | ST_X | ST_Y | SUBDATE | SUBSTRING_INDEX | SUBTIME 
   | SYSTEM_USER | TAN | TIME | TIMEDIFF | TIMESTAMP 
   | TIMESTAMPADD | TIMESTAMPDIFF | TIME_FORMAT | TIME_TO_SEC 
   | TOUCHES | TO_BASE64 | TO_DAYS | TO_SECONDS | UCASE 
   | UNCOMPRESS | UNCOMPRESSED_LENGTH | UNHEX | UNIX_TIMESTAMP
   | UPDATEXML | UPPER | UUID | UUID_SHORT 
   | VALIDATE_PASSWORD_STRENGTH | VERSION 
   | WAIT_UNTIL_SQL_THREAD_AFTER_GTIDS | WEEK | WEEKDAY 
   | WEEKOFYEAR | WEIGHT_STRING | WITHIN | YEAR | YEARWEEK 
   | Y_FUNCTION | X_FUNCTION
   ;
