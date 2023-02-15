/*
IBM Db2 SQL grammar.
The MIT License (MIT).

Copyright (c) 2023, Michał Lorek.

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

parser grammar Db2Parser;

options { tokenVocab=Db2Lexer; }

db2_file
    : batch* EOF
    ;

batch
    : sql_command SEMI?
    ;

sql_command
    : ddl_command
    | dml_command
    | dcl_command
    | tcl_command
    | other_command
    ;

ddl_command
    : alter_command
    | create_command
    | drop_command
    ;

dml_command
    : select
    | insert
    | update
    | delete
    | merge
    | truncate
    ;

// DML
select
    : SELECT
    ;

select_statement
    : SELECT
    ;

delete
    : DELETE
    ;

insert
    : INSERT
    ;

merge
    : MERGE
    ;

truncate
    : TRUNCATE TABLE? table_name ((DROP | REUSE) STORAGE)?
        ((IGNORE | RESTRICT WHEN) DELETE TRIGGERS)? (CONTINUE IDENTITY)?
        IMMEDIATE?
    ;

update
    : UPDATE
    ;

// DCL
dcl_command
    : grant_statement
    | revoke_database_authorities
    | revoke_exemption
    | revoke_global_variable_privileges
    | revoke_index_privileges
    | revoke_module_privileges
    | revoke_package_privileges
    | revoke_role
    | revoke_routine_privileges
    | revoke_schema_privileges
    | revoke_security_label
    | revoke_sequence_privileges
    | revoke_server_privileges
    | revoke_setsessionuser_privilege
    | revoke_table_space_privileges
    | revoke_table_view_or_nickname_privileges
    | revoke_workload_privileges
    | revoke_xsr_object_privileges
    ;

grant_statement
    : grant_database_authorities
    | grant_exemption
    | grant_global_variable_privileges
    | grant_index_privileges
    | grant_module_privileges
    | grant_package_privileges
    | grant_role
    | grant_routine_privileges
    | grant_schema_privileges
    | grant_security_label
    | grant_sequence_privileges
    | grant_server_privileges
    | grant_setsessionuser_privilege
    | grant_table_space_privileges
    | grant_table_view_or_nickname_privileges
    | grant_workload_privileges
    | grant_xsr_object_privileges
    ;

grant_database_authorities
    : GRANT db_privilege_list ON DATABASE TO (grantee_list | PUBLIC)
    ;

db_privilege_list
    : db_privilege (',' db_privilege)*
    ;

db_privilege
    : ACCESSCTRL
    | BINDADD
    | CONNECT
    | CREATETAB
    | CREATE_EXTERNAL_ROUTINE
    | CREATE_NOT_FENCED_ROUTINE
    | CREATE_SOURCE_OBJECT
    | DATAACCESS
    | DBADM (WITH | WITHOUT)? (DATAACCESS | ACCESSCTRL)?
    | EXPLAIN
    | IMPLICIT_SCHEMA
    | LOAD
    | QUIESCE_CONNECT
    | SECADM
    | SQLADM
    | WLMADM
    ;

grantee
    : user_group_role? authorization_name
    ;

grantee_user_group
    : user_group authorization_name
    ;

user_group
    : USER
    | GROUP
    ;

grantee_list
    : grantee (',' grantee)*
    ;

grantee_list_public
    : grantee_list
    | PUBLIC
    ;

grantee_list_user_group
    : grantee_user_group (',' grantee_user_group)?
    ;

grant_exemption
    : GRANT EXEMPTION ON RULE exemption_privilege FOR policy_name TO grantee_list
    ;

exemption_privilege
    : DB2LBACREADARRAY
    | DB2LBACREADSET
    | DB2LBACREADTREE
    | DB2LBACWRITEARRAY (WRITEDOWN | WRITEUP)
    | DB2LBACWRITESET
    | DB2LBACWRITETREE
    | ALL
    ;

grant_global_variable_privileges
    : GRANT variable_privilege ON VARIABLE variable_name TO (grantee_list | PUBLIC) with_grant_option?
    ;

variable_privilege
    : ALL PRIVILEGES?
    | read_write (',' read_write)?
    ;

read_write
    : READ
    | WRITE
    ;

with_grant_option
    : WITH GRANT OPTION
    ;

grant_index_privileges
    : GRANT CONTROL ON INDEX index_name TO (grantee_list | PUBLIC)
    ;

grant_module_privileges
    : GRANT EXECUTE ON MODULE module_name TO (grantee_list | PUBLIC) with_grant_option?
    ;

grant_package_privileges
    : GRANT package_privilege_list ON (PACKAGE | PROGRAM) package_name TO (grantee_list | PUBLIC) with_grant_option?
    ;

package_privilege_list
    : package_privilege (',' package_privilege)*
    ;

package_privilege
    : BIND
    | CONTROL
    | EXECUTE
    | RUN
    ;

grant_role
    : GRANT ROLE? role_list TO (grantee_list | PUBLIC) with_grant_option?
    ;

role_list
    : role_name (',' role_name)*
    ;

grant_routine_privileges
    : GRANT EXECUTE ON ( function_designator
                       | FUNCTION (schema '.')? '*'
                       | method_designator
                       | METHOD '*' FOR (type_name | (schema '.')? '*')
                       | procedure_designator
                       | PROCEDURE (schema '.')? '*'
                       ) TO (grantee_list | PUBLIC) with_grant_option?
    ;

grant_schema_privileges
    : GRANT ( ALL PRIVILEGES?
            | schema_privilege_list
            ) ON (SCHEMA schema_name | CURRENT SCHEMA) TO (grantee_list | PUBLIC) with_grant_option?
    ;

schema_privilege_list
    : schema_privilege (',' schema_privilege)*
    ;

schema_privilege
    : ACCESSCTRL
    | ALTERIN
    | CREATEIN
    | DATAACCESS
    | DELETEIN
    | DROPIN
    | EXECUTEIN
    | INSERTIN
    | LOAD
    | SCHEMAADM
    | SELECTIN
    | UPDATEIN
    ;

grant_security_label
    : GRANT SECURITY LABEL security_label_name TO grantee_list (FOR (ALL | READ | WRITE) ACCESS)?
    ;

grant_sequence_privileges
    : GRANT sequence_privilege_list ON SEQUENCE sequence_name TO (grantee_list | PUBLIC) with_grant_option?
    ;

sequence_privilege_list
    : sequence_privilege (',' sequence_privilege)?
    ;

sequence_privilege
    : USAGE
    | ALTER
    ;

grant_server_privileges
    : GRANT PASSTHRU ON SERVER server_name TO (grantee_list | PUBLIC)
    ;

grant_setsessionuser_privilege
    : GRANT SETSESSIONUSER ON (user_list | PUBLIC) TO grantee_list_user_group
    ;

user_list
    : user_auth (',' user_auth)*
    ;

user_auth
    : USER session_authorization_name
    ;

grant_table_space_privileges
    : GRANT USE OF TABLESPACE tablespace_name TO (grantee_list | PUBLIC) with_grant_option?
    ;

grant_table_view_or_nickname_privileges
    : GRANT ( ALL PRIVILEGES?
            | tvn_privilege_list
            ) ON TABLE? (table_name | view_name | nick_name) TO (grantee_list | PUBLIC) with_grant_option?
    ;

tvn_privilege_list
    : tvn_privilege (',' tvn_privilege)*
    ;

tvn_privilege
    : ALTER
    | CONTROL
    | DELETE
    | INDEX
    | INSERT
    | REFERENCES column_list_paren?
    | SELECT
    | UPDATE column_list_paren?
    ;

column_list_paren
    : '(' column_list ')'
    ;

column_list
    : column_name (',' column_name)*
    ;

grant_workload_privileges
    : GRANT USAGE ON WORKLOAD workload_name TO (grantee_list | PUBLIC)
    ;

grant_xsr_object_privileges
    : GRANT USAGE ON XSROBJECT xsrobject_name TO PUBLIC
    ;

revoke_database_authorities
    : REVOKE db_privilege_list ON DATABASE FROM grantee_list_public by_all?
    ;

by_all
    : BY ALL
    ;

revoke_exemption
    : REVOKE EXEMPTION ON RULE exemption_privilege FOR policy_name FROM grantee_list_public
    ;

revoke_global_variable_privileges
    : REVOKE variable_privilege ON VARIABLE variable_name FROM grantee_list_public by_all? RESTRICT?
    ;

revoke_index_privileges
    : REVOKE CONTROL ON INDEX index_name FROM grantee_list_public by_all?
    ;

revoke_module_privileges
    : REVOKE EXECUTE ON MODULE module_name FROM grantee_list_public
    ;

revoke_package_privileges
    : REVOKE package_privilege ON (PACKAGE | PROGRAM) package_name FROM grantee_list_public by_all?
    ;

revoke_role
    : REVOKE (ADMIN OPTION FOR)? ROLE? role_list FROM grantee_list_public by_all?
    ;

revoke_routine_privileges
    : REVOKE EXECUTE ON ( function_designator
                        | FUNCTION (schema '.')? '*'
                        | method_designator
                        | METHOD '*' FOR (type_name | (schema '.')? '*')
                        | procedure_designator
                        | PROCEDURE (schema '.')? '*'
                        ) FROM  grantee_list_public by_all? RESTRICT
    ;

revoke_schema_privileges
    : REVOKE ( ALL PRIVILEGES?
             | schema_privilege_list
             ) ON (SCHEMA schema_name | CURRENT SCHEMA) FROM grantee_list_public by_all?
    ;

revoke_security_label
    : REVOKE SECURITY LABEL security_label_name FROM grantee_list_public
    ;

revoke_sequence_privileges
    : REVOKE sequence_privilege_list ON SEQUENCE sequence_name FROM grantee_list_public by_all? RESTRICT?
    ;

revoke_server_privileges
    : REVOKE PASSTHRU ON SERVER server_name FROM grantee_list_public by_all?
    ;

revoke_setsessionuser_privilege
    : REVOKE SETSESSIONUSER user_list FROM grantee_list_user_group
    ;

revoke_table_space_privileges
    : REVOKE USE OF TABLESPACE tablespace_name FROM grantee_list_public by_all?
    ;

revoke_table_view_or_nickname_privileges
    : REVOKE ( ALL PRIVILEGES?
             | tvn_privilege_list
             ) ON TABLE? (table_name | view_name | nick_name) FROM grantee_list_public by_all?
    ;

revoke_workload_privileges
    : REVOKE USAGE ON WORKLOAD workload_name FROM grantee_list_public by_all?
    ;

revoke_xsr_object_privileges
    : REVOKE USAGE ON XSROBJECT xsrobject_name FROM PUBLIC by_all?
    ;

user_group_role
    : USER
    | GROUP
    | ROLE
    ;

// TCL
tcl_command
    : commit
    | rollback
    | savepoint
    | release_savepoint
    ;

rollback
    : ROLLBACK WORK? (TO SAVEPOINT savepoint_name?)?
    ;

savepoint
    : SAVEPOINT savepoint_name UNIQUE? ON ROLLBACK RETAIN CURSORS (ON ROLLBACK RETAIN LOCKS)?
    ;

release_savepoint
    : RELEASE TO? SAVEPOINT savepoint_name
    ;

// ALTER
alter_command
    : alter_audit_policy
    | alter_bufferpool
    | alter_database_partition_group
    | alter_database
    | alter_event_monitor
    | alter_function
    | alter_histogram_template
    | alter_index
    | alter_mask
    | alter_method
    | alter_module
    | alter_nickname
    | alter_package
    | alter_permission
    | alter_procedure_external
    | alter_procedure_sourced
    | alter_procedure_sql
    | alter_schema
    | alter_security_label_component
    | alter_security_policy
    | alter_sequence
    | alter_server
    | alter_service_class
    | alter_stogroup
    | alter_table
    | alter_tablespace
    | alter_threshold
    | alter_trigger
    | alter_trusted_context
    | alter_type
    | alter_usage_list
    | alter_user_mapping
    | alter_view
    | alter_work_action_set
    | alter_work_class_set
    | alter_workload
    | alter_wrapper
    | alter_xsrobject
    ;

allocate_cursor
    : ALLOCATE cursor_name CURSOR FOR RESULT SET rs_locator_variable
    ;

alter_audit_policy
    : ALTER AUDIT POLICY policy_name
        ( CATEGORIES ( ALL
                     | AUDIT
                     | CHECKING
                     | CONTEXT
                     | EXECUTE (WITH DATA)?
                     | OBJMAINT
                     | SECMAINT
                     | SYSADMIN
                     | VALIDATE
                     )
                     STATUS ( BOTH
                            | FAILURE
                            | NONE
                            | SUCCESS
                            )

        )
        | ERROR TYPE (NORMAL | AUDIT)
    ;

alter_bufferpool
    : ALTER BUFFERPOOL bufferpool_name
       ( (IMMEDIATE | DEFERRED)? (MEMBER member_number)? SIZE (number_of_pages | number_of_pages? AUTOMATIC)
       | ADD DATABASE PARTITION GROUP db_partition_group_name
       | NUMBLOCKPAGES number_of_pages (BLOCKSIZE number_of_pages)?
       | BLOCKSIZE number_of_pages
       )
    ;

alter_database_partition_group
    : ALTER DATABASE PARTITION GROUP db_partition_name db_partition_group_list_item (',' db_partition_group_list_item)*
    ;

db_partition_group_list_item
    : ADD db_partition_num_nums db_partitions_clause db_partition_options?
    | DROP db_partitions_clause
    ;

db_partition_num_nums
    : DBPARTITIONNUM
    | DBPARTITIONNUMS
    ;

db_partitions_clause
    : '(' db_partition_number_list ')'
    ;

db_partition_options
    : LIKE DBPARTITIONNUM db_partition_number
    | WITHOUT TABLESPACES
    ;

alter_database
    : ALTER DATABASE database_name?
        alter_database_opts+
    ;

alter_database_opts
    : (ADD | DROP) STORAGE ON string_list
    ;

alter_event_monitor
    : ALTER EVENT MONITOR event_monitor_name alter_event_monitor_opts+
    ;

alter_event_monitor_opts
    : ADD LOGICAL GROUP evm_group ('(' target_table_options+ ')')?
    ;

alter_function
    : ALTER function_designator alter_function_opts+
    ;

alter_function_opts
    : EXTERNAL NAME (string | id_)
    | NOT? (FENCED | SECURED | THREADSAFE)
    ;

function_designator
    : FUNCTION function_name data_type_list_paren?
    | SPECIFIC FUNCTION specific_name
    ;

data_type_list
    : data_type (',' data_type)*
    ;

data_type_list_paren
    : '(' data_type_list? ')'
    ;

alter_histogram_template
    : ALTER HISTOGRAM TEMPLATE template_name HIGH BIN VALUE bigint_constant
    ;

alter_index
    : ALTER INDEX index_name COMPRESS yes_no
    ;

yes_no
    : YES
    | NO
    ;

alter_mask
    : ALTER MASK mask_name enable_disable
    ;

enable_disable
    : ENABLE
    | DISABLE
    ;

alter_method
    : ALTER method_designator EXTERNAL NAME (string | id_)
    ;

method_designator
    : METHOD method_name data_type_list_paren? FOR type_name
    | SPECIFIC METHOD specific_name
    ;

alter_module
    : ALTER MODULE module_name
        ( ADD alter_module_opts
        | DROP (BODY | module_object_identification)
        | PUBLISH alter_module_opts
        )
    ;

alter_module_opts
    : module_condition_definition
    | module_function_definition
    | module_procedure_definition
    | module_type_definition
    | module_variable_definition
    ;

module_function_definition
    : todo
    ;

module_procedure_definition
    : todo
    ;

module_type_definition
    : todo
    ;

module_variable_definition
    : todo
    ;

module_condition_definition
    : CONDITION condition_name (FOR (SQLSTATE VALUE?)? string_constant)?
    ;

module_object_identification
    : module_function_designator
    | module_procedure_designator
    | CONDITION condition_name
    | TYPE type_name
    | VARIABLE variable_name
    ;

module_function_designator
    : FUNCTION unqualified_function_name data_type_list_paren?
    | SPECIFIC FUNCTION unqualified_specific_name
    ;

module_procedure_designator
    : PROCEDURE unqualified_procedure_name data_type_list_paren?
    | SPECIFIC PROCEDURE unqualified_specific_name
    ;

alter_nickname
    : ALTER NICKNAME nick_name
        alter_nickname_opts_1?
        alter_nickname_opts_2+
    ;

alter_nickname_opts_1
    : OPTIONS '(' alter_nickname_opts_1_item (',' alter_nickname_opts_1_item)* ')'
    ;

alter_nickname_opts_1_item
    : add_set? nick_name_option_name string_constant
    | DROP nick_name_option_name
    ;

alter_nickname_opts_2
    : ALTER COLUMN? column_name alter_nickname_opts_2_item (',' alter_nickname_opts_2_item)*
    | ADD (unique_constraint | referential_constraint | check_constraint)
    | ALTER (FOREIGN KEY | CHECK) constraint_name constraint_alteration+
    | DROP (PRIMARY KEY | (FOREIGN KEY | UNIQUE | CHECK | CONSTRAINT) constraint_name)
    | allow_disallow CACHING
    ;

alter_nickname_opts_2_item
    : LOCAL NAME column_name
    | LOCAL TYPE local_data_type
    | federated_column_options
    ;

constraint_alteration
    : enable_disable QUERY OPTIMIZATION
    | NOT ENFORCED (NOT? TRUSTED)?
    ;

alter_package
    : ALTER PACKAGE package_name (VERSION? version_id)?
        alter_package_opts*
    ;

alter_package_opts
    : ACCESS PLAN REUSE yes_no
    | OPTIMIZE PROFILE (NONE | optimization_profile_name)
    | KEEP DYNAMIC yes_no
    ;

alter_permission
    : ALTER PERMISSION permission_name enable_disable
    ;

alter_procedure_external
    : ALTER procedure_designator alter_procedure_external_opts+
    ;

alter_procedure_external_opts
    : EXTERNAL NAME (string | id_)
    | NOT? FENCED
    | NO? EXTERNAL ACTION
    | NOT? THREADSAFE
    | NEW SAVEPOINT LEVEL
    ;

procedure_designator
    : PROCEDURE procedure_name data_type_list_paren?
    | SPECIFIC PROCEDURE specific_name
    ;

alter_procedure_sourced
    : ALTER procedure_designator (ALTER PARAMETER parameter_alteration)+
    ;

parameter_alteration
    : parameter_name SET DATA TYPE data_type
    ;

alter_procedure_sql
    : ALTER procedure_designator (NO? EXTERNAL ACTION | NEW SAVEPOINT LEVEL)+
    ;

alter_schema
    : ALTER SCHEMA schema_name DATA CAPTURE (NONE | CHANGES) (ENABLE ROW MODIFICATION TRACKING)?
    ;

alter_security_label_component
    : ALTER SECURITY LABEL COMPONENT component_name add_element_clause
    ;

add_element_clause
    : ADD ELEMENT string_constant (array_element_clause | tree_element_clause)?
    ;

array_element_clause
    : (BEFORE | AFTER) string_constant
    ;

tree_element_clause
    : ROOT
    | UNDER string_constant (OVER string_constant (',' OVER string_constant)*)?
    ;

alter_security_policy
    : ALTER SECURITY POLICY security_policy_name alter_security_policy_opts+
    ;

alter_security_policy_opts
    : ADD SECURITY LABEL COMPONENT component_name
    | (OVERRIDE_ | RESTRICT) NOT AUTHORIZED WRITE SECURITY LABEL
    | (USE | IGNORE) (GROUP | ROLE) AUTHORIZATIONS
    ;

alter_sequence
    : ALTER SEQUENCE sequence_name alter_sequence_opts+
    ;

alter_sequence_opts
    : RESTART (WITH numeric_constant)?
    | INCREMENT BY numeric_constant
    | (MINVALUE numeric_constant | NO MINVALUE)
    | (MAXVALUE numeric_constant | NO MAXVALUE)
    | NO? CYCLE
    | (CACHE integer_constant | NO CACHE)
    | NO? ORDER
    ;

alter_server
    : ALTER SERVER ( server_name (VERSION server_version)?
                   | TYPE server_type (VERSION server_version (WRAPPER wrapper_name)?)?
                   )
        (OPTIONS '(' alter_server_opts (',' alter_server_opts)* ')')?
    ;

alter_server_opts
    : add_set? server_option_name string_constant
    | DROP server_option_name
    ;

alter_service_class
    : ALTER SERVICE CLASS_ service_class_name (UNDER service_superclass_name)?
        alter_service_class_opts+
    ;

alter_service_class_opts
    : soft_hard? RESOURCE SHARES integer_constant
    | soft_hard? CPU SHARES integer_constant
    | CPU LIMIT (integer_constant | NONE)
    | ACTIVITY SORTMEM LIMIT (integer_constant | NONE)
    | MINIMUM RESOURCE SHARE integer_constant PERCENT
    | ADMISSION QUEUE ORDER (FIFO | LATENCY)
    | DEGREE_ SCALEBACK default_on_off?
    | MAXIMUM DEGREE_ default_on_off?
    | PREFETCH PRIORITY default_high_medium_low
    | OUTBOUND CORRELATOR (NONE | string_constant)
    | BUFFERPOOL PRIORITY default_high_medium_low
    | COLLECT AGGREGATE ACTIVITY DATA extended_base_none?
    | COLLECT AGGREGATE REQUEST DATA base_none?
    | COLLECT AGGREGATE UNIT OF WORK DATA base_none?
    | COLLECT REQUEST METRICS extended_base_none?
    | ACTIVITY (LIFETIME | QUEUETIME | ESTIMATEDCOST | EXECUTETIME | INTERARRIVALTIME) HISTOGRAM TEMPLATE template_name
    | REQUEST EXECUTETIME HISTOGRAM TEMPLATE template_name
    | UOW LIFETIME HISTOGRAM TEMPLATE template_name
    | enable_disable
    ;

default_on_off
    : DEFAULT
    | on_off
    ;

default_high_medium_low
    : DEFAULT
    | high_medium_low
    ;

alter_stogroup
    : ALTER STOGROUP storagegroup_name alter_stogroup_opts+
    ;

alter_stogroup_opts
    : (ADD | DROP) string_list
    | OVERHEAD number_of_milliseconds
    | DEVICE READ RATE number_megabytes_per_second
    | DATA TAG (integer_constant | NONE)
    | SET AS DEFAULT
    ;

alter_table
    : ALTER TABLE table_name
        ( alter_table_opts+
        | ADD PARTITION add_partition
        | ATTACH PARTITION attach_partition
        | DETACH PARTITION partition_name INTO table_name
        | ADD SECURITY POLICY policy_name
        | DROP SECURITY POLICY
        | ADD VERSIONING USE HISTORY TABLE history_table_name
        | DROP VERSIONING
        )
    ;

alter_table_opts
    : ADD ( COLUMN? column_definition
          | unique_constraint
          | referential_constraint
          | check_constraint
          | distribution_clause
          | RESTRICT ON DROP
          )
    | ADD (MATERIALIZED? QUERY)? materialized_query_definition
    | ALTER (FOREIGN KEY | CHECK) constraint_name constraint_alteration
    | ALTER COLUMN? column_alteration
    | activate_deactivate (ROW | COLUMN) ACCESS CONTROL
    | RENAME s=column_name TO t=column_name
    | DROP ( PRIMARY KEY
           | (FOREIGN KEY | UNIQUE | CHECK | CONSTRAINT) constraint_name
           | COLUMN? column_name cascade_restrict?
           | RESTRICT ON DROP
           )
    | DROP DISTRIBUTION
    | DROP MATERIALIZED? QUERY
    | ADD PERIOD period_definition_alter
    | DROP PERIOD period_name
    | DATA CAPTURE (NONE | CHANGE (INCLUDE LONGVAR COLUMNS)?)
    | ACTIVITY NOT LOGGED INITIALLY (WITH EMPTY TABLE)?
    | PCTFREE integer_value
    | LOCKSIZE (ROW | BLOCKINSERT | TABLE)
    | APPEND on_off
    | NOT? VOLATILE CARDINALITY?
    | COMPRESS (YES (ADAPTIVE | STATIC) | NO)
    | activate_deactivate VALUE COMPRESSION
    | LOG INDEX BUILD null_on_off
    ;

null_on_off
    : NULL_
    | on_off
    ;

cascade_restrict
    : CASCADE
    | RESTRICT
    ;

materialized_query_definition
    : '(' fullselect ')' refreshable_table_options*
    ;

refreshable_table_options
    : DATA INITIALLY DEFERRED
    | REFRESH (DEFERRED | IMMEDIATE)
    | enable_disable QUERY OPTIMIZATION
    | MAINTAINED BY (USER | REPLICATION | FEDERATED_TOOL | SYSTEM)
    ;

column_alteration
    : column_name ( SET ( DATA TYPE
                        | NOT NULL_
                        | INLINE LENGTH integer_value
                        | default_clause
                        | EXPRESSION as_generated_expression_clause
                        | (NOT | IMPLICITLY) HIDDEN_
                        )
                  | SET generation_alteration
                  | (SET generation_alteration)? identity_alteration
                  | SET generation_attribute as_identity_clause
                  | SET GENERATE ALWAYS? (as_generated_expression_clause | as_row_transaction_start_id_clause | as_row_transaction_timestamp_clause)
                  | DROP (DEFAULT | GENERATED | NOT NULL_)
                  | ADD SCOPE (typed_table_name | typed_view_name)
                  | COMPRESS (SYSTEM DEFAULT | OFF)
                  | SECURED WITH security_label_name
                  | DROP COLUMN SECURITY
                  )
    ;

generation_alteration
    : SET GENERATED (ALWAYS | BY DEFAULT)
    ;

identity_alteration
    : SET INCREMENT BY numeric_constant
    | SET (NO MINVALUE | MINVALUE numeric_constant)
    | SET (NO MAXVALUE | MAXVALUE numeric_constant)
    | SET NO? CYCLE
    | SET (NO CACHE | CACHE integer_constant)
    | SET NO? ORDER
    | RESTART (WITH numeric_constant)?
    ;

generation_attribute
    : GENERATED (ALWAYS | BY DEFAULT)?
    ;

as_identity_clause
    : AS IDENTITY ('(' as_identity_clause_opts+ ')')?
    ;

as_identity_clause_opts
    : START WITH numeric_constant
    | INCREMENT BY numeric_constant
    | (NO MINVALUE | MINVALUE numeric_constant)
    | (NO MAXVALUE | MAXVALUE numeric_constant)
    | NO? CYCLE
    | (NO CACHE | CACHE integer_constant)
    ;

period_definition_alter
    : (SYSTEM_TIME | BUSINESS_TIME) '(' b=column_name ',' e=column_name ')'
    ;

add_partition
    : partition_name? boundary_spec_alter (IN tablespace_name)
        (INDEX IN tablespace_name (LONG IN tablespace_name)?)?
    ;

boundary_spec_alter
    : starting_clause ending_clause
    | ending_clause
    ;

attach_partition
    : partition_name? boundary_spec_alter FROM table_name (REQUIRE MATCHING INDEXES)?
    ;

activate_deactivate
    : ACTIVATE
    | DEACTIVATE
    ;

alter_tablespace
    : ALTER TABLESPACE tablespace_name alter_tablespace_opts+
    ;

alter_tablespace_opts
    : ADD add_clause
    | BEGIN NEW STRIPE SET db_container_clause on_db_partitions_clause?
    | DROP drop_container_clause on_db_partitions_clause?
    | REDUCE (db_container_clause | all_containers_clause | MAX | STOP | integer_value kmg_percent?)? on_db_partitions_clause?
    | (EXTEND | RESIZE) (db_container_clause | all_containers_clause) on_db_partitions_clause?
    | REBALANCE (SUSPEND | RESUME)?
    | PREFETCHSIZE (AUTOMATIC | number_of_pages | integer_value km)
    | BUFFERPOOL bufferpool_name
    | OVERHEAD (number_of_milliseconds | INHERIT)
    | TRANSFERRATE (number_of_milliseconds | INHERIT)
    | NO? FILE SYSTEM CACHING
    | DROPPED TABLE RECOVERY yes_no
    | SWITCH ONLINE
    | AUTORESIZE yes_no
    | INCREASESIZE integer_value kmg_percent
    | MAXSIZE (integer_value kmg | NONE)
    | CONVERT TO LARGE
    | LOWER HIGH WATER MARK STOP?
    | USING STOGROUP storagegroup_name
    | DATA TAG (integer_constant | INHERIT | NONE)
    | MANAGED BY AUTOMATIC STORAGE
    ;

add_clause
    : (TO STRIPE SET stripeset)? db_container_clause on_db_partitions_clause?
    | system_container_clause on_db_partitions_clause
    ;

db_container_clause
    : '(' db_container_clause_opts (',' db_container_clause_opts)* ')'
    ;

db_container_clause_opts
    : file_device string (number_of_pages | integer_value kmg)
    ;

drop_container_clause
    : '(' file_device string (',' file_device string)* ')'
    ;

file_device
    : FILE
    | DEVICE
    ;

all_containers_clause
    : '(' ALL CONTAINERS? (number_of_pages | integer_value kmg) ')'
    ;

system_container_clause
    : '(' string_list ')'
    ;

stripeset
    : todo
    ;

km
    : K
    | M
    ;

kmg_percent
    : kmg
    | PERCENT
    ;

alter_threshold
    : ALTER THRESHOLD threshold_name alter_threshold_opts+
    ;

alter_threshold_opts
    : WHEN ( alter_threshold_predicate (PERFORM ACTION | alter_threshold_exceeded_actions+)?
           | (EXCEEDED alter_threshold_exceeded_actions+)?
           )
    | enable_disable
    ;

alter_threshold_predicate
    : TOTALMEMBERCONNECTIONS '>' integer_value
    | TOTALSCMEMBERCONNECTIONS '>' integer_value (AND QUEUEDCONNECTIONS ('>' integer_value | UNBOUNDED))?
    | CONNECTIONIDLETIME '>' integer_value day_to_minutes
    | (CONCURRENTWORKLOADOCCURRENCES | CONCURRENTWORKLOADACTIVITIES) '>' integer_value
    | CONCURRENTDBCOORDACTIVITIES '>' integer_value (AND QUEUEDCONNECTIONS ('>' integer_value | UNBOUNDED))?
    | ESTIMATEDSQLCOST '>' bigint_value
    | SQLROWSRETURNED '>' integer_value
    | (ACTIVITYTOTALTIME | UOWTOTALTIME) '>' integer_value day_to_seconds
    | (SQLTEMPSPACE | AGGSQLTEMPSPACE) '>' integer_value kmg
    | (SQLROWSREAD | SQLROWSREADINSC) '>' bigint_value checking_every?
    | (CPUTIME | CPUTIMEINSC) '>' integer_value hour_to_seconds checking_every?
    | (ACTIVITYTOTALRUNTIME | ACTIVITYTOTALRUNTIMEINALLSC) '>' integer_value day_to_seconds
    | SORTSHRHEAPUTIL '>' integer_value PERCENT (AND BLOCKING ADMISSION FOR '>' integer_value day_to_seconds)?
    | DATATAGINSC NOT? IN '(' integer_constant_list ')'
    ;

alter_threshold_exceeded_actions
    : COLLECT ACTIVITY DATA (alter_collect_activity_data_clause | NONE)
    | (STOP EXECUTION | CONTINUE | FORCE APPLICATION | remap_activity_action)
    ;

dt_units
    : DAY
    | DAYS
    | HOUR
    | HOURS
    | MINUTE
    | MINUTES
    ;

dt_units_with_seconds
    : dt_units
    | SECONDS
    ;

alter_trigger
    : ALTER TRIGGER trigger_name NOT? SECURED
    ;

alter_trusted_context
    : ALTER TRUSTED CONTEXT context_name alter_trusted_context_opts+
    ;

alter_trusted_context_opts
    : ALTER alter_trusted_context_opts_alter_opts+
    | ADD ATTRIBUTES '(' address_clause (',' address_clause)* ')'
    | DROP ATTRIBUTES '(' ADDRESS address_value (',' ADDRESS address_value)* ')'
    | user_clause
    ;

alter_trusted_context_opts_alter_opts
    : SYSTEM AUTHID authorization_name
    | ATTRIBUTES '(' addr_clause_encryption_val (',' addr_clause_encryption_val)* ')'
    | (NO DEFAULT ROLE | DEFAULT ROLE role_name)
    | enable_disable
    ;

addr_clause_encryption_val
    : address_clause
    | ENCRYPTION encryption_value
    ;

address_clause
    : ADDRESS address_value (WITH ENCRYPTION encryption_value)?
    ;

user_clause
    : (ADD | REPLACE) USE FOR use_for_opts (',' use_for_opts)*
    | DROP USE FOR use_for_opts_2 (',' use_for_opts_2)*
    ;

use_for_opts
    : (authorization_name (ROLE role_name)? | PUBLIC)
        (with_without AUTHENTICATION)?
    ;

use_for_opts_2
    : authorization_name
    | PUBLIC
    ;

alter_type
    : ALTER TYPE type_name alter_type_opts+
    ;

alter_type_opts
    : ADD ATTRIBUTE attribute_definition
    | DROP ATTRIBUTE attribute_name RESTRICT?
    | ADD METHOD method_specification
    | ALTER method_identifier method_options
    | DROP method_identifier RESTRICT?
    ;

method_identifier
    : METHOD method_name ('(' data_type_list_paren?  ')')?
    | SPECIFIC METHOD specific_name
    ;

method_options
    : NOT? FENCED
    | NOT? THREADSAFE
    ;

alter_usage_list
    : ALTER USAGE LIST usage_list_name alter_usage_list_opts_item+
    ;

alter_usage_list_opts_item
    : LIST SIZE integer_value
    | WHEN FULL (WRAP | DEACTIVATE)
    | (INACTIVE | ACTIVE) ON START DATABASE
    ;

alter_user_mapping
    : ALTER USER MAPPING FOR (authorization_name | USER | PUBLIC) SERVER server_name
        OPTIONS '(' alter_user_mapping_opts_item (',' alter_user_mapping_opts_item)* ')'
    ;

alter_user_mapping_opts_item
    : add_set? user_mapping_option_name string_constant
    | DROP user_mapping_option_name
    ;

add_set
    : ADD
    | SET
    ;

alter_view
    : ALTER VIEW view_name
        ( alter_view_opts+
        | enable_disable? QUERY OPTIMIZATION
        )
    ;

alter_view_opts
    : ALTER COLUMN? column_name ADD SCOPE (typed_table_name | typed_view_name)
    ;

alter_work_action_set
    : ALTER WORK ACTION SET work_action_set_name alter_work_action_set_opts+
    ;

alter_work_action_set_opts
    : ADD work_action_definition
    | ALTER work_action_alteration
    | DROP (WORK ACTION)? work_action_name
    | enable_disable
    ;

work_action_alteration
    : (WORK ACTION)? work_action_name work_action_alteration_opts+
    ;

work_action_alteration_opts
    : SET WORK CLASS_ work_class_name
    | alter_action_types_clause
    | ACTIVITY (LIFETIME | QUEUETIME | EXECUTETIME | ESTIMATEDCOST | INTERARRIVALTIME) HISTOGRAM TEMPLATE template_name
    | enable_disable
    ;

alter_action_types_clause
    : MAP ACTIVITY (with_without NESTED)? TO service_subclass_name
    | WHEN (threshold_predicate_clause (PERFORM ACTION | alter_threshold_exceeded_actions)? | (EXCEEDED alter_threshold_exceeded_actions)?)
    | PREVENT EXECUTION
    | COUNT ACTIVITY
    | COLLECT ACTIVITY DATA alter_collect_activity_data_clause
    | COLLECT AGGREGATE ACTIVITY DATA (BASE | EXTENDED)?
    ;

threshold_predicate_clause
    : CONCURRENTDBCOORDACTIVITIES '>' integer_value (AND QUEUEDACTIVITIES ('>' integer_value | UNBOUNDED))?
    | SQLTEMPSPACE '>' kmg
    | SQLROWSRETURNED '>' integer_value
    | ESTIMATEDSQLCOST '>' bigint_value
    | CPUTIME '>' integer_value hours_minutes checking_every?
    | SQLROWSREAD '>' bigint_value checking_every?
    | SORTSHRHEAPUTIL '>' integer_value PERCENT (AND BLOCKING ADMISSION FOR '>' integer_value day_to_seconds)?
    | (ACTIVITYTOTALTIME | ACTIVITYTOTALRUNTIME) '>' integer_value day_to_seconds
    ;

alter_work_class_set
    : ALTER WORK CLASS_ SET work_class_set_name alter_work_class_set_opts+
    ;

alter_work_class_set_opts
    : ADD work_class_definition
    | ALTER work_class_alteration
    | DROP (WORK CLASS_)? work_class_name
    ;

work_class_alteration
    : (WORK CLASS_)? work_class_name work_class_alteration_opts+
    ;

work_class_alteration_opts
    : for_from_to_alter_clause
    | schema_alter_clause
    | data_tag_alter_clause
    | position_clause
    ;

for_from_to_alter_clause
    : FOR ( (TIMERONCOST | CARDINALITY) FROM from_value (TO (UNBOUNDED | to_value)?)? | ALL UNITS UNBOUNDED)
    ;

schema_alter_clause
    : ROUTINES (IN SCHEMA schema_name | ALL)
    ;

data_tag_alter_clause
    : DATA TAG LIST CONTAINS (integer_constant | ANY)
    ;

alter_workload
    : ALTER WORKLOAD workload_name alter_workload_opts_item*
    ;

alter_workload_opts_item
    : ADD connection_attributes
    | DROP connection_attributes
    | allow_disallow DB ACCESS
    | enable_disable
    | MAXIMUM DEGREE_ (DEFAULT | degree)
    | SERVICE CLASS_ service_class_name (UNDER service_superclass_name)?
    | POSITION (CRITICAL | HIGH | MEDIUM | LOW)
    | COLLECT ACTIVITY DATA (alter_collect_activity_data_clause | NONE)
    | COLLECT ACTIVITY METRICS DATA extended_base_none?
    | COLLECT AGGREGATE ACTIVITY DATA extended_base_none?
    | COLLECT AGGREGATE UNIT OF WORK base_none?
    | COLLECT LOCK TIMEOUT DATA (alter_collect_history_clause | NONE)
    | COLLECT DEADLOCK DATA alter_collect_history_clause
    | COLLECT LOCK WAIT DATA (alter_collect_lock_wait_data_clause | NONE)
    | COLLECT UNIT OF WORK DATA ( BASE (INCLUDE package_executable LIST (',' package_executable LIST)*)?
                                | NONE
                                )
    | ACTIVITY LIFETIME HISTOGRAM TEMPLATE template_name
    ;

package_executable
    : PACKAGE
    | EXECUTABLE
    ;

base_none
    : BASE
    | NONE
    ;

extended_base_none
    : EXTENDED
    | base_none
    ;

alter_collect_activity_data_clause
    : (ON COORDINATOR MEMBER? | ON ALL MEMBERS?)
        (WITHOUT DETAILS | WITH with_opts (',' with_opts)* (AND VALUES)?)
    ;

with_opts
    : DETAILS
    | SECTION (INCLUDE ACTUALS BASE)?
    ;

alter_collect_history_clause
    : WITHOUT HISTORY
    | WITH HISTORY (AND VALUES)?
    ;

alter_collect_lock_wait_data_clause
    : FOR LOCKS WAITING MORE_ THAN wait_time (SECONDS | MICROSECONDS)
    | alter_collect_history_clause //todo dot
    ;

alter_wrapper
    : ALTER WRAPPER wrapper_name OPTIONS '(' alter_wrapper_opts_item (',' alter_wrapper_opts_item)* ')'
    ;

alter_wrapper_opts_item
    : add_set? wrapper_option_name string_constant
    | DROP wrapper_option_name
    ;

alter_xsrobject
    : ALTER XSROBJECT xsrobject_name enable_disable DECOMPOSITION
    ;

string
    : STRING_LITERAL
    ;

string_constant
    : string
    ;

numeric_constant
    : todo
    ;

data_type
    : built_in_type
    | anchored_data_type
    | row_type_name
    | array_type_name
    ;

anchored_data_type
    : ANCHOR (DATA TYPE)? TO? ( variable_name
                              | table_name '.' column_name
                              | ROW OF? (table_name | view_name | cursor_variable_name)
                              )
    ;

anchored_non_row_data_type
    : ANCHOR DATA TYPE TO (variable_name | table_name '.' column_name)
    ;

anchored_row_data_type
    : ANCHOR (DATA TYPE)? TO? ( variable_name
                              | ROW OF ( table_name
                                       | view_name
                                       | cursor_variable_name
                                       )
                              )
    ;

source_data_type
    : built_in_type
    | anchored_data_type
    ;

data_type_constrainst
    : (NOT NULL_)? (CHECK '(' check_condition ')')?
    ;

check_condition
    : expression
    ;

data_type_2
    : (INTEGER | INT)
    | (VARCHAR | char_character VARYING) '(' integer_value octets_codeunits? ')'
    | anchored_non_row_data_type
    ;

built_in_type
    : ( SMALLINT
      | INT
      | INTEGER
      | BIGINT
      )
    | ( DEC
      | DECIMAL
      | NUMERIC
      | NUM
      ) '(' integer_value (',' integer_value)? ')'
    | ( FLOAT ('(' integer_value ')')?
      | REAL
      | DOUBLE PRECISION?
      )
    | ( char_character VARYING?
      | VARCHAR
      ) ('(' integer_value octets_codeunits? ')')? (FOR BIT DATA)?
    | ( CLOB
      | char_character LARGE OBJECT
      ) ('(' integer_value kmg? octets_codeunits? ')')?
    | (GRAPHIC | VARGRAPHIC) ('(' integer_value codeunits? ')')?
    | DBCLOB ('(' integer_value kmg? codeunits? ')')?
    | (NCHAR | NATIONAL char_character) integer_paren?
    | (NVARCHAR | NCHAR VARYING | NATIONAL char_character VARYING) integer_paren
    | (NCLOB | NCHAR LARGE OBJECT | NATIONAL CHARACTER LARGE OBJECT) integer_kmg_paren?
    | BINARY integer_paren?
    | (VARBINARY | BINARY VARYING) integer_paren
    | (BLOB | BINARY LARGE OBJECT) integer_kmg_paren?
    | (DATE | TIME | TIMESTAMP integer_paren?)
    | BOOLEAN
    | XML
    | CURSOR
    ;

integer_paren
    : '(' integer_value ')'
    ;

integer_kmg_paren
    : '(' integer_value kmg? ')'
    ;

char_character
    : CHAR
    | CHARACTER
    ;

octets_codeunits
    : OCTETS
    | CODEUNITS32
    ;

codeunits
    : CODEUNITS16
    | CODEUNITS32
    ;

kmg
    : K
    | M
    | G
    ;

rs_locator_variable
    : id_
    ;

integer_constant_list
    : integer_constant (',' integer_constant)*
    ;

integer_constant
    : DECIMAL_LITERAL
    ;

integer_value
    : DECIMAL_LITERAL
    ;

positive_integer
    : todo
    ;

bigint_value
    : DECIMAL_LITERAL
    ;


bigint_constant
    : DECIMAL_LITERAL
    ;

member_number
    : id_
    ;

version_id
    : id_
    ;

// DROP
drop_command
    : DROP ( alias_designator
           | AUDIT POLICY policy_name
           | BUFFERPOOL bufferpool_name
           | DATABASE PARTITION GROUP db_partition_group_name
           | EVENT MONITOR event_monitor_name
           | function_designator RESTRICT?
           | FUNCTION MAPPING function_mapping_name
           | HISTOGRAM TEMPLATE template_name
           | INDEX index_name
           | INDEX EXTENSION index_extension_name RESTRICT
           | MASK mask_name
           | method_designator RESTRICT?
           | MODULE module_name
           | NICKNAME nick_name
           | PACKAGE package_name (VERSION? version_id)?
           | PERMISSION permission_name
           | procedure_designator RESTRICT?
           | ROLE role_name
           | SCHEMA schema_name RESTRICT
           | SECURITY LABEL security_label_name RESTRICT?
           | SECURITY LABEL COMPONENT sec_label_comp_name RESTRICT?
           | SECURITY POLICY security_policy_name RESTRICT?
           | SEQUENCE sequence_name RESTRICT?
           | SERVER server_name
           | service_class_designator RESTRICT?
           | STOGROUP storagegroup_name RESTRICT?
           | TABLE (IF EXISTS)? table_name
           | TABLE HIERARCHY root_table_name
           | (TABLESPACE | TABLESPACES) tablespace_name_list
           | (TRANSFORM | TRANSFORMS) (ALL | group_name) FOR type_name
           | THRESHOLD threshold_name
           | TRIGGER trigger_name
           | TRUSTED CONTEXT context_name
           | TYPE type_name RESTRICT?
           | TYPE MAPPING type_mapping_name
           | USAGE LIST usage_list_name
           | USER MAPPING FOR (authorization_name | USER) SERVER server_name
           | VARIABLE variable_name RESTRICT?
           | VIEW view_name
           | VIEW HIERARCHY root_view_name
           | WORK ACTION SET work_action_set_name
           | WORK CLASS_ SET work_class_set_name
           | WORKLOAD workload_name
           | WRAPPER wrapper_name
           | XSROBJECT xsrobject_name
           )
    ;

alias_designator
    : PUBLIC? ALIAS alias_name (FOR (TABLE | MODULE | SEQUENCE))?
    ;

service_class_designator
    : SERVICE CLASS_ service_class_name (UNDER service_superclass_name)?
    ;

tablespace_name_list
    : tablespace_name (',' tablespace_name)*
    ;

// OTHER
other_command
    : allocate_cursor
    | associate_locators
    | audit
    | begin_declare_section
    | call
    | close
    | comment
    | connect_type_1
    | connect_type_2
    | declare_cursor
    | declare_global_temporary_table
    | describe
    | describe_input
    | describe_output
    | disconnect
    | end_declare_section
    | execute
    | execute_immediate
    | explain
    | fetch
    | flush_bufferpools
    | flush_event_monitor
    | flush_federated_cache
    | flush_optimization_profile_cache
    | flush_package_cache
    | flush_authentication_cache
    | free_locator
    | get_diagnostics
    | lock_table
    | pipe
    | refresh_table
    | release_connection
    | rename
    | rename_stogroup
    | rename_tablespace
    | set

    ;

associate_locators
    : ASSOCIATE (RESULT SET)? (LOCATOR | LOCATORS) rs_locator_variable (',' rs_locator_variable)* WITH PROCEDURE procedure_name
    ;

audit
    : AUDIT ( DATABASE
            | TABLE table_name
            | TRUSTED CONTEXT context_name
            | (USER | GROUP | ROLE) authorization_name
            | ACCESSCTRL
            | CREATE_SECURE_OBJECT
            )
        ( (USING | REPLACE) POLICY policy_name
        | REMOVE POLICY
        )
    ;

begin_declare_section
    : BEGIN DECLARE SECTION
    ;

call
    : CALL procedure_name arg_list_paren? (parameter_name '=>')? (expression | DEFAULT | NULL_)
    ;

arg_list_paren
    : '(' arg_list? ')'
    ;

arg_list
    : argument (',' argument)*
    ;

argument
    : id_
    ;

close
    : CLOSE (cursor_name | cursor_variable_name) (WITH RELEASE)?
    ;

comment
    : COMMENT ON ( comment_objects IS string_constant
                 | (table_name | view_name) '(' column_comment (',' column_comment)* ')'
                 )
    ;

column_comment
    : column_name IS string_constant
    ;

comment_objects
    : alias_designator
    | AUDIT POLICY policy_name
    | COLUMN (table_name | view_name) '.' column_name
    | CONSTRAINT table_name '.' constraint_name
    | DATABASE PARTITION GROUP db_partition_group_name
    | function_designator
    | FUNCTION MAPPING function_mapping_name
    | HISTOGRAM TEMPLATE template_name
    | INDEX index_name
    | MASK mask_name
    | MODULE module_name
    | NICKNAME nick_name
    | PACKAGE package_name (VERSION? version_id)?
    | procedure_designator
    | ROLE role_name
    | SCHEMA schema_name
    | SECURITY LABEL security_label_name
    | SECURITY LABEL COMPONENT sec_label_comp_name
    | SECURITY POLICY security_policy_name
    | SEQUENCE sequence_name
    | SERVER server_name
    | SERVER OPTION server_option_name FOR remote_server
    | service_class_designator
    | TABLE (table_name | view_name)
    | TABLESPACE tablespace_name
    | THRESHOLD threshold_name
    | TRIGGER trigger_name
    | TRUSTED CONTEXT context_name
    | TYPE type_name
    | TYPE MAPPING type_mapping_name
    | USAGE LIST usage_list_name
    | VARIABLE variable_name
    | WORK ACTION SET work_action_set_name
    | WORK CLASS_ SET work_class_set_name
    | WORKLOAD workload_name
    | WRAPPER wrapper_name
    | XSROBJECT xsrobject_name
    ;

commit
    : COMMIT WORK?
    ;

connect_type_1
    : CONNECT (TO (server_name | host_variable) lock_block? authorization? | RESET | authorization)?
    ;

authorization
    : USER authorization_name passwords?
    | accesstoken
    | APIKEY api_key
    ;

passwords
    : USING password_ (NEW password_ CONFIRM password_)?
    | CHANGE PASSWORD
    ;

lock_block
    : IN SHARE MODE
    | IN EXCLUSIVE MODE (ON SINGLE MEMBER)?
    ;

accesstoken
    : ACCESSTOKEN token (ACCESSTOKENTYPE token_type)
    ;

token
    : todo
    ;

api_key
    : todo
    ;

token_type
    : todo
    ;

connect_type_2
    : todo
    ;

declare_cursor
    : DECLARE
    ;

declare_global_temporary_table
    : DECLARE
    ;

describe
    : DESCRIBE
    ;

describe_input
    : DESCRIBE INPUT statement_name INTO descriptor_name
    ;

describe_output
    : DESCRIBE OUTPUT statement_name INTO descriptor_name
    ;

disconnect
    : DISCONNECT (server_name | host_variable | CURRENT | ALL SQL?)
    ;

end_declare_section
    : END DECLARE SECTION
    ;

execute
    : EXECUTE statement_name
        (INTO (assignment_target (',' assignment_target)* | DESCRIPTOR descriptor_name))?
        (USING (host_variable_expression (',' host_variable_expression)* | DESCRIPTOR input_descriptor_name))?
        (FOR (host_variable | integer_constant) ROWS)?
    ;

host_variable_expression
    : host_variable
    | expression
    ;

assignment_target
    : global_variable_name
    | host_variable_name
    | sql_parameter_name
    | transition_variable_name
    | array_variable_name '[' array_index ']'
    | field_reference
    ;

execute_immediate
    : EXECUTE IMMEDIATE expression
    ;

explain
    : EXPLAIN
        (PLAN SELECTION | ALL | PLAN)
        ((FOR | WITH) SNAPSHOT)?
        (WITH REOPT ONCE)?
        (SET QUERYNO '=' integer_value)?
        (SET QUERYTAG '=' string_constant)? FOR
        (explainable_sql_statement | XQUERY string)
    ;

explainable_sql_statement
    : todo
    ;

fetch
    : FETCH FROM? (cursor_name | cursor_variable_name)
        (INTO assignment_target (',' assignment_target)* | USING DESCRIPTOR descriptor_name)
    ;

flush_bufferpools
    : FLUSH (BUFFERPOOL | BUFFERPOOLS) ALL
    ;

flush_event_monitor
    : FLUSH EVENT MONITOR event_monitor_name BUFFER?
    ;

flush_federated_cache
    : FLUSH FEDERATED CACHE
         ( FOR ( remote_object_name
               | data_source_name '.' schema_name '.' '*'
               | data_source_name '.' '*' '.' '*'
               | SERVER data_source_name
               | ALL
               )
         )?
    ;

flush_optimization_profile_cache
    : FLUSH OPTIMIZATION PROFILE CACHE (ALL | optimization_profile_name)
    ;

flush_package_cache
    : FLUSH PACKAGE CACHE DYNAMIC (FOR EXECUTABLE ID executable_id USING HARD INVALIDATION)?
    ;

flush_authentication_cache
    : FLUSH AUTHENTICATION CACHE (FOR ALL)?
    ;

free_locator
    : FREE LOCATOR variable_name (',' variable_name)*
    ;

get_diagnostics
    : GET DIAGNOSTICS (statement_information | condition_information)
    ;

statement_information
    : variable '=' (DB2_RETURN_STATUS | DB2_SQL_NESTING_LEVEL | ROW_COUNT)
    ;

condition_information
    : EXCEPTION condition_var_assignment (',' condition_var_assignment)*
    ;

condition_var_assignment
    : variable '=' (DB2_TOKEN_STRING | MESSAGE_TEXT)
    ;

lock_table
    : LOCK TABLE (table_name | nick_name) IN (SHARE | EXCLUSIVE) MODE
    ;

pipe
    : PIPE
    ;

refresh_table
    : REFRESH TABLE
    ;

release_connection
    : RELEASE (server_name | host_variable | CURRENT | ALL SQL?)
    ;

rename
    : RENAME (TABLE? source_table_name | INDEX source_index_name) TO target_identifier
    ;

rename_stogroup
    : RENAME STOGROUP source_storagegroup_name TO target_storagegroup_name
    ;

rename_tablespace
    : RENAME TABLESPACE source_tablespace_name TO target_tablespace_name
    ;

set
    : SET ( COMPILATION ENVIRONMENT EQ? host_variable
          | CONNECTION (server_name | host_variable)
          | CURRENT DECFLOAT ROUNDING MODE EQ? (ROUND_CEILING | ROUND_DOWN | ROUND_FLOOR | ROUND_HALF_EVEN | ROUND_HALF_UP | string_constant | host_variable)
          | CURRENT? DEFAULT TRANSFORM GROUP EQ? group_name
          | CURRENT DEGREE_ EQ? (string_constant | host_variable)
          | CURRENT EXPLAIN MODE EQ? (yes_no | EXPLAIN NORCAC? | REOPT | (RECOMMEND|EVALUATE) (INDEXES|PARTITIONINGS) | host_variable)
          | CURRENT EXPLAIN SNAPSHOT EQ? (yes_no | EXPLAIN | REOPT | host_variable)
          | CURRENT FEDERATED ASYNCHRONY EQ? (ANY | integer_constant | host_variable)
          | CURRENT IMPLICIT XMLPARSE OPTION EQ? (string_constant | host_variable)
          | CURRENT? ISOLATION EQ? (UR | CS | RR | RS | RESET)
          | CURRENT LOCALE (LC_MESSAGES_ | LC_TIME_) EQ? (host_variable | string_constant)
          | CURRENT? LOCK TIMEOUT EQ? (NOT? WAIT | NULL_ | WAIT integer_constant | host_variable)
          | CURRENT MAINTAINED TABLE? TYPES (FOR OPTIMIZATION)? EQ? (ALL | NONE | host_variable | maintain_opt_list)
          | CURRENT MDC ROLLOUT MODE (NONE | IMMEDIATE | DEFERRED | host_variable)
          | CURRENT OPTIMIZATION PROFILE EQ? (optimization_profile_name | host_variable | string_constant | NULL_)
          | CURRENT PACKAGE PATH EQ? pkg_opt_list
          | CURRENT PACKAGESET EQ? (string_constant | host_variable)
          | CURRENT QUERY OPTIMIZATION EQ? (SINGLE_DIGIT | host_variable)
          | CURRENT REFRESH AGE EQ? (numeric_constant | ANY | host_variable)
          | CURRENT SQL_CCFLAGS EQ?? (variable | string_constant)
          | CURRENT TEMPORAL (BUSINESS_TIME | SYSTEM_TIME) EQ? (NULL_ | expression)
          | ENCRYPTION PASSWORD EQ? (host_variable | string_constant)
          | EVENT MONITOR event_monitor_name STATE EQ? (host_variable | L_ZERO | L_ONE)
          | INTEGRITY ( FOR table_name (',' table_name)* (OFF access_mode_clause cascade_clause | FULL ACCESS | PRUNE)
                      | FOR table_name table_checked_options_list (',' table_name table_checked_options_list)* IMMEDIATE CHECKED check_options?
                      | FOR table_name table_unchecked_options (',' table_name table_unchecked_options)* IMMEDIATE UNCHECKED
                      )
          | PASSTHRU (server_name | RESET)
          | (CURRENT? PATH | CURRENT_PATH) EQ? path_opt_list
          | ROLE EQ? role_name
          | CURRENT? SCHEMA EQ? (schema_name | USER | SESSION_USER | SYSTEM_USER | CURRENT_USER | host_variable | string_constant)
          | SERVER OPTION server_option_name TO string_constant FOR SERVER server_name
          | (SESSION AUTHORIZATION | SESSION_USER) EQ? (authorization_name | USER | CURRENT_USER | SYSTEM_USER | host_variable | string_constant)
          | USAGE LIST usage_list_name STATE EQ? (ACTIVE | INACTIVE | RELEASED | host_variable)
          | ( var_def_list
            | boolean_variable_name EQ (search_condition | TRUE | FALSE | NULL_)
            | array_variable_name '[' array_index ']' EQ (expression | NULL_)
            | target_cursor_variable EQ (cursor_variable_name | cursor_value_constructor | NULL_)
            | target_row_variable EQ ( '(' expr_null (',' expr_null)* ')'
                                     | '(' row_fullselect ')'
                                     | row_expression
                                     | NULL_
                                     )
            )
          )
    ;

access_mode_clause
    : (NO | READ) ACCESS
    ;

cascade_clause
    : CASCADE (IMMEDIATE to_descendent_types? | DEFERRED)
    ;

to_descendent_types
    : TO ALL TABLES
    | TO table_type_list
    ;

table_type_list
    : table_type (',' table_type)*
    ;

table_type
    : MATERIALIZED QUERY TABLES
    | FOREIGN KEY TABLES
    | STAGING TABLES
    ;

table_checked_options_list
    : table_checked_options (',' table_checked_options)*
    ;

table_checked_options
    : online_options
    | GENERATE IDENTITY
    | query_optimization_options
    ;

online_options
    : ALLOW (NO | READ | WRITE) ACCESS
    ;

query_optimization_options
    : (ALLOW QUERY OPTIMIZATION)? USING REFRESH DEFERRED TABLES (WITH REFRESH AGE ANY)?
    ;

check_options
    : incremental_options exception_clause
    ;

incremental_options
    : NOT? INCREMENTAL
    ;

exception_clause
    : FOR EXCEPTION in_table_use_clause (',' in_table_use_clause)*
    ;

in_table_use_clause
    : IN table_name USE table_name
    ;

table_unchecked_options
    : integrity_options full_access? (',' integrity_options full_access?)*
    ;

full_access
    : FULL ACCESS
    ;

integrity_options
    : ALL
    | integrity_options_item (',' integrity_options_item)*
    ;

integrity_options_item
    : FOREIGN KEY
    | CHECK
    | MATERIALIZED QUERY
    | GENERATED COLUMN
    | STAGING
    ;

var_def_list
    : var_def (',' var_def)*
    ;

var_def
    : target_variable EQ expr_null_default
    | '(' target_variable (',' target_variable)* ')' EQ? ('(' expr_null_default (',' expr_null_default)* ')'  | '(' row_fullselect ')')
    ;

expr_null
    : expression
    | NULL_
    ;

expr_null_default
    : expression
    | NULL_
    | DEFAULT
    ;

array_index
    : todo
    ;

row_fullselect
    : todo
    ;

target_variable
    : global_variable_name
    | host_variable
    | parameter_marker
    | sql_parameter_name
    | field_reference
    | (sql_variable_name | transition_variable_name) attribute_name*
    ;

target_cursor_variable
    : todo
    ;

target_row_variable
    : global_variable_name
    | parameter_marker
    | sql_parameter_name
    | sql_variable_name
    | row_array_element_specification
    | row_field_reference
    ;

row_array_element_specification
    : todo
    ;

row_field_reference
    : todo
    ;

field_reference
    : row_variable_name '.' field_name
    ;

search_condition
    : todo
    ;

row_expression
    : todo
    ;

path_opt_list
    : path_opt (',' path_opt)*
    ;

path_opt
    : schema_name
    | SYSTEM PATH
    | USER
    | (CURRENT PATH | CURRENT_PATH)
    | CURRENT PACKAGE PATH
    | host_variable
    | string_constant
    ;

pkg_opt_list
    : pkg_opt (',' pkg_opt)*
    ;

pkg_opt
    : schema_name
    | CURRENT ( PACKAGE? PATH
              | USER
              )
    | CURRENT_PATH
    | CURRENT_USER
    | SESSION_USER
    | SYSTEM_USER
    | USER
    | host_variable
    | string_constant
    ;

maintain_opt_list
    : maintain_opt (',' maintain_opt)*
    ;

maintain_opt
    : FEDERATED_TOOL
    | SYSTEM
    | USER
    | REPLICATION
    | CURRENT MAINTAINED TABLE? TYPES (FOR OPTIMIZATION)?
    ;

variable
    : todo
    ;

host_variable
    : todo // :hv1
    ;

transfer_ownership
    : TRANSFER OWNERSHIP OF objects TO new_owner ((REVOKE | PRESERVE) PRIVILEGES)?
    ;

objects
    : alias_designator
    | CONSTRAINT table_name '.' constraint_name
    | DATABASE PARTITION GROUP db_partition_group_name
    | EVENT MONITOR event_monitor_name
    | function_designator
    | FUNCTION MAPPING function_mapping_name
    | INDEX index_name
    | INDEX EXTENSION index_extension_name
    | method_designator
    | NICKNAME nick_name
    | PACKAGE package_name (VERSION? version_id)?
    | procedure_designator
    | SCHEMA schema_name
    | SEQUENCE sequence_name
    | TABLE table_name
    | TABLE HIERARCHY root_table_name
    | TABLESPACE tablespace_name
    | TRIGGER trigger_name
    | DISTINCT? TYPE type_name
    | TYPE MAPPING type_mapping_name
    | VARIABLE variable_name
    | VIEW view_name
    | VIEW HIERARCHY root_view_name
    | XSROBJECT xsrobject_name
    ;

whenever
    : ( NOT FOUND
      | SQLERROR
      | SQLWARNING) ( CONTINUE
                    | (GOTO | GO TO) ':' host_label
                    | DO ( function_name
                         | BREAK
                         | CONTINUE
                         )
                    )
    ;

for
    : FOR
    ;

goto
    : GOTO label
    ;

if
    : IF search_condition THEN sql_routine_statement
        (ELSEIF search_condition THEN sql_routine_statement)*
        (ELSE sql_routine_statement)?
        END IF
    ;

include
    : INCLUDE (SQLCA | SQLDA | name)
    ;

iterate
    : ITERATE label
    ;

leave
    : LEAVE label
    ;

loop
    : (label ':')? LOOP sql_routine_statement END LOOP label?
    ;

open
    : OPEN
    ;

prepare
    : PREPARE
    ;

repeat
    : REPEAT
    ;

return
    : RETURN (expression | NULL_ | (WITH common_table_expression (',' common_table_expression)*)? fullselect)?
    ;

while
    : WHILE
    ;

sql_routine_statement
    : todo
    ;

common_table_expression
    : todo
    ;


// CREATE
create_command
    : create_alias
    | create_audit_policy
    | create_bufferpool
    | create_database_partition_group
    | create_event_monitor_activities
    | create_event_monitor_change_history
    | create_event_monitor_locking
    | create_event_monitor_package_cache
    | create_event_monitor_statistics
    | create_event_monitor_threshold
    | create_event_monitor_unit_of_work
    | create_external_table
    | create_function
    | create_function_aggregate_interface
    | create_function_external_scalar
    | create_function_external_table
    | create_function_old_db_external_function
    | create_function_sourced_or_template
    | create_function_sql_scalar_table_or_row
    | create_function_mapping
    | create_global_temporary_table
    | create_histogram_template
    | create_index
    | create_index_extension
    | create_mask
    | create_method
    | create_module
    | create_nickname
    | create_permission
    | create_procedure
    | create_procedure_external
    | create_procedure_sourced
    | create_procedure_sql
    | create_role
    | create_schema
    | create_security_label_component
    | create_security_label
    | create_security_policy
    | create_sequence
    | create_service_class
    | create_server
    | create_stogroup
    | create_synonym
    | create_table
    | create_tablespace
    | create_threshold
    | create_transform
    | create_trigger
    | create_trusted_context
    //| create_type
    | create_type_array
    | create_type_cursor
    | create_type_distinct
    | create_type_row
    | create_type_structured
    | create_type_mapping
    | create_usage_list
    | create_user_mapping
    | create_variable
    | create_view
    | create_work_action_set
    | create_work_class_set
    | create_workload
    | create_wrapper
    ;

create_alias
    : CREATE or_replace? PUBLIC? ALIAS (table_alias | module_alias | sequence_alias)
    ;

table_alias
    : alias_name FOR TABLE? (table_name | view_name | nick_name | alias_name)
    ;

module_alias
    : alias_name FOR MODULE (module_name | alias_name)
    ;

sequence_alias
    : alias_name FOR SEQUENCE (sequence_name | alias_name)
    ;

or_replace
    : OR REPLACE
    ;

create_audit_policy
    : CREATE AUDIT POLICY policy_name audit_policy_opts+
    ;

audit_policy_opts
    : CATEGORIES audit_policy_categories_opts (',' audit_policy_categories_opts)*
    | ERROR TYPE (NORMAL | AUDIT)
    ;

audit_policy_categories_opts
    : ( ALL
      | AUDIT
      | CHECKING
      | CONTEXT
      | EXECUTE (with_without DATA)?
      | OBJMAINT
      | SECMAINT
      | SYSADMIN
      | VALIDATE
      ) STATUS (BOTH | FAILURE | NONE | SUCCESS)
    ;

create_bufferpool
    : CREATE BUFFERPOOL bufferpool_name
        (IMMEDIATE | DEFERRED)?
        (ALL DBPARTITIONNUMS | DATABASE PARTITION GROUP db_partition_group_name (',' db_partition_group_name)*)?
        (SIZE number_of_pages? AUTOMATIC?)?
        bufferpool_opts
    ;

bufferpool_opts
    : except_clause
    | NUMBLOCKPAGES number_of_pages (BLOCKSIZE number_of_pages)?
    | PAGESIZE integer_value K?
    ;

except_clause
    : EXCEPT ON (MEMBER | MEMBERS) '(' member_list ')'
    ;

member_list
    : member_list_item (',' member_list_item)*
    ;

member_list_item
    : member_number (TO member_number)? SIZE number_of_pages
    ;

create_database_partition_group
    : CREATE DATABASE PARTITION GROUP db_partition_group_name
        ( ON ALL DBPARTITIONNUMS
        | ON (DBPARTITIONNUMS | DBPARTITIONNUM) '(' db_partition_number_list ')'
        )?
    ;

create_event_monitor_activities
    : CREATE EVENT MONITOR event_monitor_name FOR ACTIVITIES
        WRITE TO (TABLE formatted_event_table_info_3* | PIPE pipe_name | FILE path_name file_options)
    ;

formatted_event_table_info_3
    : evm_group ('(' target_table_options (','? target_table_options)* ')')?
    | BLOCKED
    ;

create_event_monitor_change_history
    : CREATE EVENT MONITOR event_monitor_name FOR CHANGE HISTORY WHERE EVENT IN '(' event_control_list? ')'
        WRITE TO TABLE formatted_event_table_info
        autostart_manualstart?
    ;

event_control_list
    : event_control (',' event_control)*
    ;

event_control
    : ALL
    | ADC
    | BACKUP
    | CFGALL
    | DBCFG
    | DBCFGVALUES
    | DBMCFG
    | CBMCFGVALUES
    | DDLALL
    | DDLDATA
    | DDLFEDERATED
    | DDLMONITOR
    | DDLSECURITY
    | DDLSQL
    | DDLSTORAGE
    | DDLWLM
    | DDLXML
    | LOAD
    | MOVETABLE
    | ONLINERECOVERY
    | REDISTRIBUTE
    | REGVAR
    | REGVARVALUES
    | REORG
    | RESTORE
    | ROLLFORWARD
    | RUNSTATS
    | UTILALL
    ;

create_event_monitor_locking
    : CREATE EVENT MONITOR event_monitor_name FOR LOCKING
        WRITE TO (TABLE formatted_event_table_info | UNFORMATTED EVENT TABLE ('(' target_table_options ')')?)
        autostart_manualstart?
    ;

create_event_monitor_package_cache
    : CREATE EVENT MONITOR event_monitor_name FOR PACKAGE CACHE filter_and_collection_options
        WRITE TO (TABLE formatted_event_table_info | UNFORMATTED EVENT TABLE ('(' target_table_options ')')?)
        autostart_manualstart?
    ;

filter_and_collection_options
    : (WHERE event_condition)? (COLLECT (BASE | DETAILED)? DATA)?
    ;

event_condition
    : event_condition_item (AND event_condition_item)*
    ;

event_condition_item
    : UPDATED_SINCE_BOUNDARY_TIME
    | (NUM_EXECUTIONS | STMT_EXEC_TIME) ('>' | '<' | '<=' | '=' | '>=') integer_constant
    ;

create_event_monitor_statistics
    : CREATE EVENT MONITOR event_monitor_name FOR STATISTICS
        WRITE TO (TABLE formatted_event_table_info_2 | PIPE pipe_name | FILE path_name file_options)
            event_monitor_statistics_opts
    ;

event_monitor_statistics_opts
    : (AUTOSTART | MANUALSTART)
    | ON MEMBER member_number
    | LOCAL
    ;

create_event_monitor_threshold
    : CREATE EVENT MONITOR event_monitor_name FOR THRESHOLD VIOLATIONS
        WRITE TO (TABLE formatted_event_table_info_2* | PIPE pipe_name | FILE path_name file_options)
        event_monitor_threshold_opts*
    ;

formatted_event_table_info_2
    : evm_group ('(' target_table_options (','? target_table_options)* ')')?
    | BUFFERSIZE pages
    | (BLOCKED | NONBLOCKED)
    ;

file_options
    : MAXFILES (NONE | number_of_files)
    | MAXFILESIZE (pages | NONE)
    | BUFFERSIZE pages
    | (BLOCKED | NONBLOCKED)
    | (APPEND | REPLACE)
    ;

event_monitor_threshold_opts
    : autostart_manualstart
    | ON MEMBER member_number
    | LOCAL
    ;

pages
    : integer_value
    ;

create_event_monitor_unit_of_work
    : CREATE EVENT MONITOR event_monitor_name FOR UNIT OF WORK
        WRITE TO (TABLE formatted_event_table_info? | UNFORMATTED EVENT TABLE ('(' target_table_options ')')?)
        autostart_manualstart?
    ;

formatted_event_table_info
    : evm_group ('(' target_table_options (','? target_table_options)* ')')?
    ;

autostart_manualstart
    : AUTOSTART
    | MANUALSTART
    ;

evm_group
    : todo
    ;

target_table_options
    : TABLE table_name
    | IN tablespace_name
    | PCTDEACTIVATE integer_value
    ;

create_external_table
    : CREATE EXTERNAL TABLE table_name ('(' column_definition (',' column_definition)* ')' | LIKE (table_name | view_name | nick_name))
        USING '(' ext_table_option ext_table_option_value (',' ext_table_option ext_table_option_value)* ')'
    ;

ext_table_option
    : id_
    ;

ext_table_option_value
    : id_
    | string
    | todo
    ;

create_function
    : todo
    ;

create_function_aggregate_interface
    : CREATE or_replace? FUNCTION function_name '(' agg_fn_param_decl (',' agg_fn_param_decl)* ')'
        ( RETURNS (data_type_2 | data_type_2 CAST FROM data_type_2) agg_fn_option_list
        | AGGREGATE WITH '(' state_variable_declaration (',' state_variable_declaration)* ')' USING
            (IN MODULE module_name)? INITIALIZE procedure_designator ACCUMULATE procedure_designator
                MERGE procedure_designator FINALIZE function_designator
        ) todo
    ;

agg_fn_param_decl
    : (IN? parameter_name)? data_type_1 default_clause?
    ;

agg_fn_option_list
    :
    ;

state_variable_declaration
    :
    ;

create_function_external_scalar
    : CREATE or_replace? FUNCTION function_name '(' ext_scalar_param_decl (',' ext_scalar_param_decl)* ')'
        RETURNS (data_type_2 as_locator? | data_type_3 CAST FROM data_type_2 as_locator?)
        ext_scalar_option_list
    ;

ext_scalar_param_decl
    : (in_out_inout? parameter_name)? data_type default_clause?
    ;

ext_scalar_option_list
    : ext_scalar_option_list_item+
    ;

ext_scalar_option_list_item
    : LANGUAGE (C_ | JAVA | CLR | OLE | CPP | PYTHON)
    | SPECIFIC specific_name
    | EXTERNAL (NAME (string | id_))?
    | PARAMETER STYLE (DB2GENERAL | JAVA | SQL | NPSGENERIC)
    | PARAMETER CCSID ascii_unicode
    | NOT? DETERMINISTIC
    | FENCED (NOT? THREADSAFE)?
    | NOT FENCED THREADSAFE?
    | RETURNS NULL_ ON NULL_ INPUT
    | CALLED ON NULL_ INPUT
    | READS SQL DATA
    | NO SQL
    | CONTAINS SQL
    | STATIC DISPATCH
    | NO? EXTERNAL ACTION
    | NO SCRATCHPAD
    | SCRATCHPAD length
    | NO? FINAL CALL
    | allow_disallow PARALLEL
    | NO? DBINFO
    | TRANSFORM GROUP group_name
    | PREDICATES '(' predicate_specification ')'
    | INHERIT SPECIAL REGISTERS
    | NOT? SECURED
    | STAY RESIDENT NO
    ;

predicate_specification
    : WHEN ('=' | '<>' | '<' | '>' | '<=' | '>=') (constant_ | EXPRESSION AS expression_name)
        (data_filter index_exploitation? | index_exploitation data_filter?)
    ;

data_filter
    : FILTER USING (function_invocation | case_expression)
    ;

function_invocation
    : todo
    ;

index_exploitation
    : SEARCH BY EXACT? INDEX EXTENSION index_extension_name exploitation_rule+
    ;

exploitation_rule
    : WHEN KEY '(' parameter_name ')' USE search_method_name '(' parameter_name (',' parameter_name)* ')'
    ;

create_function_external_table
    : CREATE or_replace? FUNCTION function_name '(' ext_table_param_decl_list ')'
        RETURNS (TABLE '(' column_name data_type_2 as_locator? (',' column_name data_type_2 as_locator?)* ')' | GENERIC TABLE)
        ext_table_option_list
    ;

ext_table_param_decl_list
    : ext_table_param_decl (',' ext_table_param_decl)*
    ;

ext_table_param_decl
    : parameter_name? data_type default_clause? as_locator?
    ;

ext_table_option_list
    : ext_table_option_list_item+
    ;

ext_table_option_list_item
    : LANGUAGE (C_ | JAVA | CLR | OLE | CPP)
    | SPECIFIC specific_name
    | EXTERNAL (NAME (string | id_))?
    | PARAMETER STYLE (DB2GENERAL | SQL | NPSGENERIC)
    | PARAMETER CCSID ascii_unicode
    | NOT? DETERMINISTIC
    | FENCED (NOT? THREADSAFE)?
    | NOT FENCED THREADSAFE?
    | RETURNS NULL_ ON NULL_ INPUT
    | CALLED ON NULL_ INPUT
    | READS SQL DATA
    | NO SQL
    | CONTAINS SQL
    | STATIC DISPATCH
    | NO? EXTERNAL ACTION
    | NO SCRATCHPAD
    | SCRATCHPAD length
    | NO? FINAL CALL
    | DISALLOW PARALLEL
    | ALLOW PARALLEL EXECUTE ON ALL (DATABASE PARTITIONS)? RESULT TABLE DISTRIBUTED
    | NO? DBINFO
    | CARDINALITY integer_value
    | TRANSFORM GROUP group_name
    | INHERIT SPECIAL REGISTERS
    | NOT? SECURED
    | STAY RESIDENT NO
    ;

create_function_old_db_external_function
    : CREATE FUNCTION function_name '(' param_decl_list_3 ')'
        RETURNS TABLE '(' column_name data_type_2 (',' column_name data_type_2)* ')' oledb_option_list
    ;

oledb_option_list
    : oledb_option_list_item+
    ;

oledb_option_list_item
    : LANGUAGE OLEDB
    | SPECIFIC specific_name
    | EXTERNAL NAME string
    | NOT? DETERMINISTIC
    | STATIC DISPATCH
    | RETURNS NULL_ ON NULL_ INPUT
    | CALLED ON NULL_ INPUT
    | CARDINALITY integer_value
    | NOT? SECURED
    | NO? EXTERNAL ACTION
    ;

create_function_sourced_or_template
    : CREATE FUNCTION function_name '(' param_decl_list_3 ')'
        fn_return_opts?
    ;

fn_return_opts
    : fn_return_opts_item+
    ;

fn_return_opts_item
    : RETURNS data_type_2
    | SPECIFIC specific_name
    | ( SOURCE (function_name | SPECIFIC specific_name | function_name '(' data_type_list? ')') (PARAMETER CCSID ascii_unicode)?
      | AS TEMPLATE template_opts?
      )
    ;

template_opts
    : template_opts_item+
    ;

template_opts_item
    : NOT? DETERMINISTIC
    | NO? EXTERNAL ACTION
    ;

ascii_unicode
    : ASCII
    | UNICODE
    ;

param_decl_list_3
    : param_decl_3 (',' param_decl_3)*
    ;

param_decl_3
    : parameter_name data_type default_clause?
    ;

create_function_sql_scalar_table_or_row
    : CREATE or_replace? FUNCTION function_name '(' param_decl_list_2 ')'
        RETURNS ( data_type_2
                | ROW column_list
                | TABLE (column_list | row_type_name | anchored_row_data_type | ELEMENT OF array_type_name)
                ) option_list sql_function_body
    ;

param_decl_list_2
    : param_decl_2 (',' param_decl_2)*
    ;

param_decl_2
    : in_out_inout? parameter_name data_type default_clause?
    ;

sql_function_body
    : RETURN
    | compound_sql_compiled
    | compound_sql_inlined
    ;

create_function_mapping
    : CREATE FUNCTION MAPPING function_mapping_name? FOR (function_name '(' data_type_list ')' | SPECIFIC specific_name)
        (SERVER server_name | SERVER TYPE server_type (VERSION server_version (WRAPPER wrapper_name)?)?)
        function_options? (WITH INFIX)?
    ;

function_options
    : OPTIONS '(' function_option_name string_constant (',' function_option_name string_constant)* ')'
    ;

function_option_name
    : id_
    ;

create_global_temporary_table
    : CREATE GLOBAL TEMPORARY TABLE table_name
          ( '(' column_definition (',' todo)* ')'
          | LIKE (table_name | view_name) copy_options?
          | AS '(' fullselect ')' WITH NO DATA copy_options? todo
          )
          create_global_temporary_table_opts?
    ;

create_global_temporary_table_opts
    : create_global_temporary_table_item+
    ;

create_global_temporary_table_item
    : ON COMMIT delete_preserve ROWS
    | NOT LOGGED ON ROLLBACK delete_preserve ROWS
    | LOGGED
    | IN tablespace_name
    | distribution_clause
    ;

delete_preserve
    : DELETE
    | PRESERVE
    ;

create_histogram_template
    : CREATE HISTOGRAM TEMPLATE template_name HIGH BIN VALUE bigint_constant
    ;

create_index
    : CREATE UNIQUE? INDEX index_name ON (table_name | nick_name) '(' index_col_opts ')' todo
    ;

index_col_opts
    : index_col_opts_item (',' index_col_opts_item)*
    ;

index_col_opts_item
    : (column_name | key_expression) (ASC | DESC | RANDOM)
    | BUSINESS_TIME WITHOUT OVERLAPS
    ;

key_expression
    : todo
    ;

create_index_extension
    : CREATE INDEX EXTENSION index_extension_name param_list? index_maintenance index_search
    ;

param_list
    : parameter_name data_type (',' parameter_name data_type)*
    ;

index_maintenance
    : FROM SOURCE KEY '(' parameter_name data_type ')' GENERATE KEY USING table_function_invocation
    ;

table_function_invocation
    : todo
    ;

index_search
    : WITH TARGET KEY '(' param_list ')' SEARCH METHOD search_method_definition (',' search_method_definition)*
    ;

search_method_definition
    : WHEN method_name '(' param_list ')' RANGE THROUGH range_producing_funciton_invocation
        (FILTER USING (index_filtering_function_invocation | case_expression))?
    ;

create_mask
    : CREATE or_replace? MASK mask_name ON table_name (AS? correlation_name)? FOR COLUMN column_name RETURN case_expression enable_disable?
    ;

case_expression
    : todo
    ;

range_producing_funciton_invocation
    : todo
    ;

index_filtering_function_invocation
    : todo
    ;

create_method
    : CREATE ( METHOD (method_name | method_signature) FOR type_name
             | SPECIFIC METHOD specific_name
             )
             ( method_opts?
             | INHERIT ISOLATION LEVEL with_without LOCK REQUEST sql_method_body
             )
    ;

method_opts
    : method_opts_item+
    ;

method_opts_item
    : EXTERNAL (NAME (string | id_))?
    | TRANSFORM GROUP group_name
    ;

method_signature
    : method_name '(' method_param_list? ')' (RETURNS (data_type_2 as_locator? | data_type_3 CAST FROM data_type_4 as_locator?))?
    ;

method_param_list
    : parameter_name? data_type_1 as_locator? (',' parameter_name? data_type_1 as_locator?)*
    ;

data_type_3
    : data_type
    ;

data_type_4
    : data_type
    ;

sql_method_body
    : RETURN
    | compound_sql_inlined
    ;

compound_sql_inlined
    : (label ':')? BEGIN (NOT? ATOMIC)?
todo
        END (label ':')?
    ;

sql_statement_inlined
    : call
    | for
    | (WITH cte (',' cte)*)? fullselect
    | get_diagnostics
    | if
    | insert
    | iterate
    | leave
    | merge
    | return
//    | searched_delete
//    | searched_update
    | set
//    | signal
    | while
    ;

compound_sql_compiled
    : BEGIN COMPOUND NOT? ATOMIC STATIC (STOP AFTER FIRST host_variable STATEMENTS)?
        (sql_statement_compiled ';')* END COMPOUND
    ;

sql_statement_compiled
    : todo //all except
    ;
/*
  CALL                FETCH
  CLOSE               OPEN
  CONNECT             PREPARE
  Compound SQL        RELEASE (Connection)
  DESCRIBE            ROLLBACK
  DISCONNECT          SET CONNECTION
  EXECUTE IMMEDIATE   SET variable
*/


create_module
    : CREATE or_replace? MODULE module_name
    ;

create_nickname
    : CREATE or_replace? NICKNAME nick_name (FOR remote_object_name | non_relational_data_definition)
        (OPTIONS '(' nick_name_option_name string_constant (',' nick_name_option_name string_constant)* ')')?
    ;

nick_name_option_name
    : id_
    ;

remote_object_name
    : todo
    ;

non_relational_data_definition
    : nick_name_column_list FOR SERVER server_name
    ;

nick_name_column_list
    : '(' nick_name_column_list_item (',' nick_name_column_list_item)* ')'
    ;

nick_name_column_list_item
    : nick_name_column_definition
    | unique_constraint
    | referential_constraint
    | check_constraint
    ;

nick_name_column_definition
    : column_name local_data_type nick_name_column_options*
    ;

nick_name_column_options
    : NOT NULL_
    | (CONSTRAINT constraint_name)? ( (PRIMARY KEY | UNIQUE) constraint_attributes
                                    | references_clause
                                    | CHECK '(' check_condition ')' constraint_attributes
                                    )
    | federated_column_options
    ;

federated_column_options
    : OPTIONS '(' column_option_name string_constant (',' column_option_name string_constant)* ')'
    ;

column_option_name
    : id_
    ;

create_permission
    : CREATE or_replace? PERMISSION permission_name ON table_name (AS? correlation_name)?
        FOR ROWS WHERE search_condition ENFORCED FOR ALL ACCESS enable_disable?
    ;

create_procedure
    : todo
    ;

create_procedure_external
    : CREATE or_replace? PROCEDURE procedure_name proc_ext_param_list? option_list_2?
    ;

proc_ext_param_list
    : '(' proc_ext_param (',' proc_ext_param)* ')'
    ;

proc_ext_param
    : in_out_inout parameter_name? data_type default_clause?
    ;

option_list_2
    : option_list_2_item+
    ;

option_list_2_item
    : LANGUAGE (C_ | JAVA | COBOL | CLR | OLE)
    | SPECIFIC specific_name
    | DYNAMIC RESULT SETS integer_value
    | (MODIFIES | READS) SQL DATA
    | (NO | CONTAINS) SQL
    | NOT? DETERMINISTIC
    | CALLED ON NULL_ INPUT
    | old_new SAVEPOINT LEVEL
    | EXTERNAL (NAME (string | id_))?
    | FENCED NOT? THREADSAFE
    | NOT FENCED THREADSAFE?
    | COMMIT ON RETURN yes_no
    | AUTONOMOUS
    | NO? EXTERNAL ACTION
    | INHERIT SPECIAL REGISTERS
    | PARAMETER STYLE (DB2GENERAL | DB2SQL | GENERAL | GENERAL WITH RULES | JAVA | SQL)
    | PARAMETER CCSID (ASCII | UNICODE)
    | PROGRAM TYPE (SUB | MAIN)
    | NO? DBINFO
    | STAY RESIDENT NO
    ;

create_procedure_sourced
    : CREATE or_replace? PROCEDURE procedure_name source_procedure_clause option_list_1?
    ;

source_procedure_clause
    : SOURCE source_object_name ('(' ')' | NUMBER OF PARAMETERS integer_value)?
        (UNIQUE ID unique_id)? FOR SERVER server_name
    ;

source_object_name
    : source_schema_name '.' (source_package_name '.')? source_procedure_name
    ;

option_list_1
    : option_list_1_item+
    ;

option_list_1_item
    : SPECIFIC specific_name
    | WITH RETURN TO CALLER ALL
    | WITH RETURN TO ('(' result_set_element_number (',' result_set_element_number)* ')' | ALL)
    | (NO SQL | CONTAINS SQL | (MODIFIES | READS) SQL DATA)
    | NOT? DETERMINISTIC
    | NO? EXTERNAL ACTION
    ;

result_set_element_number
    : integer_constant
    ;

unique_id
    : string_constant
    ;

create_procedure_sql
    : CREATE or_replace? PROCEDURE procedure_name ('(' proc_parameter_list? ')')? option_list? sql_procedure_body
    ;

proc_parameter_list
    : proc_parameter_list_item (',' proc_parameter_list_item)*
    ;

proc_parameter_list_item
    : in_out_inout? parameter_name data_type default_clause?
    ;

in_out_inout
    : IN
    | OUT
    | INOUT
    ;

option_list
    : option_list_item+
    ;

option_list_item
    : LANGUAGE SQL
    | SPECIFIC specific_name
    | DYNAMIC RESULT SETS integer_value
    | (MODIFIES | READS) SQL DATA
    | CONTAINS SQL
    | NOT? DETERMINISTIC
    | CALLED ON NULL_ INPUT
    | COMMIT ON RETURN yes_no
    | AUTONOMOUS
    | INHERIT SPECIAL REGISTERS
    | old_new SAVEPOINT LEVEL
    | NO? EXTERNAL ACTION
    | PARAMETER CCSID (ASCII | UNICODE)
    ;

sql_procedure_body
    : sql_procedure_statement
    ;

create_role
    : CREATE ROLE role_name
    ;

create_schema
    : CREATE SCHEMA (schema_name | AUTHORIZATION authorization_name | schema_name AUTHORIZATION authorization_name)
            (DATA CAPTURE (NONE | CHANGES))?
            schema_sql_statement*
    ;

schema_sql_statement
    : create_table
    | create_view
    | create_index
    | comment
    | grant_statement
    ;

create_security_label_component
    : CREATE SECURITY LABEL COMPONENT component_name (array_clause | set_clause | tree_clause)
    ;

array_clause
    : ARRAY '[' string_constant (',' string_constant)* ']'
    ;

set_clause
    : SET '{' string_constant (',' string_constant)* '}'
    ;

tree_clause
    : TREE '(' string_constant ROOT tree_clause_item* ')'
    ;

tree_clause_item
    : ',' string_constant UNDER string_constant
    ;

create_security_label
    : CREATE SECURITY LABEL security_label_name create_security_label_item (',' create_security_label_item)*
    ;

create_security_label_item
    : COMPONENT component_name string_constant (',' string_constant)*
    ;

create_security_policy
    : CREATE SECURITY POLICY security_policy_name COMPONENTS  component_name (',' component_name)*
        WITH DB2LBACRULES ((OVERRIDE_ | RESTRICT) NOT AUTHORIZED WRITE SECURITY LABEL)?
    ;

create_sequence
    : CREATE or_replace? SEQUENCE sequence_name (AS (INTEGER | data_type))?
        create_sequence_opts?
    ;

create_sequence_opts
    : create_sequence_opts_item+
    ;

create_sequence_opts_item
    : START WITH numeric_constant
    | INCREMENT BY numeric_constant
    | (MINVALUE numeric_constant | NO MINVALUE)
    | (MAXVALUE numeric_constant | NO MAXVALUE)
    | NO? CYCLE
    | (CACHE numeric_constant | NO CACHE)
    | NO? ORDER
    ;

create_service_class
    : CREATE SERVICE CLASS_ service_class_name (UNDER service_superclass_name)?
        (FOR WORKLOAD TYPE (CUSTOM | BATCH | INTERACTIVE | MIXED))?
        (soft_hard? RESOURCE SHARES integer_constant)?
        (soft_hard? CPU SHARES integer_constant)?
        (CPU LIMIT (integer_constant | NONE))?
        (MINIMUM RESOURCE SHARE integer_constant PERCENT)?
        (ADMISSION QUEUE ORDER (FIFO | LATENCY))?
        (DEGREE_ SCALEBACK (DEFAULT | on_off))?
        (MAXIMUM DEGREE_ (DEFAULT | NONE | degree))?
        (PREFETCH PRIORITY (DEFAULT | high_medium_low))?
        (OUTBOUND CORRELATOR (NONE | string_constant))?
        (BUFFERPOOL PRIORITY (DEFAULT | high_medium_low))?
        (COLLECT AGGREGATE ACTIVITY DATA (NONE | BASE | EXTENDED)?)? //todo TEST
        (COLLECT AGGREGATE REQUEST DATA (NONE | BASE)?)? //todo TEST
        (COLLECT AGGREGATE UNIT OF WORK DATA (NONE | BASE)?)? //todo TEST
        (COLLECT REQUEST METRICS (NONE | BASE | EXTENDED)?)?
        histogram_templace_clause enable_disable?
    ;

high_medium_low
    : HIGH
    | MEDIUM
    | LOW
    ;

on_off
    : ON
    | OFF
    ;

soft_hard
    : SOFT
    | HARD
    ;

create_server
    : CREATE SERVER server_name (TYPE server_type)? (VERSION server_version) WRAPPER wrapper_name
        (AUTHORIZATION authorization_name PASSWORD password_)?
        (OPTIONS '(' server_option_name string_constant (',' server_option_name string_constant)* ')')?
    ;

password_
    : todo // "pass"
    ;

create_stogroup
    : CREATE STOGROUP storagegroup_name ON string (',' string)* create_stogroup_opts*
    ;

create_stogroup_opts
    : OVERHEAD number_of_milliseconds
    | DEVICE READ RATE number_megabytes_per_second
    | DATA TAG (integer_constant | NONE)
    | SET AS DEFAULT
    ;

create_synonym
    : CREATE //todo as alias
    ;

create_table
    : CREATE TABLE if_not_exists? table_name
        ( element_list
        | OF type_name typed_table_options?
        | LIKE (table_name | view_name | nick_name) copy_options?
        | as_result_table copy_options?
        | materialized_query_options
        | staging_table_definition
        )
        (ORGANIZED BY (ROW | COLUMN | (ROW USING)? (dimensions_clause | KEY SEQUENCE sequence_key_spec | INSERT TIME)))?
        create_table_opts+
    ;

create_table_opts
    : DATA CAPTURE (NONE | CHANGES)
    | tablespace_clauses
    | distribution_clause
    | partitioning_clause
    | COMPRESS (YES ADAPTIVE? | YES STATIC | NO)
    | VALUE COMPRESSION
    | WITH RESTRICT ON DROP
    | NOT LOGGED INITIALLY
    | CCSID (ASCII | UNICODE)
    | SECURITY POLICY policy_name
    | OPTIONS '(' table_option_list ')'
    ;

table_option_list
    : table_option_list_item (',' table_option_list_item)*
    ;

table_option_list_item
    : table_option_name string_constant
    ;

table_option_name
    : string
    ;

element_list
    : element_list_item (',' element_list_item)*
    ;

element_list_item
    : column_definition
    | period_definition
    | unique_constraint
    | referential_constraint
    | check_constraint
    ;

column_definition
    : column_name data_type? column_options? //todo dt
    ;

period_definition
    : PERIOD (SYSTEM_TIME | BUSINESS_TIME) '(' column_name ',' column_name ')'
    ;

unique_constraint
    : (CONSTRAINT constraint_name)? (UNIQUE | PRIMARY KEY) '(' column_list (',' BUSINESS_TIME WITHOUT OVERLAPS)? ')' constraint_attributes?
    ;

referential_constraint
    : (CONSTRAINT constraint_name)? FOREIGN KEY '(' column_list ')' references_clause
    ;

check_constraint
    : (CONSTRAINT constraint_name)? CHECK '(' check_condition ')' constraint_attributes?
    ;

column_options
    : column_options_item //todo ?
    ;

column_options_item
    : NOT NULL_
    | lob_options
    | SCOPE (typed_table_name | typed_view_name)
    | (CONSTRAINT constraint_name)? ( (PRIMARY KEY | UNIQUE)
                                    | references_clause
                                    | CHECK '(' check_condition ')'
                                    ) constraint_attributes?
    | (default_clause | generated_clause)
    | INLINE LENGTH integer_value
    | COMPRESS SYSTEM DEFAULT
    | COLUMN? SECURED WITH security_label_name
    | (NOT | IMPLICITLY) HIDDEN_
    ;

//data_type

//built_in_type

references_clause
    : REFERENCES (table_name | nick_name) (column_list_paren)? rule_clause? constraint_attributes?
    ;

rule_clause
    : ON DELETE (NO ACTION | RESTRICT | CASCADE | SET NULL_)
    | ON UPDATE (NO ACTION | RESTRICT) //todo bullet
    ;

constraint_attributes
    : (ENFORCED | NOT ENFORCED (NOT? TRUSTED)?)
    | enable_disable QUERY OPTIMIZATION //TODO bullet
    ;

default_clause
    : WITH? DEFAULT default_values?
    ;

default_values
    : constant_
    | datetime_special_register
    | user_special_register
    | CURRENT (SCHEMA | MEMBER)
    | NULL_
    | cast_function '(' (constant_ | datetime_special_register | user_special_register) ')'
    | (EMPTY_CLOB | EMPTY_DBCLOB | EMPTY_NCLOB | EMPTY_BLOB) '(' ')'
    ;

generated_clause
    : GENERATED (ALWAYS | BY DEFAULT)? (identity_options | as_row_change_timestamp_clause)
    | GENERATED ALWAYS? (as_generated_expression_clause | as_row_transaction_timestamp_clause | as_row_transaction_start_id_clause)
    ;

datetime_special_register
    : CURRENT (DATE | TIME | TIMESTAMP)
    ;

user_special_register
    : CURRENT USER
    | SESSION_USER
    | SYSTEM_USER
    ;

cast_function
    : todo
    ;

identity_options
    : AS IDENTITY ('(' identity_options_item+ ')')?
    ;

identity_options_item
    : START WITH numeric_constant
    | INCREMENT BY numeric_constant
    | (MINVALUE numeric_constant | NO MINVALUE)
    | (MAXVALUE numeric_constant | NO MAXVALUE)
    | NO? CYCLE
    | (CACHE integer_constant | NO CACHE)
    | NO? ORDER
    ;

as_row_change_timestamp_clause
    : FOR EACH ROW ON UPDATE AS ROW CHANGE TIMESTAMP
    ;

as_generated_expression_clause
    : AS '(' generation_expression ')'
    ;

generation_expression
    : todo
    ;

as_row_transaction_timestamp_clause
    : AS ROW (BEGIN | END)
    ;

as_row_transaction_start_id_clause
    : AS TRANSACTION START ID
    ;

oid_column_definition
    : REF IS oid_column_name USE GENERATED
    ;

range_partition_spec
    : '(' partition_expression_list ')' '(' partition_element_list ')'
    ;

partition_expression_list
    : partition_expression (',' partition_expression)*
    ;

partition_expression
    : column_name (NULLS first_last)?
    ;

partition_element_list
    : partition_element (',' partition_element)*
    ;

partition_element
    : (PARTITION partition_name)? boundary_spec partition_tablespace_options?
    | boundary_spec EVERY ( '(' constant_ duration_label? ')'
                          | constant_ duration_label?
                          )
    ;

boundary_spec
    : starting_clause ending_clause
    ;

partition_tablespace_options
    : (IN tablespace_name)? (INDEX IN tablespace_name)? (LONG IN tablespace_name)?
    ;

duration_label
    : YEAR
    | YEARS
    | MONTH
    | MONTHS
    | DAY
    | DAYS
    | HOUR
    | HOURS
    | MINUTE
    | MINUTES
    | SECOND
    | SECONDS
    | MICROSECOND
    | MICROSECONDS
    ;

starting_clause
    : STARTING FROM? ('(' const_min_max_list ')' | const_min_max)
    ;

const_min_max_list
    : const_min_max (',' const_min_max)*
    ;

const_min_max
    : constant_
    | MINVALUE
    | MAXVALUE
    ;

ending_clause
    : ENDING AT? ('(' const_min_max_list ')' | const_min_max) (INCLUSIVE | EXCLUSIVE)?
    ;

typed_table_options
    : (HIERARCHY hierarchy_name | under_clause)? typed_element_list?
    ;

typed_element_list
    : '(' typed_element_list_item (',' typed_element_list_item)* ')'
    ;

typed_element_list_item
    : oid_column_name
    | with_options
    | unique_constraint
    | check_constraint
    ;

as_result_table
    : column_list_paren? AS '(' fullselect ')' WITH NO? DATA
    ;

copy_options
    : (INCLUDING | EXCLUDING) COLUMN? DEFAULTS
    ;

materialized_query_options
    : column_list_paren? AS '(' fullselect ')' WITH NO? DATA
    ;

staging_table_definition
    : column_list_paren? FOR table_name PROPAGATE IMMEDIATE
    ;

dimensions_clause
    : DIMENSIONS? '(' col_names (',' col_names)* ')'
    ;

col_names
    : column_name
    | column_list_paren
    ;

sequence_key_spec
    : '(' sequence_key_spec_list ')' (ALLOW | DISALLOW) OVERFLOW (PCTFREE integer_value)?
    ;

sequence_key_spec_list
    : sequence_key_spec_list_item (',' sequence_key_spec_list_item)*
    ;

sequence_key_spec_list_item
    : column_name (STARTING FROM? constant_)? ENDING AT? constant_
    ;

tablespace_clauses
    : IN? tablespace_name_list NO? CYCLE (INDEX IN tablespace_name)? (LONG IN tablespace_name_list)?
    ;

distribution_clause
    : DISTRIBUTE BY (HASH '(' column_list ')' | REPLICATION | RANDOM)
    ;

partitioning_clause
    : PARTITION BY RANGE? range_partition_spec
    ;

if_not_exists
    : IF NOT EXISTS
    ;

create_tablespace
    : CREATE (LARGE | REGULAR | (SYSTEM | USER)? TEMPORARY)? TABLESPACE tablespace_name
        (IN (DATABASE PARTITION GROUP)? db_partition_group_name)?
        (PAGESIZE integer_value K?)?
        ( MANAGED BY ( AUTOMATIC STORAGE storage_group? size_attributes?
                     | SYSTEM system_containers+
                     | DATABASE database_containers+ size_attributes?
                     )
        )?
        ( EXTENTSIZE ( number_of_pages
                     | integer_value (K | M)
                     )
        )?
        ( PREFETCHSIZE ( AUTOMATIC
                       | number_of_pages
                       | integer_value (K | M)
                       )?
        )?
        (BUFFERPOOL bufferpool_name)?
        (OVERHEAD (number_of_milliseconds | INHERIT))?
        (NO? FILE SYSTEM CACHING)?
        (TRANSFERRATE (number_of_milliseconds | INHERIT))?
        (DATA TAG (integer_constant | INHERIT | NONE))?
        (DROPPED TABLE RECOVERY (ON | OFF))?
    ;

storage_group
    : USING STOGROUP storagegroup_name
    ;

size_attributes
    : (AUTORESIZE yes_no)?
      (INITIALSIZE integer_value kmg)?
      (INCREASESIZE integer_value (PERCENT | kmg))?
      (MAXSIZE (NONE | integer_value kmg))?
    ;

system_containers
    : USING '(' container_string_list ')' on_db_partitions_clause?
    ;

container_string_list
    : string (',' string)*
    ;

database_containers
    : USING container_clause on_db_partitions_clause?
    ;

container_clause
    : '(' container_clause_list ')'
    ;

container_clause_list
    : container_clause_list_item (',' container_clause_list_item)*
    ;

container_clause_list_item
    : file_device string (number_of_pages | integer_value kmg)
    ;

on_db_partitions_clause
    : ON db_partition_num_nums '(' db_partition_number_list ')'
    ;

db_partition_number_list
    : db_partition_number_list_item (',' db_partition_number_list_item)*
    ;

db_partition_number_list_item
    : db_partition_number (TO db_partition_number)?
    ;

db_partition_number
    : integer_value
    ;

number_of_pages
    : integer_value
    ;

number_of_files
    : integer_value
    ;

number_of_milliseconds
    : integer_value
    ;

number_megabytes_per_second
    : integer_value
    ;

create_threshold
    : CREATE THRESHOLD threshold_name FOR threshold_domain ACTIVITIES?
     (ENFORCEMENT DEFAULT | ENFORCEMENT enforcement_scope) enable_disable WHEN threshold_predicate threshold_exceeded_actions_2
    ;

threshold_domain
    : DATABASE
    | SERVICE CLASS_ service_class_name (UNDER service_class_name)?
    | STATEMENT (TEXT statement_text | REFERENCE executable_id)
    | WORKLOAD workload_name
    ;

statement_text
    : todo
    ;

executable_id
    : todo
    ;

enforcement_scope
    : DATABASE
    | MEMBER
    | WORKLOAD OCCURENCE
    ;

threshold_predicate
    : TOTALMEMBERCONNECTIONS '>' integer_value
    | TOTALSCMEMBERCONNECTIONS '>' integer_value (AND QUEUEDCONNECTIONS ('>' integer_value | UNBOUNDED))?
    | CONNECTIONIDLETIME '>' integer_value day_to_minutes
    | CONCURRENTWORKLOADOCCURRENCES '>' integer_value
    | CONCURRENTWORKLOADACTIVITIES '>' integer_value
    | CONCURRENTDBCOORDACTIVITIES '>' integer_value (AND QUEUEDACTIVITIES ('>' integer_value | UNBOUNDED))?
    | ESTIMATEDSQLCOST '>' integer_value
    | SQLROWSRETURNED '>' integer_value
    | (ACTIVITYTOTALTIME | UOWTOTALTIME) '>' integer_value day_to_seconds
    | (SQLTEMPSPACE | AGGSQLTEMPSPACE) '>' kmg
    | (SQLROWSREAD | SQLROWSREADINSC) '>' bigint_value checking_every?
    | (CPUTIME | CPUTIMEINSC) '>' hour_to_seconds integer_value checking_every?
    | (ACTIVITYTOTALRUNTIME | ACTIVITYTOTALRUNTIMEINALLSC) '>' integer_value day_to_seconds
    | SORTSHRHEAPUTIL '>' integer_value PERCENT (AND BLOCKING ADMISSION FOR '>' integer_value day_to_seconds)?
    | DATATAGINSC (NOT? IN) '(' integer_constant_list ')'
    ;

checking_every
    : CHECKING EVERY integer_value second_seconds
    ;

hour_to_seconds
    : HOUR
    | HOURS
    | MINUTE
    | MINUTES
    | SECOND
    | SECONDS
    ;

day_to_minutes
    : DAY
    | DAYS
    | HOUR
    | HOURS
    | MINUTE
    | MINUTES
    ;

day_to_seconds
    : DAY
    | DAYS
    | HOUR
    | HOURS
    | MINUTE
    | MINUTES
    | SECONDS
    ;

threshold_exceeded_actions_2
    : (COLLECT ACTIVITY DATA (todo) )?
        (WITHOUT DETAILS | WITH details_section (AND VALUES)?)?
        (STOP EXECUTION | CONTINUE | FOR APPLICATION | remap_activity_action)
    ;

details_section
    : DETAILS (',' SECTION)?
    ;

remap_activity_action
    : REMAP ACTIVITY TO service_subclass_name ((NO | LOG) EVENT MONITOR RECORD)?
    ;

create_transform
    : CREATE (TRANSFORM | TRANSFORMS) FOR type_name tranform_list
    ;

tranform_list
    : tranform_list_item+
    ;

tranform_list_item
    : group_name '(' transform_group_list ')'
    ;

transform_group_list
    : transform_group_list_item (',' transform_group_list_item)*
    ;

transform_group_list_item
    : (TO | FROM) SQL WITH function_designator
    ;

create_trigger
    : CREATE or_replace? TRIGGER trigger_name ((NO CASCADE)? BEFORE | AFTER | INSTEAD OF) trigger_event ON (table_name | view_name)
    (
        REFERENCING ref_list
    )?
    FOR EACH (ROW | STATEMENT) (NOT? SECURED)? triggered_action
    ;

ref_list
    : ref_list_item+
    ;

ref_list_item
    : old_new AS? correlation_name
    | old_new TABLE AS? identifier
    ;

old_new
    : OLD
    | NEW
    ;

correlation_name
    : todo
    ;

identifier
    : todo
    ;

trigger_event
    : OR
    | INSERT
    | DELETE
    | UPDATE (OF column_list)?
    ;

triggered_action
    : (WHEN '(' search_condition ')')? label? sql_procedure_statement
    ;

sql_procedure_statement
    : CALL
    | FOR
    | IF
    | todo
    ;

create_trusted_context
    : CREATE TRUSTED CONTEXT context_name BASE UPON CONNECTION USING SYSTEM AUTHID authorization_name
        ( ATTRIBUTES '(' attr_list ')'
        | (NO DEFAULT ROLE | DEFAULT ROLE role_name)?
        | enable_disable?
        )
        (WITH USE FOR auth_list)?
    ;

attr_list
    : attr_list_item (',' attr_list_item)*
    ;

attr_list_item
    : ADDRESS address_value (WITH ENCRYPTION encryption_value)?
    | ENCRYPTION encryption_value
    ;

auth_list
    : auth_list_item (',' auth_list_item)*
    ;

auth_list_item
    : ( authorization_name (ROLE role_name)?
      | PUBLIC
      ) (with_without AUTHENTICATION)?
    ;

address_value
    : string_constant
    ;

encryption_value
    : string_constant
    ;

//create_type
//    : CREATE
//    ;

create_type_array
    : CREATE or_replace? TYPE type_name AS data_type ARRAY '[' (MAX_INT | integer_constant | data_type_2) ']'
    ;

create_type_cursor
    : CREATE or_replace? TYPE type_name (anchored_row_data_type | row_type_name)? CURSOR
    ;

create_type_distinct
    : CREATE TYPE distinct_type_name AS source_data_type (WITH STRONG TYPE RULES | WITH WEAK TYPE RULES data_type_constrainst)?
    ;

create_type_row
    : CREATE or_replace? TYPE type_name AS ROW (field_definition_list_paren | anchored_row_data_type)
    ;

field_definition_list_paren
    : '(' field_definition_list ')'
    ;

field_definition_list
    : field_definition (',' field_definition)*
    ;

field_definition
    : field_name data_type
    ;

create_type_structured
    : CREATE TYPE type_name (UNDER supertype_name)? (AS attribute_definition_list_paren)?
        structured_type_seq+
        method_specification_list?
    ;

structured_type_seq
    : NOT? INSTANTIABLE
    | CAST '(' SOURCE AS REF ')' WITH function_name
    | CAST '(' REF AS SOURCE ')' WITH function_name
    | INLINE LENGTH integer_value
    | WITHOUT COMPARISONS
    | NOT FINAL
    | MODE DB2SQL
    | WITH FUNCTION ACCESS
    | REF USING rep_type
    ;

attribute_definition_list_paren
    : '(' attribute_definition_list ')'
    ;

attribute_definition_list
    : attribute_definition (',' attribute_definition)*
    ;

attribute_definition
    : attribute_name data_type lob_options?
    ;

method_specification_list
    : method_specification (',' method_specification)*
    ;

method_specification
    : OVERRIDING? METHOD method_name param_decl_list_paren method_specification_seq+
    ;

method_specification_seq
    : RETURNS (data_type as_locator? | data_type CAST FROM data_type as_locator?)
    | SPECIFIC specific_name
    | SELF AS RESULT
    | sql_routine_characteristics
    | external_routine_characteristics
    ;

as_locator
    : AS LOCATOR
    ;

param_decl_list_paren
    : '(' param_decl_list ')'
    ;

param_decl_list
    : param_decl (',' param_decl)*
    ;

param_decl
    : parameter_name? data_type_2 as_locator?
    ;

sql_routine_characteristics
    : LANGUAGE SQL
    | PARAMETER CCSID (ASCII | UNICODE)
    | NOT? DETERMINISTIC
    | NO? EXTERNAL ACTION
    | READS SQL DATA
    | CONTAINS SQL
    | CALLED ON NULL_ INPUT
    | INHERIT SPECIAL REGISTERS
    ;

external_routine_characteristics
    : LANGUAGE (C_ | JAVA | OLE)
    | PARAMETER STYLE (DB2GENERAL | SQL)
    | PARAMETER CCSID (ASCII | UNICODE)
    | NOT? DETERMINISTIC
    | FENCED (NOT? THREADSAFE)?
    | NOT FENCED THREADSAFE?
    | CALLED ON NULL_ INPUT
    | RETURNS NULL_ ON NULL_ INPUT
    | READS SQL DATA
    | NO SQL
    | CONTAINS SQL
    | NO? EXTERNAL ACTION
    | NO SCRATCHPAD
    | SCRATCHPAD length?
    | NO? FINAL CALL
    | (ALLOW | DISALLOW) PARALLEL
    | NO? DBINFO
    | INHERIT SPECIAL REGISTERS
    ;

length
    : integer_value
    ;

rep_type
    : (SMALLINT | INTEGER | INT | BIGINT)
    | (DECIMAL | DEC | NUMERIC | NUM) ('(' integer_value (',' integer_value)? ')')?
    | DECFLOAT ('(' integer_value ')')?
    | char_character integer_paren? for_bit_data?
    | varchars integer_paren for_bit_data?
    | BINARY integer_paren?
    | varbinaries integer_paren
    | GRAPHIC integer_paren?
    | VARGRAPHIC integer_paren
    ;

varchars
    : VARCHAR
    | char_character VARYING
    ;

varbinaries
    : VARBINARY
    | BINARY VARYING
    ;

for_bit_data
    : FOR BIT DATA
    ;

lob_options
    : NOT? LOGGED
    | NOT? COMPACT //TODO DOT
    ;

create_type_mapping
    : CREATE TYPE MAPPING type_mapping_name?
         from_to (LOCAL TYPE)? local_data_type
         from_to remote_server REMOTE? TYPE data_source_data_type for_bit_data_precision
    ;

for_bit_data_precision
    : for_bit_data
    | '(' (precision | precision '..' precision) (',' (scale | scale '..' scale))? ')' precision_scale_comp?
    ;

precision
    : integer_value
    ;

scale
    : integer_value
    ;

precision_scale_comp
    : P_ ('='|'>'|'<'|'>='|'<='|'<>') S_
    ;

from_to
    : FROM
    | TO
    ;

data_source_data_type
    : todo
    ;

local_data_type
    : built_in_type
    ;

remote_server
    : SERVER server_name
    | SERVER TYPE server_type (VERSION server_version (WRAPPER wrapper_name)?)?
    ;

server_version
    : version ('.' release ('.' mod)? )?
    | string_constant
    ;

server_type
    : id_
    ;

version
    : integer_value
    ;

release
    : integer_value
    ;

mod
    : integer_value
    ;

create_usage_list
    : CREATE USAGE LIST usage_list_name FOR (TABLE | INDEX) object_name
        (LIST SIZE integer_value)?
        (WHEN FULL (WRAP | DEACTIVATE))?
        ((INACTIVE | ACTIVE) ON START DATABASE)?
    ;

create_user_mapping
    : CREATE USER MAPPING FOR (authorization_name | USER | PUBLIC) SERVER server_name OPTIONS user_mapping_options_paren
    ;

user_mapping_options_paren
    : '(' user_mapping_options (',' user_mapping_options)* ')'
    ;

user_mapping_options
    : user_mapping_option_name string
    ;

create_variable
    : CREATE or_replace? VARIABLE variable_name data_type_1
            ( (DEFAULT | CONSTANT)? NULL_
            | (DEFAULT | CONSTANT) ( constant_
                                   | special_register
                                   | global_variable
                                   | '(' cursor_value_constructor ')'
                                   | '(' expression ')'
                                   )
            )
    ;

constant_
    : integer_constant
    | bigint_constant
    | todo
    ;

special_register
    : id_
    ;

global_variable
    : id_
    ;

data_type_1
    : built_in_type
    | anchored_variable_data_type
    | array_type_name
    | cursor_type_name
    | distinct_type_name
    | REF '(' type_name ')'
    | row_type_name
    ;

cursor_value_constructor
    : (ASENSITIVE | INSENSITIVE) CURSOR param_decl_list_paren? holdability FOR select_statement
    ;

anchored_variable_data_type
    : ANCHOR (DATA TYPE)? TO? ( variable_name
                              | table_name '.' column_name
                              | ROW OF? (table_name | view_name | cursor_variable_name)
                              )
    ;

holdability
    : (WITHOUT | WITH) HOLD
    ;

create_view
    : CREATE or_replace? VIEW view_name ( column_list_paren
                                        | OF type_name (root_view_definition | subview_definition)
                                        )?
            (WITH cte_list)? fullselect
            create_view_seq*
    ;

create_view_seq
    : WITH (CASCADED | LOCAL)? CHECK OPTION
    | WITH NO? ROW MOVEMENT
    ;

cte_list
    : cte (',' cte)*
    ;

cte
    : todo
    ;

fullselect
    : (subselect | '(' fullselect ')' values_clause)
        todo
    ;

subselect
    : select_clause from_clause where_clause? group_by_clause? having_clause?
        order_by_clause? offset_clause? fetch_clause? isolation_clause?
    ;

select_clause
    : SELECT (ALL | DISTINCT)?
    ;

from_clause
    : FROM table_reference (',' table_reference)*
    ;

table_reference
    : singles_table_reference
    | single_view_reference
    | single_nickname_reference
    | only_table_reference
    | outer_table_reference
    | analyze_table_reference
    | nested_table_reference
    | data_change_table_reference
    | table_function_reference
    | collection_derived_table
    | xmltable_expression
    //| joined_table
    | external_table_reference
    ;

singles_table_reference
    : table_name period_specification* correlation_clause? tablesample_clause?
    ;

period_specification
    : FOR (SYSTEM_TIME | BUSINESS_TIME) (AS OF value | FROM value TO value | BETWEEN value AND value)
    ;

value
    : todo
    ;

correlation_clause
    : AS? correlation_name column_list_paren?
    ;

tablesample_clause
    : TABLESAMPLE (BERNOULLI | SYSTEM) '(' numeric_expression ')' (REPEATABLE numeric_expression)?
    ;

numeric_expression
    : todo
    ;

single_view_reference
    : view_name period_specification* correlation_clause?
    ;

single_nickname_reference
    : nick_name correlation_clause?
    ;

only_table_reference
    : ONLY '(' (table_name | view_name) ')' correlation_clause?
    ;

outer_table_reference
    : OUTER '(' (table_name | view_name) ')' correlation_clause?
    ;

analyze_table_reference
    : (table_name | view_name) ANALYZE TABLE  '('  implementation_clause')'
    ;

implementation_clause
    : IMPLEMENTATION string
    ;

nested_table_reference
    : (LATERAL (continue_handler WITHIN)?)? '(' (WITH cte_list)? fullselect ')' correlation_clause?
    ;

continue_handler
    : RETURN DATA UNTIL specific_condition_value (',' specific_condition_value)*
    ;

specific_condition_value
    : FEDERATED SQLSTATE VALUE? string_constant (SQLCODE integer_constant (',' integer_constant)*)?
    ;

data_change_table_reference
    : ( final_new TABLE '(' insert_statement ')'
      | final_new_old TABLE '(' searched_update_statement ')'
      | OLD TABLE '(' seached_delete_statement ')'
      ) correlation_clause?
    ;

insert_statement
    : todo
    ;

searched_update_statement
    : todo
    ;

seached_delete_statement
    : todo
    ;

final_new
    : FINAL
    | NEW
    ;

final_new_old
    : FINAL
    | NEW
    | OLD
    ;

table_function_reference
    : TABLE '(' function_name '(' expression (',' expression)* ')' table_udf_cardinality_clause? ')'
        (correlation_clause | typed_correlation_clause)?
    ;

table_udf_cardinality_clause
    : CARDINALITY integer_constant
    | CARDINALITY MULTIPLIER numeric_constant
    ;

typed_correlation_clause
    : AS? correlation_name ('(' column_name_data_type (',' column_name_data_type)* ')')?
    ;

column_name_data_type
    : column_name data_type
    ;

collection_derived_table
    : UNNEST table_function (WITH ORDINALITY)? correlation_clause?
    ;

table_function
    : todo
    ;

xmltable_expression
    : xmltable_function correlation_clause?
    ;

xmltable_function
    : todo
    ;

joined_table
    : table_reference (INNER | outer)? JOIN table_reference (ON join_condition | USING '(' column_list ')')
    | table_reference CROSS JOIN table_reference
    | '(' joined_table ')'
    ;

join_condition
    : todo
    ;

outer
    : (LEFT | RIGHT | FULL) OUTER?
    ;

external_table_reference
    : EXTERNAL file_name (AS? correlation_name)?
        ( '(' column_definition_2 (',' column_definition_2)* ')'
        | LIKE (table_name | view_name | nick_name)
        )
    ;

column_definition_2
    : column_name built_in_type (NOT NULL_)?
    ;

file_name
    : todo
    ;

where_clause
    : WHERE search_condition
    ;

group_by_clause
    : GROUP BY group_by_clause_opts (',' group_by_clause_opts)*
    ;

group_by_clause_opts
    : grouping_expression
    | grouping_sets
    | super_groups
    ;

grouping_expression
    : todo
    ;

grouping_sets
    : GROUPING SETS '(' ')'
    ;

super_groups
    : ROLLUP
    | CUBE
    | grant_total
    ;

grouping_expression_list
    : todo
    ;

grant_total
    : '(' ')'
    ;

having_clause
    : search_condition
    ;

order_by_clause
    : order_by_clause_opts (',' order_by_clause_opts)*
    | INPUT SEQUENCE
    ;

order_by_clause_opts
    : sort_key (asc_desc (NULLS first_last)?)?
    | ORDER OF table_designator
    ;

table_designator
    : id_
    ;

asc_desc
    : ASC
    | DESC
    ;

first_last
    : FIRST
    | LAST
    ;

sort_key
    : simple_column_name
    | simple_integer
    | sork_key_expression
    ;

simple_column_name
    : todo
    ;

simple_integer
    : todo
    ;

sork_key_expression
    : todo
    ;

offset_clause
    : OFFSET offset_row_count row_rows
    ;

offset_row_count
    : todo
    ;

fetch_clause
    : FETCH NEXT fetch_row_count? row_rows ONLY
    ;

fetch_row_count
    : todo
    ;

row_rows
    : ROW
    | ROWS
    ;

isolation_clause
    : WITH ( RR lock_request_clause?
           | RS lock_request_clause?
           | CS
           | UR
           )
    ;

lock_request_clause
    : USE AND KEEP (SHARE | UPDATE | EXCLUSIVE) LOCKS
    ;

values_clause
    : VALUES values_row (',' values_row)*
    ;

values_row
    : (expression | NULL_ | row_expression)
    | '(' expr_null (',' expr_null)* ')'
    ;

root_view_definition
    : MODE DB2SQL '(' oid_column (',' with_options)? ')'
    ;

subview_definition
    : MODE DB2SQL under_clause ('(' with_options ')')? EXTEND?
    ;

oid_column
    : REF IS oid_column_name USER GENERATED UNCHECKED?
    ;

with_options
    : with_option_def (',' with_option_def)*
    ;

with_option_def
    : column_name WITH OPTIONS (with_option_scope_def+ | READ ONLY)
    ;

with_option_scope_def
    : SCOPE (typed_table_name | typed_view_name)
    ;

under_clause
    : UNDER superview_name INHERIT SELECT PRIVILEGES
    ;

create_work_action_set
    : CREATE WORK ACTION SET work_action_set_name FOR (DATABASE | SERVICE CLASS_ service_superclass_name | WORKLOAD workload_name)
        USING WORK CLASS_ SET work_class_set_name (work_action_definition_list_paren)?
    ;

work_action_definition_list_paren
    : '(' work_action_definition_list ')'
    ;

work_action_definition_list
    : work_action_definition (',' work_action_definition)*
    ;

work_action_definition
    : WORK ACTION work_action_name ON WORK CLASS_ work_class_name action_types_clause histogram_templace_clause* enable_disable?
    ;

action_types_clause
    : MAP ACTIVITY (with_without NESTED)? TO service_subclass_name
    | WHEN threshold_types_clause threshold_exceeded_actions
    | PREVENT EXECUTION
    | COLLECT ACTIVITY DATA collect_activity_data_clause
    | COLLECT AGGREGATE ACTIVITY DATA (BASE | EXTENDED)?
    ;

threshold_types_clause
    : CONCURRENTDBCOORDACTIVITIES '>' integer_value (AND QUEUEDACTIVITIES ('>' integer_value | UNBOUNDED))?
    | SQLTEMPSPACE '>' integer_value kmg
    | SQLROWSRETURNED '>' integer_value
    | ESTIMATEDSQLCOST '>' bigint_value
    | CPUTIME '>' integer_value hours_minutes (CHECKING EVERY integer_value second_seconds)?
    | SQLROWSREAD '>' bigint_value (CHECKING EVERY integer_value second_seconds)?
    | SORTSHRHEAPUTIL '>' integer_value PERCENT (AND BLOCKING ADMISSION FOR '>' integer_value dt_units_with_seconds)?
    | ACTIVITYTOTALTIME '>' integer_value dt_units_with_seconds
    | ACTIVITYTOTALRUNTIME '>' integer_value dt_units_with_seconds
    ;

second_seconds
    : SECOND
    | SECONDS
    ;

hours_minutes
    : HOUR
    | HOURS
    | MINUTE
    | MINUTES
    ;

threshold_exceeded_actions
    : (COLLECT ACTIVITY DATA (NONE | collect_activity_data_clause))? (STOP EXECUTION | CONTINUE)
    ;

collect_activity_data_clause
    : (ON COORDINATOR MEMBER? | ON ALL MEMBERS?)?
        ( WITHOUT DETAILS
        | WITH ( DETAILS
               | SECTION (INCLUDE ACTUALS BASE)?
               ) (AND VALUES)?
        )?
    ;

with_without
    : WITH
    | WITHOUT
    ;

histogram_templace_clause
    : ACTIVITY (LIFETIME | QUEUETIME | EXECUTETIME | ESTIMATEDCOST | INTERARRIVALTIME)
        HISTOGRAM TEMPLATE (SYSDEFAULTHISTOGRAM | template_name)
    ;

create_work_class_set
    : CREATE WORK CLASS_ SET work_class_set_name work_class_definition_list_paren?
    ;

work_class_definition_list_paren
    : '(' work_class_definition_list ')'
    ;

work_class_definition_list
    : work_class_definition (',' work_class_definition)*
    ;

work_class_definition
    : (WORK CLASS_)? work_class_name work_attributes position_clause
    ;

work_attributes
    : WORK TYPE ( CALL schema_clause?
                | (READ | WRITE | DML) for_from_to_clause? data_tag_clause?
                | DDL
                | LOAD
                | ALL for_from_to_clause? schema_clause? data_tag_clause?
                )
    ;

position_clause
    : POSITION LAST
    | POSITION (BEFORE | AFTER) work_class_name
    | POSITION AT position_
    ;

position_
    : positive_integer
    ;

for_from_to_clause
    : FOR (TIMERONCOST | CARDINALITY) FROM from_value (TO (UNBOUNDED | to_value))?
    ;

from_value
    : todo
    ;

to_value
    : todo
    ;

data_tag_clause
    : DATA TAG LIST CONTAINS integer_constant
    ;

schema_clause
    : ROUTINES IN SCHEMA schema_name
    ;

create_workload
    : CREATE WORKLOAD workload_name connection_attributes+ workload_attributes
        position_clause_2?
        (PRIORITY (CRITICAL | HIGH | MEDIUM | LOW))?
        (COLLECT ACTIVITY METRICS (NONE | BASE | EXTENDED)?)?
        (COLLECT ACTIVITY DATA (NONE | collect_on_clause collect_details_clause))?
        (COLLECT AGGREGATE ACTIVITY DATA (BASE | EXTENDED)?)?
        (COLLECT AGGREGATE UNIT OF WORK DATA BASE?)?
        (COLLECT LOCK TIMEOUT DATA (NONE | WITH HISTORY (AND VALUES)?)?)?
        (COLLECT DEADLOCK DATA (WITH HISTORY (AND VALUES)?)?)?
        (COLLECT LOCK WAIT DATA collect_lock_wait_options)?
        (COLLECT UNIT OF WORK DATA (BASE (INCLUDE pkg_exec_seq)?)?)?
        histogram_templace_clause
    ;

pkg_exec_seq
    : PACKAGE LIST (',' EXECUTABLE LIST)?
    | EXECUTABLE LIST (',' PACKAGE LIST)?
    ;

position_clause_2
    : POSITION LAST
    | POSITION (BEFORE | AFTER) workload_name
    | POSITION AT position_
    ;

connection_attributes
    : ADDRESS string_list_paren
    | APPLNAME string_list_paren
    | SYSTEM_USER string_list_paren
    | SESSION_USER (GROUP | ROLE)? string_list_paren
    | CURRENT (CLIENT_USERID | CLIENT_APPLNAME | CLIENT_WRKSTNNAME | CLIENT_ACCTNG) string_list_paren
    ;

string_list
    : string (',' string)*
    ;


string_list_paren
    : '(' string+ ')'
    ;

workload_attributes
    : enable_disable? (allow_disallow DB ACCESS)? (MAXIMUM DEGREE_ (DEFAULT | degree))?
        (SERVICE CLASS_ (SYSDEFAULTUSERCLASS | service_class_name (UNDER service_superclass_name)?))?
    ;

degree
    : todo
    ;

allow_disallow
    : ALLOW
    | DISALLOW
    ;

collect_on_clause
    : ON COORDINATOR MEMBER?
    | ON ALL MEMBERS?
    ;

collect_details_clause
    : WITHOUT DETAILS
    | WITH  (DETAILS | SECTION (INCLUDE ACTUALS BASE)?)? (AND VALUES)?
    ;

collect_lock_wait_options
    : FOR LOCKS WAITING MORE_ THAN (wait_time (SECONDS | MICROSECONDS) | L_ONE SECOND) (WITHOUT HISTORY | WITH HISTORY (AND VALUES)?)?
    ;

wait_time
    : integer_value
    ;

create_wrapper
    : CREATE WRAPPER wrapper_name (LIBRARY library_name) (OPTIONS '(' wrapper_option_list ')')?
    ;

wrapper_option_list
    : wrapper_option (',' wrapper_option)*
    ;

wrapper_option
    : wrapper_option_name string
    ;


// EXPRESSION
expression
    : todo
    ;

todo
    : id_
    ;

// ID

id_
    : ID
    ;

name
    : id_
    ;

label
    : id_
    ;

host_label
    : id_
    ;

library_name
    : id_
    ;

array_type_name
    : id_
    ;

attribute_name
    : id_
    ;

row_type_name
    : id_
    ;

authorization_name
    : id_
    ;

boolean_variable_name
    : id_
    ;

array_variable_name
    : id_
    ;

column_name
    : id_
    ;

constraint_name
    : id_
    ;

descriptor_name
    : id_
    ;

distinct_type_name
    : id_
    ;

cursor_name
    : id_
    ;

cursor_type_name
    : id_
    ;

condition_name
    : id_
    ;

data_source_name
    : id_
    ;

expression_name
    : id_
    ;

group_name
    : id_
    ;

policy_name
    : id_
    ;

bufferpool_name
    : id_
    ;

db_partition_name
    : id_
    ;

database_name
    : id_
    ;

event_monitor_name
    : id_
    ;

field_name
    : id_
    ;

function_name
    : id_
    ;

function_mapping_name
    : id_
    ;

global_variable_name
    : id_
    ;

hierarchy_name
    : id_
    ;

host_variable_name
    : id_
    ;

parameter_marker
    : id_
    ;

template_name
    : id_
    ;

index_name
    : id_
    ;

index_extension_name
    : id_
    ;

input_descriptor_name
    : id_
    ;

mask_name
    : id_
    ;

method_name
    : id_
    ;

module_name
    : id_
    ;

new_owner
    : id_
    ;

nick_name
    : id_
    ;

object_name
    : id_
    ;

oid_column_name
    : id_
    ;

optimization_profile_name
    : id_
    ;

package_name
    : id_
    ;

partition_name
    : id_
    ;

path_name
    : id_
    ;

permission_name
    : id_
    ;

pipe_name
    : id_
    ;

procedure_name
    : id_
    ;

role_name
    : id_
    ;

root_table_name
    : id_
    ;

root_view_name
    : id_
    ;

row_variable_name
    : id_
    ;

source_schema_name
    : id_
    ;

source_package_name
    : id_
    ;

source_procedure_name
    : id_
    ;

sql_parameter_name
    : id_
    ;

sql_variable_name
    : id_
    ;

transition_variable_name
    : id_
    ;

savepoint_name
    : id_
    ;

specific_name
    : id_
    ;

schema
    : schema_name
    ;

schema_name
    : id_
    ;

search_method_name
    : id_
    ;

server_name
    : id_
    ;

server_option_name
    : id_
    ;

session_authorization_name
    : id_
    ;

component_name
    : id_
    ;

sec_label_comp_name
    : id_
    ;

security_policy_name
    : id_
    ;

security_label_name
    : id_
    ;

sequence_name
    : id_
    ;

service_class_name
    : id_
    ;

service_superclass_name
    : id_
    ;

storagegroup_name
    : id_
    ;

supertype_name
    : id_
    ;

superview_name
    : id_
    ;

service_subclass_name
    : id_
    ;

statement_name
    : id_
    ;

table_name
    : id_
    ;

tablespace_name
    : id_
    ;

target_identifier
    : id_
    ;

threshold_name
    : id_
    ;

trigger_name
    : id_
    ;

context_name
    : id_
    ;

usage_list_name
    : id_
    ;

type_name
    : id_
    ;

type_mapping_name
    : id_
    ;

typed_table_name
    : id_
    ;

typed_view_name
    : id_
    ;

user_mapping_option_name
    : id_
    ;

view_name
    : id_
    ;

variable_name
    : id_
    ;

work_action_set_name
    : id_
    ;

work_class_set_name
    : id_
    ;

workload_name
    : id_
    ;

work_action_name
    : id_
    ;

work_class_name
    : id_
    ;

wrapper_name
    : id_
    ;

wrapper_option_name
    : id_
    ;

xsrobject_name
    : id_
    ;

parameter_name
    : id_
    ;

cursor_variable_name
    : id_
    ;

alias_name
    : id_
    ;

db_partition_group_name
    : id_
    ;

source_index_name
    : id_
    ;

source_table_name
    : id_
    ;

source_storagegroup_name
    : id_
    ;

target_storagegroup_name
    : id_
    ;

source_tablespace_name
    : id_
    ;

target_tablespace_name
    : id_
    ;

unqualified_function_name
    : id_
    ;

unqualified_procedure_name
    : id_
    ;

unqualified_specific_name
    : id_
    ;

period_name
    : id_
    ;

history_table_name
    : id_
    ;
