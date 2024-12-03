parser grammar TeradataSQLDCLParser;

import TeradataSQLDataTypesParser
     , TeradataSQLIdentifiersParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

dcl_stat
    : give_stat
    | grant_stat
    | revoke_stat
    ;

/*****************
    GIVE statement
*/
give_stat : GIVE database_name TO recipient_name=database_name ;

/******************
    GRANT statement
*/
grant_stat
    : grant_role_stat
    | grant_monitor_stat
    | grant_sql_form_stat
    | grant_connect_through_stat
    | grant_logon_stat
    | grant_map_stat
    | grant_zone_stat
    | grant_zone_override_stat
    ;

grant_monitor_stat
    : GRANT ( MONITOR (PRIVILEGES|BUT NOT monitor_privilege (',' monitor_privilege)* )?
            | monitor_privilege (',' monitor_privilege)*
            ) TO (( grantee (',' grantee)*|PUBLIC) with_grant_option?
                 | role_name (',' role_name)*
                 )
   ;

grant_role_stat
    : GRANT role_name (',' role_name)
      TO (user_or_role=role_name) (',' (user_or_role=role_name) )*
      with_admin_option?
    ;

grant_sql_form_stat
    : GRANT ( ( ALL PRIVILEGES?
              | (ALL BUT)? privilege (',' privilege)*
              | CTCONTROL
              ) ON privilege_object
            | map_privilege (',' map_privilege)*
            | role_privilege (',' role_privilege)*
            | profile_privilege (',' role_privilege)*
            | zone_privilege (',' zone_privilege)*
            | CONSTRAINT ASSIGNMENT
            | CONSTRAINT DEFINITION
            )
      TO ( grantee (',' grantee)* with_grant_option?
         | PUBLIC with_grant_option?
         | role_name (',' role_name)*
         )
    ;

grant_connect_through_stat
    : GRANT CONNECT THROUGH trusted_user_name=user_name (WITH TRUST_ONLY)?
      TO ( application_user_name=user_name (',' application_user_name=user_name)*
           ( WITH ROLE role_name (',' role_name)* (WITH PROFILE profile_name)?
           | WITH PROFILE profile_name
           )
         | PERMANENT permanent_user_name=user_name (',' permanent_user_name=user_name)*
           (WITH ROLE role_name (',' role_name)*|WITHOUT ROLE)
         )
    ;

grant_logon_stat
    :  GRANT LOGON ON (host_id+=integer_literal (',' host_id+=integer_literal)* | ALL)
       (AS DEFAULT|(TO|FROM) user_name (',' user_name)* )
       (WITH NULL PASSWORD)?
    ;

grant_map_stat
    : GRANT MAP map_name=unqualified_name TO (user_or_role=role_name (',' user_or_role=role_name)* with_grant_option?|PUBLIC)
    ;

grant_zone_stat
    : GRANT ZONE zone_name=unqualified_name TO user_or_role=role_name (',' user_or_role=role_name)*
    ;

grant_zone_override_stat
    : GRANT ZONE OVERRIDE TO user_name (',' user_name)*
    ;

/*******************
    REVOKE statement
*/
revoke_stat
    : revoke_monitor_stat
    | revoke_role_stat
    | revoke_sql_form_stat
    | revoke_connect_through_stat
    | revoke_logon_stat
    | revoke_map_stat
    | revoke_zone_stat
    | revoke_zone_override_stat
    ;

revoke_monitor_stat
    : REVOKE grant_option_for?
      ( MONITOR (PRIVILEGES|BUT NOT monitor_privilege (',' monitor_privilege)* )?
      | monitor_privilege (',' monitor_privilege)*
      ) (TO|FROM) ( revokee (',' revokee)*|PUBLIC)
   ;

revoke_role_stat
    : REVOKE (ADMIN OPTION FOR)? role_name (',' role_name)*
      (TO|FROM) (user_or_role=role_name) (',' (user_or_role=role_name) )*
    ;

revoke_sql_form_stat
    : REVOKE grant_option_for?
            ( ( ALL PRIVILEGES?
              | (ALL BUT)? privilege (',' privilege)*
              ) ON privilege_object
            | map_privilege (',' map_privilege)*
            | role_privilege (',' role_privilege)*
            | profile_privilege (',' role_privilege)*
            | zone_privilege (',' zone_privilege)*
            )
      (TO|FROM) ( revokee (',' revokee)*
         | PUBLIC
         | role_name (',' role_name)*
         )
    ;

revoke_connect_through_stat
    : REVOKE CONNECT THROUGH trusted_user_name=user_name
      ( (TO|FROM) ( application_user_name=user_name (',' application_user_name=user_name)*
                    ( WITH ROLE role_name (',' role_name)* (WITH PROFILE profile_name)?
                    | WITH PROFILE profile_name
                    )
                  | PERMANENT permanent_user_name=user_name (',' permanent_user_name=user_name)*
                    (WITH ROLE role_name (',' role_name)* )?
                  )
      | WITH TRUST ONLY
      )
    ;

revoke_logon_stat
    :  REVOKE LOGON ON (host_id+=integer_literal (',' host_id+=integer_literal)* | ALL)
       (AS DEFAULT|(TO|FROM) user_name (',' user_name)* )
    ;

revoke_map_stat
    : REVOKE grant_option_for? MAP map_name=unqualified_name
      (TO|FROM) (user_or_role=role_name (',' user_or_role=role_name)*|PUBLIC)
    ;

revoke_zone_stat
    : REVOKE ZONE zone_name=unqualified_name (TO|FROM) user_or_role=role_name (',' user_or_role=role_name)*
    ;

revoke_zone_override_stat
    : REVOKE ZONE OVERRIDE (TO|FROM) user_name (',' user_name)*
    ;

/*
    Shared rules
*/

privilege
    : ALTER EXTERNAL PROCEDURE
    | ALTER FUNCTION
    | ALTER PROCEDURE
    | ANY
    | CHECKPOINT
    | CREATE AUTHORIZATION
    | CREATE DATABASE
    | CREATE DATASET SCHEMA
    | CREATE EXTERNAL PROCEDURE
    | CREATE FUNCTION
    | CREATE GLOP
    | CREATE MACRO
    | CREATE OWNER PROCEDURE
    | CREATE PROCEDURE
    | CREATE SERVER
    | CREATE TABLE
    | CREATE TRIGGER
    | CREATE USER
    | CREATE VIEW
    | DATABASE
    | DELETE
    | DROP AUTHORIZATION
    | DROP DATABASE
    | DROP DATASET SCHEMA
    | DROP FUNCTION
    | DROP GLOP
    | DROP MACRO
    | DROP PROCEDURE
    | DROP SERVER
    | DROP TABLE
    | DROP TRIGGER
    | DROP USER
    | DROP VIEW
    | DUMP
    | EXECUTE
    | EXECUTE FUNCTION
    | EXECUTE PROCEDURE
    | FUNCTION
    | GLOP
    | GLOP MEMBER
    | INDEX
    | INSERT
    | INSERT column_list
    | MACRO
    | NONTEMPORAL
    | OVERRIDE
    | OVERRIDE DELETE
    | OVERRIDE DUMP
    | OVERRIDE INSERT
    | OVERRIDE RESTORE
    | OVERRIDE SELECT
    | OVERRIDE UPDATE
    | PROCEDURE
    | REFERENCES
    | REFERENCES column_list
    | RESTORE
    | RETRIEVE
    | SELECT
    | SELECT column_list
    | SHOW
    | STATISTICS
    | TABLE
    | TRIGGER
    | UDT METHOD
    | UDTMETHOD
    | UDT TYPE
    | UDTTYPE
    | UDT USAGE
    | UDTUSAGE
    | UPDATE
    | UPDATE column_list
    | USER
    | VIEW
    | WITH DATASET SCHEMA
    ;

privilege_object
    : object_name
    | PROCEDURE procedure_name
    | SPECIFIC FUNCTION function_name
    | FUNCTION? function_name '(' (function_parameter (',' function_parameter)* )? ')'
    | TYPE sysudtlib? udt_name
    ;

map_privilege : CREATE MAP | DROP MAP | MAP ;

role_privilege : CREATE ROLE | DROP ROLE | ROLE ;

profile_privilege : CREATE PROFILE | DROP PROFILE | PROFILE ;

zone_privilege : CREATE ZONE | DROP ZONE | ZONE ;

monitor_privilege : ABORTSESSION | MONRESOURCE | MONSESSION | SETRESRATE | SETSESSRATE ;

grantee : ALL? database_name;

revokee : ALL? database_name;

function_parameter : parameter_name? data_type ;

with_admin_option : WITH ADMIN OPTION ;

with_grant_option : WITH GRANT OPTION ;

grant_option_for : GRANT OPTION FOR ;
