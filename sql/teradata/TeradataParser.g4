/*
Teradata grammar.
The MIT License (MIT).

Copyright (c) 2023, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software'.'
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARe'.'
*/

parser grammar TeradataParser;

options { tokenVocab=TeradataLexer; }

teradata_file
    : batch* EOF
    ;

batch
    : ddl_command SEMI?
    ;

ddl_command
    : alter_constraint
    | alter_function
    | alter_hash_index
    | alter_join_index
    | alter_method
    | alter_option
    | alter_partitioning
    | alter_procedure_external
    | alter_procedure_sql
    | alter_table
    | alter_table_2
    | alter_table_to_current
    | alter_trigger
    | alter_type
    | alter_zone
    | begin_isolated_loading
    | begin_logging
    | begin_query_capture
    | begin_query_logging

    | comment

    | create_authorization
    | create_cast
    | create_constraint
    | create_database
    | create_error_table
    | create_foreign_table
    | create_function
    | create_function_external
    | create_function_mapping
    | create_function_table_form
    | create_global_temporary_trace_table
    | create_globl_set
    | create_hash_index
    | create_index
    | create_join_index
    | create_macro
    | create_map
    | create_method
    | create_ordering
    | create_procedure
    | create_procedure_external
    | create_profile
    | create_recursive_view
    | create_role
    | create_schema
    | create_table
    | create_table_as
    | create_table_queue_table_form
    | create_transform
    | create_trigger
    | create_type
    | create_type_1
    | create_type_2
    | create_type_3
    | create_type_4
    | create_type_struct
    | create_user
    | create_view
    | create_zone
    //...

    | drop_autorization
    | drop_cast
    | drop_constraint
    | drop_database
    | drop_error_table
    | drop_function
    | drop_function_mapping
    | drop_glop_set
    | drop_hash_index
    | drop_index
    | drop_join_index
    | drop_macro
    | drop_map
    | drop_option
    | drop_ordering
    | drop_procedure
    | drop_profile
    | drop_role
    | drop_schema
    | drop_statistics
    | drop_table
    | drop_transform
    | drop_trigger
    | drop_type
    | drop_user
    | drop_view
    | drop_zone
    ;

alter_constraint
    : ALTER CONSTRAINT constraint_name AS
    (
        VALUES '(' name':'value (',' name':'value)* ')'
        | FUNCTION ( ( ADD | REPLACE ) action_ syslib_dot function_name | DROP action_ )
    )
    ;

action_
    : DELETE | INSERT | SELECT | UPDATE
    ;

alter_function
    : ALTER ( SPECIFIC FUNCTION ( database_name_dot | user_name_dot )? specific_function_name |
        FUNCTION ( database_name_dot | user_name_dot )? function_name
          ( '(' ( data_type | sysudtlib_dot? user_defined_type_name )
//          (','dots)?
          ')' )?
      )
      ( EXECUTE NOT? PROTECTED | COMPILE ONLY? )
    ;

sysudtlib_dot
    : SYSUDTLIB'.'
    ;

syslib_dot
    : SYSLIB '.'
    ;

data_type
    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
    ( TIME | TIMESTAMP ) ( '('fractional_seconds_precision')' )? (WITH TIME ZONE)? |
    INTERVAL YEAR ('('precision')')? (TO MONTH)? |
    INTERVAL MONTH ('('precision')')? |
    INTERVAL DAY ('('precision')')? (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
    INTERVAL HOUR ('('precision')')? (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
    INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
    INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
    PERIOD '('DATE')' |
    PERIOD '(' ( TIME | TIMESTAMP ) ('('precision')')? (WITH TIME ZONE)? ')' |
    REAL |
    DOUBLE PRECISION |
    FLOAT ( '('integer')' )? |
    NUMBER ( '(' ( integer | '*') (',' integer)? dots ')' )? |
    ( DECIMAL | NUMERIC ) ( '(' integer (',' integer)? dots ')' )? |
    ( CHAR | BYTE | GRAPHIC ) ( '('integer')' )? |
    ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ( '('integer')' )? |
    LONG VARCHAR |
    LONG VARGRAPHIC |
    ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB ) '(' integer ( G | K | M )? ')' |
    sysudtlib_dot? ( XML | XMLTYPE ) ( '(' integer ( G | K | M )? ')' )? ( INLINE LENGTH integer )? |
    sysudtlib_dot? JSON ( '(' integer ( K | M )? ')' )? ( INLINE LENGTH integer )?
    ( CHARACTER SET ( UNICODE | LATIN ) | STORAGE FORMAT ( BSON | UBJSON ) )? |
    sysudtlib_dot? ST_GEOMETRY ( '('integer ( K | M )?')' )? ( INLINE LENGTH integer )? |
    sysudtlib_dot? DATASET ( '('integer ( K | M )?')' )? ( INLINE LENGTH integer )? storage_format |
    sysudtlib_dot? ( user_defined_type_name | MBR | array_type_name | varray_name )
    ))
    ;

storage_format
    : STORAGE FORMAT ( AVRO | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
        ( WITH SCHEMA database_dot? schema_name )?
    ;

database_dot
    : database '.'
    ;

database_name_dot
    : database_name '.'
    ;

alter_hash_index
    :ALTER HASH INDEX ( database_dot | user_name_dot )? hash_index_name ','
  MAP '=' map_name ( COLOCATE USING colocation_name )?
    ;

user_name_dot
    : user_name '.'
    ;

alter_join_index
    : ALTER JOIN INDEX ( database_dot | user_name_dot )? join_index_name ','
    MAP '=' map_name ( COLOCATE USING colocation_name )?
    ;

alter_method
    :ALTER ( specific_method_clause | method_clause )
    FOR user_defined_type_name ( EXECUTE NOT? PROTECTED | COMPILE ONLY? )
    ;

specific_method_clause
    : SPECIFIC METHOD sysudtlib_dot? specific_method_name sysudtlib_dot?
    ;

method_clause
    : ( INSTANCE | CONSTRUCTOR )? METHOD sysudtlib_dot? method_name
  ( '(' ( data_type | sysudtlib_dot? user_defined_type_name ) (','dots)? ')' )?
    ;

alter_procedure_external
    : ALTER PROCEDURE ( database_dot | user_name_dot )? procedure_name
    LANGUAGE ( C | CPP | JAVA )
    ( COMPILE ONLY? ( AT TIME ZONE ( LOCAL | sign? string ) )? |
    EXECUTE NOT? PROTECTED
    )
    ;

alter_procedure_sql
    : ALTER PROCEDURE ( database_dot | user_name_dot )? procedure_name
        ( LANGUAGE SQL )?
        COMPILE ( WITH ( NO? SPL | NO? WARNING )(','dots)? )?
      //  ( AT TIME ZONE ( LOCAL | sign? 'quotestring' ) )? )?
    ;

alter_table
    : ALTER TABLE ( database_name_dot | user_name_dot )? table_name
  ( alter_option (',' alter_option)* |
    table_option (',' table_option)* ( alter_option (',' alter_option)* )? |
    normalize |
    DROP NORMALIZE |
    modify_primary |
    MODIFY NO PRIMARY INDEX? (alter_partitioning )? |
    MODIFY alter_partitioning |
    FROM TIME ZONE '=' sign? string
      (',' TIMEDATEWZCONTROL '=' number )? (',' WITH TIME ZONE )? |
    ( SET | RESET ) DOWN
  )
    ;

alter_option
    : ADD add_option
      | MODIFY ( CONSTRAINT? name )? CHECK '(' boolean_condition ')'
      | RENAME ( column_name as_to column_name
               | constraint_name as_to constraint_name
               )
      | DROP drop_option
    ;

as_to
    : AS
    | TO
    ;

table_option
    : ( NO? FALLBACK PROTECTION? |
        WITH JOURNAL TABLE '=' ( database_name_dot )? table_name |
        ( NO | DUAL )? ( BEFORE )? JOURNAL |
        ON COMMIT ( DELETE | PRESERVE ) ROWS |
        NO? LOG |
        ( NO | DUAL | NOT? LOCAL )? AFTER JOURNAL |
        CHECKSUM '=' ( DEFAULT | ON | OFF ) ( IMMEDIATE )? |
        DEFAULT FREESPACE |
        FREESPACE '=' integer PERCENT? |
        ( DEFAULT | NO ) MERGEBLOCKRATIO |
        MERGEBLOCKRATIO '=' integer PERCENT? |
        ( DATABLOCKSIZE '=' data_block_size ( BYTES | KBYTES | KILOBYTES )? |
        ( MINIMUM | MAXIMUM | DEFAULT ) DATABLOCKSIZE
        ) IMMEDIATE |
        blockcompression |
        WITH NO? CONCURRENT? ISOLATED LOADING ( FOR ( ALL | INSERT | NONE ) )?
        ( USING FAST MODE ( ON | OFF ) )?
    )
    ;

normalize
    : ADD NORMALIZE ( ALL BUT '(' column_name (',' column_name)* ')' )?
  ON column_name ( ON ( MEETS OR OVERLAPS | OVERLAPS OR MEETS ) )?
    ;

modify_primary
    : MODIFY ( (NOT)? UNIQUE )? PRIMARY ( AMP )? ( INDEX )?
  ( index_name | NOT NAMED )? ( '(' column_name (','column_name)* ')' )? ( alter_partitioning )?
    ;

alter_partitioning
    : ( ( PARTITION BY ( partitioning_level | '(' partitioning_level (',' partitioning_level)* ')' ) |
    ( DROP range_expression ( ADD range_expression )? | ADD range_expression
    )
    //(','dots)?
    ( WITH ( INSERT INTO? save_table | DELETE ) )? |
      NOT PARTITIONED
))
    ;

add_option
    : ( ( column_specification | '(' column_specification (','column_specification)* ')' ) ( INTO column_name )? |

  ( COLUMN | ROW | SYSTEM )? '(' ( column_name | column_specification (','column_specification)* ) ')'
    ( (NO)? AUTO COMPRESS )? |

  '(' column_name ')' (NO)? AUTO COMPRESS |

  PERIOD FOR period_name '(' period_begin ',' period_end ')' |

  ( CONSTRAINT name )?
    ( FOREIGN KEY '(' column_name (',' column_name)* ')' references |
      ( UNIQUE | PRIMARY KEY ) '(' column_name (',' column_name)* ')' |
       CHECK '(' boolean_condition ')' |
    ) |
      row_level_security_constraint_column_name (',' row_level_security_constraint_column_name)* CONSTRAINT
)
    ;

drop_option
    : ( PERIOD FOR period_name |
  name ( IDENTITY )? |
  CONSTRAINT name |
  ( CONSTRAINT name )? FOREIGN KEY '(' column_name (',' column_name)* ')' references |
  ( ( CONSTRAINT )? name )? CHECK |
  INCONSISTENT REFERENCES |
  row_level_security_constraint_column_name (',' row_level_security_constraint_column_name)* CONSTRAINT
)
    ;

blockcompression
    : BLOCKCOMPRESSION '=' ( AUTOTEMP | MANUAL | ALWAYS | NEVER | DEFAULT )
  (',' BLOCKCOMPRESSIONALGORITHM '=' ( ZLIB | ELZS_H | DEFAULT ) )?
  (',' BLOCKCOMPRESSIONLEVEL '=' ( value | DEFAULT ) )?
    ;

partitioning_level
    : ( partitioning_expression |
  COLUMN ( (NO)? AUTO COMPRESS )? ( ( ALL BUT )? '(' column_partition (',' column_partition)* ')' )?
) ( ADD constant )?
    ;

range_expression
    : ( RANGE | RANGE '#' Ln )
    ( BETWEEN range (','dots)? (',' ( NO RANGE ( ( OR | ',' ) UNKNOWN )? | UNKNOWN ) )? |
    NO RANGE ( ( OR | ',' ) UNKNOWN )? |
    UNKNOWN |
    WHERE conditional_expression
    )
    ;

column_specification
    : column_name data_type ( column_attribute column_attribute* )?
    ;

references
    : REFERENCES ( WITH NO? CHECK OPTION )? referenced_table_name
        ( '(' referenced_column_name (','referenced_column_name)* ')' )?
    ;

column_partition
    : ( COLUMN | ROW )? ( column_name | '(' column_name (',' column_name)* ')' )
    ( NO? AUTO COMPRESS )?
    ;

range
    : start_expression ( AND end_expression )? ( EACH range_size )?
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//  ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//  INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//  INTERVAL MONTH ('(' precision')')? |
//  INTERVAL DAY ('(' precision')')?
//    (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//  INTERVAL HOUR ('('precision')')?
//    (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//  INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//  INTERVAL SECOND ('('precision')' (',' fractional_seconds_precision)?')' |
//  PERIOD '('DATE')' |
//  PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//  REAL |
//  DOUBLE PRECISION |
//  FLOAT ('('integer')')? |
//  NUMBER ('('( integer | '*' ) (',' integer )?dots')')? |
//  ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//  ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//  ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//  LONG VARCHAR |
//  LONG VARGRAPHIC |
//  ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB ) '('integer ( G | K | M )?')' |
//  sysudtlib_dot? ( XML | XMLTYPE ) ('('integer ( G | K | M )?')')? ( INLINE LENGTH integer )? |
//  sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? ( CHARACTER SET ( UNICODE | LATIN ) | STORAGE FORMAT ( BSON | UBJSON ) )? |
//  sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//  sysudtlib_dot? DATASET ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//    storage_format |
//  sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//))
//    ;

column_attribute
    : ( ( UPPERCASE | UC ) |
        (NOT)? ( CASESPECIFIC | CS ) |
        FORMAT quotestring |
        TITLE quotestring |
        NAMED name |
        DEFAULT ( number | USER | DATE | TIME | NULL_ ) |
        WITH DEFAULT |
        CHARACTER SET server_character_set |
        NOT? NULL_ |
        NOT? AUTO COLUMN |
        NO COMPRESS |
        COMPRESS ( constant | '(' constant_null  (',' constant_null)* ')' )? |
        COMPRESS USING compress_UDF_name DECOMPRESS USING decompress_UDF_name |
        ( CONSTRAINT constraint_name )?
        ( UNIQUE | PRIMARY KEY | CHECK '(' boolean_condition ')' | references )
    )
    ;

constant_null
    : constant | NULL_
    ;

//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//    ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

alter_table_2
    : ALTER TABLE ( database_name_dot | user_name_dot )? table_name ','
    MAP '=' map_name ( COLOCATE USING colocation_name )?
    ;

alter_table_to_current
    : ALTER TABLE ( table_name | join_index_name ) TO CURRENT
    ( WITH ( INSERT (INTO)? save_table | DELETE )? )?
    ;

alter_trigger
    : ALTER TRIGGER ( database_name_dot | user_name_dot )?
        ( trigger_name ( ENABLED | DISABLED | TIMESTAMP ) |
          table_name ( ENABLED | DISABLED )
        )
    ;

alter_type
    : ALTER TYPE sysudtlib_dot? user_defined_type_name (
     ( ADD | DROP )? ( attribute_clause | method_clause | specific_method_clause ) |
      COMPILE (ONLY)?
)
    ;

alter_zone
    : ALTER ZONE zone_name ( ADD ROOT ( database_name | user_name ) | DROP ROOT )?
    ;

begin_isolated_loading
    :BEGIN CONCURRENT? ISOLATED LOADING ON table_specification (','dots)?
    USING QUERY BAND string// 'LDILoadGroup = value'
    ( IN? ( SINGLE | MULTIPLE ) SESSION )?
    ;

table_specification
    : ( database_name_dot )? table_name
    ;

begin_logging
    : BEGIN LOGGING (DENIALS)? (WITH TEXT)? ON
  ( logging_frequency )?
  ( FOR CONSTRAINT constraint_name )?
  ( ALL | operation (','dots)? | GRANT )
  ( BY user_name (','dots)? )?
  ( ON item_list )?
    ;

logging_frequency
    : ( FIRST | LAST | FIRST AND LAST | EACH )
    ;

item_list
    : ( AUTHORIZATION authorization_name |
  DATABASE database_name |
  USER user_name |
  ( TABLE | VIEW | MACRO | PROCEDURE | FUNCTION | FUNCTION MAPPING | TYPE )
    ( database_name_dot | user_name_dot )? object_name
) (','dots)?
    ;

begin_query_capture
    : BEGIN QUERY CAPTURE
    ( FOR INDEX ANALYSIS )?
    ( WITH ( VERBOSE | ( DETAILED )? STATSUSAGE )(','dots)? )?
    ( INTO qcd_name )?
    AS WORKLOAD workload_name
    ;

begin_query_logging
    : BEGIN QUERY LOGGING
        ( WITH with_item (','dots)? )?
        ( MODE '=' todo )?
        ( LIMIT limit_item ( AND limit_item )? )?
        ON on_items
    ;

with_item
    : ( ALL |
        EXPLAIN |
        LOCK '=' todo |
        NONE |
        ( NO COLUMNS )? OBJECTS |
        PARAMINFO |
        FEATUREINFO |
        SQL |
        ( DETAILED )? STATSUSAGE |
        STEPINFO |
        USECOUNT |
        UTILITYINFO |
        ( VERBOSE )? XMLPLAN
      )
    ;

limit_item
    : ( SQLTEXT ( '=' number)? |

        ( SUMMARY '=' number',' number',' number | THRESHOLD ( '=' number )? )
          ( CPUTIME | CPUTIMENORM | ELAPSEDSEC | ELAPSEDTIME | IOCOUNT )?
      )
    ;

on_items
    : ( ( ALL | user_name ) ( ACCOUNT '=' ( string | '(' string (','dots)?')' ) )? |

        ( user_name | database_name )(','dots)? |
          APPLNAME '=' ( string | '('string (','dots)?')'
        )
      )
    ;

checkpoint_isolated_loading
    : CHECKPOINT (CONCURRENT)? ISOLATED LOADING FOR QUERY BAND string// 'LDILoadGroup = value'
    ;

collect_statistics
    : COLLECT ( SUMMARY )? ( STATISTICS | STAT )
        ( USING using_option ( AND using_option )?(dots)? )?
        ( ( UNIQUE )? INDEX index_specification | COLUMN column_specification )?(dots)?
        ON collection_source FROM from_option
    ;

using_option
    : ( SAMPLE | SYSTEM SAMPLE | SAMPLE number PERCENT | NO SAMPLE |
        ( SYSTEM THRESHOLD | THRESHOLD number | NO THRESHOLD ) ( PERCENT | DAYS )? |
        MAXINTERVALS number |
        SYSTEM MAXINTERVALS |
        MAXVALUELENGTH number |
        SYSTEM MAXVALUELENGTH
       ) ( FOR CURRENT )?
    ;

index_specification
    : ( index_name |
        ( index_name )? ( ALL )? '(' column_name (','dots)? ')'
          ( ORDER BY ( VALUES | HASH ) '(' column_name ')' )?
      )
    ;

//column_specification
//    : ( ( expression |
//          column_name |
//          '(' ( expression | column_name | PARTITION )(','dots)? ')'
//        ) ( (AS)? statistics_name )? |
//        PARTITION |
//        statistics_name
//      )
//    ;

collection_source
    : ( ( TEMPORARY )? ( database_name_dot | user_name_dot )? table_name |
        ( database_name_dot | user_name_dot )? ( join_index_name | hash_index_name )
      )
    ;

from_option
    : ( ( TEMPORARY )? ( database_name_dot | user_name_dot )? table_name |
        ( database_name_dot | user_name_dot )? ( join_index_name | hash_index_name )
      ) ( COLUMN (
            ( column_name | PARTITION ) |
            statistics_name |
            '(' ( column_name | PARTITION )(','dots)? ')'
          )
        )?
    ;

comment
    : COMMENT (ON)? ( object_kind | object_kind )
        ( database_name_dot | user_name_dot )? object_name
        ( ( AS | IS )? string )?
    ;

create_authorization
    : ( CREATE | REPLACE ) AUTHORIZATION ( database_name_dot | user_name_dot )? authorization_name
        ( AS ( DEFINER | INVOKER )? TRUSTED )?
        USER string
        PASSWORD string

    ;

create_cast
    : ( CREATE | REPLACE ) CAST '(' source AS target ')'
        WITH ( specific_method | specific_function )
        ( AS ASSIGNMENT )?
    ;

source
    : ( source_predefined_data_type | sysudtlib_dot? source_UDT_name )
    ;

target
    : ( target_predefined_data_type | sysudtlib_dot? target_UDT_name )
    ;

specific_method
    : ( SPECIFIC METHOD specific_method_name |
        ( INSTANCE )? METHOD method_name '(' ( data_name | user_defined_type_name )?(','dots)? ')'
      ) FOR user_defined_type_name
    ;

specific_function
    : ( SPECIFIC FUNCTION specific_function_name |
        FUNCTION function_name '('( data_name | user_defined_type_name )?(','dots)?')'
      )
    ;

create_constraint
    : CREATE CONSTRAINT constraint_name data_type ','
        ( (NOT)? NULL_',' )?
        VALUES '(' name':'value (','dots)? ')' ',' constraint (','dots)?
    ;

constraint
    : ( DELETE SYSLIB'.'d=function_name |
        INSERT SYSLIB'.'i=function_name |
        SELECT SYSLIB'.'s=function_name |
        UPDATE SYSLIB'.'u=function_name
      )
    ;

create_database
    : ( CREATE DATABASE | CD ) name ( FROM database_name )? AS database_attribute ((',')?dots)?
    ;

database_attribute
    : ( ( PERMANENT | PERM | SPOOL | TEMPORARY ) '=' ( number | constant_expression ) (BYTES)?
          ( SKEW '=' ( constant_expression | DEFAULT ) ( PERCENT )? )? |
        ACCOUNT '=' string |
        DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
        (NO)? FALLBACK (PROTECTION)? |
        ( NO | DUAL )? (BEFORE)? JOURNAL |
        ( NO | DUAL | (NOT)? LOCAL )? AFTER JOURNAL |
        DEFAULT JOURNAL TABLE '=' ( database_name_dot )? table_name
      )
    ;

create_error_table
    : CREATE ERROR TABLE ( error_table_name_specification )?
        FOR data_table_name_specification (NO RLS)?
    ;

error_table_name_specification
    : ( database_name_dot | user_name_dot)? error_table_name
    ;

data_table_name_specification
    : ( database_name_dot | user_name_dot )? data_table_name
    ;

create_foreign_table
    : CREATE (MULTISET)? FOREIGN TABLE table_specification
        ( ',' table_option (','dots)? )?
        ( ',' external_security_clause )?
        ( '(' location_column',' ( payload_column | data_column_definition )')' )?
        USING '('
          LOCATION '(' string ')'

          ( PATHPATTERN '(' string ')' )?
//          ( MANIFEST '(' ( 'TRUE' | 'FALSE' ) ')' )?
//          ( TABLE_FORMAT '(''DELTALAKE'')' )?
//          ( ROWFORMAT '(' 'encoding_format' ')' )?
//          ( STOREDAS '(' ( 'TEXTFILE' | 'PARQUET' ) ')')?
//          ( HEADER '(' ( 'TRUE' | 'FALSE' ) ')' )?
//          ( STRIP_EXTERIOR_SPACES '(' ( 'TRUE' | 'FALSE' ) ')' )?
          ( STRIP_ENCLOSING_CHAR '(''NONE'')' )?
        ')'
        ( (',')? NO PRIMARY INDEX )?

        ( (',')? PARTITION BY '(' ( COLUMN',' )? partition_column_spec (','dots)? ')' )?
    ;

//table_specification
//    : (database_name_dot | user_name_dot)? table_name
//    ;

external_security_clause
    : EXTERNAL SECURITY ( ( INVOKER | DEFINER ) TRUSTED )?
        ( database_name_dot| user_name_dot)?authorization_name
    ;

partition_column_spec
    : pcN=column_name dt=data_type
    ;

create_function_external
    : ( CREATE | REPLACE ) FUNCTION ( database_name_dot | user_name_dot )? function_name
        '(' parameter_specification (dots)? ')' RETURNS r=data_type ( CAST FROM data_type )?
        language_and_access_specification
        function_attribute (dots)?
        ( USING GLOP SET glop_set_name )?
        EXTERNAL ( NAME
          ( external_function_name
//          | 'code_specification (delimiterdots)?'
//          | 'JAR_ID_specification'
          ) )?
        ( PARAMETER STYLE ( SQL | TD_GENERAL | JAVA ) )?
        ( FOR ( COMPRESS | DECOMPRESS ) )?
        ( EXTERNAL SECURITY ( DEFINER ( authorization_name )? | INVOKER ) )?
    ;

parameter_specification
    : ( ( parameter_name )? data_type | ',' )
    ;

create_function_table_form
    :( CREATE | REPLACE ) FUNCTION ( database_name_dot | user_name_dot )? function_name
       '(' parameter_specification (dots)? ')' RETURNS TABLE table_specification
       language_and_access_specification
       function_attribute (dots)?
       ( USING ( GLOP SET )? glop_set_name )?
       EXTERNAL
         ( NAME ( external_function_name
//                | 'code_specification (delimiterdots)?'
//                | 'JAR_ID_specification'
                ) )?
       ( PARAMETER STYLE ( SQL | JAVA | SQLTABLE ) )?
       ( EXTERNAL SECURITY ( DEFINER ( authorization_name )? | INVOKER ) )?
       ( EXECUTE MAP '=' map_name ( COLOCATE USING colocation_name )? )?
    ;

//parameter_specification
//    : ( ( parameter_name )? data_type | ',' )
//    ;
//
//table_specification
//    : ( '(' column_specification (','dots)? ')' |
//
//       VARYING ( COLUMNS '(' maximum_output_columns ')' |
//
//                 USING FUNCTION ( database_name_2. | user_name_2. )?
//                   ( function_name )?
//               )
//     )
//    ;

language_and_access_specification
    : ( language_clause sql_data_access |
       external_data_access
     )
    ;

language_clause
    : C
    | CPP
    | JAVA
    | SAS
    ;

external_data_access
    : todo
    ;

function_attribute
    : ( SPECIFIC ( database_name_dot | user_name_dot )? specific_function_name |
       PARAMETER STYLE ( SQL | JAVA ) |
       (NOT)? DETERMINISTIC |
       CALLED ON NULL_ INPUT
     )
    ;

code_specification
    : ( F delimiter function_entry_name |
       ( S | C ) path_specification
     )
    ;

jar_id_specification
    : JAR_ID':'java_class_name'.'method_name
       //( '(' java_parameter_class (','dots)? ')' 'returns' java_parameter_class )?
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//       ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//       INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//       INTERVAL MONTH ('(' precision')')? |
//       INTERVAL DAY ('(' precision')')?
//         (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//       INTERVAL HOUR ('('precision')')?
//         (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//       INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//       INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//       PERIOD '('DATE')' |
//       PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//       REAL |
//       DOUBLE PRECISION |
//       FLOAT ('('integer')')? |
//       NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//       ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//       ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//       ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//       LONG VARCHAR |
//       LONG VARGRAPHIC |
//       ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//         '('integer ( G | K | M )?')' |
//       sysudtlib_dot? ( XML | XMLTYPE ) ('('integer ( G | K | M )?')')?
//         ( INLINE LENGTH integer )? |
//       sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//         ( CHARACTER SET ( UNICODE | LATIN ) )? |
//       sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//       sysudtlib_dot? DATASET ('('integer ( K | M )?')')?
//         ( INLINE LENGTH integer )? storage_format |
//       sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//     ))
//    ;
//
//column_specification
//    : column_name column_data_type
//    ;

path_specification
    : ( I delimiter name_on_server delimiter include_name |
       L delimiter library_name |
       O delimiter name_on_server delimiter object_name |
       P delimiter package_name |
       S delimiter name_on_server delimiter source_name |
       NS delimiter source_file delimiter include_file
     )
    ;

java_parameter_class
    : //( primitive ()? ( ()? )?)? | object ( ()? )? )
    ;

//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//       ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

create_function
    : ( CREATE | REPLACE ) FUNCTION ( database_name_dot | user_name_dot )? function_name
        '(' parameter_specification (','dots)? ')' RETURNS r=data_type
        language_and_access_specification
        ( function_attribute (dots)? )?
        ( SQL SECURITY DEFINER )? COLLATION INVOKER INLINE TYPE //'1'
        RETURN return_expression
    ;

//parameter_specification
//    : parameter_name parameter_data_type
//    ;
//
//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('(' precision')')? |
//        INTERVAL DAY ('(' precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//        PERIOD '('DATE')' |
//        PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB ) '('integer ( G | K | M )?')' |
//        sysudtlib_dot? ( XML | XMLTYPE ) ('('integer ( G | K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//          ( CHARACTER SET ( UNICODE | LATIN ) )? |
//        sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? DATASET ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//          storage_format |
//        sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//      ))
//    ;
//
//language_and_access_specification
//    : ( ( language_clause)? SQL_data_access |
//        SQL_data_access ( language_clause )?
//      )
//    ;
//
//function_attribute
//    : ( SPECIFIC ( database_name_2. | user_name_2. )? specific_function_name |
//        (NOT)? DETERMINISTIC |
//        CALLED ON NULL_ INPUT |
//        RETURNS NULL_ ON NULL_ INPUT
//      )
//    ;
//
//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//        ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

create_function_mapping
    : ( CREATE | REPLACE ) FUNCTION MAPPING ( database_name_dot | user_name_dot )? function_mapping_name
        FOR ( ( database_name_dot | user_name_dot ) ( schema_name'.' )? )? function_name
        ( SERVER ( database_name_dot | user_name_dot )? server_name )?
        ( EXTERNAL SECURITY ( DEFINER | INVOKER )? TRUSTED )? authorization_name
        ( MAP JSON '('json_document')' )?
        //( STOREDAS '(' ( 'PARQUET' | 'TEXTFILE' ) ')' )?
        ( USING table_list )?
    ;

table_list
    : ( ANY IN TABLE | name ('('value')')? ( IN TABLE | OUT TABLE )? ) (','dots)?
    ;

create_global_temporary_trace_table
    : CREATE GLOBAL TEMPORARY TRACE TABLE ( database_name_dot | user_name_dot )? table_name
        '(' proc_id BYTE'(' number ')' ',' sequence INTEGER ( column_specification (','dots)? )? ')'
        ( ON COMMIT ( DELETE | PRESERVE ) ROWS )?
    ;

//column_specification
//    : column_name data_type data_type_attribute (dots)?
//    ;
//
//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('(' precision')')? |
//        INTERVAL DAY ('(' precision')')?
//          ( TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          ( TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ( '('precision (',' fractional_seconds_precision )? ')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC
//      ))
//    ;

data_type_attribute
    : ( NOT NULL_ |
        ( UPPERCASE | UC ) |
        (NOT)? ( CASESPECIFIC | CS ) |
        FORMAT quotestring |
        TITLE quotestring |
        NAMED name |
        CHARACTER SET server_character_set
      )
    ;

create_globl_set
    : CREATE GLOP SET ( database_name_dot | user_name_dot )? glop_set_name
    ;

create_hash_index
    : CREATE HASH INDEX ( database_name_dot | user_name_dot )? hash_index_name (',' index )?
        '(' column_name (','dots)?')' ON table_name
        ( BY '(' column_name (','dots)?')' )?
        ( ORDER BY (
            VALUES ( '(' column_name (','dots)? ')' )? |
            HASH '(' column_name (','dots)? ')' |
            '(' column_name (','dots)? ')'
          )
        )?
    ;

index
    : ( MAP map_name ( COLOCATE USING colocation_name )? |
          (NO)? FALLBACK (PROTECTION)? |
          CHECKSUM '=' ( ON | DEFAULT | OFF ) |
          BLOCKCOMPRESSION '=' ( AUTOTEMP | DEFAULT | MANUAL | NEVER )
      ) (','dots)?
    ;

create_index
    : CREATE index_specification (','dots)? ON ( table_specification | join_index_specification )
    ;

//index_specification
//    : (UNIQUE)? INDEX (index_name)? (ALL)? '('index_column_name (','dots)? ')'
//          ( ordering_clause )?
//          ( loading_clause )?
//    ;
//
//table_specification
//    : (TEMPORARY)? ( database_name_dot | user_name_dot )? table_name
//    ;

join_index_specification
    : ( database_name_dot | user_name_dot )? join_index_name
    ;

ordering_clause
    : ORDER BY ( VALUES | HASH )? ( '('o=column_name')' )?
    ;

loading_clause
    : WITH NO? LOAD IDENTITY
    ;

create_join_index
    : CREATE JOIN INDEX ( database_name_dot | user_name_dot )?
        ( table_option (','dots)? )?
        select_clause
        FROM source (','dots)?
        ( WHERE search_condition )?
        GROUP BY grouping_or_ordering_specification (','dots)?
        ORDER BY grouping_or_ordering_specification (','dots)?
        ( index ((',')?dots)? )?
    ;

//table_option
//    : ( MAP '=' map_name ( COLOCATE USING colocation_name )? |
//        (NO)? FALLBACK ( PROTECTION )? |
//        CHECKSUM '=' integrity_checking_level |
//        BLOCKCOMPRESSION '=' block_compression_option
//      )
//    ;

select_clause
    : AS SELECT
        ( selection (','dots)? |
          '(' selection (','dots)? ')' ',' '(' selection (','dots)? ')' |
          ( COLUMN | ROW )? '(' selection (','dots)? ')' ( (NO)? AUTO COMPRESS )?
        )
    ;

//source
//    : ( ( database_name_dot | user_name_dot )? table_name ( (AS)? corrolation_name )? |
//        joined_table
//      )
//    ;

grouping_or_ordering_specification
    : ( column_name | column_position | column_alias | expression_alias )
    ;

//index
//    : ( ( UNIQUE )? PRIMARY INDEX ( index_name )? '(' primary_index_column (','dots)? ')' |
//        NO PRIMARY INDEX |
//        PRIMARY AMP ( INDEX )? ( index_name )? '(' index_column_name (','dots)? ')' |
//        PARTITION BY ( partitioning_level | '(' partitioning_level (','dots)? ')' ) |
//        INDEX ( index_name )? ( ALL )? '(' index_column_name (','dots)? ')'
//          ORDER BY ( VALUES | HASH )? ( '(' order_column_name ')' )?
//      )
//    ;

selection
    : //( ( database_name_dot | user_name_dot )? table_name )? ( column_name | ROWID ) | aggregation_clause )
    ;

//TODO left-recursive
joined_table
    : todo
//    ( '(' joined_table ')'
//        | joined_table ( INNER | ( LEFT | RIGHT ) OUTER? )?
//          JOIN joined_table ON search_condition |
//        table_name (AS? correlation_name)?
//      )
    ;

//partitioning_level
//    : ( partitioning_expression |
//        COLUMN ( (NO)? AUTO COMPRESS )? ( ( ALL BUT )? column_partition )?
//      ) ( ADD constant )?
//    ;

aggregation_clause
    :  ( expression |
        SUM '(' numeric_expression ')' |
        ( COUNT | MIN | MAX ) '(' value_expression ')' |
        EXTRACT '(' ( YEAR | MONTH ) FROM date_expression ')'
      ) ( AS? expression_alias )?
    ;

create_macro
    : ( CREATE MACRO | CM | REPLACE MACRO ) ( database_name_dot )? macro_name
        ( '(' macro_parameter (','dots)? ')' )?
        AS '(' ( USING using_modifier )?
             ( LOCKING locking modifier )?(dots)?
             sql_statement ';'
            ')'
    ;

sql_statement
    : todo
    ;

macro_parameter
    : parameter_name type_declaration ( type_attribute )?dots
    ;

create_map
    : CREATE MAP s=map_name FROM c=map_name SPARSE AMPCOUNT '=' number
    ;

create_method
    : CREATE ( INSTANCE | CONSTRUCTOR )? METHOD sysudtlib_dot? method_name
        '(' locator_specification (','dots)? ')'
        RETURNS r=data_type ( CAST FROM c=data_type )? FOR user_defined_type_name
        ( USING GLOP SET glop_set_name )?
        EXTERNAL ( NAME ( e=method_name
                        //| 'item_list ( delimiterdots )?'
                        ) )?
        ( EXTERNAL SECURITY ( DEFINER ( authorization_name )? | INVOKER ) )?
    ;

locator_specification
    : ( parameter_name )? data_type ( AS LOCATOR )?
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ( '('fractional_seconds_precision')' )? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('('precision')')? |
//        INTERVAL DAY ('('precision')')?
//         (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//        INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//        PERIOD '('DATE')' |
//        PERIOD '(' ( TIME | TIMESTAMP ) ('('precision')')? (WITH TIME ZONE)? ')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ( '('integer')' )? |
//        NUMBER ( '(' ( integer | '*') (',' integer)?dots ')' )? |
//        ( DECIMAL | NUMERIC ) ( '(' integer (',' integer)?dots ')' )? |
//        ( CHAR | BYTE | GRAPHIC ) ( '('integer')' )? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ( '('integer')' )? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '(' integer ( G | K | M )? ')' |
//        ( XML | XMLTYPE ) |
//        JSON ( '(' integer ')' )? ( CHARACTER SET ( UNICODE | LATIN ) )? |
//        sysudtlib_dot? ( UDT_name | ST_Geometry | MBR | ARRAY_name | VARRAY_name )
//      ))
//    ;
//
//item_list
//    : ( F delimiter method_entry_name | D | ( S | C ) S_or_C_item )
//    ;

s_or_c_item
    : ( I delimiter name_on_server delimiter include_name |
        L delimiter library_name |
        O delimiter name_on_server delimiter object_name |
        S delimiter name_on_server delimiter source_name
      )
    ;

create_ordering
    : ( CREATE | REPLACE ) ORDERING FOR ( SYSUDTLIB'.' )? user_defined_type_name
        ORDER FULL BY MAP WITH ( method_specification | function_specification )
    ;

method_specification
    : ( SPECIFIC METHOD ( SYSUDTLIB'.' )? specific_method_name |
        ( INSTANCE )? METHOD ( SYSUDTLIB'.' )? method_name
          '(' ( ( data_type | ( SYSUDTLIB'.' )? user_defined_type_name ) (','dots)? )? ')'
      ) FOR user_defined_type_name
    ;

function_specification
    : ( SPECIFIC FUNCTION ( SYSUDTLIB'.' )? specific_function_name |
        FUNCTION ( SYSUDTLIB'.' )? function_name
          '(' ( ( data_type | ( SYSUDTLIB'.' )? user_defined_type_name ) (','dots)? )? ')'
      )
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('(' precision')')? |
//        INTERVAL DAY ('(' precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//        PERIOD '('DATE')' |
//        PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '('integer ( G | K | M )?')' |
//        ( XML | XMLTYPE ) |
//        JSON ( '(' integer ')' )? ( CHARACTER SET ( UNICODE | LATIN ) )? |
//        sysudtlib_dot? ( UDT_name | ST_Geometry | MBR | ARRAY_name | VARRAY_name )
//      ))
//    ;

create_procedure_external
    : ( CREATE | REPLACE ) PROCEDURE ( database_name_dot | user_name_dot )? procedure_name
        '(' parameter_specification (','dots)? ')'
        ( DYNAMIC RESULT SETS number_of_sets )?
        language_and_access_specification
        parameter_style_specification
        ( USING GLOP SET glop_set_name )?
        ( SQL SECURITY privilege_option )?
        EXTERNAL ( NAME ( e=procedure_name
//                        | 'code_specification (delimiterdots)?'
//                        | 'JAR_ID_specification'
                        ) )?
        ( parameter_style_specification )?
        ( EXTERNAL SECURITY ( DEFINER ( authorization_name )? | INVOKER ) )?
    //  You can specify language_and_access_specification and parameter_style_specification in the reverse order.
    ;

//parameter_specification
//    : ( IN | OUT | INOUT )? parameter_name data_type
//    ;
//
//language_and_access_specification
//    : ( language_clause ( SQL_data_access )? |
//        SQL_data_access ( language_clause )? |
//        external_data_access
//      )
//    ;

parameter_style_specification
    : PARAMETER STYLE ( SQL | TD_GENERAL | JAVA )
    ;

//code_specification
//    : ( F delimiter function_entry_name |
//        D |
//        ( S | C ) path_specification
//      )
//    ;
//
//jar_id_specification
//    : JAR_ID':'java_class_name'.'java_method_name
//        ( '(' java_parameter_class (','dots)? ')' 'returns' java_parameter_class )?
//    ;
//
//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('(' precision')')? |
//        INTERVAL DAY ('(' precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')?
//          ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//        PERIOD '('DATE')' |
//        PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB ) '('integer ( G | K | M )?')' |
//        sysudtlib_dot? ( XML | XMLTYPE ) ('('integer ( G | K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//          ( CHARACTER SET ( UNICODE | LATIN ) )? |
//        sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? DATASET ('('integer ( K | M )?')')?
//          ( INLINE LENGTH integer )? storage_format |
//        sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//      ))
//    ;
//
//path_specification
//    : ( I delimiter name_on_server delimiter include_name |
//        L delimiter library_name |
//        O delimiter name_on_server delimiter object_name |
//        P delimiter package_name |
//        S delimiter name_on_server delimiter source_name |
//        NS delimiter source_file delimiter include_file
//      )
//    ;
//
//java_parameter_class
//    : //( primitive ()? ( ()? )?)? | object ( ()? )? )
//    ;
//
//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//        ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

create_procedure
    : ( CREATE | REPLACE ) PROCEDURE ( database_name_dot | user_name_dot )? procedure_name
        '(' ( parameter_specification (','dots)? )? ')'
        sql_data_access
        ( DYNAMIC RESULT SETS number_of_sets )?
        ( SQL SECURITY privilege_option )?
        statement
    ;

//parameter_specification
//    : ( IN | OUT | INOUT )? parameter_name data_type
//    ;

sql_data_access
    : ( CONTAINS SQL | MODIFIES SQL DATA | READS SQL DATA )
    ;

privilege_option
    : ( CREATOR | DEFINER | INVOKER | OWNER )
    ;

statement
    : ( sql_statement |
        BEGIN sql_multistatement_request END REQUEST |
        compound_statement |
        open_statement |
        fetch_statement |
        assignment_statement |
        condition_statement |
        ( label_name ':' )? iteration_statement ( label_name )? |
        diagnostic_statement |
        ITERATE label_name |
        LEAVE label_name
      )
    ;

sql_multistatement_request
    : todo
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('('precision')')? |
//        INTERVAL DAY ('('precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')?
//          ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ('(' precision (',' fractional_seconds_precision )? ')' |
//        PERIOD '('DATE')' |
//        PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '('integer ( G | K | M )?')' |
//        sysudtlib_dot? ( XML | XMLTYPE )
//          ('('integer ( G | K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//          ( CHARACTER SET ( UNICODE | LATIN ) )? |
//        sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//        sysudtlib_dot? DATASET ('('integer ( K | M )?')')?
//          ( INLINE LENGTH integer )? storage_format |
//        sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//      ) )
//    ;

compound_statement
    : ( label_name ':' )? BEGIN
        ( local_declaration (dots)? )?
        ( cursor_declaration (dots)? )?
        ( condition_handler (dots)? )?
        ( statement (dots)? )?
        END ( label_name )?
    ;

open_statement
    : OPEN cursor_name ( USING ( sql_identifier | sql_parameter ) (','dots)? )? ';'
    ;

sql_identifier
    : todo
    ;

sql_parameter
    : todo
    ;

fetch_statement
    : FETCH ( ( NEXT | FIRST )? FROM )? cursor_name
        INTO ( l=variable_name | parameter_reference ) (','dots)? ';'
    ;

assignment_statement
    : SET assignment_target '=' assignment_source
    ;

condition_statement
    : ( case_statement | if_statement )
    ;

iteration_statement
    : ( while_statement | loop_statement | for_statement | repeat_statement )
    ;

diagnostic_statement
    : ( ( SIGNAL signal_specification | RESIGNAL ( signal_specification )? )
          ( SET condition_information_item '=' value )? |

        GET DIAGNOSTICS ( EXCEPTION condition_number )? diagnostic_assignment (','dots)?
      ) ';'
    ;

//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//        ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

local_declaration
    : DECLARE ( variable_name (','dots)? data_type ( DEFAULT ( literal | NULL_ ) )? |
                condition_name CONDITION ( FOR sqlstate_code )?
              )
    ;

cursor_declaration
    : DECLARE cursor_name ( (NO)? SCROLL )? CURSOR
        ( WITHOUT RETURN | WITH RETURN ( ONLY )? ( TO ( CALLER | CLIENT ) )? )?
        FOR ( cursor_specification ( FOR ( READ ONLY | UPDATE ) )? | statement_name )
        ( PREPARE statement_name
          FROM ( string | statement_string_variable ) )?
    ;

condition_handler
    : DECLARE ( ( CONTINUE | EXIT ) HANDLER | condition_name CONDITION )
        ( FOR
          ( sqlstate_specification (','dots)? handler_action_statement |

            ( SQLEXCEPTION | SQLWARNING | NOT FOUND | condition_name ) (','dots)?
              handler_action_statement |

            sqlstate_specification (','dots)?
          )
        )? ';'
    ;

case_statement
    : CASE ( operand when_operand_clause (dots)? |
             when_condition_clause (dots)?
           )
           ( ELSE statement';' (dots)? )?
      END CASE
    ;

if_statement
    : IF conditional_expression THEN statement';' (dots)?
        ( ELSEIF conditional_expression THEN statement';' (dots)? )?(dots)?
        ( ELSE statement';' (dots)? )?
      END IF
    ;

while_statement
    : WHILE conditional_expression
       DO statement';' (dots)?
      END WHILE
    ;

loop_statement
    : LOOP
        statement';' (dots)?
      END LOOP
    ;

for_statement
    : FOR for_loop_variable AS ( cursor_name CURSOR FOR )? cursor_specification
        DO statement';' (dots)?
      END FOR
    ;

repeat_statement
    : REPEAT
        statement';' (dots)? UNTIL conditional_expression
      END REPEAT
    ;

signal_specification
    : ( condition_name | SQLSTATE ( VALUE )? sqlstate_code )
    ;

diagnostic_assignment
    : ( parameter_name | variable_name ) '=' statement_information_item
    ;

cursor_specification
    : SELECT selection (','dots)? FROM source where_clause ( other_SELECT_clause (dots)? )?
    ;

where_clause
    : todo
    ;

sqlstate_specification
    : SQLSTATE ( VALUE )? sqlstate_code
    ;

when_operand_clause
    : WHEN operand THEN statement';' (dots)?
    ;

when_condition_clause
    : WHEN conditional_expression THEN statement';' (dots)?
    ;

//selection
//    : ( '*' | column_name ( (AS)? alias_name )? | expression (AS)? alias_name )
//    ;
//
//source
//    : ( table_name (','dots)? |
//
//        table_name ( INNER | ( LEFT | RIGHT | FULL ) ( OUTER )? ) JOIN
//          table_name ON condition
//      )
//    ;

create_profile
    : CREATE PROFILE profile_name ( AS profile )?
    ;

profile
    : ( ACCOUNT '=' ( string | '(' string (','dots)? ')' | NULL_ ) |
        DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
        DEFAULT DATABASE '=' ( database_name | NULL_ ) |
        SPOOL '=' ( ( number | constant_expression ) (BYTES)? | NULL_ ) |
        TEMPORARY '=' ( ( number | constant_expression ) (BYTES)? | NULL_ ) |
        PASSWORD (ATTRIBUTES)? '=' ( '('attribute (','dots)?')' | NULL_ ) |
        QUERY_BAND '=' //'pair (dots)?'
            ( '('(NOT)? DEFAULT')' )? |
        IGNORE QUERY_BAND VALUES '=' //'pair (dots)?'
        | TRANSFORM '(' transformation (','dots)? ')' |
        COST PROFILE '=' ( c=profile_name | NULL_ ) |
        CONSTRAINT '=' constraint_specification (','dots)?
      ) (','dots)?
    ;

attribute
    : attribute_name '=' ( value | NULL_ )
    ;

pair
    : pair_name '=' pair_value';'
    ;

transformation
    : data_type '=' group_name
    ;

constraint_specification
    : rc=column_name
        '(' ( level_specification (','dots)? | category_name (','dots)? | NULL_ ) ')'
    ;

level_specification
    : level_name (DEFAULT)?
    ;

create_recursive_view
    :  ( CREATE | REPLACE ) RECURSIVE VIEW ( database_name_dot | user_name_dot )? view_name
        '(' column_name (',' column_name)* ')' AS
          ( view_specification | '(' view_specification ')' )
    ;

view_specification
    : ( locking_specification (dots)? )?
      ( date_specification )?
      seed_statement
      UNION ALL ( union_specification (dots)? )?
      recursive_statement
    ;

locking_specification
    : LOCKING item_to_lock ( FOR | IN )? lock_type ( MODE )? ( NOWAIT )?
    ;

date_specification
    : AS OF calendar_function '('
        ( DATE date_expression | TIMESTAMP ( WITH TIME ZONE )? timestamp_expression )
          (',' calendar_name )?
      ')'
    ;

seed_statement
    : ( SELECT | SEL ) ( DISTINCT | ALL )? ( '*' | seed_statement_selection (','dots)? )
        FROM seed_statement_source (','dots)?
        ( WHERE search_condition )?
        ( GROUP BY grouping_specification (','dots)? )?
        ( HAVING having_condition )?
        ( QUALIFY qualify_condition )?
    ;

union_specification
    : ( seed_statement_specification (dots)? | recursive_statement UNION ALL )
    ;

recursive_statement
    : ( SELECT | SEL ) ( ALL )? ( '*'| recursive_statement_selection (','dots)? )
        FROM recursive_statement_source (','dots)?
        ( WHERE search_condition )?
    ;

item_to_lock
    : ( ( DATABASE )? ( database_name | user_name ) |
        ( TABLE )? ( database_name_dot | user_name_dot )? table_name |
        ( VIEW )? ( database_name_dot | user_name_dot )? view_name |
        ROW
      )
    ;

lock_type
    : ( ACCESS |
        ( EXCLUSIVE | EXCL ) |
        SHARE |
        READ ( OVERRIDE )? |
        WRITE |
        LOAD COMMITTED
      )
    ;

seed_statement_selection
    : ( expression | ( database_name_dot | user_name_dot )? table_name'.'* )
    ;

seed_statement_source
    : ( ( database_name_dot | user_name_dot )?
          ( table_name ( (AS)? correlation_name )? |

            joined_table
              ( join_on | CROSS JOIN ( database_name_dot | user_name_dot )? single_table )
          ) |

          '(' subquery ')' (AS)? d=table_name ( '(' column_name (','dots)? ')' )?
      )
    ;

grouping_specification
    : ( ordinary_grouping_set |
        empty_grouping_set |
        rollup_list |
        cube_list |
        grouping_set_specification
      )
    ;

seed_statement_specification
    : seed_statement UNION ALL
    ;

recursive_statement_selection
    : ( expression ( (AS)? correlation_name )? | table_name'.'* )
    ;

recursive_statement_source
    : ( table_name ( (AS)? correlation_name )? |

        joined_table
          ( ( INNER | ( LEFT | RIGHT | FULL ) ( OUTER )? )?
              JOIN joined_table ON search_condition |

            CROSS JOIN single_table
          )
      )
    ;

join_on
    : ( INNER | ( LEFT | RIGHT | FULL ) ( OUTER)? )? JOIN
        ( database_name_dot | user_name_dot )? joined_table ON search_condition
    ;

create_role
    : CREATE (EXTERNAL)? ROLE role_name
    ;

create_schema
    : CREATE storage_format SCHEMA sysudtlib_dot? schema_name AS schema
    ;

create_table
    : CREATE table_kind TABLE table_specification
        ( ',' table_option (','dots)? )? '(' column_partition_definition ')'
        ( index (','dots)? )?
        ( table_preservation )?
    ;

table_kind
    : ( SET | MULTISET )? ( GLOBAL TEMPORARY | VOLATILE )?
    ;

//table_specification
//    : ( database_name_dot | user_name_dot )? table_name
//    ;
//
//table_option
//    : ( MAP '=' map_name (COLOCATE USING colocation_name |
//        (NO)? FALLBACK (PROTECTION)? |
//        WITH JOURNAL TABLE '=' table_specification |
//        (NO)? LOG |
//        ( NO | DUAL )? (BEFORE)? JOURNAL |
//        ( NO | DUAL | LOCAL | NOT LOCAL )? AFTER JOURNAL |
//        CHECKSUM '=' ( DEFAULT | ON | OFF ) |
//        FREESPACE '=' integer (PERCENT)? |
//        mergeblockratio |
//        datablocksize |
//        blockcompression |
//        isolated_loading
//      ) )
//    ;

column_partition_definition
    :(
      column_name data_type ( column_data_type_attribute (','dots)? )? |
       ( COLUMN | ROW )? '(' column_name data_type (column_attributes)? (','dots)? ')'
         ( (NO)? AUTO COMPRESS)? |
       PERIOD FOR period_name '(' pb=column_name ',' pe=column_name ')' |
       normalize_option |
       table_constraint
       )?(','dots)?
    ;

//index
//    : (UNIQUE)? PRIMARY INDEX (index_name)? '(' index_column_name (','dots)? ')' |
//      NO PRIMARY INDEX |
//      PRIMARY AMP (INDEX)? (index_name)? '(' index_column_name (','dots)? ')' |
//      PARTITION BY ( partitioning_level | '(' partitioning_level (','dots)? ')' ) |
//      UNIQUE INDEX ( index_name )? ( '(' index_column_name (','dots)? ')' )? (loading)? |
//      INDEX (index_name)? (ALL)? '(' index_column_name (','dots)? ')' (ordering)? (loading)?
//    ;

table_preservation
    : ON COMMIT ( DELETE | PRESERVE ) ROWS
    ;

mergeblockratio
    : ( DEFAULT MERGEBLOCKRATIO |
        MERGEBLOCKRATIO '=' integer (PERCENT)? |
        NO MERGEBLOCKRATIO
      )
    ;

datablocksize
    : DATABLOCKSIZE '=' (
        data_block_size ( BYTES | KBYTES | KILOBYTES )? |
        ( MINIMUM | MAXIMUM | DEFAULT ) DATABLOCKSIZE
      )
    ;

//blockcompression
//    : BLOCKCOMPRESSION '=' ( AUTOTEMP | MANUAL | ALWAYS | NEVER | DEFAULT )
//         (',' BLOCKCOMPRESSIONALGORITHM '=' ( ZLIB | ELZS_H | DEFAULT ) )?
//         (',' BLOCKCOMPRESSIONLEVEL '=' ( value | DEFAULT ) )?
//    ;

isolated_loading
    : WITH (NO)? (CONCURRENT)? ISOLATED LOADING ( FOR ( ALL | INSERT | NONE ) )?
    ;

column_data_type_attribute
    : ( ( UPPERCASE | UC ) |
        (NOT)? ( CASESPECIFIC | CS ) |
        FORMAT quotestring |
        TITLE quotestring |
        NAMED name |
        DEFAULT ( number | USER | DATE | TIME | NULL_ ) |
        WITH DEFAULT |
        CHARACTER SET server_character_set |
        (NOT)? NULL_ |
        (NOT)? AUTO COLUMN |
        compression_attribute |
        column_constraint_attribute |
        identity_column
      )
    ;

normalize_option
    : NORMALIZE ( ALL BUT '('column_name (','dots)?')' )?
         ON normalize_column
         ( ON ( MEETS OR OVERLAPS | OVERLAPS (OR MEETS)?) )?
    ;

table_constraint
    : CONSTRAINT constraint_name
        ( ( UNIQUE | PRIMARY KEY ) '('column_name (','dots)?')' |
            CHECK '('boolean_condition')' |
            FOREIGN KEY '('referencing_column (','dots)?')' references )
    ;

//partitioning_level
//    : ( partitioning_expression |
//          COLUMN ( (NO)? AUTO COMPRESS |
//          COLUMN ( (NO)? AUTO COMPRESS )? ( ALL BUT )? column_partition )?
//        ) ( ADD constant )?
//    ;

loading
    : WITH (NO)? LOAD IDENTITY
    ;

ordering
    : ORDER BY ( VALUES | HASH )? ( '(' o=column_name ')' )?
    ;

compression_attribute
    : ( NO COMPRESS |

        COMPRESS ( constant | '(' ( constant | NULL_ ) (','dots)? ')' )? |

        COMPRESS USING compress_UDF_name DECOMPRESS
          USING decompress_UDF_name
      )
    ;

column_constraint_attribute
    : ( CONSTRAINT constraint_name )?
        ( UNIQUE | PRIMARY KEY | CHECK '(' boolean_condition ')' | references ) |
        ( row_level_security_constraint_column_name (','dots)? )? CONSTRAINT
    ;

identity_column
    : GENERATE (ALWAYS | BY DEFAULT) AS IDENTITY
         ( '(' START WITH constant |
             INCREMENT BY constant |
             MINVALUE constant//‭	|‬
             NO MINVALUE |
             MAXVALUE constant |
             NO MAXVALUE |
             ( NO )? CYCLE ')' )?
    ;

//references
//    : REFERENCES ( WITH (NO)? CHECK OPTION )? referenced_table_name
//         ( '(' referenced_column_name (','dots)? ')' )?
//    ;
//
//column_partition
//    : '(' ( COLUMN | ROW )? ( column_name | '(' column_name (','dots)? ')' )
//          ((NO)? AUTO COMPRESS)?
//        ')' (','dots)?
//    ;

create_table_as
    : ( CREATE table_kind TABLE | CT ) table_specification
        ( table_option (',' table_option)* )?
        '(' attribute (',' attribute)* ')'
        as_clause
        ( ',' index ( ','? index )? )?
        ( table_preservation )?
    ;

//table_kind
//    : ( SET | MULTISET )? ( GLOBAL TEMPORARY | VOLATILE )?
//    ;
//
//table_specification
//    : ( database_name_dot | user_name_dot )? table_name
//    ;
//
//table_option
//    : ( MAP '=' map_name (COLOCATE USING colocation_name |
//        (NO)? FALLBACK (PROTECTION)? |
//        WITH JOURNAL TABLE '=' table_specification |
//        (NO)? LOG |
//        ( NO | DUAL )? (BEFORE)? JOURNAL |
//        ( NO | DUAL | LOCAL | NOT LOCAL )? AFTER JOURNAL |
//        CHECKSUM '=' ( DEFAULT | ON | OFF ) |
//        FREESPACE '=' integer (PERCENT)? |
//        mergeblockratio |
//        datablocksize |
//        blockcompression |
//        isolated_loading
//      ) )
//    ;
//
//attribute
//    : ( column_specification |
//        ( COLUMN | ROW )? '(' column_specification (','dots)? ')' ( (NO)? AUTOCOMPRESS )? |
//        table_constraint
//      )
//    ;

as_clause
    : AS source_table ( subquery_clause )? WITH (NO)? DATA ( AND (NO)? STATISTICS )?
    ;

//index
//    : UNIQUE? PRIMARY INDEX (index_name)? '(' index_column_name (','dots)? ')' |
//      NO PRIMARY INDEX |
//      PRIMARY AMP (INDEX)? (index_name)? '(' index_column_name (','dots)? ')' |
//      PARTITION BY ( partitioning_level | '(' partitioning_level (','dots)? ')' ) |
//      UNIQUE INDEX ( index_name )? ( '(' index_column_name (','dots)? ')' )? (loading)? |
//      INDEX (index_name)? (ALL)? '(' index_column_name (','dots)? ')' (ordering)? (loading)?
//      (','dots)?
//    ;
//
//table_preservation
//    : ON COMMIT ( DELETE | PRESERVE ) ROWS
//    ;
//
//mergeblockratio
//    : ( DEFAULT MERGEBLOCKRATIO |
//        MERGEBLOCKRATIO '=' integer (PERCENT)? |
//        NO MERGEBLOCKRATIO
//      )
//    ;
//
//datablocksize
//    : DATABLOCKSIZE '=' (
//        data_block_size ( BYTES | KBYTES | KILOBYTES )? |
//        ( MINIMUM | MAXIMUM | DEFAULT ) DATABLOCKSIZE
//      )
//    ;
//
//blockcompression
//    : BLOCKCOMPRESSION '=' ( AUTOTEMP | MANUAL | ALWAYS | NEVER | DEFAULT )
//         (',' BLOCKCOMPRESSIONALGORITHM '=' ( ZLIB | ELZS_H | DEFAULT ) )?
//         (',' BLOCKCOMPRESSIONLEVEL '=' ( value | DEFAULT ) )?
//    ;
//
//isolated_loading
//    : WITH (NO)? (CONCURRENT)? ISOLATED LOADING ( FOR ( ALL | INSERT | NONE ) )?
//    ;

subquery_clause
    : AS source_table WITH (NO)? DATA ( AND (NO)? STATISTICS )?
    ;

//column_specification
//    : column_name ( column_data_type_attribute (dots)? )?
//    ;
//
//partitioning_level
//    : ( partitioning_expression |
//          COLUMN ( (NO)? AUTO COMPRESS |
//          COLUMN ( (NO)? AUTO COMPRESS )? ( ALL BUT )? column_partition )?
//        ) ( ADD constant )?
//    ;
//
//loading
//    : WITH (NO)? LOAD IDENTITY
//    ;
//
//ordering
//    : ORDER BY ( VALUES | HASH )? ( '(' order_column_name ')' )?
//    ;
//
//column_data_type_attribute
//    : ( ( UPPERCASE | UC ) |
//        (NOT)? ( CASESPECIFIC | CS ) |
//        FORMAT quotestring |
//        TITLE quotestring |
//        NAMED name |
//        DEFAULT ( number | USER | DATE | TIME | NULL_ ) |
//        WITH DEFAULT |
//        CHARACTER SET server_character_set |
//        (NOT)? NULL_ |
//        (NOT)? AUTO COLUMN |
//        compression_attribute |
//        column_constraint_attribute |
//        identity_column
//      )
//    ;
//
//compression_attribute
//    : ( NO COMPRESS |
//
//        COMPRESS ( constant | '(' ( constant | NULL_ ) (','dots)? ')' )? |
//
//        COMPRESS USING compress_UDF_name DECOMPRESS
//          USING decompress_UDF_name
//      )
//    ;
//
//column_constraint_attribute
//    : ( CONSTRAINT constraint_name )?
//        ( UNIQUE | PRIMARY KEY | CHECK '(' boolean_condition ')' | references ) |
//        ( row_level_security_constraint_column_name (','dots)? )? CONSTRAINT
//    ;
//
//identity_column
//    : GENERATE (ALWAYS | BY DEFAULT) AS IDENTITY
//         ( '(' START WITH constant |
//             INCREMENT BY constant |
//             MINVALUE constant //‭	|‬
//             NO MINVALUE |
//             MAXVALUE constant |
//             NO MAXVALUE |
//             ( NO )? CYCLE ')' )?
//    ;

create_table_queue_table_form
    : ( CREATE table_kind | CT ) table_specification','
  QUEUE ( table_option (','dots)? )?
  '(' qits_specification ')'
  ( index_specification ((',')?dots)? )?
    ;

//table_kind
//    : ( SET | MULTISET )?
//    ;
//
//table_specification
//    : ( database_name_dot | user_name_dot )? table_name
//    ;
//
//table_option
//    : ( MAP '=' map_name |
//  (NO)? FALLBACK ( PROTECTION )? |
//  NO JOURNAL |
//  FREESPACE '=' integer ( PERCENT )? |
//  data_block_size |
//  blockcompression |
//  CHECKSUM '=' ( ON | OFF | DEFAULT )
//)
//    ;

qits_specification
    : qits_column_name TIMESTAMP ( '(' todo ')' )? NOT NULL_ DEFAULT
    CURRENT_TIMESTAMP ( '(' todo ')' )?
    ( data_type_attribute (dots)? |
    ( CONSTRAINT name )? CHECK '(' boolean_condition ')'
    )?
    qits_attribute (','dots)?
    ;

//index_specification
//    : ( ( UNIQUE )? INDEX ( index_name )? '(' index_column_name (','dots)? ')' |
//
//  ( UNIQUE )? PRIMARY INDEX ( index_name )? '(' primary_index_column (','dots)? ')' |
//
//  INDEX ( index_name )? '(' index_column_name (','dots)? ')'
//    ORDER BY ( VALUES | HASH )? '(' order_column_name ')'
//)
//    ;

data_block_size
    : ( DATABLOCKSIZE '=' integer ( BYTE(S)? | KBYTE(S)? | KILOBYTE(S)? )? |
  ( MINIMUM | MAXIMUM )? DATABLOCKSIZE )
    ;

//blockcompression
//    : BLOCKCOMPRESSION '=' ( AUTOTEMP | MANUAL | ALWAYS | NEVER | DEFAULT )
//  (',' BLOCKCOMPRESSIONALGORITHM '=' ( ZLIB | ELZS_H | DEFAULT ) )?
//  (',' BLOCKCOMPRESSIONLEVEL '=' ( value | DEFAULT ) )?
//    ;
//
//data_type_attribute
//    : ( ( UPPERCASE | UC ) |
//  (NOT)? ( CASESPECIFIC | CS ) |
//  FORMAT quotestring |
//  TITLE quotestring |
//  NAMED name |
//  DEFAULT ( number | USER | DATE | TIME | NULL_ ) |
//  WITH DEFAULT |
//  CHARACTER SET server_character_set
//)
//    ;

qits_attribute
    : ( column_name data_type column_attribute2 (dots)? |
  column_constraint_attributes
)
    ;

qits_column_name
    : todo
    ;

column_attribute2
    : ( data_type_attribute (dots)? |
  (NOT)? NULL_ |
  compression_attributes |
  column_constraint_attributes |
  column_identity_attributes |
)
    ;

column_constraint_attributes
    : ( CONSTRAINT name )?
  ( ( UNIQUE | PRIMARY KEY ) '(' column_name (','dots)? ')' | CHECK '('boolean_condition ')' )
    ;

compression_attributes
    : COMPRESS ( ( constant | NULL_ ) | '(' ( constant | NULL_ ) (','dots)? ')' )?
    ;

column_identity_attributes
    : GENERATED ( ALWAYS | BY DEFAULT ) AS IDENTITY ( '(' identity_attribute (dots)? ')' )?
    ;

identity_attribute
    : ( START WITH |
  INCREMENT BY |
  (NO)? MINVALUE |
  (NO)? MAXVALUE |
  (NO)? CYCLE
)
    ;

create_transform
    : ( CREATE | REPLACE ) TRANSFORM FOR sysudtlib_dot? user_defined_type_name
        transform_specification (dots)?
    ;

transform_specification
    : transform_group '(' TO SQL WITH to_method_or_function FROM SQL WITH from_method_or_function ')'
    ;

to_method_or_function
//from_method_or_function
    : ( specific_method | instance_method | specific_function | function )
    ;

//specific_method
//    : SPECIFIC sysudtlib_dot? specific_method_name FOR sysudtlib_dot? UDT_name
//    ;

instance_method
    : INSTANCE? METHOD sysudtlib_dot? method_name
        '(' ( data_type | sysudtlib_dot? user_defined_type_name )? (','dots)? ')' FOR sysudtlib_dot? user_defined_type_name
    ;

//specific_function
//    : SPECIFIC FUNCTION sysudtlib_dot? specific_function_name
//    ;

function
    : FUNCTION sysudtlib_dot? function_name
        '(' ( data_type | sysudtlib_dot? user_defined_type_name )? (','dots)? ')'
    ;

create_trigger
    : ( CREATE | REPLACE ) TRIGGER ( database_name_dot )? trigger_name
        ( ENABLED | DISABLED )?
        ( BEFORE | AFTER ) ( temporal_option )? triggering_event
        ON ( database_name_dot )? table_name
        ( ORDER integer )?
        REFERENCING reference (dots)?
        ( FOR EACH ( ROW | STATEMENT ) )?
        ( WHEN '(' search_condition ')' )?
        ( trigger_action | BEGIN ATOMIC trigger_action END )
    ;

triggering_event
    : ( INSERT |
        DELETE |
        UPDATE ( OF ( column_name (','dots)? | '(' column_name (','dots)? ')' ) )?
      )
    ;

reference
    : ( OLD ( ROW )? (AS)? old_transition=variable_name |
        NEW ( ROW )? (AS)? new_transition=variable_name |
        ( OLD_TABLE | OLD TABLE ) (AS)? old_transition2=table_name |
        ( NEW_TABLE | NEW TABLE ) (AS)? new_transition2=table_name |
        OLD_NEW_TABLE (AS)? old_new=table_name '(' o=value',' n=value ')'
      )
    ;

trigger_action
    : ( sql_procedure_statement (';'dots)? |
        '(' sql_procedure_statement (';'dots)? ')'
      )
    ;

sql_procedure_statement
    : todo
    ;

//One-Dimensional ARRAY - Teradata Form
create_type_1
    : CREATE TYPE sysudtlib_dot? array_type_name
  AS data_type ARRAY ( number_of_elements )?
  ( DEFAULT NULL_ )?
    ;
//One-Dimensional VARRAY - Oracle-Compatible Form
create_type_2
    : CREATE TYPE sysudtlib_dot? array_type_name
  AS ( VARYING ARRAY | VARRAY ) '(' number_of_elements ')' OF data_type
  ( DEFAULT NULL_ )?
    ;

//Multidimensional Array - Teradata Form
create_type_3
    : CREATE TYPE sysudtlib_dot? array_type_name
  AS data_type ARRAY ( ( l=bound ':' u=bound | maximum_size ) )?
  ( DEFAULT NULL_ )?
    ;
//Multidimensional Array - Oracle-Compatible Form

create_type_4
    : CREATE TYPE sysudtlib_dot? array_type_name
  AS ( VARYING ARRAY | VARRAY ) '(' ( l=bound ':' u=bound | maximum_size ) ')'
    OF data_type ( DEFAULT NULL_ )?
    ;

//Array Data Types
//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//  ( TIME | TIMESTAMP )? ('('fractional_seconds_precision')')? (WITH TIME ZONE)? |
//  INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//  INTERVAL MONTH ('('precision')')? |
//  INTERVAL DAY ('('precision')')?
//    (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? ) |
//  INTERVAL HOUR ('('precision')')?
//    (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//  INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
//  INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//  PERIOD '('DATE')' |
//  PERIOD '('( TIME | TIMESTAMP )')' ('('precision')')? (WITH TIME ZONE)? |
//  REAL |
//  DOUBLE PRECISION |
//  FLOAT ('('integer')')? |
//  NUMBER ('('( integer | '*') (',' integer)?dots ')')? |
//  ( DECIMAL | NUMERIC ) ('('integer (',' integer)?dots')')? |
//  ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//  ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//  LONG VARCHAR |
//  LONG VARGRAPHIC
//)
//    ;

create_type
    : CREATE TYPE ( SYSUDTLIB'.' )? user_defined_type_name AS data_type
        ( CHARACTER SET server_character_set )?
        FINAL ( method_specification )?
    ;

//data_type
//    :
//      ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('(' precision')')? |
//        INTERVAL DAY ('(' precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//        INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//        REAL |
//        DOUBLE PRECISION |
//        FLOAT ('('integer')')? |
//        NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//        ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//        ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//        LONG VARCHAR |
//        LONG VARGRAPHIC |
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '('integer ( G | K | M )?')'
//      )
//    ;

//method_specification
//    : ( INSTANCE )? METHOD ( SYSUDTLIB'.' )? method_name
//        '(' parameter_specification (','dots)? ')'
//        RETURNS returns_parameter_specification ( AS LOCATOR )?
//        ( CAST FROM ( data_type | ( SYSUDTLIB'.' )? UDT_name ) ( AS LOCATOR )? )?
//        ( SPECIFIC ( SYSUDTLIB'.' )? specific_method_name )?
//        ( SELF AS RESULT )?
//        language_and_access_specification
//        type_attribute (dots)?
//      //You can specify language_and_access_specification and type_attribute in the reverse order.
//    ;
//
//parameter_specification
//    : ( parameter_name )? (
//        data_type ( CHARACTER SET server_character_set )? |
//        ( SYSUDTLIB'.' )? UDT_name
//      ) ( AS LOCATOR )?
//    ;

returns_parameter_specification
    : data_type ( CHARACTER SET server_character_set )? |
        ( SYSUDTLIB'.' )? user_defined_type_name
       ( AS LOCATOR )?
    ;

//language_and_access_specification
//    : language_clause SQL_data_access
//      //If language_and_access_specification is before type_attribute',' you can specify language_clause and SQL_data_access in the reverse order.
//    ;

type_attribute
    : ( SPECIFIC ( SYSUDTLIB'.' )? specific_method_name |
        PARAMETER STYLE ( SQL | TD_GENERAL ) |
        (NOT)? DETERMINISTIC |
        CALLED ON NULL_ INPUT |
        RETURNS NULL_ ON NULL_ INPUT
      )
    ;

create_type_struct
    : CREATE TYPE ( SYSUDTLIB'.' )? user_defined_type_name AS '(' attribute_specification (','dots)? ')'
  ( INSTANTIABLE )? NOT FINAL ( method_specification (','dots)? )? (';')?
    ;

attribute_specification
    : attribute_name (
  p=data_type ( CHARACTER SET server_character_set )? |
  user_defined_type_name
)
    ;

//method_specification
//    : ( INSTANCE | CONSTRUCTOR )? METHOD ( SYSUDTLIB'.' )? method_name
//  '(' parameter_specification (','dots)? ')'
//  RETURNS returns_parameter_specification ( AS LOCATOR )?
//  ( CAST FROM ( data_type | ( SYSUDTLIB'.' )? UDT_name ) ( AS LOCATOR )? )?
//  ( SPECIFIC ( SYSUDTLIB'.' )? specific_method_name )?
//  ( SELF AS RESULT )?
//  language_and_access_specification
//  type_attribute (dots)?
//    ;

//You can specify language_and_access_specification and type_attribute in the reverse order.

//parameter_specification
//    : ( parameter_name )? (
//  data_type ( CHARACTER SET server_character_set )? |
//  ( SYSUDTLIB'.' )? UDT_name
//) ( AS LOCATOR )?
//    ;
//
//returns_parameter_specification
//    : data_type ( CHARACTER SET server_character_set )? |
//  ( SYSUDTLIB'.' )? UDT_name
//  ( AS LOCATOR )?
//    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//  ( TIME | TIMESTAMP ) ('(' fractional_seconds_precision')')? (WITH TIME ZONE)? |
//  INTERVAL YEAR ('(' precision')')? (TO MONTH)? |
//  INTERVAL MONTH ('(' precision')')? |
//  INTERVAL DAY ('(' precision')')?
//   (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//  INTERVAL HOUR ('('precision')')?
//    (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ) )? |
//  INTERVAL MINUTE ('('precision')')? ( TO SECOND ('('fractional_seconds_precision')')? )? |
//  INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision)? ')' |
//  PERIOD '('DATE')' |
//  PERIOD '('( TIME | TIMESTAMP ) ('('precision')')? ( WITH TIME ZONE )?')' |
//  REAL |
//  DOUBLE PRECISION |
//  FLOAT ('('integer')')? |
//  NUMBER ('('( integer | '*') (',' integer )?dots')')? |
//  ( DECIMAL | NUMERIC ) ('('integer (',' integer )?dots')')? |
//  ( CHAR | BYTE | GRAPHIC ) ('('integer')')? |
//  ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ('('integer')')? |
//  LONG VARCHAR |
//  LONG VARGRAPHIC |
//  ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//    '('integer ( G | K | M )?')' |
//  sysudtlib_dot? ( XML | XMLTYPE ) ('('integer ( G | K | M )?')')?
//    ( INLINE LENGTH integer )? |
//  sysudtlib_dot? JSON ('('integer ( K | M )?')')? ( INLINE LENGTH integer )?
//    ( CHARACTER SET ( UNICODE | LATIN ) )? |
//  sysudtlib_dot? ST_GEOMETRY ('('integer ( K | M )?')')? ( INLINE LENGTH integer )? |
//  sysudtlib_dot? ( UDT_name | MBR )
//)
//    ;

//language_and_access_specification
//    : language_clause SQL_data_access
//    ;
//
////If language_and_access_specification is before type_attribute',' you can specify language_clause and SQL_data_access in the reverse order.
//
//type_attribute
//    : ( SPECIFIC ( SYSUDTLIB'.' )? specific_method_name |
//  PARAMETER STYLE ( SQL | TD_GENERAL ) |
//  (NOT)? DETERMINISTIC |
//  CALLED ON NULL_ INPUT |
//  RETURNS NULL_ ON NULL_ INPUT
//)
//    ;

create_user
    : CREATE USER user_name ( FROM database_name )? AS
  ( PERMANENT | PERM ) '=' number (BYTES)? ( skew_specification )? (',')?
  PASSWORD '=' temp_password (',')?
  ( user_attribute ((',')?dots)? )? (';')?
    ;

skew_specification
    : SKEW '=' ( constant_expression | DEFAULT ) (PERCENT)?
    ;

user_attribute
    : ( STARTUP '=' string |
  ( TEMPORARY | SPOOL ) '=' ( number | constant_expression ) (BYTES)? ( skew_specification )? |
  DEFAULT DATABASE '=' database_name |
  COLLATION '=' collation_sequence |
  ACCOUNT '=' ( string | '(' string (','dots)? ')' | NULL_ ) |
  DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
  (NO)? FALLBACK (PROTECTION)? |
  ( NO | DUAL )? (BEFORE)? JOURNAL |
  ( NO | DUAL | (NOT)? LOCAL )? AFTER JOURNAL |
  DEFAULT JOURNAL TABLE '=' ( database_name_dot )? table_name |
  TIME ZONE '=' ( LOCAL | ( sign )? string | string | NULL_ ) |
  DATEFORM '=' ( INTEGERDATE | ANSIDATE | NULL_ ) |
  DEFAULT CHARACTER SET server_character_set |
  DEFAULT ROLE '=' ( role_name | NONE | NULL_ | ALL ) |
  PROFILE '=' ( profile_name | NULL_ ) |
  TRANSFORM '(' transform_specification (','dots)?')' |
  DBA |
  CONSTRAINT '=' constraint (','dots)? |
  EXPORTWIDTH ( string | DEFAULT )
)
    ;

//transform_specification
//    : data_type '=' group_name
//    ;
//
//constraint
//    : row_level_security_constraint_column_name (
//  '(' ( level_name (DEFAULT)? | category_name ) (','dots)?')' |
//  '('NULL')'
//)
//    ;

create_view
    : ( CREATE VIEW | CV | REPLACE VIEW ) ( database_name_dot | user_name_dot )? view_name
        ( '(' column_name (','dots)? ')' )? AS
        ( view_specification | '(' view_specification ')' ) (';')?
    ;

//view_specification
//    : ( locking_clause dots )?
//      ( as_of_clause )?
//      ( WITH ( nonrecursive_with_modifier | recursive_with_modifier ) (','dots)? )?
//      SELECT selection
//      FROM source (','dots)?
//      ( WHERE search_condition )?
//      ( GROUP BY group_specification (','dots)? )?
//      ( HAVING having_condition )?
//      ( QUALIFY qualify_condition )?
//      ( WITH CHECK OPTION )?
//      ( ORDER BY order_by_specification (','dots)? )?
//    ;

locking_clause
    : LOCKING item_to_lock ( FOR | IN )? lock_type ( MODE )? ( NOWAIT )?
    ;

as_of_clause
    : AS OF calendar_function '('
        ( DATE | TIMESTAMP ( WITH TIME ZONE )? ) expression (',' s=calendar_name )? ')'
    ;

nonrecursive_with_modifier
    : query_name ( '(' column_name (','dots)? ')' )? AS '(' select_expression ')'
    ;

recursive_with_modifier
    : RECURSIVE query_name ( '(' column_name (','dots)? ')' )? AS '('
        seed union_specification (union_specification dots)?
      ')'
    ;

//selection
//    : ( DISTINCT | ALL )? ( TOP ( n | m PERCENT ) )? ( WITH TIES )?
//        ( '*' | sub_selection (','dots)? )
//    ;
//
//source
//    : ( ( database_name_dot | user_name_dot )? ( table_name | view_name ) ( (AS)? correlation_name )? |
//
//        joined_table_source |
//
//        derived_table_source
//      )
//    ;

group_specification
    : ( ordinary_grouping_set |
        empty_grouping_set |
        rollup_list |
        cube_list |
        grouping_sets_specification
      )
    ;

order_by_specification
    : ( expression |

        ( ( ( database_name_dot | user_name_dot )? table_name'.' )? column_name |
          cn=alias |
          column_position
        ) ( ASC | DESC )?
      )
    ;

//item_to_lock
//    : ( ( DATABASE )? ( database_name | user_name ) |
//        ( TABLE )? ( database_name_dot | user_name_dot )? table_name |
//        ( VIEW )? ( database_name_dot | user_name_dot )? view_name |
//        ROW
//      )
//    ;
//
//lock_type
//    : ( ACCESS |
//        ( EXCLUSIVE | EXCL ) |
//        SHARE |
//        READ ( OVERRIDE )? |
//        WRITE |
//        LOAD COMMITTED
//      )
//    ;

seed
    : ( SELECT | SEL ) ( DISTINCT | ALL )? ( '*' | seed_selection (','dots)? )
        FROM seed_source
        WHERE wcs=search_condition
        ( GROUP BY group_specification (','dots)? )?
        ( ( HAVING | QUALIFY ) hq=search_condition )?
        ( ORDER BY order_by_specification (','dots)? )?
    ;

//union_specification
//    : UNION ALL ( seed | recursive )
//    ;

sub_selection
    : ( expression ( (AS)? e=alias )? |
        ( database_name_dot | user_name_dot )? table_name'.' '*'
      )
    ;

joined_table_source
    : joined_table ( ( INNER | ( LEFT | RIGHT | FULL ) ( OUTER )? )?
                       JOIN joined_table ON search_condition |

                       CROSS JOIN single_table
                  )
    ;

derived_table_source
    : '(' subquery ')' AS? d=table_name ( '(' column_name (','dots)? ')' )?
    ;

seed_selection
    : ( expression ( AS? e=alias )? |
        table_name'.' '*'
      )
    ;

seed_source
    : ( table_name ( (AS)? correlation_name )? |
        joined_table_source |
        derived_table_source
      )
    ;

recursive
    : ( SELECT | SEL ) ( '*' | seed_selection (','dots)? )
        FROM ( implicit_join (','dots)? | explicit_join )
        WHERE wsc=search_condition
    ;

implicit_join
    : ( query_name_specifier (dots)? | table_name ) ( (AS)? correlation_name )?
    ;

explicit_join
    : ( ( query_name | join_table_name ) LEFT ( OUTER )? JOIN joined_table |

        join_table_name RIGHT ( OUTER )? JOIN ( query_name | joined_table ) |

        query_name INNER JOIN joined_table |

        join_table_name INNER JOIN query_name

      ) ON search_condition
    ;

query_name_specifier
    : query_name ( (AS)? correlation_name )?
    ;

create_zone
    : CREATE ZONE zone_name ( ROOT ( database_name | user_name ) )?
    ;

database
    : DATABASE database_name
    ;

delete_database
    : ( DELETE | DEL )? DATABASE database_name (ALL)?
    ;

delete_user
    : ( DELETE | DEL ) user_name (ALL)?
    ;

drop_autorization
    : DROP AUTHORIZATION ( database_name_dot )? authorization_name
    ;

drop_cast
    : DROP CAST ( database_name_dot )? '(' s=data_type AS t=data_type ')'
    ;

drop_constraint
    : DROP CONSTRAINT constraint_name
    ;

drop_database
    : DROP DATABASE database_name
    ;

drop_error_table
    : DROP ERROR TABLE (
        FOR ( database_name_dot | user_name_dot )? data_table_name |
        ( database_name_dot | user_name_dot )? error_table_name
      )
    ;

drop_function
    : DROP ( SPECIFIC FUNCTION ( database_name_dot )? specific_function_name |
             FUNCTION ( database_name_dot )? function_name ( '(' data_type (','dots)? ')' )?
           )
    ;

drop_function_mapping
    : DROP FUNCTION MAPPING ( database_name_dot | user_name_dot )? function_mapping_name
    ;

drop_glop_set
    : DROP GLOP SET ( database_name_dot | user_name_dot )? glop_set_name
    ;

drop_hash_index
    : DROP HASH INDEX ( database_name_dot )? hash_index_name
    ;

drop_index
    : DROP INDEX ( index_specification | index_definition )
    ;

//index_specification
//    : ( database_name_dot )? index_name
//        ON ( table_specification | join_index_specification )
//    ;

index_definition
    : ( '(' column_name (','dots)? ')' | ( database_name_dot )? index_name )
        ( order_clause )?
        ON ( table_specification | join_index_specification )
    ;

//table_specification
//    : ( TEMPORARY )? ( database_name_dot )? table_name
//    ;
//
//join_index_specification
//    : ( database_name_dot )? join_index_name
//    ;

order_clause
    : ORDER BY ( VALUES | HASH )? '(' column_name ')'
    ;

drop_join_index
    : DROP JOIN INDEX ( database_name_dot )? join_index_name
    ;

drop_macro
    : DROP MACRO ( database_name_dot | user_name_dot )? macro_name
    ;

drop_map
    : DROP MAP map_name
    ;

drop_ordering
    : DROP ORDERING ( database_name_dot )? user_defined_type_name
    ;

drop_procedure
    : DROP PROCEDURE ( database_name_dot | user_name_dot )? procedure_name
    ;

drop_profile
    : DROP PROFILE profile_name
    ;

drop_role
    : DROP (EXTERNAL)? ROLE ( database_name_dot )? role_name
    ;

drop_schema
    : DROP storage_format SCHEMA sysudtlib_dot? schema_name
    ;

drop_statistics
    : DROP ( STATISTICS | STATS | STAT ) ( statistic )?(','dots)? ON indexed_item
    ;

statistic
    : ( (UNIQUE)? INDEX ( index_name )? (ALL)? '(' column_name (','dots)? ')'
          ( ORDER BY ( VALUES | HASH )? '(' column_name ')' )? |

        COLUMN ( column_index | '(' column_index (','dots)? ')' )
      )
    ;

indexed_item
    : ( (TEMPORARY)? ( database_name_dot | user_name_dot )? table_name |
        ( database_name_dot | user_name_dot )? ( join_index_name | hash_index_name )
      )
    ;

column_index
    : ( column_name ( AS? statistics_name )? | PARTITION )
    ;

drop_table
    : DROP (TEMPORARY)? (FOREIGN)? TABLE ( database_name_dot | user_name_dot )? table_name (ALL)?
    ;

drop_transform
    : DROP TRANSFORM ( database_name_dot )? ( transform_group_name | ALL )
        FOR user_defined_type_name
    ;

drop_trigger
    : DROP TRIGGER ( database_name_dot | user_name_dot )? trigger_name
    ;

drop_type
    : DROP TYPE ( database_name_dot )? user_defined_type_name
    ;

drop_user
    : DROP USER user_name
    ;

drop_view
    : DROP VIEW ( database_name_dot | user_name )? view_name
    ;

drop_zone
    : DROP ZONE zone_name
    ;

end_isolated_logging
    : END (CONCURRENT)? ISOLATED LOADING FOR
        QUERY BAND //'LDILoadGroup = value'
        ( OVERRIDE SESSION )?
    ;

end_logging
    : END LOGGING ( DENIALS )? ( WITH TEXT )? ON operation_specification
        ( FOR CONSTRAINT constraint_name )?
        ( BY user_name (','dots)? )?
        ( ON logged_item (','dots)? )?
    ;

operation_specification
    : ( ( operation | GRANT )(','dots)? | ALL )
    ;

logged_item
    : ( AUTHORIZATION authorization_name |

        DATABASE database_name |

        USER user_name |

        ( TABLE |
          VIEW |
          MACRO |
          PROCEDURE |
          FUNCTION |
          FUNCTION WRAPPING |
          TYPE
        ) ( database_name_dot | user_name_dot )? object_name
      )
    ;

end_query_capture
    : END QUERY CAPTURE
    ;

end_query_logging
    : END QUERY LOGGING ON (
        account_specification |
        ( user_name | database_name ) (','dots)? |
        application_specification
      )
    ;

account_specification
    : ( ALL | user_name ) ( ACCOUNT '=' ( string | '(' string (','dots)?')' ) )?
    ;

application_specification
    : APPLNAME '=' ( string | '(' string (','dots)?')' )
    ;

flush_query
    : FLUSH QUERY LOGGING WITH flush_option
    ;

help_cast
    : HELP CAST sysudtlib_dot? user_defined_type_name ( SOURCE | TARGET )?
    ;

help_column_1
// Column or Column List from Table
    : HELP COLUMN column_name (','dots)? FROM source (','dots)?
    ;

help_column_2
//All Columns from Table
    : HELP COLUMN * FROM source (','dots)?
    ;

help_column_3
//Table Columns
    : HELP COLUMN column_spec (','dots)?
    ;

help_column_4
//All Columns in Tables or Indexes
    : HELP COLUMN column_spec (','dots)?
    ;

help_column_5
//Column Expression
    : HELP COLUMN expression (','dots)?
    ;

help_column_6
//Column Expression from Table or Index
    : HELP COLUMN column_spec (','dots)?
    ;

help_column_7
//Column from Error Table for Data Table
    : HELP COLUMN column_name FROM ERROR TABLE FOR ( database_name_dot | user_name_dot )? data_table_name
    ;

help_column_8
//Column from Error Table
    : HELP COLUMN column_name FROM ( database_name_dot | user_name_dot )? error_table_name
    ;

//source
//    : ( database_name_dot | user_name_dot )?
//        ( table_name | join_index_name | hash_index_name )
//    ;

column_spec
    : source'.'column_name
    ;

help_constraint
    : HELP CONSTRAINT ( database_name_dot | user_name_dot )?
        ( table_name'.' | view_name'.' ) constraint_name
    ;

help_database
    : HELP DATABASE database_name
    ;
help_error_table
    : HELP ERROR TABLE FOR ( database_name_dot | user_name_dot)? data_table_name
    ;

help_function
    : HELP ( SPECIFIC FUNCTION ( database_name_dot | user_name_dot )? specific_function_name |
             FUNCTION ( database_name_dot | user_name_dot )? function_name ('('data_type (','dots)?')')?
           )
    ;

help_hash_index
    : HELP HASH INDEX ( database_name_dot | user_name_dot )? hash_index_name
    ;

help_index
    : HELP INDEX (
        (TEMPORARY)? ( database_name_dot | user_name_dot )? table_name |
        ( database_name_dot | user_name_dot )? ( join_index_name | hash_index_name | view_name )
      ) ( '(' column_name (','dots)? ')' )?
    ;

help_join_index
    : HELP JOIN INDEX ( database_name_dot | user_name_dot )? join_index_name
    ;

help_macro
    : HELP MACRO ( database_name_dot | user_name_dot )? macro_name
    ;

help_method
    : HELP ( method_clause | specific_method_clause )
    ;

//method_clause
//    : ( INSTANCE | CONSTRUCTOR )? METHOD (database_name_dot)? method_name
//        ( '(' user_defined_type_name (','dots)? ')' )? FOR user_defined_type_name
//    ;
//
//specific_method_clause
//    : SPECIFIC METHOD (database_name_dot)?specific_method_name
//    ;

help_online
    : HELP ( 'HELP' |
             ( SQL |
                ARCHIVE |
                DUMP |
                FASTEXPORT |
                FASTLOAD |
                MULTILOAD |
                PMPC |
                TPCCONS |
                SPL
             ) ( command_name )?
      )
    ;

help_procedure
    : HELP PROCEDURE ( database_name_dot)? procedure_name ( ATTRIBUTES | ATTR | ATTRS )?
    ;

help_session
    : HELP SESSION (CONSTRAINT)?
    ;

help_statistics
    : HELP (CURRENT)? STATISTICS (ON)? (
        (TEMPORARY)? ( database_name_dot | user_name_dot )? table_name |
        ( database_name_dot | user_name_dot ) ( join_index_name | hash_index_name )
      )
    ;

help_statistics_qcd
    : HELP (CURRENT)? STATISTICS
        (ON)? ( database_name_dot | user_name_dot )? object_name

        FROM qcd_name
        ( FOR QUERY query_id )?

        ( SAMPLID statistics_id )?
        ( USING MODIFIED )?
    ;

help_schema
    : HELP storage_format SCHEMA sysudtlib_dot? schema_name
    ;

help_table
    : HELP TABLE ( database_name_dot | user_name_dot)? ( table_name | error_table_name ) ( LAYER '('layer_name')')?
    ;

help_transform
    : HELP TRANSFORM ( database_name_dot | user_name_dot )? user_defined_type_name
    ;

help_trigger
    : HELP TRIGGER ( database_name_dot | user_name_dot )? ( trigger_name | table_name )
    ;

help_type
    : HELP TYPE sysudtlib_dot? user_defined_type_name ( ATTRIBUTE | METHOD )?
    ;

help_user
    : HELP USER user_name
    ;

help_view
    : HELP VIEW ( database_name_dot | user_name )? view_name
    ;

help_volatile_table
    : HELP VOLATILE TABLE ( v=table_name )?
    ;

incremental_restore
    : INCREMENTAL RESTORE ALLOW WRITE FOR object_list
    ;

object_list
    : ( database_name('.'table_name)? | user_name('.'table_name)? | table_name ) (','dots)?
    ;

logging_incremental_archive_off
    : LOGGING INCREMENTAL ARCHIVE OFF FOR object_list
    ;

//object_list
//    : ( database_name('.'table_name)? | user_name('.'table_name)? | table_name ) (','dots)?
//    ;

logging_incremental_archive_on
    : LOGGING INCREMENTAL ARCHIVE ON FOR object_list (','dots)? (',' DELETE LOG ROWS )?
    ;
//
//object_list
//    : ( database_name('.'table_name)? | user_name('.'table_name)? | table_name ) (','dots)?
//    ;

modify_database
    : MODIFY DATABASE database_name AS database_attribute ((',')?dots)?
    ;

//database_attribute
//    : ( ( PERMANENT | PERM | SPOOL | TEMPORARY ) '=' ( n | constant_expression ) (BYTES)?
//          ( SKEW '=' ( constant_expression | DEFAULT ) ( PERCENT )? )? |
//        ACCOUNT '=' 'account_string' |
//        DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
//        (NO)? FALLBACK (PROTECTION)? |
//        ( NO | DUAL )? (BEFORE)? JOURNAL |
//        ( NO | DUAL | (NOT)? LOCAL )? AFTER JOURNAL |
//        DEFAULT JOURNAL TABLE '=' ( database_name_dot )? table_name |
//        DROP DEFAULT JOURNAL TABLE '=' ( database_name_dot )? table_name
//      )
//    ;

modify_profile
    : MODIFY PROFILE profile_name AS profile
    ;

//profile
//    : ( ACCOUNT '=' ( 'account_string' | '(''account_string' (','dots)? ')' | NULL_ ) |
//
//        DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
//
//        DEFAULT DATABASE '=' ( database_name | NULL_ ) |
//
//        SPOOL '=' ( ( n | constant_expression ) (BYTES)? | NULL_ ) |
//
//        TEMPORARY '=' ( ( n | constant_expression ) (BYTES)? | NULL_ ) |
//
//        PASSWORD (ATTRIBUTES)? '=' ( '('attribute (','dots)?')' | NULL_ ) |
//
//        QUERY_BAND '=' ( 'pair (dots)?' ( '('(NOT)? DEFAULT')' )? | '(' (NOT)? DEFAULT ')' | NULL_ )
//
//        IGNORE QUERY_BAND VALUES '=' 'pair (dots)?' |
//
//        TRANSFORM '(' ( transformation (','dots)? )? ')' |
//
//        COST PROFILE '=' ( cost_profile_name | NULL_ ) |
//
//        CONSTRAINT '=' constraint_specification (','dots)?
//      )
//    ;
//
//attribute
//    : attribute_name '=' ( value | NULL_ )
//    ;
//
//pair
//    : pair_name '=' pair_value
//    ;
//
//transformation
//    : data_type '=' group_name
//    ;
//
//constraint_specification
//    : row_level_security_constraint_name '('( level_specification (','dots)? | category_name (','dots)? | NULL_ )')'
//    ;
//
//level_specification
//    : level_name (DEFAULT)?
//    ;

modify_user
    : MODIFY USER user_name AS user_attribute ((',')?dots)?
    ;

//user_attribute
//    : ( ( PERMANENT | PERM | TEMPORARY | SPOOL ) '=' ( n | constant_expression ) (BYTES)?
//           ( SKEW ( constant_expression | DEFAULT ) (PERCENT)?)? |
//        STARTUP '=' ( NULL_ | quotestring ) |
//        PASSWORD '=' password (FOR USER)? |
//        RELEASE PASSWORD LOCK |
//        ACCOUNT '=' ( 'account_string' | '(''account_string' (','dots)?')' | NULL_ ) |
//        DEFAULT MAP '=' ( map_name | NULL_ ) ( OVERRIDE (NOT)? ON ERROR )? |
//        DEFAULT DATABASE '=' database_name |
//        COLLATION '=' collation_sequence |
//        (NO)? FALLBACK (PROTECTION)? |
//        ( NO | DUAL )? (BEFORE)? JOURNAL |
//        ( NO | DUAL | (NOT)? LOCAL )? AFTER JOURNAL |
//        DEFAULT JOURNAL TABLE '=' ( database_name_dot )? table_name |
//        DROP DEFAULT JOURNAL TABLE ( '=' table_name )? |
//        TIME ZONE '=' ( LOCAL | (sign)? 'quotestring' | 'time_zone_string' | NULL_ ) |
//        DATEFORM '=' ( INTEGERDATE | ANSIDATE | NULL_ ) |
//        DEFAULT CHARACTER SET server_character_set |
//        DEFAULT ROLE '=' ( role_name | NONE | NULL_ | ALL ) |
//        PROFILE '=' ( profile_name | NULL_ ) |
//        TRANSFORM '(' ( transform_specification (','dots)? )? ')' |
//        (NOT)? DBA |
//        EXPORTWIDTH ( 'export_definition_name' | DEFAULT ) |
//        CONSTRAINT '=' constraint (','dots)?
//      )
//    ;

//transform_specification
//    : data_type '=' group_name
//    ;
//
//constraint
//    : row_level_security_constraint_column_name (
//        '(' ( level_name (DEFAULT)? | category_name ) (','dots)?')' |
//        '('NULL')'
//      )
//    ;

rename_function_external
    : RENAME (
        SPECIFIC FUNCTION ( database_name_dot )? specific_function_name
          ( TO | AS ) specific_function_name |

        FUNCTION ( database_name_dot )? function_name ( '(' data_type (','dots)? ')' )?
          ( TO | AS ) function_name
      )
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//        ( TIME | TIMESTAMP ) ( '('fractional_seconds_precision')' )? (WITH TIME ZONE)? |
//        INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//        INTERVAL MONTH ('('precision')')? |
//        INTERVAL DAY ('('precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//        INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//
//        PERIOD '('DATE')' |
//
//        PERIOD '(' ( TIME | TIMESTAMP ) ('('precision')')? (WITH TIME ZONE)? ')' |
//
//        REAL |
//
//        DOUBLE PRECISION |
//
//        FLOAT ( '('integer')' )? |
//
//        NUMBER ( '(' ( integer | '*') (',' integer)?dots ')' )? |
//
//        ( DECIMAL | NUMERIC ) ( '(' integer (',' integer)?dots ')' )? |
//
//        ( CHAR | BYTE | GRAPHIC ) ( '('integer')' )? |
//
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ( '('integer')' )? |
//
//        LONG VARCHAR |
//
//        LONG VARGRAPHIC |
//
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '(' integer ( G | K | M )? ')' |
//
//        ( XML | XMLTYPE ) |
//
//        JSON ( '(' integer ')' )? ( CHARACTER SET ( UNICODE | LATIN ) )? |
//
//        sysudtlib_dot? ( UDT_name | ST_Geometry | MBR | ARRAY_name | VARRAY_name )
//      )
//    ;

rename_function_sql
    : RENAME (
        SPECIFIC FUNCTION ( database_name_dot )? specific_function_name
          ( TO | AS ) specific_function_name |

        FUNCTION ( database_name_dot )? function_name ( '(' data_type (','dots)? ')' )?
          ( TO | AS ) function_name
      )
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//
//        ( TIME | TIMESTAMP ) ( '('fractional_seconds_precision')' )? (WITH TIME ZONE)? |
//
//        INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//
//        INTERVAL MONTH ('('precision')')? |
//
//        INTERVAL DAY ('('precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//
//        INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
//
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//
//        PERIOD '('DATE')' |
//
//        PERIOD '(' ( TIME | TIMESTAMP ) ('('precision')')? (WITH TIME ZONE)? ')' |
//
//        REAL |
//
//        DOUBLE PRECISION |
//
//        FLOAT ( '('integer')' )? |
//
//        NUMBER ( '(' ( integer | '*') (',' integer)?dots ')' )? |
//
//        ( DECIMAL | NUMERIC ) ( '(' integer (',' integer)?dots ')' )? |
//
//        ( CHAR | BYTE | GRAPHIC ) ( '('integer')' )? |
//
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) ( '('integer')' )? |
//
//        LONG VARCHAR |
//
//        LONG VARGRAPHIC |
//
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '(' integer ( G | K | M )? ')' |
//
//        ( XML | XMLTYPE ) |
//
//        JSON ( '(' integer ')' )? ( CHARACTER SET ( UNICODE | LATIN ) )? |
//
//        sysudtlib_dot? ( UDT_name | ST_Geometry | MBR | ARRAY_name | VARRAY_name )
//      )
//    ;

rename_macro
    : RENAME MACRO ( database_name_dot | user_name_dot )? o=macro_name
        ( TO | AS ) ( database_name_dot | user_name_dot )? n=macro_name
    ;

rename_procedure
    : RENAME PROCEDURE ( database_name_dot | user_name_dot )? o=procedure_name
        ( TO | AS ) ( database_name_dot | user_name_dot )? n=procedure_name
    ;

rename_table
    : RENAME TABLE ( database_name_dot | user_name_dot )? o=table_name
        ( TO | AS ) ( database_name_dot | user_name_dot )? n=table_name
    ;

rename_trigger
    : RENAME TRIGGER ( database_name_dot | user_name_dot )? o=name
        ( TO | AS ) ( database_name_dot | user_name_dot )? n=name
    ;

rename_view
    : RENAME VIEW ( database_name_dot | user_name_dot )? o=view_name
        ( TO | AS )? ( database_name_dot | user_name_dot )? n=view_name
    ;

replace_method
    : REPLACE ( specific_method_clause | method_clause )
        EXTERNAL ( NAME ( e=method_name | ',' item_list delimiter dots? ',' ) )? ';'?
    ;

//specific_method_clause
//    : SPECIFIC METHOD sysudtlib_dot? specific_method_name
//    ;
//
//method_clause
//    : (INSTANCE | CONSTRUCTOR ) METHOD (SYSUDTLIB' .') method_name
//          '(' parameter_name? ( data_type | sysudtlib_dot?UDT_name )? ( AS LOCATOR )? ')'
//          FOR sysudtlib_dot UDT_name
//    ;
//
//item_list
//    : ( S | C ) s_or_c_item
//    ;

//s_or_c_item
//    : ( I delimiter name_on_server delimiter include_name |
//        L delimiter library_name |
//        O delimiter name_on_server delimiter object_name |
//        S delimiter name_on_server delimiter source_name
//      )
//    ;

replace_query_logging
    : REPLACE QUERY LOGGING
        ( WITH with_item (','dots)? )?
        ( MODE_ = number )?
        ( LIMIT limit_item (AND limit_item)? )?
        ON on_items ';'?
    ;

//with_item
//    : ( ALL |
//        EXPLAIN |
//        LOCK = number |
//        NONE |
//        ( NO COLUMNS )? OBJECTS |
//        PARAMINFO |
//        FEATUREINFO |
//        SQL |
//        DETAILED? STATSUSAGE |
//        STEPINFO |
//        USECOUNT |
//        UTILITYINFO |
//        VERBOSE? XMLPLAN
//      )?
//    ;

//limit_item
//    : ( SQLTEXT ('='n)? |
//        ( SUMMARY '=' n1',' n2',' n3 | THRESHOLD ('='n)? )
//          ( CPUTIME | CPUTIMENORM | ELAPSEDSEC | ELAPSEDTIME | IOCOUNT )?
//      )
//    ;

//on_items
//    : ( ( ALL | user_name ) [ ACCOUNT = { 'account_string' | ('account_string' [,dots]) } ]
//    |
//        { user_name | database_name }[,dots]
//        |
//          APPLNAME'=' { 'application_name' | ('application_name' [,dots])
//        }
//      )
//    ;

set_query_band
    : SET QUERY_BAND '=' //( 'band_specification (dots)?' | NONE )
        UPDATE?
        FOR ( SESSION VOLATILE? | TRANSACTION )
    ;

band_specification
    : pair_name '=' pair_value';'
    ;

set_role
    : SET ROLE ( role_name | EXTERNAL | NONE | NULL_ | ALL )
    ;

set_session
    : ( SET SESSION | SS ) KEYWORD '='? value
    ;

set_session_account
    : ( SET SESSION | SS ) ACCOUNT '=' string FOR ( SESSION | REQUEST )
    ;

set_session_calendar
    : ( SET SESSION | SS ) CALENDAR '=' calendar_name
    ;

set_session_character_set
    : ( SET SESSION | SS ) CHARACTER SET UNICODE PASS THROUGH ( ON | OFF )?
    ;

set_session_collation
    : ( SET SESSION | SS ) COLLATION collation_sequence
    ;

set_session_constraint
    : ( SET SESSION | SS ) constraint (','dots)?
    ;

//constraint
//    : CONSTRAINT= row_level_security_constraint_name (
//        '(' level_name | category_name (','dots)? | NULL_ ')'
//      )
//    ;

set_session_database
    : ( SET SESSION | SS ) DATABASE database_name
    ;

set_session_dateform
    : ( SET SESSION | SS ) DATEFORM '=' ( ANSIDATE | INTEGERDATE )?
    ;

set_session_debug_function
    : ( SET SESSION | SS ) DEBUG (
        FUNCTION ( database_name_dot | user_name_dot )? function_name |
        PROCEDURE ( database_name_dot | user_name_dot )? procedure_name |
        METHOD ( database_name_dot | user_name_dot )? method_name
        )
        ( ON | OFF )
    ;

set_session_dot_notation
    : SET SESSION DOT NOTATION (
        DEFAULT |
        LIST |
        NULL_ |
        ERROR
        ) ON ERROR
    ;

set_session_for_isolated_loading
    : ( SET SESSION | SS ) FOR (NO)? (CONCURRENT)? ISOLATED LOADING
    ;

set_session_function_trace
    : SET SESSION FUNCTION TRACE
        ( trace_enabling_specification | OFF )
    ;

trace_enabling_specification
    : USING mask_string FOR (TRACE)? TABLE ( database_name_dot )? table_name
    ;

set_session_json_ignore_errors
    : ( SET SESSION | SS ) JSON IGNORE ERRORS ( ON | OFF )
    ;

set_session_searchuifdbpath
    : SET SESSION SEARCHUIFDBPATH '=' ( database_name | user_name )?(','dots)?
    ;

set_session_transaction_isolation_level
    : SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL isolation_level
    ;

isolation_level
    : ( READ UNCOMMITTED | RU | SERIALIZABLE | SR )
    ;

set_time_zone
    : SET TIME ZONE (
        LOCAL |
        USER |
        expression |
        ( sign )? string |
        string
      )
    ;

set_transform_group_for_type
    : SET TRANSFORM GROUP FOR TYPE user_defined_type_name transform_group
    ;

show_function_mapping
    : SHOW (IN XML)? FUNCTION MAPPING ( database_name_dot | user_name_dot )? function_mapping_name
    ;

show_map
    : SHOW ( IN XML )? MAP map_name
    ;

show_object
    : SHOW (
        ( IN XML )? HASH INDEX ( database_name_dot | user_name_dot )? hash_index_name |
        ( IN XML )? JOIN INDEX ( database_name_dot | user_name_dot )? join_index_name |
        MACRO ( database_name_dot | user_name_dot )? macro_name |
        ( TEMPORARY )? TABLE ( database_name_dot | user_name_dot )? table_name |
        ERROR TABLE FOR ( database_name_dot | user_name_dot )? data_table_name |
        ( IN XML )? TABLE ( database_name_dot | user_name_dot )? error_table_name |
        TRIGGER ( database_name_dot | user_name_dot )? trigger_name |
        ( IN XML )? VIEW ( database_name_dot | user_name_dot )? view_name |
        PROCEDURE ( database_name_dot | user_name_dot )? procedure_name |
        SPECIFIC FUNCTION ( database_name_dot | user_name_dot )? specific_function_name |
        FUNCTION ( database_name_dot | user_name_dot )? function_name
         ( '(' ( data_type | user_defined_type_name )(','dots)? ')' )? |
        SPECIFIC METHOD sysudtlib_dot? specific_method_name |
        method |
        CAST sysudtlib_dot? user_defined_type_name |
        TYPE sysudtlib_dot? ( user_defined_type_name | array_name | varray_name ) |
        storage_format SCHEMA sysudtlib_dot? schema_name |
        FILE ( uifd=database_name_dot | uifu=user_name_dot )? uifn=name |
        CONSTRAINT constraint_name |
        AUTHORIZATION authorization_name |
        GLOP SET glop_set_name
      )
    ;

//data_type
//    : ( INTEGER | SMALLINT | BIGINT | BYTEINT | DATE |
//
//        ( TIME | TIMESTAMP ) ( '('fractional_seconds_precision')' )? (WITH TIME ZONE)? |
//
//        INTERVAL YEAR ('('precision')')? (TO MONTH)? |
//
//        INTERVAL MONTH ('('precision')')? |
//
//        INTERVAL DAY ('('precision')')?
//          (TO ( HOUR | MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//
//        INTERVAL HOUR ('('precision')')?
//          (TO ( MINUTE | SECOND ('('fractional_seconds_precision')')? ))? |
//
//        INTERVAL MINUTE ('('precision')')? (TO SECOND ('('fractional_seconds_precision')')?)? |
//
//        INTERVAL SECOND ( '(' precision (',' fractional_seconds_precision )? ')' |
//
//        PERIOD '('DATE')' |
//
//        PERIOD '(' ( TIME | TIMESTAMP ) ('('precision')')? (WITH TIME ZONE)? ')' |
//
//        REAL |
//
//        DOUBLE PRECISION |
//
//        FLOAT ( '('integer')' )? |
//
//        NUMBER ( '(' ( integer | '*') (',' integer )? ')' )? |
//
//        ( DECIMAL | NUMERIC ) ( '(' integer (',' integer )? ')' )? |
//
//        ( CHAR | BYTE | GRAPHIC ) ( '('integer')' )? |
//
//        ( VARCHAR | CHAR VARYING | VARBYTE | VARGRAPHIC ) '('integer')' |
//
//        LONG VARCHAR |
//
//        LONG VARGRAPHIC |
//
//        ( BINARY LARGE OBJECT | BLOB | CHARACTER LARGE OBJECT | CLOB )
//          '(' integer ( G | K | M )? ')' |
//
//        sysudtlib_dot? ( XML | XMLTYPE ) ( '(' integer ( G | K | M )? ')' )?
//          ( INLINE LENGTH integer )? |
//
//        sysudtlib_dot? JSON ( '(' integer ( K | M )? ')' )? ( INLINE LENGTH integer )?
//          ( CHARACTER SET ( UNICODE | LATIN ) | STORAGE FORMAT ( BSON | UBJSON ) )? |
//
//        sysudtlib_dot? ST_GEOMETRY ( '('integer ( K | M )?')' )? ( INLINE LENGTH integer )? |
//
//        sysudtlib_dot? DATASET ( '('integer ( K | M )?')' )? ( INLINE LENGTH integer )?
//          storage_format |
//
//        sysudtlib_dot? ( UDT_name | MBR | ARRAY_name | VARRAY_name )
//      )
//    ;

method
    : ( INSTANCE | CONSTRUCTOR )? METHOD sysudtlib_dot? method_name
        ( '(' ( data_type | user_defined_type_name )(','dots)? ')' )?
        FOR user_defined_type_name
    ;

//storage_format
//    : STORAGE FORMAT ( Avro | CSV ( CHARACTER SET ( UNICODE | LATIN ) )? )
//        ( WITH SCHEMA ( database'.' )? schema_name )?
//    ;

show_query_logging
    : SHOW QUERY LOGGING ON (
        account_specification |
        ( user_name | database_name ) (','dots)? |
        application_specification
      )
    ;

//account_specification
//    : ( ALL | user_name_1 ) ( ACCOUNT '=' ( 'account_name' | '(''account_name' (','dots)?')' ) )?
//    ;
//
//application_specification
//    : APPLNAME '=' ( 'application_name' | '(''application_name' (','dots)?')' )
//    ;

show_request
    : SHOW (IN XML)? (QUALIFIED)? dml_request
    ;

dml_request
    : todo
    ;

show_statistics
    : SHOW (IN XML)? (SUMMARY)? (CURRENT)? ( STATISTICS | STATS | STAT )
        ( VALUES (SEQUENCED)? )?
        ( ( UNIQUE )? INDEX index_name |

          ( UNIQUE )? INDEX ( index_name )? (ALL)? '(' column_name (','dots)? ')'
            ( ORDER BY ( VALUES | HASH ) '(' column_name ')' )? |

          COLUMN column_specification
        )
        ON table_specification
    ;

//column_specification
//    : ( ( expression | '(' expression (','dots)? ')' ) AS statistics_name |
//
//        ( statistics_name |
//          column_name |
//          PARTITION |
//          '(' ( column_name | PARTITION ) (','dots)? ')'
//        ) ( (AS)? statistics_name )?
//      )
//    ;
//
//table_specification
//    : ( (TEMPORARY)? ( database_name_dot | user_name_dot )? table_name_1 |
//
//        ( database_name_dot | database_name_dot )? ( join_index_name | hash_index_name )
//      )
//    ;

//QCD Form
show_statistics_qcd
    : SHOW (IN XML)? ( STATISTICS | STATS | STAT )
        ( VALUES ( SEQUENCED )? )?
        ( ( UNIQUE )? INDEX index_name |

          ( UNIQUE )? INDEX ( index_name )? (ALL)? '(' column_name (','dots)? ')'
            ( ORDER BY ( VALUES | HASH ) '(' column_name ')' )? |

           COLUMN column_specification
         )
         ON table_specification
         ( FOR QUERY query_id )?
         ( SAMPLEID statistics_id )?
         ( USING MODIFIED )?
    ;

//column_specification
//    : ( statistics_name |
//        column_name |
//        PARTITION |
//        '(' ( column_name | PARTITION ) (','dots)? ')'
//      ) ( (AS)? statistics_name )?
//    ;
//
//table_specification
//    : ( database_name_dot | user_name_dot )?
//        ( table_name | join_index_name | hash_index_name ) FROM qcd_name
//    ;

show_table
    : SHOW (IN XML)? (TEMPORARY)? TABLE ( database_name_dot | user_name_dot )? table_name
    ;

boolean_condition
    : todo
    ;

expression
    : todo
    ;


todo
    : SEMI
    ;

dots
    : SEMI
    ;

integer
    : todo
    ;

precision
    : todo
    ;

schema_name
    : todo
    ;

view_name
    : todo
    ;

procedure_name
    : todo
    ;

database_name
    : todo
    ;

table_name
    : todo
    ;

column_name
    : todo
    ;

constraint_name
    : todo
    ;

name
    : todo
    ;

value
    : todo
    ;

user_name
    : todo
    ;

join_index_name
    : todo
    ;

hash_index_name
    : todo
    ;

array_type_name
    : todo
    ;

trigger_name
    : todo
    ;

statistics_name
    : todo
    ;

function_name
    : todo
    ;

map_name
    : todo
    ;

colocation_name
    : todo
    ;

quotestring
    : todo
    ;

constant
    : todo
    ;

zone_name
    : todo
    ;

index_name
    : todo
    ;

method_name
    : todo
    ;

role_name
    : todo
    ;

profile_name
    : todo
    ;

specific_method_name
    : todo
    ;

correlation_name
    : todo
    ;

join_table_name
    : todo
    ;

query_name
    : todo
    ;

authorization_name
    : todo
    ;

specific_function_name
    : todo
    ;

function_mapping_name
    : todo
    ;

macro_name
    : todo
    ;

error_table_name
    : todo
    ;

data_table_name
    : todo
    ;

transform_group_name
    : todo
    ;

user_defined_type_name
    : todo
    ;

array_name
    : todo
    ;

varray_name
    : todo
    ;

object_name
    : todo
    ;

command_name
    : todo
    ;

attribute_name
    : todo
    ;

category_name
    : todo
    ;

level_name
    : todo
    ;

group_name
    : todo
    ;

parameter_name
    : todo
    ;

referenced_table_name
    : todo
    ;

qcd_name
    : todo
    ;

referenced_column_name
    : todo
    ;

row_level_security_constraint_column_name
    : todo
    ;

compress_UDF_name
    : todo
    ;

decompress_UDF_name
    : todo
    ;

external_function_name
    : todo
    ;

workload_name
    : todo
    ;

fractional_seconds_precision
    : todo
    ;

sign
    : todo
    ;

delimiter
    : todo
    ;

number
    : todo
    ;

partitioning_expression
    : todo
    ;

conditional_expression
    : todo
    ;

start_expression
    : todo
    ;

end_expression
    : todo
    ;

constant_expression
    : todo
    ;

object_kind
    : todo
    ;

source_predefined_data_type
    : todo
    ;

source_UDT_name
    : todo
    ;

target_UDT_name
    : todo
    ;

target_predefined_data_type
    : todo
    ;

data_name
    : todo
    ;

function_entry_name
    : todo
    ;

java_class_name
    : todo
    ;

return_expression
    : todo
    ;

search_condition
    : todo
    ;

library_name
    : todo
    ;

name_on_server
    : todo
    ;

package_name
    : todo
    ;

source_file
    : todo
    ;

include_name
    : todo
    ;

source_name
    : todo
    ;

include_file
    : todo
    ;

server_name
    : todo
    ;

json_document
    : todo
    ;

server_character_set
    : todo
    ;

number_of_sets
    : todo
    ;

label_name
    : todo
    ;

cursor_name
    : todo
    ;

variable_name
    : todo
    ;

statement_name
    : todo
    ;

literal
    : todo
    ;

condition_name
    : todo
    ;

period_name
    : todo
    ;

layer_name
    : todo
    ;

calendar_name
    : todo
    ;

pair_name
    : todo
    ;

pair_value
    : todo
    ;

collation_sequence
    : todo
    ;

mask_string
    : todo
    ;

query_id
    : todo
    ;

statistics_id
    : todo
    ;

transform_group
    : todo
    ;

flush_option
    : todo
    ;

operation
    : todo
    ;

alias
    : todo
    ;

bound
    : todo
    ;

date_expression
    : todo
    ;

timestamp_expression
    : todo
    ;

having_condition
    : todo
    ;

qualify_condition
    : todo
    ;

operand
    : todo
    ;

sqlstate_code
    : todo
    ;

handler_action_statement
    : todo
    ;

statement_string_variable
    : todo
    ;

condition_number
    : todo
    ;

condition_information_item
    : todo
    ;

parameter_reference
    : todo
    ;

assignment_target
    : todo
    ;

assignment_source
    : todo
    ;

numeric_expression
    : todo
    ;

value_expression
    : todo
    ;

expression_alias
    : todo
    ;

using_modifier
    : todo
    ;

type_declaration
    : todo
    ;

locking
    : todo
    ;

modifier
    : todo
    ;

column_position
    : todo
    ;

column_alias
    : todo
    ;

proc_id
    : todo
    ;

sequence
    : todo
    ;

location_column
    : todo
    ;

payload_column
    : todo
    ;

data_column_definition
    : todo
    ;

attribute_clause
    : todo
    ;

save_table
    : todo
    ;

range_size
    : todo
    ;

period_begin
    : todo
    ;

period_end
    : todo
    ;

for_loop_variable
    : todo
    ;

statement_information_item
    : todo
    ;

other_SELECT_clause
    : todo
    ;

calendar_function
    : todo
    ;

subquery
    : todo
    ;

single_table
    : todo
    ;

glop_set_name
    : todo
    ;

schema
    : todo
    ;

ordinary_grouping_set
    : todo
    ;

empty_grouping_set
    : todo
    ;

rollup_list
    : todo
    ;

cube_list
    : todo
    ;

grouping_set_specification
    : todo
    ;

column_attributes
    : todo
    ;

normalize_column
    : todo
    ;

referencing_column
    : todo
    ;

source_table
    : todo
    ;

from_method_or_function
    : todo
    ;

temporal_option
    : todo
    ;

number_of_elements
    : todo
    ;

maximum_size
    : todo
    ;

temp_password
    : todo
    ;

select_expression
    : todo
    ;

grouping_sets_specification
    : todo
    ;

string
    : todo
    ;
