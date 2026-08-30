/*
Apache Impala grammar.
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
parser grammar ImpalaParser;


options { tokenVocab = ImpalaLexer; }
impala_file
    : batch* EOF
    ;

batch
    : sql_command SEMI?
    ;

sql_command
    : ddl_command
    | dml_command
    | other_command
    ;

ddl_command
    : alter_command
    | create_command
    | drop_command
    ;

dml_command
    : select_statement
    | delete_statement
    | insert_statement
    | update_statement
    | upsert_statement
    | truncate_table_statement
    | values_statement
    ;

select_statement
    : (WITH name AS '('select_expression')' (',' name AS '('select_expression')')* )?
      SELECT
        (ALL | DISTINCT)?
        STRAIGHT_JOIN?
        expression (',' expression)*
      FROM table_reference (',' table_reference)*
      ( (FULL | (LEFT | RIGHT)? INNER | (LEFT | RIGHT)? OUTER | (LEFT | RIGHT)? SEMI | (LEFT | RIGHT)? ANTI | CROSS)?
        JOIN table_reference
      //  [ON join_equality_clauses | USING (col1[, col2 ...] ] ...
      WHERE conditions
      GROUP BY (column | expression) (',' (column | expression))*
      HAVING conditions
      ORDER BY order_by_item (',' order_by_item)*
      LIMIT expression (OFFSET expression)?
      (UNION ALL? select_statement)?
      )*
    ;

select_expression
    : todo
    ;

conditions
    : todo
    ;

expression
    : todo
    ;

order_by_item
    : column | expression (ASC | DESC)? (NULLS FIRST | NULLS LAST)?
    ;

table_reference
    :  (table_name | '(' subquery ')')
        ( TABLESAMPLE SYSTEM'('percentage')' (REPEATABLE'('seed')')? )?
    ;

subquery
    : todo
    ;

delete_statement
    : DELETE FROM? (db_name'.')?table_name ( WHERE where_conditions )?
    | DELETE table_ref FROM (joined_table_refs)? ( WHERE where_conditions )?
    ;

table_ref
    : todo
    ;

insert_statement
    : with_clause?
        INSERT hint_clause? (INTO | OVERWRITE) TABLE? table_name
        ( '('column_list')' )?
        ( PARTITION '('partition_clause')')?
      (
          hint_clause? select_statement
        | VALUES //'('value [, value ...]')' [',' '('value [, value ...]')' ...]
      )
    ;

with_clause
    : todo
    ;

partition_clause
    : column_name ('=' constant)? (',' column_name '=' constant)*
    ;

hint_clause
    : hint_with_dashes
    //| hint_with_cstyle_delimiters
    | hint_with_brackets
    ;

hint_with_dashes
    :  todo //-- +SHUFFLE | -- +NOSHUFFLE -- +CLUSTERED
    ;

hint_with_cstyle_comments
    :  todo /* +SHUFFLE */ | /* +NOSHUFFLE */ | /* +CLUSTERED */
    ;

hint_with_brackets
    : SHUFFLE
    | NOSHUFFLE
        //(With this hint format, the square brackets are part of the syntax.)
    ;

update_statement
    : UPDATE (db_name'.')table_name SET column '=' val (',' column '=' val)*
        ( FROM joined_table_refs )?
        ( WHERE where_conditions )?
    ;

joined_table_refs
    : todo
    ;

where_conditions
    : todo
    ;

upsert_statement
    : UPSERT hint_clause? INTO TABLE? (db_name'.')table_name
        ('(' column_list ')')?
      (
          hint_clause? select_statement
        | VALUES //(value [, value ...]) [, (value [, value ...]) ...]
      )
    ;

hint_clause2
    : SHUFFLE
    | NOSHUFFLE
      //  (Note: the square brackets are part of the syntax.)
    ;

truncate_table_statement
    : TRUNCATE TABLE? if_exists? (name '.')? name
    ;

values_statement
    : VALUES '('row')'//[, (row), ...];
    | SELECT select_list FROM //(VALUES (row)[, (row), ...]) AS alias;
    ;

select_list
    : todo
    ;

row
    : column //[[AS alias], column [AS alias], ...]
    ;

alter_command
    : alter_database
    | alter_table
    | alter_view
    ;

alter_database
    : ALTER DATABASE db=name SET OWNER USER u=name
    ;

alter_table
    : ALTER TABLE (od=name '.')? ot=name RENAME TO (nd=name '.')? nt=name
    | ALTER TABLE name ADD if_not_exists? COLUMNS '(' col_spec (',' col_spec)* ')'
    | ALTER TABLE name REPLACE COLUMNS '(' col_spec (',' col_spec)* ')'
    | ALTER TABLE name ADD COLUMN if_not_exists? col_spec
    | ALTER TABLE name DROP COLUMN? col=name
    | ALTER TABLE name CHANGE col=name col_spec
    | ALTER TABLE name SET OWNER USER u=name
      // Kudu tables only.
    | ALTER TABLE name ALTER COLUMN? col=name (SET kudu_storage_attr attr_value | DROP DEFAULT)
      // Non-Kudu tables only.
    | ALTER TABLE name ALTER COLUMN? col=name SET COMMENT string
    | ALTER TABLE name ADD if_not_exists? PARTITION '('partition_spec')'
        location_spec?
        cache_spec?
    | ALTER TABLE name ADD if_not_exists? RANGE PARTITION kudu_partition_spec
    | ALTER TABLE name DROP if_exists? PARTITION '('partition_spec')' PURGE?
    | ALTER TABLE name DROP if_exists? RANGE PARTITION kudu_partition_spec
    | ALTER TABLE name RECOVER PARTITIONS
    | ALTER TABLE name (PARTITION '(' partition_spec ')')?
        SET ( FILEFORMAT file_format
            | ROW FORMAT row_format
            | LOCATION hdfs_path
            | TBLPROPERTIES '(' kv_properties ')'
            | SERDEPROPERTIES '(' kv_properties ')'
            )
    | ALTER TABLE name col=name '(' kv_properties ')'
    | ALTER TABLE name (PARTITION '(' partition_spec ')')? SET ( CACHED IN string (WITH REPLICATION '=' integer)? | UNCACHED )
    ;

attr_value
    : todo
    ;

kudu_storage_attr
    : DEFAULT
    | BLOCK_SIZE
    | ENCODING
    | COMPRESSION
    ;

statsKey
    : todo
//    numDVs
//    | numNulls
//    | avgSize
//    | maxSize
    ;

new_name
    : (nd=name '.')? nt=name
    ;

col_spec
    : col=name type_name COMMENT string kudu_attributes?
    ;

type_name
    : primitive_type
    | complex_type
    ;

primitive_type
    : TINYINT
    | SMALLINT
    | INT
    | BIGINT
    | BOOLEAN
    | FLOAT
    | DOUBLE
    | DECIMAL
    | STRING
    | CHAR
    | VARCHAR
    | TIMESTAMP
    ;

complex_type
    : struct_type
    | array_type
    | map_type
    ;

struct_type
    : STRUCT '<' name ':' (primitive_type | complex_type) (COMMENT string)? (',' name ':' (primitive_type | complex_type) (COMMENT string)?)* '>'
    ;

array_type
    : ARRAY '<' (primitive_type | complex_type) '>'
    ;

map_type
    : MAP '<' primitive_type',' (primitive_type | complex_type) '>'
    ;

kudu_attributes
    : NOT? NULL_
    | ENCODING codec
    | COMPRESSION comp_algorithm
    | DEFAULT constant
    | BLOCK_SIZE number
    ;

codec
    : AUTO_ENCODING
    | PLAIN_ENCODING
    | RLE
    | DICT_ENCODING
    | BIT_SHIFFLE
    | PREFIX_ENCODING
    ;

constant
    : NULL_
    | number
    | string
    ;

comp_algorithm
    : LZ4
    | SNAPPY
    | ZLIB
    ;

partition_spec
    : simple_partition_spec
    | complex_partition_spec
    ;

simple_partition_spec
    : pc=name '=' v=constant
    ;

complex_partition_spec
    : comparison_expression_on_partition_col
    ;

comparison_expression_on_partition_col
    :
    ;

kudu_partition_spec
    : constant range_operator VALUES range_operator constant
    | VALUE '=' constant
    ;

range_operator
    :
    ;

cache_spec
    : CACHED IN string (WITH REPLICATION '=' integer)?
    | UNCACHED
    ;

location_spec
    : LOCATION hdfs_path
    ;

kv_properties
    : key_name '=' value_name (',' key_name '=' value_name)*
    ;

key_name
    : string
    ;

value_name
    : string
    ;

file_format
    : PARQUET
    | TEXTFILE
    | RCFILE
    | SEQUENCEFILE
    | AVRO
    ;

row_format
    : DELIMITED
        (FIELDS TERMINATED BY string (ESCAPED BY string)?)?
        (LINES TERMINATED BY string)?
    ;

alter_view
    : ALTER VIEW (name '.')? name (('(' name (COMMENT string)? (',' name (COMMENT string)?)* ')')?
        AS select_statement | RENAME TO (name '.')? name | SET OWNER USER name)
    ;

create_command
   : create_database
   | create_function
   | create_role
   | create_table
   | create_view
   ;

create_database
   : CREATE (DATABASE | SCHEMA) if_not_exists? db=name (COMMENT string)? (LOCATION hdfs_path)?
   ;

hdfs_path
    : todo
    ;

create_function
    ://To create a persistent scalar C++ UDF with CREATE FUNCTION:

     CREATE FUNCTION if_not_exists? (db_name'.')? function_name '(' (arg_type(',' arg_type)*)? ')'
       RETURNS return_type
       LOCATION string
       SYMBOL '=' string
 //    To create a persistent Java UDF with CREATE FUNCTION:
     CREATE FUNCTION if_not_exists? (db_name'.')? function_name
       LOCATION string
       SYMBOL '=' string
  //   To create a persistent UDA, which must be written in C++, issue a CREATE AGGREGATE FUNCTION statement:

     CREATE AGGREGATE? FUNCTION if_not_exists? (db_name'.')?function_name'(' (arg_type (',' arg_type)*)? ')'
       RETURNS return_type
       (INTERMEDIATE type_spec)?
       LOCATION string
       (INIT_FN'='string)?
       UPDATE_FN'='string
       MERGE_FN'='string
       (PREPARE_FN'='string)?
       (CLOSEFN'='string)?
       (SERIALIZE_FN'='string)?
       (FINALIZE_FN'='string)?
    ;

arg_type
    : todo
    ;

type_spec
    : todo
    ;

create_role
    : CREATE ROLE r=name
    ;

create_table
    : CREATE EXTERNAL? TABLE if_not_exists? (db_name'.')? table_name
        '(' column_name data_type
          constraint_specification?
          (COMMENT string)?
          //[, ...]
        ')'
        //[PARTITIONED BY (col_name data_type [COMMENT 'col_comment'], ...)]
        //[SORT BY ([column [, column ...]])]
        (COMMENT string)?
        (ROW FORMAT row_format)?
        //[WITH SERDEPROPERTIES ('key1'='value1', 'key2'='value2', ...)]
        (STORED AS file_format)?
        (LOCATION string)?
        (CACHED IN string (WITH REPLICATION '=' integer)? | UNCACHED)?
        //[TBLPROPERTIES ('key1'='value1', 'key2'='value2', ...)]
    | CREATE EXTERNAL? TABLE if_not_exists? (db_name '.')? table_name
//  [PARTITIONED BY (col_name[, ...])]
//  [SORT BY ([column [, column ...]])]
//  (COMMENT 'table_comment')?
//  (ROW FORMAT row_format)?
//  [WITH SERDEPROPERTIES ('key1'='value1', 'key2'='value2', ...)]
//  [STORED AS ctas_file_format]
//  [LOCATION 'hdfs_path']
//    [CACHED IN 'pool_name' [WITH REPLICATION = integer] | UNCACHED]
//  [TBLPROPERTIES ('key1'='value1', 'key2'='value2', ...)]
        AS
        select_statement
    ;

data_type
    : todo
    ;

constraint_specification
    : todo
    ;

create_view
    : CREATE VIEW if_not_exists? view_name
          ('(' column_name (COMMENT string)? (',' column_name (COMMENT string)?)* ')' )?
          (COMMENT string)?
          (TBLPROPERTIES '(' string '=' string (',' string '=' string)* ')' )?
        AS select_statement
    ;

drop_command
    : drop_database
    | drop_function
    | drop_role
    | drop_stats
    | drop_table
    | drop_view
    ;

drop_database
    : DROP (DATABASE | SCHEMA) if_exists? db=name (RESTRICT | CASCADE)?
    ;

drop_function
    : DROP AGGREGATE? FUNCTION if_exists? (db=name '.')? fn=name '(' type_list ')'
        todo
    ;

type_list
    : type (',' type)*
    ;

type
    : INT
    | todo
    ;

drop_role
    : DROP ROLE r=name
    ;

drop_stats
    : DROP ( STATS (db=name '.')? tbl=name
           | INCREMENTAL STATS (db=name '.')? tbl=name PARTITION '(' partition_spec ')'
           )
    ;

drop_table
    : DROP TABLE if_exists? (db=name '.')? tbl=name PURGE?
    ;

drop_view
    : DROP VIEW if_exists? (db=name '.')? vw=name
    ;

other_command
    : analyze_table_refresh_metadata
    | analyze_table_compute_statistics
    | comment_statement
    | compute_stats_statement
    | describe_statement
    | explain_statement
    | grant_statement
    | invalidate_metadata_statement
    | load_data_statement
    | refresh_statement
    | refresh_authorization_statement
    | refresh_functions_statement
    | revoke_statement
    | set_statement
    | show_statement
    | shutdown_statement
    | use_statement
    ;

analyze_table_refresh_metadata
    : ANALYZE TABLE (tn=name | TABLE'(' tfn=name '(' parameters ')' ')')?
        (COLUMNS ('(' column_list ')' | NONE))?
        REFRESH METADATA (todo LEVEL)?
        ((COMPUTE | ESTIMATE) | STATISTICS (SAMPLE number PERCENT)?)?
    | ANALYZE TABLE tn=name DROP (METADATA|STATISTICS)? if_exists?
    | ANALYZE TABLE (pn=name'.'sn=name'.')? tn=name COMPUTE STATISTICS ('(' column_list ')')? (SAMPLE number PERCENT)?
    | REFRESH TABLE METADATA (COLUMNS '(' column_list ')' | NONE )? tp=path
    ;

parameters
    :
    ;

path
    :
    ;

number
    : DECIMAL_LITERAL
    ;

integer
    : number
    ;

analyze_table_compute_statistics
    : ANALYZE TABLE (ws=name'.')? tn=name COMPUTE STATISTICS ('(' column_list ')')? (SAMPLE number PERCENT)?
    ;

comment_statement
    : COMMENT ON ( DATABASE db=name
                 | TABLE (db=name '.')? tbl=name
                 | COLUMN (db=name '.')? tbl=name '.' col=name
                 ) comment_clause
    ;

comment_clause
    : IS (string | NULL_)
    ;

compute_stats_statement
    : COMPUTE STATS (db_name'.')? table_name  ( '(' column_list ')' )?
        (TABLESAMPLE SYSTEM'('percentage')' (REPEATABLE'('seed')')?)?
    | COMPUTE INCREMENTAL STATS (db_name'.')? table_name (PARTITION '('partition_spec')')?
    ;

column_list
    : column_name (',' column_name)*
    ;

describe_statement
    : DESCRIBE DATABASE? (FORMATTED | EXTENDED)? object_name
    ;

object_name
    : (name '.')? name //[.complex_col_name ...]
    | name
    ;

explain_statement
    : EXPLAIN (select_query | ctas_statement | insert_statement)
    ;

select_query
    : todo
    ;

ctas_statement
    : todo
    ;

grant_statement
    : GRANT ROLE r=name TO GROUP g=name
    ;

invalidate_metadata_statement
    : INVALIDATE METADATA ((d=name'.')?t=name)?
    ;

load_data_statement
    : LOAD DATA INPATH string OVERWRITE? INTO TABLE table_name
        (PARTITION '(' column'=' val (',' column '=' val)* ')')?
    ;

val
    : todo
    ;

refresh_statement
    : REFRESH (d=name'.')?t=name (PARTITION '(' column '=' val (',' column '=' val)* ')')?
    ;

refresh_authorization_statement
    : REFRESH AUTHORIZATION
    ;

refresh_functions_statement
    : REFRESH FUNCTIONS d=name
    ;

revoke_statement
    : REVOKE ROLE r=name FROM GROUP g=name
    | REVOKE privilege ON
    ;

privilege
    : ALL
    | ALTER
    | CREATE
    | DROP
    | INSERT
    | REFRESH
    | SELECT ('(' col=name ')')?
    ;

object_type
    : SERVER
    | URI
    | DATABASE
    | TABLE
    ;

set_statement
    : SET (ALL | query_option '=' option_value)
    ;

query_option
    : ABORT_ON_ERROR
    | ALLOW_ERASURE_CODED_FILES
    | ALLOW_UNSUPPORTED_FORMATS
    | APPX_COUNT_DISTINCT
    | BATCH_SIZE
    | BROADCAST_BYTES_LIMIT
    | BUFFER_POOL_LIMIT
    | COMPRESSION_CODEC
    | COMPUTE_STATS_MIN_SAMPLE_SIZE
    | DEBUG_ACTION
    | DECIMAL_V2
    | DEFAULT_FILE_FORMAT
    | DEFAULT_HINTS_INSERT_STATEMENT
    | DEFAULT_JOIN_DISTRIBUTION_MODE
    | DEFAULT_SPILLABLE_BUFFER_SIZE
    | DEFAULT_TRANSACTIONAL_TYPE
    | DELETE_STATS_IN_TRUNCATE
    | DISABLE_CODEGEN
    | DISABLE_CODEGEN_ROWS_THRESHOLD
    | DISABLE_HBASE_NUM_ROWS_ESTIMATE
    | DISABLE_ROW_RUNTIME_FILTERING
    | DISABLE_STREAMING_PREAGGREGATIONS
    | DISABLE_UNSAFE_SPILLS
    | ENABLE_EXPR_REWRITES
    | EXEC_SINGLE_NODE_ROWS_THRESHOLD
    | EXEC_TIME_LIMIT_S
    | EXPLAIN_LEVEL
    | FETCH_ROWS_TIMEOUT_MS
    | JOIN_ROWS_PRODUCED_LIMIT
    | HBASE_CACHE_BLOCKS
    | HBASE_CACHING
    | IDLE_SESSION_TIMEOUT
    | KUDU_READ_MODE
    | LIVE_PROGRESS
    | LIVE_SUMMARY
    | MAX_ERRORS
    | MAX_MEM_ESTIMATE_FOR_ADMISSION
    | MAX_RESULT_SPOOLING_MEM
    | MAX_ROW_SIZE
    | MAX_SCAN_RANGE_LENGTH
    | MAX_SPILLED_RESULT_SPOOLING_MEM
    | MEM_LIMIT
    | MIN_SPILLABLE_BUFFER_SIZE
    | MT_DOP
    | NUM_NODES
    | NUM_ROWS_PRODUCED_LIMIT
    | NUM_SCANNER_THREADS
    | OPTIMIZE_PARTITION_KEY_SCANS
    | PARQUET_COMPRESSION_CODEC
    | PARQUET_ANNOTATE_STRINGS_UTF8
    | PARQUET_ARRAY_RESOLUTION
    | PARQUET_DICTIONARY_FILTERING
    | PARQUET_FALLBACK_SCHEMA_RESOLUTION
    | PARQUET_FILE_SIZE
    | PARQUET_OBJECT_STORE_SPLIT_SIZE
    | PARQUET_PAGE_ROW_COUNT_LIMIT
    | PARQUET_READ_STATISTICS
    | PARQUET_READ_PAGE_INDEX
    | PARQUET_WRITE_PAGE_INDEX
    | PREFETCH_MODE
    | QUERY_TIMEOUT_S
    | REFRESH_UPDATED_HMS_PARTITIONS
    | REPLICA_PREFERENCE
    | REQUEST_POOL
    | RESOURCE_TRACE_RATIO
    | RETRY_FAILED_QUERIES
    | RUNTIME_BLOOM_FILTER_SIZE
    | RUNTIME_FILTER_MAX_SIZE
    | RUNTIME_FILTER_MIN_SIZE
    | RUNTIME_FILTER_MODE
    | RUNTIME_FILTER_WAIT_TIME_MS
    | S3_SKIP_INSERT_STAGING
    | SCAN_BYTES_LIMIT
    | SCHEDULE_RANDOM_REPLICA
    | SCRATCH_LIMIT
    | SHUFFLE_DISTINCT_EXPRS
    | SPOOL_QUERY_RESULTS
    | SUPPORT_START_OVER
    | SYNC_DDL
    | THREAD_RESERVATION_AGGREGATE_LIMIT
    | THREAD_RESERVATION_LIMIT
    | TIMEZONE
    | TOPN_BYTES_LIMIT
    | UTF8_MODE
    | EXPAND_COMPLEX_TYPES
    ;

option_value
    : TRUE
    | FALSE
    | ONE
    | ZERO
    ;

show_statement
    : SHOW DATABASES like_pattern?
    | SHOW SCHEMAS like_pattern? // an alias for SHOW DATABASES
    | SHOW TABLES (IN d=name)? like_pattern?
    | SHOW (AGGREGATE | ANALYTIC) FUNCTIONS (IN d=name)? like_pattern?
    | SHOW CREATE TABLE (d=name'.')?t=name
    | SHOW CREATE VIEW (d=name'.')?v=name
    | SHOW TABLE STATS (d=name'.')?t=name
    | SHOW COLUMN STATS (d=name'.')?t=name
    | SHOW PARTITIONS (d=name'.')?t=name
    | SHOW RANGE? PARTITIONS (d=name'.')?t=name
    | SHOW FILES IN (d=name'.')?t=name (PARTITION '('key_col_expression (',' key_col_expression)* ')')?
    | SHOW ROLES
    | SHOW CURRENT ROLES
    | SHOW ROLE GRANT GROUP g=name
    | SHOW GRANT USER u=name
    | SHOW GRANT USER u=name ON SERVER
    | SHOW GRANT USER u=name ON DATABASE d=name
    | SHOW GRANT USER u=name ON TABLE (d=name'.')?t=name
    | SHOW GRANT USER u=name ON URI uri
    | SHOW GRANT USER u=name ON COLUMN ((d=name'.')?t=name'.')?c=name
    | SHOW GRANT ROLE u=name
    | SHOW GRANT ROLE u=name ON SERVER
    | SHOW GRANT ROLE u=name ON DATABASE d=name
    | SHOW GRANT ROLE u=name ON TABLE (d=name'.')?t=name
    | SHOW GRANT ROLE u=name ON URI uri
    | SHOW GRANT ROLE u=name ON COLUMN ((d=name'.')?t=name'.')?c=name
    | SHOW GRANT GROUP g=name ON SERVER
    | SHOW GRANT GROUP g=name ON DATABASE d=name
    | SHOW GRANT GROUP g=name ON TABLE (d=name'.')?t=name
    | SHOW GRANT GROUP g=name ON URI uri
    | SHOW GRANT GROUP g=name ON COLUMN ((d=name'.')?t=name'.')?c=name
    ;

like_pattern
    : LIKE? p=string
    ;

key_col_expression
    : todo
    ;

uri
    : string
    ;

shutdown_statement
    : ':'SHUTDOWN'('')'
    | ':'SHUTDOWN'(' (host_name(':'port_number)?)? ')'
    | ':'SHUTDOWN'('deadline')'
    | ':'SHUTDOWN'(' (host_name(':'port_number)?)? ',' deadline')'
    ;

deadline
    : todo
    ;

port_number
    : todo
    ;

percentage
    : todo
    ;

seed
    : todo
    ;

use_statement
    : USE db=name
    ;

if_not_exists
    : IF NOT EXISTS
    ;

if_exists
    : IF EXISTS
    ;

column
    : ID
    ;

column_name
    : ID
    ;

table_name
    : ID
    ;

name
    : ID
    ;

host_name
    : ID
    ;

db_name
    : ID
    ;

function_name
    : ID
    ;

view_name
    : ID
    ;

string
    : STRING_LITERAL
    ;

return_type
    : todo
    ;

todo
    : ';'
    ;
