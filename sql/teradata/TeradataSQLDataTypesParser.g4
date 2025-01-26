parser grammar TeradataSQLDataTypesParser;

import TeradataSQLLiteralsParser
     , TeradataSQLIdentifiersParser
     , TeradataSQLExpressionsParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

data_type
    : numeric_data_type
    | char_data_type
    | binary_data_type
    | datetime_type
    | period_type
    | udt_type
    ;

variable_data_type
    : numeric_data_type
    | char_data_type
    | precisionless_char_data_type
    | binary_data_type
    | datetime_type
    | period_type
    | udt_type
    ;

external_function_data_type
    : numeric_data_type
    | char_data_type
    | precisionless_char_data_type
    | lob_as_locator_data_type
    | binary_data_type
    | datetime_type
    | period_type
    | udt_type
    | TD_ANYTYPE
    | VARIANT_TYPE
    ;

numeric_data_type
    : BYTEINT
    | SMALLINT
    | INTEGER
    | INT
    | BIGINT
    | (DECIMAL|DEC|NUMERIC) ('('precision=integer_literal? (',' scale=integer_literal)? ')' )?
    | ( FLOAT ('(' precision=integer_literal ')')?
      | REAL
      | DOUBLE PRECISION
      ) // YES, it is possible to specify precision for FLOAT
    | NUMBER ('(' (precision=integer_literal|'*') (',' scale=integer_literal)? ')' )?
    ;

char_data_type
    : (CHARACTER|CHAR) type_precision? character_set_phrase?
    | GRAPHIC type_precision?
    | (VARCHAR|CHARACTER VARYING|CHAR VARYING) type_precision character_set_phrase?
    | LONG VARCHAR
    | VARGRAPHIC type_precision
    | LONG VARGRAPHIC
    | (CLOB|CHARACTER LARGE OBJECT) max_length_k_m_g? latin_unicode_character_set_phrase?
    ;

precisionless_char_data_type // exclusively for variable definitions in procedures
    : VARCHAR character_set_phrase?
    | VARGRAPHIC character_set_phrase?
    ;

lob_as_locator_data_type
    : (CLOB|CHARACTER LARGE OBJECT) AS LOCATOR
    | (BLOB|BINARY LARGE OBJECT) AS LOCATOR
    ;

binary_data_type
    : BYTE type_precision?
    | VARBYTE type_precision
    | (BLOB|BINARY LARGE OBJECT) max_length_k_m_g?
    ;

datetime_type
    : DATE
    | TIME type_precision? with_time_zone?
    | TIMESTAMP type_precision? with_time_zone?
    | INTERVAL interval_period_spec
    ;

period_type
    : PERIOD '(' DATE ')'
    | PERIOD '(' TIME type_precision? with_time_zone? ')'
    | PERIOD '(' TIMESTAMP type_precision? with_time_zone? ')'
    ;

udt_type
    : sysudtlib? (XML|XMLTYPE) max_length_k_m_g? inline_length?
    | sysudtlib? JSON max_length_k_m? inline_length? (latin_unicode_character_set_phrase|json_storage_format)?
    | sysudtlib? ST_GEOMETRY max_length_k_m? inline_length?
    | sysudtlib? DATASET max_length_k_m? inline_length? dataset_storage_format_clause
    | sysudtlib? unqualified_name
    ;

data_type_attribute
    : uppercase_phrase
    | casespecific_phrase
    | format_phrase
    | column_naming_phrase
    | with_time_zone
    | at_timezone
    | character_set_phrase
    | default_value_control_phrase
    ;

default_value_control_phrase
    : NOT? NULL
    | DEFAULT default_value
    | WITH DEFAULT
    ;

default_value : ('-'|'+') float_literal | ('-'|'+') integer_literal | literal | builtin_function ;

column_naming_phrase
    : named_phrase
    | title_phrase
    ;

sysudtlib : 'SYSUDTLIB' '.' ;

interval_period_spec
    : YEAR type_precision? (TO MONTH)?
    | MONTH type_precision?
    | DAY type_precision? (TO HOUR|TO MINUTE|TO SECOND type_precision? )?
    | HOUR type_precision? (TO MINUTE|TO SECOND type_precision? )?
    | MINUTE type_precision? (TO SECOND type_precision? )?
    | SECOND ('(' precision=integer_literal (',' fractional_precision=integer_literal)? ')' )?
    ;

type_precision : '(' precision=integer_literal ')' ;

max_length_k_m_g : '(' max_length=integer_literal (K|M|G)? ')' ;

max_length_k_m : '(' max_length=integer_literal (K|M)? ')' ;

character_set_phrase : (CHARACTER|CHAR) SET (LATIN|UNICODE|GRAPHIC|KANJISJIS) ;

uppercase_phrase : UPPERCASE|UC;

casespecific_phrase : not=(NOT|'^')? (CASESPECIFIC|CS) ;

format_phrase : FORMAT (format=char_string_literal|date_format=DATE_STRING|time_format=TIME_STRING) ;

title_phrase : TITLE title=char_string_literal ;

named_phrase : NAMED alias_name ;

latin_unicode_character_set_phrase : (CHARACTER|CHAR) SET (LATIN|UNICODE) ;

inline_length : INLINE LENGTH length=integer_literal ;

json_storage_format : STORAGE FORMAT (BSON|UBJSON) ;

dataset_storage_format_clause : STORAGE FORMAT dataset_storage_format with_schema? ;

dataset_storage_format : AVRO | CSV latin_unicode_character_set_phrase? ;

with_schema : WITH SCHEMA schema_name=table_name ;

with_time_zone : WITH TIME ZONE ;
