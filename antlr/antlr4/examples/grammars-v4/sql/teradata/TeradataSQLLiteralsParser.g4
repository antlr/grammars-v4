parser grammar TeradataSQLLiteralsParser;

options {
    tokenVocab=TeradataSQLLexer;
}

literal
    : hex_byte_literal
    | char_string_literal
    | unicode_char_string_literal
    | hex_char_string_literal
    | integer_literal
    | hex_integer_literal
    | float_literal
    | date_literal
    | time_literal
    | timestamp_literal
    | interval_literal
    | period_literal
    | NULL
    ;

hex_byte_literal : HEX_BYTE_LITERAL ;

// Multiple consecutive character literals are treated as if the strings are concatenated.
char_string_literal : character_set_prefix?  CHAR_STRING+ ;

unicode_char_string_literal
    : character_set_prefix?
      UNICODE_STRING_LEADING CHAR_STRING*
      UESCAPE CHAR_STRING
    ;

hex_char_string_literal : character_set_prefix? HEX_STRING ;

integer_literal : UNSIGNED_INTEGER ;

hex_integer_literal : HEX_INTEGER_LITERAL ;

float_literal : FLOAT_LITERAL ;

character_set_prefix : CS_LATIN | CS_UNICODE | CS_KANJISJIS | CS_GRAPHIC ;
// ANSI SQL DateTime literals differ from other SQL literals in that they are always prefaced by a keyword
//or keywords.

//Teradata SQL literals used with date or time values are simple string literals and are interpreted as character
//data. These are converted to a date or time value based on the context, which is usually provided by a
//FORMAT clause.
date_literal : DATE? DATE_STRING ;

time_literal : TIME? TIME_STRING ;

timestamp_literal : TIMESTAMP? TIMESTAMP_STRING ;

interval_literal : INTERVAL '-'? (CHAR_STRING|TIME_STRING|DATE_STRING|TIMESTAMP_STRING) interval_qualifier ;

interval_qualifier
    : YEAR
    | YEAR TO MONTH
    | MONTH
    | DAY
    | DAY TO HOUR
    | DAY TO MINUTE
    | DAY TO SECOND
    | HOUR
    | HOUR TO MINUTE
    | HOUR TO SECOND
    | MINUTE
    | MINUTE TO SECOND
    | SECOND
    ;

period_literal : PERIOD PERIOD_STRING ;
