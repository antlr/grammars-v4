parser grammar TeradataSQLIdentifiersParser;

import TeradataSQLNonReservedWordsParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

column_name
    : database_name '.' unqualified_table_name=unqualified_name '.' unqualified_column_name
    | unqualified_table_name=unqualified_name '.' unqualified_column_name
    | unqualified_column_name
    ;

unqualified_column_name : OBJECT_NAME | nonreserved_word | SAMPLEID | ROWID ;

unqualified_name : OBJECT_NAME | nonreserved_word ;

object_name : (database_name '.')? (OBJECT_NAME|nonreserved_word) ;

table_name : (database_name '.')? (OBJECT_NAME|nonreserved_word) ;

procedure_name : (database_name '.')? (OBJECT_NAME|nonreserved_word) ;

function_name : (database_name '.')? (OBJECT_NAME|nonreserved_word) ;

macro_name : (database_name '.')? (OBJECT_NAME|nonreserved_word) ;

database_name : OBJECT_NAME | nonreserved_word ;

user_name : OBJECT_NAME | nonreserved_word ;

role_name : OBJECT_NAME | ADMIN | nonreserved_word ;

profile_name : OBJECT_NAME | nonreserved_word ;

alias_name : OBJECT_NAME | nonreserved_word ;

variable_name : OBJECT_NAME | nonreserved_word ;

parameter_name : OBJECT_NAME | nonreserved_word ;

label_name : OBJECT_NAME | nonreserved_word ;

condition_name : OBJECT_NAME | nonreserved_word ;

cursor_name : OBJECT_NAME | nonreserved_word ;

statement_name : OBJECT_NAME | nonreserved_word ;

statistics_name : OBJECT_NAME | nonreserved_word ;

udt_name : OBJECT_NAME | nonreserved_word ;

attribute_name : OBJECT_NAME | nonreserved_word ;

method_name : OBJECT_NAME | nonreserved_word ;

anchor_name
    : ANCHOR_MILLISECOND
    | ANCHOR_SECOND
    | ANCHOR_MINUTE
    | ANCHOR_HOUR
    | DAY
    | WEEK_BEGIN
    | WEEK_END
    | MONTH_BEGIN
    | MONTH_END
    | QUARTER_BEGIN
    | QUARTER_END
    | YEAR_BEGIN
    | YEAR_END
    | MONDAY
    | TUESDAY
    | WEDNESDAY
    | THURSDAY
    | FRIDAY
    | SATURDAY
    | SUNDAY
    ;
