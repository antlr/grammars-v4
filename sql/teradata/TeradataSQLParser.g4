parser grammar TeradataSQLParser;

import TeradataSQLDDLParser
     , TeradataSQLDMLParser
     , TeradataSQLDCLParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

sql_script : unit_stat (';'  unit_stat)* ';'? EOF ;

unit_stat
    : ddl_stat
    | dcl_stat
    | dml_stat
    ;
