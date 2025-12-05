parser grammar TeradataSQLParser;

import TeradataSQLDDLParser
     , TeradataSQLDMLParser
     , TeradataSQLDCLParser
     ;

options {
    tokenVocab=TeradataSQLLexer;
}

@members {
    public enum Standard { V1, V2, V3 }
    private Standard standard = Standard.V2;
    public void setStandard(Standard s) { standard = s; }
    private boolean atLeast(Standard s) { return standard.ordinal() >= s.ordinal(); }
    public boolean isV1OrLater() { return atLeast(Standard.V1); }
    public boolean isV2OrLater() { return atLeast(Standard.V2); }
    public boolean isV3OrLater() { return atLeast(Standard.V3); }
}

sql_script : unit_stat (';'  unit_stat)* ';'? EOF ;

unit_stat
    : ddl_stat
    | dcl_stat
    | dml_stat
    ;
