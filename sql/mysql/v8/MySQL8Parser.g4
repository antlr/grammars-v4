parser grammar MySQL8Parser;

import MySQLParser;

options {
    tokenVocab = MySQLLexer;
    superClass = MySQLParserBase;
}

@members {
    public int mysqlMajor = 8;
}

queries8
    : queries
    ;
