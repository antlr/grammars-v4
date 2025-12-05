parser grammar PostgreSQL18Parser;

import PostgreSQLParser;

options {
    tokenVocab = PostgreSQLLexer;
    superClass = PostgreSQLParserBase;
}

@members {
    public int pgVersion = 18;
}

root18
    : root
    ;
