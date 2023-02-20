parser grammar PropertiesParser;

options { tokenVocab=PropertiesLexer; }

propertiesFile
    : row*
    ;
row
    : comment
    | line
    ;
line
    : key (DELIMITER value=key?)? eol
    ;
key
    : CHARACTER+
    ;
eol
    : NEWLINE+
    | EOF
    ;
comment
    : COMMENT eol
    ;