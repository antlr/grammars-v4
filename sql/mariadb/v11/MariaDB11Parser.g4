parser grammar MariaDB11Parser;

import MariaDBParser;

options {
    tokenVocab = MariaDBLexer;
}

@members {
    public int mariaMajor = 11;
}

root11
    : root
    ;
