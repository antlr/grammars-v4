// https://www.crockford.com/mckeeman.html

grammar McKeemanForm;

grammar_
    : rule_ (Newline rule_)* EOF
    ;

Space
    : ' '
    ;

Newline
    : '\r'
    | '\n'
    | '\r\n'
    ;

Name
    : [a-zA-Z_]+
    ;

Indentation
    : '    '
    ;

rule_
    : Name Newline nothing? alternative+
    ;

nothing
    : Indentation '"' '"' Newline
    ;

alternative
    : Indentation item (Space item)* Newline
    ;

item
    : Name
    | Singleton
    | range exclude?
    | String
    ;

Singleton
    : '\'' . '\''
    | '\'' HexCode '\''
    ;

fragment HexCode
    : '1' '0' Hex Hex Hex Hex
    | Hex? Hex Hex Hex Hex
    ;

fragment Hex
    : [0-9a-fA-F]
    ;

range
    : Singleton Space '.' Space Singleton
    ;

exclude
    : Space '-' Space (Singleton | range) exclude?
    ;

String
    : '"' .*? '"'
    ;
