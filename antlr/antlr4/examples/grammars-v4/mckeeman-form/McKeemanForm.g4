// https://www.crockford.com/mckeeman.html

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

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
    | range_ exclude?
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

range_
    : Singleton Space '.' Space Singleton
    ;

exclude
    : Space '-' Space (Singleton | range_) exclude?
    ;

String
    : '"' .*? '"'
    ;