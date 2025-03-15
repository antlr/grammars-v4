// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar BencodingParser;

options {
    tokenVocab = BencodingLexer;
}

// https://wiki.theory.org/BitTorrentSpecification#Bencoding
data
    : values EOF
    ;

values
    : value*
    ;

value
    : integer
    | STRING
    | list
    | dict
    ;

integer
    : INT_START INTEGER END
    ;

list
    : LIST_START values END
    ;

dict
    : DICT_START key_value* END
    ;

key_value
    : STRING value
    ;