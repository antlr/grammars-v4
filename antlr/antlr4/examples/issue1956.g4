// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar issue1165;

r
    : 'hi' 'there'
    ;

WS
    : [ \t\n\r]+ -> skip
    ;

EMPTY_STR
    : -> type(EMPTY_STR)
    ;