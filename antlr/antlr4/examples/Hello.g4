// define a grammar called Hello

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Hello;

r
    : 'hello' ID
    ;

ID
    : [a-z]+
    ;

WS
    : [ \t\r\n]+ -> skip
    ;