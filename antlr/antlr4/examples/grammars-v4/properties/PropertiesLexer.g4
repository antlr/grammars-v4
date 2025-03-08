// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar PropertiesLexer;

COMMENT   : [!#] ~[\r\n]*;
NEWLINE   : [\r\n]+;
DELIMITER : [:=] -> pushMode(VALUE_MODE);
SLASH     : '\\' -> more, pushMode(INSIDE);
CHARACTER : ~ [!#:=\r\n];

mode INSIDE;

SLASH_DELIMITER : ~[\r\n]    -> type(CHARACTER), popMode;
SLASH_JOINT     : '\r'? '\n' -> type(CHARACTER), popMode;

mode VALUE_MODE;
VALUE_TERM      : '\r'? '\n' -> type(NEWLINE), popMode;
VALUE_SLASH     : '\\'       -> more, pushMode(INSIDE);
VALUE_CHARACTER : ~ [\r\n]   -> type(CHARACTER);