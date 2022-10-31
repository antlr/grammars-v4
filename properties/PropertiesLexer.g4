lexer grammar PropertiesLexer;

COMMENT
    : [!#] ~[\r\n]*
    ;
NEWLINE
    : [\r\n]+
    ;
DELIMITER
    : [:=]  -> pushMode(VALUE_MODE)
    ;
SLASH
    : '\\'  -> more, pushMode(INSIDE)
    ;
CHARACTER
    : ~ [!#:=\r\n]
    ;

mode INSIDE;

SLASH_DELIMITER: ~[\r\n] -> type(CHARACTER), popMode;
SLASH_JOINT: '\r'? '\n' -> type(CHARACTER), popMode;

mode VALUE_MODE;
VALUE_TERM: '\r'? '\n' -> type(NEWLINE), popMode;
VALUE_SLASH: '\\'  -> more, pushMode(INSIDE);
VALUE_CHARACTER: ~ [\r\n] -> type(CHARACTER);
