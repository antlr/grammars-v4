
grammar properties;

propertiesFile
    :
    row+ ;

row
    :
    (comment | decl)
    ;

decl
    : key '=' value?
    ;

key
    :
    TEXT;

value
    :
    TEXT | STRING;

comment
    :
    COMMENT;

TEXT
    :
    [a-zA-Z0-9 @:._/,%{}-]+;

STRING
    :
    '"' ('""'|~'"')* '"' ; // quote-quote is an escaped quote

COMMENT
    : '#' ~ [\r\n]*
    ;

TERMINATOR
    : [\r\n]+ -> channel(HIDDEN)
    ;