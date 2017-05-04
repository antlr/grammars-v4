
grammar properties;

propertiesFile
    : 
    row+ ;

row 
    : 
    (comment | decl)? EOL;

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
    [a-zA-Z0-9 @:._/,-]+;

STRING 
    : 
    '"' ('""'|~'"')* '"' ; // quote-quote is an escaped quote
EOL 
    : 
    '\r'? '\n' ;

COMMENT
    : '#' ~ [\r\n]*
    ;
