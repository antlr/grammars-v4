grammar spice;

spicefile
    : title elementblock END
    ;

title
    :  ~(CR)* CR
    ;

elementblock
    : identifier nodeid+
    ;    

identifier
    : IDENTIFIER
    ;

IDENTIFIER
    : ('a'..'z' | 'A'..'Z')  ('a'..'z'|'A'..'Z' | '0'..'9'| '_')*
    ;

nodeid
    : INT
    ;

INT
    : ('0'..'9')+
    ;

END
    : '.END'
    ;

LETTER
    :('a'..'z' |'A'..'Z' )
    ;

COMMENT_1
    : '*' .*? CR -> skip
    ;

COMMENT_2
    : ';' .*? CR -> skip
    ;

CR
    : ('\r' | '\n')+
    ;

WS
    : [ \t]->skip
    ;