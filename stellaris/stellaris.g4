grammar stellaris;

content: 
   expr+ EOF
   ;

expr: 
   keyval+
   ;

keyval:
   key ('=' | '>' | '<')+ val
   ;

key: 
   id_ | attrib
   ;

val: 
   id_ | attrib | group
   ;

attrib: 
   id_ accessor (attrib| id_)
   ;

accessor: 
   '.'|'@'|':'
   ;

group: 
   '{' (expr* | id_) '}'
   ;

id_: 
   IDENTIFIER | STRING | INTEGER
   ;

IDENTIFIER: 
   IDENITIFIERHEAD IDENITIFIERBODY*
   ;

INTEGER: 
   [+-]? INTEGERFRAG
   ;

fragment INTEGERFRAG: 
   [0-9]+
   ;

fragment IDENITIFIERHEAD: 
   [a-zA-Z]
   ;

fragment IDENITIFIERBODY
   : IDENITIFIERHEAD | [0-9_]
   ;

STRING: 
   '"' ~["\r\n]* '"'
   ;

COMMENT: 
   '#' ~[\r\n]* -> channel(HIDDEN)
   ;

SPACE: 
   [ \t\f] -> channel(HIDDEN)
   ;

NL: 
   [\r\n] -> channel(HIDDEN)
   ;


