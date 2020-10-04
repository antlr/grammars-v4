grammar stellaris;

content: 
   expr+
   ;

expr: 
   keyval+
   ;

keyval:
   key ('=' | '>' | '<')+ val
   ;

key: 
   id | attrib
   ;

val: 
   id | attrib | group
   ;

attrib: 
   id accessor (attrib| id)
   ;

accessor: 
   '.'|'@'|':'
   ;

group: 
   '{' (expr* | id) '}'
   ;

id: 
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


