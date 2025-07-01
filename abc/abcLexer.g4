lexer grammar abcLexer;

fragment LETDIG
   : [a-zA-Z0-9]
   ;

TRIPLE
   : LETDIG LETDIG LETDIG
   ;

STRING
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

COLON: ':';

BAR
   : '|'
   ;

NUMBER
   : [0-9]+
   ;

COMMENT
   : '%' ~ [\r\n]* -> skip
   ;

VAL_
   : COLON ~ [\r\n]*
   ;

WS
   : [ \t\r\n]+ -> skip
   ;


