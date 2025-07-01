lexer grammar abcLexer;

fragment LETDIG
   : [a-zA-Z0-9]
   ;

TRIPLE
   : LETDIG LETDIG LETDIG -> mode(BODY)
   ;

STRING
   : [a-zA-Z] [a-zA-Z0-9]*
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

mode BODY;

BAR
   : '|'
   ;

   COLON: ':';


NOTE: [a-gA-G0-9];

TRIPLET: NOTE NOTE NOTE;

WS2
   : [ \t\r\n]+ -> skip
   ;
