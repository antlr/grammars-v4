grammar issue1165;

r : 'hi' 'there' ;
WS : [ \t\n\r]+ -> skip ;
EMPTY_STR           : -> type(EMPTY_STR) ;
