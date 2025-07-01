parser grammar abcParser;


options { tokenVocab = abcLexer; }
abc
   : headers body EOF
   ;

headers
   : pair+
   ;

pair
   : key VAL_
   ;

key
   : STRING
   ;

body
   : bar (BAR bar)*
   ;

bar
   : NUMBER? TRIPLE+ COLON?
   ;


