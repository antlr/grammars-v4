lexer grammar BencodingLexer;

options { superClass=BencodingLexerBase; }

INT_START
 : 'i'
 ;

INTEGER
 : '-'? [0-9]+
 ;

STRING_START
 : [0-9]+ ':' {setStringLength();} -> skip, pushMode(StringMode)
 ;

LIST_START
 : 'l'
 ;

DICT_START
 : 'd'
 ;

END
 : 'e'
 ;

OTHER
 : .
 ;

mode StringMode;

 STRING : ( {consumeStringChars()}? . )+ -> popMode;