lexer grammar bnfLexer;

X1 : '<' ;
X2 : '>' ;
X3 : '"' ;
X4 : '\'' ;
ASSIGN1 : '::=' ;
ASSIGN2 : ':' '\u{2261}' ;
ASSIGN3 : '-->' ;
ASSIGN4 : '\u{2192}' ;
OR1 : '|' ;
OR2 : 'o' '\u{203E}' 'r' '\u{203E}' ;
OR3 : '\u{2223}' ;
ID : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_')+ ;
WS : [ \t]+ ;
NL : [\r\n]+ ;
TEXT : . ;
