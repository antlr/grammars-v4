lexer grammar LarkLexer;

channels { OFF_CHANNEL }

COLON: ':' ;
LC : '{' ;
RC : '}' ;
LP : '(' ;
RP : ')' ;
LB : '[' ;
RB : ']' ;
COMMA : ',' ;
DOT : '.' ;
ARROW : '->' ;
IGNORE : '%ignore' ;
IMPORT : '%import' ;
OVERRIDE : '%override' ;
DECLARE : '%declare' ;
DD : '..' ;
SQ : '~' ;

VBAR: '|' ;
OP: [+*] | '?' ;
RULE: '!'? [_?]? [a-z] [_a-z0-9]* ;
TOKEN: '_'? [A-Z] [_A-Z0-9]* ;
STRING: FSTRING 'i'? ;
REGEXP: '/' ('\\' '/' | '\\' '\\' | ~'/' ) ('\\' '/' | '\\' '\\' | ~'/' )*? '/' [imslux]* ;
NL: ('\r'? '\n')+ (' '| '\t' | '\n' | '\r' | '\f' | 'u2B7F' )*  -> channel(OFF_CHANNEL) ;

//
// Strings
//
fragment FSTRING : '"' (~["\\\r\n] | EscapeSequence)* '"';
fragment EscapeSequence : '\\' [btnfr"'\\] ;

//
// Numbers
//
fragment DIGIT: '0' .. '9' ;
fragment HEXDIGIT: 'a' .. 'f' | 'A' .. 'F' | DIGIT ;
fragment INT: DIGIT+ ;
NUMBER: ('+' | '-')? INT ;

//
// Whitespace
//
WS_INLINE: (' ' | '\t')+ -> channel(OFF_CHANNEL) ;

COMMENT: '//' ~[\n\r]* -> channel(OFF_CHANNEL) ;

