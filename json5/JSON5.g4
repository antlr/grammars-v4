
/** Taken from "The Definitive ANTLR 4 Reference" by Terence Parr */

// Derived from http://json.org
grammar JSON5;

json5
   : value
   ;

obj
   : '{' pair (',' pair)* ','? '}'
   | '{' '}'
   ;

pair
   : key ':' value
   ;

key
   : STRING
   | IDENTIFIER
   ;

arr
   : '[' value (',' value)* ','? ']'
   | '[' ']'
   ;

value
   : STRING
   | number
   | obj
   | arr
   | 'true'
   | 'false'
   | 'null'
   ;

STRING
   : '"' DOUBLE_QUOTE_CHAR* '"'
   | '\'' SINGLE_QUOTE_CHAR* '\''
   ;

IDENTIFIER
   : IDENTIFIER_START IDENTIFIER_PART*
   ;

fragment IDENTIFIER_START
   : [\p{L}]
   | '$'
   | '_'
   | UNICODE_ESC
   ;

fragment IDENTIFIER_PART
   : IDENTIFIER_START
   | [\p{M}]
   | [\p{N}]
   | [\p{Pc}]
   | '\u200C'
   | '\u200D'
   ;

fragment DOUBLE_QUOTE_CHAR
   : '\\' ["\\/bfnrt\n]
   | ~ ["\\\u0000-\u001F]
   | UNICODE_ESC
   ;

fragment SINGLE_QUOTE_CHAR
   : '\\' ['\\/bfnrt\n]
   | ~ ['\\\u0000-\u001F]
   | UNICODE_ESC
   ;

fragment UNICODE_ESC
   : '\\u' HEX HEX HEX HEX
   ;

fragment HEX
   : [0-9a-fA-F]
   ;


fragment SAFECODEPOINT
   : ~ ["\\\u0000-\u001F]
   ;

number
   : DECNUM
   | HEXNUM
   | INFINITY
   | NAN
   ;

DECNUM
   : [+-]? INT ('.' [0-9]*)? EXP?
   | [+-]? '.' [0-9]+ EXP?
   ;

HEXNUM
   : [+-]? '0' [xX] HEX+
   ;

INFINITY
   : [+-]? 'Infinity'
   ;
NAN
   : 'NaN'
   ;

fragment INT
   : '0' | [1-9] [0-9]*
   ;

// no leading zeros

fragment EXP
   : [Ee] [+\-]? INT
   ;

// \- since - means "range" inside [...]

WS
   : [ \t\n\r] + -> skip
   ;
