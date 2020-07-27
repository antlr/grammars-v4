// Student Main
// 2020-07-22
// Public domain

// JSON5 is a superset of JSON, it included some feature from ES5.1
// See https://json5.org/
// Derived from ../json/JSON.g4 which original derived from http://json.org
grammar JSON5;

json5
   : value? EOF
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
   | LITERAL
   ;

value
   : STRING
   | NUMBER
   | obj
   | arr
   | LITERAL
   ;

arr
   : '[' value (',' value)* ','? ']'
   | '[' ']'
   ;

// Lexer

SINGLE_LINE_COMMENT
   : '//' .*? (NEWLINE | EOF) -> skip
   ;

MULTI_LINE_COMMENT
   : '/*' .*? '*/' -> skip
   ;

LITERAL
   : 'true'
   | 'false'
   | 'null'
   ;

STRING
   : '"' DOUBLE_QUOTE_CHAR* '"'
   | '\'' SINGLE_QUOTE_CHAR* '\''
   ;

fragment DOUBLE_QUOTE_CHAR
   : ~["\\\r\n]
   | ESCAPE_SEQUENCE
   ;

fragment SINGLE_QUOTE_CHAR
   : ~['\\\r\n]
   | ESCAPE_SEQUENCE
   ;

fragment ESCAPE_SEQUENCE
   : '\\'
   ( NEWLINE
   | UNICODE_SEQUENCE       // \u1234
   | ['"\\/bfnrtv]          // single escape char
   | ~['"\\bfnrtv0-9xu\r\n] // non escape char
   | '0'                    // \0
   | 'x' HEX HEX            // \x3a
   )
   ;

IDENTIFIER
   : IDENTIFIER_START IDENTIFIER_PART*
   ;

fragment IDENTIFIER_START
   : [\p{L}]
   | '$'
   | '_'
   | '\\' UNICODE_SEQUENCE
   ;

fragment IDENTIFIER_PART
   : IDENTIFIER_START
   | [\p{M}]
   | [\p{N}]
   | [\p{Pc}]
   | '\u200C'
   | '\u200D'
   ;

fragment UNICODE_SEQUENCE
   : 'u' HEX HEX HEX HEX
   ;

NUMBER
   : SYMBOL
   ( INT ('.' [0-9]*)? EXP? // +1.e2, 1234, 1234.5
   | '.' [0-9]+ EXP?        // -.2e3
   | '0' [xX] HEX+          // 0x12345678
   | 'Infinity'
   | 'NaN'
   )
   ;

fragment SYMBOL
   : ('+' | '-')?
   ;

fragment HEX
   : [0-9a-fA-F]
   ;

fragment INT
   : '0' | [1-9] [0-9]*
   ;

fragment EXP
   : [Ee] SYMBOL INT
   ;

fragment NEWLINE
   : '\r\n'
   | [\r\n\u2028\u2029]
   ;

WS
   : [ \t\n\r] + -> skip
   ;
