lexer grammar abbLexer;

options { caseInsensitive = true; }

MODULE : 'module' ;
ENDMODULE : 'endmodule' ;
PROC: 'PROC';
ENDPROC: 'ENDPROC';
LOCAL: 'LOCAL';
CONST: 'CONST';
PERS: 'PERS';
VAR: 'VAR';
TOOLDATA: 'TOOLDATA';
WOBJDATA: 'WOBJDATA';
SPEEDDATA: 'SPEEDDATA';
ZONEDATA: 'ZONEDATA';
CLOCK: 'CLOCK';
BOOL: 'BOOL';
ON_CALL: '\\ON';
OFF_CALL: '\\OFF';

SLASH               : '/' ;
EQUALS              : ':=' ;
COMMA               : ',';
CURLY_OPEN          : '{';
CURLY_CLOSE         : '}';
COLON               : ':';
SEMICOLON           : ';';
BRACKET_OPEN        : '(';
BRACKET_CLOSE       : ')';
SQUARE_OPEN         : '[';
SQUARE_CLOSE        : ']';
DOT                 : '.';
DOUBLEDOT           : '..';
REL_BIGGER          : '>';
REL_BIGGER_OR_EQUAL : '>=';
REL_SMALLER         : '<';
REL_SMALLER_OR_EQUAL: '<=';
REL_EQUAL           : '==';
REL_NOTEQUAL        : '<>';
PLUS                : '+';
MINUS               : '-';
MULTIPLY            : '*';
PERCENT             : '%';
HASH                : '#';

WS
   : (' ' | '\t' | '\u000C') -> skip
   ;

NEWLINE
   : '\r'? '\n'
   ;

LINE_COMMENT
   : '!' ~ ('\n' | '\r')* -> skip
   ;

BOOLLITERAL
   : 'FALSE'
   | 'TRUE'
   ;

CHARLITERAL
   : '\'' (EscapeSequence | ~ ('\'' | '\\' | '\r' | '\n')) '\''
   ;

STRINGLITERAL
   : '"' (EscapeSequence | ~ ('\\' | '"' | '\r' | '\n'))* '"'
   ;

fragment EscapeSequence
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' | '0' .. '3' '0' .. '7' '0' .. '7' | '0' .. '7' '0' .. '7' | '0' .. '7')
   ;

FLOATLITERAL
   : ('0' .. '9') + '.' ('0' .. '9')* Exponent? | '.' ('0' .. '9') + Exponent? | ('0' .. '9') + Exponent
   ;

fragment Exponent
   : 'E' ('+' | '-')? ('0' .. '9') +
   ;

INTLITERAL
   : ('0' .. '9') + | HexPrefix HexDigit + HexSuffix | BinPrefix BinDigit + BinSuffix
   ;

fragment HexPrefix
   : '\'' 'H'
   ;

fragment HexDigit
   : '0' .. '9' | 'A' .. 'F'
   ;

fragment HexSuffix
   : '\''
   ;

fragment BinPrefix
   : '\'' 'B'
   ;

fragment BinDigit
   : '0' | '1'
   ;

fragment BinSuffix
   : '\''
   ;

IDENTIFIER
   : IdentifierStart IdentifierPart*
   ;

fragment IdentifierStart
   : 'A' .. 'Z' | '_'
   ;

fragment IdentifierPart
   : IdentifierStart | '0' .. '9'
   ;
