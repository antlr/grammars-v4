lexer grammar abbLexer;

MODULE : 'module' ;
ENDMODULE : 'endmodule' ;
PROC : P R O C ;
ENDPROC : E N D P R O C;
LOCAL : L O C A L ;
CONST : C O N S T ;
PERS : P E R S ;
VAR : V A R ;
TOOLDATA : T O O L D A T A ;
WOBJDATA : W O B J D A T A ;
SPEEDDATA : S P E E D D A T A ;
ZONEDATA : Z O N E D A T A ;
CLOCK : C L O C K ;
BOOL : B O O L ;
ON_CALL : '\\' O N ;
OFF_CALL : '\\' O F F ;

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

fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


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
   : (F A L S E | T R U E)
   ;

CHARLITERAL
   : '\'' (EscapeSequence | ~ ('\'' | '\\' | '\r' | '\n')) '\''
   ;


STRINGLITERAL
   : '"' (EscapeSequence | ~ ('\\' | '"' | '\r' | '\n'))* '"'
   ;

fragment EscapeSequence
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' | ('0' .. '3') ('0' .. '7') ('0' .. '7') | ('0' .. '7') ('0' .. '7') | ('0' .. '7'))
   ;

FLOATLITERAL
   : ('0' .. '9') + '.' ('0' .. '9')* Exponent? | '.' ('0' .. '9') + Exponent? | ('0' .. '9') + Exponent
   ;


fragment Exponent
   : E ('+' | '-')? ('0' .. '9') +
   ;


INTLITERAL
   : ('0' .. '9') + | HexPrefix HexDigit + HexSuffix | BinPrefix BinDigit + BinSuffix
   ;


fragment HexPrefix
   : '\'' H
   ;


fragment HexDigit
   : ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
   ;


fragment HexSuffix
   : '\''
   ;


fragment BinPrefix
   : '\'' B
   ;


fragment BinDigit
   : ('0' | '1')
   ;


fragment BinSuffix
   : '\''
   ;

IDENTIFIER
   : IdentifierStart IdentifierPart*
   ;


fragment IdentifierStart
   : 'a' .. 'z' | 'A' .. 'Z' | '_'
   ;


fragment IdentifierPart
   : (IdentifierStart | '0' .. '9')
   ;