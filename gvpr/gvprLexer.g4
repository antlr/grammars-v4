lexer grammar  gvprLexer;

options { superClass = GvprLexerBase; }

// Insert here @header for C++ lexer.

MLCOMMENT : '/*' .*? '*/' -> channel(HIDDEN);
SLCOMMENT : '//' ~[\n\r]* -> channel(HIDDEN);
SHELLCOMMENT : '#' { this.IsColumnZero() }? ~[\n\r]* -> channel(HIDDEN);

IntegerConstant
    : DecimalConstant IntegerSuffix?
    | OctalConstant IntegerSuffix?
    | HexadecimalConstant IntegerSuffix?
    | BinaryConstant
    ;

FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

AEQ : '=' ;
AMEQ : '-=' ;
AMP : '&' ;
AND : '&&' ;
APEQ : '+=' ;
ASEQ : '*=' ;
ASLEQ : '/=' ;
BANG : '!' ;
BEGIN : 'BEGIN' ;
BEG_G : 'BEG_G' ;
BREAK : 'break' ;
CASE : 'case' ;
CB : ']' ;
CCBC : '}' ;
CIRCUMFLEX : '^' ;
CHAR : 'char' ;
COLON : ':' ;
COMMA : ',' ;
CONTINUE : 'continue' ;
CP : ')' ;
DEC : '--' ;
DEFAULT : 'default' ;
DOUBLE : 'double' ;
DOLLAR : '$' ;
E : 'E' ;
ELSE : 'else' ;
END : 'END' ;
END_G : 'END_G' ;
EQ : '==' ;
EXIT : 'exit' ;
FLOAT : 'float' ;
FOR : 'for' ;
GE : '>=' ;
GT : '>' ;
GSUB : 'gsub' ;
IF : 'if' ;
IN_OP : 'in' ;
INC : '++' ;
INT : 'int' ;
ITERATER : 'forr' ;
LE : '<=' ;
LT : '<' ;
LONG : 'long' ;
LSH : 'lsh' ;
N : 'N' ;
NE : '!=' ;
OB : '[' ;
OCBC : '{' ;
OP : '(' ;
OR : '||' ;
PERCENT : '%' ;
POUND : '#' ;
PRINTF : 'printf' ;
PROCEDURE : 'procedure' ;
QUERY : 'query' ;
RAND : 'rand' ;
RETURN : 'return' ;
RSH : 'rsh' ;
SCANF : 'scanf' ;
SEMI_COLON : ';' ;
SPLIT : 'split' ;
SPRINTF : 'sprintf' ;
SQUIGGLE : '~' ;
PLUS : '+' ;
QM : '?' ;
DOT : '.' ;
MINUS : '-' ;
SLASH : '/' ;
PIPE : '|' ;
SRAND : 'srand' ;
SSCANF : 'sscanf' ;
STAR : '*' ;
STATIC : 'static' ;
STRING : 'string' ;
SUB : 'sub' ;
SUBSTR : 'substr' ;
SWITCH : 'switch' ;
TOKENS : 'tokens' ;
UNSET : 'unset' ;
UNSIGNED : 'unsigned' ;
VOID : 'void' ;
WHILE : 'while' ;
XPRINT : 'print' ;

StringLit
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

ID
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

WS : [ \t\n\r]+ -> channel(HIDDEN);

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    ;

fragment
IdentifierNondigit
    :   Nondigit
    |   UniversalCharacterName
    //|   // other implementation-defined characters...
    | '$'
    | '#'
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
BinaryConstant
    : '0' [bB] [0-1]+
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
DigitSequence
    :   Digit+
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix (HexadecimalFractionalConstant | HexadecimalDigitSequence) BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   [eE] Sign? DigitSequence
    ;

fragment
Sign
    :   [+-]
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;


fragment
FloatingSuffix
    :   [flFL]
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
BinaryExponentPart
    :   [pP] Sign? DigitSequence
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\.]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit OctalDigit? OctalDigit?
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;
