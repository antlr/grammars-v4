lexer grammar css3Lexer;

channels { ERROR }

OpenBracket:                    '[';
CloseBracket:                   ']';
OpenParen:                      '(';
CloseParen:                     ')';
OpenBrace:                      '{';
CloseBrace:                     '}';
SemiColon:                      ';';
Equal:                          '=';
Colon:                          ':';
Dot:                            '.';
Multiply:                       '*';
Divide:                         '/';
Pipe:                           '|';
Underscore:                     '_';

fragment At
    : '@'
    ;

fragment Hex
    : [0-9a-fA-F]
    ;

fragment NewlineOrSpace
    : '\r\n'
    | [ \t\r\n\f]
    |
    ;

fragment Unicode
    : '\\' Hex Hex? Hex? Hex? Hex? Hex? NewlineOrSpace
    ;

fragment Escape
    : Unicode
    | '\\' ~[\r\n\f0-9a-fA-F]
    ;

fragment Nmstart
    : [_a-zA-Z]
    | Nonascii
    | Escape
    ;

fragment Nmchar
    : [_a-zA-Z0-9\-]
    | Nonascii
    | Escape
    ;

// CSS2.2 Grammar defines the following, but I'm not sure how to add them to parser for error handling
// BadString :
// BadUri :
// BadComment :
// BadUri :

Comment
    : '/*' ~'*'* '*'+ ( ~[/*] ~'*'* '*'+ )* '/'
    ;

fragment Name
    : Nmchar+
    ;

Url
    : U R L '(' Whitespace ( [!#$%&*-~] | Nonascii | Escape )* Whitespace ')'
    ;

Space
    : [ \t\r\n\f]+
    ;

fragment Whitespace
    : Space
    |
    ;

fragment Newline
    : '\n'
    | '\r\n'
    | '\r'
    | '\f'
    ;

fragment ZeroToFourZeros
    : '0'? '0'? '0'? '0'?
    ;

fragment A
    : 'a'
    | 'A'
    | '\\' ZeroToFourZeros ('41'|'61') NewlineOrSpace
    ;

fragment B
    : 'b'
    | 'B'
    | '\\' ZeroToFourZeros ('42'|'62') NewlineOrSpace
    ;

fragment C
    : 'c'
    | 'C'
    | '\\' ZeroToFourZeros ('43'|'63') NewlineOrSpace
    ;

fragment D
    : 'd'
    | 'D'
    | '\\' ZeroToFourZeros ('44'|'64') NewlineOrSpace
    ;

fragment E
    : 'e'
    | 'E'
    | '\\' ZeroToFourZeros ('45'|'65') NewlineOrSpace
    ;

fragment F
    : 'f'
    | 'F'
    | '\\' ZeroToFourZeros ('46'|'66') NewlineOrSpace
    ;

fragment G
    : 'g'
    | 'G'
    | '\\' ZeroToFourZeros ('47'|'67') NewlineOrSpace
    | '\\g'
    | '\\G'
    ;

fragment H
    : 'h'
    | 'H'
    | '\\' ZeroToFourZeros ('48'|'68') NewlineOrSpace
    | '\\h'
    | '\\H'
    ;

fragment I
    : 'i'
    | 'I'
    | '\\' ZeroToFourZeros ('49'|'69') NewlineOrSpace
    | '\\i'
    | '\\I'
    ;

fragment K
    : 'k'
    | 'K'
    | '\\' ZeroToFourZeros ('4b'|'6b') NewlineOrSpace
    | '\\k'
    | '\\K'
    ;

fragment L
    : 'l'
    | 'L'
    | '\\' ZeroToFourZeros ('4c'|'6c') NewlineOrSpace
    | '\\l'
    | '\\L'
    ;

fragment M
    : 'm'
    | 'M'
    | '\\' ZeroToFourZeros ('4d'|'6d') NewlineOrSpace
    | '\\m'
    | '\\M'
    ;

fragment N
    : 'n'
    | 'N'
    | '\\' ZeroToFourZeros ('4e'|'6e') NewlineOrSpace
    | '\\n'
    | '\\N'
    ;

fragment O
    : 'o'
    | 'O'
    | '\\' ZeroToFourZeros ('4f'|'6f') NewlineOrSpace
    | '\\o'
    | '\\O'
    ;

fragment P
    : 'p'
    | 'P'
    | '\\' ZeroToFourZeros ('50'|'70') NewlineOrSpace
    | '\\p'
    | '\\P'
    ;

fragment Q
    : 'q'
    | 'Q'
    | '\\' ZeroToFourZeros ('51'|'71') NewlineOrSpace
    | '\\q'
    | '\\Q'
    ;

fragment R
    : 'r'
    | 'R'
    | '\\' ZeroToFourZeros ('52'|'72') NewlineOrSpace
    | '\\r'
    | '\\R'
    ;

fragment S
    : 's'
    | 'S'
    | '\\' ZeroToFourZeros ('53'|'73') NewlineOrSpace
    | '\\s'
    | '\\S'
    ;

fragment T
    : 't'
    | 'T'
    | '\\' ZeroToFourZeros ('54'|'74') NewlineOrSpace
    | '\\t'
    | '\\T'
    ;

fragment U
    : 'u'
    | 'U'
    | '\\' ZeroToFourZeros ('55'|'75') NewlineOrSpace
    | '\\u'
    | '\\U'
    ;

fragment V
    : 'v'
    | 'V'
    | '\\' ZeroToFourZeros ('56'|'76') NewlineOrSpace
    | '\\v'
    | '\\V'
    ;

fragment W
    : 'w'
    | 'W'
    | '\\' ZeroToFourZeros ('57'|'77') NewlineOrSpace
    | '\\w'
    | '\\W'
    ;

fragment X
    : 'x'
    | 'X'
    | '\\' ZeroToFourZeros ('58'|'78') NewlineOrSpace
    | '\\x'
    | '\\X'
    ;

fragment Y
    : 'y'
    | 'Y'
    | '\\' ZeroToFourZeros ('59'|'79') NewlineOrSpace
    | '\\y'
    | '\\Y'
    ;

fragment Z
    : 'z'
    | 'Z'
    | '\\' ZeroToFourZeros ('5a'|'7a') NewlineOrSpace
    | '\\z'
    | '\\Z'
    ;

fragment DashChar
    : '-'
    | '\\' ZeroToFourZeros '2d' NewlineOrSpace
    ;

Cdo
    : '<!--'
    ;

Cdc
    : '-->'
    ;

Includes
    : '~='
    ;

DashMatch
    : '|='
    ;

Hash
    : '#' Name
    ;

Import
    : At I M P O R T
    ;

Page
    : At P A G E
    ;

Media
    : At M E D I A
    ;

Namespace
    : At N A M E S P A C E
    ;

Charset
    : '@charset '
    ;

Important
    : '!' ( Space | Comment )* I M P O R T A N T
    ;

fragment FontRelative
    : Number E M
    | Number E X
    | Number C H
    | Number R E M
    ;

// https://www.w3.org/TR/css3-values/#viewport-relative-lengths
fragment ViewportRelative
    : Number V W
    | Number V H
    | Number V M I N
    | Number V M A X
    ;

fragment AbsLength
    : Number P X
    | Number C M
    | Number M M
    | Number I N
    | Number P T
    | Number P C
    | Number Q
    ;

fragment Angle
    : Number D E G
    | Number R A D
    | Number G R A D
    | Number T U R N
    ;

fragment Time
    : Number M S
    | Number S
    ;

fragment Freq
    : Number H Z
    | Number K H Z
    ;

Percentage
    : Number '%'
    ;

Url_
    : 'url('
    ;

UnicodeRange
    : [u|U] '+?' '?'? '?'? '?'? '?'? '?'?
    | [u|U] '+' Hex '?'? '?'? '?'? '?'? '?'?
    | [u|U] '+' Hex Hex '?'? '?'? '?'? '?'?
    | [u|U] '+' Hex Hex Hex '?'? '?'? '?'?
    | [u|U] '+' Hex Hex Hex Hex '?'? '?'?
    | [u|U] '+' Hex Hex Hex Hex Hex '?'?
    ;

// https://www.w3.org/TR/css3-mediaqueries/
MediaOnly
    : O N L Y
    ;

Not
    : N O T
    ;

And
    : A N D
    ;

fragment Resolution
    : Number D P I
    | Number D P C M
    | Number D P P X
    ;

fragment Length
    : AbsLength
    | FontRelative
    | ViewportRelative
    ;

Dimension
    : Length
    | Time
    | Freq
    | Resolution
    | Angle
    ;

UnknownDimension
    : Number Ident
    ;

// https://www.w3.org/TR/css3-selectors/
fragment Nonascii
    : ~[\u0000-\u007f]
    ;

Plus
    : '+'
    ;

Minus
    : '-'
    ;

Greater
    : '>'
    ;

Comma
    : ','
    ;

Tilde
    : '~'
    ;

PseudoNot
    : ':' N O T '('
    ;

Number
    : [0-9]+
    | [0-9]* '.' [0-9]+
    ;

String_
    : '"' ( ~[\n\r\f\\"] | '\\' Newline | Nonascii | Escape )* '"'
    | '\'' ( ~[\n\r\f\\'] | '\\' Newline | Nonascii | Escape )* '\''
    ;

PrefixMatch
    : '^='
    ;

SuffixMatch
    : '$='
    ;

SubstringMatch
    : '*='
    ;

// https://www.w3.org/TR/css-fonts-3/#font-face-rule
FontFace
    : At F O N T DashChar F A C E
    ;

// https://www.w3.org/TR/css3-conditional/
Supports
    : At S U P P O R T S
    ;

Or
    : O R
    ;

// https://www.w3.org/TR/css3-animations/
fragment VendorPrefix
    : '-' M O Z '-'
    | '-' W E B K I T '-'
    | '-' O '-'
    ;

Keyframes
    : At VendorPrefix? K E Y F R A M E S
    ;

From
    : F R O M
    ;

To
    : T O
    ;

// https://www.w3.org/TR/css3-values/#calc-syntax
Calc
    : 'calc('
    ;

// https://www.w3.org/TR/css-device-adapt-1/
Viewport
    : At V I E W P O R T
    ;

// https://www.w3.org/TR/css-counter-styles-3/
CounterStyle
    : At C O U N T E R DashChar S T Y L E
    ;

// https://www.w3.org/TR/css-fonts-3/
FontFeatureValues
    : At F O N T DashChar F E A T U R E DashChar V A L U E S
    ;

// https://msdn.microsoft.com/en-us/library/ms532847.aspx
DxImageTransform
    : 'progid:DXImageTransform.Microsoft.' Function_
    ;

AtKeyword
    : At Ident
    ;

// Variables
// https://www.w3.org/TR/css-variables-1
Variable
    : '--' Nmstart Nmchar*
    ;

Var
    : 'var('
    ;

// Give Ident least priority so that more specific rules matches first
Ident
    : '-'? Nmstart Nmchar*
    ;

Function_
    : Ident '('
    ;

UnexpectedCharacter:            . -> channel(ERROR);
