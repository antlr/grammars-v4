/*
 [The "BSD licence"]
 Copyright (c) 2014 Vlad Shlosberg
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

lexer grammar ScssLexer;

// Tokens
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

fragment Url
    : ( [!#$%&*-~] | Nonascii | Escape )*
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
    : '@' I M P O R T
    ;

Use
    : '@' U S E
    ;

Require
    : '@' R E Q U I R E
    ;

Default
    : '!' D E F A U L T
    ;

Page
    : '@' P A G E
    ;

Media
    : '@' M E D I A
    ;

Namespace
    : '@' N A M E S P A C E
    ;

fragment AtKeyword
    : '@' Ident
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

Uri
    : U R L '(' Whitespace String_ Whitespace ')'
    | U R L '(' Whitespace Url Whitespace ')'
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

Semi
    : ';'
    ;

Times
    : '*'
    ;

Colon
    : ':'
    ;

BlockStart
    : '{'
    ;

BlockEnd
    : '}'
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

Div
    : '/'
    ;

Lparen
    : '('
    ;

Rparen
    : ')'
    ;

Lbrack
    : '['
    ;

Rbrack
    : ']'
    ;

At
    : '@'
    ;

Eq
    : '='
    ;

Pipe
    : '|'
    ;

Dot
    : '.'
    ;

Under
    : '_'
    ;

Dollar
    : '$'
    ;

// https://www.w3.org/TR/css-fonts-3/#font-face-rule
FontFace
    : '@' F O N T DashChar F A C E
    ;

// https://www.w3.org/TR/css3-conditional/
Supports
    : '@' S U P P O R T S
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
    : '@' VendorPrefix? K E Y F R A M E S
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
    : '@' V I E W P O R T
    ;

// https://www.w3.org/TR/css-counter-styles-3/
CounterStyle
    : '@' C O U N T E R DashChar S T Y L E
    ;

// https://www.w3.org/TR/css-fonts-3/
FontFeatureValues
    : '@' F O N T DashChar F E A T U R E DashChar V A L U E S
    ;

// https://msdn.microsoft.com/en-us/library/ms532847.aspx
DxImageTransform
    : 'progid:DXImageTransform.Microsoft.' Function_
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



/**
NULL_              : 'null';


IN              : 'in';

Unit
  : ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
  ;

COMBINE_COMPARE : '&&' | '||';

Ellipsis          : '...';

InterpolationStart
  : HASH BlockStart -> pushMode(IDENTIFY)
  ;

//Separators
LPAREN          : '(';
RPAREN          : ')';
BlockStart      : '{';
BlockEnd        : '}';
LBRACK          : '[';
RBRACK          : ']';
GT              : '>';
TIL             : '~';

LT              : '<';
COLON           : ':';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';
DOLLAR          : '$';
AT              : '@';
AND             : '&';
HASH            : '#';
PLUS            : '+';
TIMES           : '*';
DIV             : '/';
MINUS           : '-';
PERC            : '%';

// When a variable or parenthesized statement is negated, there cannot be a
// space after the - or +.
MINUS_DOLLAR    : MINUS DOLLAR;
PLUS_DOLLAR     : PLUS DOLLAR;
MINUS_LPAREN    : MINUS LPAREN;
PLUS_LPAREN     : PLUS LPAREN;


UrlStart
  : 'url' LPAREN -> pushMode(URL_STARTED)
  ;



EQEQ            : '==';
NOTEQ           : '!=';



EQ              : '=';
PIPE_EQ         : '|=';
TILD_EQ         : '~=';



MIXIN           : '@mixin';
FUNCTION        : '@function';
FONT_FACE       : '@font-face';
FORWARD         : '@forward';
AT_ELSE         : '@else';
IF              : 'if';
AT_IF           : '@if';
AT_FOR          : '@for';
AT_WHILE        : '@while';
AT_EACH         : '@each';
INCLUDE         : '@include';
IMPORT          : '@import';
USE             : '@use';
REQUIRE         : '@require';
RETURN          : '@return';
MEDIA           : '@media';
CONTENT         : '@content';
KEYFRAMES       : '@keyframes';

FROM            : 'from';
TO              : 'to';
THROUGH         : 'through';
POUND_DEFAULT   : '!default';
IMPORTANT       : '!important';
ONLY            : 'only';
NOT             : 'not';
AND_WORD        : 'and';
USING           : 'using';
AS              : 'as';
WITH            : 'with';

Identifier
  : (('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
    ('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
  | '-' ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
    ('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*) -> pushMode(IDENTIFY)
  ;

PseudoIdentifier
  : COLON COLON? Identifier -> pushMode(IDENTIFY)
  ;

FunctionIdentifier
    : Identifier LPAREN
    ;

fragment STRING
    : '"' (~('"'|'\n'|'\r'))* '"'
    | '\'' (~('\''|'\n'|'\r'))* '\''
    ;

// string literals
StringLiteral
  : STRING
  ;


Number
  : '-' (('0'..'9')* '.')? ('0'..'9')+
  | (('0'..'9')* '.')? ('0'..'9')+
  ;

Var
  : FunctionIdentifier MINUS Identifier RPAREN
  ;

Color
  : '#' ('0'..'9'|'a'..'f'|'A'..'F')+
  ;


// Whitespace -- ignored
WS
  : (' '|'\t'|'\n'|'\r'|'\r\n')+ -> skip
  ;

// Single-line comments
SL_COMMENT
  : '//'
    (~('\n'|'\r'))* ('\n'|'\r'('\n')?) -> skip
  ;


// multiple-line comments
//COMMENT
//  : '/*' .*? '*///' -> skip
/*  ;

mode URL_STARTED;
UrlEnd                 : RPAREN -> popMode;
Url                    :  STRING | (~(')' | '\n' | '\r' | ';'))+;

mode IDENTIFY;
BlockStart_ID          : BlockStart -> popMode, type(BlockStart);
SPACE                  : WS -> popMode, skip;
DOLLAR_ID              : DOLLAR -> type(DOLLAR);


InterpolationStartAfter  : InterpolationStart;
InterpolationEnd_ID    : BlockEnd -> type(BlockEnd);
IdentifierAfter        : Identifier;

// All tokens that can signal the end of identifiers
Ellipsis_ID               : Ellipsis -> popMode, type(Ellipsis);
DOT_ID                    : DOT -> popMode, type(DOT);
LBRACK_ID                 : LBRACK -> popMode, type(LBRACK);
RBRACK_ID                 : RBRACK -> popMode, type(RBRACK);
LPAREN_ID                 : LPAREN -> popMode, type(LPAREN);
RPAREN_ID                 : RPAREN -> popMode, type(RPAREN);
COLON_ID                  : COLON -> popMode, type(COLON);
COMMA_ID                  : COMMA -> popMode, type(COMMA);
SEMI_ID                   : SEMI -> popMode, type(SEMI);
EQ_ID                     : EQ -> popMode, type(EQ);
PIPE_EQ_ID                : PIPE_EQ -> popMode, type(PIPE_EQ);
TILD_EQ_ID                : TILD_EQ -> popMode, type(TILD_EQ);
PseudoIdentifier_ID       : PseudoIdentifier -> popMode, type(PseudoIdentifier);

*/
