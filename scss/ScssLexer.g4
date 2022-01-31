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
AT_ELSE         : '@else';
IF              : 'if';
AT_IF           : '@if';
AT_FOR          : '@for';
AT_WHILE        : '@while';
AT_EACH         : '@each';
INCLUDE         : '@include';
IMPORT          : '@import';
USE             : '@use';
RETURN          : '@return';
MEDIA           : '@media';
CONTENT         : '@content';

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
COMMENT
  : '/*' .*? '*/' -> skip
  ;

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
