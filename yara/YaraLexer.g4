/*
YARA grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

lexer grammar YaraLexer;

ALL                       : 'all';
AND                       : 'and';
ANY                       : 'any';
AT                        : 'at';
CONDITION                 : 'condition' -> mode(DEFAULT_MODE);
CONTAINS                  : 'contains';
ENDSWITH                  : 'endswith';
ENTRYPOINT                : 'entrypoint';
FALSE                     : 'false';
FILESIZE                  : 'filesize';
FOR                       : 'for';
GLOBAL                    : 'global';
IMPORT                    : 'import';
ICONTAINS                 : 'icontains';
IENDSWITH                 : 'iendswith';
IEQUALS                   : 'iequals';
IN                        : 'in';
INCLUDE                   : 'include';
INT16                     : 'int16';
INT16BE                   : 'int16be';
INT32                     : 'int32';
INT32BE                   : 'int32be';
INT8                      : 'int8';
INT8BE                    : 'int8be';
ISTARTSWITH               : 'istartswith';
MATCHES                   : 'matches';
META                      : 'meta' ;
NONE                      : 'none';
NOT                       : 'not';
OF                        : 'of';
OR                        : 'or';
PRIVATE                   : 'private';
RULE                      : 'rule';
STARTSWITH                : 'startswith';
STRINGS                   : 'strings' -> mode(STR);
THEM                      : 'them';
TRUE                      : 'true';
UINT16                    : 'uint16';
UINT16BE                  : 'uint16be';
UINT32                    : 'uint32';
UINT32BE                  : 'uint32be';
UINT8                     : 'uint8';
UINT8BE                   : 'uint8be';
DEFINED                   : 'defined';

LP                        : '(';
RP                        : ')';
LCB                       : '{';
RCB                       : '}';
LSB                       : '[';
RSB                       : ']';
SLASH                     : '/';
PIPE                      : '|';
QM                        : '?';
EM                        : '!';
AT_                       : '@';
COLON                     : ':';
ASSIGN                    : '=';
MINUS                     : '-';
PLUS                      : '+';
STAR                      : '*';
DIV                       : '\\';
MOD                       : '%';
BITAND                    : '&';
BITXOR                    : '^';
BITNOT                    : '~';
LT                        : '<';
LE                        : '<=';
GT                        : '>';
GE                        : '>=';
EQUAL                     : '==';
NE                        : '!=';
DOT                       : '.';
LSHIFT                    : '<<';
RSHIFT                    : '>>';
RANGE                     : '..';
COMMA                     : ',';
DOLLAR                    : '$';
HASH                      : '#';

WHITE_SPACE:             [ \t\r\n]+                      -> channel(HIDDEN);

BLOCK_COMMENT:           '/*' (BLOCK_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT:            '//' ~[\r\n]*                   -> channel(HIDDEN);

DECIMAL_LITERAL          : DEC_DIGIT+;
HEX_STR                  : BYTE_MASKED+;
HEX_LITERAL              : '0x' HEX_DIGIT+;
STRING_ID                : '$' ALPHA_NUM_UNDERSCORE*;
ID                       : [a-zA-Z_] [a-zA-Z0-9_]* '*'?;
STRING_WILD              : '$' ALPHA_NUM_UNDERSCORE* '*';

DOUBLE_QUOTE_STR         : '"' (~'"' | ESCAPE_SEQUENCE)+ '"';
COUNT_REF                : '#' ID;
OFFSET_REF               : '@' ID;
LENGTH_REF               : '!' ID;
SIZE_LITERAL             : DEC_DIGIT+ ('MB' | 'KB')?;
BYTE_MASKED              : HEX_DIGIT_MASKED HEX_DIGIT_MASKED;

fragment ALPHA_NUM_UNDERSCORE
    : [a-zA-Z0-9_]
    ;

fragment ESCAPE_SEQUENCE
    : '\\' [tnr"\\]
    | '\\' 'x' HEX_DIGIT HEX_DIGIT
    ;

fragment HEX_DIGIT
    : [a-fA-F0-9]
    ;

fragment HEX_DIGIT_MASKED
    : [a-fA-F0-9?]
    ;

fragment DEC_DIGIT
    : [0-9]
    ;

mode STR;

WS_S                      : WHITE_SPACE -> type(WHITE_SPACE), channel(HIDDEN);
BLOCK_COMMENT_S           : BLOCK_COMMENT -> type(BLOCK_COMMENT), channel(HIDDEN);
LINE_COMMENT_S            : LINE_COMMENT -> type(LINE_COMMENT), channel(HIDDEN);

LP_S                      : LP -> type(LP);
RP_S                      : RP -> type(RP);

ASCII                     : 'ascii';
BASE64                    : 'base64';
BASE64WIDE                : 'base64wide';
FULLWORD                  : 'fullword';
NOCASE                    : 'nocase';
PRIVATE_S                 : PRIVATE -> type(PRIVATE);
WIDE                      : 'wide';
XOR                       : 'xor';

COLON_S                   : COLON -> type(COLON);
ASSIGN_S                  : '=';
LCB_S                     : '{' -> mode(WILD);
STRING_ID_S               : STRING_ID -> type(STRING_ID);
DOUBLE_QUOTE_STR_S        : DOUBLE_QUOTE_STR -> type(DOUBLE_QUOTE_STR);
REGEX_STR                 : '/' (~'/' | '\\/')+ '/' 'i'? 's'?;
CONDITION_S               : CONDITION -> type(CONDITION), mode(DEFAULT_MODE);

mode WILD;

WS_W                      : WHITE_SPACE -> type(WHITE_SPACE), channel(HIDDEN);
BLOCK_COMMENT_W           : BLOCK_COMMENT -> type(BLOCK_COMMENT), channel(HIDDEN);
LINE_COMMENT_W            : LINE_COMMENT -> type(LINE_COMMENT), channel(HIDDEN);

DASH_W                    : MINUS -> type(MINUS);
HEX_W                     : HEX_STR -> type(HEX_STR);
LSB_W                     : LSB -> type(LSB), mode(JUMP);
LRB_W                     : LP -> type(LP), mode(ALT);
RCB_W                     : RCB -> type(RCB), mode(STR);

mode JUMP;

WS_J                      : WHITE_SPACE -> type(WHITE_SPACE), channel(HIDDEN);
BLOCK_COMMENT_J           : BLOCK_COMMENT -> type(BLOCK_COMMENT), channel(HIDDEN);
LINE_COMMENT_J            : LINE_COMMENT -> type(LINE_COMMENT), channel(HIDDEN);

DEC                       : [0-9]+;
DASH_J                    : MINUS -> type(MINUS);
RSB_J                     : RSB -> type(RSB), mode(WILD);

mode ALT;

WS_A                      : WHITE_SPACE -> type(WHITE_SPACE), channel(HIDDEN);
BLOCK_COMMENT_A           : BLOCK_COMMENT -> type(BLOCK_COMMENT), channel(HIDDEN);
LINE_COMMENT_A            : LINE_COMMENT -> type(LINE_COMMENT), channel(HIDDEN);

HEX_A                     : HEX_STR -> type(HEX_STR);
PIPE_A                    : PIPE -> type(PIPE);
RRB_A                     : RP -> type(RP), mode(WILD);
