/*
Zig language grammar.
The MIT License (MIT).

Copyright (c) 2025, Michał Lorek.

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

lexer grammar ZigLexer;

options {
    caseInsensitive = false;
}

ADDRSPACE : 'addrspace';
ALIGN : 'align';
ALLOWZERO : 'allowzero';
AND : 'and';
ANYFRAME : 'anyframe';
ANYTYPE : 'anytype';
ASM : 'asm';
BREAK : 'break';
CALLCONV : 'callconv';
CATCH : 'catch';
COMPTIME : 'comptime';
CONST : 'const';
CONTINUE : 'continue';
DEFER : 'defer';
ELSE : 'else';
ENUM : 'enum';
ERRDEFER : 'errdefer';
ERROR : 'error';
EXPORT : 'export';
EXTERN : 'extern';
FN : 'fn';
FOR : 'for';
IF : 'if';
INLINE : 'inline';
LINKSECTION : 'linksection';
NOALIAS : 'noalias';
NOINLINE : 'noinline';
NOSUSPEND : 'nosuspend';
OPAQUE : 'opaque';
OR : 'or';
ORELSE : 'orelse';
PACKED : 'packed';
PUB : 'pub';
RESUME : 'resume';
RETURN : 'return';
STRUCT : 'struct';
SUSPEND : 'suspend';
SWITCH : 'switch';
TEST : 'test';
THREADLOCAL : 'threadlocal';
TRY : 'try';
UNION : 'union';
UNREACHABLE : 'unreachable';
VAR : 'var';
VOID : 'void';
VOLATILE : 'volatile';
WHILE : 'while';

SPACE: [ \t\r\n]+ -> channel(HIDDEN);


AMPERSAND : '&';
AMPERSANDEQUAL : '&=';
ASTERISK : '*';
ASTERISK2 : '**';
ASTERISKEQUAL : '*=';
ASTERISKPERCENT : '*%';
ASTERISKPERCENTEQUAL : '*%=';
ASTERISKPIPE : '*|';
ASTERISKPIPEEQUAL : '*|=';
CARET : '^';
CARETEQUAL : '^=';
COLON : ':';
COMMA : ',';
DOT : '.';
DOT2 : '..';
DOT3 : '...';
DOTASTERISK : '.*';
DOTQUESTIONMARK : '.?';
EQUAL : '=';
EQUALEQUAL : '==';
EQUALRARROW : '=>';
EXCLAMATIONMARK : '!';
EXCLAMATIONMARKEQUAL : '!=';
LARROW : '<';
LARROW2 : '<<';
LARROW2EQUAL : '<<=';
LARROW2PIPE : '<<|';
LARROW2PIPEEQUAL : '<<|=';
LARROWEQUAL : '<=';
LBRACE : '{';
LBRACKET : '[';
LPAREN : '(';
MINUS : '-';
MINUSEQUAL : '-=';
MINUSPERCENT : '-%';
MINUSPERCENTEQUAL : '-%=';
MINUSPIPE : '-|';
MINUSPIPEEQUAL : '-|=';
MINUSRARROW : '->';
PERCENT : '%';
PERCENTEQUAL : '%=';
PIPE : '|';
PIPE2 : '||';
PIPEEQUAL : '|=';
PLUS : '+';
PLUS2 : '++';
PLUSEQUAL : '+=';
PLUSPERCENT : '+%';
PLUSPERCENTEQUAL : '+%=';
PLUSPIPE : '+|';
PLUSPIPEEQUAL : '+|=';
LETTERC : 'c';
QUESTIONMARK : '?';
RARROW : '>';
RARROW2 : '>>';
RARROW2EQUAL : '>>=';
RARROWEQUAL : '>=';
RBRACE : '}';
RBRACKET : ']';
RPAREN : ')';
SEMICOLON : ';';
SLASH : '/';
SLASHEQUAL : '/=';
TILDE : '~';

//# *** Tokens ***

fragment Bin : [01];
fragment Bin_ : '_'? Bin;
fragment Oct : [0-7];
fragment Oct_ : '_'? Oct;
fragment Hex : [0-9a-fA-F];
fragment Hex_ : '_'? Hex;
fragment Dec : [0-9];
fragment Dec_ : '_'? Dec;

fragment Bin_int : Bin Bin_*;
fragment Oct_int : Oct Oct_*;
fragment Dec_int : Dec Dec_*;
fragment Hex_int : Hex Hex_*;

fragment Ox80_oxBF : [\u0200-\u0277];
fragment OxF4 : '\u0364';
fragment Ox80_ox8F : [\u0200-\u0217];
fragment OxF1_oxF3 : [\u0361-\u0363];
fragment OxF0 : '\u0360';
fragment Ox90_0xBF : [\u0220-\u0277];
fragment OxEE_oxEF : [\u0356-\u0357];
fragment OxED : '\u0355';
fragment Ox80_ox9F : [\u0200-\u0237];
fragment OxE1_oxEC : [\u0341-\u0354];
fragment OxE0 : '\u0340';
fragment OxA0_oxBF : [\u0240-\u0277];
fragment OxC2_oxDF : [\u0302-\u0337];

//# From https://lemire.me/blog/2018/05/09/how-quickly-can-you-check-that-a-string-is-valid-unicode-utf-8/
//# First Byte      Second Byte     Third Byte      Fourth Byte
//# [0x00,0x7F]
//# [0xC2,0xDF]     [0x80,0xBF]
//#    0xE0         [0xA0,0xBF]     [0x80,0xBF]
//# [0xE1,0xEC]     [0x80,0xBF]     [0x80,0xBF]
//#    0xED         [0x80,0x9F]     [0x80,0xBF]
//# [0xEE,0xEF]     [0x80,0xBF]     [0x80,0xBF]
//#    0xF0         [0x90,0xBF]     [0x80,0xBF]     [0x80,0xBF]
//# [0xF1,0xF3]     [0x80,0xBF]     [0x80,0xBF]     [0x80,0xBF]
//#    0xF4         [0x80,0x8F]     [0x80,0xBF]     [0x80,0xBF]

fragment Multibyte_utf8 :
       OxF4      Ox80_ox8F Ox80_oxBF Ox80_oxBF
     | OxF1_oxF3 Ox80_oxBF Ox80_oxBF Ox80_oxBF
     | OxF0      Ox90_0xBF Ox80_oxBF Ox80_oxBF
     | OxEE_oxEF Ox80_oxBF Ox80_oxBF
     | OxED      Ox80_ox9F Ox80_oxBF
     | OxE1_oxEC Ox80_oxBF Ox80_oxBF
     | OxE0      OxA0_oxBF Ox80_oxBF
     | OxC2_oxDF Ox80_oxBF;

fragment Non_control_ascii : [\u0040-\u0176];

fragment Char_escape
    : '\\x' Hex Hex
    | '\\u{" hex+ "}'
    | '\\' [nr\\t'"]
    ;

fragment Char_char
    : Multibyte_utf8
    | Char_escape
    | Non_control_ascii
    ;

String_char
    : Multibyte_utf8
    | Char_escape
    | Non_control_ascii
    ;

Container_doc_comment : ('//!' ~[\n]* [ \n]* )+ -> channel(HIDDEN);
Doc_comment : ('///' ~[^\n]* [ \n]* )+ -> channel(HIDDEN);
Line_comment : '//' ~[\n]* '////' [^\n]* -> channel(HIDDEN);
Line_string : ('\\\\' [^\n]* [ \n]*)+;

IDENTIFIER : [A-Za-z_] [A-Za-z_0-9]* | '@' STRINGLITERALSINGLE;
STRINGLITERALSINGLE : '"' String_char* '"';
STRINGLITERAL
    : STRINGLITERALSINGLE
    | Line_string+
    ;
CHAR_LITERAL : '\'' Char_char '\'';

FLOAT
    : '0x' Hex_int '.' Hex_int ([pP] [-+]? Dec_int)?
    |      Dec_int '.' Dec_int ([eE] [-+]? Dec_int)?
    | '0x' Hex_int [pP] [-+]? Dec_int
    |      Dec_int [eE] [-+]? Dec_int;
INTEGER
    : '0b' Bin_int
    | '0o' Oct_int
    | '0x' Hex_int
    |      Dec_int;
BUILTINIDENTIFIER : '@';

TODO : 'TODO';