/*
MIT License

Copyright (c) 2023 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

lexer grammar DesktopEntryLexer;

HASH : '#' -> channel(HIDDEN), pushMode(COMMENT_MODE) ;
LEFT_BRACKET : '[' -> mode(HEADER_MODE) ;
NEWLINE : '\r'? '\n' -> channel(HIDDEN) ;
SPACE : [ \t]+ -> channel(HIDDEN) ;

mode COMMENT_MODE;
COMMENT_TEXT : ~[\r\n]+ -> channel(HIDDEN) ;
NEWLINE_0 : NEWLINE -> channel(HIDDEN), type(NEWLINE), popMode ;

mode HEADER_MODE;
GROUP_NAME : ASCII_PRINTABLE_NO_BRACKETS+ ;
NEWLINE_1 : NEWLINE -> channel(HIDDEN), type(NEWLINE), mode(ENTRY_MODE) ;
RIGHT_BRACKET : ']' ;

mode ENTRY_MODE;
HASH_0 : HASH -> channel(HIDDEN), type(HASH), pushMode(COMMENT_MODE) ;
KEY_NAME : IDENTIFIER -> mode(KEY_MODE) ;
LEFT_BRACKET_0 : LEFT_BRACKET -> type(LEFT_BRACKET), mode(HEADER_MODE) ;
NEWLINE_2 : NEWLINE -> channel(HIDDEN), type(NEWLINE) ;
SPACE_0 : SPACE -> channel(HIDDEN), type(SPACE) ;

mode KEY_MODE;
EQUAL : '=' -> mode(VALUE_MODE) ;
LEFT_BRACKET_1 : LEFT_BRACKET -> type(LEFT_BRACKET), pushMode(LOCALE_MODE) ;
SPACE_1 : SPACE -> channel(HIDDEN), type(SPACE) ;

mode VALUE_MODE;
TRUE : 'true' ;
FALSE : 'false' ;
NEWLINE_3 : NEWLINE -> channel(HIDDEN), type(NEWLINE), mode(ENTRY_MODE) ;
NUMBER : [0-9]+ ( '.' [0-9]+ )? ;
SEMICOLON : ';' ;
SPACE_2 : SPACE -> channel(HIDDEN), type(SPACE) ;
STRING : ( ~[\n\r\\;] | ESC_SEQ )+ ;

mode LOCALE_MODE;
AT : '@' -> mode(MODIFIER_MODE) ;
DOT : '.' -> mode(ENCODING_MODE) ;
LANGUAGE : IDENTIFIER ;
RIGHT_BRACKET_0 : RIGHT_BRACKET -> type(RIGHT_BRACKET), popMode ;
UNDERSCORE : '_' -> mode(COUNTRY_MODE) ;

mode COUNTRY_MODE;
AT_0 : '@' -> mode(MODIFIER_MODE) ;
COUNTRY : IDENTIFIER ;
DOT_0 : '.' -> mode(ENCODING_MODE) ;
RIGHT_BRACKET_1 : RIGHT_BRACKET -> type(RIGHT_BRACKET), popMode ;

mode ENCODING_MODE;
AT_1 : '@' -> mode(MODIFIER_MODE) ;
ENCODING : IDENTIFIER ;
RIGHT_BRACKET_2 : RIGHT_BRACKET -> type(RIGHT_BRACKET), popMode ;

mode MODIFIER_MODE;
MODIFIER : IDENTIFIER ;
RIGHT_BRACKET_3 : RIGHT_BRACKET -> type(RIGHT_BRACKET), popMode ;

fragment ASCII_PRINTABLE_NO_BRACKETS : [\u0020-\u005a\u005c\u005e-\u007e] ;
fragment ESC_SEQ : '\\' [nrst\\;] ;
fragment IDENTIFIER : [A-Za-z] [A-Za-z0-9\-]* ;
