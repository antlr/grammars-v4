/*
MIT License

Copyright (c) 2016+ Ivo Smid

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
lexer grammar SwiftFinLexer;

BLOCK1 : LBrace '1' Colon -> pushMode(InsideValue)
       ;

BLOCK2 : LBrace '2' Colon -> pushMode(InsideValue)
       ;

BLOCK3 : LBrace '3' Colon -> pushMode(InsideMaps)
       ;

fragment
Block4 : LBrace '4' Colon
       ;

BLOCK4_A : Block4 Crlf -> pushMode(InsideB4)
         ;

BLOCK4_B : Block4 -> pushMode(InsideMaps)
         ;

BLOCK5 : LBrace '5' Colon -> pushMode(InsideMaps)
       ;

LBRACE : LBrace;
RBRACE : RBrace;
COLON  : Colon;
CRLF   : Crlf;

fragment
Crlf : '\r'? '\n'
     ;

fragment LBrace : '{' ;
fragment RBrace : '}' ;
fragment Colon  : ':' ;
fragment Minus  : '-' ;
fragment Any    : . ;

mode InsideMaps;

M_LBRACE : LBrace -> type(LBRACE), pushMode(InsideMaps);
M_RBRACE : RBrace -> type(RBRACE), popMode;
M_COLON  : Colon ;
M_VALUE  : ~[:] ;

mode InsideB4;

B4_END      : Minus RBrace -> popMode;
B4_COLON    : Colon ;
B4_CRLF     : Crlf ;
B4_VALUE    : ~[:] ;
//SPECIALS    : Any ; // every other token

mode InsideValue;
V_END   : RBrace -> popMode;
V_VALUE : Any ;
