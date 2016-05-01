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
parser grammar SwiftFinParser;

options {
	tokenVocab = SwiftFinLexer ;
}

messages : message+ EOF
         ;

message : block1 block2? block3? block4? block5?
        ;

block1 : BLOCK1 value V_END
       ;

block2 : BLOCK2 value V_END
       ;

block3 : BLOCK3 map RBRACE
       ;

block4 : BLOCK4_A block4Item+ B4_END
       | BLOCK4_B map RBRACE
       ;

block4Item : B4_COLON block4Field B4_COLON block4Line+
           ;

block4Field : B4_VALUE+
            ;

block4Line : B4_VALUE+ B4_CRLF
           | B4_VALUE+ B4_COLON (B4_VALUE | B4_COLON)* B4_CRLF
           | B4_COLON B4_COLON+ (B4_VALUE | B4_COLON)* B4_CRLF
           | B4_COLON+ B4_VALUE+ B4_COLON* B4_CRLF
           ;

block5 : BLOCK5 map RBRACE
       ;

value : V_VALUE+
      ;

map : keyValue+ ;

keyValue : LBRACE mKey M_COLON mValue? RBRACE
         ;

mKey : M_VALUE+
     ;

mValue : (M_VALUE | M_COLON)+
       ;
