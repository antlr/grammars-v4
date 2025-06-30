/*
BSD License

Copyright (c) 2025, Tom Everett and Ian MacDonald
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar D2;

diagram
   : statement* EOF
   ;

statement
   : nodeDeclaration
   | edgeDeclaration
   | attributeBlock
   | COMMENT
   ;

nodeDeclaration
   : name label? attributeBlock? (COLON block)?
   ;

edgeDeclaration
   : name (edgeOp name)+ label? attributeBlock?
   ;

block
   : LBRACE statement* RBRACE
   ;

label
   : COLON name
   ;

attributeBlock
   : LBRACK attributeEntry (COMMA attributeEntry)* COMMA? RBRACK
   ;

attributeEntry
   : name COLON expression
   ;

expression
   : STRING
   | NUMBER
   | BOOLEAN
   ;

edgeOp
   : ARROW
   | DASH
   | LARROW
   ;

name
   : IDENTIFIER (DOT IDENTIFIER)*
   ;

BOOLEAN
   : 'true'
   | 'false'
   ;

NUMBER
   : '-'? DIGIT+ ('.' DIGIT+)?
   ;

STRING
   : '"' (~ ["\\\r\n] | ESC)* '"'
   ;

fragment ESC
   : '\\' ["\\/bfnrt]
   ;

IDENTIFIER
   : [a-zA-Z_] [a-zA-Z0-9_'-]*
   ;

COMMENT
   : '#' ~ [\r\n]* -> skip
   ;

LBRACK
   : '['
   ;

RBRACK
   : ']'
   ;

LBRACE
   : '{'
   ;

RBRACE
   : '}'
   ;

COLON
   : ':'
   ;

COMMA
   : ','
   ;

DOT
   : '.'
   ;

ARROW
   : '->'
   ;

LARROW
   : '<-'
   ;

DASH
   : '--'
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

fragment DIGIT
   : [0-9]
   ;

