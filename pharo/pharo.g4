/*
BSD License

Copyright (c) 2026, Tom Everett
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

// A best-effort grammar for a practical subset of the Pharo language
// (https://pharo.org/), a modern Smalltalk-80 dialect. It covers the
// classic fileout-style surface syntax: class definitions and method
// definitions (ClassName >> selector), plus the full Smalltalk message
// language (unary/binary/keyword message sends, cascades, blocks,
// assignment, literal/byte/dynamic arrays, symbols, and pragmas) that
// also appears standalone as ordinary Pharo scripts/expressions.
grammar pharo;

program
    : chunk* EOF
    ;

chunk
    : classDefinition
    | methodDefinition
    | temps
    | statement
    ;

classDefinition
    : identifier KEYWORD symbolLiteral
      KEYWORD STRING
      KEYWORD STRING
      KEYWORD STRING
    ;

methodDefinition
    : identifier 'class'? '>>' methodPattern methodBody
    ;

methodPattern
    : IDENTIFIER                            # unaryMethodPattern
    | BINSEL IDENTIFIER                      # binaryMethodPattern
    | (KEYWORD IDENTIFIER)+                  # keywordMethodPattern
    ;

methodBody
    : pragma* temps? statement*
    ;

pragma
    : BINSEL pragmaBody BINSEL
    ;

pragmaBody
    : (KEYWORD pragmaArg)+
    | IDENTIFIER
    ;

pragmaArg
    : literal
    | identifier
    ;

temps
    : '|' identifier* '|'
    ;

statement
    : '^' expression '.'?
    | expression '.'?
    ;

expression
    : identifier ':=' expression   # assignment
    | cascade                      # plainExpr
    ;

cascade
    : keywordExpr (';' message)*
    ;

message
    : KEYWORD binaryExpr (KEYWORD binaryExpr)*   # cascadeKeywordMessage
    | BINSEL binaryExpr                           # cascadeBinaryMessage
    | IDENTIFIER                                  # cascadeUnaryMessage
    ;

keywordExpr
    : binaryExpr (KEYWORD binaryExpr)*
    ;

binaryExpr
    : unaryExpr (BINSEL unaryExpr)*
    ;

unaryExpr
    : primary IDENTIFIER*
    ;

primary
    : identifier
    | literal
    | '(' expression ')'
    | block
    | dynamicArray
    ;

block
    : '[' blockParams? temps? statement* ']'
    ;

blockParams
    : (':' IDENTIFIER)+ '|'
    ;

dynamicArray
    : '{' (expression ('.' expression)* '.'?)? '}'
    ;

identifier
    : IDENTIFIER
    | 'self'
    | 'super'
    | 'thisContext'
    | 'nil'
    | 'true'
    | 'false'
    ;

literal
    : INTEGER
    | FLOAT
    | STRING
    | CHAR
    | symbolLiteral
    | arrayLiteral
    | byteArrayLiteral
    ;

symbolLiteral
    : SYMBOL
    | QUOTED_SYMBOL
    | BINARY_SYMBOL
    ;

arrayLiteral
    : '#' '(' arrayElement* ')'
    ;

arrayElement
    : literal
    | identifier
    | KEYWORD
    | arrayLiteral
    | byteArrayLiteral
    ;

byteArrayLiteral
    : '#' '[' INTEGER* ']'
    ;

SYMBOL
    : '#' [a-zA-Z_] [a-zA-Z0-9_]* (':' [a-zA-Z_] [a-zA-Z0-9_]*)* ':'?
    ;

BINARY_SYMBOL
    : '#' [-+*/~<>=&|@%,?!\\]+
    ;

QUOTED_SYMBOL
    : '#' STRING
    ;

KEYWORD
    : [a-zA-Z_] [a-zA-Z0-9_]* ':'
    ;

IDENTIFIER
    : [a-zA-Z_] [a-zA-Z0-9_]*
    ;

BINSEL
    : [-+*/~<>=&|@%,?!\\]+
    ;

FLOAT
    : '-'? [0-9]+ '.' [0-9]+ ([eEdDqQ] '-'? [0-9]+)?
    ;

INTEGER
    : '-'? [0-9]+ ('r' [0-9A-Za-z]+)?
    ;

STRING
    : '\'' ('\'\'' | ~['])* '\''
    ;

CHAR
    : '$' .
    ;

COMMENT
    : '"' ('""' | ~["])* '"' -> skip
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
