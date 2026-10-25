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

/*
* RoboTalk (the RoboWar robot programming language) is a flat, Reverse
* Polish Notation token stream, not a block-structured language: every
* element (number, label definition, label/variable reference, quoted
* variable, operator) is just a token, and tokens are separated by
* insignificant delimiters (spaces, tabs, newlines, commas, semicolons -
* see the RoboWar manual, section III). IF/IFE/IFG/IFEG/JUMP/CALL/RETURN
* etc. are ordinary operator words acting on the stack, not syntactic
* control-flow keywords requiring a matching block, so there is no
* nested statement/expression grammar to speak of beyond the lexical
* rules below (numbers, quoted variables, label definitions, comments).
*
* This grammar is split into a lexer grammar and a parser grammar
* because the nestable '{' ... '}' block-comment form needs lexer
* modes, which ANTLR4 only allows in a standalone lexer grammar.
*/
lexer grammar robotalkLexer;

COLON
   : ':'
   ;

NUMBER
   : ('+' | '-')? DIGIT+
   ;

QUOTED
   : WORD '\''
   ;

WORD
   : LETTER (LETTER | DIGIT | '_')*
   ;

OP
   : '+'
   | '-'
   | '*'
   | '/'
   | '='
   | '!'
   | '>'
   | '<'
   ;

// line comments, beginning with '#' (this also covers assembler
// directives, which begin with '#!!' and are otherwise just comments
// as far as syntax is concerned)
LINE_COMMENT
   : '#' ~ [\r\n]* -> skip
   ;

// nestable block comments, delimited by '{' and '}'
COMMENT_START
   : '{' -> skip, pushMode(COMMENT)
   ;

WS
   : [ \t\r\n,;]+ -> skip
   ;

fragment DIGIT
   : [0-9]
   ;

fragment LETTER
   : [a-zA-Z]
   ;

mode COMMENT;

COMMENT_NESTED_START
   : '{' -> skip, pushMode(COMMENT)
   ;

COMMENT_END
   : '}' -> skip, popMode
   ;

COMMENT_TEXT
   : ~ [{}]+ -> skip
   ;
