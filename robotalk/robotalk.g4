/*
BSD License
Copyright (c) 2020, Tom Everett
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
grammar robotalk;

program
   :  command* method*
   ;

command
   : action
   | repeat_statement
   | while_statement
   | if_statement
   | return_statement
   | assignment_statement
   | method_call eol
   ;

action
   : move_action
   | turn_action
   | take_action
   | drop_action
   | zap_action
   ;

move_action
   : 'move' expression eol
   ;

turn_action
   : 'turn' direction eol
   ;

take_action
   : 'take' thing eol
   ;

drop_action
   : 'drop' thing eol
   ;

zap_action
   : 'zap' eol
   ;

repeat_statement
   : 'repeat' expression block
   ;

while_statement
   : 'while' condition block
   ;

if_statement
   : 'if' condition block ('else' block)?
   ;

return_statement
   : 'return' expression eol
   ;

assignment_statement
   : variable '=' expression eol
   ;

method_call
   : variable '(' (expression (',' expression)*)? ')'
   ;

variable
   : NAME
   ;

direction
   : 'north'
   | 'east'
   | 'south'
   | 'west'
   | 'left'
   | 'right'
   | 'around'
   ;

thing
   : 'WALL'
   | 'ENEMY'
   | 'FUEL'
   | 'ARMOR'
   | 'ZAP_GUN'
   ;

block
   : '{' eol command* '}' eol
   ;

comparison
   : expression comparator expression
   ;

condition
   : logical_condition
   | 'seeing' thing
   | 'smelling' thing
   | 'facing' thing
   ;

logical_condition
   : disjunct ('or' disjunct)*
   ;

disjunct
   : conjunct ('and' conjunct)*
   ;

conjunct
   : 'not'? logical_factor
   ;

logical_factor
   : comparison
   ;

comparator
   : '<'
   | '='
   | '='
   | '!=' '='
   | '>'
   ;

method
   : 'define' NAME variable* eol command* 'end' eol
   ;

eol
   : EOL*
   ;

expression
   : term (add_operator term)*
   ;

term
   : factor (multiply_operator factor)*
   ;

add_operator
   : '+'
   | '-'
   ;

multiply_operator
   : '*'
   | '/'
   | '%'
   ;

factor
   : variable
   | NUMBER
   | '(' expression ')'
   | 'distance'
   | method_call
   ;

NAME
   : LETTER (LETTER | DIGIT | '_')*
   ;

NUMBER
   : DIGIT DIGIT*
   ;

EOL
   : '\n'
   ;

DIGIT
   : [0-9]
   ;

LETTER
   : [a-zA-Z]
   ;

