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
grammar snowball;

program
   : p* EOF
   ;

p
   : declaration
   | r_definition
   | g_definition
   | 'backwardmode' '(' p ')'
   ;

declaration
   : 'strings' '(' s_name* ')'
   | 'integers' '(' i_name* ')'
   | 'booleans' '(' b_name* ')'
   | 'routines' '(' r_name* ')'
   | 'externals' '(' r_name* ')'
   | 'groupings' '(' g_name* ')'
   ;

r_definition
   : 'define' r_name 'as' c
   ;

g_definition
   : 'define' g_name g (PLUS_OR_MINUS g)*
   ;

c
   : '(' c* ')'
   | i_command
   | s_command
   | c 'or' c
   | c 'and' c
   | 'not' c
   | 'test' c
   | 'try' c
   | 'do' c
   | 'fail' c
   | 'goto' c
   | 'gopast' c
   | 'repeat' c
   | 'loop' ae c
   | 'atleast' ae c
   | s
   | '=' s
   | 'insert' s
   | 'attach' s
   | '<-' s
   | 'delete'
   | 'hop' ae
   | 'next'
   | '=>' s_name
   | '['
   | ']'
   | '->' s_name
   | 'setmark' i_name
   | 'tomark' ae
   | 'atmark' ae
   | 'tolimit'
   | 'atlimit'
   | 'setlimit' c 'for' c
   | 'backwards' c
   | 'reverse' c
   | 'substring'
   | 'among' '(' (LITERAL_STRING r_name? | c)* ')'
   | 'set' b_name
   | 'unset' b_name
   | b_name
   | r_name
   | g_name
   | 'non' '-'? g_name
   | 'true'
   | 'false'
   | '?'
   ;

i_command
   : '$' i_name '=' ae
   | '$' i_name '+=' ae
   | '$' i_name '-=' ae
   | '$' i_name '*=' ae
   | '$' i_name '/=' ae
   | '$' i_name '==' ae
   | '$' i_name '!=' ae
   | '$' i_name '>' ae
   | '$' i_name '>=' ae
   | '$' i_name '<' ae
   | '$' i_name '<=' ae
   ;

s_command
   : '$' s_name c
   ;

s
   : s_name
   | LITERAL_STRING
   ;

g
   : g_name
   | LITERAL_STRING
   ;

s_name
   : NAME
   ;

i_name
   : NAME
   ;

b_name
   : NAME
   ;

r_name
   : NAME
   ;

g_name
   : NAME
   ;

ae
   : '(' ae ')'
   | ae '+' ae
   | ae '-' ae
   | ae '*' ae
   | ae '/' ae
   | '-' ae
   | 'maxint'
   | 'minint'
   | 'cursor'
   | 'limit'
   | 'size'
   | 'sizeof' s_name
   | i_name
   | NUMBER
   ;

LITERAL_STRING
   : '\'' ~ '\''* '\''
   ;

NUMBER
   : DIGIT+
   ;

PLUS_OR_MINUS
   : '+'
   | '-'
   ;

NAME
   : LETTER (LETTER | DIGIT | '_')*
   ;

fragment LETTER
   : [a-zA-Z]
   ;

fragment DIGIT
   : [0-9]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

