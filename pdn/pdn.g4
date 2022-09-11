/*
BSD License

Copyright (c) 2017, Tom Everett
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

grammar pdn;

game
   : tags moves EOF
   ;

tags
   : tag*
   ;

tag
   : '[' text string ']'
   ;

moves
   : move+ (result | '*')+
   ;

move
   : movenum movespec+
   ;

movespec
   : (MOVE1 | MOVE2) (result | '*')?
   ;

movenum
   : number '.'
   ;

result
   : '1/2-1/2'
   | '1-0'
   | '0-1'
   ;

text
   : TEXT
   ;

string
   : STRING
   ;

number
   : NUMBER
   ;


MOVE1
   : ('0' .. '9') + 'x' ('0' .. '9') +
   ;


MOVE2
   : ('0' .. '9') + '-' ('0' .. '9') +
   ;


NUMBER
   : ('0' .. '9') +
   ;


TEXT
   : [a-zA-Z] [a-zA-Z0-9_] +
   ;


STRING
   : '"' ('""' | ~ '"')* '"'
   ;


COMMENT
   : '{' .*? '}' -> skip
   ;


WS
   : [ \t\r\n] + -> skip
   ;
