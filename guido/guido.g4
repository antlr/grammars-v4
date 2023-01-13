/*
BSD License

Copyright (c) 2013, Tom Everett
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

grammar guido;

prog
   : ((segment +) | sequencelist) EOF
   ;

segment
   : '{' sequencelist + '}'
   ;

sequencelist
   : sequence (',' sequence)*
   ;

sequence
   : '[' (tag | note | chord) + ']'
   ;

tag
   : TAGSTART tagname parameters? notes?
   ;

tagname
   : title
   | tempo
   | clef
   | meter
   | slur
   | key
   | barformat
   | staff
   | t
   | repeatEnd
   ;

parameters
   : '<' parameter (',' parameter)* '>'
   ;

parameter
   : STRINGLITERAL
   | number
   | kvpair
   ;

kvpair
   : STRING '=' (STRING | NUMBER) +
   ;

notes
   : '(' (note | chord) + ')'
   ;

note
   : notename accidental? octave? duration? dotting?
   ;

chord
   : '{' note (',' note)* '}'
   ;

notename
   : STRING
   | REST
   ;

accidental
   : '#'
   | '&'
   ;

number
   : ('-' | '+')? NUMBER
   ;

octave
   : number
   ;

fraction
   : number? ('/' number)?
   ;

duration
   : '*'? fraction
   ;

dotting
   : '.' +
   ;

title
   : 'title'
   ;

tempo
   : 'tempo'
   ;

clef
   : 'clef'
   ;

meter
   : 'meter'
   ;

slur
   : 'slur'
   ;

key
   : 'key'
   ;

barformat
   : 'barFormat'
   ;

staff
   : 'staff'
   ;

repeatEnd
   : 'repeatEnd'
   ;

t
   : 't'
   ;


TAGSTART
   : '\\'
   ;


REST
   : '_'
   ;


NUMBER
   : [0-9] +
   ;


STRING
   : [a-zA-Z] +
   ;


STRINGLITERAL
   : '"' ~'"'* '"'
   ;


COMMENT
   : '%' ~ [\r\n]* -> skip
   ;


WS
   : [ \t\r\n] -> skip
   ;
