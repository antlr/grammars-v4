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

grammar morsecode;

morsecode
   : letter (SPACE letter)+
   ;

letter
   : a
   | b
   | c
   | d
   | e
   | f
   | g
   | h
   | i
   | j
   | k
   | l
   | m
   | n
   | o
   | p
   | q
   | r
   | s
   | t
   | u
   | v
   | w
   | x
   | y
   | z
   | one
   | two
   | three
   | four
   | five
   | six
   | seven
   | eight
   | nine
   | zero
   ;

a
   : DOT DASH
   ;

b
   : DASH DOT DOT DOT
   ;

c
   : DASH DOT DASH DOT
   ;

d
   : DASH DOT DOT
   ;

e
   : DOT
   ;

f
   : DOT DOT DASH DOT
   ;

g
   : DASH DASH DOT
   ;

h
   : DOT DOT DOT DOT
   ;

i
   : DOT DOT
   ;

j
   : DOT DASH DASH DASH
   ;

k
   : DASH DOT DASH
   ;

l
   : DOT DASH DOT DOT
   ;

m
   : DASH DASH
   ;

n
   : DASH DOT
   ;

o
   : DASH DASH DASH
   ;

p
   : DOT DASH DASH DOT
   ;

q
   : DASH DASH DOT DASH
   ;

r
   : DOT DASH DOT
   ;

s
   : DOT DOT DOT
   ;

t
   : DASH
   ;

u
   : DOT DOT DASH
   ;

v
   : DOT DOT DOT DASH
   ;

w
   : DOT DASH DASH
   ;

x
   : DASH DOT DOT DASH
   ;

y
   : DASH DOT DASH DASH
   ;

z
   : DASH DASH DOT DOT
   ;

one
   : DOT DASH DASH DASH DASH
   ;

two
   : DOT DOT DASH DASH DASH
   ;

three
   : DOT DOT DOT DASH DASH
   ;

four
   : DOT DOT DOT DOT DASH
   ;

five
   : DOT DOT DOT DOT DOT
   ;

six
   : DASH DOT DOT DOT DOT
   ;

seven
   : DASH DASH DOT DOT DOT
   ;

eight
   : DASH DASH DASH DOT DOT
   ;

nine
   : DASH DASH DASH DASH DOT
   ;

zero
   : DASH DASH DASH DASH DASH
   ;


DOT
   : '.'
   ;


DASH
   : '-'
   ;

SPACE
    : ' '
    ;

WS
   : [\t\r\n] -> skip
   ;
