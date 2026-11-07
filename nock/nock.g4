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

// Grammar for Nock, the low-level combinator/VM language of Urbit.
// See https://developers.urbit.org/reference/nock/definition ("Nock 4K", nock.pdf).
//
// A noun is an atom (unsigned integer) or a cell (ordered pair of nouns),
// written bracketed and right-associative: [a b c] == [a [b c]].
// The dot-notation reduction operators (?, +, =, /, #, *) are each applied
// to a single noun argument, which is itself usually written as a cell.
// The increment operator is conventionally written ".+" to avoid ambiguity
// with a signed numeric literal.

grammar nock;

nock
   : noun EOF
   ;

noun
   : ATOM
   | '[' noun noun+ ']'
   | WUT noun
   | DOTLUS noun
   | TIS noun
   | FAS noun
   | HAX noun
   | TAR noun
   ;

WUT
   : '?'
   ;

DOTLUS
   : '.+'
   ;

TIS
   : '='
   ;

FAS
   : '/'
   ;

HAX
   : '#'
   ;

TAR
   : '*'
   ;

ATOM
   : [0-9]+
   ;

WS
   : [ \t\r\n]+ -> skip
   ;
