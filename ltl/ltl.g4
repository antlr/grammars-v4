/*
BSD License

Copyright (c) 2021, Tom Everett
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

// https://en.wikipedia.org/wiki/Mathematical_operators_and_symbols_in_Unicode


// http://www.eecs.qmul.ac.uk/~pm/SaR/2004ltl.pdf

grammar ltl;

file_ : proposition EOF ;

proposition
   : 'true'
   | 'false'
   | ATOMIC
   | '(' proposition ')'
   | proposition (LTL_AND | LTL_OR | LTL_RIGHTWARDS_SINGLE_ARROW) proposition
   | LTL_NOT proposition
   | (LTL_GLOBALLY | LTL_FINALLY | LTL_NEXT) proposition
   | proposition (LTL_UNTIL | LTL_WEAK | LTL_RELEASE) proposition
   ;

ATOMIC
   : [a-z]+
   ;

LTL_UNTIL
   : 'U'
   ;

LTL_GLOBALLY
   : 'G'
   ;

LTL_FINALLY
   : 'F'
   ;

LTL_NEXT
   : 'X'
   ;

LTL_WEAK
   : 'W'
   ;

LTL_RELEASE
   : 'R'
   ;

LTL_RIGHTWARDS_SINGLE_ARROW
   : '\u2192'
   ;

LTL_AND
   : '\u2227'
   ;

LTL_OR
   : '\u2228'
   ;

LTL_NOT
   : '\u2310'
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

