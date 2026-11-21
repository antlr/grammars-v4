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

// https://www.multicians.org/


// https://www.multicians.org/sitemap.html


//


// ALM (Assembly Language for Multics) source, as seen in real ALM listings


// published on multicians.org (e.g. pxss.alm, the Multics traffic


// controller): a sequence of lines, each an optional label followed by an


// optional statement (a machine instruction or a pseudo-op), both of which


// are syntactically just a keyword followed by a comma-separated operand


// list. Multics has hundreds of instruction mnemonics and pseudo-ops, so


// rather than enumerate them, any identifier may appear in that position;


// this grammar captures the (quite rich) operand/addressing syntax: based


// addressing (bp|offset), external references (seg$offset), arithmetic


// (a+b, a*b), the location counter (*), literal constants (=o..., =v...),


// and parenthesized operand groups (used by instructions such as mlr).

grammar alm;

file_
   : line_* EOF
   ;

line_
   : label statement?
   | statement
   ;

label
   : IDENTIFIER ':'
   ;
   // A statement is a machine instruction or a pseudo-op (name, entry, include,

   // link, equ, oct, dec, vfd, bss, segdef, segref, macro, even, ...); a

   // leading '&' introduces a macro-preprocessor directive such as &end.

statement
   : '&'? IDENTIFIER operandlist?
   ;

operandlist
   : operand (',' operand)*
   ;

operand
   : term ('|' term)*
   ;

term
   : ('+' | '-')? factor (('+' | '-') factor)*
   ;

factor
   : atom (('*' | '/') atom)*
   ;

atom
   : LITERALPREFIX? primary
   ;

primary
   : NUMBER
   | STRING
   | IDENTIFIER ('(' operandlist? ')')?
   | '*'
   | '(' operandlist? ')'
   ;
   // used by pseudo-ops such as aci (ASCII string data)

STRING
   : '\'' ~ ['\r\n]* '\''
   ;
   // A literal constant is introduced by '=' followed by a type letter, e.g.

   // =o400000 (octal), =v18/6,... (vfd-style field literal), =d12 (decimal).

LITERALPREFIX
   : '=' [a-zA-Z]?
   ;

IDENTIFIER
   : [a-zA-Z_] [a-zA-Z0-9_.$]*
   ;

NUMBER
   : [0-9]+
   ;

COMMENT
   : '"' ~ [\r\n]* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

