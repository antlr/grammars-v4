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

/*
    https://www.ietf.org/rfc/rfc5228.txt
*/
grammar sieve;

start
   : commands EOF
   ;

commands
   : command+
   ;

command
   : IDENTIFIER arguments (';' | block)
   ;

arguments
   : argument* (test | testlist)?
   ;

argument
   : stringlist
   | string
   | NUMBER
   | TAG
   ;

testlist
   : '(' test (',' test)* ')'
   ;

test
   : IDENTIFIER arguments
   ;

stringlist
   : '[' string (',' string)* ']'
   ;

string
   : QUOTEDSTRING
   | multiline
   ;

block
   : '{' commands '}'
   ;

multiline
   : 'text:' (multilineliteral | multilinedotstart)* '.'
   ;

comparator
   : ':comparator' string
   ;

multilineliteral
   : OCTETNOTPERIOD OCTETNOTCRLF*
   ;

multilinedotstart
   : '.' OCTETNOTCRLF+
   ;

LINECOMMENT
   : '#' ~ [\r\n]* -> skip
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

QUOTEDSTRING
   : '"' OCTETNOTQSPECIAL+ '"'
   ;

DIGIT
   : [0-9]
   ;

ALPHA
   : [A-Za-z]
   ;

IDENTIFIER
   : (ALPHA | '_') (ALPHA | DIGIT | '_')*
   ;

TAG
   : ':' IDENTIFIER
   ;
   // a single octet other than NUL, CR, or LF

OCTETNOTCRLF
   : [\u0001-\u0009]
   | [\u000B-\u000C]
   | [\u000E-\u00FF]
   ;
   // a single octet other than NUL,CR, LF, or period

OCTETNOTPERIOD
   : [\u0001-\u0009]
   | [\u000B-\u000C]
   | [\u000E-\u002D]
   | [\u002F-\u00FF]
   ;
   // a single octet other than NUL, CR, LF, double-quote, or backslash

OCTETNOTQSPECIAL
   : [\u0001-\u0009]
   | [\u000B-\u000C]
   | [\u000E-\u0021]
   | [\u0023-\u005B]
   | [\u005D-\u00FF]
   ;
   // either a CRLF pair, OR a single octet other than NUL, CR, LF, or star

NOTSTAR
   : [\u0001-\u0009]
   | [\u000B-\u000C]
   | [\u000E-\u0029]
   | [\u002B-\u00FF]
   ;
   // either a CRLF pair, OR a single octet  other than NUL, CR, LF, star, or slash

ADDRESSPART
   : ':localpart'
   | ':domain'
   | ':all'
   ;

MATCHTYPE
   : ':is'
   | ':contains'
   | ':matches'
   ;

NUMBER
   : DIGIT+ QUANTIFIER?
   ;

STAR
   : '*'
   ;

QUANTIFIER
   : 'K'
   | 'M'
   | 'G'
   ;

