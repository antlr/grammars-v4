/*
BSD License

Copyright (c) 2019, Tom Everett
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
grammar sieve;

start
   : commands
   ;

commands
   : command
   ;

command
   : IDENTIFIER arguments (';' | block)
   ;

arguments
   : argument* (test | testlist)?
   ;

argument
   : stringlist
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
   | string
   ;

string
   : quotedstring multiline
   ;

block
   : '{' commands '}'
   ;

multiline
   : 'text:' ('  ' HTAB)* (HASHCOMMENT | CRLF) (multilineliteral | multilinedotstart)* '.' CRLF
   ;

multilineliteral
   : (OCTETNOTPERIOD OCTETNOTCRLF*)? CRLF
   ;

multilinedotstart
   : '.' OCTETNOTCRLF+ CRLF
   ;

quotedstring
   : DQUOTE quotedtext DQUOTE
   ;

quotedtext
   : (quotedsafe | quotedspecial | quotedother)*
   ;

quotedsafe
   : CRLF
   | OCTETNOTQSPECIAL
   ;

quotedspecial
   : '\\' (DQUOTE | '\\')
   ;

quotedother
   : '\\' OCTETNOTQSPECIAL
   ;

OCTETNOTCRLF
   : [0x01-0x09]
   | [0x0B-0x0C]
   | [0x0E-0xFF]
   ;

OCTETNOTPERIOD
   : [0x01-0x09]
   | [0x0B-0x0C]
   | [0x0E-0x2D]
   | [0x2F-0xFF]
   ;

OCTETNOTQSPECIAL
   : [0%x01-0x09]
   | [0x0B-0x0C]
   | [0x0E-0x21]
   | [0x23-0x5B]
   | [0x5D-oxFF]
   ;

ADDRESSPART
   : ':localpart'
   | ':domain'
   | ':all'
   ;

HASHCOMMENT
   : '#' octetnotcrlf* CRLF
   ;

comparator
   : ':comparator' string
   ;

MATCHTYPE
   : ':is'
   | ':contains'
   | ':matches'
   ;

DQUOTE
   : '"'
   ;

NUMBER
   : DIGIT+ QUANTIFIER?
   ;

TAG
   : ':' IDENTIFIER
   ;

STAR
   : '*'
   ;

QUANTIFIER
   : 'K'
   | 'M'
   | 'G'
   ;

IDENTIFIER
   : (ALPHA | '_')* (ALPHA | DIGIT | '_')
   ;

DIGIT
   : [0-9]
   ;

ALPHA
   : [A-Za-z]
   ;

CRLF
   : [\r\n]
   ;

WS
   : [ \t\r\n] -> skip
   ;

