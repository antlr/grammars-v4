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

// https://www.doi.org


// https://tools.ietf.org/id/draft-paskin-doi-uri-04.txt

grammar doiurl;

doiuri
   : scheme ':' encodeddoi ('?' query)? ('#' fragment_)? EOF?
   ;

scheme
   : 'doi'
   ;

encodeddoi
   : prefix_ '/' suffix
   ;

prefix_
   : segment
   ;

suffix
   : segment ('/' segment)*
   ;

segment
   : PCHAR+
   ;

query
   : (PCHAR | '/' | '?')*
   ;

fragment_
   : (PCHAR | '/' | '?')*
   ;

PCHAR
   : UNRESERVED
   | ESCAPED
   | ';'
   | ':'
   | '@'
   | '&'
   | '='
   | '+'
   | '$'
   | ','
   ;

fragment UNRESERVED
   : ALPHA
   | DIGIT
   | MARK
   ;

fragment ESCAPED
   : '%' HEXDIG HEXDIG
   ;

fragment MARK
   : '-'
   | '_'
   | '.'
   | '!'
   | '~'
   | '*'
   | '\''
   | '('
   | ')'
   ;
   // https://tools.ietf.org/html/rfc2234

fragment ALPHA
   : [a-zA-Z]
   ;
   // https://tools.ietf.org/html/rfc2234

fragment DIGIT
   : [0-9]
   ;
   // https:tools.ietf.org/html/rfc2234

fragment HEXDIG
   : [0-9A-F]
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

