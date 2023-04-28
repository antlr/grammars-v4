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
grammar zone;

zonefile
   : orig include* record*
   ;

orig
   : '$ORIGIN' domainname
   ;

include
   : '$INCLUDE' filename domainname?
   ;

record
   : (domainname | '@')? rr
   ;

rr
   : rr1
   | rr2
   ;

rr1
   : ttl? record_class? record_type? record_data
   ;

rr2
   : record_class? ttl? record_type? record_data
   ;

ttl
   : integer
   ;

record_class
   : 'IN'
   ;

record_type
   : 'A'
   ;

filename
   : LETTER+
   ;

integer
   : DIGIT+
   ;

record_data
   : (LETTER | DIGIT | '.')+
   ;

domainname
   : subdomain
   ;

//subdomain
 //  : (subdomain '.')? label
 //  ;

label
   : letter (ldhstr? LETDIG)?
   ;

letter
   : LETTER
   ;

ldhstr
   : LETDIGHYP
   | (LETDIGHYP ldhstr)
   ;

LETDIGDOT
   : LETTER
   | DIGIT
   | '.'
   ;

LETDIGHYP
   : LETTER
   | DIGIT
   | '-'
   ;

LETDIG
   : LETTER
   | DIGIT
   ;

DIGIT
   : [0-9]
   ;

LETTER
   : [a-zA-Z]
   ;

COMMENT
   : ';' ~ [\r\n]*
   ;

WS
   : [ \t\r\n] -> skip
   ;

