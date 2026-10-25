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
grammar zone;

options { caseInsensitive = true; }

zonefile
   : (entry? EOL)* entry? EOF
   ;

entry
   : HWS? directive HWS?
   | record HWS?
   | HWS
   ;

directive
   : ORIGIN HWS name
   | INCLUDE HWS (name | STRING) (HWS name)?
   | DEFAULT_TTL HWS TTL
   ;

record
   : name HWS rr
   | HWS rr
   ;

rr
   : (TTL HWS (CLASS HWS)? | CLASS HWS (TTL HWS)?)? record_type HWS record_data
   ;

record_type
   : RECORD_TYPE
   | GENERIC_TYPE
   ;

record_data
   : field (HWS field)*
   ;

field
   : name
   | STRING
   | group
   ;

group
   : '(' (name | STRING | HWS | EOL)* ')'
   ;

name
   : WORD
   | TTL
   | CLASS
   | RECORD_TYPE
   | GENERIC_TYPE
   ;

ORIGIN: '$ORIGIN';
INCLUDE: '$INCLUDE';
DEFAULT_TTL: '$TTL';
CLASS: 'IN' | 'CH' | 'HS';
RECORD_TYPE
   : 'A' | 'AAAA' | 'NS' | 'SOA' | 'MX' | 'CNAME' | 'PTR' | 'TXT'
   | 'SRV' | 'CAA' | 'DNAME' | 'NAPTR' | 'HINFO' | 'RP' | 'LOC'
   | 'DNSKEY' | 'DS' | 'RRSIG' | 'NSEC' | 'NSEC3' | 'NSEC3PARAM'
   | 'TLSA' | 'SSHFP' | 'SVCB' | 'HTTPS'
   ;
GENERIC_TYPE: 'TYPE' [0-9]+;
TTL: ([0-9]+ [wdhms]?)+;

STRING
   : '"' ('\\' ~[\r\n] | ~["\\\r\n])* '"'
   ;

WORD
   : ('\\' ~[\r\n] | ~[ \t\r\n();"\\$])+
   ;

COMMENT
   : ';' ~[\r\n]* -> skip
   ;

HWS
   : [ \t]+
   ;

EOL
   : '\r'? '\n'
   ;
