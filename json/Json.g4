/*
BSD License

Copyright (c) 2013, Rainer Schuster
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Rainer Schuster nor the names of its contributors
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

JSON grammar derived from:

    http://tools.ietf.org/html/rfc7159

    The JavaScript Object Notation (JSON) Data Interchange Format
    March 2014

Terminal rules mainly created by ANTLRWorks 1.5 sample code.
 */
grammar Json;

jsonText
	: jsonValue
;

jsonValue
	: 'false'
	| 'null'
	| 'true'
	| jsonObject
	| jsonArray
	| jsonNumber
	| jsonString
;

jsonNumber
	: NUMBER
;

jsonString
	: STRING
;

jsonObject
	: '{' (member (',' member)*)? '}'
;

member
	: STRING ':' jsonValue
;

jsonArray
	: '[' (jsonValue (',' jsonValue)*)? ']'
;

fragment
INT
	: '0'..'9'+
;

NUMBER
	: '-'? ('0' | ( '1'..'9' INT* )) ('.' INT+)? EXPONENT?
;

WS
	: ( ' '
	| '\t'
	| '\n'
	| '\r'
	) -> channel(HIDDEN)
;

STRING
	: '"' ( ESC_SEQ | ~('\\'|'"') )* '"'
;

fragment
EXPONENT
	: ('e'|'E') ('+'|'-')? ('0'..'9')+
;

fragment
HEX_DIGIT
	: ('0'..'9'|'a'..'f'|'A'..'F')
;

fragment
ESC_SEQ
	: '\\' ('\"'|'\\'|'/'|'b'|'f'|'n'|'r'|'t')
	| UNICODE_ESC
;

fragment
UNICODE_ESC
	: '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
;
