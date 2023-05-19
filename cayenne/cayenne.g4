/*
 BSD License

 Copyright (c) 2021, Tom Everett All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

// http://citeseer.ist.psu.edu/viewdoc/download;jsessionid=F54E0B46B27FC0AEF07271B358CE34E3?doi=10.1.1.47.155&rep=rep1&type=pdf

grammar cayenne;

file_: expr EOF ;

expr:
	'(' varid '::' type_ ')' '->' expr
	| '\\' '(' varid '::' type_ ')' '->' expr
	| expr expr
	| 'data' (conid type_* '|')*
	| conid '@' type_
	| 'case' varid 'of' arm* '::' type_
	| 'sig' sign*
	| 'struct' defn*
	| expr '.' lblid
	| id_
	| '#';

arm: '(' conid varid* ')' '->' expr ';' | varid '->' expr ';';

sign: lblid '::' type_ ';' | lblid '::' type_ '=' expr ';';

defn: vis lblid '::' type_ '=' expr ';';

vis: 'private' | 'public' abs_;

abs_: 'abstract' | 'concrete';

type_: expr;

varid: id_;

conid: id_;

lblid: id_;

id_: ID;

ID: [a-zA-Z] [a-zA-Z0-9]*;

WS: [ \r\n\t]+ -> skip;

