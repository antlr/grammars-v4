/*
BSD License

Copyright (c) 2017, Tim Hemel
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tim Hemel nor the names of its contributors
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
 * Graphstream DGS grammar.
 *
 * Adapted from http://graphstream-project.org/doc/Advanced-Concepts/The-DGS-File-Format/
 *
 */

lexer grammar DGSLexer;

MAGIC : 'DGS004' | 'DGS003';

AN : 'an';
CN : 'cn';
DN : 'dn';
AE : 'ae';
CE : 'ce';
DE : 'de';
CG : 'cg';
ST : 'st';
CL : 'cl';

INT : ('+'|'-')? ( '0' | ( [1-9] ([0-9])* ) );
REAL : INT ( '.' [0-9]*)? ( [Ee] ('+'|'-')? [0-9]*[1-9] )?;

PLUS : '+';
MINUS : '-';
COMMA : ',';
LBRACE : '{';
RBRACE : '}';
LBRACK : '[';
RBRACK : ']';
DOT : '.';
LANGLE : '<';
RANGLE : '>';
EQUALS : '=';
COLON : ':';

EOL : '\r'|'\n'|'\r\n';
WORD : ( 'a'..'z' | 'A'..'Z' ) ( 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' )* ;
STRING : SQSTRING | DQSTRING;
fragment DQSTRING : '"' (DQESC|.)*? '"';
fragment DQESC : '\\"' | '\\\\' ;
fragment SQSTRING : '\'' (SQESC|.)*? '\'';
fragment SQESC : '\\\'' | '\\\\' ;

fragment HEXBYTE : ([0-9a-fA-F]) ([0-9a-fA-F]) ;
COLOR : '#' HEXBYTE HEXBYTE HEXBYTE HEXBYTE? ;

START_COMMENT : '#' -> pushMode(CMT), skip;

WS : [ \t]+ -> skip;

mode CMT;

COMMENT: .*? EOL -> popMode;

