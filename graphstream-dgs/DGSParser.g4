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

parser grammar DGSParser;

options { tokenVocab=DGSLexer; }

dgs : header ( event | COMMENT | EOL )* EOF ;
header : MAGIC EOL identifier INT INT EOL;
event : ( an | cn | dn | ae | ce | de | cg | st | cl ) ( COMMENT | EOL ) ;

an : AN identifier attributes;
cn : CN identifier attributes;
dn : DN identifier;
ae : AE identifier identifier direction? identifier attributes;
ce : CE identifier attributes;
de : DE identifier;
cg : CG attributes;
st : ST REAL;
cl : CL;


attributes : attribute*;
attribute : (PLUS|MINUS)? identifier ( assign value ( COMMA value )* )? ;

value : STRING | INT| REAL | COLOR | array_ | a_map | identifier;

array_ : LBRACE ( value ( COMMA value )* )? RBRACE;
a_map : LBRACK ( mapping ( COMMA mapping )* )? RBRACK;
mapping : identifier assign value;
direction : LANGLE | RANGLE ;
assign : EQUALS | COLON ;
identifier : STRING | INT | WORD ( DOT WORD )* ;

