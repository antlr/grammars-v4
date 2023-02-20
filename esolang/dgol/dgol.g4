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
grammar dgol;

module
   : (usedeclaration | NL)* (subroutinedefinition | NL)* (programdefinition | librarydefinition) NL* EOF
   ;

usedeclaration
   : 'USE' IDENTIFER NL
   ;

subroutinedefinition
   : 'SUBROUTINE' IDENTIFER '(' (IDENTIFER (',' IDENTIFER)*)? ')' NL statements 'END' IDENTIFER NL
   ;

programdefinition
   : 'PROGRAM' IDENTIFER NL statements 'END' IDENTIFER NL
   ;

librarydefinition
   : 'LIBRARY' IDENTIFER (subroutinedeclaration | NL)* 'END' IDENTIFER NL
   ;

subroutinedeclaration
   : 'SUBROUTINE' IDENTIFER NL
   ;

statements
   : (statement? NL)*
   ;

statement
   : letstatement
   | ifstatement
   | dostatement
   | callstatement
   | returnstatement
   | exitstatement
   ;

identifierorzero
   : IDENTIFER
   | '0'
   ;

letstatement
   : 'LET' IDENTIFER ('=' identifierorzero | '<' IDENTIFER | '>' identifierorzero)
   ;

ifstatement
   : ifhead ('ELSE' ifhead)* ('ELSE' NL statements)? 'END' 'IF'
   ;

ifhead
   : 'IF' IDENTIFER ('=' | '>') IDENTIFER NL statements
   ;

dostatement
   : 'DO' IDENTIFER ('<' IDENTIFER)? NL statements 'END' 'DO'
   ;

callstatement
   : 'CALL' IDENTIFER ('.' IDENTIFER)? '(' (identifierorzero (',' identifierorzero)*)? ')'
   ;

returnstatement
   : 'RETURN'
   ;

exitstatement
   : 'EXIT' IDENTIFER
   ;

IDENTIFER
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

NL
   : [\r\n]+
   ;

WS
   : [ \t\r\n] -> skip
   ;

