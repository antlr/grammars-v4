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
grammar terraform;

file
   : (local | module | output | provider | variable | data | resource | terraform)+
   ;

terraform
   : 'terraform' blockbody
   ;

resource
   : 'resource' resourcetype name blockbody
   ;

data
   : 'data' resourcetype name blockbody
   ;

provider
  : 'provider' resourcetype blockbody
  ;

output
  : 'output' name blockbody
  ;

local
  : 'locals' blockbody
  ;

module
  : 'module' name blockbody
  ;

variable
   : 'variable' name blockbody
   ;

block
   : blocktype label* blockbody
   ;

blocktype
   : IDENTIFIER
   ;

resourcetype
   : STRING
   ;

name
   : STRING
   ;

label
   : STRING
   ;

blockbody
   : '{' (argument | block)* '}'
   ;

argument
   : identifier '=' expression
   ;

identifier
   : IDENTIFIER
   ;

expression
   : RESOURCEREFERENCE
   | section
   | expression OPERATOR expression
   | LPAREN expression RPAREN
   | expression '?' expression ':' expression
   ;

RESOURCEREFERENCE
   : [a-zA-Z] [a-zA-Z0-9_-]* RESOURCEINDEX? '.' ([a-zA-Z0-9_.-] RESOURCEINDEX?)+ 
   ;

RESOURCEINDEX
   : '[' [0-9]+ ']'
   ;

section
   : list
   | map
   | val
   ;

val
   : NULL
   | SIGNED_NUMBER
   | string
   | BOOL
   | IDENTIFIER index?
   | DESCRIPTION
   | filedecl
   | functioncall
   | EOF_
   ;

functioncall
   : functionname LPAREN functionarguments RPAREN
   ;

functionname
   : IDENTIFIER
   ;

functionarguments
   : //no arguments
   | expression (',' expression)*
   ;

index
   : '[' expression ']'
   ;

filedecl
   : 'file' '(' expression ')'
   ;

list
   : '[' expression (',' expression)* ','? ']'
   ;

map
   : '{' argument* '}'
   ;

string
   : STRING
   | MULTILINESTRING
   ;

fragment DIGIT
   : [0-9]
   ;

SIGNED_NUMBER
   : '+' NUMBER
   | '-' NUMBER
   | NUMBER
   ;

OPERATOR
   : '*'
   | '/'
   | '%'
   | '+'
   | '-'
   | '>'
   | '>='
   | '<'
   | '<='
   | '=='
   | '!='
   | '&&'
   | '||'
   ;

LPAREN
   : '('
   ;

RPAREN
   : ')'
   ;

EOF_
   : '<<EOF' .*? 'EOF'
   ;

NULL
   : 'nul'
   ;

NUMBER
   : DIGIT+ ('.' DIGIT+)?
   ;

BOOL
   : 'true'
   | 'false'
   ;

DESCRIPTION
   : '<<DESCRIPTION' .*? 'DESCRIPTION'
   ;

MULTILINESTRING
   : '<<-EOF' .*? 'EOF'
   ;

STRING
   : '"' (~ [\r\n"] | '""' | '\\"')* '"'
   ;

IDENTIFIER
   : [a-zA-Z] ([a-zA-Z0-9_-])*
   ;

COMMENT
  : ('#' | '//') ~ [\r\n]* -> channel(HIDDEN)
  ;

BLOCKCOMMENT
  : '/*' .*? '*/' -> channel(HIDDEN)
  ;

WS
   : [ \r\n\t]+ -> skip
   ;
