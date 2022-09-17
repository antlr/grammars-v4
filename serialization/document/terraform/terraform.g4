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

file_
   : (local | module | output | provider | variable | data | resource | terraform)+ EOF
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
  : PROVIDER resourcetype blockbody
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
   : VARIABLE name blockbody
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
   : LCURL (argument | block)* RCURL
   ;

argument
   : identifier '=' expression
   ;

identifier
   : (('local' | 'data' | 'var' | 'module') DOT)? identifierchain
   ;

identifierchain
   : (IDENTIFIER | IN | VARIABLE | PROVIDER) index? (DOT identifierchain)*
   | STAR (DOT identifierchain)*
   | inline_index (DOT identifierchain)*
   ;

inline_index
   : NATURAL_NUMBER
   ;

expression
   : section
   | expression operator_ expression
   | LPAREN expression RPAREN
   | expression '?' expression ':' expression
   | forloop
   ;

forloop
   : 'for' identifier IN expression ':' expression
   ;

section
   : list_
   | map_
   | val
   ;

val
   : NULL_
   | signed_number
   | string
   | identifier
   | BOOL
   | DESCRIPTION
   | filedecl
   | functioncall
   | EOF_
   ;

functioncall
   : functionname LPAREN functionarguments RPAREN
   | 'jsonencode' LPAREN (.)*? RPAREN
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

list_
   : '[' (expression (',' expression)* ','?)? ']'
   ;

map_
   : LCURL (argument ','?)* RCURL
   ;

string
   : STRING
   | MULTILINESTRING
   ;

fragment DIGIT
   : [0-9]
   ;

signed_number
   : ('+' | '-')? number
   ;

VARIABLE
   : 'variable'
   ;

PROVIDER
   : 'provider'
   ;

IN
   : 'in'
   ;

STAR
   : '*'
   ;

DOT
   : '.'
   ;

operator_
   : '/'
   | STAR
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

LCURL
   : '{'
   ;

RCURL
   : '}'
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

NULL_
   : 'nul'
   ;

NATURAL_NUMBER
   : DIGIT+
   ;

number
   : NATURAL_NUMBER (DOT NATURAL_NUMBER)?
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
   : '"' ( '\\"' | ~["\r\n] )* '"'
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
