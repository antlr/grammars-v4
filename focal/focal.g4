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
grammar focal;

prog
   : statement+ EOF
   ;

statement
   : linenum command
   ;

grpnum
   : INTEGER
   ;

linenum
   : grpnum '.' INTEGER
   ;

command
   : ask
   | do_
   | for_
   | set_
   | quit
   | goto_
   | if_
   | type_
   | return_
   | write_
   | comment
   ;

ask
   : ('ASK' | 'A') askpair (',' askpair)*
   ;

askpair
   : STRING_LITERAL ',' VARIABLE (',' VARIABLE)*
   ;

do_
   : ('DO' | 'D') ('all' | grpnum | linenum)
   ;

for_
   : ('FOR' | 'F') VARIABLE '=' expression ',' expression (',' expression)? ';' command
   ;

quit
   : 'QUIT'
   ;

set_
   : ('SET' | 'S') VARIABLE '=' expression (';' command)?
   ;

goto_
   : ('GOTO' | 'G') linenum?
   ;

if_
   : ('IF') expression linenum (',' linenum) (',' linenum) (';' command)?
   ;

type_
   : ('TYPE' | 'T') typeexpression (',' typeexpression)* (';' command)?
   ;

typeexpression
   : expression
   | '!'+
   | '#'+
   | STRING_LITERAL
   | ('%' INTEGER '.' INTEGER)
   ;

return_
   : 'RETURN'
   ;

write_
   : 'WRITE' (grpnum | linenum)?
   ;

comment
   : COMMENT
   ;

expression
   : primary (PLUSMIN primary)*
   ;

primary
   : term (MULOP term)*
   ;

term
   : ('(' expression ')')
   | ('[' expression ']')
   | ('<' expression '>')
   | number
   | VARIABLE
   | (VARIABLE '(' expression ')')
   | (BUILTIN '(' expression ')')
   ;

number
   : mantissa ('e' signed_)?
   ;

mantissa
   : signed_
   | (signed_ '.')
   | ('.' signed_)
   | (signed_ '.' signed_)
   ;

signed_
   : PLUSMIN? INTEGER
   ;

PLUSMIN
   : '+'
   | '-'
   ;

MULOP
   : '*'
   | '/'
   | '^'
   ;

VARIABLE
   : ALPHA (ALPHA | DIGIT)*
   ;

INTEGER
   : DIGIT+
   ;

ALPHA
   : [A-Za-z]
   ;

DIGIT
   : [0-9]
   ;

BUILTIN
   : 'fsin'
   | 'fcos'
   | 'fexp'
   | 'flog'
   | 'fatn'
   | 'fsqt'
   | 'fabs'
   | 'fsgn'
   | 'fitr'
   | 'fran'
   ;

STRING_LITERAL
   : '"' .*? '"'
   ;

COMMENT
   : 'COMMENT' ~ [\r\n]*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

