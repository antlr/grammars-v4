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

grammar amigae;

program
   : opts globalpart localpart EOF
   ;

globalpart
   : (modulestat | defstat | objdecl | constdecl | raisedecl)*
   ;

localpart
   : (procdecl | constdecl)+
   ;

modulestat
   : 'MODULE' STRINGCONST (',' STRINGCONST)* EOL
   ;

defstat
   : 'DEF' vardecllist EOL
   ;

objdecl
   : 'OBJECT' IDENT ('OF' IDENT)? EOL (vardecllist EOL)+ 'ENDOBJECT' EOL
   ;

constdecl
   : 'CONST' (CONSTANT '=' constexp) (',' (CONSTANT '=' constexp))* EOL
   | 'ENUM' (CONSTANT | CONSTANT '=' constexp) (',' (CONSTANT | CONSTANT '=' constexp))* EOL
   | 'SET' CONSTANT (',' CONSTANT)* EOL
   ;

procdecl
   : 'EXPORT'? 'PROC' IDENT '(' argdecllist? ')' ('OF' IDENT)? 'HANDLE'? (('RETURN' | 'IS') explist? EOL | EOL defstat* stats ('EXCEPT' EOL stats)? 'ENDPROC' explist? EOL)
   ;

raisedecl
   : 'RAISE' (CONSTANT 'IF' BUILTIN '(' ')' compop NUM) (',' (CONSTANT 'IF' BUILTIN '(' ')' compop NUM))*
   ;

opts
   : ('OPT' setting (',' setting)*)*
   ;

vardecllist
   : vardecl (',' vardecl)*
   ;

vardecl
   : IDENT ('=' NUM)? (':' ('LONG' | 'REAL' | 'PTR' 'TO' ptrtype))?
   | IDENT ':' objtype
   | IDENT '[' NUM ']' ':' ('ARRAY' | 'ARRAY' 'OF' ptrtype | 'STRING' | 'LIST')
   ;

argdecllist
   : argdecl (',' argdecl)*
   ;

argdecl
   : IDENT ('=' defaultarg)? (':' ('LONG' | 'REAL' | 'PTR' 'TO' ptrtype))?
   ;

ptrtype
   : objtype
   | simpletype
   ;

simpletype
   : CHAR
   | INT
   | LONG
   ;

objtype
   : IDENT
   ;

stats
   : ((onelinestat | multlinestat) EOL)*
   ;

onelinestat
   : exp
   | lval ':=' exp
   | var (',' var)+ ':=' exp
   | 'IF' exp 'THEN' onelinestat ('ELSE' onelinestat)?
   | 'FOR' var ':=' exp 'TO' exp ('STEP' NUM)? 'DO' onelinestat
   | 'WHILE' exp 'DO' onelinestat
   | 'RETURN' explist?
   | 'JUMP' IDENT
   | ('INC' | 'DEC') var
   | asm_mnemonic (operand (',' operand)*)?
   | 'INCBIN' STRINGCONST
   | simpletype NUM (',' NUM)*
   | 'VOID' exp
   ;

multlinestat
   : 'IF' exp EOL stats (('ELSEIF' exp EOL stats)*)? ('ELSE' EOL stats)? 'ENDIF'
   | 'FOR' var ':=' exp 'TO' exp ('STEP' NUM)? EOL stats 'ENDFOR'
   | 'WHILE' exp EOL stats 'ENDWHILE'
   | 'REPEAT' EOL stats 'UNTIL' exp
   | 'SELECT' var EOL ('CASE' exp EOL stats)+ ('DEFAULT' EOL stats)? 'ENDSELECT'
   | 'LOOP' EOL stats 'ENDLOOP'
   ;

explist
   : exp (',' exp)*
   ;

exp
   : '-'? item (binop item)*
   | exp 'BUT' exp
   ;

item
   : NUM
   | FNUM
   | CONSTANT
   | lval
   | STRINGCONST
   | CHARCONST
   | 'SIZEOF' IDENT
   | 'IF' exp 'THEN' exp 'ELSE' exp
   | '[' explist ']' (':' ptrtype)?
   | (BUILTIN | IDENT) '(' explist ')'
   | var ':=' exp
   | '{' IDENT '}'
   | '`' exp
   |
   ;

binop
   : mathop
   | compop
   | logop
   ;

mathop
   : '+'
   | '-'
   | '*'
   | '/'
   ;

compop
   : '='
   | '<>'
   | '>'
   | '<'
   | '>='
   | '<='
   ;

logop
   : 'AND'
   | 'OR'
   ;

constexp
   : '-'? NUM (('+' | '-' | '*' | '/') NUM)*
   ;

lval
   : var ('[' exp? ']' | '.' IDENT)* ('++' | '--')?
   | '^' var ('++' | '--')?
   |
   ;

var
   : IDENT
   ;

defaultarg
   : NUM
   ;
   // wild guess

setting
   : CHARCONST
   ;

asm_mnemonic
   : CHARCONST
   ;

operand
   : CHARCONST
   ;

CONSTANT
   : [A-Z] ([A-Z] [A-Za-z0-9_]*)?
   ;

BUILTIN
   : [A-Z] [a-z] [A-Za-z0-9_]*
   ;

IDENT
   : [a-z] [a-zA-Z0-9_]*
   ;

NUM
   : [0-9]+
   | '$' [0-9A-Fa-f]+
   | '%' [01]+
   ;

FNUM
   : [0-9]* '.' [0-9]*
   ;

STRINGCONST
   : '\'' ~ '\''* '\''
   ;

CHARCONST
   : '"' ~ '"'* '"'
   ;

EOL
   : [\r\n]+
   ;

WS
   : [ \t]+ -> skip
   ;

