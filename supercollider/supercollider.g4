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
grammar supercollider;

file_
   : topLevelStatement* EOF
   ;

topLevelStatement
   : compound
   | statement
   ;
   // A top-level ( ... ) group, as commonly used to delimit a single
   
   // "chunk" of code evaluated by the sclang interpreter.
   
compound
   : '(' statement* ')' postfix* ';'?
   ;

statement
   : argdecls
   | assignment
   | expression ';'?
   ;

argdecls
   : (argdecl ';')+
   ;

argdecl
   : 'var' instancename (',' instancename)* ('=' expression)?
   ;

assignment
   : instancename '=' expression ';'?
   ;

expression
   : primary postfix* # postfixExpr
   | '*' expression # splatExpr
   | '-' expression # negExpr
   | expression ('*' | '/' | '+' | '-' | '!' | '<>' | '<=' | '>=' | '==' | '!=' | '<' | '>') expression # binaryExpr
   | expression '..' expression # rangeExpr
   ;

postfix
   : '.' methodname arguments? block*
   | '.' '(' expressionlist? ')' block*
   | arguments block*
   | block+
   | '[' expression ']'
   ;

primary
   : block
   | array
   | literal
   | symbol
   | instancename
   | classname
   | '(' expression ')'
   ;

block
   : '{' blockargs? statement* '}'
   ;

blockargs
   : '|' blockarg (',' blockarg)* '|'
   ;

blockarg
   : instancename ('=' expression)?
   ;

arguments
   : '(' expressionlist? ')'
   ;

methodname
   : VARNAMELITERAL
   ;

classname
   : CLASSNAMELITERAL
   ;

instancename
   : VARNAMELITERAL
   ;

symbol
   : SYMBOL
   ;

expressionlist
   : callarg (',' callarg)*
   ;

callarg
   : keywordarg
   | expression
   ;

keywordarg
   : VARNAMELITERAL ':' expression
   ;

array
   : '[' (arrayelement (',' arrayelement)* ','?)? ']'
   ;

arrayelement
   : keywordarg
   | expression
   ;

literal
   : number
   | STRING1
   | STRING2
   | TRUE
   | FALSE
   | NIL
   ;

number
   : (DECIMAL | HEX | FLOAT)
   ;

TRUE
   : 'true'
   ;

FALSE
   : 'false'
   ;

NIL
   : 'nil'
   ;

fragment HEXDIGIT
   : [a-f]
   ;

fragment DECDIGIT
   : [0-9]
   ;

FLOAT
   : DECDIGIT+ ('.' DECDIGIT+)? ('e' DECDIGIT+)? 'pi'?
   ;

HEX
   : '0x' (HEXDIGIT | DECDIGIT)+
   ;

DECIMAL
   : DECDIGIT+ 'pi'?
   ;

VARNAMELITERAL
   : [a-z] [a-zA-Z0-9]*
   ;

CLASSNAMELITERAL
   : [A-Z] [a-zA-Z0-9]*
   ;

SYMBOL
   : '\\' [a-zA-Z_] [a-zA-Z0-9_]*
   ;

STRING1
   : '"' ('\\' . | ~ ["\\])* '"'
   ;

STRING2
   : '\'' ('\\' . | ~ ['\\])* '\''
   ;

COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

BLOCKCOMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

