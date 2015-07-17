    /*
BSD License

Copyright (c) 2013, Tom Everett
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

grammar mumps;

program
    : routine+
    ;

routine
    : routinename routinebody
    ;

routinename
    : PERCENT? identifier (LPAREN paramlist? RPAREN)? (SPACE+ comment)? CR
    ;

paramlist
    : param (COMMA param)*
    ;

param
    : variable
    ;

routinebody
    : line+
    ;

line
    : SPACE+ (label SPACE)? command* (SPACE+ comment)? CR
    ;

label
    : identifier
    ;

command
    : set_
    | for_
    | write_
    | quit_
    | halt_
    | new_
    | break_
    | do_
    | (CLOSE | ELSE | GOTO | HANG | IF | JOB
    | KILL | LOCK | MERGE | OPEN | READ | TCOMMIT
    | TRESTART | TROLLBACK | TSTART | USE | VIEW | XECUTE)
    ;

arglist
    : arg (SPACE* COMMA arg)*
    ;

arg
    : expression
    | (BANG
    | STRING_LITERAL)
    ;

expression
    : term  (SPACE* (ADD | MULTIPLY | SUBTRACT | DIVIDE) expression)*
    ;

term
    : variable
    | NUMBER
    | LPAREN expression RPAREN
    ;

comment
    : SEMICOLON ~(CR)*
    ;

identifier
    : IDENTIFIER
    ;

variable
    : (CARAT | DOLLAR | AMPERSAND)* identifier
    ;

set_
    : (SET | 'S') SPACE+ assign (',' assign)*
    ;

assign
    : (LPAREN? arglist RPAREN?)? SPACE* EQUALS SPACE* arg
    ;

for_
    : FOR SPACE+ term EQUALS term COLON term SPACE* (command SPACE?)* COLON (term (LT | GT) term)
    ;

halt_
    : (HALT1 | HALT2)
    ;

write_
    : (WRITE1 | WRITE2) SPACE* arglist
    ;

quit_
    : (QUIT | 'Q')
    ;

new_
    : (NEW | 'N') SPACE* arglist
    ;

break_
    : (BREAK | 'B')
    ;

do_
    : (DO | 'D') SPACE* identifier (LPAREN paramlist? RPAREN)?
    ;

BREAK
    : B R E A K
    ;

CLOSE
    : C L O S E
    ;

DO
    : D O
    ;

ELSE
    : E L S E
    ;

FOR
    : F O R
    ;

GOTO
    : G O T O
    ;

HALT1
    : H A L T
    ;

HALT2
    : H
    ;

HANG
    : H A N G
    ;

IF
    : I F
    ;

JOB
    : J O B
    ;

KILL
    : K I L L
    ;

LOCK
    : L O C K
    ;

MERGE
    : M E R G E
    ;

NEW
    : N E W
    ;

OPEN
    : O P E N
    ;

QUIT
    : Q U I T
    ;

READ
    : R E A D
    ;

SET
    : S E T
    ;

TCOMMIT
    : T C O M M I T
    ;

TRESTART
    : T R E S T A R T
    ;

TROLLBACK
    : T R O L L B A C K
    ;

TSTART
    : T S T A R T
    ;

USE
    : U S E
    ;

VIEW
    : V I E W
    ;

WRITE1
    : W R I T E
    ;

WRITE2
    : W
    ;

XECUTE
    : X E C U T E
    ;

SEMICOLON
    : ';'
    ;

COLON
    : ':'
    ;

COMMA
    : ','
    ;

DOLLAR
    : '$'
    ;

PERCENT
    : '%'
    ;

AMPERSAND
    : '&'
    ;

INDIRECT
    : '@'
    ;

CARAT
    : '^'
    ;

BANG
    : '!'
    ;


LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

GT
    : '>'
    ;

LT
    : '<'
    ;

ADD
    : '+'
    ;

SUBTRACT
    : '-'
    ;

MULTIPLY
    : '*'
    ;

DIVIDE
    : '/'
    ;

EQUALS
    : '='
    ;

IDENTIFIER
    : ('a'..'z' | 'A'..'Z')  ('a'..'z'|'A'..'Z'|'0'..'9')*
    ;

STRING_LITERAL
    : '\"' ('\"\"' | ~('\"'))* '\"'
    ;

NUMBER
    : ('0'..'9')+ ('.' ('0'..'9')+)?
    ;

SPACE
    : ' '
    ;

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');

CR
    : '\n'
    ;

WS
    : [\t]->skip
    ;
