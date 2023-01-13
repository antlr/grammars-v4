/*
BSD License

Copyright (c) 2018, Tom Everett
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

grammar pl0;

program
   : block '.' EOF
   ;

block
   : consts? vars_? procedure* statement
   ;

consts
   : CONST ident '=' number (',' ident '=' number)* ';'
   ;

vars_
   : VAR ident (',' ident)* ';'
   ;

procedure
   : PROCEDURE ident ';' block ';'
   ;

statement
   : (assignstmt | callstmt | writestmt | qstmt | bangstmt | beginstmt | ifstmt | whilestmt)?
   ;

assignstmt
   : ident ':=' expression
   ;

callstmt
   : CALL ident
   ;

writestmt
   : WRITE ident
   ;

qstmt
   : '?' ident
   ;

bangstmt
   : '!' expression
   ;

beginstmt
   : BEGIN statement (';' statement)* END
   ;

ifstmt
   : IF condition THEN statement
   ;

whilestmt
   : WHILE condition DO statement
   ;

condition
   : ODD expression
   | expression ('=' | '#' | '<' | '<=' | '>' | '>=') expression
   ;

expression
   : ('+' | '-')? term (('+' | '-') term)*
   ;

term
   : factor (('*' | '/') factor)*
   ;

factor
   : ident
   | number
   | '(' expression ')'
   ;

ident
   : STRING
   ;

number
   : NUMBER
   ;


WRITE
   : W R I T E
   ;


WHILE
   : W H I L E
   ;


DO
   : D O
   ;


IF
   : I F
   ;


THEN
   : T H E N
   ;


ODD
   : O D D
   ;


BEGIN
   : B E G I N
   ;


END
   : E N D
   ;


CALL
   : C A L L
   ;


CONST
   : C O N S T
   ;


VAR
   : V A R
   ;


PROCEDURE
   : P R O C E D U R E
   ;


fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


STRING
   : [a-zA-Z] [a-zA-Z]*
   ;


NUMBER
   : [0-9] +
   ;


WS
   : [ \t\r\n] -> skip
   ;
