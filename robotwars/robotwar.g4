/*
 [The "BSD licence"]
 Copyright (c) 2013 Tom Everett
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

grammar robotwar;

program
   : line +
   ;

line
   : (label | comment | statement)? EOL
   ;

label
   : ID
   ;
   
statement
   : ifstatement
   | tostatement
   | gosubstatement
   | gotostatement
   | endsubstatement
   | accumstatement
   ;

accumstatement
    : accumexpression;

accumexpression
    : ('='
    | '#'
    | ('<' expression)
    | ('>' expression)) statement
    ;

gosubstatement
   : 'GOSUB' label
   ;

gotostatement
   : 'GOTO' label
   ;

tostatement
   : expression? ('TO' register) +
   ;

endsubstatement
   : 'ENDSUB'
   ;

ifstatement
   : 'IF'? condition (EOL | COMMA | DOT)? statement
   ;

condition
   : expression comparison expression
   ;

expression
   : (argument (operation argument)*)
   | (operation argument)
   | argument
   ;

operation
   : '+'
   | '-'
   | '*'
   | '/'
   ;

comparison
   : '<'
   | '>'
   | '='
   | '#'
   ;

argument
   : number
   | register
   | DATA
   ;

register
   : A
   | B
   | C
   | D
   | E
   | F
   | G
   | H
   | I
   | J
   | K
   | L
   | M
   | N
   | O
   | P
   | Q
   | R
   | S
   | T
   | U
   | V
   | W
   | X
   | Y
   | Z
   | AIM
   | SHOT
   | RADAR
   | SPEEDX
   | SPEEDY
   | RANDOM
   | INDEX
   | DATA
   | DAMAGE
   ;


A
   : 'A'
   ;


B
   : 'B'
   ;


C
   : 'C'
   ;


D
   : 'D'
   ;


E
   : 'E'
   ;


F
   : 'F'
   ;


G
   : 'G'
   ;


H
   : 'H'
   ;


I
   : 'I'
   ;


J
   : 'J'
   ;


K
   : 'K'
   ;


L
   : 'L'
   ;


M
   : 'M'
   ;


N
   : 'N'
   ;


O
   : 'O'
   ;


P
   : 'P'
   ;


Q
   : 'Q'
   ;


R
   : 'R'
   ;


S
   : 'S'
   ;


T
   : 'T'
   ;


U
   : 'U'
   ;


V
   : 'V'
   ;


W
   : 'W'
   ;


X
   : 'X'
   ;


Y
   : 'Y'
   ;


Z
   : 'Z'
   ;


AIM
   : 'AIM'
   ;


SHOT
   : 'SHOT'
   ;


RADAR
   : 'RADAR'
   ;


DAMAGE
   : 'DAMAGE'
   ;


SPEEDX
   : 'SPEEDX'
   ;


SPEEDY
   : 'SPEEDY'
   ;


RANDOM
   : 'RANDOM'
   ;


INDEX
   : 'INDEX'
   ;


DATA
   : 'DATA'
   ;


DOT
   : '.'
   ;


COMMA
   : ','
   ;


ID
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9') +
   ;

number
   : ('+' | '-')? NUMBER
   ;

comment
   : COMMENT
   ;


NUMBER
   : [0-9] +
   ;


COMMENT
   : ';' ~ [\r\n]*
   ;


EOL
   : [\r\n] +
   ;


WS
   : [ \t] -> skip
   ;
