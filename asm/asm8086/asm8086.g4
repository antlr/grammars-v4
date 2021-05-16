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

grammar asm8086;

prog
   : (line ('!' line)* EOL)*
   ;

line
   : lbl? (assemblerdirective | instruction)? comment?
   ;

instruction
   : rep? opcode expressionlist?
   ;

lbl
   : label ':'?
   ;

assemblerdirective
   : org
   | end
   | if_
   | endif_
   | equ
   | db
   | dw
   | cseg
   | dd
   | dseg
   | title
   | include_
   | rw
   | rb
   | rs
   | '.'
   ;

rw
   : name? RW expression
   ;

rb
   : name? RB expression
   ;

rs
   : name? RS expression
   ;

cseg
   : CSEG expression?
   ;

dseg
   : DSEG expression?
   ;

dw
   : DW expressionlist
   ;

db
   : DB expressionlist
   ;

dd
   : DD expressionlist
   ;

equ
   : name EQU expression
   ;

if_
   : IF assemblerexpression
   ;

assemblerexpression
   : assemblerterm (assemblerlogical assemblerterm)*
   | '(' assemblerexpression ')'
   ;

assemblerlogical
   : 'eq'
   | 'ne'
   ;

assemblerterm
   : name
   | number
   | (NOT assemblerterm)
   ;

endif_
   : ENDIF
   ;

end
   : END
   ;

org
   : ORG expression
   ;

title
   : TITLE string_
   ;

include_
   : INCLUDE name
   ;

expressionlist
   : expression (',' expression)*
   ;

label
   : name
   ;

expression
   : multiplyingExpression (SIGN multiplyingExpression)*
   ;

multiplyingExpression
   : argument (('*' | '/' | 'mod' | 'and') argument)*
   ;

argument
   : number
   | dollar
   | register_
   | name
   | string_
   | ('(' expression ')')
   | ((number | name)? '[' expression ']')
   | ptr expression
   | NOT expression
   | OFFSET expression
   | LENGTH expression
   | (register_ ':') expression
   ;

ptr
   : (BYTE | WORD | DWORD)? PTR
   ;

dollar
   : DOLLAR
   ;

register_
   : REGISTER
   ;

string_
   : STRING
   ;

name
   : NAME
   ;

number
   : SIGN? NUMBER
   ;

opcode
   : OPCODE
   ;

rep
   : REP
   ;

comment
   : COMMENT
   ;


BYTE
   : B Y T E
   ;


WORD
   : W O R D
   ;


DWORD
   : D W O R D
   ;


DSEG
   : D S E G
   ;


CSEG
   : C S E G
   ;


INCLUDE
   : I N C L U D E
   ;


TITLE
   : T I T L E
   ;


END
   : E N D
   ;


ORG
   : O R G
   ;


ENDIF
   : E N D I F
   ;


IF
   : I F
   ;


EQU
   : E Q U
   ;


DW
   : D W
   ;


DB
   : D B
   ;


DD
   : D D
   ;


PTR
   : P T R
   ;


NOT
   : N O T
   ;


OFFSET
   : O F F S E T
   ;


RW
   : R W
   ;


RB
   : R B
   ;


RS
   : R S
   ;


LENGTH
   : L E N G T H
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;


REGISTER
   : A H | A L | B H | B L | C H | C L | D H | D L | A X | B X | C X | D X | C I | D I | B P | S P | I P | C S | D S | E S | S S
   ;


OPCODE
   : A A A | A A D | A A M | A A S | A D C | A D D | A N D | C A L L | C B W | C L C | C L D | C L I | C M C | C M P | C M P S B | C M P S W | C W D | D A A | D A S | D E C | D I V | E S C | H L T | I D I V | I M U L | I N | I N C | I N T | I N T O | I R E T | J A | J A E | J B | J B E | J C | J E | J G | J G E | J L | J L E | J N A | J N A E | J N B | J N B E | J N C | J N E | J N G | J N G E | J N L | J N L E | J N O | J N P | J N S | J N Z | J O | J P | J P E | J P O | J S | J Z | J C X Z | J M P | J M P S | J M P F | L A H F | L D S | L E A | L E S | L O C K | L O D S | L O D S B | L O D S W | L O O P | L O O P E | L O O P N E | L O O P N Z | L O O P Z | M O V | M O V S | M O V S B | M O V S W | M U L | N E G | N O P | N O T | O R | O U T | P O P | P O P F | P U S H | P U S H F | R C L | R C R | R E T | R E T N | R E T F | R O L | R O R | S A H F | S A L | S A R | S A L C | S B B | S C A S B | S C A S W | S H L | S H R | S T C | S T D | S T I | S T O S B | S T O S W | S U B | T E S T | W A I T | X C H G | X L A T | X O R
   ;


REP
   : R E P | R E P E | R E P N E | R E P N Z | R E P Z
   ;


DOLLAR
   : '$'
   ;


SIGN
   : '+' | '-'
   ;


NAME
   : [.a-zA-Z] [a-zA-Z0-9."_]*
   ;


NUMBER
   : [0-9a-fA-F] + ('H' | 'h')?
   ;


STRING
   : '\u0027' ~'\u0027'* '\u0027'
   ;


EOL
   : [\r\n] +
   ;


WS
   : [ \t] -> skip
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
