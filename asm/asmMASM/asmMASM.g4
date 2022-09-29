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

grammar asmMASM;

prog
   : line* EOF
   ;

line
   : (lbl | endlbl)? (assemblerdirective | masmdirectives | instruction)? EOL
   ;

instruction
   : rep? opcode expressionlist?
   ;

lbl
   : label ':'?
   ;

endlbl
   : END name?
   ;

assemblerdirective
   : org
   | if_
   | endif_
   | equ
   | db
   | dw
   | dm
   | ds
   | include
   | includelib
   | invoke
   | option
   | put
   | assign
   | segment
   | endsegment
   | group
   | label_
   | assume
   | extern_
   | public_
   | type_ expressionlist+
   ;

masmdirectives
   : masmdirective+
   ;

masmdirective
   : MASMDIRECTIVE expressionlist?
   ;

assume
   : ASSUME register_ ':' name (',' register_ ':' name)*
   ;

label_
   : name LABEL type_
   ;

type_
   : BYTE
   | SBYTE
   | WORD
   | DWORD
   ;

group
   : name GROUP name (',' name)*
   ;

segment
   : name SEGMENT align?
   ;

endsegment
   : name SEGMENTEND
   ;

align
   : BYTE | WORD | DWORD | PARA | PAGE
   | ALIGN '(' number ')'
   ;

assign
   : name ASSIGN expression
   ;

put
   : PUT expressionlist
   ;

include
   : INCLUDE expressionlist
   ;

includelib
   : INCLUDELIB expressionlist
   ;

invoke
   : INVOKE expressionlist
   ;

option
   : OPTION expressionlist
   ;

ds
   : DS expressionlist
   ;

dw
   : DW expressionlist
   ;

db
   : DB expressionlist
   ;

dm
   : DM expressionlist
   ;

dup
   : number DUP expression
   ;

equ
   : EQU expression
   ;

extern_
   : EXTERN expression
   ;

public_
   : PUBLIC expression
   ;

if_
   : IF expression
   ;

endif_
   : ENDIF
   ;

org
   : ORG expression
   ;

expressionlist
   : expression (',' expression)*
   ;

label
   : name
   | gross
   ;

expression
   : multiplyingExpression (SIGN multiplyingExpression)*
   ;

multiplyingExpression
   : argument (('*' | '/') argument)*
   ;

argument
   : number
   | dollar
   | ques
   | register_
   | (name ':')? name
   | string
   | '(' expression ')'
   | '[' expression ']'
   | NOT expression
   | OFFSET expression
   | gross
   | dup
   ;

/*
 MASM allows opcode names such as "RET" to be label names and also allows assemlber directives such as "PUT" as names
*/
gross
   : opcode
   | grossrawassemblerdirective
   ;

grossrawassemblerdirective
   : PUT
   | IF
   | ENDIF
   | ORG
   | EQU
   ;

dollar
   : DOLLAR
   ;

ques
   : QUES
   ;

register_
   : REGISTER
   ;

string
   : STRING1
   | STRING2
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

ORG
   : O R G
   ;

END
   : E N D
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


DM
   : D M
   ;


DS
   : D S
   ;


INCLUDE
   : I N C L U D E
   ;

INCLUDELIB
   : I N C L U D E L I B
   ;

INVOKE
   : I N V O K E
   ;

OPTION
   : O P T I O N
   ;

PUT
   : P U T
   ;


NOT
   : N O T
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


ASSIGN
   : '='
   ;


OFFSET
   : O F F S E T
   ;


DOLLAR
   : '$'
   ;


QUES
   : '?'
   ;


SEGMENT
   : S E G M E N T
   ;


SEGMENTEND
   : E N D S
   ;


GROUP
   : G R O U P
   ;


BYTE
   : B Y T E
   ;

SBYTE
   : S B Y T E
   ;

WORD
   : W O R D
   ;


DWORD
   : D W O R D
   ;


PARA
   : P A R A
   ;


PAGE
   : P A G E
   ;


ALIGN
   : A L I G N
   ;


LABEL
   : L A B E L
   ;


DUP
   : D U P
   ;


ASSUME
   : A S S U M E
   ;


SIGN
   : '+' | '-'
   ;


EXTERN
   : E X T E R N
   ;

PUBLIC
   : P U B L I C
   ;

MASMDIRECTIVE
   : '.' [a-zA-Z0-9] +
   ;


NAME
   : [_a-zA-Z] [a-zA-Z0-9._@]*
   ;


NUMBER
   : [0-9a-fA-F] + ('H' | 'h')?
   ;


STRING1
   : '"' ~'"'* '"'
   ;


STRING2
   : '\u0027' ~'\u0027'* '\u0027'
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
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
