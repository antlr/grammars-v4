/*
BSD License

Copyright (c) 2023, Tom Everett
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
grammar chip8;

file_
   : instruction_* EOF
   ;

instruction_
   : cls
   | ret
   | sys
   | jp
   | call
   | se
   | sne
   | se1
   | ld
   | add
   | ld2
   | or
   | and
   | xor
   | add2
   | sub
   | shr
   | subn
   | shl
   | sne2
   | ld3
   | jp2
   | rnd
   | drw
   | skp
   | sknp
   | ld4
   | ld5
   | ld6
   | ld7
   | add3
   | ld8
   | ld9
   | ld10
   | ld11
   ;

cls
   : 'CLS'
   ;

ret
   : 'RET'
   ;

sys
   : 'SYS' ADDR
   ;

jp
   : 'JP' ADDR
   ;

call
   : 'CALL' ADDR
   ;

se
   : 'SE' REGISTER ',' BYTE
   ;

sne
   : 'SNE' REGISTER ',' BYTE
   ;

se1
   : 'SE' REGISTER ',' REGISTER
   ;

ld
   : 'LD' REGISTER ',' BYTE
   ;

add
   : 'ADD' REGISTER ',' BYTE
   ;

ld2
   : 'LD' REGISTER ',' REGISTER
   ;

or
   : 'OR' REGISTER ',' REGISTER
   ;

and
   : 'AND' REGISTER ',' REGISTER
   ;

xor
   : 'XOR' REGISTER ',' REGISTER
   ;

add2
   : 'ADD' REGISTER ',' REGISTER
   ;

sub
   : 'SUB' REGISTER ',' REGISTER
   ;

shr
   : 'SHR' REGISTER '{' ',' REGISTER '}'
   ;

subn
   : 'SUBN' REGISTER ',' REGISTER
   ;

shl
   : 'SHL' REGISTER '{' ',' REGISTER '}'
   ;

sne2
   : REGISTER ',' REGISTER
   ;

ld3
   : 'LD' 'I' ',' ADDR
   ;

jp2
   : 'JP' 'V' '0' ',' ADDR
   ;

rnd
   : 'RND' REGISTER ',' BYTE
   ;

drw
   : 'DRW' REGISTER ',' REGISTER ',' DIGIT
   ;

skp
   : 'SKP' REGISTER
   ;

sknp
   : 'SKNP' REGISTER
   ;

ld4
   : 'LD' REGISTER ',' 'DT'
   ;

ld5
   : 'LD' REGISTER ',' 'K'
   ;

ld6
   : 'LD' 'DT' ',' REGISTER
   ;

ld7
   : 'LD' 'ST' ',' REGISTER
   ;

add3
   : 'ADD' 'I' ',' REGISTER
   ;

ld8
   : 'LD' 'F' ',' REGISTER
   ;

ld9
   : 'LD' 'B' ',' REGISTER
   ;

ld10
   : '[' 'I' ']' ',' REGISTER
   ;

ld11
   : 'LD' REGISTER ',' '[' 'I' ']'
   ;

REGISTER
   : 'V' DIGIT
   ;

ADDR
   : DIGIT DIGIT DIGIT
   ;

BYTE
   : DIGIT DIGIT
   ;

DIGIT
   : [0-9A-F]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

