/*
BSD License
Copyright (c) 2016, Tom Everett
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
/*
Adapted from https://github.com/jynnantonix/lolcode/blob/master/BNFGrammar.txt
*/

grammar lolcode;

program
   : 'HAI' code_block 'KTHXBYE'?
   ;

code_block
   : statement +
   ;

statement
   : loop
   | declaration
   | comment
   | print_block
   | if_block
   | input_block
   | func_decl
   | assignment
   | expression
   ;

loop
   : 'IM IN YR' LABEL 'WILE' expression code_block 'IM OUTTA YR' LABEL
   ;

declaration
   : 'I HAS A' LABEL
   | 'I HAS A' LABEL 'ITZ' < value >
   ;

comment
   : 'BTW' STRING
   | 'OBTW' STRING 'TLDR'
   ;

print_block
   : 'VISIBLE' expression* 'MKAY?'?
   ;

if_block
   : 'O RLY?' 'YA RLY' code_block 'OIC'
   | 'O RLY?' 'YA RLY' code_block else_if_block 'OIC'
   ;

else_if_block
   : 'MEBBE' expression code_block else_if_block
   | 'NO WAI' code_block
   | 'MEBBE' expression code_block
   ;

input_block
   : 'GIMMEH' LABEL
   ;

func_decl
   : 'HOW DUZ I' LABEL (('YR' LABEL) ('AN YR' LABEL)*)? code_block 'IF U SAY SO'
   ;

assignment
   : LABEL 'R' expression
   ;

expression
   : equals
   | both
   | not_equals
   | greater
   | less
   | add
   | sub
   | mul
   | div
   | mod
   | cast
   | either
   | all
   | any
   | not
   | func
   | LABEL
   | ATOM
   ;

equals
   : 'BOTH SAEM' expression 'AN' expression
   ;

not_equals
   : 'DIFFRINT' expression 'AN' expression
   ;

both
   : 'BOTH OF' expression 'AN' expression
   ;

either
   : 'EITHER OF' expression 'AN' expression
   ;

greater
   : 'BIGGR OF' expression 'AN' expression
   ;

less
   : 'SMALLR OF' expression 'AN' expression
   ;

add
   : 'SUM OF' expression 'AN' expression
   ;

sub
   : 'DIFF OF' expression 'AN' expression
   ;

mul
   : 'PRODUKT OF' expression 'AN' expression
   ;

div
   : 'QUOSHUNT OF' expression 'AN' expression
   ;

mod
   : 'MOD OF' expression 'AN' expression
   ;

cast
   : 'MAEK' expression 'A' < type >
   ;

all
   : 'ALL OF' expression ('AN' expression)* 'MKAY?'
   ;

any
   : 'ANY OF' expression ('AN' expression)* 'MKAY?'
   ;

not
   : 'NOT' expression
   ;

func
   : LABEL expression + 'MKAY?'
   ;


LABEL
   : ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')+
   ;


ATOM
   : 'WIN' | 'FAIL' | 'NOOB' | ('0' .. '9')+ | ('0' .. '9')* '.' ('0' .. '9')* | STRING
   ;


STRING
   : '"' ('\'"' | ~'"')* '"'
   ;


WS
   : [ \r\n] -> skip
   ;
