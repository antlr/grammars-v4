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
grammar chip8octo;

file_
   : (preproc_ | instruction_ | num_)* EOF
   ;

instruction_
   : preproc_
   | clear_
   | assign_
   | loop_
   | sprite_
   | addeq_
   | subeq_
   | if_
   | end_
   | load_
   | save_
   | labelnane_
   | oreq_
   | xoreq_
   | andeq_
   | shleq_
   | shreq_
   ;

preproc_
   : const_
   | alias_
   | label_
   | unpack_
   ;

const_
   : COLON 'const' constname_ num_
   ;

constname_
   : IDENTIFIER
   ;

alias_
   : COLON 'alias' aliasname_ (IDENTIFER | num_ | register_)
   ;

aliasname_
   : IDENTIFER
   ;

label_
   : COLON labelnane_
   ;

labelnane_
   : IDENTIFIER
   ;

unpack_
   : COLON 'unpack' (register_ | num_ | IDENTIFIER) (register_ | num_ | IDENTIFIER)
   ;

save_
   : 'save' atom
   ;

clear_
   : 'clear'
   ;

end_
   : ';'
   ;

assign_
   : register_ ':=' atom
   ;

loop_
   : 'loop' instruction_* 'again'
   ;

sprite_
   : 'sprite' atom atom atom
   ;

addeq_
   : atom '+=' atom
   ;

subeq_
   : atom '-=' atom
   ;

oreq_
   : atom '|=' atom
   ;

andeq_
   : atom '&=' atom
   ;

xoreq_
   : atom '^=' atom
   ;

shleq_
   : atom '<<=' atom
   ;

shreq_
   : atom '>>=' atom
   ;

if_
   : 'if' condition_ 'then' instruction_?
   ;

condition_
   : atom COMPARATOR atom
   ;

load_
   : 'load' register_
   ;

atom
   : num_
   | register_
   | aliasname_
   | labelnane_
   | constname_
   ;

register_
   : REGISTER
   | I
   | DELAY
   | BUZZER
   ;

num_
   : DECIMAL
   | HEXBYTE
   ;

COLON
   : ':'
   ;

COMPARATOR
   : '=='
   | '!='
   ;

DELAY
   : 'delay'
   ;

BUZZER
   : 'buzzer'
   ;

REGISTER
   : ('v' | 'V') [0-9A-F]
   ;

I
   : 'i'
   ;

DECIMAL
   : ('-')? [0-9]+
   ;

HEXBYTE
   : '0x' [0-9A-F] [0-9A-F]?
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9-_?]*
   ;

COMMENT
   : '#' ~ [\r\n]* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

