/*
BSD License

Copyright (c) 2019, Tom Everett
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
grammar scotty;

prog
   : program_lines EOF
   ;

program_lines
   : prefix_exp
   | fn_def program_lines
   | var_assign program_lines
   ;

var_assign
   : identifier '=' prefix_exp
   ;

fn_def
   : 'fun' identifier identifier '=' prefix_exp
   ;

prefix_exp
   : ('+' prefix_exp prefix_exp)
   | ('-' prefix_exp prefix_exp)
   | ('*' prefix_exp prefix_exp)
   | ('/' prefix_exp prefix_exp)
   | ('(' identifier prefix_exp ')')
   | identifier
   | number
   ;

identifier
   : LETTER id_tail
   | LETTER
   ;

id_tail
   : LETTER id_tail
   | DIGIT id_tail
   | LETTER
   | DIGIT
   ;

number
   : '-' digits
   | digits
   ;

digits
   : DIGIT digits
   | '.' digits
   | DIGIT
   ;

DIGIT
   : [0-9]
   ;

LETTER
   : [A-Za-z]
   ;

WS
   : [ \t\r\n] -> skip
   ;

