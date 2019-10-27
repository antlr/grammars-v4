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

grammar logo;
module : ( use-declaration | newline )*, ( subroutine-definition | newline )*, ( program-definition | library-definition ), { newline } ;

use-declaration : "USE", identifier, newline ;

subroutine-definition : "SUBROUTINE", identifier, "(", [ identifier, { ",", identifier } ], ")", newline, statements, "END", identifier, newline ;

program-definition : "PROGRAM", identifier, newline, statements, "END", identifier, newline ;

library-definition : "LIBRARY", identifier, { subroutine-declaration | newline }, "END", identifier, newline ;

subroutine-declaration : "SUBROUTINE", identifier, newline ;

statements : { [ statement ], newline } ;

statement : let-statement | if-statement | do-statement | call-statement | return-statement | exit-statement ;

identifier-or-0 : identifier | "0" ;

let-statement : "LET", identifier, ( "=", identifier-or-0 | "<", identifier | ">", identifier-or-0 ) ;

if-statement : if-head, { "ELSE", if-head }, [ "ELSE", newline, statements ], "END", "IF" ;

if-head : "IF", identifier, ( "=" | ">" ), identifier, newline, statements ;

do-statement : "DO", identifier, [ "<", identifier ], newline, statements, "END", "DO" ;

call-statement : "CALL", identifier, [ ".", identifier ], "(", [ identifier-or-0, { ",", identifier-or-0 } ], ")" ;

return-statement : "RETURN" ;

exit-statement : "EXIT", identifier ;

WS
   : [ \t\r\n] -> skip
   ;
