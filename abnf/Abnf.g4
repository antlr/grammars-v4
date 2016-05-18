/*
BSD License

Copyright (c) 2013, Rainer Schuster
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Rainer Schuster nor the names of its contributors
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

ABNF grammar derived from:

    http://tools.ietf.org/html/rfc5234

    Augmented BNF for Syntax Specifications: ABNF
    January 2008

    http://tools.ietf.org/html/rfc7405

    Case-Sensitive String Support in ABNF
    December 2014

Terminal rules mainly created by ANTLRWorks 1.5 sample code.
 */
grammar Abnf;

// Note: Whitespace handling not as strict as in the specification.

rulelist
   : rule_* EOF
   ;

rule_
   : ID ( '=' | '=/' ) elements
   ;

elements
   : alternation
   ;

alternation
   : concatenation ( '/' concatenation )*
   ;

concatenation
   : repetition ( repetition )*
   ;

repetition
   : repeat? element
   ;

repeat
   : INT | ( INT? '*' INT? )
   ;

element
   : ID | group | option | STRING | NumberValue | ProseValue
   ;

group
   : '(' alternation ')'
   ;

option
   : '[' alternation ']'
   ;


NumberValue
   : '%' ( BinaryValue | DecimalValue | HexValue )
   ;


fragment BinaryValue
   : 'b' BIT+ ( ( '.' BIT+ )+ | ( '-' BIT+ ) )?
   ;


fragment DecimalValue
   : 'd' DIGIT+ ( ( '.' DIGIT+ )+ | ( '-' DIGIT+ ) )?
   ;


fragment HexValue
   : 'x' HEX_DIGIT+ ( ( '.' HEX_DIGIT+ )+ | ( '-' HEX_DIGIT+ ) )?
   ;


ProseValue
   : '<' ( ~ '>' )* '>'
   ;


ID
   : ( 'a' .. 'z' | 'A' .. 'Z' ) ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' )*
   ;


INT
   : '0' .. '9'+
   ;


COMMENT
   : ';' ~ ( '\n' | '\r' )* '\r'? '\n' -> channel ( HIDDEN )
   ;


WS
   : ( ' ' | '\t' | '\r' | '\n' ) -> channel ( HIDDEN )
   ;


STRING
   : ( '%s' | '%i' )? '"' ( ~ '"' )* '"'
   ;


fragment BIT
   : '0' .. '1'
   ;


fragment DIGIT
   : '0' .. '9'
   ;


// Note: from the RFC errata (http://www.rfc-editor.org/errata_search.php?rfc=5234&eid=4040):
// > ABNF strings are case insensitive and the character set for these strings is US-ASCII.
// > So the definition of HEXDIG already allows for both upper and lower case (or a mixture).
fragment HEX_DIGIT
   : ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
   ;
