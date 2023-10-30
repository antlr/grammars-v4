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

grammar bnf;

rulelist
    : rule_* EOF
;

rule_
    : lhs ASSIGN rhs
    ;

lhs
    : id_
    ;

rhs
    : alternatives
    ;

alternatives
    : alternative ('|' alternative)*
    ;

alternative
    : element*
    ;

element
    : optional_
    | zeroormore
    | oneormore
    | text_
    | id_
    ;

optional_
    : '[' alternatives ']'
    ;

zeroormore
    : '{' alternatives '}'
    ;

oneormore
    : '(' alternatives ')'
    ;

text_
    : ID
    ;

id_
    : '<' ruleid '>'
    ;

ruleid
    : ID
    ;

ASSIGN
    : '::='
    ;

Right_Parenthesis
    : ')'
    ;

Left_Parenthesis
    : '('
    ;

Right_Curly_Bracket
    : '}'
    ;

Left_Curly_Bracket
    : '{'
    ;

Right_Square_Bracket
    : ']'
    ;

Left_Square_Bracket
    : '['
    ;

Vertical_Line
    : '|'
    ;

Greater_Than_Sign
    : '>'
    ;

Less_Than_Sign
    : '<'
    ;

ID
    : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|' ')+
    ;

WS
    : [ \r\n\t] -> skip
    ;
