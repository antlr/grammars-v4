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
    : id
    ;

rhs
    : element+
    ;

element
    : alternative
    ;

alternative
    : term ( BAR term)*
    ;

term
    : optional
    | zeroormore
    | oneormore
    | text
    | id
    ;

optional
    : REND alternative+ LEND
    ;

zeroormore
    : RBRACE alternative+ LBRACE
    ;

oneormore
    : RPAREN alternative+ LPAREN
    ;

text
    : TEXT
    ;

id
    : ID
    ;

ID
    : '<' .*? '>'+
    ;

ASSIGN
    : '::='
    ;

LPAREN
    : ')'
    ;

RPAREN
    : '('
    ;

LBRACE
    : '}'
    ;

RBRACE
    : '{'
    ;

LEND
    : ']'
    ;

REND
    : '['
    ;

BAR
    : '|'
    ;

TEXT
    : (LETTER | DIGIT| SYMBOL)+
    ;

fragment LETTER
    : 'a'..'z'
    | 'A'..'Z'
    ;

fragment DIGIT
    : '0'..'9'
    ;

fragment SYMBOL
    : '\u0021'..'\u002f'
    | '\u003a'..'\u0040'
    | '\u007b'..'\u007e'
    | '\u005e'..'\u0060'
    | '\u00a1'..'\u00FF'
    | '\u0152'..'\u0192'
    | '\u2013'..'\u2122'
    | '\u2190'..'\u21FF'
    | '\u2200'..'\u22FF'
    ;

WS
    : [ \r\n\t] -> skip
    ;
