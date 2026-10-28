/*
MIT License

Copyright (c) 2025 xndcn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

grammar blueprint;

blueprint
    : definition* EOF
    ;

definition
    : assignment
    | module
    ;

assignment
    : IDENT ('+=' | '=') expression
    ;

module
    : IDENT ('{' propertyList '}' | '(' propertyList ')')
    ;

propertyList
    : (property (',' property)* ','?)?
    ;

property
    : IDENT (':' | '=') expression
    ;

expression
    : value operator?
    ;

operator
    : '+' expression
    ;

value
    : BOOLEAN
    | select
    | variable
    | INTEGER
    | STRING
    | listValue
    | mapValue
    ;

select
    : 'select' '(' conditions ',' '{' selectCase+ '}' ')'
    ;

variable
    : IDENT
    ;

listValue
    : '[' (expression (',' expression)*)? ','? ']'
    ;

mapValue
    : '{' propertyList '}'
    ;

conditions
    : singleCondition
    | '(' singleCondition (',' singleCondition)* ','? ')'
    ;

singleCondition
    : IDENT '(' (STRING (',' STRING)*)? ','? ')'
    ;

selectCase
    : selectPatterns ':' (expression | 'unset') ','?
    ;

selectPatterns
    : selectOnePattern
    | '(' selectOnePattern (',' selectOnePattern)* ','? ')'
    ;

selectOnePattern
    : ('any' selectBinding?) | 'default' | BOOLEAN | STRING
    ;

selectBinding
    : '@' IDENT
    ;


BOOLEAN
    : 'true' | 'false'
    ;

INTEGER
    : '-'? [0-9]+
    ;

IDENT
    : [a-zA-Z_][a-zA-Z0-9_-]*
    ;

STRING
    : '"' (~["\r\n] | '\\"')* '"'
    ;

COMMENT
    : '//' ~[\r\n]* -> channel (HIDDEN)
    ;

MULTILINE_COMMENT
    : '/*' .*? '*/' -> channel (HIDDEN)
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
