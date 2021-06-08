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
/*
* http://www.mit.edu/afs.new/sipb/project/pike/tutorial/tutorial_onepage.html#D
*/

grammar pike;

program
   : definition*
   ;

definition
   : impo
   | inheritance
   | function_declaration
   | function_definition
   | variables
   | constant
   | class_def
   ;

impo
   : modifiers? 'import' constant_identifier ';'
   ;

inheritance
   : modifiers? 'inherit' program_specifier (':' IDENTIFIER)? ';'
   ;

function_declaration
   : modifiers? type_ IDENTIFIER '(' arguments ')' ';'
   ;

function_definition
   : modifiers? type_ IDENTIFIER '(' arguments ')' block
   ;

variables
   : modifiers? type_ variable_names ';'
   ;

variable_names
   : variable_name (',' variable_name)*
   ;

variable_name
   : '*'* IDENTIFIER ('=' expression2)?
   ;

constant
   : modifiers? 'constant' constant_names ';'
   ;

constant_names
   : constant_name (',' constant_name)*
   ;

constant_name
   : IDENTIFIER '=' expression2
   ;

class_def
   : modifiers? 'class' ';'?
   ;

class_implementation
   : 'class' IDENTIFIER? '{' program '}'
   ;

modifiers
   : 'static'
   | 'private'
   | 'nomask'
   | 'public'
   | 'protected'
   | 'inline'
   ;

block
   : '{' statement* '}'
   ;

statement
   : expression2 ';'
   | cond
   | while_stmt
   | do_while_stmt
   | for_stmt
   | switch_stmt
   | case_stmt
   | default_stmt
   | block
   | foreach_stmt
   | break_stmt
   | continue_stmt
   | ';'
   ;

cond
   : 'if' statement ('else' statement)?
   ;

while_stmt
   : 'while''(' expression ')' statement
   ;

do_while_stmt
   : 'do' statement while_stmt '(' expression ')' ';'
   ;

for_stmt
   : 'for' '(' expression? ';' expression? ';' expression? ')' statement
   ;

switch_stmt
   : 'switch' '(' expression ')' block
   ;

case_stmt
   : 'case' expression ('..' expression)? ':'
   ;

default_stmt
   : 'default' ':'
   ;

foreach_stmt
   : 'foreach' '(' expression ':' expression6 ')' statement
   ;

break_stmt
   : 'break' ';'
   ;

continue_stmt
   : 'continue' ';'
   ;

expression
   : expression2 (',' expression2)*
   ;

expression2
   : (lvalue ('=' | '+=' | '*=' | '/=' | '&=' | '|=' | '^=' | '<<=' | '>>=' | '%='))* expression3
   ;

expression3
   : expression4 ('?' expression3 ':' expression3)?
   ;

expression4
   : (expression5 ('||' | '&&' | '|' | '^' | '&' | '==' | '!=' | '>' | '<' | '>=' | '<=' | '<<' | '>>' | '+' | '*' | '/' | '%'))* expression5
   ;

expression5
   : expression6
   | '(' type_ ')' expression5
   | '--' expression6
   | '++' expression6
   | expression6 '--'
   | expression6 '++'
   | '~' expression5
   | '-' expression5
   ;

expression6
   :
   ( STRING
   | NUMBER
   | FLOAT
   | catch_
   | gauge
   | sscanf
   | lambda
   | class_implementation
   | constant_identifier
   | mapping
   | multiset
   | array
   | parenthesis
   ) extension*
   ;

extension
   : '(' expression_list ')'
   | '->' IDENTIFIER
   | '[' expression ('..' expression)? ']'
   ;

catch_
   : 'catch' ('(' expression ')' | block)
   ;

gauge
   : 'gauge' ('(' expression ')' | block)
   ;

sscanf
   : 'sscanf' '(' expression2 ',' expression2 (',' lvalue)* ')'
   ;

lvalue
   : 'lambda' expression6
   | type_ IDENTIFIER
   ;

lambda
   : 'lambda' '(' arguments ')' block
   ;

constant_identifier
   : IDENTIFIER ('.' IDENTIFIER)*
   ;


array
   : '({' expression_list '})'
   ;

multiset
   : '(<' expression_list '>)'
   ;

mapping
   : '([' (expression ':' expression (',' expression ':' expression)*)? ','? '])'
   ;


program_specifier
   :
   /*string_constant |*/ constant_identifier
   ;

parenthesis
   : '(' expression ')'
   ;

expression_list
   : (splice_expression (',' splice_expression)*)? ','?
   ;

splice_expression
   : '@'? expression2
   ;

argument
   : type_ ('...')? (IDENTIFIER)?
   ;

arguments
   : (argument (',' argument)*)? (',')?
   ;

type_
   : ('int' | 'string' | 'float' | 'program' | ('object' ('(' program_specifier ')')?) | ('mapping' ('(' type_ ':' type_ ')')?) | ('array' ('(' type_ ')')?) | ('multiset' ('(' type_ ')')?) | ('function' function_type?)) ('*')*
   ;

function_type
   : '(' type_ (',' type_)* ('...')? ')'
   ;


IDENTIFIER
   : LETTER (LETTER | DIGIT)* | '+' | '/' | '%' | '*' | '&' | '|' | '^' | '~' | '<' | '<<' | '<=' | '>' | '>>' | '>=' | '==' | '!=' | '!' | '()' | '-' | '->' | '->=' | '[]' | '[]='
   ;


LETTER
   : 'a' .. 'z' | 'A' .. 'Z' | '_'
   ;


DIGIT
   : '0' .. '9'
   ;


FLOAT
   : DIGIT DIGIT* '.' DIGIT*
   ;


NUMBER
   : DIGIT DIGIT* | '0x' DIGIT*
   ;


STRING
   : '"' ~ '"'* '"'
   ;


WS
   : [ \t\r\n] -> skip
   ;
