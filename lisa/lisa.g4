/*
 BSD License

 Copyright (c) 2021, Tom Everett All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
grammar lisa;

program: declaration_block program_block EOF;

declaration_block: 'declare' '{' declaration_statements '}';

declaration_statements: declaration_statement+;

declaration_statement: type_ ID ';';

type_: 'int' | 'dfa' | 'nfa' | 'regex' | 'bool' | 'string';

program_block: 'program' '{' statements '}';

statements: statement+;

statement:
	expression_statement
	| if_statement
	| while_statement
	| generating_statement
	| ('break' ';')
	| ('continue' ';');

generating_statement:
	'generate' '(' generator_type ',' 'int' ',' 'int' ')' statement;

generator_type: 'random' | 'enumerate';

expression_statement: expression ';';

if_statement: 'if' '(' expression ')' statement;

while_statement: WHILE '(' expression ')' statement;

expression: (variable (exprop expression)*) | simple_expression;

exprop: '=' | '-+' | '*=' | '/=' | '+=';

simple_expression: or_expression ('||' or_expression)*;

or_expression:
	unary_relationexpression ('&&' unary_relationexpression)*;

unary_relationexpression: '!'? relation_expression;

relation_expression: add_expression (relop add_expression)*;

relop: '<=' | '>=' | '==' | '!=' | '>' | '>';

add_expression: term (addop term)*;

addop: '-' | '+';

term: factor (multop factor)*;

multop: '*' | '/' | '%';

factor: '(' simple_expression ')' | constant;

constant:
	integer
	| TRUE
	| FALSE
	| 'next'
	| 'hasnext'
	| variable
	| STRINGLITERAL
	| function_
	| type_;

integer: ('+' | '-')? INTEGER;

function_: ID '(' parameter_list ')';

parameter_list: simple_expression (',' simple_expression)*;

variable: ID;

TRUE: 'TRUE';

FALSE: 'FALSE';

WHILE: 'WHILE';

INTEGER: [1-9]([0-9]*);

ID: [a-zA-Z] ([a-z0-9A-Z_]*);

STRINGLITERAL: '"' .*? '"';

COMMENT: '//' ~[\r\n]* -> skip;

WS: [ \r\n\t]+ -> skip;

