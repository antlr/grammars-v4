/*
[The "BSD licence"]
Copyright (c) 2012 Tom Everett
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

grammar modelica;

stored_definition
   : ('within' (name)? ';')* (('final')? class_definition ';')* EOF
   ;

class_definition
   : ('encapsulated')? class_prefixes class_specifier
   ;

class_specifier
   : long_class_specifier
   | short_class_specifier
   | der_class_specifier
   ;

class_prefixes
   : ('partial')? ('class' | 'model' | ('operator')? 'record' | 'block' | ('expandable')? 'connector' | 'type' | 'package' | (('pure' | 'impure'))? ('operator')? 'function' | 'operator')
   ;

long_class_specifier
   : IDENT string_comment composition 'end' IDENT
   | 'extends' IDENT (class_modification)? string_comment composition 'end' IDENT
   ;

short_class_specifier
   : IDENT '=' base_prefix name (array_subscripts)? (class_modification)? comment
   | IDENT '=' 'enumeration' '(' ((enum_list)? | ':') ')' comment
   ;

der_class_specifier
   : IDENT '=' 'der' '(' name ',' IDENT (',' IDENT)* ')' comment
   ;

base_prefix
   : type_prefix
   ;

enum_list
   : enumeration_literal (',' enumeration_literal)*
   ;

enumeration_literal
   : IDENT comment
   ;

composition
   : element_list ('public' element_list | 'protected' element_list | equation_section | algorithm_section)* ('external' (language_specification)? (external_function_call)? (annotation)? ';')? (annotation ';')?
   ;

language_specification
   : STRING
   ;

external_function_call
   : (component_reference '=')? IDENT '(' (expression_list)? ')'
   ;

element_list
   : (element ';')*
   ;

element
   : import_clause
   | extends_clause
   | ('redeclare')? ('final')? ('inner')? ('outer')? ((class_definition | component_clause) | 'replaceable' (class_definition | component_clause) (constraining_clause comment)?)
   ;

import_clause
   : 'import' (IDENT '=' name | name '.*' | name '.{' import_list '}' | name ) comment
   ;

import_list
   : IDENT (',' IDENT)*
   ;

extends_clause
   : 'extends' name (class_modification)? (annotation)?
   ;

constraining_clause
   : 'constrainedby' name (class_modification)?
   ;

component_clause
   : type_prefix type_specifier (array_subscripts)? component_list
   ;

type_prefix
   : ('flow' | 'stream')? ('discrete' | 'parameter' | 'constant')? ('input' | 'output')?
   ;

type_specifier
   : name
   ;

component_list
   : component_declaration (',' component_declaration)*
   ;

component_declaration
   : declaration (condition_attribute)? comment
   ;

condition_attribute
   : 'if' expression
   ;

declaration
   : IDENT (array_subscripts)? (modification)?
   ;

modification
   : class_modification ('=' expression)?
   | '=' expression
   | ':=' expression
   ;

class_modification
   : '(' (argument_list)? ')'
   ;

argument_list
   : argument (',' argument)*
   ;

argument
   : element_modification_or_replaceable
   | element_redeclaration
   ;

element_modification_or_replaceable
   : ('each')? ('final')? (element_modification | element_replaceable)
   ;

element_modification
   : name (modification)? string_comment
   ;

element_redeclaration
   : 'redeclare' ('each')? ('final')? ((short_class_definition | component_clause1) | element_replaceable)
   ;

element_replaceable
   : 'replaceable' (short_class_definition | component_clause1) (constraining_clause)?
   ;

component_clause1
   : type_prefix type_specifier component_declaration1
   ;

component_declaration1
   : declaration comment
   ;

short_class_definition
   : class_prefixes short_class_specifier
   ;

equation_section
   : ('initial')? 'equation' (equation ';')*
   ;

algorithm_section
   : ('initial')? 'algorithm' (statement ';')*
   ;

equation
   : (simple_expression '=' expression | if_equation | for_equation | connect_clause | when_equation | name function_call_args) comment
   ;

statement
   : (component_reference (':=' expression | function_call_args) | '(' output_expression_list ')' ':=' component_reference function_call_args | 'break' | 'return' | if_statement | for_statement | while_statement | when_statement) comment
   ;

if_equation
   : 'if' expression 'then' (equation ';')* ('elseif' expression 'then' (equation ';')*)* ('else' (equation ';')*)? 'end' 'if'
   ;

if_statement
   : 'if' expression 'then' (statement ';')* ('elseif' expression 'then' (statement ';')*)* ('else' (statement ';')*)? 'end' 'if'
   ;

for_equation
   : 'for' for_indices 'loop' (equation ';')* 'end' 'for'
   ;

for_statement
   : 'for' for_indices 'loop' (statement ';')* 'end' 'for'
   ;

for_indices
   : for_index (',' for_index)*
   ;

for_index
   : IDENT ('in' expression)?
   ;

while_statement
   : 'while' expression 'loop' (statement ';')* 'end' 'while'
   ;

when_equation
   : 'when' expression 'then' (equation ';')* ('elsewhen' expression 'then' (equation ';')*)* 'end' 'when'
   ;

when_statement
   : 'when' expression 'then' (statement ';')* ('elsewhen' expression 'then' (statement ';')*)* 'end' 'when'
   ;

connect_clause
   : 'connect' '(' component_reference ',' component_reference ')'
   ;

expression
   : simple_expression
   | 'if' expression 'then' expression ('elseif' expression 'then' expression)* 'else' expression
   ;

simple_expression
   : logical_expression (':' logical_expression (':' logical_expression)?)?
   ;

logical_expression
   : logical_term ('or' logical_term)*
   ;

logical_term
   : logical_factor ('and' logical_factor)*
   ;

logical_factor
   : ('not')? relation
   ;

relation
   : arithmetic_expression (rel_op arithmetic_expression)?
   ;

rel_op
   : '<'
   | '<='
   | '>'
   | '>='
   | '=='
   | '<>'
   ;

arithmetic_expression
   : (add_op)? term (add_op term)*
   ;

add_op
   : '+'
   | '-'
   | '.+'
   | '.-'
   ;

term
   : factor (mul_op factor)*
   ;

mul_op
   : '*'
   | '/'
   | '.*'
   | './'
   ;

factor
   : primary (('^' | '.^') primary)?
   ;

primary
   : UNSIGNED_NUMBER
   | STRING
   | 'false'
   | 'true'
   | (name | 'der' | 'initial') function_call_args
   | component_reference
   | '(' output_expression_list ')'
   | '[' expression_list (';' expression_list)* ']'
   | '{' function_arguments '}'
   | 'end'
   ;

name
   : ('.')? IDENT ('.' IDENT)*
   ;

component_reference
   : ('.')? IDENT (array_subscripts)? ('.' IDENT (array_subscripts)?)*
   ;

function_call_args
   : '(' (function_arguments)? ')'
   ;

function_arguments
   : function_argument (',' function_arguments | 'for' for_indices)?
   | named_arguments
   ;

named_arguments
   : named_argument (',' named_arguments)?
   ;

named_argument
   : IDENT '=' function_argument
   ;

function_argument
   : 'function' name '(' (named_arguments)? ')'
   | expression
   ;

output_expression_list
   : (expression)? (',' (expression)?)*
   ;

expression_list
   : expression (',' expression)*
   ;

array_subscripts
   : '[' subscript_ (',' subscript_)* ']'
   ;

subscript_
   : ':'
   | expression
   ;

comment
   : string_comment (annotation)?
   ;

string_comment
   : (STRING ('+' STRING)*)?
   ;

annotation
   : 'annotation' class_modification
   ;


IDENT
   : NONDIGIT (DIGIT | NONDIGIT)* | Q_IDENT
   ;


fragment Q_IDENT
   : '\'' (Q_CHAR | S_ESCAPE) (Q_CHAR | S_ESCAPE)* '\''
   ;


fragment S_CHAR
   : ~ ["\\]
   ;


fragment NONDIGIT
   : '_' | 'a' .. 'z' | 'A' .. 'Z'
   ;


STRING
   : '"' ( S_CHAR | S_ESCAPE )* '"'
   ;


fragment Q_CHAR
   : NONDIGIT | DIGIT | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '>' | '=' | '?' | '@' | '[' | ']' | '^' | '{' | '}' | '|' | '~'
   ;


fragment S_ESCAPE
   : '\\' ('’' | '\'' | '"' | '?' | '\\' | 'a' | 'b' | 'f' | 'n' | 'r' | 't' | 'v')
   ;


fragment DIGIT
   : '0' .. '9'
   ;


fragment UNSIGNED_INTEGER
   : DIGIT (DIGIT)*
   ;


UNSIGNED_NUMBER
   : UNSIGNED_INTEGER ('.' (UNSIGNED_INTEGER)?)? (('e' | 'E') ('+' | '-')? UNSIGNED_INTEGER)?
   ;


WS
   : [ \r\n\t] + -> channel (HIDDEN)
   ;

COMMENT
    :   '/*' .*? '*/' -> channel (HIDDEN)
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> channel (HIDDEN)
    ;
