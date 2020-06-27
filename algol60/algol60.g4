grammar algol60;

/*
[The "BSD licence"]
Copyright (c) 2018 Tom Everett
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
program
   : (block | compound_statement) EOF
   ;

block
   : (label ':')? block_head compound_tail
   ;

block_head
   : 'begin' (declaration ';')+
   ;

compound_statement
   : (label ':')? 'begin' compound_tail
   ;

compound_tail
   : (statement ';')* statement 'end'
   ;

declaration
   : type_declaration
   | array_declaration
   | switch_declaration
   | procedure_declaration
   ;

type_declaration
   : local_or_own_type type_list
   ;

local_or_own_type
   : 'own'? type
   ;

type
   : 'real'
   | 'integer'
   | 'boolean'
   ;

type_list
   : simple_variable (',' simple_variable)*
   ;

array_declaration
   : local_or_own_type? 'array' array_list
   ;

array_list
   : array_segment (',' array_segment)*
   ;

array_segment
   : (array_identifier ',')* array_identifier bound_pair_list
   ;

array_identifier
   : identifier
   ;

bound_pair_list
   : bound_pair (',' bound_pair)*
   ;

bound_pair
   : lower_bound ':' upper_bound
   ;

upper_bound
   : arithmetic_expression
   ;

lower_bound
   : arithmetic_expression
   ;

switch_declaration
   : 'switch' switch_identifier ':=' switch_list
   ;

switch_identifier
   : identifier
   ;

switch_list
   : designational_expression (',' designational_expression)*
   ;

procedure_declaration
   :  type? 'procedure' procedure_heading procedure_body
   ;

procedure_heading
   : procedure_identifier formal_parameter_part? ';' value_part? specification_part?
   ;

procedure_identifier
   : identifier
   ;

formal_parameter_part
   : formal_parameter_list
   ;

formal_parameter_list
   : '(' formal_parameter (parameter_delimiter formal_parameter)* ')'
   ;

formal_parameter
   : identifier
   ;

value_part
   : 'value' identifier_list ';'
   ;

specification_part
   : specifier identifier_list (';' specifier identifier_list)*
   ;

specifier
   : STRING
   | type
   | type? 'array'
   | 'label'
   | 'switch'
   | type? 'procedure'
   ;

identifier_list
   : identifier (',' identifier)*
   ;

procedure_body
   : statement
   ;

statement
   : unconditional_statement
   | conditional_statement
   | for_statement
   ;

unconditional_statement
   : basic_statement
   | compound_statement
   | block
   ;

basic_statement
   : (label ':')* unlabelled_basic_statement?
   ;

label
   : identifier
   | unsigned_integer
   ;

unlabelled_basic_statement
   : assignment_statement
   | go_to_statement
   | procedure_statement
   ;

assignment_statement
   : left_part_list (arithmetic_expression | boolean_expression)
   ;

left_part_list
   : left_part+
   ;

left_part
   : (variable | procedure_identifier) ':='
   ;

go_to_statement
   : 'goto' designational_expression
   ;

designational_expression
   : label
   | switch_designator
   | if_clause designational_expression 'else' designational_expression
   ;

switch_designator
   : switch_identifier subscript_expression?
   ;

procedure_statement
   : procedure_identifier actual_parameter_part?
   ;

actual_parameter_part
   : actual_parameter_list
   ;

actual_parameter_list
   : actual_parameter (parameter_delimiter actual_parameter)*
   ;

parameter_delimiter
   : ','
   | ')' letter_string ':' '('
   ;

actual_parameter
   : STRING
   | expression
   | array_identifier
   | switch_identifier
   | procedure_identifier
   ;

conditional_statement
   : (label ':')* ((if_statement ('else' statement)?) | (if_clause for_statement)) 
   ;

if_statement
   : if_clause unconditional_statement
   ;

if_clause
   : 'if' boolean_expression 'then'
   ;

for_statement
   : (label ':')* for_clause statement
   ;

for_clause
   : 'for' variable ':=' for_list 'do'
   ;

for_list
   : for_list_element (',' for_list_element)*
   ;

for_list_element
   : arithmetic_expression?
   ;

arithmetic_expression
   : 'step' arithmetic_expression 'until' arithmetic_expression
   | arithmetic_expression 'while' boolean_expression
   ;

expression
   : arithmetic_expression
   | boolean_expression
   | designational_expression
   ;

arithmetic_expression2
   : simple_arithmetic_expression
   | if_clause simple_arithmetic_expression 'else' arithmetic_expression
   ;

simple_arithmetic_expression
   : adding_operator? term (adding_operator term)*
   ;

adding_operator
   : '+'
   | ' –'
   ;

term
   : factor (multiplying_operator factor)*
   ;

multiplying_operator
   : '×'
   | '/'
   | '÷'
   ;

factor
   : primary ('↑' primary)*
   ;

primary
   : unsigned_number
   | variable
   | function_designator
   | '(' arithmetic_expression ')'
   ;

unsigned_number
   : decimal_number
   | exponential_part
   | decimal_number exponential_part
   ;

decimal_number
   : unsigned_integer
   | decimal_fraction
   | unsigned_integer decimal_fraction
   ;

unsigned_integer
   : DIGIT+
   ;

decimal_fraction
   : '.' unsigned_integer
   ;

exponential_part
   : '10' integer
   ;

integer
   : ('+' |'–')? unsigned_integer
   ;

boolean_expression:
   if_clause boolean_expression 'else' boolean_expression
   | boolean_expression '≣' boolean_expression
   | boolean_expression '⊃' boolean_expression
   | boolean_expression '⋁' boolean_expression
   | boolean_expression '⋀' boolean_expression
   | '¬' boolean_expression
   | logical_value
   | variable
   | function_designator
   | relation
   | '(' boolean_expression ')'
   ;


relation
   : simple_arithmetic_expression relational_operator simple_arithmetic_expression
   ;

relational_operator
   : '<'
   | '≤'
   | '='
   | '≠'
   | '>'
   | '≥'
   ;

function_designator
   : procedure_identifier actual_parameter_part
   ;

variable
   : simple_variable
   | subscripted_variable
   ;

simple_variable
   : variable_identifier
   ;

variable_identifier
   : identifier
   ;

subscripted_variable
   : array_identifier '[' subscript_list ']'
   ;

subscript_list
   : subscript_expression (',' subscript_expression)*
   ;

subscript_expression
   : arithmetic_expression
   ;

STRING
   :  '"' ~ ["\r\n]* '"'
   ;

open_string
   : proper_string? STRING (proper_string? STRING)+
   ;

proper_string
   : STRING
   ;

letter_string
   : LETTER+
   ;

identifier
   : LETTER (LETTER | DIGIT)*
   ;

basic_symbol
   : LETTER
   | DIGIT
   | logical_value
   | delimiter
   ;

LETTER
   : 'a' .. 'z'
   | 'A' .. 'Z'
   ;

DIGIT
   : '0' .. '9'
   ;

logical_value
   : 'true'
   | 'false'
   ;

delimiter
   : operator
   | SEPARATOR
   | bracket
   | DECLARATOR
   | specificator
   ;

operator
   : ARITHMETIC_OPERATOR
   | RELATIONAL_OPERATOR
   | LOGICAL_OPERATOR
   | SEQUENTIAL_OPERATOR
   ;

ARITHMETIC_OPERATOR
   : '+'
   | '–'
   | '×'
   | '/'
   | '÷'
   | '↑'
   ;


RELATIONAL_OPERATOR
   : '<' | '≤' | '=' | '≠' | '>' | '≥'
   ;

LOGICAL_OPERATOR
   : '≣'
   | '⊃'
   | '⋁'
   | '⋀'
   | '¬'
   ;

SEQUENTIAL_OPERATOR
   : 'goto'
   | 'if'
   | 'then'
   | 'else'
   | 'for'
   | 'do'
   ;

SEPARATOR
   : ','
   | '.'
   | '10'
   | ':'
   | ';'
   | ':='
   | '_'
   | 'step'
   | 'until'
   | 'while'
   | 'comment'
   ;

bracket
   : '('
   | ')'
   | '['
   | ']'
   | '`'
   | '\''
   | 'begin'
   | 'end'
   ;

DECLARATOR
   : 'own'
   | 'boolean'
   | 'integer'
   | 'real'
   | 'array'
   | 'switch'
   | 'procedure'
   ;

specificator
   : 'string'
   | 'label'
   | 'value'
   ;


WS
   : [ \r\n\t] + -> skip
   ;
