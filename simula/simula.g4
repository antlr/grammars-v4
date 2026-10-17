/*
BSD License

Copyright (c) 2026, Tom Everett
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
grammar simula;

fragment LETTER
   : [a-zA-Z]
   ;

DIGIT
   : [0-9]
   ;

identifier
   : IDENTIFIER
   ;

unsigned_number
   : decimal_number exponent_part?
   | exponent_part
   ;

decimal_number
   : unsigned_integer decimal_fraction?
   | decimal_fraction
   ;

decimal_fraction
   : '.' unsigned_integer
   ;

exponent_part
   : ('&' | '&&') ('+' | '-')? unsigned_integer
   ;
   // The standard also allows a radix-prefixed integer form ("radix R
   
   // radix-digit..."), e.g. "4R123" for base-4 literals. It is omitted here:
   
   // in a combined ANTLR grammar, quoted literals used inline in parser rules
   
   // (like radix's '2'/'4'/'8') are always given lexer priority over same-length
   
   // matches from explicit lexer rules such as DIGIT, regardless of where DIGIT
   
   // is declared in the file. That made every plain decimal literal "2", "4",
   
   // "8" (and so on) ambiguous with a dead-end radix parse (since a bare radix
   
   // value is never actually followed by 'R'), breaking ordinary number
   
   // literals throughout the language.
   
unsigned_integer
   : DIGIT (DIGIT | '_')*
   ;

string
   : simple_string ( /*string_separator*/
   
   simple_string)*
   ;
   //string_separator
   
   //   : token_separator (token_separator)*
   
   //   ;
   
simple_string
   : STRING
   ;

fragment ISO_CODE_FRAGMENT
   : '!' DIGIT DIGIT? DIGIT? '!'
   ;
   // The lexer has no notion of "inside a string", so string content must be
   
   // matched as a single atomic token: otherwise keyword literals (e.g. 'or',
   
   // 'end') occurring as substrings of the string's text would be tokenized
   
   // as those keywords instead of as plain characters.
   
STRING
   : '"' (ISO_CODE_FRAGMENT | '""' | ~ ["\r\n])* '"'
   ;

character_constant
   : CHARACTER_CONSTANT
   ;

CHARACTER_CONSTANT
   : '\'' (ISO_CODE_FRAGMENT | ~ ['\r\n]) '\''
   ;

format_effector
   : 'BS'
   | 'HT'
   | 'LF'
   | 'VT'
   | 'FF'
   | 'CR'
   ;

type
   : value_type
   | reference_type
   ;

value_type
   : arithmetic_type
   | 'boolean'
   | 'character'
   ;

arithmetic_type
   : integer_type
   | real_type
   ;

integer_type
   : 'short'? 'integer'
   ;

real_type
   : 'long'? 'real'
   ;

reference_type
   : object_reference_type
   | 'text'
   ;

object_reference_type
   : 'ref' '(' qualification ')'
   ;

qualification
   : class_identifier
   ;

expression
   : value_expression
   | reference_expression
   | designational_expression
   ;

value_expression
   : arithmetic_expression
   | boolean_expression
   | character_expression
   ;

reference_expression
   : object_expression
   | text_expression
   ;
   // variable, identifier_1, remote_identifier, subscripted_variable,
   
   // array_identifier_1, procedure_identifier_1 and function_designator are
   
   // mutually left-recursive in the standard's own grammar (identifiers can be
   
   // remote-accessed via '.', subscripted via '(...)', and those expressions
   
   // themselves can be identifiers again). ANTLR4 only auto-resolves direct
   
   // left recursion within a single rule, so they are flattened here into one
   
   // combining rule, with the standard's separate rule names kept as aliases.
   
designator
   : identifier
   | designator '.' attribute_identifier
   | designator callsuffix
   | designator 'qua' class_identifier
   ;

callsuffix
   : '(' actual_parameter (',' actual_parameter)* ')'
   ;

variable
   : designator
   ;

simple_variable_1
   : designator
   ;

subscripted_variable
   : designator
   ;

array_identifier_1
   : designator
   ;

subscript_list
   : subscript_expression (',' subscript_expression)*
   ;

subscript_expression
   : arithmetic_expression
   ;

function_designator
   : designator
   ;

procedure_identifier_1
   : designator
   ;

actual_parameter_part
   : '(' actual_parameter (',' actual_parameter)* ')'
   ;

actual_parameter
   : expression
   | designator
   ;

identifier_1
   : designator
   ;

remote_identifier
   : designator
   ;

attribute_identifier
   : identifier
   ;

boolean_expression
   : simple_boolean_expression
   | if_clause simple_boolean_expression 'else' boolean_expression
   ;

simple_boolean_expression
   : boolean_tertiary ('or' 'else' boolean_tertiary)*
   ;

boolean_tertiary
   : equivalence ('and' 'then' equivalence)*
   ;

equivalence
   : implication ('eqv' implication)*
   ;

implication
   : boolean_term ('imp' boolean_term)*
   ;

boolean_term
   : boolean_factor ('or' boolean_factor)*
   ;

boolean_factor
   : boolean_secondary ('and' boolean_secondary)*
   ;

boolean_secondary
   : 'not'? boolean_primary
   ;

boolean_primary
   : logical_value
   | variable
   | function_designator
   | relation
   | '(' boolean_expression ')'
   ;

logical_value
   : 'true'
   | 'false'
   ;

relation
   : arithmetic_relation
   | character_relation
   | text_value_relation
   | object_relation
   | object_reference_relation
   | text_reference_relation
   ;

value_relational_operator
   : '<'
   | '<='
   | '='
   | '>='
   | '>'
   | '<>'
   ;

reference_comparator
   : '=='
   | '=/='
   ;

arithmetic_relation
   : simple_arithmetic_expression value_relational_operator simple_arithmetic_expression
   ;

character_relation
   : simple_character_expression value_relational_operator simple_character_expression
   ;

text_value_relation
   : simple_text_expression value_relational_operator simple_text_expression
   ;

object_relation
   : simple_object_expression 'is' class_identifier
   | simple_object_expression 'in' class_identifier
   ;

object_reference_relation
   : simple_object_expression reference_comparator simple_object_expression
   ;

text_reference_relation
   : simple_text_expression reference_comparator simple_text_expression
   ;

arithmetic_expression
   : simple_arithmetic_expression
   | if_clause simple_arithmetic_expression 'else' arithmetic_expression
   ;

simple_arithmetic_expression
   : ('+' | '-')* term (('+' | '-') term)*
   ;

term
   : factor (('*' | '/' | '//') factor)*
   ;

factor
   : primary ('**' primary)*
   ;

primary
   : unsigned_number
   | variable
   | function_designator
   | '(' arithmetic_expression ')'
   ;

character_expression
   : simple_character_expression
   | if_clause simple_character_expression 'else' character_expression
   ;

simple_character_expression
   : character_constant
   | variable
   | function_designator
   | '(' character_expression ')'
   ;

text_expression
   : simple_text_expression
   | if_clause simple_text_expression 'else' text_expression
   ;

simple_text_expression
   : text_primary ('&' text_primary)*
   ;

text_primary
   : 'notext'
   | string
   | designator
   | '(' text_expression ')'
   ;

object_expression
   : simple_object_expression
   | if_clause simple_object_expression 'else' object_expression
   ;

simple_object_expression
   : 'none'
   | designator
   | object_generator
   | local_object
   | '(' object_expression ')'
   ;

object_generator
   : 'new' class_identifier actual_parameter_part?
   ;

local_object
   : 'this' class_identifier
   ;

qualified_object
   : designator
   ;

designational_expression
   : simple_designational_expression
   | if_clause simple_designational_expression 'else' designational_expression
   ;

simple_designational_expression
   : label
   | switch_designator
   | '(' designational_expression ')'
   ;

switch_designator
   : switch_identifier '(' subscript_expression ')'
   ;

switch_identifier
   : identifier
   ;

label
   : identifier
   ;

statement
   : (label ':')* unconditional_statement
   | (label ':')* conditional_statement
   | (label ':')* for_statement
   ;

unconditional_statement
   : assignment_statement
   | while_statement
   | goto_statement
   | procedure_statement
   | object_generator
   | connection_statement
   | block
   | compound_statement
   | dummy_statement
   | activation_statement
   ;

assignment_statement
   : value_assignment
   | reference_assignment
   ;

value_assignment
   : value_left_part ':=' value_right_part
   ;

value_left_part
   : destination
   | simple_text_expression
   ;

value_right_part
   : value_expression
   | text_expression
   | value_assignment
   ;

destination
   : variable
   | procedure_identifier
   ;

reference_assignment
   : reference_left_part ':-' reference_right_part
   ;

reference_left_part
   : destination
   ;

reference_right_part
   : reference_expression
   | reference_assignment
   ;

conditional_statement
   : if_clause (label ':')* unconditional_statement ('else' statement)?
   | if_clause (label ':')* for_statement
   ;

if_clause
   : 'if' boolean_expression 'then'
   ;

while_statement
   : 'while' boolean_expression 'do' statement
   ;

for_statement
   : for_clause statement
   ;

for_clause
   : 'for' simple_variable for_right_part 'do'
   ;

simple_variable
   : identifier
   ;

for_right_part
   : ':=' value_for_list_element (',' value_for_list_element)*
   | ':-' reference_for_list_element (',' reference_for_list_element)*
   ;

value_for_list_element
   : value_expression ('while' boolean_expression)?
   | text_expression
   | arithmetic_expression 'step' arithmetic_expression 'until' arithmetic_expression
   ;

reference_for_list_element
   : reference_expression ('while' boolean_expression)?
   ;

goto_statement
   : ('goto' | ('go' 'to')) designational_expression
   ;

procedure_statement
   : procedure_identifier_1
   ;

connection_statement
   : 'inspect' object_expression when_clause when_clause* otherwise_clause?
   | 'inspect' object_expression 'do' connection_block_2 otherwise_clause?
   ;

when_clause
   : 'when' class_identifier 'do' connection_block_1
   ;

otherwise_clause
   : 'otherwise' statement
   ;

connection_block_1
   : statement
   ;

connection_block_2
   : statement
   ;

compound_statement
   : 'begin' compound_tail
   ;

compound_tail
   : statement (';' statement)* 'end'
   ;

block
   : subblock
   | prefixed_block
   ;

subblock
   : block_head ';' compound_tail
   ;

block_head
   : 'begin' declaration (';' declaration)*
   ;

prefixed_block
   : block_prefix main_block
   ;

block_prefix
   : class_identifier actual_parameter_part?
   ;

main_block
   : block
   | compound_statement
   ;

dummy_statement
   : 'empty'
   ;

declaration
   : simple_variable_sdeclaration
   | array_declaration
   | switch_declaration
   | procedure_declaration
   | class_declaration
   | external_declaration
   ;

simple_variable_sdeclaration
   : type type_list
   ;

type_list
   : type_list_element (',' type_list_element)*
   ;

type_list_element
   : identifier
   | constant_element
   ;

array_declaration
   : type? 'array' array_segment (',' array_segment)*
   ;

array_segment
   : array_identifier (',' array_identifier)* '(' bound_pair_list ')'
   ;

array_identifier
   : identifier
   ;

bound_pair_list
   : bound_pair (',' bound_pair)*
   ;

bound_pair
   : arithmetic_expression ':' arithmetic_expression
   ;

switch_declaration
   : 'switch' switch_identifier ':=' switch_list
   ;

switch_list
   : designational_expression (',' designational_expression)* procedure_declaration
   ;

procedure_declaration
   : type? 'procedure' procedure_heading ';' procedure_body
   ;

procedure_heading
   : procedure_identifier (formal_parameter_part ';' mode_part? specification_part)?
   ;

procedure_body
   : statement
   ;

procedure_identifier
   : identifier
   ;

formal_parameter_part
   : '(' formal_parameter (',' formal_parameter)* ')'
   ;

formal_parameter
   : identifier
   ;

specification_part
   : specifier identifier_list (';' specifier identifier_list)*
   ;

specifier
   : type ('array' | 'procedure')?
   | 'label'
   | 'switch'
   ;

mode_part
   : name_part value_part?
   | value_part name_part?
   ;

name_part
   : 'name' identifier_list
   ;

value_part
   : 'value' identifier_list
   ;

identifier_list
   : identifier (',' identifier)*
   ;

class_declaration
   : prefix? main_part
   ;

prefix
   : class_identifier
   ;

main_part
   : 'class' class_identifier (formal_parameter_part ';' value_part? specification_part)? ';' (protection_part ';')? (virtual_part ';')* class_body
   ;

class_identifier
   : identifier
   ;

class_body
   : statement
   | split_body
   ;

split_body
   : initial_operations inner_part final_operations
   ;

initial_operations
   : ('begin' | block_head ';') (statement ';')*
   ;

inner_part
   : (label ':')* 'inner'
   ;

final_operations
   : 'end'
   | ';' compound_tail
   ;

virtual_part
   : 'virtual' ':' virtual_spec ';' (virtual_spec ';')*
   ;

virtual_spec
   : specifier identifier_list
   | 'procedure' procedure_identifier procedure_specification
   ;

protection_part
   : protection_specification (';' protection_specification)*
   ;

protection_specification
   : 'hidden' identifier_list
   | 'protected' identifier_list
   | 'hidden' 'protected' identifier_list
   | 'protected' 'hidden' identifier_list
   ;

constant_element
   : identifier '=' value_expression
   | identifier '=' string
   ;

simula_source_module
   : external_head? (program | procedure_declaration | class_declaration)
   ;

external_head
   : external_declaration ';' (external_declaration ';')*
   ;

external_declaration
   : external_procedure_declaration
   | external_class_declaration
   ;

program
   : statement
   ;

external_procedure_declaration
   : 'external' kind? type? 'procedure' external_list
   | 'external' kind 'procedure' external_item procedure_specification
   ;

kind
   : identifier
   ;

procedure_specification
   : 'is' procedure_declaration
   ;

external_class_declaration
   : 'external' 'class' external_list
   ;

external_list
   : external_item (',' external_item)*
   ;

external_item
   : identifier ('=' external_identification)?
   ;

external_identification
   : string
   ;

activation_statement
   : activation_clause scheduling_clause?
   ;

activation_clause
   : activator object_expression
   ;

activator
   : 'activate'
   | 'reactivate'
   ;

scheduling_clause
   : timing_clause
   | ('before' | 'after') object_expression
   ;

timing_clause
   : simple_timing_clause 'prior'?
   ;

simple_timing_clause
   : ('at' | 'delay') arithmetic_expression
   ;
   // An identifier must be matched as a single atomic token (not reassembled
   
   // in the parser from individual LETTER/DIGIT tokens): otherwise a keyword
   
   // occurring as a substring of a longer identifier (e.g. 'text' inside
   
   // "outtext", or 'in' inside "outint") would be lexed as that keyword
   
   // instead of as part of the identifier. It must also be declared after all
   
   // the keyword literals above, so that an exact keyword match (e.g.
   
   // 'integer') wins the lexer's same-length tie-break over IDENTIFIER.
   
IDENTIFIER
   : LETTER (LETTER | DIGIT | '_')*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

