/*
Ada 95 grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
parser grammar Ada95Parser;

options { tokenVocab = Ada95Lexer; }

/*
2 - Lexical Elements
*/

identifier
   : IDENTIFIER_
   ;

numeric_literal
   : NUMERIC_LITERAL_
   ;

character_literal
   : CHARACTER_LITERAL
   ;

string_literal
   : STRING_LITERAL_
   ;
/*
3 - Declarations and Types
*/
   
   
basic_declaration
   : type_declaration
   | subtype_declaration
   | object_declaration
   | number_declaration
   | subprogram_declaration
   | abstract_subprogram_declaration
   | package_declaration
   | renaming_declaration
   | exception_declaration
   | generic_declaration
   | generic_instantiation
   ;

defining_identifier
   : identifier
   ;

type_declaration
   : full_type_declaration
   | incomplete_type_declaration
   | private_type_declaration
   | private_extension_declaration
   ;

full_type_declaration
   : TYPE defining_identifier known_discriminant_part? IS type_definition ';'
   | task_type_declaration
   | protected_type_declaration
   ;

type_definition
   : enumeration_type_definition
   | integer_type_definition
   | real_type_definition
   | array_type_definition
   | record_type_definition
   | access_type_definition
   | derived_type_definition
   ;

subtype_declaration
   : SUBTYPE defining_identifier IS subtype_indication ';'
   ;

subtype_indication
   : subtype_mark = name constraint?
   ;

constraint
   : scalar_constraint
   | composite_constraint
   ;

scalar_constraint
   : range_constraint
   | digits_constraint
   | delta_constraint
   ;

composite_constraint
   : index_constraint
   | discriminant_constraint
   ;

object_declaration
   : defining_identifier_list ':' ALIASED? CONSTANT? subtype_indication (':=' expression)? ';'
   | defining_identifier_list ':' ALIASED? CONSTANT? array_type_definition (':=' expression)? ';'
   | single_task_declaration
   | single_protected_declaration
   ;

defining_identifier_list
   : defining_identifier (',' defining_identifier)*
   ;

number_declaration
   : defining_identifier_list ':' CONSTANT ':=' static_expression = expression ';'
   ;

derived_type_definition
   : ABSTRACT? NEW parent_subtype_indication = subtype_indication record_extension_part?
   ;

range_constraint
   : RANGE_ range
   ;

range
   : range_attribute_reference
   | simple_expression '..' simple_expression
   ;

enumeration_type_definition
   : '(' enumeration_literal_specification (',' enumeration_literal_specification)* ')'
   ;

enumeration_literal_specification
   : defining_identifier
   | defining_character_literal
   ;

defining_character_literal
   : character_literal
   ;

integer_type_definition
   : signed_integer_type_definition
   | modular_type_definition
   ;

signed_integer_type_definition
   : RANGE_ static_simple_expression = simple_expression '..' static_simple_expression = simple_expression
   ;

modular_type_definition
   : MOD static_expression = expression
   ;

real_type_definition
   : floating_point_definition
   | fixed_point_definition
   ;

floating_point_definition
   : DIGITS static_expression = expression real_range_specification?
   ;

real_range_specification
   : range static_simple_expression = simple_expression '..' static_simple_expression = simple_expression
   ;

fixed_point_definition
   : ordinary_fixed_point_definition
   | decimal_fixed_point_definition
   ;

ordinary_fixed_point_definition
   : DELTA static_expression = expression real_range_specification
   ;

decimal_fixed_point_definition
   : DELTA static_expression = expression DIGITS static_expression = expression real_range_specification?
   ;

digits_constraint
   : DIGITS static_expression = expression range_constraint?
   ;

array_type_definition
   : unconstrained_array_definition
   | constrained_array_definition
   ;

unconstrained_array_definition
   : ARRAY '(' index_subtype_definition (',' index_subtype_definition)* ')' OF component_definition
   ;

index_subtype_definition
   : subtype_mark = name range '<>'
   ;

constrained_array_definition
   : ARRAY '(' discrete_subtype_definition (',' discrete_subtype_definition)* ')' OF component_definition
   ;

discrete_subtype_definition
   : discrete_subtype_indication = subtype_indication
   | range
   ;

component_definition
   : ALIASED? subtype_indication
   ;

index_constraint
   : '(' discrete_range (',' discrete_range)* ')'
   ;

discrete_range
   : discrete_subtype_indication = subtype_indication
   | range
   ;

discriminant_part
   : unknown_discriminant_part
   | known_discriminant_part
   ;

unknown_discriminant_part
   : '(' '<>' ')'
   ;

known_discriminant_part
   : '(' discriminant_specification (';' discriminant_specification)* ')'
   ;

discriminant_specification
   : defining_identifier_list ':' subtype_mark = name (':=' default_expression)?
   | defining_identifier_list ':' access_definition (':=' default_expression)?
   ;

default_expression
   : expression
   ;

discriminant_constraint
   : '(' discriminant_association (',' discriminant_association)* ')'
   ;

discriminant_association
   : (discriminant_selector_name = selector_name ('|' discriminant_selector_name = selector_name)* '=>')? expression
   ;

record_type_definition
   : (ABSTRACT? TAGGED)? LIMITED? record_definition
   ;

record_definition
   : RECORD component_list END RECORD
   | NULL_ RECORD
   ;

component_list
   : component_item component_item*
   | component_item* variant_part
   | NULL_ ';'
   ;

component_item
   : component_declaration
   | aspect_clause
   ;

component_declaration
   : defining_identifier_list ':' component_definition (':=' default_expression)? ';'
   ;

variant_part
   : CASE discriminant_direct_name = direct_name IS variant variant* END CASE ';'
   ;

variant
   : WHEN discrete_choice_list '=>' component_list
   ;

discrete_choice_list
   : discrete_choice ('|' discrete_choice)*
   ;

discrete_choice
   : expression
   | discrete_range
   | OTHERS
   ;

record_extension_part
   : WITH record_definition
   ;

access_type_definition
   : access_to_object_definition
   | access_to_subprogram_definition
   ;

access_to_object_definition
   : ACCESS general_access_modifier? subtype_indication
   ;

general_access_modifier
   : ALL
   | CONSTANT
   ;

access_to_subprogram_definition
   : ACCESS PROTECTED? PROCEDURE parameter_profile
   | ACCESS PROTECTED? FUNCTION parameter_and_result_profile
   ;

access_definition
   : ACCESS subtype_mark = name
   ;

incomplete_type_declaration
   : TYPE defining_identifier discriminant_part? ';'
   ;

declarative_part
   : declarative_item*
   ;

declarative_item
   : basic_declarative_item
   | body
   ;

basic_declarative_item
   : basic_declaration
   | aspect_clause
   | use_clause
   ;

body
   : proper_body
   | body_stub
   ;

proper_body
   : subprogram_body
   | package_body
   | task_body
   | protected_body
   ;
/*
4 - Names and Expressions
*/
   
   
name
   : direct_name
   | name '.' ALL
   | prefix = name '(' expression (',' expression)* ')' //indexed_component
   | prefix = name '(' discrete_range ')' //slice
   | prefix = name '.' selector_name //selected_component
   | prefix = name SQ attribute_designator //attribute_reference
   | subtype_mark = name '(' expression ')'
   | subtype_mark = name '(' name ')'
   | function_prefix = name actual_parameter_part
   | character_literal
   ;

direct_name
   : identifier
   | operator_symbol
   ;

selector_name
   : identifier
   | character_literal
   | operator_symbol
   ;

attribute_designator
   : identifier ('(' static_expression = expression ')')?
   | ACCESS
   | DELTA
   | DIGITS
   ;

range_attribute_reference
   : prefix = name SQ range_attribute_designator
   ;

range_attribute_designator
   : RANGE_ ('(' static_expression = expression ')')?
   ;

aggregate
   : record_aggregate
   | extension_aggregate
   | array_aggregate
   ;

record_aggregate
   : '(' record_component_association_list ')'
   ;

record_component_association_list
   : record_component_association (',' record_component_association)*
   | NULL_ RECORD
   ;

record_component_association
   : (component_choice_list '=>')? expression
   ;

component_choice_list
   : component_selector_name = selector_name ('|' component_selector_name = selector_name)*
   | OTHERS
   ;

extension_aggregate
   : '(' ancestor_part WITH record_component_association_list ')'
   ;

ancestor_part
   : expression
   | subtype_mark = name
   ;

array_aggregate
   : positional_array_aggregate
   | named_array_aggregate
   ;

positional_array_aggregate
   : '(' expression ',' expression (',' expression)* ')'
   | '(' expression (',' expression)* ',' OTHERS '=>' expression ')'
   ;

named_array_aggregate
   : '(' array_component_association (',' array_component_association)* ')'
   ;

array_component_association
   : discrete_choice_list '=>' expression
   ;

expression
   : relation (AND relation)*
   | relation (AND THEN relation)*
   | relation (OR relation)*
   | relation (OR ELSE relation)*
   | relation (XOR relation)*
   ;

relation
   : simple_expression (relational_operator simple_expression)?
   | simple_expression NOT? IN range
   | simple_expression NOT? IN subtype_mark = name
   ;

simple_expression
   : unary_adding_operator? term (binary_adding_operator term)*
   ;

term
   : factor (multiplying_operator factor)*
   ;

factor
   : primary ('**' primary)?
   | ABS primary
   | NOT primary
   ;

primary
   : numeric_literal
   | NULL_
   | string_literal
   | aggregate
   | name
   | qualified_expression
   | allocator
   | '(' expression ')'
   ;

logical_operator
   : AND
   | OR
   | XOR
   ;

relational_operator
   : '='
   | '/='
   | '<'
   | '<='
   | '>'
   | '>='
   ;

binary_adding_operator
   : '+'
   | '-'
   | '&'
   ;

unary_adding_operator
   : '+'
   | '-'
   ;

multiplying_operator
   : '*'
   | '/'
   | MOD
   | REM
   ;

highest_precedence_operator
   : '**'
   | ABS
   | NOT
   ;

qualified_expression
   : subtype_mark = name SQ '(' expression ')'
   | subtype_mark = name SQ aggregate
   ;

allocator
   : NEW subtype_indication
   | NEW qualified_expression
   ;
/*
5 - Statements
*/
   
   
sequence_of_statements
   : statement statement*
   ;

statement
   : label* simple_statement
   | label* compound_statement
   ;

simple_statement
   : null_statement
   | assignment_statement
   | exit_statement
   | goto_statement
   | procedure_call_statement
   | return_statement
   | entry_call_statement
   | requeue_statement
   | delay_statement
   | abort_statement
   | raise_statement
   | code_statement
   ;

compound_statement
   : if_statement
   | case_statement
   | loop_statement
   | block_statement
   | accept_statement
   | select_statement
   ;

null_statement
   : NULL_ ';'
   ;

label
   : '<<' label_statement_identifier = statement_identifier '>>'
   ;

statement_identifier
   : direct_name
   ;

assignment_statement
   : variable_name = name ':=' expression ';'
   ;

if_statement
   : IF condition THEN sequence_of_statements (ELSIF condition THEN sequence_of_statements)* (ELSE sequence_of_statements)? END IF ';'
   ;

condition
   : boolean_expression
   ;

case_statement
   : CASE expression IS case_statement_alternative case_statement_alternative* END CASE ';'
   ;

case_statement_alternative
   : WHEN discrete_choice_list '=>' sequence_of_statements
   ;

loop_statement
   : (loop_statement_identifier = statement_identifier ':')? iteration_scheme? LOOP sequence_of_statements END LOOP loop_identifier = identifier? ';'
   ;

iteration_scheme
   : WHILE condition
   | FOR loop_parameter_specification
   ;

loop_parameter_specification
   : defining_identifier IN REVERSE? discrete_subtype_definition
   ;

block_statement
   : (block_statement_identifier = statement_identifier ':')? (DECLARE declarative_part)? BEGIN handled_sequence_of_statements END block_identifier = identifier? ';'
   ;

exit_statement
   : EXIT loop_name = name? (WHEN condition)? ';'
   ;

goto_statement
   : GOTO label_name = name ';'
   ;
/*
6 - Subprograms
*/
   
   
subprogram_declaration
   : subprogram_specification ';'
   ;

abstract_subprogram_declaration
   : subprogram_specification IS ABSTRACT ';'
   ;

subprogram_specification
   : PROCEDURE defining_program_unit_name parameter_profile
   | FUNCTION defining_designator parameter_and_result_profile
   ;

designator
   : (parent_unit_name '.')? identifier
   | operator_symbol
   ;

defining_designator
   : defining_program_unit_name
   | defining_operator_symbol
   ;

defining_program_unit_name
   : (parent_unit_name '.')? defining_identifier
   ;

operator_symbol
   : string_literal
   ;

defining_operator_symbol
   : operator_symbol
   ;

parameter_profile
   : formal_part?
   ;

parameter_and_result_profile
   : formal_part? RETURN subtype_mark = name
   ;

formal_part
   : '(' parameter_specification (';' parameter_specification)* ')'
   ;

parameter_specification
   : defining_identifier_list ':' mode_ subtype_mark = name (':=' default_expression)?
   | defining_identifier_list ':' access_definition (':=' default_expression)?
   ;

mode_
   : IN? OUT?
   ;

subprogram_body
   : subprogram_specification IS declarative_part BEGIN handled_sequence_of_statements END designator? ';'
   ;

procedure_call_statement
   : procedure_name = name ';'
   | procedure_prefix = name actual_parameter_part ';'
   ;

actual_parameter_part
   : '(' parameter_association (',' parameter_association)* ')'
   ;

parameter_association
   : (formal_parameter_selector_name = selector_name '=>')? explicit_actual_parameter
   ;

explicit_actual_parameter
   : expression
   | variable_name = name
   ;

return_statement
   : RETURN expression? ';'
   ;
/*
7 - Packages
*/
   
   
package_declaration
   : package_specification ';'
   ;

package_specification
   : PACKAGE defining_program_unit_name IS basic_declarative_item* (PRIVATE basic_declarative_item*)? END ((parent_unit_name '.')? identifier)?
   ;

package_body
   : PACKAGE BODY_ defining_program_unit_name IS declarative_part (BEGIN handled_sequence_of_statements)? END ((parent_unit_name '.')? identifier)? ';'
   ;

private_type_declaration
   : TYPE defining_identifier discriminant_part? IS (ABSTRACT? TAGGED)? LIMITED? PRIVATE ';'
   ;

private_extension_declaration
   : TYPE defining_identifier discriminant_part? IS ABSTRACT? NEW ancestor_subtype_indication = subtype_indication WITH PRIVATE ';'
   ;
/*
8 - Visibility Rules
*/
   
   
use_clause
   : use_package_clause
   | use_type_clause
   ;

use_package_clause
   : USE package_name = name (',' package_nam = name)* ';'
   ;

use_type_clause
   : USE TYPE subtype_mark = name (',' subtype_mark = name)* ';'
   ;

renaming_declaration
   : object_renaming_declaration
   | exception_renaming_declaration
   | package_renaming_declaration
   | subprogram_renaming_declaration
   | generic_renaming_declaration
   ;

object_renaming_declaration
   : defining_identifier ':' subtype_mark = name RENAMES object_name = name ';'
   ;

exception_renaming_declaration
   : defining_identifier ':' EXCEPTION RENAMES exception_name = name ';'
   ;

package_renaming_declaration
   : PACKAGE defining_program_unit_name RENAMES package_name = name ';'
   ;

subprogram_renaming_declaration
   : subprogram_specification RENAMES callable_entity_name = name ';'
   ;

generic_renaming_declaration
   : GENERIC PACKAGE defining_program_unit_name RENAMES generic_package_name = name ';'
   | GENERIC PROCEDURE defining_program_unit_name RENAMES generic_procedure_name = name ';'
   | GENERIC FUNCTION defining_program_unit_name RENAMES generic_function_name = name ';'
   ;
/*
9 - Tasks and Synchronization
*/
   
   
task_type_declaration
   : TASK TYPE defining_identifier known_discriminant_part? (IS task_definition)? ';'
   ;

single_task_declaration
   : TASK defining_identifier (IS task_definition)? ';'
   ;

task_definition
   : task_item* (PRIVATE task_item*)? END task_identifier = identifier?
   ;

task_item
   : entry_declaration
   | aspect_clause
   ;

task_body
   : TASK BODY_ defining_identifier IS declarative_part BEGIN handled_sequence_of_statements END task_identifier = identifier? ';'
   ;

protected_type_declaration
   : PROTECTED TYPE defining_identifier known_discriminant_part? IS protected_definition ';'
   ;

single_protected_declaration
   : PROTECTED defining_identifier IS protected_definition ';'
   ;

protected_definition
   : protected_operation_declaration* (PRIVATE protected_element_declaration*)? END protected_identifier = identifier?
   ;

protected_operation_declaration
   : subprogram_declaration
   | entry_declaration
   | aspect_clause
   ;

protected_element_declaration
   : protected_operation_declaration
   | component_declaration
   ;

protected_body
   : PROTECTED BODY_ defining_identifier IS protected_operation_item* END protected_identifier = identifier? ';'
   ;

protected_operation_item
   : subprogram_declaration
   | subprogram_body
   | entry_body
   | aspect_clause
   ;

entry_declaration
   : ENTRY defining_identifier ('(' discrete_subtype_definition ')')? parameter_profile ';'
   ;

accept_statement
   : ACCEPT_ entry_direct_name = direct_name ('(' entry_index ')')? parameter_profile (DO handled_sequence_of_statements END entry_identifier = identifier?)? ';'
   ;

entry_index
   : expression
   ;

entry_body
   : ENTRY defining_identifier entry_body_formal_part entry_barrier IS declarative_part BEGIN handled_sequence_of_statements END entry_identifier = identifier? ';'
   ;

entry_body_formal_part
   : ('(' entry_index_specification ')')? parameter_profile
   ;

entry_barrier
   : WHEN condition
   ;

entry_index_specification
   : FOR defining_identifier IN discrete_subtype_definition
   ;

entry_call_statement
   : entry_name = name actual_parameter_part? ';'
   ;

requeue_statement
   : REQUEUE entry_name = name (WITH ABORT)? ';'
   ;

delay_statement
   : delay_until_statement
   | delay_relative_statement
   ;

delay_until_statement
   : DELAY UNTIL delay_expression = expression ';'
   ;

delay_relative_statement
   : DELAY delay_expression = expression ';'
   ;

select_statement
   : selective_accept
   | timed_entry_call
   | conditional_entry_call
   | asynchronous_select
   ;

selective_accept
   : SELECT guard? select_alternative (OR guard? select_alternative)* (ELSE sequence_of_statements)? END SELECT ';'
   ;

guard
   : WHEN condition '=>'
   ;

select_alternative
   : accept_alternative
   | delay_alternative
   | terminate_alternative
   ;

accept_alternative
   : accept_statement sequence_of_statements?
   ;

delay_alternative
   : delay_statement sequence_of_statements?
   ;

terminate_alternative
   : TERMINATE ';'
   ;

timed_entry_call
   : SELECT entry_call_alternative OR delay_alternative END SELECT ';'
   ;

entry_call_alternative
   : entry_call_statement sequence_of_statements?
   ;

conditional_entry_call
   : SELECT entry_call_alternative ELSE sequence_of_statements END SELECT ';'
   ;

asynchronous_select
   : SELECT triggering_alternative THEN ABORT abortable_part END SELECT ';'
   ;

triggering_alternative
   : triggering_statement sequence_of_statements?
   ;

triggering_statement
   : entry_call_statement
   | delay_statement
   ;

abortable_part
   : sequence_of_statements
   ;

abort_statement
   : ABORT task_name = name (',' task_name = name)* ';'
   ;
/*
10 - Program Structure and Compilation Issues
*/
   
   
compilation
   : compilation_unit* EOF
   ;

compilation_unit
   : context_clause library_item
   | context_clause subunit
   ;

library_item
   : PRIVATE? library_unit_declaration
   | library_unit_body
   | PRIVATE? library_unit_renaming_declaration
   ;

library_unit_declaration
   : subprogram_declaration
   | package_declaration
   | generic_declaration
   | generic_instantiation
   ;

library_unit_renaming_declaration
   : package_renaming_declaration
   | generic_renaming_declaration
   | subprogram_renaming_declaration
   ;

library_unit_body
   : subprogram_body
   | package_body
   ;

parent_unit_name
   : name
   ;

context_clause
   : context_item*
   ;

context_item
   : with_clause
   | use_clause
   ;

with_clause
   : WITH library_unit_name = name (',' library_unit_name = name)* ';'
   ;

body_stub
   : subprogram_body_stub
   | package_body_stub
   | task_body_stub
   | protected_body_stub
   ;

subprogram_body_stub
   : subprogram_specification IS SEPARATE ';'
   ;

package_body_stub
   : PACKAGE BODY_ defining_identifier IS SEPARATE ';'
   ;

task_body_stub
   : TASK BODY_ defining_identifier IS SEPARATE ';'
   ;

protected_body_stub
   : PROTECTED BODY_ defining_identifier IS SEPARATE ';'
   ;

subunit
   : SEPARATE '(' parent_unit_name ')' proper_body
   ;
/*
11 - Exceptions
*/
   
   
exception_declaration
   : defining_identifier_list ':' EXCEPTION ';'
   ;

handled_sequence_of_statements
   : sequence_of_statements (EXCEPTION exception_handler exception_handler*)?
   ;

exception_handler
   : WHEN (choice_parameter_specification ':')? exception_choice ('|' exception_choice)* '=>' sequence_of_statements
   ;

choice_parameter_specification
   : defining_identifier
   ;

exception_choice
   : exception_name = name
   | OTHERS
   ;

raise_statement
   : RAISE exception_name = name? ';'
   ;
/*
12 - Generic Units
*/
   
   
generic_declaration
   : generic_subprogram_declaration
   | generic_package_declaration
   ;

generic_subprogram_declaration
   : generic_formal_part subprogram_specification ';'
   ;

generic_package_declaration
   : generic_formal_part package_specification ';'
   ;

generic_formal_part
   : GENERIC (generic_formal_parameter_declaration | use_clause)*
   ;

generic_formal_parameter_declaration
   : formal_object_declaration
   | formal_type_declaration
   | formal_subprogram_declaration
   | formal_package_declaration
   ;

generic_instantiation
   : PACKAGE defining_program_unit_name IS NEW generic_package_name = name generic_actual_part? ';'
   | PROCEDURE defining_program_unit_name IS NEW generic_procedure_name = name generic_actual_part? ';'
   | FUNCTION defining_designator IS NEW generic_function_name = name generic_actual_part? ';'
   ;

generic_actual_part
   : '(' generic_association (',' generic_association)* ')'
   ;

generic_association
   : (generic_formal_parameter_selector_name = selector_name '=>')? explicit_generic_actual_parameter
   ;

explicit_generic_actual_parameter
   : expression
   | variable_name = name
   | subprogram_name = name
   | entry_name = name
   | subtype_mark = name
   | package_instance_name = name
   ;

formal_object_declaration
   : defining_identifier_list ':' mode_ subtype_mark = name (':=' default_expression)? ';'
   ;

formal_type_declaration
   : TYPE defining_identifier discriminant_part? IS formal_type_definition ';'
   ;

formal_type_definition
   : formal_private_type_definition
   | formal_derived_type_definition
   | formal_discrete_type_definition
   | formal_signed_integer_type_definition
   | formal_modular_type_definition
   | formal_floating_point_definition
   | formal_ordinary_fixed_point_definition
   | formal_decimal_fixed_point_definition
   | formal_array_type_definition
   | formal_access_type_definition
   ;

formal_private_type_definition
   : (ABSTRACT? TAGGED)? LIMITED? PRIVATE
   ;

formal_derived_type_definition
   : ABSTRACT? NEW subtype_mark = name (WITH PRIVATE)?
   ;

formal_discrete_type_definition
   : '(' '<>' ')'
   ;

formal_signed_integer_type_definition
   : RANGE_ '<>'
   ;

formal_modular_type_definition
   : MOD '<>'
   ;

formal_floating_point_definition
   : DIGITS '<>'
   ;

formal_ordinary_fixed_point_definition
   : DELTA '<>'
   ;

formal_decimal_fixed_point_definition
   : DELTA '<>' DIGITS '<>'
   ;

formal_array_type_definition
   : array_type_definition
   ;

formal_access_type_definition
   : access_type_definition
   ;

formal_subprogram_declaration
   : WITH subprogram_specification (IS subprogram_default)? ';'
   ;

subprogram_default
   : default_name
   | '<>'
   ;

default_name
   : name
   ;

formal_package_declaration
   : WITH PACKAGE defining_identifier IS NEW generic_package_name = name formal_package_actual_part ';'
   ;

formal_package_actual_part
   : '(' '<>' ')'
   | generic_actual_part?
   ;
/*
13 - Representation Issues
*/
   
   
aspect_clause
   : attribute_definition_clause
   | enumeration_representation_clause
   | record_representation_clause
   | at_clause
   ;

local_name
   : direct_name
   | direct_name SQ attribute_designator
   | library_unit_name = name
   ;

attribute_definition_clause
   : FOR local_name SQ attribute_designator USE expression ';'
   | FOR local_name SQ attribute_designator USE name ';'
   ;

enumeration_representation_clause
   : FOR first_subtype_local_name = name USE enumeration_aggregate ';'
   ;

enumeration_aggregate
   : array_aggregate
   ;

record_representation_clause
   : FOR first_subtype_local_name = name USE RECORD mod_clause? component_clause* END RECORD ';'
   ;

component_clause
   : component_local_name = name AT position RANGE_ first_bit '..' last_bit ';'
   ;

position
   : static_expression = expression
   ;

first_bit
   : static_simple_expression = simple_expression
   ;

last_bit
   : static_simple_expression = simple_expression
   ;

code_statement
   : qualified_expression ';'
   ;

restriction
   : restriction_identifier = identifier
   | restriction_parameter_identifier = identifier '=>' expression
   ;
/*
J
*/
   
   
at_clause
   : FOR direct_name USE AT expression ';'
   ;

delta_constraint
   : DELTA static_expression = expression range_constraint?
   ;

mod_clause
   : AT MOD static_expression = expression ';'
   ;

boolean_expression
   : expression
   ;
