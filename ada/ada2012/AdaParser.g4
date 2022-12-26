/*
Ada 2012 grammar.
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

parser grammar AdaParser;

options { tokenVocab = AdaLexer; }

compilation
    : compilation_unit* EOF
    ;

compilation_unit
    : context_item* (library_item | subunit)
    ;

context_item
    : with_clause
    | use_clause
    ;

with_clause
    : limited_with_clause
    | nonlimited_with_clause
    ;

limited_with_clause
    : LIMITED PRIVATE? WITH name (',' name)* SEMI
    ;

nonlimited_with_clause
    : PRIVATE? WITH name (',' name)* SEMI
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

subunit
    : SEPARATE '(' name ')' proper_body
    ;

basic_declaration
    : type_declaration
    | subtype_declaration
    | object_declaration
    | number_declaration
    | subprogram_declaration
    | abstract_subprogram_declaration
    | null_procedure_declaration
    | expression_function_declaration
    | package_declaration
    | renaming_declaration
    | exception_declaration
    | generic_declaration
    | generic_instantiation
    ;

type_declaration
    : full_type_declaration
    | incomplete_type_declaration
    | private_type_declaration
    | private_extension_declaration
    ;

private_type_declaration
    : TYPE defining_identifier discriminant_part? IS (ABSTRACT? TAGGED)? LIMITED? PRIVATE
        aspect_specification? SEMI
    ;

private_extension_declaration
    : TYPE defining_identifier discriminant_part? IS
        ABSTRACT? (LIMITED | SYNCHRONIZED) NEW ancestor_subtype_indication
        (AND interface_list)? WITH PRIVATE
        aspect_specification? SEMI
    ;

ancestor_subtype_indication
    : subtype_indication
    ;

full_type_declaration
    : TYPE defining_identifier known_discriminant_part? IS type_definition aspect_specification? SEMI
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
    | interface_type_definition
    ;

task_type_declaration
    : TASK TYPE defining_identifier known_discriminant_part?
        aspect_specification? (IS  (NEW interface_list WITH)? task_definition)? SEMI
    ;

single_task_declaration
    : TASK defining_identifier aspect_specification? (IS (NEW interface_list WITH)? task_definition)? SEMI
    ;

task_definition
    : task_item* ( PRIVATE task_item* )? END task_identifier?
    ;

task_item
    : entry_declaration
    | aspect_clause
    ;

task_body
    : TASK BODY_ defining_identifier aspect_specification? IS declarative_part
        BEGIN handled_sequence_of_statements END task_identifier? SEMI
    ;

protected_type_declaration
    : PROTECTED TYPE defining_identifier known_discriminant_part?
        aspect_specification? IS (NEW interface_list WITH)? protected_definition SEMI
    ;

single_protected_declaration
    : PROTECTED defining_identifier aspect_specification? IS (NEW interface_list WITH)? protected_definition SEMI
    ;

protected_definition
    : protected_operation_declaration*
        ( PRIVATE protected_element_declaration* )?
        END protected_identifier?
    ;

protected_identifier
    : id_
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
    : PROTECTED BODY_ defining_identifier aspect_specification? IS protected_operation_item* END protected_identifier? SEMI
    ;

protected_operation_item
    : subprogram_declaration
    | subprogram_body
    | entry_body
    | aspect_clause
    ;

defining_identifier
    : id_
    ;

defining_identifier_list
    : defining_identifier (',' defining_identifier)*
    ;

subtype_declaration
    : SUBTYPE defining_identifier IS subtype_indication aspect_specification?
    ;

subtype_indication
    : null_exclusion? subtype_mark constraint?
    ;

id_
    : IDENTIFIER
    ;

subtype_mark
    : id_
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

delta_constraint
    : DELTA expression range_constraint?
    ;

composite_constraint
    : index_constraint
    | discriminant_constraint
    ;

digits_constraint
    : DIGITS expression range_constraint?
    ;

range_constraint
    : RANGE_ range
    ;

index_constraint
    : '(' discrete_range (',' discrete_range)* ')'
    ;

discrete_range
    : discrete_subtype_indication
    | range
    ;

range
    : range_attribute_reference
    | simple_expression DOTDOT simple_expression
    ;

discriminant_part
    : unknown_discriminant_part
    | known_discriminant_part
    ;

unknown_discriminant_part
    : '(' BOX ')'
    ;

known_discriminant_part
    : '(' discriminant_specification (SEMI discriminant_specification)* ')'
    ;

discriminant_specification
    : defining_identifier_list COLON null_exclusion? subtype_mark assign_expression?
    | defining_identifier_list COLON access_definition assign_expression?
    ;

default_expression
    : expression
    ;

discriminant_constraint
    : '(' discriminant_association (',' discriminant_association)* ')'
    ;

discriminant_association
    : (discriminant_selector_name (VL discriminant_selector_name)* ARROW)? expression
    ;

discriminant_selector_name
    : selector_name
    ;

record_type_definition
    : (ABSTRACT? TAGGED)? LIMITED? record_definition
    ;

record_definition
    : RECORD component_list END RECORD
    | NULL_ RECORD
    ;

component_list
    : component_item+
    | component_item* variant_part
    | NULL_ SEMI
    ;

component_item
    : component_declaration
    | aspect_clause
    ;

component_declaration
    : defining_identifier_list COLON component_definition (ASSIGN default_expression)? aspect_specification? SEMI
    ;

variant_part
    : CASE discriminant_direct_name IS variant+ END CASE SEMI
    ;

discriminant_direct_name
    : direct_name
    ;

variant
    : WHEN discrete_choice_list ARROW component_list
    ;

discrete_choice_list
    : discrete_choice (VL discrete_choice)*
    ;

discrete_choice
    : choice_expression
    | discrete_subtype_indication
    | range
    | OTHERS
    ;

discrete_subtype_indication
    : subtype_indication
    ;

record_extension_part
    : WITH record_definition
    ;

aspect_specification
    : WITH aspect_mark (ARROW aspect_definition)? (',' aspect_mark (ARROW aspect_definition)?)*
    ;

aspect_mark
    : aspect_identifier (SQ id_)? //TODO Class or id_
    ;

aspect_identifier
    : id_
    ;

aspect_definition
    : name
    | expression
    | id_
    ;

attribute_definition_clause
    : FOR local_name SQ attribute_designator USE expression SEMI
    | FOR local_name SQ attribute_designator USE name SEMI
    ;

object_declaration
    : defining_identifier_list COLON ALIASED? CONSTANT? subtype_indication assign_expression? aspect_specification? SEMI
    | defining_identifier_list COLON ALIASED? CONSTANT? access_definition assign_expression? aspect_specification? SEMI
    | defining_identifier_list COLON ALIASED? CONSTANT? array_type_definition assign_expression? aspect_specification? SEMI
    | single_task_declaration
    | single_protected_declaration
    ;

task_identifier
    : id_
    ;

entry_declaration
    : overriding_indicator? ENTRY defining_identifier ('('discrete_subtype_definition')')? formal_part? //parameter_profile
      aspect_specification? SEMI
    ;

accept_statement
    : ACCEPT_ entry_direct_name ('('entry_index')')? formal_part? /*parameter_profile*/
        (DO handled_sequence_of_statements END entry_identifier? )? SEMI
    ;

entry_direct_name
    : direct_name
    ;

entry_index
    : expression
    ;

entry_body
    : ENTRY defining_identifier entry_body_formal_part  entry_barrier IS declarative_part
        BEGIN handled_sequence_of_statements END entry_identifier? SEMI
    ;

entry_identifier
    : id_
    ;

entry_body_formal_part
    : ('('entry_index_specification')')? formal_part? //parameter_profile
    ;

entry_barrier
    : WHEN condition
    ;

entry_index_specification
    : FOR defining_identifier IN discrete_subtype_definition
    ;

derived_type_definition
    : ABSTRACT? LIMITED? NEW parent_subtype_indication ( (AND interface_list)? record_extension_part )?
    ;

parent_subtype_indication
    : subtype_indication
    ;

enumeration_type_definition
    : '(' enumeration_literal_specification (',' enumeration_literal_specification)* ')'
    ;

enumeration_literal_specification
    : defining_identifier
    | defining_character_literal
    ;

integer_type_definition
    : signed_integer_type_definition
    | modular_type_definition
    ;

signed_integer_type_definition
    : RANGE_ expression '..' expression
    ;

modular_type_definition
    : MOD expression
    ;

real_type_definition
    : floating_point_definition
    | fixed_point_definition
    ;

floating_point_definition
    : DIGITS expression real_range_specification?
    ;

real_range_specification
    : RANGE_ expression DOTDOT expression
    ;

fixed_point_definition
    : ordinary_fixed_point_definition
    | decimal_fixed_point_definition
    ;

ordinary_fixed_point_definition
    : DELTA expression real_range_specification
    ;

decimal_fixed_point_definition
    : DELTA expression DIGITS expression real_range_specification?
    ;

array_type_definition
    : unconstrained_array_definition
    | constrained_array_definition
    ;

unconstrained_array_definition
    : ARRAY '(' index_subtype_definition (',' index_subtype_definition)* ')' OF component_definition
    ;

constrained_array_definition
    : ARRAY '(' discrete_subtype_definition (',' discrete_subtype_definition)* ')' OF component_definition
    ;

index_subtype_definition
    : subtype_mark RANGE_ BOX
    ;

discrete_subtype_definition
    : discrete_subtype_indication
    | range
    ;

component_definition
    : ALIASED? subtype_indication
    | ALIASED? access_definition
    ;

defining_character_literal
    : CHARACTER_LITERAL_
    ;

assign_expression
    : ASSIGN expression
    ;

number_declaration
    : defining_identifier_list COLON CONSTANT ASSIGN expression
    ;

expression
    : relation (AND relation)*
    | relation (AND THEN relation)*
    | relation (OR relation)*
    | relation (OR ELSE relation)*
    | relation (XOR relation)*
    ;

choice_expression
    : choice_relation (AND choice_relation)*
    | choice_relation (OR choice_relation)*
    | choice_relation (XOR choice_relation)*
    | choice_relation (AND THEN choice_relation)*
    | choice_relation (OR ELSE choice_relation)*
    ;

choice_relation
    : simple_expression (relational_operator simple_expression)?
    ;

relation
    : simple_expression (relational_operator simple_expression)?
    | simple_expression NOT? IN membership_choice_list
    ;

membership_choice_list
    : membership_choice (VL membership_choice)*
    ;

membership_choice
    : choice_expression
    | range
    | subtype_mark
    ;

simple_expression
    : (unary_adding_operator)? term (binary_adding_operator term)*
    ;

term
    : factor (multiplying_operator factor)*
    ;

factor
    : primary (EXPON primary)?
    | ABS primary
    | NOT primary
    ;

primary
    : numeric_literal
    | NULL_
    | string_literal
    | aggregate
    | name
    | allocator
    | '(' expression ')'
    | '(' conditional_expression ')'
    | '(' qualified_expression ')'
    ;

numeric_literal
    : NUMERIC_LITERAL_
    ;

character_literal
    : CHARACTER_LITERAL_
    ;

string_literal
    : STRING_LITERAL_
    ;

logical_operator
    : AND
    | OR
    | XOR
    ;

relational_operator
    : EQ
    | NE
    | LE
    | GT
    | LE
    | GE
    ;

binary_adding_operator
    : PLUS
    | MINUS
    | AMPERSAND
    ;

unary_adding_operator
    : PLUS
    | MINUS
    ;

multiplying_operator
    : MULT
    | DIV
    | MOD
    | REM
    ;

highest_precedence_operator
    : EXPON
    | ABS
    | NOT
    ;

conditional_expression
    : if_expression
    | case_expression
    ;

if_expression
    : IF condition THEN dependent_expression
        (ELSIF condition THEN dependent_expression)*
        (ELSE dependent_expression)?
    ;

condition
    : boolean_expression
    ;

boolean_expression
    : expression
    ;

case_expression
    : CASE selecting_expression IS case_expression_alternative (',' case_expression_alternative)*
    ;

case_expression_alternative
    : WHEN discrete_choice_list ARROW dependent_expression
    ;

selecting_expression
    : expression
    ;

dependent_expression
    : expression
    ;

quantified_expression
    : FOR quantifier loop_parameter_specification ARROW predicate
    | FOR quantifier iterator_specification ARROW predicate
    ;

quantifier
    : ALL
    | SOME
    ;

predicate
    : boolean_expression
    ;

subprogram_declaration
    : overriding_indicator? subprogram_specification aspect_specification? SEMI
    ;

subprogram_specification
    : procedure_specification
    | function_specification
    ;

procedure_specification
    : PROCEDURE defining_program_unit_name formal_part? //parameter_profile?
    ;

function_specification
    : FUNCTION defining_designator parameter_and_result_profile
    ;

designator
    : (name DOT)? id_
    | operator_symbol
    ;

defining_designator
    : defining_program_unit_name
    | defining_operator_symbol
    ;

defining_program_unit_name
    : (name DOT)? defining_identifier
    ;

operator_symbol
    : string_literal
    ;

defining_operator_symbol
    : operator_symbol
    ;

//parameter_profile
//    : formal_part?
//    ;

parameter_and_result_profile
    : formal_part? RETURN null_exclusion? subtype_mark
    | formal_part? RETURN access_definition
    ;

formal_part
    : '(' parameter_specification (SEMI parameter_specification)* ')'
    ;

parameter_specification
    : defining_identifier_list COLON ALIASED? mode_ null_exclusion? subtype_mark (ASSIGN default_expression)?
    | defining_identifier_list COLON access_definition (ASSIGN default_expression)?
    ;

mode_
    : IN? OUT?
    ;

subprogram_body
    : overriding_indicator? subprogram_specification aspect_specification? IS declarative_part
        BEGIN handled_sequence_of_statements END designator? SEMI
    ;

handled_sequence_of_statements
    : sequence_of_statements (EXCEPTION exception_handler+ )?
    ;

exception_handler
    : WHEN (choice_parameter_specification COLON)? exception_choice (VL exception_choice)* ARROW sequence_of_statements
    ;

choice_parameter_specification
    : defining_identifier
    ;

exception_choice
    : name
    | OTHERS
    ;

overriding_indicator
    : NOT? OVERRIDING
    ;

raise_statement
    : RAISE SEMI
    | RAISE name (WITH string_expression)? SEMI
    ;

string_expression
    : expression
    ;

abstract_subprogram_declaration
    : overriding_indicator? subprogram_specification IS ABSTRACT aspect_specification?
    ;

interface_type_definition
    : (LIMITED | TASK | PROTECTED | SYNCHRONIZED)? INTERFACE (AND interface_list)?
    ;

interface_list
    : interface_subtype_mark (AND interface_subtype_mark)*
    ;

interface_subtype_mark
    : subtype_mark
    ;

access_type_definition
    : null_exclusion? access_to_object_definition
    | null_exclusion? access_to_subprogram_definition
    ;

access_to_object_definition
    : ACCESS general_access_modifier? subtype_indication
    ;

general_access_modifier
    : ALL
    | CONSTANT
    ;

access_to_subprogram_definition
    : ACCESS PROTECTED? PROCEDURE formal_part? //parameter_profile
    | ACCESS PROTECTED? FUNCTION parameter_and_result_profile
    ;

null_exclusion
    : NOT NULL_
    ;

access_definition
    : null_exclusion? ACCESS CONSTANT? subtype_mark
    | null_exclusion? ACCESS PROTECTED? PROCEDURE formal_part? //parameter_profile
    | null_exclusion? ACCESS PROTECTED? FUNCTION parameter_and_result_profile
    ;

incomplete_type_declaration
    : TYPE defining_identifier discriminant_part? (IS TAGGED)? SEMI
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

name
    : direct_name
    //| explicit_dereference
    | name DOT ALL
    | name '(' expression (',' expression)* ')' //indexed_component
    | name '(' discrete_range ')'  //slice
    | name DOT selector_name       //selected_component
    | name SQ attribute_designator //attribute_reference
    | type_conversion
    | name actual_parameter_part   //function_call
    | character_literal
    | qualified_expression
    //| name                       //generalized_reference
    | name actual_parameter_part   //generalized_indexing
    ;

package_body
    : PACKAGE BODY_ defining_program_unit_name aspect_specification? IS
        declarative_part
        (BEGIN handled_sequence_of_statements)?
        END ( (name DOT)? id_)? SEMI
    ;

body_stub
    : subprogram_body_stub
    | package_body_stub
    | task_body_stub
    | protected_body_stub
    ;

subprogram_body_stub
    : overriding_indicator? subprogram_specification IS SEPARATE aspect_specification? SEMI
    ;

package_body_stub
    : PACKAGE BODY_ defining_identifier IS SEPARATE aspect_specification? SEMI
    ;

task_body_stub
    : TASK BODY_ defining_identifier IS SEPARATE aspect_specification? SEMI
    ;

protected_body_stub
    : PROTECTED BODY_ defining_identifier IS SEPARATE aspect_specification? SEMI
    ;

selector_name
    : id_
    | character_literal
    | operator_symbol
    ;

direct_name
    : id_
    | operator_symbol
    ;

attribute_designator
    : id_ ('(' expression ')')?
    | ACCESS
    | DELTA
    | DIGITS
    | MOD
    ;

range_attribute_reference
//    : prefix '\'' range_attribute_designator
    : name '\'' range_attribute_designator
    ;

range_attribute_designator
    : RANGE_ ('(' expression ')')?
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
    : (component_choice_list ARROW)? expression
    | component_choice_list ARROW BOX
    ;

component_choice_list
    : component_selector_name (VL component_selector_name)*
    | OTHERS
    ;

component_selector_name
    : selector_name
    ;

extension_aggregate
    : '(' ancestor_part WITH record_component_association_list ')'
    ;

array_aggregate
    : positional_array_aggregate
    | named_array_aggregate
    ;

positional_array_aggregate
    : '(' expression ',' expression (',' expression)* ')'
    | '(' expression (',' expression)* ',' OTHERS ARROW expression ')'
    | '(' expression (',' expression)* ',' OTHERS ARROW BOX ')'
    ;

named_array_aggregate
    : '(' array_component_association (',' array_component_association)* ')'
    ;

array_component_association
    : discrete_choice_list ARROW expression
    | discrete_choice_list ARROW BOX
    ;

ancestor_part
    : expression
    | subtype_mark
    ;

type_conversion
    : subtype_mark '(' expression ')'
    | subtype_mark '(' name ')'
    ;

qualified_expression
    : subtype_mark SQ '(' expression ')'
    | subtype_mark SQ '(' aggregate ')'
    ;

allocator
    : NEW (subpool_specification)? subtype_indication
    | NEW (subpool_specification)? qualified_expression
    ;

subpool_specification
    : '(' name ')'
    ;

null_procedure_declaration
    : overriding_indicator? procedure_specification IS NULL_ aspect_specification? SEMI
    ;

expression_function_declaration
    : overriding_indicator? function_specification IS '(' expression ')' aspect_specification? SEMI
    ;

package_declaration
    : package_specification SEMI
    ;

renaming_declaration
    : object_renaming_declaration
    | exception_renaming_declaration
    | package_renaming_declaration
    | subprogram_renaming_declaration
    | generic_renaming_declaration
    ;

exception_renaming_declaration
    : defining_identifier COLON EXCEPTION RENAMES name aspect_specification? SEMI
    ;

package_renaming_declaration
    : PACKAGE defining_program_unit_name RENAMES name aspect_specification? SEMI
    ;

subprogram_renaming_declaration
    : overriding_indicator? subprogram_specification RENAMES name aspect_specification? SEMI
    ;

generic_renaming_declaration
    : GENERIC PACKAGE defining_program_unit_name RENAMES name aspect_specification? SEMI
    | GENERIC PROCEDURE defining_program_unit_name RENAMES name aspect_specification? SEMI
    | GENERIC FUNCTION defining_program_unit_name RENAMES name aspect_specification? SEMI
    ;

object_renaming_declaration
    : defining_identifier COLON null_exclusion? subtype_mark RENAMES name aspect_specification? SEMI
    | defining_identifier COLON access_definition RENAMES name aspect_specification? SEMI
    ;

exception_declaration
    : defining_identifier_list COLON EXCEPTION aspect_specification? SEMI
    ;

generic_declaration
    : generic_subprogram_declaration
    | generic_package_declaration
    ;

generic_subprogram_declaration
    : generic_formal_part subprogram_specification aspect_specification? SEMI
    ;

generic_package_declaration
    : generic_formal_part package_specification SEMI
    ;

package_specification
    : PACKAGE defining_program_unit_name aspect_specification? IS
        basic_declarative_item*
        (PRIVATE basic_declarative_item*)?
        END ( (name DOT)? id_)?
    ;

generic_formal_part
    : GENERIC (generic_formal_parameter_specification | use_clause)*
    ;

use_clause
    : use_package_clause
    | use_type_clause
    ;

use_package_clause
    : USE name (',' name)* SEMI
    ;

use_type_clause
    : USE ALL? TYPE subtype_mark (',' subtype_mark)* SEMI
    ;

generic_formal_parameter_specification
    : formal_object_declaration
    | formal_type_declaration
    | formal_subprogram_declaration
    | formal_package_declaration
    ;

formal_package_declaration
    : WITH PACKAGE defining_identifier IS NEW name formal_package_actual_part aspect_specification? SEMI
    ;

formal_package_actual_part
    : '(' (OTHERS ARROW)? BOX ')'
    | generic_actual_part?
    | '(' formal_package_association (',' formal_package_association)* (',' OTHERS ARROW BOX)? ')'
    ;

formal_package_association
    : generic_association
    | generic_formal_parameter_selector_name ARROW BOX
    ;

formal_subprogram_declaration
    : formal_concrete_subprogram_declaration
    | formal_abstract_subprogram_declaration
    ;

formal_concrete_subprogram_declaration
    : WITH subprogram_specification (IS subprogram_default)? aspect_specification? SEMI
    ;

formal_abstract_subprogram_declaration
    : WITH subprogram_specification IS ABSTRACT subprogram_default? aspect_specification? SEMI
    ;

formal_object_declaration
    : defining_identifier_list COLON mode_ null_exclusion? subtype_mark (ASSIGN default_expression)? aspect_specification? SEMI
    | defining_identifier_list COLON mode_ access_definition (ASSIGN default_expression)? aspect_specification?
    ;

generic_instantiation
    : PACKAGE defining_program_unit_name IS NEW name generic_actual_part? aspect_specification?
    | overriding_indicator? PROCEDURE defining_program_unit_name IS NEW name generic_actual_part? aspect_specification?
    | overriding_indicator? FUNCTION defining_designator IS NEW name generic_actual_part? aspect_specification?
    ;

formal_type_declaration
    : formal_complete_type_declaration
    | formal_incomplete_type_declaration
    ;

formal_complete_type_declaration
    : TYPE defining_identifier discriminant_part? IS formal_type_definition aspect_specification? SEMI
    ;

formal_incomplete_type_declaration
    : TYPE defining_identifier discriminant_part? (IS TAGGED)? SEMI
    ;

formal_type_definition
    : formal_private_type_definition
    | formal_derived_type_definition
    | formal_discrete_type_definition
    | formal_signed_integer_type_definition
    | formal_modular_type_definition
    | formal_floating_point_type_definition
    | formal_ordinary_fixed_point_type_definition
    | formal_decimal_fixed_point_type_definition
    | formal_array_type_definition
    | formal_access_type_definition
    | formal_interface_type_definition
    ;

formal_private_type_definition
    : ((ABSTRACT)? TAGGED)? LIMITED? PRIVATE
    ;

formal_derived_type_definition
    : ABSTRACT? LIMITED? SYNCHRONIZED NEW subtype_mark ( (AND interface_list)? WITH PRIVATE )?
    ;

formal_discrete_type_definition
    : '(' BOX ')'
    ;

formal_signed_integer_type_definition
    : range BOX
    ;

formal_modular_type_definition
    : MOD BOX
    ;

formal_floating_point_type_definition
    : DIGITS BOX
    ;

formal_ordinary_fixed_point_type_definition
    : DELTA BOX
    ;

formal_decimal_fixed_point_type_definition
    : DELTA BOX DIGITS BOX
    ;

formal_array_type_definition
    : array_type_definition
    ;

formal_access_type_definition
    : access_type_definition
    ;

formal_interface_type_definition
    : interface_type_definition
    ;

generic_actual_part
    : '(' generic_association (',' generic_association)* ')'
    ;

generic_association
    : (generic_formal_parameter_selector_name ARROW)? explicit_generic_actual_parameter
    ;

explicit_generic_actual_parameter
    : expression
    | name
    | subtype_mark
    ;

subprogram_default
    : name
    | BOX
    | NULL_
    ;

aspect_clause
    : attribute_definition_clause
    | enumeration_representation_clause
    | record_representation_clause
    | at_clause
    ;

at_clause
    : FOR direct_name USE AT expression SEMI
    ;

mod_clause
    : AT MOD expression SEMI
    ;

enumeration_representation_clause
    : FOR first_subtype_local_name USE enumeration_aggregate SEMI
    ;

first_subtype_local_name
    : local_name
    ;

enumeration_aggregate
    : array_aggregate
    ;

record_representation_clause
    : FOR first_subtype_local_name USE RECORD mod_clause? component_clause* END RECORD?
    ;

component_clause
    : component_local_name AT position RANGE_ first_bit DOTDOT last_bit SEMI
    ;

component_local_name
    : local_name
    ;

position
    : expression
    ;

first_bit
    : expression
    ;

last_bit
    : expression
    ;

code_statement
    : qualified_expression
    ;

local_name
    : direct_name
    | direct_name SQ attribute_designator
    | name
    ;

generic_formal_parameter_selector_name
    : selector_name
    ;

sequence_of_statements
    : statement+ label*
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
    | simple_return_statement
    | entry_call_statement
    | requeue_statement
    | delay_statement
    | abort_statement
    | raise_statement
    | code_statement
    ;

requeue_statement
    : REQUEUE name (WITH ABORT)? SEMI
    ;

simple_return_statement
    : RETURN expression? SEMI
    ;

extended_return_object_declaration
    : defining_identifier COLON ALIASED? CONSTANT? return_subtype_indication (ASSIGN expression)?
    ;

extended_return_statement
    : RETURN extended_return_object_declaration (DO handled_sequence_of_statements END RETURN)? SEMI
    ;

return_subtype_indication
    : subtype_indication
    | access_definition
    ;

procedure_call_statement
    : name
    | procedure_prefix actual_parameter_part SEMI
    ;

actual_parameter_part
    : '(' parameter_association (',' parameter_association)* ')'
    ;

parameter_association
    : (formal_parameter_selector_name ARROW)? explicit_actual_parameter
    ;

formal_parameter_selector_name
    : selector_name
    ;

explicit_actual_parameter
    : expression
    | name
    ;

procedure_prefix
    : name
    ;

abort_statement
    : ABORT name (',' name)*
    ;

delay_statement
    : delay_until_statement
    | delay_relative_statement
    ;

delay_until_statement
    : DELAY UNTIL delay_expression
    ;

delay_relative_statement
    : DELAY delay_expression
    ;

delay_expression
    : expression
    ;

compound_statement
    : if_statement
    | case_statement
    | loop_statement
    | block_statement
    | extended_return_statement
    | accept_statement
    | select_statement
    ;

select_statement
    : selective_accept
    | timed_entry_call
    | conditional_entry_call
    | asynchronous_select
    ;

selective_accept
    : SELECT guard? select_alternative ( OR guard? select_alternative )*
        ( ELSE sequence_of_statements )? END SELECT SEMI
    ;

guard
    : WHEN condition ARROW
    ;

select_alternative
    : accept_alternative
    | delay_alternative
    | terminate_alternative
    ;

asynchronous_select
    : SELECT triggering_alternative THEN ABORT abortable_part END SELECT SEMI
    ;

triggering_alternative
    : triggering_statement sequence_of_statements?
    ;

triggering_statement
    : procedure_or_entry_call
    | delay_statement
    ;

abortable_part
    : sequence_of_statements
    ;

conditional_entry_call
    : SELECT entry_call_alternative
        ELSE sequence_of_statements
        END SELECT SEMI
    ;

timed_entry_call
    : SELECT entry_call_alternative OR delay_alternative END SELECT SEMI
    ;

entry_call_alternative
    : procedure_or_entry_call sequence_of_statements?
    ;

procedure_or_entry_call
    : procedure_call_statement
    | entry_call_statement
    ;

entry_call_statement
    : name actual_parameter_part? SEMI
    ;

accept_alternative
    : accept_statement sequence_of_statements?
    ;

delay_alternative
    : delay_statement sequence_of_statements?
    ;

terminate_alternative
    : TERMINATE SEMI
    ;

null_statement
    : NULL_ SEMI
    ;

label
    : label_statement_identifier
    ;

label_statement_identifier
    : statement_identifier
    ;

statement_identifier
    : direct_name
    ;

assignment_statement
    : name ASSIGN expression SEMI
    ;

if_statement
    : IF condition THEN sequence_of_statements
        (ELSIF condition THEN sequence_of_statements)*
        (ELSE sequence_of_statements)?
        END IF SEMI
    ;

case_statement
    : CASE selecting_expression IS case_statement_alternative case_statement_alternative* END CASE SEMI
    ;

case_statement_alternative
    : WHEN discrete_choice_list ARROW sequence_of_statements
    ;

loop_statement
    : (loop_statement_identifier COLON)? iteration_scheme? LOOP sequence_of_statements END LOOP loop_identifier? SEMI
    ;

loop_statement_identifier
    : statement_identifier
    ;

loop_identifier
    : id_
    ;

iteration_scheme
    : WHILE condition
    | FOR loop_parameter_specification
    | FOR iterator_specification
    ;

loop_parameter_specification
    : defining_identifier IN REVERSE? discrete_subtype_definition
    ;

iterator_specification
    : defining_identifier IN REVERSE? name
    | defining_identifier (COLON subtype_indication) OF REVERSE? name
    ;

block_statement
    : (block_statement_identifier COLON)? (DECLARE declarative_part)?
        BEGIN handled_sequence_of_statements END (block_identifier)? SEMI
    ;

block_statement_identifier
    : statement_identifier
    ;

block_identifier
    : id_
    ;

exit_statement
    : EXIT name? (WHEN condition)? SEMI
    ;

goto_statement
    : GOTO name
    ;
