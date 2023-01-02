/*
Ada 83 grammar.
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
parser grammar Ada83Parser;


options { tokenVocab = Ada83Lexer; }
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
   : CHARACTER_LITERAL_
   ;

string_literal
   : STRING_LITERAL_
   ;
/*
3 - Declarations and Types
*/
   
   
basic_declaration
   : object_declaration
   | number_declaration
   | type_declaration
   | subtype_declaration
   | subprogram_declaration
   | package_declaration
   | task_declaration
   | generic_declaration
   | exception_declaration
   | generic_instantiation
   | renaming_declaration
   | deferred_constant_declaration
   ;

object_declaration
   : identifier_list ':' CONSTANT? subtype_indication (':=' expression)? ';'
   | identifier_list ':' CONSTANT? constrained_array_definition (':=' expression)? ';'
   ;

number_declaration
   : identifier_list ':' CONSTANT ':=' universal_static = expression ';'
   ;

type_declaration
   : full_type_declaration
   | incomplete_type_declaration
   | private_type_declaration
   ;

full_type_declaration
   : TYPE identifier discriminant_part? IS type_definition ';'
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
   : SUBTYPE identifier IS subtype_indication ';'
   ;

subtype_indication
   : type_mark = name constraint?
   ;

constraint
   : range_constraint
   | floating_point_constraint
   | fixed_point_constraint
   | index_constraint
   | discriminant_constraint
   ;

derived_type_definition
   : NEW subtype_indication
   ;

identifier_list
   : identifier (',' identifier)*
   ;

range_constraint
   : range range
   ;

range
   : prefix = name SQ attribute_designator // range_attribute
   | simple_expression '..' simple_expression
   ;

enumeration_type_definition
   : '(' enumeration_literal_specification (',' enumeration_literal_specification)* ')'
   ;

enumeration_literal_specification
   : enumeration_literal
   ;

enumeration_literal
   : identifier
   | character_literal
   ;

integer_type_definition
   : range_constraint
   ;

real_type_definition
   : floating_point_constraint
   | fixed_point_constraint
   ;

floating_point_constraint
   : floating_accuracy_definition range_constraint?
   ;

floating_accuracy_definition
   : DIGITS static_simple = expression
   ;

fixed_point_constraint
   : fixed_accuracy_definition range_constraint?
   ;

fixed_accuracy_definition
   : DELTA static_simple = expression
   ;

array_type_definition
   : unconstrained_array_definition
   | constrained_array_definition
   ;

unconstrained_array_definition
   : ARRAY '(' index_subtype_definition (',' index_subtype_definition)* ')' OF subtype_indication
   ;

constrained_array_definition
   : ARRAY index_constraint OF subtype_indication
   ;

index_subtype_definition
   : type_mark = name range '<>'
   ;

index_constraint
   : '(' discrete_range (',' discrete_range)* ')'
   ;

discrete_range
   : subtype_indication
   | range
   ;

record_type_definition
   : RECORD component_list END RECORD
   ;

component_list
   : component_declaration component_declaration*
   | component_declaration* variant_part
   | NULL_ ';'
   ;

component_declaration
   : identifier_list ':' component_subtype_definition (':=' expression)? ';'
   ;

component_subtype_definition
   : subtype_indication
   ;

discriminant_part
   : '(' discriminant_specification (';' discriminant_specification)* ')'
   ;

discriminant_specification
   : identifier_list ':' type_mark = name (':=' expression)?
   ;

discriminant_constraint
   : '(' discriminant_association (',' discriminant_association)* ')'
   ;

discriminant_association
   : (discriminant = simple_name ('|' discriminant = simple_name)* '=>')? expression
   ;

variant_part
   : CASE discriminant = simple_name IS variant variant* END CASE ';'
   ;

variant
   : WHEN choice ('|' choice)* '=>' component_list
   ;

choice
   : simple_expression
   | discrete_range
   | OTHERS
   | component = simple_name
   ;

access_type_definition
   : ACCESS subtype_indication
   ;

incomplete_type_declaration
   : TYPE identifier discriminant_part? ';'
   ;

basic_declarative_item
   : basic_declaration
   | representation_clause
   | use_clause
   ;

later_declarative_item
   : body
   | subprogram_declaration
   | package_declaration
   | task_declaration
   | generic_declaration
   | use_clause
   | generic_instantiation
   ;

body
   : proper_body
   | body_stub
   ;

proper_body
   : subprogram_body
   | package_body
   | task_body
   ;
/*
4 - Names
*/
   
   
name
   : simple_name
   | character_literal
   | operator_symbol
   | prefix = name '(' expression (',' expression)* ')' //indexed_component
   | prefix = name '(' discrete_range ')' //slice
   | prefix = name '.' selector //selected_component
   | prefix = name SQ attribute_designator //attribute
   | name actual_parameter_part //fn call
   
   ;

simple_name
   : identifier
   ;

selector
   : simple_name
   | character_literal
   | operator_symbol
   | ALL
   ;

attribute_designator
   : simple_name ('(' universal_static = expression ')')?
   ;

aggregate
   : '(' component_association (',' component_association)* ')'
   ;

component_association
   : (choice ('|' choice)* '=>')? expression
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
   | simple_expression NOT? IN type_mark = name
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
   | aggregate
   | string_literal
   | name
   | allocator
   | function = name actual_parameter_part? //function_call
   | type_conversion
   | qualified_expression
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

type_conversion
   : type_mark = name '(' expression ')'
   ;

qualified_expression
   : type_mark = name SQ '(' expression ')'
   | type_mark = name SQ aggregate
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
   | procedure_call_statement
   | exit_statement
   | return_statement
   | goto_statement
   | entry_call_statement
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

label
   : '<<' simple_name '>>' // label_simple_name
   
   ;

null_statement
   : NULL_ ';'
   ;

assignment_statement
   : variable = name ':=' expression ';'
   ;

if_statement
   : IF condition THEN sequence_of_statements (ELSIF condition THEN sequence_of_statements)* (ELSE sequence_of_statements)? END IF ';'
   ;

condition
   : boolean = expression
   ;

case_statement
   : CASE expression IS case_statement_alternative case_statement_alternative* END CASE ';'
   ;

case_statement_alternative
   : WHEN choice ('|' choice)* '=>' sequence_of_statements
   ;

loop_statement
   : (loop = simple_name ':')? iteration_scheme? LOOP sequence_of_statements END LOOP loop = simple_name? ';'
   ;

iteration_scheme
   : WHILE condition
   | FOR loop_parameter_specification
   ;

loop_parameter_specification
   : identifier IN REVERSE? discrete_range
   ;

block_statement
   : (block = simple_name ':')? (DECLARE basic_declarative_item* later_declarative_item*)? BEGIN sequence_of_statements (EXCEPTION_ exception_handler exception_handler*)? END block = simple_name? ';'
   ;

exit_statement
   : EXIT loop = name? (WHEN condition)? ';'
   ;

return_statement
   : RETURN expression? ';'
   ;

goto_statement
   : GOTO name ';' // label_name
   
   ;
/*
6 - Subprograms
*/
   
   
subprogram_declaration
   : subprogram_specification ';'
   ;

subprogram_specification
   : PROCEDURE identifier formal_part?
   | FUNCTION designator formal_part? RETURN type_mark = name
   ;

designator
   : identifier
   | operator_symbol
   ;

operator_symbol
   : string_literal
   ;

formal_part
   : '(' parameter_specification (';' parameter_specification)? ')'
   ;

parameter_specification
   : identifier_list ':' mode_ type_mark = name (':=' expression)?
   ;

mode_
   : IN? OUT?
   ;

subprogram_body
   : subprogram_specification IS basic_declarative_item* later_declarative_item* BEGIN sequence_of_statements (EXCEPTION_ exception_handler exception_handler*)? END designator? ';'
   ;

procedure_call_statement
   : name actual_parameter_part? ';'
   ;

actual_parameter_part
   : '(' parameter_association (',' parameter_association)* ')'
   ;

parameter_association
   : (formal_parameter '=>')? actual_parameter
   ;

formal_parameter
   : parameter = simple_name
   ;

actual_parameter
   : expression
   | name
   | name '(' name ')'
   ;
/*
7 - Packages
*/
   
   
package_declaration
   : package_specification ';'
   ;

package_specification
   : PACKAGE identifier IS basic_declarative_item* (PRIVATE basic_declarative_item*)? END package = simple_name?
   ;

package_body
   : PACKAGE BODY_ package = simple_name IS basic_declarative_item* later_declarative_item* (BEGIN sequence_of_statements (EXCEPTION_ exception_handler exception_handler*)?)? END package = simple_name? ';'
   ;

private_type_declaration
   : TYPE identifier discriminant_part? IS LIMITED? PRIVATE ';'
   ;

deferred_constant_declaration
   : identifier_list ':' CONSTANT type_mark = name ';'
   ;
/*
8 - Visibility Rules
*/
   
   
use_clause
   : USE package = name (',' package = name)* ';'
   ;

renaming_declaration
   : identifier ':' name RENAMES name ';'
   | identifier ':' EXCEPTION_ RENAMES name ';'
   | PACKAGE identifier RENAMES name ';'
   | subprogram_specification RENAMES name ';'
   ;
/*
9 - Tasks
*/
   
   
task_declaration
   : task_specification ';'
   ;

task_specification
   : TASK TYPE? identifier (IS entry_declaration* representation_clause* END task = simple_name?)?
   ;

task_body
   : TASK BODY_ task = simple_name IS basic_declarative_item* later_declarative_item* BEGIN sequence_of_statements (EXCEPTION_ exception_handler exception_handler*)? END task = simple_name? ';'
   ;

entry_declaration
   : ENTRY identifier ('(' discrete_range ')')? formal_part? ';'
   ;

entry_call_statement
   : entry = name actual_parameter_part? ';'
   ;

accept_statement
   : ACCEPT_ entry = simple_name ('(' entry_index ')')? formal_part? (DO sequence_of_statements END entry = simple_name?)? ';'
   ;

entry_index
   : expression
   ;

delay_statement
   : DELAY simple_expression ';'
   ;

select_statement
   : selective_wait
   | conditional_entry_call
   | timed_entry_call
   ;

selective_wait
   : SELECT select_alternative (OR select_alternative)* (ELSE sequence_of_statements)? END SELECT ';'
   ;

select_alternative
   : (WHEN condition '=>')? selective_wait_alternative
   ;

selective_wait_alternative
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

conditional_entry_call
   : SELECT entry_call_statement sequence_of_statements? ELSE sequence_of_statements END SELECT ';'
   ;

timed_entry_call
   : SELECT entry_call_statement sequence_of_statements? OR delay_alternative END SELECT ';'
   ;

abort_statement
   : ABORT task = name (',' task = name)* ';'
   ;
/*
10 - Program Structure and Compilation Issues
*/
   
   
compilation
   : compilation_unit*
   ;

compilation_unit
   : context_clause library_unit
   | context_clause secondary_unit
   ;

library_unit
   : subprogram_declaration
   | package_declaration
   | generic_declaration
   | generic_instantiation
   | subprogram_body
   ;

secondary_unit
   : library_unit_body
   | subunit
   ;

library_unit_body
   : subprogram_body
   | package_body
   ;

context_clause
   : (with_clause use_clause*)*
   ;

with_clause
   : WITH unit = simple_name (',' unit = simple_name)* ';'
   ;

body_stub
   : subprogram_specification IS SEPARATE ';'
   | PACKAGE BODY_ package = simple_name IS SEPARATE ';'
   | TASK BODY_ task = simple_name IS SEPARATE ';'
   ;

subunit
   : SEPARATE '(' parent_unit = name ')' proper_body
   ;
/*
11 - Exceptions
*/
   
   
exception_declaration
   : identifier_list ':' EXCEPTION_ ';'
   ;

exception_handler
   : WHEN exception_choice ('|' exception_choice)* '=>' sequence_of_statements
   ;

exception_choice
   : name
   | OTHERS
   ;

raise_statement
   : RAISE name? ';'
   ;
/*
12 - Generic Units
*/
   
   
generic_declaration
   : generic_specification ';'
   ;

generic_specification
   : generic_formal_part subprogram_specification
   | generic_formal_part package_specification
   ;

generic_formal_part
   : GENERIC generic_parameter_declaration*
   ;

generic_parameter_declaration
   : identifier_list ':' (IN OUT?)? type_mark = name (':=' expression)? ';'
   | TYPE identifier IS generic_type_definition ';'
   | private_type_declaration
   | WITH subprogram_specification (IS name)? ';'
   | WITH subprogram_specification (IS '<>')? ';'
   ;

generic_type_definition
   : '(' '<>' ')'
   | RANGE_ '<>'
   | DIGITS '<>'
   | DELTA '<>'
   | array_type_definition
   | access_type_definition
   ;

generic_instantiation
   : PACKAGE identifier IS NEW generic_package = name generic_actual_part? ';'
   | PROCEDURE identifier IS NEW generic_procedure = name generic_actual_part? ';'
   | FUNCTION designator IS NEW generic_function = name generic_actual_part? ';'
   ;

generic_actual_part
   : '(' generic_association (',' generic_association)* ')'
   ;

generic_association
   : (generic_formal_parameter '=>')? generic_actual_parameter
   ;

generic_formal_parameter
   : parameter = simple_name
   | operator_symbol
   ;

generic_actual_parameter
   : expression
   | variable = name
   | subprogram = name
   | entry = name
   | type_mark = name
   ;
/*
13 - Representation Clauses and Implementation-Dependent Features
*/
   
   
representation_clause
   : type_representation_clause
   | address_clause
   ;

type_representation_clause
   : length_clause
   | enumeration_representation_clause
   | record_representation_clause
   ;

length_clause
   : FOR prefix = name SQ attribute_designator USE simple_expression ';'
   ;

enumeration_representation_clause
   : FOR type = simple_name USE aggregate ';'
   ;

record_representation_clause
   : FOR type = simple_name USE RECORD alignment_clause? component_clause* END RECORD ';'
   ;

alignment_clause
   : AT MOD static_simple = expression ';'
   ;

component_clause
   : component = name AT static_simple = expression RANGE_ static = range ';'
   ;

address_clause
   : FOR simple_name USE AT simple_expression ';'
   ;

code_statement
   : type_mark = name SQ record = aggregate ';'
   ;
/*

*/
   
   
