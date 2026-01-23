//
//  Copyright (C) 2025 ANTLR Grammars
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You may have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

// Author : Lucas Moss <48305572+LucasMoss@users.noreply.github.com>
// 
// This grammar is based on the VHDL grammar by Denis Gavrish in the
// ANTLR Grammars v4 repository : <https://github.com/antlr/grammars-v4>
//
// In addition to the minimal test files in the `examples` folder, this
// grammar has been tested on all VHDL in:
//     surf       <https://github.com/slaclab/surf>
//     open-logic <https://github.com/open-logic/open-logic>
//
// This grammar has been updated to support parts of the VHDL-2008 specification.
// It does not support PSL or VUnit features.
//
// Supports the following VHDL-2008 features:
//  block comments
//  process(all)
//  else generate, elsif generate, for generate
//  'subtype in signal declaration
//  AND, OR, XOR, NAND, XNOR unary reduction functions
//  generic types in package instances, subprograms and entities

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar vhdl2008;

options {
    caseInsensitive = true;
}

ABS
    : 'ABS'
    ;

ACCESS
    : 'ACCESS'
    ;

ACROSS
    : 'ACROSS'
    ;

AFTER
    : 'AFTER'
    ;

ALIAS
    : 'ALIAS'
    ;

ALL
    : 'ALL'
    ;

AND
    : 'AND'
    ;

ARCHITECTURE
    : 'ARCHITECTURE'
    ;

ARRAY
    : 'ARRAY'
    ;

ASSERT
    : 'ASSERT'
    ;

ATTRIBUTE
    : 'ATTRIBUTE'
    ;

BEGIN
    : 'BEGIN'
    ;

BLOCK
    : 'BLOCK'
    ;

BODY
    : 'BODY'
    ;

BREAK
    : 'BREAK'
    ;

BUFFER
    : 'BUFFER'
    ;

BUS
    : 'BUS'
    ;

CASE
    : 'CASE'
    ;

COMPONENT
    : 'COMPONENT'
    ;

CONFIGURATION
    : 'CONFIGURATION'
    ;

CONSTANT
    : 'CONSTANT'
    ;

CONTEXT
    : 'CONTEXT'
    ;

DEFAULT
    : 'DEFAULT'
    ;

DISCONNECT
    : 'DISCONNECT'
    ;

DOWNTO
    : 'DOWNTO'
    ;

END
    : 'END'
    ;

ENTITY
    : 'ENTITY'
    ;

ELSE
    : 'ELSE'
    ;

ELSIF
    : 'ELSIF'
    ;

EXIT
    : 'EXIT'
    ;

FILE
    : 'FILE'
    ;

FOR
    : 'FOR'
    ;

FORCE
    : 'FORCE'
    ;

FUNCTION
    : 'FUNCTION'
    ;

GENERATE
    : 'GENERATE'
    ;

GENERIC
    : 'GENERIC'
    ;

GROUP
    : 'GROUP'
    ;

GUARDED
    : 'GUARDED'
    ;

IF
    : 'IF'
    ;

IMPURE
    : 'IMPURE'
    ;

IN
    : 'IN'
    ;

INERTIAL
    : 'INERTIAL'
    ;

INOUT
    : 'INOUT'
    ;

IS
    : 'IS'
    ;

LABEL
    : 'LABEL'
    ;

LIBRARY
    : 'LIBRARY'
    ;

LIMIT
    : 'LIMIT'
    ;

LINKAGE
    : 'LINKAGE'
    ;

LITERAL
    : 'LITERAL'
    ;

LOOP
    : 'LOOP'
    ;

MAP
    : 'MAP'
    ;

MOD
    : 'MOD'
    ;

NAND
    : 'NAND'
    ;

NATURE
    : 'NATURE'
    ;

NEW
    : 'NEW'
    ;

NEXT
    : 'NEXT'
    ;

NOISE
    : 'NOISE'
    ;

NOR
    : 'NOR'
    ;

NOT
    : 'NOT'
    ;

NULL_
    : 'NULL'
    ;

OF
    : 'OF'
    ;

ON
    : 'ON'
    ;

OPEN
    : 'OPEN'
    ;

OR
    : 'OR'
    ;

OTHERS
    : 'OTHERS'
    ;

OUT
    : 'OUT'
    ;

PACKAGE
    : 'PACKAGE'
    ;

PARAMETER
    : 'PARAMETER'
    ;

PORT
    : 'PORT'
    ;

POSTPONED
    : 'POSTPONED'
    ;

PROCESS
    : 'PROCESS'
    ;

PROCEDURE
    : 'PROCEDURE'
    ;

PROCEDURAL
    : 'PROCEDURAL'
    ;

PROTECTED
    : 'PROTECTED'
    ;

PURE
    : 'PURE'
    ;

QUANTITY
    : 'QUANTITY'
    ;

RANGE
    : 'RANGE'
    ;

RELEASE
    : 'RELEASE'
    ;

REVERSE_RANGE
    : 'REVERSE_RANGE'
    ;

REJECT
    : 'REJECT'
    ;

REM
    : 'REM'
    ;

RECORD
    : 'RECORD'
    ;

REFERENCE
    : 'REFERENCE'
    ;

REGISTER
    : 'REGISTER'
    ;

REPORT
    : 'REPORT'
    ;

RETURN
    : 'RETURN'
    ;

ROL
    : 'ROL'
    ;

ROR
    : 'ROR'
    ;

SELECT
    : 'SELECT'
    ;

SEVERITY
    : 'SEVERITY'
    ;

SHARED
    : 'SHARED'
    ;

SIGNAL
    : 'SIGNAL'
    ;

SLA
    : 'SLA'
    ;

SLL
    : 'SLL'
    ;

SPECTRUM
    : 'SPECTRUM'
    ;

SRA
    : 'SRA'
    ;

SRL
    : 'SRL'
    ;

SUBNATURE
    : 'SUBNATURE'
    ;

SUBTYPE
    : 'SUBTYPE'
    ;

TERMINAL
    : 'TERMINAL'
    ;

THEN
    : 'THEN'
    ;

THROUGH
    : 'THROUGH'
    ;

TO
    : 'TO'
    ;

TOLERANCE
    : 'TOLERANCE'
    ;

TRANSPORT
    : 'TRANSPORT'
    ;

TYPE
    : 'TYPE'
    ;

UNAFFECTED
    : 'UNAFFECTED'
    ;

UNITS
    : 'UNITS'
    ;

UNTIL
    : 'UNTIL'
    ;

USE
    : 'USE'
    ;

VARIABLE
    : 'VARIABLE'
    ;

WAIT
    : 'WAIT'
    ;

WITH
    : 'WITH'
    ;

WHEN
    : 'WHEN'
    ;

WHILE
    : 'WHILE'
    ;

XNOR
    : 'XNOR'
    ;

XOR
    : 'XOR'
    ;

//------------------------------------------Parser----------------------------------------

abstract_literal
    : DECIMAL_LITERAL
    | BASED_LITERAL
    ;

access_type_definition
    : ACCESS subtype_indication
    ;

actual_designator
    : (INERTIAL)? expression
    | name
    | subtype_indication
    | OPEN
    ;

actual_parameter_part
    : association_list
    ;

actual_part
    : actual_designator
    | name LPAREN actual_designator RPAREN
    //| type_mark LPAREN actual_designator RPAREN // Equivalent to name
    ;

adding_operator
    : PLUS
    | MINUS
    | AMPERSAND
    ;

aggregate
    : LPAREN element_association (COMMA element_association)* RPAREN
    ;

alias_declaration
    // : ALIAS alias_designator (COLON subtype_indication)? IS name (signature)? SEMI
    : ALIAS alias_designator (COLON subtype_indication)? IS name SEMI
    ;

alias_designator
    : identifier
    | CHARACTER_LITERAL
    //| operator_symbol
    | STRING_LITERAL // Equivalent to operator_symbol
    ;

allocator
    : NEW subtype_indication
    | NEW qualified_expression
    ;

architecture_body
    : ARCHITECTURE identifier OF name IS architecture_declarative_part BEGIN architecture_statement_part END (
        ARCHITECTURE
    )? (identifier)? SEMI
    ;

architecture_declarative_part
    : (block_declarative_item)*
    ;

architecture_statement_part
    : (concurrent_statement)*
    ;

array_constraint
    : index_constraint (array_element_constraint)?
    | OPEN (array_element_constraint)
    ;

array_element_constraint
    : element_constraint
    ;

array_element_resolution
    : resolution_indication
    ;

array_type_definition
    : unbounded_array_definition
    | constrained_array_definition
    ;

assertion
    : ASSERT condition (REPORT expression)? (SEVERITY expression)?
    ;

assertion_statement
    : (label_colon)? assertion SEMI
    ;

association_element
    : (formal_part ARROW)? actual_part
    ;

association_list
    : association_element (COMMA association_element)*
    ;

attribute_declaration
    : ATTRIBUTE label_colon name SEMI
    ;

// Need to add tokens here since they will be consumed by the Lexer
attribute_designator
    : identifier
    | RANGE
    | REVERSE_RANGE
    | ACROSS
    | THROUGH
    | REFERENCE
    | TOLERANCE
    | SUBTYPE
    ;

attribute_name
    // : prefix (signature)? APOSTROPHE attribute_designator (LPAREN expression RPAREN)?
    : prefix APOSTROPHE attribute_designator (LPAREN expression RPAREN)?
    ;

attribute_specification
    : ATTRIBUTE attribute_designator OF entity_specification IS expression SEMI
    ;

base
    : INTEGER
    ;

// Lexer : basic_identifier

binding_indication
    : (USE entity_aspect)? (generic_map_aspect)? (port_map_aspect)?
    ;

// Lexer : bit_string_literal

// bit_value

block_configuration
    : FOR block_specification (use_clause)* (configuration_item)* END FOR SEMI
    ;

block_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | signal_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | component_declaration
    | attribute_declaration
    | attribute_specification
    | configuration_specification
    | disconnection_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

block_declarative_part
    : (block_declarative_item)*
    ;

block_header
    : (generic_clause ( generic_map_aspect SEMI)?)? (port_clause ( port_map_aspect SEMI)?)?
    ;

block_specification
    : name
    //| label (LPAREN generate_specification RPAREN)?
    identifier (LPAREN generate_specification RPAREN)?
    ;

block_statement
    : label_colon BLOCK (LPAREN expression RPAREN)? (IS)? block_header block_declarative_part BEGIN block_statement_part END BLOCK (
        identifier
    )? SEMI
    ;

block_statement_part
    : (concurrent_statement)*
    ;

case_generate_alternative
    : WHEN (label_colon)? choices ARROW generate_statement_body
    ;

case_generate_statement
    : CASE expression GENERATE case_generate_alternative (case_generate_alternative)*
    ;

case_statement
    : (label_colon)? CASE (QUESTION)? expression IS (case_statement_alternative)+ END CASE (
        QUESTION
    )? (identifier)? SEMI
    ;

case_statement_alternative
    : WHEN choices ARROW sequence_of_statements
    ;

choice
    : simple_expression
    | discrete_range
    | identifier
    | OTHERS
    ;

choices
    : choice (BAR choice)*
    ;

component_configuration
    : FOR component_specification (binding_indication SEMI)? (block_configuration)? END FOR SEMI
    ;

component_declaration
    : COMPONENT identifier (IS)? (generic_clause)? (port_clause)? END COMPONENT (identifier)? SEMI
    ;

component_instantiation_statement
    : label_colon instantiated_unit (generic_map_aspect)? (port_map_aspect)? SEMI
    ;

component_specification
    : instantiation_list COLON name
    ;

composite_type_definition
    : array_type_definition
    | record_type_definition
    ;

concurrent_assertion_statement
    : (label_colon)? (POSTPONED)? assertion SEMI
    ;

concurrent_conditional_signal_assignment
    : target LEQ (GUARDED)? (delay_mechanism)? conditional_waveforms SEMI
    ;

concurrent_procedure_call_statement
    : (label_colon)? (POSTPONED)? procedure_call SEMI
    ;

concurrent_selected_signal_assignment
    : WITH expression SELECT (QUESTION)? target LEQ (GUARDED)? (delay_mechanism)? selected_waveforms SEMI
    ;

concurrent_signal_assignment_statement
    : (label_colon)? (POSTPONED)? (
        conditional_signal_assignment
        | selected_signal_assignment
        | concurrent_selected_signal_assignment
    )
    ;

concurrent_simple_signal_assignment
    : target LEQ (GUARDED)? (delay_mechanism)? waveform SEMI
    ;

concurrent_statement
    : block_statement
    | process_statement
    | concurrent_procedure_call_statement
    | concurrent_assertion_statement
    | concurrent_signal_assignment_statement
    | component_instantiation_statement
    | generate_statement
    ;

condition
    : expression
    ;

condition_clause
    : UNTIL condition
    ;

// Lexer : condition_operator

conditional_expression
    : expression WHEN condition (ELSE expression WHEN condition)* (ELSE expression)?
    ;

conditional_force_assignment
    : target LEQ FORCE (force_mode)? conditional_expression SEMI
    ;

conditional_signal_assignment
    : conditional_waveform_assignment
    | conditional_force_assignment
    ;

conditional_variable_assignment
    : target WALRUS conditional_expression
    ;

conditional_waveform_assignment
    : target LEQ (delay_mechanism)? conditional_waveforms SEMI
    ;

conditional_waveforms
    //: waveform WHEN condition (ELSE waveform WHEN condition)* (ELSE waveform)
    : waveform (WHEN condition (ELSE conditional_waveforms)?)?
    ;

configuration_declaration
    : CONFIGURATION identifier OF name IS configuration_declarative_part block_configuration END (
        CONFIGURATION
    )? (identifier)? SEMI
    ;

configuration_declarative_item
    : use_clause
    | attribute_specification
    | group_declaration
    ;

configuration_declarative_part
    : (configuration_declarative_item)*
    ;

configuration_item
    : block_configuration
    | component_configuration
    ;

configuration_specification
    : FOR component_specification binding_indication SEMI // Equivalent to simple_configuration_specification
    // | compound_configuration_specification // Only used for vunit
    ;

constant_declaration
    : CONSTANT identifier_list COLON subtype_indication (WALRUS expression)? SEMI
    ;

constrained_array_definition
    : ARRAY index_constraint OF subtype_indication
    ;

constraint
    : range_constraint
    | index_constraint
    | record_constraint
    ;

context_clause
    : (context_item)*
    ;

context_declaration
    // : CONTEXT identifier IS context_clause END (CONTEXT)? (simple_name)? SEMI
    : CONTEXT identifier IS context_clause END (CONTEXT)? (identifier)? SEMI
    ;

context_item
    : library_clause
    | use_clause
    | context_reference
    ;

context_reference
    : CONTEXT selected_name (COMMA selected_name)* SEMI
    ;

DECIMAL_LITERAL
    : INTEGER (DOT INTEGER)? (EXPONENT)?
    ;

delay_mechanism
    : TRANSPORT
    | ( REJECT expression)? INERTIAL
    ;

design_file
    : (design_unit)+ EOF
    ;

design_unit
    : context_clause library_unit
    ;

designator
    : identifier
    // | operator_symbol
    | STRING_LITERAL // Equivalent to operator_symbol
    ;

direction
    : TO
    | DOWNTO
    ;

disconnection_specification
    : DISCONNECT guarded_signal_specification AFTER expression SEMI
    ;

discrete_range
    : subtype_indication
    | range_decl
    ;

element_association
    : (choices ARROW)? expression
    ;

element_constraint
    : array_constraint
    | record_constraint
    ;

element_declaration
    : identifier_list COLON element_subtype_definition SEMI
    ;

element_resolution
    : array_element_resolution
    | record_resolution
    ;

element_subtype_definition
    : subtype_indication
    ;

entity_aspect
    : ENTITY name (LPAREN identifier RPAREN)?
    | CONFIGURATION name
    | OPEN
    ;

entity_class
    : ENTITY
    | ARCHITECTURE
    | CONFIGURATION
    | PROCEDURE
    | FUNCTION
    | PACKAGE
    | TYPE
    | SUBTYPE
    | CONSTANT
    | SIGNAL
    | VARIABLE
    | COMPONENT
    | LABEL
    | LITERAL
    | UNITS
    | GROUP
    | FILE
    // | PROPERTY
    // | SEQUENCE
    ;

entity_class_entry
    : entity_class (BOX)?
    ;

entity_class_entry_list
    : entity_class_entry (COMMA entity_class_entry)*
    ;

entity_declaration
    : ENTITY identifier IS entity_header entity_declarative_part (BEGIN entity_statement_part)? END (
        ENTITY
    )? (identifier)? SEMI
    ;

entity_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | signal_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | disconnection_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

entity_declarative_part
    : (entity_declarative_item)*
    ;

entity_designator
    // : entity_tag (signature)?
    : entity_tag
    ;

entity_header
    : (generic_clause)? (port_clause)?
    ;

entity_name_list
    : entity_designator (COMMA entity_designator)*
    | OTHERS
    | ALL
    ;

entity_specification
    : entity_name_list COLON entity_class
    ;

entity_statement
    : concurrent_assertion_statement
    | process_statement
    | concurrent_procedure_call_statement
    ;

entity_statement_part
    : (entity_statement)*
    ;

entity_tag
    : identifier
    | CHARACTER_LITERAL
    | STRING_LITERAL
    ;

enumeration_literal
    : identifier
    | CHARACTER_LITERAL
    ;

enumeration_type_definition
    : LPAREN enumeration_literal (COMMA enumeration_literal)* RPAREN
    ;

exit_statement
    // : (label_colon)? EXIT (label)? (WHEN condition)? SEMI
    : (label_colon)? EXIT (identifier)? (WHEN condition)? SEMI
    ;

// Lexer : exponent

expression
    : (CONDITION_OPERATOR primary)
    // | logical_expression
    | (relation (logical_operator relation)*) // Optimised logical_expression
    ;

// Lexer : extended_digit

// Lexer : extended_identifier

// external_name
//     : external_constant_name
//     | external_signal_name
//     | external_variable_name
//     ;

// external_constant_name
//     : LSHIFT external_pathname COLON subtype_indication RSHIFT
//     ;

// external_signal_name
//     : LSHIFT external_pathname COLON subtype_indication RSHIFT
//     ;

// external_variable_name
//     : LSHIFT external_pathname COLON subtype_indication RSHIFT
//     ;

// external_pathname
//     : package_pathname
//     | absolute_pathname
//     | relative_pathname
//     ;

factor
    : primary (DOUBLESTAR primary)?
    | ABS primary
    | NOT primary
    | logical_operator primary
    ;

file_declaration
    : FILE identifier_list COLON subtype_indication (file_open_information)? SEMI
    ;

file_logical_name
    : expression
    ;

file_open_information
    : (OPEN expression)? IS file_logical_name
    ;

file_type_definition
    : FILE OF subtype_indication
    ;

floating_type_definition
    : range_constraint
    ;

for_generate_statement
    : FOR parameter_specification GENERATE generate_statement_body
    ;

force_mode
    : IN
    | OUT
    ;

// formal_designator
//     : name
//     ;

formal_parameter_list
    : interface_list
    ;

formal_part
    // : formal_designator
    // | name LPAREN formal_designator RPAREN
    // | type_mark LPAREN formal_designator RPAREN
    : name (LPAREN name RPAREN)? // Equivalent to all above statements
    ;

full_type_declaration
    : TYPE identifier IS type_definition SEMI
    ;

function_call
    : name (LPAREN actual_parameter_part RPAREN)?
    ;

function_specification
    : (PURE | IMPURE)? FUNCTION designator subprogram_header (
        LPAREN (PARAMETER)? formal_parameter_list RPAREN
    )?
    // RETURN type_mark
    RETURN name
    ;

generate_specification
    : discrete_range
    | expression
    // | label
    | identifier
    ;

generate_statement
    : label_colon? (for_generate_statement | if_generate_statement | case_generate_statement) END GENERATE (
        identifier
    )? SEMI
    ;

generate_statement_body
    : (block_declarative_part BEGIN)? concurrent_statement*
    ;

generic_clause
    : GENERIC LPAREN generic_list RPAREN SEMI
    ;

generic_list
    : interface_list
    ;

generic_map_aspect
    : GENERIC MAP LPAREN association_list RPAREN
    ;

graphic_character
    : (EXTENDED_IDENTIFIER)
    ;

group_constituent
    : name
    | CHARACTER_LITERAL
    ;

group_constituent_list
    : group_constituent (COMMA group_constituent)*
    ;

group_declaration
    : GROUP label_colon name LPAREN group_constituent_list RPAREN SEMI
    ;

group_template_declaration
    : GROUP identifier IS LPAREN entity_class_entry_list RPAREN SEMI
    ;

guarded_signal_specification
    //: signal_list COLON type_mark
    : signal_list COLON name
    ;

identifier
    : BASIC_IDENTIFIER
    | EXTENDED_IDENTIFIER
    ;

identifier_list
    : identifier (COMMA identifier)*
    ;

if_generate_statement
    : IF condition GENERATE generate_statement_body (
        ELSIF condition GENERATE generate_statement_body
    )* (ELSE GENERATE generate_statement_body)?
    ;

if_statement
    : (label_colon)? IF condition THEN sequence_of_statements (
        ELSIF condition THEN sequence_of_statements
    )* (ELSE sequence_of_statements)? END IF
    // (label)? 
    (identifier)? SEMI
    ;

incomplete_type_declaration
    : TYPE identifier
    ;

index_constraint
    : LPAREN discrete_range (COMMA discrete_range)* RPAREN
    ;

index_subtype_definition
    //: type_mark RANGE BOX
    : name RANGE BOX
    ;

indexed_name
    : prefix LPAREN expression (COMMA expression)* RPAREN
    ;

instantiated_unit
    : (COMPONENT)? name
    | ENTITY name (LPAREN identifier RPAREN)?
    | CONFIGURATION name
    ;

instantiation_list
    : identifier (COMMA identifier)*
    | OTHERS
    | ALL
    ;

// Lexer : integer

// integer_type_definition
//     : range_constraint
//     ;

interface_constant_declaration
    : (CONSTANT)? identifier_list COLON (IN)? subtype_indication (WALRUS expression)?
    ;

interface_declaration
    : interface_object_declaration
    | interface_type_declaration
    | interface_subprogram_declaration
    | interface_package_declaration
    ;

// interface_element
//     : interface_declaration
//     ;

interface_file_declaration
    : FILE identifier_list COLON subtype_indication
    ;

interface_function_specification
    : (PURE | IMPURE)? FUNCTION designator ((PARAMETER)? LPAREN formal_parameter_list RPAREN)?
    // RETURN type_mark
    RETURN name
    ;

interface_incomplete_type_declaration
    : TYPE identifier
    ;

interface_list
    // : interface_element (SEMI interface_element)*
    : interface_declaration (SEMI interface_declaration)*
    ;

interface_object_declaration
    : interface_constant_declaration
    | interface_signal_declaration
    | interface_variable_declaration
    | interface_file_declaration
    ;

interface_package_declaration
    : PACKAGE identifier IS NEW name interface_package_generic_map_aspect
    ;

interface_package_generic_map_aspect
    : generic_map_aspect
    | GENERIC MAP LPAREN (BOX | DEFAULT) RPAREN
    ;

interface_procedure_specification
    : PROCEDURE designator (((PARAMETER)? LPAREN formal_parameter_list RPAREN))?
    ;

interface_signal_declaration
    : (SIGNAL)? identifier_list COLON (mode_rule)? subtype_indication (BUS)? (WALRUS expression)?
    ;

interface_subprogram_declaration
    : interface_subprogram_specification (IS interface_subprogram_default)
    ;

interface_subprogram_default
    : name
    | BOX
    ;

interface_subprogram_specification
    : interface_procedure_specification
    | interface_function_specification
    ;

interface_type_declaration
    : interface_incomplete_type_declaration
    ;

interface_variable_declaration
    : (VARIABLE)? identifier_list COLON (mode_rule)? subtype_indication (WALRUS expression)?
    ;

iteration_scheme
    : WHILE condition
    | FOR parameter_specification
    ;

// label
//     : identifier
//     ;
// ANTLR optimisation : combine label and COLON rules
label_colon
    : identifier COLON
    ;

// Lexer : letter

// Lexer : letter_or_digit

library_clause
    // : LIBRARY logical_name_list SEMI
    : LIBRARY identifier_list SEMI // Equivalent to logical_name_list
    ;

library_unit
    : primary_unit
    | secondary_unit
    ;

literal
    : numeric_literal
    | enumeration_literal
    | STRING_LITERAL
    | BIT_STRING_LITERAL
    | NULL_
    ;

// logical_expression
//     : relation (AND relation)*
//     | relation (OR relation)*
//     | relation (XOR relation)*
//     | relation (NAND relation)*
//     | relation (NOR relation)*
//     | relation (XNOR relation)*
//     ;

// logical_name
//     : identifier
//     ;

// logical_name_list // Equivalent to identifier_list
//     : logical_name (COMMA logical_name)*
//     ;

logical_operator
    : AND
    | OR
    | NAND
    | NOR
    | XOR
    | XNOR
    ;

loop_statement
    : (label_colon)? (iteration_scheme)? LOOP sequence_of_statements END LOOP
    // (label)?
    (identifier)? SEMI
    ;

// miscellaneous_operator // Rule not used for anything
//     : DOUBLESTAR
//     | ABS
//     | NOT
//     ;

mode_rule
    : IN
    | OUT
    | INOUT
    | BUFFER
    | LINKAGE
    ;

multiplying_operator
    : MUL
    | DIV
    | MOD
    | REM
    ;

name
    : (
        identifier       // simple_name
        | STRING_LITERAL // operator_symbol
        | CHARACTER_LITERAL
    ) (name_part)*
    ;

name_part
    : (DOT suffix)+                                               // selected_name
    | LPAREN actual_parameter_part RPAREN                         // indexed_name
    | LPAREN discrete_range RPAREN                                // slice_name
    | APOSTROPHE attribute_designator (LPAREN expression RPAREN)? // attribute_name
    ;

next_statement
    // : (label COLON)? NEXT (label)? (WHEN condition)? SEMI
    : (label_colon)? NEXT (identifier)? (WHEN condition)? SEMI
    ;

null_statement
    // (label COLON)> NULL_ SEMI
    : (label_colon)? NULL_ SEMI
    ;

numeric_literal
    : abstract_literal
    | physical_literal
    ;

object_declaration
    : constant_declaration
    | signal_declaration
    | variable_declaration
    | file_declaration
    ;

// operator_symbol
//     : STRING_LITERAL
//     ;

package_body
    //: PACKAGE BODY identifier IS package_body_declarative_part END (PACKAGE BODY)? (simple_name)? SEMI
    : PACKAGE BODY identifier IS package_body_declarative_part END (PACKAGE BODY)? (identifier)? SEMI
    ;

package_body_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

package_body_declarative_part
    : (package_body_declarative_item)*
    ;

package_declaration
    // : PACKAGE identifier IS package_header package_declarative_part END (PACKAGE)? (simple_name)? SEMI
    : PACKAGE identifier IS package_header package_declarative_part END (PACKAGE)? (identifier)? SEMI
    ;

package_declarative_item
    : subprogram_declaration
    | subprogram_instantiation_declaration
    | package_declaration
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | signal_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | component_declaration
    | attribute_declaration
    | attribute_specification
    | disconnection_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

package_declarative_part
    : (package_declarative_item)*
    ;

package_header
    : (generic_clause (generic_map_aspect SEMI)?)?
    ;

package_instantiation_declaration
    : PACKAGE identifier IS NEW name (LPAREN generic_map_aspect RPAREN) SEMI
    ;

// package_pathname
//     : AT name DOT (identifier DOT)* identifier
//     ;

parameter_specification
    : identifier IN discrete_range
    ;

// partial_pathname
//     : (pathname_element DOT)* identifier
//     ;

// pathname_element
//     : identifier (LPAREN expression RPAREN)?
//     ;

physical_literal
    : abstract_literal (: identifier)
    ;

physical_type_definition
    : range_constraint UNITS primary_unit_declaration (secondary_unit_declaration)* END UNITS (
        // simple_name
        identifier
    )?
    ;

port_clause
    : PORT LPAREN port_list RPAREN SEMI
    ;

port_list
    : interface_list
    ;

port_map_aspect
    : PORT MAP LPAREN association_list RPAREN
    ;

prefix
    : name
    | function_call
    ;

primary
    : name
    | literal
    | aggregate
    | function_call
    | qualified_expression
    | type_conversion
    | allocator
    | LPAREN expression RPAREN
    ;

primary_unit
    : entity_declaration
    | configuration_declaration
    | package_declaration
    | package_instantiation_declaration
    | context_declaration
    ;

primary_unit_declaration
    : identifier SEMI
    ;

procedure_call
    : name (LPAREN actual_parameter_part RPAREN)?
    ;

procedure_call_statement
    //: (label COLON)? procedure_call SEMI
    : (label_colon)? procedure_call SEMI
    ;

procedure_specification
    : PROCEDURE designator subprogram_header ((PARAMETER)? LPAREN formal_parameter_list RPAREN)?
    ;

procedural_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

procedural_statement_part
    : (sequential_statement)*
    ;

process_declarative_item
    : subprogram_declaration
    | subprogram_body
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

process_declarative_part
    : (process_declarative_item)*
    ;

process_sensitivity_list
    : ALL
    | sensitivity_list
    ;

process_statement
    : (label_colon)? (POSTPONED)? PROCESS (LPAREN process_sensitivity_list RPAREN)? (IS)? process_declarative_part BEGIN process_statement_part END (
        POSTPONED
    )? PROCESS (identifier)? SEMI
    ;

process_statement_part
    : (sequential_statement)*
    ;

protected_type_body
    // : PROTECTED BODY protected_type_body_declarative_part END PROTECTED BODY (simple_name)?
    : PROTECTED BODY protected_type_body_declarative_part END PROTECTED BODY (identifier)?
    ;

protected_type_body_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

protected_type_body_declarative_part
    : (protected_type_body_declarative_item)?
    ;

protected_type_declaration
    //: PROTECTED protected_type_declarative_part END PROTECTED (simple_name)?
    : PROTECTED protected_type_declarative_part END PROTECTED (identifier)?
    ;

protected_type_declarative_item
    : subprogram_declaration
    | subprogram_instantiation_declaration
    | attribute_specification
    | use_clause
    ;

protected_type_declarative_part
    : (protected_type_declarative_item)?
    ;

protected_type_definition
    : protected_type_declaration
    | protected_type_body
    ;

qualified_expression
    : name APOSTROPHE (aggregate | LPAREN expression RPAREN)
    ;

range_decl
    : attribute_name
    | (simple_expression direction simple_expression)
    ;

range_constraint
    : RANGE range_decl
    ;

record_constraint
    : LPAREN record_element_constraint (COMMA record_element_constraint)* RPAREN
    ;

record_element_constraint
    //: simple_name element_constraint
    : identifier element_constraint
    ;

record_element_resolution
    //: simple_name resolution_indication
    : identifier resolution_indication
    ;

record_resolution
    : record_element_resolution (COMMA record_element_resolution)*
    ;

record_type_definition
    : RECORD (element_declaration)+ END RECORD (identifier)?
    ;

relation
    : shift_expression (relational_operator shift_expression)?
    ;

relational_operator
    : EQ
    | NEQ
    | LESSTHAN
    | LEQ
    | GREATERTHAN
    | GEQ
    | CONDITION_EQ
    | CONDITION_NEQ
    | CONDITION_LESSTHAN
    | CONDITION_LEQ
    | CONDITION_GREATERTHAN
    | CONDITION_GEQ
    ;

report_statement
    : (label_colon)? REPORT expression (SEVERITY expression)? SEMI
    ;

resolution_indication
    : name
    | (LPAREN element_resolution RPAREN)
    ;

return_statement
    : (label_colon)? RETURN (expression)? SEMI
    ;

scalar_type_definition
    : enumeration_type_definition
    // | integer_type_definition
    // | floating_type_definition
    | range_constraint // Equivalent to both integer_type_definition and floating_type_definition
    | physical_type_definition
    ;

secondary_unit
    : architecture_body
    | package_body
    ;

secondary_unit_declaration
    : identifier EQ physical_literal SEMI
    ;

selected_expressions
    : (expression WHEN choices COMMA)* expression WHEN choices
    ;

selected_force_assignment
    : WITH expression SELECT (QUESTION)? target LEQ FORCE (force_mode)? selected_expressions SEMI
    ;

selected_name
    : prefix DOT suffix
    ;

selected_signal_assignment
    : selected_waveform_assignment
    | selected_force_assignment
    ;

selected_variable_assignment
    : WITH expression SELECT (QUESTION)? target WALRUS selected_expressions SEMI
    ;

selected_waveform_assignment
    : WITH expression SELECT (QUESTION)? target LEQ (delay_mechanism)? selected_waveforms SEMI
    ;

selected_waveforms
    //: (waveform WHEN choices COMMA)* waveform WHEN choices
    : waveform WHEN choices (COMMA waveform WHEN choices)*
    ;

sensitivity_clause
    : ON sensitivity_list
    ;

sensitivity_list
    : name (COMMA name)*
    ;

sequence_of_statements
    : (sequential_statement)*
    ;

sequential_statement
    : wait_statement
    | assertion_statement
    | report_statement
    | signal_assignment_statement
    | variable_assignment_statement
    | procedure_call_statement
    | if_statement
    | case_statement
    | loop_statement
    | next_statement
    | exit_statement
    | return_statement
    | null_statement
    ;

shift_expression
    : simple_expression (: shift_operator simple_expression)?
    ;

shift_operator
    : SLL
    | SRL
    | SLA
    | SRA
    | ROL
    | ROR
    ;

// sign // Only used in one other rule
//     : PLUS | MINUS
//     ;

signal_assignment_statement
    : (label_colon)? (
        simple_signal_assignment
        | conditional_signal_assignment
        | selected_signal_assignment
    )
    ;

signal_declaration
    : SIGNAL identifier_list COLON (subtype_indication) (signal_kind)? (WALRUS expression)? SEMI
    ;

signal_kind
    : REGISTER
    | BUS
    ;

signal_list
    : name (COMMA name)*
    | OTHERS
    | ALL
    ;

// Signatures never get matched before other rules,
// and no-one uses them in most HDL designs
// signature
//     : ( (type_mark (COMMA type_mark)?) (RETURN type_mark)? )?
//     ;

// simple_configuration_specification // Only used in one other rule
//     : FOR component_specification binding_indication SEMI (END FOR SEMI)?
//     ;

simple_expression
    // : (sign)? term (adding_operator term)*
    : (PLUS | MINUS)? term (adding_operator term)*
    ;

simple_force_assignment
    // : target LEQ FORCE (force_mode)? expression SEMI
    : (force_mode)? expression
    ;

// simple_name
//     : identifier
//     ;

simple_release_assignment
    // : target LEQ RELEASE (force_mode)? SEMI
    : (force_mode)?
    ;

simple_signal_assignment
    // : simple_waveform_assignment
    // | simple_force_assignment
    // | simple_release_assignment
    : target LEQ (simple_waveform_assignment | simple_force_assignment | simple_release_assignment) SEMI
    ;

simple_waveform_assignment
    // : target LEQ (delay_mechanism)? waveform SEMI
    : (delay_mechanism)? waveform
    ;

simple_variable_assignment
    : target WALRUS expression SEMI
    ;

slice_name
    : prefix LPAREN discrete_range RPAREN
    ;

subprogram_body
    : subprogram_specification IS subprogram_declarative_part BEGIN subprogram_statement_part END (
        subprogram_kind
    )? (designator)? SEMI
    ;

subprogram_declaration
    : subprogram_specification SEMI
    ;

subprogram_declarative_item
    : subprogram_declaration
    | subprogram_body
    | subprogram_instantiation_declaration
    | package_declaration
    | package_body
    | package_instantiation_declaration
    | type_declaration
    | subtype_declaration
    | constant_declaration
    | variable_declaration
    | file_declaration
    | alias_declaration
    | attribute_declaration
    | attribute_specification
    | use_clause
    | group_template_declaration
    | group_declaration
    ;

subprogram_declarative_part
    : (subprogram_declarative_item)*
    ;

subprogram_header
    : (GENERIC LPAREN generic_list RPAREN (generic_map_aspect))?
    ;

subprogram_instantiation_declaration
    // : subprogram_kind identifier IS NEW name (signature)? (generic_map_aspect) SEMI
    : subprogram_kind identifier IS NEW name (generic_map_aspect) SEMI
    ;

subprogram_kind
    : PROCEDURE
    | FUNCTION
    ;

subprogram_specification
    : procedure_specification
    | function_specification
    ;

subprogram_statement_part
    : (sequential_statement)*
    ;

subtype_declaration
    : SUBTYPE identifier IS subtype_indication SEMI
    ;

subtype_indication
    // : (resolution_indication)? type_mark (constraint)?
    : (resolution_indication)? name (constraint)?
    ;

suffix
    : identifier
    | CHARACTER_LITERAL
    // | operator_symbol
    | STRING_LITERAL // Equivalent to operator_symbol
    | ALL
    ;

target
    : name
    | aggregate
    ;

term
    : factor (multiplying_operator factor)*
    ;

timeout_clause
    : FOR expression
    ;

tool_directive
    : BACKTICK identifier (graphic_character)*
    ;

type_conversion
    // : type_mark LPAREN expression RPAREN
    : name LPAREN expression RPAREN
    ;

type_declaration
    : full_type_declaration
    | incomplete_type_declaration
    ;

type_definition
    : scalar_type_definition
    | composite_type_definition
    | access_type_definition
    | file_type_definition
    | protected_type_definition
    ;

// type_mark
//     : name
//     ;

unbounded_array_definition
    : ARRAY LPAREN index_subtype_definition (COMMA index_subtype_definition)* RPAREN OF subtype_indication
    ;

use_clause
    : USE selected_name (COMMA selected_name)* SEMI
    ;

variable_assignment_statement
    : (label_colon)? (
        simple_variable_assignment
        | conditional_variable_assignment
        | selected_variable_assignment
    )
    ;

variable_declaration
    : (SHARED)? VARIABLE identifier_list COLON subtype_indication (WALRUS expression)? SEMI
    ;

wait_statement
    : (label_colon)? WAIT (sensitivity_clause)? (condition_clause)? (timeout_clause)? SEMI
    ;

waveform
    : waveform_element (COMMA waveform_element)*
    | UNAFFECTED
    ;

waveform_element
    : expression (AFTER expression)?
    | NULL_ (AFTER expression)?
    ;

//------------------------------------------Lexer-----------------------------------------

STRING_LITERAL
    : '"' (~('"' | '\n' | '\r') | '\'' | '""')* '"'
    ;

BASED_LITERAL
    : INTEGER HASH BASED_INTEGER (DOT BASED_INTEGER)? HASH (EXPONENT)?
    ;

BIT_STRING_LITERAL
    : BIT_STRING_LITERAL_BINARY
    | BIT_STRING_LITERAL_OCTAL
    | BIT_STRING_LITERAL_HEX
    ;

BIT_STRING_LITERAL_BINARY
    : 'B"' ('1' | '0' | '_')+ '"'
    ;

BIT_STRING_LITERAL_OCTAL
    : 'O"' ('7' | '6' | '5' | '4' | '3' | '2' | '1' | '0' | '_')+ '"'
    ;

BIT_STRING_LITERAL_HEX
    : 'X"' (
        'F'
        | 'E'
        | 'D'
        | 'C'
        | 'B'
        | 'A'
        | '9'
        | '8'
        | '7'
        | '6'
        | '5'
        | '4'
        | '3'
        | '2'
        | '1'
        | '0'
        | '_'
    )+ '"'
    ;

BASIC_IDENTIFIER
    : LETTER ('_' ( LETTER | DIGIT) | LETTER | DIGIT)*
    ;

EXTENDED_IDENTIFIER
    : '\\' (
        LETTER
        | '0' ..'9'
        | '&'
        | '('
        | ')'
        | '+'
        | ','
        | '-'
        | '.'
        | '/'
        | ':'
        | ';'
        | '<'
        | '='
        | '>'
        | '|'
        | ' '
        | OTHER_SPECIAL_CHARACTER
        | '\\'
        | '#'
        | '['
        | ']'
        | '_'
    )+ '\\'
    ;

LETTER
    : 'A' ..'Z'
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> skip
    ;

COMMENT
    : '--' (~'\n')* -> skip
    ;

TAB
    : ('\t')+ -> skip
    ;

SPACE
    : (' ')+ -> skip
    ;

NEWLINE
    : '\n' -> skip
    ;

CR
    : '\r' -> skip
    ;

CHARACTER_LITERAL
    : APOSTROPHE . APOSTROPHE
    ;

OTHER_SPECIAL_CHARACTER
    : '!'
    | '$'
    | '%'
    | '@'
    | '?'
    | '^'
    | '`'
    | '{'
    | '}'
    | '~'
    | ' '
    | '\u00A4'
    | '\u00A6'
    | '\u00A7'
    | '\u00A9'
    | '\u00AB'
    | '\u00AC'
    | '\u00AD'
    | '\u00AE'
    | '\u00B0'
    | '\u00B1'
    | '\u00B5'
    | '\u00B6'
    | '\u00B7'
    | '\u2116'
    | '\u00BB'
    | '\u0400' ..'\u045E'
    ;

DOUBLESTAR
    : '**'
    ;

CONDITION_OPERATOR
    : '??'
    ;

CONDITION_EQ
    : '?='
    ;

CONDITION_NEQ
    : '?/E'
    ;

CONDITION_LEQ
    : '?<='
    ;

CONDITION_LESSTHAN
    : '?<'
    ;

CONDITION_GEQ
    : '?>='
    ;

CONDITION_GREATERTHAN
    : '?>'
    ;

ASSIGN
    : '=='
    ;

AT
    : '@'
    ;

LEQ
    : '<='
    ;

GEQ
    : '>='
    ;

ARROW
    : '=>'
    ;

HASH
    : '#'
    ;

NEQ
    : '/='
    ;

WALRUS
    : ':='
    ;

BOX
    : '<>'
    ;

DBLQUOTE
    : '"'
    ;

SEMI
    : ';'
    ;

COMMA
    : ','
    ;

AMPERSAND
    : '&'
    ;

LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

LBRACKET
    : '['
    ;

RBRACKET
    : ']'
    ;

COLON
    : ':'
    ;

MUL
    : '*'
    ;

DIV
    : '/'
    ;

PLUS
    : '+'
    ;

MINUS
    : '-'
    ;

// LSHIFT
//     : '<<'
//     ;

LESSTHAN
    : '<'
    ;

// RSHIFT
//     : '>>'
//     ;

GREATERTHAN
    : '>'
    ;

EQ
    : '='
    ;

BAR
    : '|'
    ;

DOT
    : '.'
    ;

BACKSLASH
    : '\\'
    ;

BACKTICK
    : '`'
    ;

EXPONENT
    : 'E' ('+' | '-')? INTEGER (DOT INTEGER)?
    ;

HEXDIGIT
    : 'A' ..'F'
    ;

INTEGER
    : DIGIT ('_' | DIGIT)*
    ;

DIGIT
    : '0' ..'9'
    ;

BASED_INTEGER
    : EXTENDED_DIGIT ('_' | EXTENDED_DIGIT)*
    ;

EXTENDED_DIGIT
    : (DIGIT | LETTER)
    ;

APOSTROPHE
    : '\''
    ;

QUESTION
    : '?'
    ;