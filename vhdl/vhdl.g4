//
//  Copyright (C) 2010-2014  Denis Gavrish
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
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

grammar vhdl;

ABS: A B S;
ACCESS : A C C E S S;
ACROSS : A C R O S S;
AFTER : A F T E R;
ALIAS : A L I A S;
ALL : A L L;
AND : A N D;
ARCHITECTURE : A R C H I T E C T U R E;
ARRAY : A R R A Y;
ASSERT : A S S E R T;
ATTRIBUTE : A T T R I B U T E;
BEGIN : B E G I N;
BLOCK : B L O C K;
BODY : B O D Y;
BREAK : B R E A K;
BUFFER : B U F F E R;
BUS : B U S;
CASE : C A S E;
COMPONENT : C O M P O N E N T;
CONFIGURATION : C O N F I G U R A T I O N;
CONSTANT : C O N S T A N T;
DISCONNECT : D I S C O N N E C T;
DOWNTO : D O W N T O;
END : E N D;
ENTITY : E N T I T Y;
ELSE : E L S E;
ELSIF : E L S I F;
EXIT : E X I T;
FILE : F I L E;
FOR : F O R;
FUNCTION : F U N C T I O N;
GENERATE : G E N E R A T E;
GENERIC : G E N E R I C;
GROUP : G R O U P;
GUARDED : G U A R D E D;
IF : I F;
IMPURE : I M P U R E;
IN : I N;
INERTIAL : I N E R T I A L;
INOUT : I N O U T;
IS : I S;
LABEL : L A B E L;
LIBRARY : L I B R A R Y;
LIMIT : L I M I T;
LINKAGE : L I N K A G E;
LITERAL : L I T E R A L;
LOOP : L O O P;
MAP : M A P;
MOD : M O D;
NAND : N A N D;
NATURE : N A T U R E;
NEW : N E W;
NEXT : N E X T;
NOISE : N O I S E;
NOR : N O R;
NOT : N O T;
NULL : N U L L;
OF : O F;
ON : O N;
OPEN : O P E N;
OR : O R;
OTHERS : O T H E R S;
OUT : O U T;
PACKAGE : P A C K A G E;
PORT : P O R T;
POSTPONED : P O S T P O N E D;
PROCESS : P R O C E S S;
PROCEDURE : P R O C E D  U R E;
PROCEDURAL : P R O C E D U R A L;
PURE : P U R E;
QUANTITY : Q U A N T I T Y;
RANGE : R A N G E;
REVERSE_RANGE : R E V E R S E '_' R A N G E;
REJECT : R E J E C T;
REM : R E M;
RECORD : R E C O R D;
REFERENCE : R E F E R E N C E;
REGISTER : R E G I S T E R;
REPORT : R E P O R T;
RETURN : R E T U R N;
ROL : R O L;
ROR : R O R;
SELECT : S E L E C T;
SEVERITY : S E V E R I T Y;
SHARED : S H A R E D;
SIGNAL : S I G N A L;
SLA : S L A;
SLL : S L L;
SPECTRUM : S P E C T R U M;
SRA : S R A;
SRL : S R L;
SUBNATURE : S U B N A T U R E;
SUBTYPE : S U B T Y P E;
TERMINAL : T E R M I N A L;
THEN : T H E N;
THROUGH : T H R O U G H;
TO : T O;
TOLERANCE : T O L E R A N C E;
TRANSPORT : T R A N S P O R T;
TYPE : T Y P E;
UNAFFECTED : U N A F F E C T E D;
UNITS : U N I T S;
UNTIL : U N T I L;
USE : U S E;
VARIABLE : V A R I A B L E;
WAIT : W A I T;
WITH : W I T H;
WHEN : W H E N;
WHILE : W H I L E;
XNOR : X N O R;
XOR : X O R;

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');


//------------------------------------------Parser----------------------------------------

abstract_literal
   :  INTEGER
   |  REAL_LITERAL
   |  BASE_LITERAL
   ;

access_type_definition
  : ACCESS subtype_indication
  ;

across_aspect
  : identifier_list ( tolerance_aspect )? ( VARASGN expression )? ACROSS
  ;

actual_designator
  : expression
  | OPEN
  ;

actual_parameter_part
  : association_list
  ;

actual_part
  : name LPAREN actual_designator RPAREN
  | actual_designator
  ;

adding_operator
  : PLUS
  | MINUS
  | AMPERSAND
  ;

aggregate
  : LPAREN element_association ( COMMA element_association )* RPAREN
  ;

alias_declaration
  : ALIAS alias_designator ( COLON alias_indication )? IS
    name ( signature )? SEMI
  ;

alias_designator
  : identifier
  | CHARACTER_LITERAL
  | STRING_LITERAL
  ;

alias_indication
  : subnature_indication
  | subtype_indication
  ;

allocator
  : NEW ( qualified_expression | subtype_indication )
  ;

architecture_body
  : ARCHITECTURE identifier OF identifier IS
    architecture_declarative_part
    BEGIN
    architecture_statement_part
    END ( ARCHITECTURE )? ( identifier )? SEMI
  ;

architecture_declarative_part
  : ( block_declarative_item )*
  ;

architecture_statement
  : block_statement
  | process_statement
  | ( label_colon )? concurrent_procedure_call_statement
  | ( label_colon )? concurrent_assertion_statement
  | ( label_colon )? ( POSTPONED )? concurrent_signal_assignment_statement
  | component_instantiation_statement
  | generate_statement
  | concurrent_break_statement
  | simultaneous_statement
  ;

architecture_statement_part
  : ( architecture_statement )*
  ;

array_nature_definition
  : unconstrained_nature_definition
  | constrained_nature_definition
  ;

array_type_definition
  : unconstrained_array_definition
  | constrained_array_definition
  ;

assertion
  : ASSERT condition ( REPORT expression )? ( SEVERITY expression )?
  ;

assertion_statement
  : ( label_colon )? assertion SEMI
  ;

association_element
  : ( formal_part ARROW )? actual_part
  ;

association_list
  : association_element ( COMMA association_element )*
  ;

attribute_declaration
  : ATTRIBUTE label_colon name SEMI
  ;

// Need to add several tokens here, for they are both, VHDLAMS reserved words
// and attribute names.
// (25.2.2004, e.f.)
attribute_designator
  : identifier
  | RANGE
  | REVERSE_RANGE
  | ACROSS
  | THROUGH
  | REFERENCE
  | TOLERANCE
  ;

attribute_specification
  : ATTRIBUTE attribute_designator OF entity_specification IS expression SEMI
  ;

base_unit_declaration
  : identifier SEMI
  ;

binding_indication
  : ( USE entity_aspect )? ( generic_map_aspect )? ( port_map_aspect )?
  ;

block_configuration
  : FOR block_specification
    ( use_clause )*
    ( configuration_item )*
    END FOR SEMI
  ;

block_declarative_item
  : subprogram_declaration
  | subprogram_body
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
  | step_limit_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  | nature_declaration
  | subnature_declaration
  | quantity_declaration
  | terminal_declaration
  ;

block_declarative_part
  : ( block_declarative_item )*
  ;

block_header
  : ( generic_clause ( generic_map_aspect SEMI )? )?
    ( port_clause ( port_map_aspect SEMI )? )?
  ;

block_specification
  : identifier ( LPAREN index_specification RPAREN )?
  | name
  ;

block_statement
  : label_colon BLOCK ( LPAREN expression RPAREN )? ( IS )?
    block_header
    block_declarative_part BEGIN
    block_statement_part 
    END BLOCK ( identifier )? SEMI
  ;

block_statement_part
  : ( architecture_statement )*
  ;

branch_quantity_declaration
  : QUANTITY ( across_aspect )?
    ( through_aspect )? terminal_aspect SEMI
  ;

break_element
  : ( break_selector_clause )? name ARROW expression
  ;

break_list
  : break_element ( COMMA break_element )*
  ;

break_selector_clause
  : FOR name USE
  ;

break_statement
  : ( label_colon )? BREAK ( break_list )? ( WHEN condition )? SEMI
  ;

case_statement
  : ( label_colon )? CASE expression IS
    ( case_statement_alternative )+
    END CASE ( identifier )? SEMI
  ;

case_statement_alternative
  : WHEN choices ARROW sequence_of_statements
  ;

choice
  : identifier
  | discrete_range
  | simple_expression
  | OTHERS
  ;

choices
  : choice ( BAR choice )*
  ;

component_configuration
  : FOR component_specification
    ( binding_indication SEMI )?
    ( block_configuration )?
    END FOR SEMI
  ;

component_declaration
  : COMPONENT identifier ( IS )?
    ( generic_clause )?
    ( port_clause )?
    END COMPONENT ( identifier )? SEMI
  ;

component_instantiation_statement
  : label_colon instantiated_unit
    ( generic_map_aspect )?
    ( port_map_aspect )? SEMI
  ;

component_specification
  : instantiation_list COLON name
  ;

composite_nature_definition
  : array_nature_definition
  | record_nature_definition
  ;

composite_type_definition
  : array_type_definition
  | record_type_definition
  ;

concurrent_assertion_statement
  : ( label_colon )? ( POSTPONED )? assertion SEMI
  ;

concurrent_break_statement
  : ( label_colon )? BREAK ( break_list )? ( sensitivity_clause )?
    ( WHEN condition )? SEMI
  ;

concurrent_procedure_call_statement
  : ( label_colon )? ( POSTPONED )? procedure_call SEMI
  ;

concurrent_signal_assignment_statement
  : ( label_colon )? ( POSTPONED )?
    ( conditional_signal_assignment | selected_signal_assignment )
  ;

condition
  : expression
  ;

condition_clause
  : UNTIL condition
  ;

conditional_signal_assignment
  : target LE opts conditional_waveforms SEMI
  ;

conditional_waveforms
  : waveform ( WHEN condition (ELSE conditional_waveforms)?)?
  ;

configuration_declaration
  : CONFIGURATION identifier OF name IS
    configuration_declarative_part
    block_configuration
    END ( CONFIGURATION )? ( identifier )? SEMI
  ;

configuration_declarative_item
  : use_clause
  | attribute_specification
  | group_declaration
  ;

configuration_declarative_part
  : ( configuration_declarative_item )*
  ;

configuration_item
  : block_configuration
  | component_configuration
  ;

configuration_specification
  : FOR component_specification binding_indication SEMI
  ;

constant_declaration
  : CONSTANT identifier_list COLON subtype_indication
    ( VARASGN expression )? SEMI
  ;

constrained_array_definition
  : ARRAY index_constraint OF subtype_indication
  ;

constrained_nature_definition
  : ARRAY index_constraint OF subnature_indication
  ;

constraint
  : range_constraint
  | index_constraint
  ;

context_clause
  : ( context_item )*
  ;

context_item
  : library_clause
  | use_clause
  ;

delay_mechanism
  : TRANSPORT
  | ( REJECT expression )? INERTIAL
  ;

design_file
  : ( design_unit )* EOF
  ;

design_unit
  : context_clause library_unit
  ;

designator
  : identifier
  | STRING_LITERAL
  ;

direction
  : TO
  | DOWNTO
  ;

disconnection_specification
  : DISCONNECT guarded_signal_specification AFTER expression SEMI
  ;

discrete_range
  : range_decl
  | subtype_indication
  ;

element_association
  : (  choices ARROW )? expression
  ;

element_declaration
  : identifier_list COLON element_subtype_definition SEMI
  ;

element_subnature_definition
  : subnature_indication
  ;

element_subtype_definition
  : subtype_indication
  ;

entity_aspect
  : ENTITY name ( LPAREN identifier RPAREN )?
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
  | NATURE
  | SUBNATURE
  | QUANTITY
  | TERMINAL
  ;

entity_class_entry
  : entity_class ( BOX )?
  ;

entity_class_entry_list
  : entity_class_entry ( COMMA entity_class_entry )*
  ;

entity_declaration
  : ENTITY identifier IS entity_header
    entity_declarative_part
    ( BEGIN entity_statement_part )?
    END ( ENTITY )? ( identifier )? SEMI
  ;

entity_declarative_item
  : subprogram_declaration
  | subprogram_body
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
  | step_limit_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  | nature_declaration
  | subnature_declaration
  | quantity_declaration
  | terminal_declaration
  ;

entity_declarative_part
  : ( entity_declarative_item )*
  ;

entity_designator
  : entity_tag ( signature )?
  ;

entity_header
  : ( generic_clause )?
    ( port_clause )?
  ;

entity_name_list
  : entity_designator ( COMMA entity_designator )*
  | OTHERS
  | ALL
  ;

entity_specification
  : entity_name_list COLON entity_class
  ;

entity_statement
  :  concurrent_assertion_statement
  |  process_statement
  | concurrent_procedure_call_statement
  ;

entity_statement_part
  : ( entity_statement )*
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
  : LPAREN enumeration_literal ( COMMA enumeration_literal )* RPAREN
  ;

exit_statement
  : ( label_colon )? EXIT ( identifier )? ( WHEN condition )? SEMI
  ;

// NOTE that NAND/NOR are in (...)* now (used to be in (...)?).
// (21.1.2004, e.f.)
expression
  : relation ( : logical_operator relation )*
  ;

factor
  : primary ( : DOUBLESTAR primary )?
  | ABS primary
  | NOT primary
  ;

file_declaration
  : FILE identifier_list COLON subtype_indication
    ( file_open_information )? SEMI
  ;

file_logical_name
  : expression
  ;

file_open_information
  : ( OPEN expression )? IS file_logical_name
  ;

file_type_definition
  : FILE OF subtype_indication
  ;

formal_parameter_list
  : interface_list
  ;

formal_part
  : identifier
   | identifier LPAREN explicit_range  RPAREN 
  ;

free_quantity_declaration
  : QUANTITY identifier_list COLON subtype_indication
    ( VARASGN expression )? SEMI
  ;

generate_statement
  : label_colon generation_scheme
    GENERATE
    ( ( block_declarative_item )* BEGIN )?
    ( architecture_statement )*
    END GENERATE ( identifier )? SEMI
  ;

generation_scheme
  : FOR parameter_specification
  | IF condition
  ;

generic_clause
  : GENERIC LPAREN generic_list RPAREN SEMI
  ;

generic_list
  : interface_constant_declaration (SEMI interface_constant_declaration)*
  ;

generic_map_aspect
  : GENERIC MAP LPAREN association_list RPAREN
  ;

group_constituent
  : name
  | CHARACTER_LITERAL
  ;

group_constituent_list
  : group_constituent ( COMMA group_constituent )*
  ;

group_declaration
  : GROUP label_colon name
    LPAREN group_constituent_list RPAREN SEMI
  ;

group_template_declaration
  : GROUP identifier IS LPAREN entity_class_entry_list RPAREN SEMI
  ;

guarded_signal_specification
  : signal_list COLON name
  ;

identifier
  : BASIC_IDENTIFIER
  | EXTENDED_IDENTIFIER
  ;

identifier_list
  : identifier ( COMMA identifier )*
  ;

if_statement
  : ( label_colon )? IF condition THEN
    sequence_of_statements
    ( ELSIF condition THEN sequence_of_statements )* 
    ( ELSE sequence_of_statements )?
    END IF ( identifier )? SEMI
  ;

index_constraint
  : LPAREN discrete_range ( COMMA discrete_range )* RPAREN
  ;

index_specification
  : discrete_range
  | expression
  ;

index_subtype_definition
  : name RANGE BOX
  ;

instantiated_unit
  : ( COMPONENT )? name
  | ENTITY name ( LPAREN identifier RPAREN )?
  | CONFIGURATION name
  ;

instantiation_list
  : identifier ( COMMA identifier )*
  | OTHERS
  | ALL
  ;

interface_constant_declaration
  : ( CONSTANT )? identifier_list COLON ( IN )? subtype_indication
    ( VARASGN expression )?
  ;

interface_declaration
  :  interface_constant_declaration
  |  interface_signal_declaration
  | interface_variable_declaration
  | interface_file_declaration
  | interface_terminal_declaration
  | interface_quantity_declaration
  ;

interface_element
  : interface_declaration
  ;

interface_file_declaration
  : FILE identifier_list COLON subtype_indication
  ;

interface_signal_list 
  : interface_signal_declaration ( SEMI interface_signal_declaration )*
  ;

interface_port_list 
  : interface_port_declaration ( SEMI interface_port_declaration )*
  ;

interface_list
  : interface_element ( SEMI interface_element )*
  ;

interface_quantity_declaration
  : QUANTITY identifier_list COLON ( IN | OUT )? subtype_indication
    ( VARASGN expression )?
  ;

interface_port_declaration
  : identifier_list COLON ( signal_mode )? subtype_indication
    ( BUS )? ( VARASGN expression )?
  ;

interface_signal_declaration
  : SIGNAL identifier_list COLON ( signal_mode )? subtype_indication
    ( BUS )? ( VARASGN expression )?
  ;

interface_terminal_declaration
  : TERMINAL identifier_list COLON subnature_indication
  ;

interface_variable_declaration
  : ( VARIABLE )? identifier_list COLON
    ( signal_mode )? subtype_indication ( VARASGN expression )?
  ;

iteration_scheme
  : WHILE condition
  | FOR parameter_specification
  ;

label_colon
  : identifier COLON
  ;

library_clause
  : LIBRARY logical_name_list SEMI
  ;

library_unit
  : secondary_unit  | primary_unit
  ;

literal
  : NULL
  | BIT_STRING_LITERAL
  | STRING_LITERAL
  | enumeration_literal
  | numeric_literal
  ;

logical_name
  : identifier
  ;

logical_name_list
  : logical_name ( COMMA logical_name )*
  ;

logical_operator
  : AND
  | OR
  | NAND
  | NOR
  | XOR
  | XNOR
  ;

loop_statement
  : ( label_colon )? ( iteration_scheme )?
    LOOP
    sequence_of_statements 
    END LOOP ( identifier )? SEMI
  ;

signal_mode
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


// was
//   name
//     : simple_name
//     | operator_symbol
//     | selected_name
//     | indexed_name
//     | slice_name
//     | attribute_name
//     ;
// changed to avoid left-recursion to name (from selected_name, indexed_name,
// slice_name, and attribute_name, respectively)
// (2.2.2004, e.f.) + (12.07.2017, o.p.)
name
  : ( identifier | STRING_LITERAL ) ( name_part )*
  ;

name_part
  : selected_name_part
  | function_call_or_indexed_name_part
  | slice_name_part
  | attribute_name_part
   ;

selected_name
   : identifier (DOT suffix)*
   ;

selected_name_part
  : ( DOT suffix )+
  ;

function_call_or_indexed_name_part
  : LPAREN actual_parameter_part RPAREN
  ;

slice_name_part
  : LPAREN discrete_range RPAREN
  ;

attribute_name_part
  : ( signature )? APOSTROPHE attribute_designator ( LPAREN expression RPAREN )?
  ;

nature_declaration
  : NATURE identifier IS nature_definition SEMI
  ;

nature_definition
  : scalar_nature_definition
  | composite_nature_definition
  ;

nature_element_declaration
  : identifier_list COLON element_subnature_definition
  ;

next_statement
  : ( label_colon )? NEXT ( identifier )? ( WHEN condition )? SEMI
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
  | terminal_declaration
  | quantity_declaration
  ;

opts
  : ( GUARDED )? ( delay_mechanism )?
  ;

package_body
  : PACKAGE BODY identifier IS
    package_body_declarative_part
    END ( PACKAGE BODY )? ( identifier )? SEMI
  ;

package_body_declarative_item
  : subprogram_declaration
  | subprogram_body
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | file_declaration
  | alias_declaration
  | use_clause
  | group_template_declaration
  | group_declaration
  ;

package_body_declarative_part
  : ( package_body_declarative_item )*
  ;

package_declaration
  : PACKAGE identifier IS
    package_declarative_part
    END ( PACKAGE )? ( identifier )? SEMI
  ;

package_declarative_item
  : subprogram_declaration
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
  | nature_declaration
  | subnature_declaration
  | terminal_declaration
  ;

package_declarative_part
  : ( package_declarative_item )*
  ;

parameter_specification
  : identifier IN discrete_range
  ;

physical_literal
  : abstract_literal (: identifier)
  ;

physical_type_definition
  : range_constraint UNITS base_unit_declaration
    ( secondary_unit_declaration )* 
    END UNITS ( identifier )?
  ;

port_clause
  : PORT LPAREN port_list RPAREN SEMI
  ;

port_list
  : interface_port_list
  ;

port_map_aspect
  : PORT MAP LPAREN association_list RPAREN
  ;

primary
  : literal
  | qualified_expression
  | LPAREN expression RPAREN
  | allocator
  | aggregate
  | name
  ;

primary_unit
  : entity_declaration
  | configuration_declaration
  | package_declaration
  ;

procedural_declarative_item
  : subprogram_declaration
  | subprogram_body
  | type_declaration
  | subtype_declaration
  | constant_declaration
  | variable_declaration
  | alias_declaration
  | attribute_declaration
  | attribute_specification
  | use_clause
  | group_template_declaration
  | group_declaration
  ;

procedural_declarative_part
  : ( procedural_declarative_item )*
  ;

procedural_statement_part
  : ( sequential_statement )*
  ;

procedure_call
  : selected_name ( LPAREN actual_parameter_part RPAREN )?
  ;

procedure_call_statement
  : ( label_colon )? procedure_call SEMI
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
  : ( process_declarative_item )*
  ;

process_statement
  : ( label_colon )? ( POSTPONED )? PROCESS
    ( LPAREN sensitivity_list RPAREN )? ( IS )?
    process_declarative_part
    BEGIN
    process_statement_part 
    END ( POSTPONED )? PROCESS ( identifier )? SEMI
  ;

process_statement_part
  : ( sequential_statement )*
  ;

qualified_expression
  : subtype_indication APOSTROPHE  ( aggregate | LPAREN expression RPAREN )
  ;

quantity_declaration
  : free_quantity_declaration
  | branch_quantity_declaration
  | source_quantity_declaration
  ;

quantity_list
  : name ( COMMA name )*
  | OTHERS
  | ALL
  ;

quantity_specification
  : quantity_list COLON name
  ;

range_decl
  : explicit_range
  | name
  ;

explicit_range
  : simple_expression ( direction simple_expression )?
  ;

range_constraint
  : RANGE range_decl
  ;

record_nature_definition
  : RECORD ( nature_element_declaration )+
    END RECORD ( identifier )?
  ;

record_type_definition
  : RECORD ( element_declaration )+
    END RECORD ( identifier )?
  ;

relation
  : shift_expression
    ( : relational_operator shift_expression )?
  ;

relational_operator
  : EQ
  | NEQ
  | LOWERTHAN
  | LE
  | GREATERTHAN
  | GE
  ;

report_statement
  : ( label_colon )? REPORT expression ( SEVERITY expression )? SEMI
  ;

return_statement
  : ( label_colon )? RETURN ( expression )? SEMI
  ;

scalar_nature_definition
  : name ACROSS name THROUGH name REFERENCE
  ;

scalar_type_definition
  : physical_type_definition
  | enumeration_type_definition
  | range_constraint
  ;

secondary_unit
  : architecture_body
  | package_body
  ;

secondary_unit_declaration
  : identifier EQ physical_literal SEMI
  ;

selected_signal_assignment
  : WITH expression SELECT target LE opts selected_waveforms SEMI
  ;

selected_waveforms
  : waveform WHEN choices ( COMMA waveform WHEN choices )*
  ;

sensitivity_clause
  : ON sensitivity_list
  ;

sensitivity_list
  : name ( COMMA name )*
  ;

sequence_of_statements
  : ( sequential_statement )*
  ;

sequential_statement
  : wait_statement
  | assertion_statement
  | report_statement
  | signal_assignment_statement
  | variable_assignment_statement
  | if_statement
  | case_statement
  | loop_statement
  | next_statement
  | exit_statement
  | return_statement
  | ( label_colon )? NULL SEMI
  | break_statement
  | procedure_call_statement
  ;

shift_expression
  : simple_expression
    ( : shift_operator simple_expression )?
  ;

shift_operator
  : SLL
  | SRL
  | SLA
  | SRA
  | ROL
  | ROR
  ;

signal_assignment_statement
  : ( label_colon )?
    target LE ( delay_mechanism )? waveform SEMI
  ;

signal_declaration
  : SIGNAL identifier_list COLON
    subtype_indication ( signal_kind )? ( VARASGN expression )? SEMI
  ;

signal_kind
  : REGISTER
  | BUS
  ;

signal_list
  : name ( COMMA name )*
  | OTHERS
  | ALL
  ;

signature
  : LBRACKET ( name ( COMMA name )* )? ( RETURN name )? RBRACKET
  ;

// NOTE that sign is applied to first operand only (LRM does not permit
// `a op -b' - use `a op (-b)' instead).
// (3.2.2004, e.f.)
simple_expression
  : ( PLUS | MINUS )? term ( : adding_operator term )*
  ;

simple_simultaneous_statement
  : ( label_colon )?
    simple_expression ASSIGN simple_expression ( tolerance_aspect )? SEMI
  ;

simultaneous_alternative
  : WHEN choices ARROW simultaneous_statement_part
  ;

simultaneous_case_statement
  : ( label_colon )? CASE expression USE
    ( simultaneous_alternative )+ 
    END CASE ( identifier )? SEMI
  ;

simultaneous_if_statement
  : ( label_colon )? IF condition USE
    simultaneous_statement_part
    ( ELSIF condition USE simultaneous_statement_part )*
    ( ELSE simultaneous_statement_part )?
    END USE ( identifier )? SEMI
  ;

simultaneous_procedural_statement
  : ( label_colon )? PROCEDURAL ( IS )?
    procedural_declarative_part BEGIN
    procedural_statement_part 
    END PROCEDURAL ( identifier )? SEMI
  ;

simultaneous_statement
  : simple_simultaneous_statement
  | simultaneous_if_statement
  | simultaneous_case_statement
  | simultaneous_procedural_statement
  | ( label_colon )? NULL SEMI
  ;

simultaneous_statement_part
  : ( simultaneous_statement )*
  ;

source_aspect
  : SPECTRUM simple_expression COMMA simple_expression
  | NOISE simple_expression
  ;

source_quantity_declaration
  : QUANTITY identifier_list COLON subtype_indication source_aspect SEMI
  ;

step_limit_specification
  : LIMIT quantity_specification WITH expression SEMI
  ;

subnature_declaration
  : SUBNATURE identifier IS subnature_indication SEMI
  ;

subnature_indication
  : name ( index_constraint )? 
    ( TOLERANCE expression ACROSS expression THROUGH )?
  ;

subprogram_body
  : subprogram_specification IS
    subprogram_declarative_part
    BEGIN
    subprogram_statement_part
    END ( subprogram_kind )? ( designator )? SEMI
  ;

subprogram_declaration
  : subprogram_specification SEMI
  ;

subprogram_declarative_item
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

subprogram_declarative_part
  : ( subprogram_declarative_item )*
  ;

subprogram_kind
  : PROCEDURE
  | FUNCTION
  ;

subprogram_specification
  : procedure_specification
  | function_specification
  ;

procedure_specification
  : PROCEDURE designator ( LPAREN formal_parameter_list RPAREN )?
  ;

function_specification
  : ( PURE | IMPURE )? FUNCTION designator
    ( LPAREN formal_parameter_list RPAREN )? RETURN subtype_indication
  ;

subprogram_statement_part
  : ( sequential_statement )*
  ;

subtype_declaration
  : SUBTYPE identifier IS subtype_indication SEMI
  ;

// VHDLAMS 1076.1-1999 declares first name as optional. Here, second name
// is made optional to prevent antlr nondeterminism.
// (9.2.2004, e.f.)
subtype_indication
  : selected_name ( selected_name )? ( constraint )? ( tolerance_aspect )?
  ;

suffix
  : identifier
  | CHARACTER_LITERAL
  | STRING_LITERAL
  | ALL
  ;

target
  : name
  | aggregate
  ;

term
  : factor ( : multiplying_operator factor )*
  ;

terminal_aspect
  : name ( TO name )?
  ;

terminal_declaration
  : TERMINAL identifier_list COLON subnature_indication SEMI
  ;

through_aspect
  : identifier_list ( tolerance_aspect )? ( VARASGN expression )? THROUGH
  ;

timeout_clause
  : FOR expression
  ;

tolerance_aspect
  : TOLERANCE expression
  ;

type_declaration
  : TYPE identifier ( IS type_definition )? SEMI
  ;

type_definition
  : scalar_type_definition
  | composite_type_definition
  | access_type_definition
  | file_type_definition
  ;

unconstrained_array_definition
  : ARRAY LPAREN index_subtype_definition ( COMMA index_subtype_definition )*
    RPAREN OF subtype_indication
  ;

unconstrained_nature_definition
  : ARRAY LPAREN index_subtype_definition ( COMMA index_subtype_definition )*
    RPAREN OF subnature_indication
  ;

use_clause
  : USE selected_name ( COMMA selected_name )* SEMI
  ;

variable_assignment_statement
  : ( label_colon )? target VARASGN expression SEMI
  ;

variable_declaration
  : ( SHARED )? VARIABLE identifier_list COLON
    subtype_indication ( VARASGN expression )? SEMI
  ;

wait_statement
  : ( label_colon )? WAIT ( sensitivity_clause )? 
    ( condition_clause )? ( timeout_clause )? SEMI
  ;

waveform
  : waveform_element ( COMMA waveform_element )*
  | UNAFFECTED
  ;   

waveform_element
  : expression ( AFTER expression )?
  ;

//------------------------------------------Lexer-----------------------------------------
BASE_LITERAL
// INTEGER must be checked to be between and including 2 and 16 (included) i.e.
// INTEGER >=2 and INTEGER <=16
// A Based integer (a number without a . such as 3) should not have a negative exponent
// A Based fractional number with a . i.e. 3.0 may have a negative exponent
// These should be checked in the Visitor/Listener whereby an appropriate error message
// should be given
   :  INTEGER '#' BASED_INTEGER ('.'BASED_INTEGER)? '#' (EXPONENT)?
   ;

BIT_STRING_LITERAL
  : BIT_STRING_LITERAL_BINARY
  | BIT_STRING_LITERAL_OCTAL
  | BIT_STRING_LITERAL_HEX
  ;

BIT_STRING_LITERAL_BINARY
    :   ('b'|'B') '"' ('1' | '0' | '_')+ '"'
    ;

BIT_STRING_LITERAL_OCTAL
    :   ('o'|'O') '"' ('7' |'6' |'5' |'4' |'3' |'2' |'1' | '0' | '_')+ '"'
    ;

BIT_STRING_LITERAL_HEX
    :   ('x'|'X') '"' ( 'f' |'e' |'d' |'c' |'b' |'a' | 'F' |'E' |'D' |'C' |'B' |'A' | '9' | '8' | '7' |'6' |'5' |'4' |'3' |'2' |'1' | '0' | '_')+ '"'
    ;

REAL_LITERAL
   :    INTEGER '.' INTEGER  ( EXPONENT )?;

BASIC_IDENTIFIER
   :   LETTER ( '_' ( LETTER | DIGIT ) | LETTER | DIGIT )*
   ;
   
EXTENDED_IDENTIFIER
  : '\\' ( 'a'..'z' | '0'..'9' | '&' | '\'' | '(' | ')'
    | '+' | ',' | '-' | '.' | '/' | ':' | ';' | '<' | '=' | '>' | '|'
    | ' ' | OTHER_SPECIAL_CHARACTER | '\\'
    | '#' | '[' | ']' | '_' )+ '\\'
  ;

LETTER	
  :  'a'..'z' | 'A'..'Z'
  ;

COMMENT
  : '--' ( ~'\n' )* 
  -> skip
  ;

TAB
  : ( '\t' )+ -> skip 
  ;

SPACE
  : ( ' ' )+ -> skip 
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

STRING_LITERAL
  : '"' (~('"'|'\n'|'\r') | '""')* '"'
  ;

OTHER_SPECIAL_CHARACTER
  : '!' | '$' | '%' | '@' | '?' | '^' | '`' | '{' | '}' | '~'
  | ' ' | 'Ў' | 'ў' | 'Ј' | '¤' | 'Ґ' | '¦' | '§'
  | 'Ё' | '©' | 'Є' | '«' | '¬' | '­' | '®' | 'Ї'
  | '°' | '±' | 'І' | 'і' | 'ґ' | 'µ' | '¶' | '·'
  | 'ё' | '№' | 'є' | '»' | 'ј' | 'Ѕ' | 'ѕ' | 'ї'
  | 'А' | 'Б' | 'В' | 'Г' | 'Д' | 'Е' | 'Ж' | 'З'
  | 'И' | 'Й' | 'К' | 'Л' | 'М' | 'Н' | 'О' | 'П'
  | 'Р' | 'С' | 'Т' | 'У' | 'Ф' | 'Х' | 'Ц' | 'Ч'
  | 'Ш' | 'Щ' | 'Ъ' | 'Ы' | 'Ь' | 'Э' | 'Ю' | 'Я'
  | 'а' | 'б' | 'в' | 'г' | 'д' | 'е' | 'ж' | 'з'
  | 'и' | 'й' | 'к' | 'л' | 'м' | 'н' | 'о' | 'п'
  | 'р' | 'с' | 'т' | 'у' | 'ф' | 'х' | 'ц' | 'ч'
  | 'ш' | 'щ' | 'ъ' | 'ы' | 'ь' | 'э' | 'ю' | 'я'
  ;


DOUBLESTAR    : '**'  ;
ASSIGN        : '=='  ;
LE            : '<='  ;
GE            : '>='  ;
ARROW         : '=>'  ;
NEQ           : '/='  ;
VARASGN       : ':='  ;
BOX           : '<>'  ;
DBLQUOTE      : '"'   ;
SEMI          : ';'   ;
COMMA         : ','   ;
AMPERSAND     : '&'   ;
LPAREN        : '('   ;
RPAREN        : ')'   ;
LBRACKET      : '['   ;
RBRACKET      : ']'   ;
COLON         : ':'   ;
MUL           : '*'   ;
DIV           : '/'   ;
PLUS          : '+'   ;
MINUS         : '-'   ;
LOWERTHAN     : '<'   ;
GREATERTHAN   : '>'   ;
EQ            : '='   ;
BAR           : '|'   ;
DOT           : '.'   ;
BACKSLASH     : '\\'  ;
  

EXPONENT
  :  ('E'|'e') ( '+' | '-' )? INTEGER
  ;


HEXDIGIT
    :	('A'..'F'|'a'..'f')
    ;


INTEGER
  :  DIGIT ( '_' | DIGIT )*
  ;

DIGIT
  :  '0'..'9'
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
