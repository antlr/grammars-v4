/*
MIT License

Copyright (c) 2022 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

parser grammar SystemVerilogParser;
options { tokenVocab = SystemVerilogLexer; }

// A.1.1 Library source text
library_text
	: library_description* EOF
	;
library_description
	: library_declaration
	| include_statement
	| config_declaration
	| ';'
	;
library_declaration
	: 'library' library_identifier file_path_spec ( ',' file_path_spec )* library_incdir? ';'
	;
library_incdir
	: '-incdir' file_path_spec ( ',' file_path_spec )*
	;
include_statement
	: 'include' file_path_spec ';'
	;
file_path_spec
	: FILE_PATH_SPEC
	;
// A.1.2 SystemVerilog source text
source_text
	: timeunits_declaration? description* EOF
	;
description
	: module_declaration
	| udp_declaration
	| interface_declaration
	| program_declaration
	| package_declaration
	| attribute_instance* package_item
	| attribute_instance* bind_directive
	| config_declaration
	;
module_header
	: attribute_instance* module_keyword lifetime? module_identifier package_import_declaration* parameter_port_list? list_of_port_declarations? ';'
	;
module_declaration
	: module_header timeunits_declaration? module_item* 'endmodule' module_name?
	| attribute_instance* module_keyword lifetime? module_identifier '(' '.*' ')' ';' timeunits_declaration? module_item* 'endmodule' module_name?
	| 'extern' module_header
	;
module_name
	: ':' module_identifier
	;
module_keyword
	: 'module'
	| 'macromodule'
	;
interface_declaration
	: interface_header timeunits_declaration? interface_item* 'endinterface' interface_name?
	| attribute_instance* 'interface' interface_identifier '(' '.*' ')' ';' timeunits_declaration? interface_item* 'endinterface' interface_name?
	| 'extern' interface_header
	;
interface_name
	: ':' interface_identifier
	;
interface_header
	: attribute_instance* 'interface' lifetime? interface_identifier package_import_declaration* parameter_port_list? list_of_port_declarations? ';'
	;
program_declaration
	: program_header timeunits_declaration? program_item* 'endprogram' program_name?
	| attribute_instance* 'program' program_identifier '(' '.*' ')' ';' timeunits_declaration? program_item* 'endprogram' program_name?
	| 'extern' program_header
	;
program_name
	: ':' program_identifier
	;
program_header
	: attribute_instance* 'program' lifetime? program_identifier package_import_declaration* parameter_port_list? list_of_port_declarations? ';'
	;
checker_declaration
	: 'checker' checker_identifier checker_ports? ';' checker_decl_item* 'endchecker' checker_name?
	;
checker_name
	: ':' checker_identifier
	;
checker_ports
	: '(' checker_port_list? ')'
	;
checker_decl_item
	: attribute_instance* checker_item
	;
class_declaration
	: 'virtual'? 'class' lifetime? class_identifier parameter_port_list? class_extension? class_implementation? ';' class_item* 'endclass' class_name?
	;
class_name
	: ':' class_identifier
	;
class_extension
	: 'extends' class_type arg_list?
	;
class_implementation
	: 'implements' interface_class_type ( ',' interface_class_type )*
	;
interface_class_type
	: ps_identifier parameter_value_assignment?
	;
interface_class_declaration
	: 'interface' 'class' class_identifier parameter_port_list? interface_class_extension? ';' interface_class_item* 'endclass' class_name?
	;
interface_class_extension
	: 'extends' interface_class_type ( ',' interface_class_type )*
	;
interface_class_item
	: type_declaration
	| attribute_instance* interface_class_method
	| local_parameter_declaration ';'
	| parameter_declaration ';'
	| ';'
	;
interface_class_method
	: 'pure' 'virtual' method_prototype ';'
	;
package_declaration
	: attribute_instance* 'package' lifetime? package_identifier ';' timeunits_declaration? pkg_decl_item* 'endpackage' package_name?
	;
package_name
	: ':' package_identifier
	;
pkg_decl_item
	: attribute_instance* package_item
	;
timeunits_declaration
	: 'timeunit' time_literal ( '/' time_literal )? ';'
	| 'timeprecision' time_literal ';' ( 'timeunit' time_literal ';' )?
	| 'timeunit' time_literal ';' 'timeprecision' time_literal ';'
	;
// A.1.3 Module parameters and ports
parameter_port_list
	: '#' '(' list_of_param_assignments ( ',' parameter_port_declaration )* ')'
	| '#' '(' parameter_port_declaration ( ',' parameter_port_declaration )* ')'
	| '#' '(' ')'
	;
parameter_port_declaration
	: parameter_declaration
	| local_parameter_declaration
	| data_type list_of_param_assignments
	| 'type' list_of_type_assignments
	;
list_of_port_declarations
	: '(' port_decl ( ',' port_decl )* ')'
	| '(' port ( ',' port )+ ')'
	| '(' port_implicit ')'
	| '(' ')'
	;
port_decl
	: attribute_instance* ansi_port_declaration
	;
port_declaration
	: attribute_instance* inout_declaration
	| attribute_instance* input_declaration
	| attribute_instance* output_declaration
	| attribute_instance* ref_declaration
	| attribute_instance* interface_port_declaration
	;
port
	: port_implicit?
	;
port_implicit
	: port_expression
	;
port_expression
	: port_identifier constant_bit_select? '[' constant_indexed_range ']'
	| port_identifier const_member_select+ ( '[' constant_part_select_range ']' )?
	| '{' port_reference ( ',' port_reference )* '}'
	;
port_reference
	: port_identifier constant_select?
	;
port_direction
	: 'input'
	| 'output'
	| 'inout'
	| 'ref'
	;
ansi_port_declaration
	: 'interface' ( '.' modport_identifier )? port_identifier unpacked_dimension* ( '=' constant_expression )?
	| interface_identifier '.' modport_identifier port_identifier unpacked_dimension* ( '=' constant_expression )?
	| port_direction? '.' port_identifier '(' expression? ')'
	| port_direction? 'interconnect' implicit_data_type? port_identifier unpacked_dimension* ( '=' constant_expression )?
	| port_direction? 'var' data_type_or_implicit? port_identifier variable_dimension* ( '=' constant_expression )?
	| port_direction? data_type? port_identifier variable_dimension* ( '=' constant_expression )?
	| port_direction? implicit_data_type port_identifier unpacked_dimension* ( '=' constant_expression )?
	| port_direction? net_type data_type_or_implicit? port_identifier unpacked_dimension* ( '=' constant_expression )?
	;
// A.1.4 Module items
elaboration_system_task
	: '$fatal' fatal_arg_list? ';'
	| '$error' arg_list? ';'
	| '$warning' arg_list? ';'
	| '$info' arg_list? ';'
	;
fatal_arg_list
	: '(' finish_number ( ',' list_of_arguments )? ')'
	;
finish_number
	: unsigned_number
	;
module_common_item
	: module_item_declaration
	| module_program_interface_instantiation
	| assertion_item
	| bind_directive
	| continuous_assign
	| net_alias
	| initial_construct
	| final_construct
	| always_construct
	| loop_generate_construct
	| conditional_generate_construct
	| elaboration_system_task
	;
module_item
	: port_declaration ';'
	| generate_region
	| attribute_instance* parameter_override
	| attribute_instance* gate_instantiation
	| attribute_instance* module_common_item
	| attribute_instance* udp_instantiation
	| specify_block
	| attribute_instance* specparam_declaration
	| program_declaration
	| module_declaration
	| interface_declaration
	| timeunits_declaration
	;
module_item_declaration
	: package_item_declaration
	| genvar_declaration
	| clocking_declaration
	| 'default' 'clocking' clocking_identifier ';'
	| 'default' 'disable' 'iff' expression_or_dist ';'
	;
parameter_override
	: 'defparam' list_of_defparam_assignments ';'
	;
bind_directive
	: 'bind' bind_target_scope ( ':' bind_target_instance_list )? bind_instantiation ';'
	| 'bind' bind_target_instance bind_instantiation ';'
	;
bind_target_scope
	: module_identifier
	| interface_identifier
	;
bind_target_instance
	: hierarchical_identifier constant_bit_select?
	;
bind_target_instance_list
	: bind_target_instance ( ',' bind_target_instance )*
	;
bind_instantiation
	: module_program_interface_instantiation
	| checker_instantiation
	;
// A.1.5 Configuration source text
config_declaration
	: 'config' config_identifier ';' ( local_parameter_declaration ';' )* design_statement config_rule_statement* 'endconfig' config_name?
	;
config_name
	: ':' config_identifier
	;
design_statement
	: 'design' design_statement_item* ';'
	;
design_statement_item
	: ( library_identifier '.' )? cell_identifier
	;
config_rule_statement
	: default_clause liblist_clause ';'
	| inst_clause liblist_clause ';'
	| inst_clause use_clause ';'
	| cell_clause liblist_clause ';'
	| cell_clause use_clause ';'
	;
default_clause
	: 'default'
	;
inst_clause
	: 'instance' inst_name
	;
inst_name
	: topmodule_identifier ( '.' instance_identifier )*
	;
cell_clause
	: 'cell' ( library_identifier '.' )? cell_identifier
	;
liblist_clause
	: 'liblist' library_identifier*
	;
use_clause
	: 'use' ( library_identifier '.' )? cell_identifier ( ':' 'config' )?
	| 'use' named_parameter_assignment ( ',' named_parameter_assignment )* ( ':' 'config' )?
	| 'use' ( library_identifier '.' )? cell_identifier named_parameter_assignment ( ',' named_parameter_assignment )* ( ':' 'config' )?
	;
// A.1.6 Interface items
extern_tf_declaration
	: 'extern' method_prototype ';'
	| 'extern' 'forkjoin' task_prototype ';'
	;
interface_item
	: port_declaration ';'
	| generate_region
	| attribute_instance* module_common_item
	| attribute_instance* extern_tf_declaration
	| program_declaration
	| modport_declaration
	| interface_declaration
	| timeunits_declaration
	;
// A.1.7 Program items
program_item
	: port_declaration ';'
	| attribute_instance* continuous_assign
	| attribute_instance* module_item_declaration
	| attribute_instance* initial_construct
	| attribute_instance* final_construct
	| attribute_instance* concurrent_assertion_item
	| timeunits_declaration
	| loop_generate_construct
	| conditional_generate_construct
	| generate_region
	| elaboration_system_task
	;
// A.1.8 Checker items
checker_port_list
	: checker_port_item ( ',' checker_port_item )*
	;
checker_port_item
	: attribute_instance* checker_port_direction? property_formal_type? formal_port_identifier variable_dimension* ( '=' property_actual_arg )?
	;
checker_port_direction
	: 'input'
	| 'output'
	;
checker_item
	: checker_item_declaration
	| initial_construct
	| always_construct
	| final_construct
	| assertion_item
	| continuous_assign
	| loop_generate_construct
	| conditional_generate_construct
	| generate_region
	| elaboration_system_task
	;
checker_item_declaration
	: 'rand'? data_declaration
	| function_declaration
	| checker_declaration
	| assertion_item_declaration
	| covergroup_declaration
	| genvar_declaration
	| clocking_declaration
	| 'default' 'clocking' clocking_identifier ';'
	| 'default' 'disable' 'iff' expression_or_dist ';'
	| ';'
	;
// A.1.9 Class items
class_item
	: attribute_instance* class_property
	| attribute_instance* class_method
	| attribute_instance* class_constraint
	| attribute_instance* class_declaration
	| attribute_instance* covergroup_declaration
	| local_parameter_declaration ';'
	| parameter_declaration ';'
	| ';'
	;
class_property
	: 'const' ( 'protected' | 'local' | class_item_qualifier class_item_qualifier+ ) data_type const_identifier ( '=' constant_expression )? ';'
	| property_qualifier* 'automatic'? data_type list_of_variable_decl_assignments ';'
	| property_qualifier* 'const' lifetime? data_type list_of_variable_decl_assignments ';'
	| property_qualifier* 'const'? 'var' lifetime? data_type_or_implicit? list_of_variable_decl_assignments ';'
	| property_qualifier* net_type_declaration
	| property_qualifier* package_import_declaration
	| property_qualifier* type_declaration
	;
class_method
	: method_qualifier* task_declaration
	| method_qualifier* function_declaration
	| 'pure' 'virtual' class_item_qualifier* method_prototype ';'
	| 'extern' method_qualifier* method_prototype ';'
	| method_qualifier* class_constructor_declaration
	| 'extern' method_qualifier* class_constructor_prototype
	;
class_constructor_prototype
	: 'function' 'new' port_list? ';'
	;
port_list
	: '(' tf_port_list ')'
	;
class_constraint
	: constraint_prototype
	| constraint_declaration
	;
class_item_qualifier
	: 'static'
	| 'protected'
	| 'local'
	;
property_qualifier
	: random_qualifier
	| class_item_qualifier
	;
random_qualifier
	: 'rand'
	| 'randc'
	;
method_qualifier
	: 'pure'? 'virtual'
	| class_item_qualifier
	;
method_prototype
	: task_prototype
	| function_prototype
	;
class_constructor_declaration
	: 'function' class_scope? 'new' port_list? ';' block_item_declaration* super_class_constructor_call? function_statement_or_null* 'endfunction' ( ':' 'new' )?
	;
super_class_constructor_call
	: 'super' '.' 'new' arg_list? ';'
	;
// A.1.10 Constraints
constraint_declaration
	: 'static'? 'constraint' constraint_identifier constraint_block
	;
constraint_block
	: '{' constraint_block_item* '}'
	;
constraint_block_item
	: 'solve' solve_before_list 'before' solve_before_list ';'
	| constraint_expression
	;
solve_before_list
	: constraint_primary ( ',' constraint_primary )*
	;
constraint_primary
	: ( implicit_class_handle '.' | class_scope )? hierarchical_identifier select_?
	;
constraint_expression
	: 'soft'? expression_or_dist ';'
	| uniqueness_constraint ';'
	| expression '->' constraint_set
	| 'if' '(' expression ')' constraint_set ( 'else' constraint_set )?
	| 'foreach' '(' ps_or_hierarchical_array_identifier '[' loop_variables ']' ')' constraint_set
	| 'disable' 'soft' constraint_primary ';'
	;
uniqueness_constraint
	: 'unique' '{' open_range_list '}'
	;
constraint_set
	: constraint_expression
	| '{' constraint_expression* '}'
	;
dist_list
	: dist_item ( ',' dist_item )*
	;
dist_item
	: value_range dist_weight?
	;
dist_weight
	: ':=' expression
	| ':/' expression
	;
constraint_prototype
	: constraint_prototype_qualifier? 'static'? 'constraint' constraint_identifier ';'
	;
constraint_prototype_qualifier
	: 'extern'
	| 'pure'
	;
extern_constraint_declaration
	: 'static'? 'constraint' class_scope constraint_identifier constraint_block
	;
identifier_list
	: identifier ( ',' identifier )*
	;
// A.1.11 Package items
package_item
	: package_item_declaration
	| anonymous_program
	| package_export_declaration
	| timeunits_declaration
	;
package_item_declaration
	: net_declaration
	| data_declaration
	| task_declaration
	| function_declaration
	| checker_declaration
	| dpi_import_export
	| extern_constraint_declaration
	| class_declaration
	| interface_class_declaration
	| class_constructor_declaration
	| local_parameter_declaration ';'
	| parameter_declaration ';'
	| covergroup_declaration
	| assertion_item_declaration
	| ';'
	;
anonymous_program
	: 'program' ';' anonymous_program_item* 'endprogram'
	;
anonymous_program_item
	: task_declaration
	| function_declaration
	| class_declaration
	| interface_class_declaration
	| covergroup_declaration
	| class_constructor_declaration
	| ';'
	;
// A.2.1.1 Module parameter declarations
local_parameter_declaration
	: 'localparam' data_type_or_implicit? list_of_param_assignments
	| 'localparam' 'type' list_of_type_assignments
	;
parameter_declaration
	: 'parameter' data_type_or_implicit? list_of_param_assignments
	| 'parameter' 'type' list_of_type_assignments
	;
specparam_declaration
	: 'specparam' packed_dimension? list_of_specparam_assignments ';'
	;
// A.2.1.2 Port declarations
inout_declaration
	: 'inout' net_port_type? list_of_port_identifiers
	;
input_declaration
	: 'input' 'interconnect'? implicit_data_type? list_of_port_identifiers
	| 'input' net_type data_type_or_implicit? list_of_port_identifiers
	| 'input' 'var' data_type_or_implicit? list_of_variable_identifiers
	| 'input' data_type list_of_variable_identifiers
	;
output_declaration
	: 'output' 'interconnect'? implicit_data_type? list_of_port_identifiers
	| 'output' net_type data_type_or_implicit? list_of_port_identifiers
	| 'output' 'var' data_type_or_implicit? list_of_variable_port_identifiers
	| 'output' data_type list_of_variable_port_identifiers
	;
interface_port_declaration
	: interface_identifier ( '.' modport_identifier )? list_of_interface_identifiers
	;
ref_declaration
	: 'ref' variable_port_type list_of_variable_identifiers
	;
// A.2.1.3 Type declarations
data_declaration
	: 'const'? lifetime? data_type list_of_variable_decl_assignments ';'
	| 'const'? 'var' lifetime? data_type_or_implicit? list_of_variable_decl_assignments ';'
	| type_declaration
	| package_import_declaration
	| net_type_declaration
	;
package_import_declaration
	: 'import' package_import_item ( ',' package_import_item )* ';'
	;
package_import_item
	: package_identifier '::' identifier
	| package_identifier '::' '*'
	;
package_export_declaration
	: 'export' '*' '::' '*' ';'
	| 'export' package_import_item ( ',' package_import_item )* ';'
	;
genvar_declaration
	: 'genvar' list_of_genvar_identifiers ';'
	;
net_declaration
	: net_type ( drive_strength | charge_strength )? ( 'vectored' | 'scalared' )? data_type_or_implicit? delay3? list_of_net_decl_assignments ';'
	| net_type_identifier delay_control list_of_net_decl_assignments ';'
	| 'interconnect' implicit_data_type? ( '#' delay_value )? net_id ( ',' net_id )? ';'
	;
net_id
	: net_identifier unpacked_dimension*
	;
type_declaration
	: 'typedef' data_type type_identifier variable_dimension* ';'
	| 'typedef' interface_instance_identifier constant_bit_select? '.' type_identifier type_identifier ';'
	| 'typedef' ( 'enum' | 'struct' | 'union' | 'class' | 'interface' 'class' )? type_identifier ';'
	;
net_type_declaration
	: 'nettype' data_type net_type_identifier net_type_decl_with? ';'
	| 'nettype' package_or_class_scope? net_type_identifier net_type_identifier ';'
	;
net_type_decl_with
	: 'with' package_or_class_scope? tf_identifier
	;
lifetime
	: 'static'
	| 'automatic'
	;
// A.2.2.1 Net and variable types
data_type
	: integer_vector_type signing? packed_dimension*
	| integer_atom_type signing?
	| non_integer_type
	| struct_union ( 'packed' signing? )? '{' struct_union_member+ '}' packed_dimension*
	| 'enum' enum_base_type? '{' enum_name_declaration ( ',' enum_name_declaration )* '}' packed_dimension*
	| 'string'
	| 'chandle'
	| 'virtual' 'interface'? interface_identifier parameter_value_assignment? ( '.' modport_identifier )?
	| type_identifier packed_dimension+
	| '$unit' '::' type_identifier packed_dimension*
	| class_type ( '::' type_identifier packed_dimension* )?
	| 'event'
	| type_reference
	;
data_type_or_implicit
	: data_type
	| implicit_data_type
	;
implicit_data_type
	: packed_dimension+
	| signing packed_dimension*
	;
enum_base_type
	: integer_atom_type signing?
	| integer_vector_type signing? packed_dimension?
	| type_identifier packed_dimension?
	;
enum_name_declaration
	: enum_identifier enum_name_suffix_range? ( '=' constant_expression )?
	;
enum_name_suffix_range
	: '[' integral_number ( ':' integral_number )? ']'
	;
class_scope
	: class_type '::'
	;
class_type
	: ( '$unit' '::' )? class_ref ( '::' class_ref )*
	;
class_ref
	: class_identifier parameter_value_assignment?
	;
package_or_class_scope
	: class_type '::'
	| '$unit' '::'
	;
integer_type
	: integer_vector_type
	| integer_atom_type
	;
integer_atom_type
	: 'byte'
	| 'shortint'
	| 'int'
	| 'longint'
	| 'integer'
	| 'time'
	;
integer_vector_type
	: 'bit'
	| 'logic'
	| 'reg'
	;
non_integer_type
	: 'shortreal'
	| 'real'
	| 'realtime'
	;
net_type
	: 'supply0'
	| 'supply1'
	| 'tri'
	| 'triand'
	| 'trior'
	| 'trireg'
	| 'tri0'
	| 'tri1'
	| 'uwire'
	| 'wire'
	| 'wand'
	| 'wor'
	;
net_port_type
	: data_type_or_implicit
	| net_type data_type_or_implicit?
	| 'interconnect' implicit_data_type?
	;
variable_port_type
	: var_data_type
	;
var_data_type
	: data_type
	| 'var' data_type_or_implicit?
	;
signing
	: 'signed'
	| 'unsigned'
	;
simple_type
	: integer_type
	| non_integer_type
	| ps_type_or_parameter_identifier
	;
struct_union_member
	: attribute_instance* random_qualifier? data_type_or_void list_of_variable_decl_assignments ';'
	;
data_type_or_void
	: data_type
	| 'void'
	;
struct_union
	: 'struct'
	| 'union' 'tagged'?
	;
type_reference
	: 'type' '(' expression ')'
	| 'type' '(' data_type ')'
	;
// A.2.2.2 Strengths
drive_strength
	: '(' strength0 ',' strength1 ')'
	| '(' strength1 ',' strength0 ')'
	| '(' strength0 ',' 'highz1' ')'
	| '(' strength1 ',' 'highz0' ')'
	| '(' 'highz0' ',' strength1 ')'
	| '(' 'highz1' ',' strength0 ')'
	;
strength0
	: 'supply0'
	| 'strong0'
	| 'pull0'
	| 'weak0'
	;
strength1
	: 'supply1'
	| 'strong1'
	| 'pull1'
	| 'weak1'
	;
charge_strength
	: '(' 'small' ')'
	| '(' 'medium' ')'
	| '(' 'large' ')'
	;
// A.2.2.3 Delays
delay3
	: '#' delay_value
	| '#' '(' mintypmax_expression ( ',' mintypmax_expression ( ',' mintypmax_expression )? )? ')'
	;
delay2
	: '#' delay_value
	| '#' '(' mintypmax_expression ( ',' mintypmax_expression )? ')'
	;
delay_value
	: unsigned_number
	| real_number
	| ps_identifier
	| time_literal
	| '1step'
	;
// A.2.3 Declaration lists
list_of_defparam_assignments
	: defparam_assignment ( ',' defparam_assignment )*
	;
list_of_genvar_identifiers
	: genvar_identifier ( ',' genvar_identifier )*
	;
list_of_interface_identifiers
	: interface_id ( ',' interface_id )*
	;
interface_id
	: interface_identifier unpacked_dimension*
	;
list_of_net_decl_assignments
	: net_decl_assignment ( ',' net_decl_assignment )*
	;
list_of_param_assignments
	: param_assignment ( ',' param_assignment )*
	;
list_of_port_identifiers
	: port_id ( ',' port_id )*
	;
port_id
	: port_identifier unpacked_dimension*
	;
list_of_udp_port_identifiers
	: port_identifier ( ',' port_identifier )*
	;
list_of_specparam_assignments
	: specparam_assignment ( ',' specparam_assignment )*
	;
list_of_tf_variable_identifiers
	: tf_var_id ( ',' tf_var_id )*
	;
tf_var_id
	: port_identifier variable_dimension* ( '=' expression )?
	;
list_of_type_assignments
	: type_assignment ( ',' type_assignment )*
	;
list_of_variable_decl_assignments
	: variable_decl_assignment ( ',' variable_decl_assignment )*
	;
list_of_variable_identifiers
	: var_id ( ',' var_id )*
	;
var_id
	: variable_identifier variable_dimension*
	;
list_of_variable_port_identifiers
	: var_port_id ( ',' var_port_id )*
	;
var_port_id
	: port_identifier variable_dimension* ( '=' constant_expression )?
	;
// A.2.4 Declaration assignments
defparam_assignment
	: hierarchical_identifier '=' constant_mintypmax_expression
	;
net_decl_assignment
	: net_identifier unpacked_dimension* ( '=' expression )?
	;
param_assignment
	: parameter_identifier unpacked_dimension* ( '=' constant_param_expression )?
	;
specparam_assignment
	: specparam_identifier '=' constant_mintypmax_expression
	| pulse_control_specparam
	;
type_assignment
	: type_identifier ( '=' data_type )?
	;
pulse_control_specparam
	: 'PATHPULSE$' '=' '(' reject_limit_value ( ',' error_limit_value )? ')'
	| 'PATHPULSE$' specify_input_terminal_descriptor '$' specify_output_terminal_descriptor '=' '(' reject_limit_value ( ',' error_limit_value )? ')'
	;
error_limit_value
	: limit_value
	;
reject_limit_value
	: limit_value
	;
limit_value
	: constant_mintypmax_expression
	;
variable_decl_assignment
	: variable_identifier variable_dimension* ( '=' expression )?
	| dynamic_array_variable_identifier unsized_dimension variable_dimension* '=' dynamic_array_new
	| class_variable_identifier '=' class_new
	;
class_new
	: class_scope? 'new' arg_list?
	| 'new' expression
	;
dynamic_array_new
	: 'new' '[' expression ']' ( '(' expression ')' )?
	;
// A.2.5 Declaration ranges
unpacked_dimension
	: '[' constant_range ']'
	| '[' constant_expression ']'
	;
packed_dimension
	: '[' constant_range ']'
	| unsized_dimension
	;
associative_dimension
	: '[' data_type ']'
	| '[' '*' ']'
	;
variable_dimension
	: unsized_dimension
	| unpacked_dimension
	| associative_dimension
	| queue_dimension
	;
queue_dimension
	: '[' '$' ( ':' constant_expression )? ']'
	;
unsized_dimension
	: '[' ']'
	;
// A.2.6 Function declarations
function_data_type_or_implicit
	: data_type_or_void
	| implicit_data_type
	;
function_declaration
	: 'function' lifetime? function_body_declaration
	;
function_body_declaration
	: function_data_type_or_implicit? ( interface_identifier '.' | class_scope )? function_identifier ';' tf_item_declaration* function_statement_or_null* 'endfunction' function_name?
	| function_data_type_or_implicit? ( interface_identifier '.' | class_scope )? function_identifier '(' tf_port_list ')' ';' block_item_declaration* function_statement_or_null* 'endfunction' function_name?
	;
function_name
	: ':' function_identifier
	;
function_prototype
	: 'function' data_type_or_void function_identifier port_list?
	;
dpi_import_export
	: 'import' dpi_spec_string dpi_function_import_property? ( c_identifier '=' )? dpi_function_proto ';'
	| 'import' dpi_spec_string dpi_task_import_property? ( c_identifier '=' )? dpi_task_proto ';'
	| 'export' dpi_spec_string ( c_identifier '=' )? 'function' function_identifier ';'
	| 'export' dpi_spec_string ( c_identifier '=' )? 'task' task_identifier ';'
	;
dpi_spec_string
	: '"DPI-C"'
	| '"DPI"'
	;
dpi_function_import_property
	: 'context'
	| 'pure'
	;
dpi_task_import_property
	: 'context'
	;
dpi_function_proto
	: function_prototype
	;
dpi_task_proto
	: task_prototype
	;
// A.2.7 Task declarations
task_declaration
	: 'task' lifetime? task_body_declaration
	;
task_body_declaration
	: ( interface_identifier '.' | class_scope )? task_identifier ';' tf_item_declaration* statement_or_null* 'endtask' task_name?
	| ( interface_identifier '.' | class_scope )? task_identifier '(' tf_port_list ')' ';' block_item_declaration* statement_or_null* 'endtask' task_name?
	;
task_name
	: ':' task_identifier
	;
tf_item_declaration
	: block_item_declaration
	| tf_port_declaration
	;
tf_port_list
	: tf_port_item ( ',' tf_port_item )*
	;
tf_port_item
	: attribute_instance* tf_port_direction? 'var'? data_type_or_implicit? tf_port_id
	|
	;
tf_port_id
	: port_identifier variable_dimension* ( '=' expression )?
	;
tf_port_direction
	: port_direction
	| 'const' 'ref'
	;
tf_port_declaration
	: attribute_instance* tf_port_direction 'var'? data_type_or_implicit? list_of_tf_variable_identifiers ';'
	;
task_prototype
	: 'task' task_identifier port_list?
	;
// A.2.8 Block item declarations
block_item_declaration
	: attribute_instance* data_declaration
	| attribute_instance* local_parameter_declaration ';'
	| attribute_instance* parameter_declaration ';'
	| attribute_instance* let_declaration
	;
// A.2.9 Interface declarations
modport_declaration
	: 'modport' modport_item ( ',' modport_item )* ';'
	;
modport_item
	: modport_identifier '(' modport_ports_declaration ( ',' modport_ports_declaration )* ')'
	;
modport_ports_declaration
	: attribute_instance* modport_simple_ports_declaration
	| attribute_instance* modport_tf_ports_declaration
	| attribute_instance* modport_clocking_declaration
	;
modport_clocking_declaration
	: 'clocking' clocking_identifier
	;
modport_simple_ports_declaration
	: port_direction modport_simple_port ( ',' modport_simple_port )*
	;
modport_simple_port
	: port_identifier
	| '.' port_identifier '(' expression? ')'
	;
modport_tf_ports_declaration
	: import_export modport_tf_port ( ',' modport_tf_port )*
	;
modport_tf_port
	: method_prototype
	| tf_identifier
	;
import_export
	: 'import'
	| 'export'
	;
// A.2.10 Assertion declarations
concurrent_assertion_item
	: block_label? concurrent_assertion_statement
	| checker_instantiation
	;
block_label
	: block_identifier ':'
	;
concurrent_assertion_statement
	: assert_property_statement
	| assume_property_statement
	| cover_property_statement
	| cover_sequence_statement
	| restrict_property_statement
	;
assert_property_statement
	: 'assert' 'property' '(' property_spec ')' action_block
	;
assume_property_statement
	: 'assume' 'property' '(' property_spec ')' action_block
	;
cover_property_statement
	: 'cover' 'property' '(' property_spec ')' statement_or_null
	;
expect_property_statement
	: 'expect' '(' property_spec ')' action_block
	;
cover_sequence_statement
	: 'cover' 'sequence' '(' clocking_event? ( 'disable' 'iff' '(' expression_or_dist ')' )? sequence_expr ')' statement_or_null
	;
restrict_property_statement
	: 'restrict' 'property' '(' property_spec ')' ';'
	;
property_instance
	: ps_or_hierarchical_identifier prop_arg_list?
	;
prop_arg_list
	: '(' property_list_of_arguments ')'
	;
property_list_of_arguments
	: prop_ordered_arg ( ',' prop_ordered_arg )* ( ',' prop_named_arg )*
	| prop_named_arg ( ',' prop_named_arg )*
	;
prop_ordered_arg
	: property_actual_arg?
	;
prop_named_arg
	: '.' identifier '(' property_actual_arg? ')'
	;
property_actual_arg
	: property_expr
	| sequence_actual_arg
	;
assertion_item_declaration
	: property_declaration
	| sequence_declaration
	| let_declaration
	;
property_declaration
	: 'property' property_identifier prop_port_list? ';' assertion_variable_declaration* property_spec ';'? 'endproperty' property_name?
	;
property_name
	: ':' property_identifier
	;
prop_port_list
	: '(' property_port_list? ')'
	;
property_port_list
	: property_port_item ( ',' property_port_item )*
	;
property_port_item
	: attribute_instance* prop_port_item_local? property_formal_type? formal_port_identifier variable_dimension* ( '=' property_actual_arg )?
	;
prop_port_item_local
	: 'local' property_lvar_port_direction?
	;
property_lvar_port_direction
	: 'input'
	;
property_formal_type
	: sequence_formal_type
	| 'property'
	;
property_spec
	: clocking_event? ( 'disable' 'iff' '(' expression_or_dist ')' )? property_expr
	;
property_expr
	: sequence_expr
	| 'strong' '(' sequence_expr ')'
	| 'weak' '(' sequence_expr ')'
	| '(' property_expr ')'
	| 'not' property_expr
	| property_expr 'or' property_expr
	| property_expr 'and' property_expr
	| sequence_expr '|->' property_expr
	| sequence_expr '|=>' property_expr
	| 'if' expression_or_dist property_expr ( 'else' property_expr )?
	| 'case' expression_or_dist property_case_item+ 'endcase'
	| sequence_expr '#-#' property_expr
	| sequence_expr '#=#' property_expr
	| 'nexttime' property_expr
	| 'nexttime' '[' constant_expression ']' property_expr
	| 's_nexttime' property_expr
	| 's_nexttime' '[' constant_expression ']' property_expr
	| 'always' property_expr
	| 'always' '[' cycle_delay_const_range_expression ']' property_expr
	| 's_always' '[' constant_range ']' property_expr
	| 's_eventually' property_expr
	| 'eventually' '[' constant_range ']' property_expr
	| 's_eventually' '[' cycle_delay_const_range_expression ']' property_expr
	| property_expr 'until' property_expr
	| property_expr 's_until' property_expr
	| property_expr 'until_with' property_expr
	| property_expr 's_until_with' property_expr
	| property_expr 'implies' property_expr
	| property_expr 'iff' property_expr
	| 'accept_on' '(' expression_or_dist ')' property_expr
	| 'reject_on' '(' expression_or_dist ')' property_expr
	| 'sync_accept_on' '(' expression_or_dist ')' property_expr
	| 'sync_reject_on' '(' expression_or_dist ')' property_expr
	| property_instance
	| clocking_event property_expr
	;
property_case_item
	: expression_or_dist ( ',' expression_or_dist )* ':' property_expr ';'
	| 'default' ':'? property_expr ';'
	;
sequence_declaration
	: 'sequence' sequence_identifier seq_port_list? ';' assertion_variable_declaration* sequence_expr ';'? 'endsequence' sequence_name?
	;
sequence_name
	: ':' sequence_identifier
	;
seq_port_list
	: '(' sequence_port_list? ')'
	;
sequence_port_list
	: sequence_port_item ( ',' sequence_port_item )*
	;
sequence_port_item
	: attribute_instance* seq_port_item_local? sequence_formal_type? formal_port_identifier variable_dimension* ( '=' sequence_actual_arg )?
	;
seq_port_item_local
	: 'local' sequence_lvar_port_direction?
	;
sequence_lvar_port_direction
	: 'input'
	| 'inout'
	| 'output'
	;
sequence_formal_type
	: data_type_or_implicit
	| 'sequence'
	| 'untyped'
	;
sequence_expr
	: cycle_delay_range sequence_expr ( cycle_delay_range sequence_expr )*
	| sequence_expr cycle_delay_range sequence_expr ( cycle_delay_range sequence_expr )*
	| expression_or_dist boolean_abbrev?
	| sequence_instance sequence_abbrev?
	| '(' sequence_expr ( ',' sequence_match_item )* ')' sequence_abbrev?
	| sequence_expr 'and' sequence_expr
	| sequence_expr 'intersect' sequence_expr
	| sequence_expr 'or' sequence_expr
	| 'first_match' '(' sequence_expr ( ',' sequence_match_item )* ')'
	| expression_or_dist 'throughout' sequence_expr
	| sequence_expr 'within' sequence_expr
	| clocking_event sequence_expr
	;
cycle_delay_range
	: '##' constant_primary
	| '##' '[' cycle_delay_const_range_expression ']'
	| '##' '[' '*' ']'
	| '##' '[' '+' ']'
	;
sequence_method_call
	: ps_or_hierarchical_identifier seq_arg_list '.' method_identifier
	;
sequence_match_item
	: operator_assignment
	| inc_or_dec_expression
	| subroutine_call
	;
sequence_instance
	: ps_or_hierarchical_identifier seq_arg_list?
	;
seq_arg_list
	: '(' sequence_list_of_arguments ')'
	;
sequence_list_of_arguments
	: seq_ordered_arg ( ',' seq_ordered_arg )* ( ',' seq_named_arg )*
	| seq_named_arg ( ',' seq_named_arg )*
	;
seq_ordered_arg
	: sequence_actual_arg?
	;
seq_named_arg
	: '.' identifier '(' sequence_actual_arg? ')'
	;
sequence_actual_arg
	: event_expression
	| sequence_expr
	;
boolean_abbrev
	: consecutive_repetition
	| non_consecutive_repetition
	| goto_repetition
	;
sequence_abbrev
	: consecutive_repetition
	;
consecutive_repetition
	: '[' '*' const_or_range_expression ']'
	| '[' '*' ']'
	| '[' '+' ']'
	;
non_consecutive_repetition
	: '[' '=' const_or_range_expression ']'
	;
goto_repetition
	: '[' '->' const_or_range_expression ']'
	;
const_or_range_expression
	: constant_expression
	| cycle_delay_const_range_expression
	;
cycle_delay_const_range_expression
	: constant_expression ':' constant_expression
	| constant_expression ':' '$'
	;
expression_or_dist
	: expression ( 'dist' '{' dist_list '}' )?
	;
assertion_variable_declaration
	: var_data_type list_of_variable_decl_assignments ';'
	;
// A.2.11 Covergroup declarations
covergroup_declaration
	: 'covergroup' covergroup_identifier port_list? coverage_event? ';' coverage_spec_or_option* 'endgroup' covergroup_name?
	;
covergroup_name
	: ':' covergroup_identifier
	;
coverage_spec_or_option
	: attribute_instance* coverage_spec
	| attribute_instance* coverage_option ';'
	;
coverage_option
	: 'option' '.' member_identifier '=' expression
	| 'type_option' '.' member_identifier '=' constant_expression
	;
coverage_spec
	: cover_point
	| cover_cross
	;
coverage_event
	: clocking_event
	| 'with' 'function' 'sample' '(' tf_port_list ')'
	| '@@' '(' block_event_expression ')'
	;
block_event_expression
	: block_event_expression 'or' block_event_expression
	| 'begin' hierarchical_btf_identifier
	| 'end' hierarchical_btf_identifier
	;
hierarchical_btf_identifier
	: class_scope? identifier
	| hier_ref+ identifier
	| '$root' '.' hier_ref* identifier
	;
cover_point
	: cover_point_label? 'coverpoint' expression ( 'iff' '(' expression ')' )? bins_or_empty
	;
cover_point_label
	: data_type_or_implicit? cover_point_identifier ':'
	;
bins_or_empty
	: '{' attribute_instance* ( bins_or_options ';' )* '}'
	| ';'
	;
bins_or_options
	: coverage_option
	| 'wildcard'? bins_keyword bin_identifier bin_array_size? '=' '{' covergroup_range_list '}' ( 'with' '(' with_covergroup_expression ')' )? ( 'iff' '(' expression ')' )?
	| 'wildcard'? bins_keyword bin_identifier bin_array_size? '=' cover_point_identifier 'with' '(' with_covergroup_expression ')' ( 'iff' '(' expression ')' )?
	| 'wildcard'? bins_keyword bin_identifier bin_array_size? '=' set_covergroup_expression ( 'iff' '(' expression ')' )?
	| 'wildcard'? bins_keyword bin_identifier ( '[' ']' )? '=' trans_list ( 'iff' '(' expression ')' )?
	| bins_keyword bin_identifier bin_array_size? '=' 'default' ( 'iff' '(' expression ')' )?
	| bins_keyword bin_identifier '=' 'default' 'sequence' ( 'iff' '(' expression ')' )?
	;
bin_array_size
	: '[' covergroup_expression? ']'
	;
bins_keyword
	: 'bins'
	| 'illegal_bins'
	| 'ignore_bins'
	;
trans_list
	: trans_set ( ',' trans_set )*
	;
trans_set
	: '(' trans_range_list ( '=>' trans_range_list )* ')'
	;
trans_range_list
	: trans_item
	| trans_item '[' '*' repeat_range ']'
	| trans_item '[' '->' repeat_range ']'
	| trans_item '[' '=' repeat_range ']'
	;
trans_item
	: covergroup_range_list
	;
repeat_range
	: covergroup_expression ( ':' covergroup_expression )?
	;
cover_cross
	: cross_label? 'cross' list_of_cross_items ( 'iff' '(' expression ')' )? cross_body
	;
cross_label
	: cross_identifier ':'
	;
list_of_cross_items
	: cross_item ',' cross_item ( ',' cross_item )*
	;
cross_item
	: identifier
	;
cross_body
	: '{' cross_body_item* '}'
	| ';'
	;
cross_body_item
	: function_declaration
	| bins_selection_or_option ';'
	;
bins_selection_or_option
	: attribute_instance* coverage_option
	| attribute_instance* bins_selection
	;
bins_selection
	: bins_keyword bin_identifier '=' select_expression ( 'iff' '(' expression ')' )?
	;
select_expression
	: select_condition
	| '!' select_condition
	| select_expression '&&' select_expression
	| select_expression '||' select_expression
	| '(' select_expression ')'
	| select_expression 'with' '(' with_covergroup_expression ')' ( 'matches' integer_covergroup_expression )?
	| cross_identifier
	| cross_set_expression ( 'matches' integer_covergroup_expression )?
	;
select_condition
	: 'binsof' '(' bins_expression ')' ( 'intersect' '{' covergroup_range_list '}' )?
	;
bins_expression
	: variable_identifier
	| cover_point_identifier '.' bin_identifier
	;
covergroup_range_list
	: covergroup_value_range ( ',' covergroup_value_range )*
	;
covergroup_value_range
	: covergroup_expression
	| '[' covergroup_expression ':' covergroup_expression ']'
	;
with_covergroup_expression
	: covergroup_expression
	;
set_covergroup_expression
	: covergroup_expression
	;
integer_covergroup_expression
	: covergroup_expression
	;
cross_set_expression
	: covergroup_expression
	;
covergroup_expression
	: expression
	;
// A.2.12 Let declarations
let_declaration
	: 'let' let_identifier let_ports? '=' expression ';'
	;
let_ports
	: '(' let_port_list? ')'
	;
let_identifier
	: identifier
	;
let_port_list
	: let_port_item ( ',' let_port_item )*
	;
let_port_item
	: attribute_instance* let_formal_type? formal_port_identifier variable_dimension* ( '=' expression )?
	;
let_formal_type
	: data_type_or_implicit
	| 'untyped'
	;
// A.3.1 Primitive instantiation and instances
gate_instantiation
	: cmos_switchtype delay3? cmos_switch_instance ( ',' cmos_switch_instance )* ';'
	| enable_gatetype drive_strength? delay3? enable_gate_instance ( ',' enable_gate_instance )* ';'
	| mos_switchtype delay3? mos_switch_instance ( ',' mos_switch_instance )* ';'
	| n_input_gatetype drive_strength? delay2? n_input_gate_instance ( ',' n_input_gate_instance )* ';'
	| n_output_gatetype drive_strength? delay2? n_output_gate_instance ( ',' n_output_gate_instance )* ';'
	| pass_en_switchtype delay2? pass_enable_switch_instance ( ',' pass_enable_switch_instance )* ';'
	| pass_switchtype pass_switch_instance ( ',' pass_switch_instance )* ';'
	| 'pulldown' pulldown_strength? pull_gate_instance ( ',' pull_gate_instance )* ';'
	| 'pullup' pullup_strength? pull_gate_instance ( ',' pull_gate_instance )* ';'
	;
cmos_switch_instance
	: name_of_instance? '(' output_terminal ',' input_terminal ',' ncontrol_terminal ',' pcontrol_terminal ')'
	;
enable_gate_instance
	: name_of_instance? '(' output_terminal ',' input_terminal ',' enable_terminal ')'
	;
mos_switch_instance
	: name_of_instance? '(' output_terminal ',' input_terminal ',' enable_terminal ')'
	;
n_input_gate_instance
	: name_of_instance? '(' output_terminal ',' input_terminal ( ',' input_terminal )* ')'
	;
n_output_gate_instance
	: name_of_instance? '(' output_terminal ( ',' output_terminal )* ',' input_terminal ')'
	;
pass_switch_instance
	: name_of_instance? '(' inout_terminal ',' inout_terminal ')'
	;
pass_enable_switch_instance
	: name_of_instance? '(' inout_terminal ',' inout_terminal ',' enable_terminal ')'
	;
pull_gate_instance
	: name_of_instance? '(' output_terminal ')'
	;
// A.3.2 Primitive strengths
pulldown_strength
	: '(' strength0 ',' strength1 ')'
	| '(' strength1 ',' strength0 ')'
	| '(' strength0 ')'
	;
pullup_strength
	: '(' strength0 ',' strength1 ')'
	| '(' strength1 ',' strength0 ')'
	| '(' strength1 ')'
	;
// A.3.3 Primitive terminals
enable_terminal
	: expression
	;
inout_terminal
	: net_lvalue
	;
input_terminal
	: expression
	;
ncontrol_terminal
	: expression
	;
output_terminal
	: net_lvalue
	;
pcontrol_terminal
	: expression
	;
// A.3.4 Primitive gate and switch types
cmos_switchtype
	: 'cmos'
	| 'rcmos'
	;
enable_gatetype
	: 'bufif0'
	| 'bufif1'
	| 'notif0'
	| 'notif1'
	;
mos_switchtype
	: 'nmos'
	| 'pmos'
	| 'rnmos'
	| 'rpmos'
	;
n_input_gatetype
	: 'and'
	| 'nand'
	| 'or'
	| 'nor'
	| 'xor'
	| 'xnor'
	;
n_output_gatetype
	: 'buf'
	| 'not'
	;
pass_en_switchtype
	: 'tranif0'
	| 'tranif1'
	| 'rtranif1'
	| 'rtranif0'
	;
pass_switchtype
	: 'tran'
	| 'rtran'
	;
// A.4.1.1 Module instantiation
module_program_interface_instantiation
	: instance_identifier parameter_value_assignment? hierarchical_instance ( ',' hierarchical_instance )* ';'
	;
parameter_value_assignment
	: '#' '(' list_of_parameter_assignments? ')'
	;
list_of_parameter_assignments
	: ordered_parameter_assignment ( ',' ordered_parameter_assignment )*
	| named_parameter_assignment ( ',' named_parameter_assignment )*
	;
ordered_parameter_assignment
	: param_expression
	;
named_parameter_assignment
	: '.' parameter_identifier '(' param_expression? ')'
	;
hierarchical_instance
	: name_of_instance '(' list_of_port_connections ')'
	;
name_of_instance
	: instance_identifier unpacked_dimension*
	;
list_of_port_connections
	: ordered_port_connection ( ',' ordered_port_connection )*
	| named_port_connection ( ',' named_port_connection )*
	;
ordered_port_connection
	: attribute_instance* expression?
	;
named_port_connection
	: attribute_instance* '.' port_identifier port_assign?
	| attribute_instance* '.*'
	;
port_assign
	: '(' expression? ')'
	;
// A.4.1.4 Checker instantiation
checker_instantiation
	: ps_identifier name_of_instance '(' list_of_checker_port_connections ')' ';'
	;
list_of_checker_port_connections
	: ordered_checker_port_connection ( ',' ordered_checker_port_connection )*
	| named_checker_port_connection ( ',' named_checker_port_connection )*
	;
ordered_checker_port_connection
	: attribute_instance* property_actual_arg?
	;
named_checker_port_connection
	: attribute_instance* '.' formal_port_identifier checker_port_assign?
	| attribute_instance* '.*'
	;
checker_port_assign
	: '(' property_actual_arg? ')'
	;
// A.4.2 Generated instantiation
generate_region
	: 'generate' generate_item* 'endgenerate'
	;
loop_generate_construct
	: 'for' '(' genvar_initialization ';' genvar_expression ';' genvar_iteration ')' generate_block
	;
genvar_initialization
	: 'genvar'? genvar_identifier '=' constant_expression
	;
genvar_iteration
	: genvar_identifier assignment_operator genvar_expression
	| inc_or_dec_operator genvar_identifier
	| genvar_identifier inc_or_dec_operator
	;
conditional_generate_construct
	: if_generate_construct
	| case_generate_construct
	;
if_generate_construct
	: 'if' '(' constant_expression ')' generate_block ( 'else' generate_block )?
	;
case_generate_construct
	: 'case' '(' constant_expression ')' case_generate_item+ 'endcase'
	;
case_generate_item
	: constant_expression ( ',' constant_expression )* ':' generate_block
	| 'default' ':'? generate_block
	;
generate_block
	: generate_item
	| generate_block_label? 'begin' generate_block_name? generate_item* 'end' generate_block_name?
	;
generate_block_label
	: generate_block_identifier ':'
	;
generate_block_name
	: ':' generate_block_identifier
	;
generate_item
	: attribute_instance* parameter_override
	| attribute_instance* gate_instantiation
	| attribute_instance* net_declaration
	| ( attribute_instance+ | 'rand' )? data_declaration
	| attribute_instance* task_declaration
	| attribute_instance* function_declaration
	| attribute_instance* checker_declaration
	| attribute_instance* dpi_import_export
	| attribute_instance* extern_constraint_declaration
	| attribute_instance* class_declaration
	| attribute_instance* interface_class_declaration
	| attribute_instance* class_constructor_declaration
	| attribute_instance* local_parameter_declaration ';'
	| attribute_instance* parameter_declaration ';'
	| attribute_instance* covergroup_declaration
	| attribute_instance* assertion_item_declaration
	| attribute_instance* ';'
	| attribute_instance* genvar_declaration
	| attribute_instance* clocking_declaration
	| attribute_instance* 'default' 'clocking' clocking_identifier ';'
	| attribute_instance* 'default' 'disable' 'iff' expression_or_dist ';'
	| attribute_instance* module_program_interface_instantiation
	| attribute_instance* assertion_item
	| attribute_instance* udp_instantiation
	| attribute_instance* bind_directive
	| attribute_instance* continuous_assign
	| attribute_instance* net_alias
	| attribute_instance* initial_construct
	| attribute_instance* final_construct
	| attribute_instance* always_construct
	| attribute_instance* loop_generate_construct
	| attribute_instance* conditional_generate_construct
	| attribute_instance* elaboration_system_task
	| attribute_instance* extern_tf_declaration
	| generate_region
	;
// A.5.1 UDP declaration
udp_nonansi_declaration
	: attribute_instance* 'primitive' udp_identifier '(' udp_port_list ')' ';'
	;
udp_ansi_declaration
	: attribute_instance* 'primitive' udp_identifier '(' udp_declaration_port_list ')' ';'
	;
udp_declaration
	: udp_nonansi_declaration udp_port_declaration+ udp_body 'endprimitive' udp_name?
	| udp_ansi_declaration udp_body 'endprimitive' udp_name?
	| 'extern' udp_nonansi_declaration
	| 'extern' udp_ansi_declaration
	| attribute_instance* 'primitive' udp_identifier '(' '.*' ')' ';' udp_port_declaration* udp_body 'endprimitive' udp_name?
	;
udp_name
	: ':' udp_identifier
	;
// A.5.2 UDP ports
udp_port_list
	: output_port_identifier ',' input_port_identifier ( ',' input_port_identifier )*
	;
udp_declaration_port_list
	: udp_output_declaration ',' udp_input_declaration ( ',' udp_input_declaration )*
	;
udp_port_declaration
	: udp_output_declaration ';'
	| udp_input_declaration ';'
	| udp_reg_declaration ';'
	;
udp_output_declaration
	: attribute_instance* 'output' port_identifier
	| attribute_instance* 'output' 'reg' port_identifier ( '=' constant_expression )?
	;
udp_input_declaration
	: attribute_instance* 'input' list_of_udp_port_identifiers
	;
udp_reg_declaration
	: attribute_instance* 'reg' variable_identifier
	;
// A.5.3 UDP body
udp_body
	: combinational_body
	| sequential_body
	;
combinational_body
	: 'table' combinational_entry+ 'endtable'
	;
combinational_entry
	: level_input_list ':' output_symbol ';'
	;
sequential_body
	: udp_initial_statement? 'table' sequential_entry+ 'endtable'
	;
udp_initial_statement
	: 'initial' output_port_identifier '=' init_val ';'
	;
init_val
	: binary_number
	| unsigned_number
	;
sequential_entry
	: seq_input_list ':' current_state ':' next_state ';'
	;
seq_input_list
	: level_input_list
	| edge_input_list
	;
level_input_list
	: level_symbol+
	;
edge_input_list
	: level_symbol* edge_indicator level_symbol*
	;
edge_indicator
	: '(' level_symbol level_symbol ')'
	| edge_symbol
	;
current_state
	: level_symbol
	;
next_state
	: output_symbol
	| '-'
	;
output_symbol
	: OUTPUT_OR_LEVEL_SYMBOL
	;
level_symbol
	: LEVEL_ONLY_SYMBOL
	| OUTPUT_OR_LEVEL_SYMBOL
	;
edge_symbol
	: EDGE_SYMBOL
	;
// A.5.4 UDP instantiation
udp_instantiation
	: udp_identifier drive_strength? delay2? udp_instance ( ',' udp_instance )* ';'
	;
udp_instance
	: name_of_instance? '(' output_terminal ',' input_terminal ( ',' input_terminal )* ')'
	;
// A.6.1 Continuous assignment and net alias statements
continuous_assign
	: 'assign' '#' '(' mintypmax_expression ',' mintypmax_expression ( ',' mintypmax_expression )? ')' list_of_net_assignments ';'
	| 'assign' drive_strength delay3? list_of_net_assignments ';'
	| 'assign' delay_control? list_of_variable_assignments ';'
	;
list_of_net_assignments
	: net_assignment ( ',' net_assignment )*
	;
list_of_variable_assignments
	: variable_assignment ( ',' variable_assignment )*
	;
net_alias
	: 'alias' net_lvalue ( '=' net_lvalue )+ ';'
	;
net_assignment
	: net_lvalue '=' expression
	;
// A.6.2 Procedural blocks and assignments
initial_construct
	: 'initial' statement_or_null
	;
always_construct
	: always_keyword statement
	;
always_keyword
	: 'always'
	| 'always_comb'
	| 'always_latch'
	| 'always_ff'
	;
final_construct
	: 'final' function_statement
	;
blocking_assignment
	: variable_lvalue '=' delay_or_event_control expression
	| nonrange_variable_lvalue '=' dynamic_array_new
	| ( implicit_class_handle '.' | package_or_class_scope )? hierarchical_identifier select_? '=' class_new
	| operator_assignment
	;
operator_assignment
	: variable_lvalue assignment_operator expression
	;
assignment_operator
	: '='
	| '+='
	| '-='
	| '*='
	| '/='
	| '%='
	| '&='
	| '|='
	| '^='
	| '<<='
	| '>>='
	| '<<<='
	| '>>>='
	;
nonblocking_assignment
	: variable_lvalue '<=' delay_or_event_control? expression
	;
procedural_continuous_assignment
	: 'assign' variable_assignment
	| 'deassign' variable_lvalue
	| 'force' variable_assignment
	| 'release' variable_lvalue
	;
variable_assignment
	: variable_lvalue '=' expression
	;
// A.6.3 Parallel and sequential blocks
action_block
	: statement_or_null
	| statement? 'else' statement_or_null
	;
seq_block
	: 'begin' block_name? block_item_declaration* statement_or_null* 'end' block_name?
	;
block_name
	: ':' block_identifier
	;
par_block
	: 'fork' block_name? block_item_declaration* statement_or_null* join_keyword block_name?
	;
join_keyword
	: 'join'
	| 'join_any'
	| 'join_none'
	;
// A.6.4 Statements
statement_or_null
	: statement
	| attribute_instance* ';'
	;
statement
	: block_label? attribute_instance* statement_item
	;
statement_item
	: blocking_assignment ';'
	| nonblocking_assignment ';'
	| procedural_continuous_assignment ';'
	| case_statement
	| conditional_statement
	| inc_or_dec_expression ';'
	| subroutine_call_statement
	| disable_statement
	| event_trigger
	| loop_statement
	| jump_statement
	| par_block
	| procedural_timing_control_statement
	| seq_block
	| wait_statement
	| procedural_assertion_statement
	| clocking_drive ';'
	| randsequence_statement
	| randcase_statement
	| expect_property_statement
	;
function_statement
	: statement
	;
function_statement_or_null
	: function_statement
	| attribute_instance* ';'
	;
variable_identifier_list
	: variable_identifier ( ',' variable_identifier )*
	;
// A.6.5 Timing control statements
procedural_timing_control_statement
	: procedural_timing_control statement_or_null
	;
delay_or_event_control
	: delay_control
	| event_control
	| 'repeat' '(' expression ')' event_control
	;
delay_control
	: '#' delay_value
	| '#' '(' mintypmax_expression ')'
	;
event_control
	: '@' '(' event_expression ')'
	| '@' '*'
	| '@' '(' '*' ')'
	| '@' ps_or_hierarchical_identifier
	;
event_expression
	: edge_identifier? expression ( 'iff' expression )?
	| sequence_instance ( 'iff' expression )?
	| event_expression 'or' event_expression
	| event_expression ',' event_expression
	| '(' event_expression ')'
	;
procedural_timing_control
	: delay_control
	| event_control
	| cycle_delay
	;
jump_statement
	: 'return' expression? ';'
	| 'break' ';'
	| 'continue' ';'
	;
wait_statement
	: 'wait' '(' expression ')' statement_or_null
	| 'wait' 'fork' ';'
	| 'wait_order' '(' hierarchical_identifier ( ',' hierarchical_identifier )* ')' action_block
	;
event_trigger
	: '->' hierarchical_identifier ';'
	| '->>' delay_or_event_control? hierarchical_identifier ';'
	;
disable_statement
	: 'disable' hierarchical_identifier ';'
	| 'disable' 'fork' ';'
	;
// A.6.6 Conditional statements
conditional_statement
	: unique_priority? 'if' '(' cond_predicate ')' statement_or_null ( 'else' statement_or_null )?
	;
unique_priority
	: 'unique'
	| 'unique0'
	| 'priority'
	;
cond_predicate
	: expression_or_cond_pattern ( '&&&' expression_or_cond_pattern )*
	;
expression_or_cond_pattern
	: expression ( 'matches' pattern )?
	;
// A.6.7 Case statements
case_statement
	: unique_priority? case_keyword '(' case_expression ')' case_item+ 'endcase'
	| unique_priority? case_keyword '(' case_expression ')' 'matches' case_pattern_item+ 'endcase'
	| unique_priority? 'case' '(' case_expression ')' 'inside' case_inside_item+ 'endcase'
	;
case_keyword
	: 'case'
	| 'casez'
	| 'casex'
	;
case_expression
	: expression
	;
case_item
	: case_item_expression ( ',' case_item_expression )* ':' statement_or_null
	| 'default' ':'? statement_or_null
	;
case_pattern_item
	: pattern ( '&&&' expression )? ':' statement_or_null
	| 'default' ':'? statement_or_null
	;
case_inside_item
	: open_range_list ':' statement_or_null
	| 'default' ':'? statement_or_null
	;
case_item_expression
	: expression
	;
randcase_statement
	: 'randcase' randcase_item+ 'endcase'
	;
randcase_item
	: expression ':' statement_or_null
	;
open_range_list
	: open_value_range ( ',' open_value_range )*
	;
open_value_range
	: value_range
	;
// A.6.7.1 Patterns
pattern
	: '.' variable_identifier
	| '.*'
	| constant_expression
	| 'tagged' member_identifier pattern?
	| '\'' '{' pattern ( ',' pattern )* '}'
	| '\'' '{' member_pattern_pair ( ',' member_pattern_pair )* '}'
	;
member_pattern_pair
	: member_identifier ':' pattern
	;
assignment_pattern
	: '\'' '{' expression ( ',' expression )* '}'
	| '\'' '{' array_key_val_pair ( ',' array_key_val_pair )* '}'
	| '\'' '{' constant_expression '{' expression ( ',' expression )* '}' '}'
	;
array_key_val_pair
	: array_pattern_key ':' expression
	;
array_pattern_key
	: constant_expression
	| assignment_pattern_key
	;
assignment_pattern_key
	: integer_type
	| non_integer_type
	| 'local' '::' identifier
	| 'default'
	;
assignment_pattern_expression
	: assignment_pattern_expression_type? assignment_pattern
	;
assignment_pattern_expression_type
	: ps_type_or_parameter_identifier
	| integer_atom_type
	| type_reference
	;
constant_assignment_pattern_expression
	: assignment_pattern_expression
	;
assignment_pattern_net_lvalue
	: '\'' '{' net_lvalue ( ',' net_lvalue )* '}'
	;
assignment_pattern_variable_lvalue
	: '\'' '{' variable_lvalue ( ',' variable_lvalue )* '}'
	;
// A.6.8 Looping statements
loop_statement
	: 'forever' statement_or_null
	| 'repeat' '(' expression ')' statement_or_null
	| 'while' '(' expression ')' statement_or_null
	| 'for' '(' for_initialization? ';' expression? ';' for_step? ')' statement_or_null
	| 'do' statement_or_null 'while' '(' expression ')' ';'
	| 'foreach' '(' ps_or_hierarchical_array_identifier '[' loop_variables ']' ')' statement
	;
for_initialization
	: list_of_variable_assignments
	| for_variable_declaration ( ',' for_variable_declaration )*
	;
for_variable_declaration
	: 'var'? data_type for_variable_assign ( ',' for_variable_assign )*
	;
for_variable_assign
	: variable_identifier '=' expression
	;
for_step
	: for_step_assignment ( ',' for_step_assignment )*
	;
for_step_assignment
	: operator_assignment
	| inc_or_dec_expression
	| subroutine_call
	;
loop_variables
	: loop_var ( ',' loop_var )*
	;
loop_var
	: index_variable_identifier?
	;
// A.6.9 Subroutine call statements
subroutine_call_statement
	: subroutine_call ';'
	| 'void' '\'' '(' subroutine_call ')' ';'
	;
// A.6.10 Assertion statements
assertion_item
	: concurrent_assertion_item
	| deferred_immediate_assertion_item
	;
deferred_immediate_assertion_item
	: block_label? deferred_immediate_assertion_statement
	;
procedural_assertion_statement
	: concurrent_assertion_statement
	| immediate_assertion_statement
	| checker_instantiation
	;
immediate_assertion_statement
	: simple_immediate_assertion_statement
	| deferred_immediate_assertion_statement
	;
simple_immediate_assertion_statement
	: simple_immediate_assert_statement
	| simple_immediate_assume_statement
	| simple_immediate_cover_statement
	;
simple_immediate_assert_statement
	: 'assert' '(' expression ')' action_block
	;
simple_immediate_assume_statement
	: 'assume' '(' expression ')' action_block
	;
simple_immediate_cover_statement
	: 'cover' '(' expression ')' statement_or_null
	;
deferred_immediate_assertion_statement
	: deferred_immediate_assert_statement
	| deferred_immediate_assume_statement
	| deferred_immediate_cover_statement
	;
deferred_immediate_assert_statement
	: 'assert' '#' unsigned_number '(' expression ')' action_block
	| 'assert' 'final' '(' expression ')' action_block
	;
deferred_immediate_assume_statement
	: 'assume' '#' unsigned_number '(' expression ')' action_block
	| 'assume' 'final' '(' expression ')' action_block
	;
deferred_immediate_cover_statement
	: 'cover' '#' unsigned_number '(' expression ')' statement_or_null
	| 'cover' 'final' '(' expression ')' statement_or_null
	;
// A.6.11 Clocking block
clocking_declaration
	: 'default'? 'clocking' clocking_identifier? clocking_event ';' clocking_item* 'endclocking' clocking_name?
	| 'global' 'clocking' clocking_identifier? clocking_event ';' 'endclocking' clocking_name?
	;
clocking_name
	: ':' clocking_identifier
	;
clocking_event
	: '@' identifier
	| '@' '(' event_expression ')'
	;
clocking_item
	: 'default' default_skew ';'
	| clocking_direction list_of_clocking_decl_assign ';'
	| attribute_instance* assertion_item_declaration
	;
default_skew
	: 'input' clocking_skew
	| 'output' clocking_skew
	| 'input' clocking_skew 'output' clocking_skew
	;
clocking_direction
	: 'input' clocking_skew?
	| 'output' clocking_skew?
	| 'input' clocking_skew? 'output' clocking_skew?
	| 'inout'
	;
list_of_clocking_decl_assign
	: clocking_decl_assign ( ',' clocking_decl_assign )*
	;
clocking_decl_assign
	: signal_identifier ( '=' expression )?
	;
clocking_skew
	: edge_identifier delay_control?
	| delay_control
	;
clocking_drive
	: clockvar_expression '<=' cycle_delay expression
	;
cycle_delay
	: '##' integral_number
	| '##' identifier
	| '##' '(' expression ')'
	;
clockvar
	: hierarchical_identifier
	;
clockvar_expression
	: clockvar select_?
	;
// A.6.12 Randsequence
randsequence_statement
	: 'randsequence' '(' production_identifier? ')' production+ 'endsequence'
	;
production
	: data_type_or_void? production_identifier port_list? ':' rs_rule ( '|' rs_rule )* ';'
	;
rs_rule
	: rs_production_list weight_spec?
	;
weight_spec
	: ':=' weight_specification rs_code_block?
	;
rs_production_list
	: rs_prod+
	| 'rand' 'join' ( '(' expression ')' )? production_item+
	;
weight_specification
	: integral_number
	| ps_identifier
	| '(' expression ')'
	;
rs_code_block
	: '{' data_declaration* statement_or_null* '}'
	;
rs_prod
	: production_item
	| rs_code_block
	| rs_if_else
	| rs_repeat
	| rs_case
	;
production_item
	: production_identifier arg_list?
	;
rs_if_else
	: 'if' '(' expression ')' production_item ( 'else' production_item )?
	;
rs_repeat
	: 'repeat' '(' expression ')' production_item
	;
rs_case
	: 'case' '(' case_expression ')' rs_case_item+ 'endcase'
	;
rs_case_item
	: case_item_expression ( ',' case_item_expression )* ':' production_item ';'
	| 'default' ':'? production_item ';'
	;
// A.7.1 Specify block declaration
specify_block
	: 'specify' specify_item* 'endspecify'
	;
specify_item
	: specparam_declaration
	| pulsestyle_declaration
	| showcancelled_declaration
	| path_declaration
	| system_timing_check
	;
pulsestyle_declaration
	: 'pulsestyle_onevent' list_of_path_outputs ';'
	| 'pulsestyle_ondetect' list_of_path_outputs ';'
	;
showcancelled_declaration
	: 'showcancelled' list_of_path_outputs ';'
	| 'noshowcancelled' list_of_path_outputs ';'
	;
// A.7.2 Specify path declarations
path_declaration
	: simple_path_declaration ';'
	| edge_sensitive_path_declaration ';'
	| state_dependent_path_declaration ';'
	;
simple_path_declaration
	: parallel_path_description '=' path_delay_value
	| full_path_description '=' path_delay_value
	;
parallel_path_description
	: '(' specify_input_terminal_descriptor polarity_operator? '=>' specify_output_terminal_descriptor ')'
	;
full_path_description
	: '(' list_of_path_inputs polarity_operator? '*>' list_of_path_outputs ')'
	;
list_of_path_inputs
	: specify_input_terminal_descriptor ( ',' specify_input_terminal_descriptor )*
	;
list_of_path_outputs
	: specify_output_terminal_descriptor ( ',' specify_output_terminal_descriptor )*
	;
// A.7.3 Specify block terminals
specify_input_terminal_descriptor
	: input_identifier ( '[' constant_range_expression ']' )?
	;
specify_output_terminal_descriptor
	: output_identifier ( '[' constant_range_expression ']' )?
	;
input_identifier
	: port_identifier
	| interface_identifier '.' port_identifier
	;
output_identifier
	: port_identifier
	| interface_identifier '.' port_identifier
	;
// A.7.4 Specify path delays
path_delay_value
	: list_of_path_delay_expressions
	| '(' list_of_path_delay_expressions ')'
	;
list_of_path_delay_expressions
	: t_path_delay_expression
	| trise_path_delay_expression ',' tfall_path_delay_expression ( ',' tz_path_delay_expression )?
	| t01_path_delay_expression ',' t10_path_delay_expression ',' t0z_path_delay_expression ',' tz1_path_delay_expression ',' t1z_path_delay_expression ',' tz0_path_delay_expression ( ',' t0x_path_delay_expression ',' tx1_path_delay_expression ',' t1x_path_delay_expression ',' tx0_path_delay_expression ',' txz_path_delay_expression ',' tzx_path_delay_expression )?
	;
t_path_delay_expression
	: path_delay_expression
	;
trise_path_delay_expression
	: path_delay_expression
	;
tfall_path_delay_expression
	: path_delay_expression
	;
tz_path_delay_expression
	: path_delay_expression
	;
t01_path_delay_expression
	: path_delay_expression
	;
t10_path_delay_expression
	: path_delay_expression
	;
t0z_path_delay_expression
	: path_delay_expression
	;
tz1_path_delay_expression
	: path_delay_expression
	;
t1z_path_delay_expression
	: path_delay_expression
	;
tz0_path_delay_expression
	: path_delay_expression
	;
t0x_path_delay_expression
	: path_delay_expression
	;
tx1_path_delay_expression
	: path_delay_expression
	;
t1x_path_delay_expression
	: path_delay_expression
	;
tx0_path_delay_expression
	: path_delay_expression
	;
txz_path_delay_expression
	: path_delay_expression
	;
tzx_path_delay_expression
	: path_delay_expression
	;
path_delay_expression
	: constant_mintypmax_expression
	;
edge_sensitive_path_declaration
	: parallel_edge_sensitive_path_description '=' path_delay_value
	| full_edge_sensitive_path_description '=' path_delay_value
	;
parallel_edge_sensitive_path_description
	: '(' edge_identifier? specify_input_terminal_descriptor polarity_operator? '=>' '(' specify_output_terminal_descriptor polarity_operator? ':' data_source_expression ')' ')'
	;
full_edge_sensitive_path_description
	: '(' edge_identifier? list_of_path_inputs polarity_operator? '*>' '(' list_of_path_outputs polarity_operator? ':' data_source_expression ')' ')'
	;
data_source_expression
	: expression
	;
edge_identifier
	: 'posedge'
	| 'negedge'
	| 'edge'
	;
state_dependent_path_declaration
	: 'if' '(' module_path_expression ')' simple_path_declaration
	| 'if' '(' module_path_expression ')' edge_sensitive_path_declaration
	| 'ifnone' simple_path_declaration
	;
polarity_operator
	: '+'
	| '-'
	;
// A.7.5.1 System timing check commands
system_timing_check
	: setup_timing_check
	| hold_timing_check
	| setuphold_timing_check
	| recovery_timing_check
	| removal_timing_check
	| recrem_timing_check
	| skew_timing_check
	| timeskew_timing_check
	| fullskew_timing_check
	| period_timing_check
	| width_timing_check
	| nochange_timing_check
	;
setup_timing_check
	: '$setup' '(' data_event ',' reference_event ',' timing_check_limit notifier_opt? ')' ';'
	;
notifier_opt
	: ',' notifier?
	;
hold_timing_check
	: '$hold' '(' reference_event ',' data_event ',' timing_check_limit notifier_opt? ')' ';'
	;
setuphold_timing_check
	: '$setuphold' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit timing_check_opt? ')' ';'
	;
timing_check_opt
	: ',' notifier? timestamp_cond_opt?
	;
timestamp_cond_opt
	: ',' timestamp_condition? timecheck_cond_opt?
	;
timecheck_cond_opt
	: ',' timecheck_condition? delayed_ref_opt?
	;
delayed_ref_opt
	: ',' delayed_reference? delayed_data_opt?
	;
delayed_data_opt
	: ',' delayed_data?
	;
recovery_timing_check
	: '$recovery' '(' reference_event ',' data_event ',' timing_check_limit notifier_opt? ')' ';'
	;
removal_timing_check
	: '$removal' '(' reference_event ',' data_event ',' timing_check_limit notifier_opt? ')' ';'
	;
recrem_timing_check
	: '$recrem' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit timing_check_opt? ')' ';'
	;
skew_timing_check
	: '$skew' '(' reference_event ',' data_event ',' timing_check_limit notifier_opt? ')' ';'
	;
timeskew_timing_check
	: '$timeskew' '(' reference_event ',' data_event ',' timing_check_limit skew_timing_check_opt? ')' ';'
	;
skew_timing_check_opt
	: ',' notifier? event_based_flag_opt?
	;
event_based_flag_opt
	: ',' event_based_flag? remain_active_flag_opt?
	;
remain_active_flag_opt
	: ',' remain_active_flag?
	;
fullskew_timing_check
	: '$fullskew' '(' reference_event ',' data_event ',' timing_check_limit ',' timing_check_limit skew_timing_check_opt? ')' ';'
	;
period_timing_check
	: '$period' '(' controlled_reference_event ',' timing_check_limit notifier_opt? ')' ';'
	;
width_timing_check
	: '$width' '(' controlled_reference_event ',' timing_check_limit ',' threshold notifier_opt? ')' ';'
	;
nochange_timing_check
	: '$nochange' '(' reference_event ',' data_event ',' start_edge_offset ',' end_edge_offset notifier_opt? ')' ';'
	;
// A.7.5.2 System timing check command arguments
timecheck_condition
	: mintypmax_expression
	;
controlled_reference_event
	: controlled_timing_check_event
	;
data_event
	: timing_check_event
	;
delayed_data
	: terminal_identifier ( '[' constant_mintypmax_expression ']' )?
	;
delayed_reference
	: terminal_identifier ( '[' constant_mintypmax_expression ']' )?
	;
end_edge_offset
	: mintypmax_expression
	;
event_based_flag
	: constant_expression
	;
notifier
	: variable_identifier
	;
reference_event
	: timing_check_event
	;
remain_active_flag
	: constant_mintypmax_expression
	;
timestamp_condition
	: mintypmax_expression
	;
start_edge_offset
	: mintypmax_expression
	;
threshold
	: constant_expression
	;
timing_check_limit
	: expression
	;
// A.7.5.3 System timing check event definitions
timing_check_event
	: timing_check_event_control? specify_terminal_descriptor ( '&&&' timing_check_condition )?
	;
controlled_timing_check_event
	: timing_check_event_control specify_terminal_descriptor ( '&&&' timing_check_condition )?
	;
timing_check_event_control
	: 'posedge'
	| 'negedge'
	| 'edge'
	| edge_control_specifier
	;
specify_terminal_descriptor
	: ( interface_identifier '.' )? port_identifier ( '[' constant_range_expression ']' )?
	;
edge_control_specifier
	: 'edge' '[' edge_descriptor ( ',' edge_descriptor )* ']'
	;
edge_descriptor
	: SIMPLE_IDENTIFIER
	| UNSIGNED_NUMBER
	| ZERO_OR_ONE_X_OR_Z
	;
timing_check_condition
	: scalar_timing_check_condition
	| '(' scalar_timing_check_condition ')'
	;
scalar_timing_check_condition
	: expression
	| '~' expression
	| expression '==' scalar_constant
	| expression '===' scalar_constant
	| expression '!=' scalar_constant
	| expression '!==' scalar_constant
	;
scalar_constant
	: binary_number
	| unsigned_number
	;
// A.8.1 Concatenations
concatenation
	: '{' expression ( ',' expression )* '}'
	;
constant_concatenation
	: '{' constant_expression ( ',' constant_expression )* '}'
	;
constant_multiple_concatenation
	: '{' constant_expression constant_concatenation '}'
	;
module_path_concatenation
	: '{' module_path_expression ( ',' module_path_expression )* '}'
	;
module_path_multiple_concatenation
	: '{' constant_expression module_path_concatenation '}'
	;
multiple_concatenation
	: '{' expression concatenation '}'
	;
streaming_concatenation
	: '{' stream_operator slice_size? stream_concatenation '}'
	;
stream_operator
	: '>>'
	| '<<'
	;
slice_size
	: simple_type
	| constant_expression
	;
stream_concatenation
	: '{' stream_expression ( ',' stream_expression )* '}'
	;
stream_expression
	: expression ( 'with' '[' array_range_expression ']' )?
	;
array_range_expression
	: expression
	| expression ':' expression
	| expression '+:' expression
	| expression '-:' expression
	;
empty_unpacked_array_concatenation
	: '{' '}'
	;
// A.8.2 Subroutine calls
system_tf_call
	: system_tf_identifier arg_list?
	| system_tf_identifier '(' data_type ( ',' expression )? ')'
	| system_tf_identifier '(' expression ( ',' ordered_arg )* ',' clocking_event ')'
	;
arg_list
	: '(' list_of_arguments ')'
	;
subroutine_call
	: ( package_scope | '$root' '.' )? identifier attribute_instance* arg_list?
	| system_tf_call
	| method_call_root '.' array_manipulation_call
	| ( 'std' '::' | method_call_root '.' )? randomize_call
	;
list_of_arguments
	: ordered_arg ( ',' ordered_arg )* ( ',' named_arg )*
	| named_arg ( ',' named_arg )*
	;
ordered_arg
	: expression?
	;
named_arg
	: '.' identifier '(' expression? ')'
	;
array_manipulation_call
	: array_method_name attribute_instance* arg_list? ( 'with' '(' expression ')' )?
	;
randomize_call
	: 'randomize' attribute_instance* rand_list? rand_with?
	;
rand_list
	: '(' ( variable_identifier_list | 'null' )? ')'
	;
rand_with
	: 'with' id_list? constraint_block
	;
id_list
	: '(' identifier_list? ')'
	;
method_call_root
	: primary
	| implicit_class_handle
	;
array_method_name
	: method_identifier
	| 'unique'
	| 'and'
	| 'or'
	| 'xor'
	;
// A.8.3 Expressions
inc_or_dec_expression
	: inc_or_dec_operator attribute_instance* variable_lvalue
	| variable_lvalue attribute_instance* inc_or_dec_operator
	;
constant_expression
	: constant_primary
	| unary_operator attribute_instance* constant_primary
	| constant_expression '**' attribute_instance* constant_expression
	| constant_expression ( '*' | '/' | '%' ) attribute_instance* constant_expression
	| constant_expression ( '+' | '-' ) attribute_instance* constant_expression
	| constant_expression ( '>>' | '<<' | '>>>' | '<<<' ) attribute_instance* constant_expression
	| constant_expression ( '<' | '<=' | '>' | '>=' ) attribute_instance* constant_expression
	| constant_expression ( '==' | '!=' | '===' | '!==' | '==?' | '!=?' ) attribute_instance* constant_expression
	| constant_expression '&' attribute_instance* constant_expression
	| constant_expression ( '^' | '^~' | '~^' ) attribute_instance* constant_expression
	| constant_expression '|' attribute_instance* constant_expression
	| constant_expression '&&' attribute_instance* constant_expression
	| constant_expression '||' attribute_instance* constant_expression
	| <assoc=right> constant_expression '?' attribute_instance* constant_expression ':' constant_expression
	| <assoc=right> constant_expression ( '->' | '<->' ) attribute_instance* constant_expression
	;
constant_mintypmax_expression
	: constant_expression ( ':' constant_expression ':' constant_expression )?
	;
constant_param_expression
	: constant_mintypmax_expression
	| data_type
	| '$'
	;
param_expression
	: mintypmax_expression
	| data_type
	| '$'
	;
constant_range_expression
	: constant_expression
	| constant_part_select_range
	;
constant_part_select_range
	: constant_range
	| constant_indexed_range
	;
constant_range
	: constant_expression ':' constant_expression
	;
constant_indexed_range
	: constant_expression '+:' constant_expression
	| constant_expression '-:' constant_expression
	;
expression
	: primary
	| '(' operator_assignment ')'
	| unary_operator attribute_instance* primary
	| inc_or_dec_expression
	| tagged_union_expression
	| expression '**' attribute_instance* expression
	| expression ( '*' | '/' | '%' ) attribute_instance* expression
	| expression ( '+' | '-' ) attribute_instance* expression
	| expression ( '>>' | '<<' | '>>>' | '<<<' ) attribute_instance* expression
	| expression ( ( '<' | '<=' | '>' | '>=' ) attribute_instance* expression | 'inside' '{' open_range_list '}' )
	| expression ( '==' | '!=' | '===' | '!==' | '==?' | '!=?' ) attribute_instance* expression
	| expression '&' attribute_instance* expression
	| expression ( '^' | '^~' | '~^' ) attribute_instance* expression
	| expression '|' attribute_instance* expression
	| expression '&&' attribute_instance* expression
	| expression '||' attribute_instance* expression
	| <assoc=right> expression ( 'matches' pattern )? ( '&&&' expression_or_cond_pattern )* '?' attribute_instance* expression ':' expression
	| <assoc=right> expression ( '->' | '<->' ) attribute_instance* expression
	;
tagged_union_expression
	: 'tagged' member_identifier expression?
	;
value_range
	: expression
	| '[' expression ':' expression ']'
	;
mintypmax_expression
	: expression ( ':' expression ':' expression )?
	;
module_path_expression
	: module_path_primary
	| unary_module_path_operator attribute_instance* module_path_primary
	| module_path_expression ( '==' | '!=' ) attribute_instance* module_path_expression
	| module_path_expression '&' attribute_instance* module_path_expression
	| module_path_expression ( '^' | '^~' | '~^' ) attribute_instance* module_path_expression
	| module_path_expression '|' attribute_instance* module_path_expression
	| module_path_expression '&&' attribute_instance* module_path_expression
	| module_path_expression '||' attribute_instance* module_path_expression
	| <assoc=right> module_path_expression '?' attribute_instance* module_path_expression ':' module_path_expression
	;
module_path_mintypmax_expression
	: module_path_expression ( ':' module_path_expression ':' module_path_expression )?
	;
part_select_range
	: constant_range
	| indexed_range
	;
indexed_range
	: expression '+:' constant_expression
	| expression '-:' constant_expression
	;
genvar_expression
	: constant_expression
	;
// A.8.4 Primaries
constant_primary
	: primary_literal
	| ( package_or_class_scope | gen_ref+ )? identifier constant_select?
	| constant_concatenation ( '[' constant_range_expression ']' )?
	| constant_multiple_concatenation ( '[' constant_range_expression ']' )?
	| package_scope? identifier ( attribute_instance+ | attribute_instance* arg_list )
	| '$root' '.' identifier attribute_instance* arg_list?
	| system_tf_call
	| method_call_root '.' array_manipulation_call
	| ( 'std' '::' | method_call_root '.' )? randomize_call
	| '(' constant_mintypmax_expression ')'
	| constant_primary '\'' '(' constant_expression ')'
	| ( simple_type | signing | 'string' | 'const' ) '\'' '(' constant_expression ')'
	| constant_assignment_pattern_expression
	| type_reference
	| 'null'
	;
module_path_primary
	: number
	| module_path_concatenation
	| module_path_multiple_concatenation
	| ( package_scope | '$root' '.' )? identifier attribute_instance* arg_list?
	| system_tf_call
	| method_call_root '.' array_manipulation_call
	| ( 'std' '::' | method_call_root '.' )? randomize_call
	| '(' module_path_mintypmax_expression ')'
	;
primary
	: primary_literal
	| package_or_class_scope? hierarchical_identifier select_?
	| implicit_class_handle '.' ( hier_ref+ identifier | '$root' '.' hier_ref* identifier | hierarchical_identifier select_ )
	| 'local' '::' ( implicit_class_handle '.' | class_scope )? hierarchical_identifier select_?
	| empty_unpacked_array_concatenation
	| concatenation ( '[' range_expression ']' )?
	| multiple_concatenation ( '[' range_expression ']' )?
	| ( package_scope | '$root' '.' )? identifier ( attribute_instance+ | attribute_instance* arg_list )
	| system_tf_call
	| primary '.' ( array_manipulation_call | randomize_call )
	| ( 'this' '.' )? 'super' '.' ( array_manipulation_call | randomize_call )
	| ( 'std' '::' )? randomize_call
	| '(' mintypmax_expression ')'
	| primary '\'' '(' expression ')'
	| ( integer_type | non_integer_type | signing | 'string' | 'const' ) '\'' '(' expression ')'
	| assignment_pattern_expression
	| streaming_concatenation
	| sequence_method_call
	| 'this'
	| '$'
	| 'null'
	;
range_expression
	: expression
	| part_select_range
	;
primary_literal
	: number
	| time_literal
	| unbased_unsized_literal
	| string_literal
	;
time_literal
	: TIME_LITERAL
	;
implicit_class_handle
	: 'this' ( '.' 'super' )?
	| 'super'
	;
bit_select
	: ( '[' expression ']' )+
	;
select_
	: '[' part_select_range ']'
	| bit_select ( '[' part_select_range ']' )?
	| member_select+ ( '[' part_select_range ']' )?
	;
nonrange_select
	: bit_select
	| member_select+
	;
member_select
	: '.' member_identifier bit_select?
	;
constant_bit_select
	: ( '[' constant_expression ']' )+
	;
constant_select
	: '[' constant_part_select_range ']'
	| constant_bit_select ( '[' constant_part_select_range ']' )?
	| const_member_select+ ( '[' constant_part_select_range ']' )?
	;
const_member_select
	: '.' member_identifier constant_bit_select?
	;
// A.8.5 Expression left-side values
net_lvalue
	: ps_or_hierarchical_identifier constant_select?
	| '{' net_lvalue ( ',' net_lvalue )* '}'
	| assignment_pattern_expression_type? assignment_pattern_net_lvalue
	;
variable_lvalue
	: ( implicit_class_handle '.' | package_scope )? hierarchical_identifier select_?
	| '{' variable_lvalue ( ',' variable_lvalue )* '}'
	| assignment_pattern_expression_type? assignment_pattern_variable_lvalue
	| streaming_concatenation
	;
nonrange_variable_lvalue
	: ( implicit_class_handle '.' | package_scope )? hierarchical_identifier nonrange_select?
	;
// A.8.6 Operators
unary_operator
	: '+'
	| '-'
	| '!'
	| '~'
	| '&'
	| '~&'
	| '|'
	| '~|'
	| '^'
	| '~^'
	| '^~'
	;
inc_or_dec_operator
	: '++'
	| '--'
	;
unary_module_path_operator
	: '!'
	| '~'
	| '&'
	| '~&'
	| '|'
	| '~|'
	| '^'
	| '~^'
	| '^~'
	;
// A.8.7 Numbers
number
	: integral_number
	| real_number
	;
integral_number
	: decimal_number
	| octal_number
	| binary_number
	| hex_number
	;
decimal_number
	: unsigned_number
	| size? decimal_base decimal_value
	;
binary_number
	: size? binary_base binary_value
	;
octal_number
	: size? octal_base octal_value
	;
hex_number
	: size? hex_base hex_value
	;
size
	: UNSIGNED_NUMBER
	;
real_number
	: fixed_point_number
	| exponential_number
	;
fixed_point_number
	: FIXED_POINT_NUMBER
	;
exponential_number
	: EXPONENTIAL_NUMBER
	;
unsigned_number
	: UNSIGNED_NUMBER
	;
decimal_value
	: UNSIGNED_NUMBER
	| X_OR_Z_UNDERSCORE
	;
binary_value
	: BINARY_VALUE
	;
octal_value
	: OCTAL_VALUE
	;
hex_value
	: HEX_VALUE
	;
decimal_base
	: DECIMAL_BASE
	;
binary_base
	: BINARY_BASE
	;
octal_base
	: OCTAL_BASE
	;
hex_base
	: HEX_BASE
	;
unbased_unsized_literal
	: UNBASED_UNSIZED_LITERAL
	;
// A.8.8 Strings
string_literal
	: STRING_LITERAL
	;
// A.9.1 Attributes
attribute_instance
	: '(' '*' attr_spec ( ',' attr_spec )* '*' ')'
	;
attr_spec
	: attr_name ( '=' constant_expression )?
	;
attr_name
	: identifier
	;
// A.9.3 Identifiers
block_identifier
	: identifier
	;
bin_identifier
	: identifier
	;
c_identifier
	: SIMPLE_IDENTIFIER
	;
cell_identifier
	: identifier
	;
checker_identifier
	: identifier
	;
class_identifier
	: identifier
	;
class_variable_identifier
	: identifier
	;
clocking_identifier
	: identifier
	;
config_identifier
	: identifier
	;
const_identifier
	: identifier
	;
constraint_identifier
	: identifier
	;
covergroup_identifier
	: identifier
	;
cover_point_identifier
	: identifier
	;
cross_identifier
	: identifier
	;
dynamic_array_variable_identifier
	: identifier
	;
enum_identifier
	: identifier
	;
escaped_identifier
	: ESCAPED_IDENTIFIER
	;
formal_port_identifier
	: identifier
	;
function_identifier
	: identifier
	;
generate_block_identifier
	: identifier
	;
genvar_identifier
	: identifier
	;
hierarchical_identifier
	: ( '$root' '.' )? hier_ref* identifier
	;
hier_ref
	: identifier constant_bit_select? '.'
	;
identifier
	: simple_identifier
	| escaped_identifier
	;
index_variable_identifier
	: identifier
	;
interface_identifier
	: identifier
	;
interface_instance_identifier
	: identifier
	;
input_port_identifier
	: identifier
	;
instance_identifier
	: identifier
	;
library_identifier
	: identifier
	;
member_identifier
	: identifier
	;
method_identifier
	: identifier
	;
modport_identifier
	: identifier
	;
module_identifier
	: identifier
	;
net_identifier
	: identifier
	;
net_type_identifier
	: identifier
	;
output_port_identifier
	: identifier
	;
package_identifier
	: identifier
	;
package_scope
	: package_identifier '::'
	| '$unit' '::'
	;
parameter_identifier
	: identifier
	;
port_identifier
	: identifier
	;
production_identifier
	: identifier
	;
program_identifier
	: identifier
	;
property_identifier
	: identifier
	;
ps_identifier
	: package_scope? identifier
	;
ps_or_hierarchical_array_identifier
	: ( implicit_class_handle '.' | package_or_class_scope )? hierarchical_identifier
	;
ps_or_hierarchical_identifier
	: package_scope? identifier
	| hier_ref+ identifier
	| '$root' '.' hier_ref* identifier
	;
ps_type_or_parameter_identifier
	: ( 'local' '::' | package_or_class_scope | gen_ref+ )? identifier
	;
gen_ref
	: generate_block_identifier ( '[' constant_expression ']' )? '.'
	;
sequence_identifier
	: identifier
	;
signal_identifier
	: identifier
	;
simple_identifier
	: SIMPLE_IDENTIFIER
	;
specparam_identifier
	: identifier
	;
system_tf_identifier
	: SYSTEM_TF_IDENTIFIER
	| '$error'
	| '$fatal'
	| '$info'
	| '$warning'
	;
task_identifier
	: identifier
	;
tf_identifier
	: identifier
	;
terminal_identifier
	: identifier
	;
topmodule_identifier
	: identifier
	;
type_identifier
	: identifier
	;
udp_identifier
	: identifier
	;
variable_identifier
	: identifier
	;
