// Author: Mustafa Said Ağca
// License: MIT

parser grammar VerilogParser;

options { tokenVocab=VerilogLexer; }

// 17. System tasks and functions
// 17.1 Display system tasks
// 17.1.1 The display and write tasks

display_tasks
	: display_task_name (LEFT_PARENTHESIS list_of_arguments RIGHT_PARENTHESIS)? SEMICOLON
	;

display_task_name
	: DOLLAR_DISPLAY
	| DOLLAR_DISPLAYB
	| DOLLAR_DISPLAYO
	| DOLLAR_DISPLAYH
	| DOLLAR_WRITE
	| DOLLAR_WRITEB
	| DOLLAR_WRITEO
	| DOLLAR_WRITEH
	;

list_of_arguments
	: argument? (COMMA argument)*
	;

argument
	: expression
	| constant_expression
	| time_function
	| stime_function
	| realtime_function
	;

// 17.1.2 Strobed monitoring

strobe_tasks
	: strobe_task_name (LEFT_PARENTHESIS list_of_arguments RIGHT_PARENTHESIS)? SEMICOLON
	;

strobe_task_name
	: DOLLAR_STROBE
	| DOLLAR_STROBEB
	| DOLLAR_STROBEO
	| DOLLAR_STROBEH
	;

// 17.1.3 Continuous monitoring

monitor_tasks
	: monitor_task_name (LEFT_PARENTHESIS list_of_arguments RIGHT_PARENTHESIS)? SEMICOLON
	| DOLLAR_MONITORON SEMICOLON
	| DOLLAR_MONITOROFF SEMICOLON
	;

monitor_task_name
	: DOLLAR_MONITOR
	| DOLLAR_MONITORB
	| DOLLAR_MONITORO
	| DOLLAR_MONITORH
	;

// 17.2 File input-output system tasks and functions
// 17.2.1 Opening and closing files

file_open_function
	: multi_channel_descriptor EQUAL DOLLAR_FOPEN LEFT_PARENTHESIS file_name RIGHT_PARENTHESIS SEMICOLON
	| fd EQUAL DOLLAR_FOPEN LEFT_PARENTHESIS file_name COMMA type_ RIGHT_PARENTHESIS SEMICOLON
	;

file_close_task
	: DOLLAR_FCLOSE LEFT_PARENTHESIS multi_channel_descriptor RIGHT_PARENTHESIS SEMICOLON
	| DOLLAR_FCLOSE LEFT_PARENTHESIS fd RIGHT_PARENTHESIS SEMICOLON
	;

multi_channel_descriptor
	: variable_identifier
	;

fd
	: variable_identifier
	;

file_name
	: STRING
	;

type_
	: STRING
	| variable_identifier
	;

// 17.2.2 File output system tasks

file_output_tasks
	: file_output_task_name LEFT_PARENTHESIS multi_channel_descriptor (COMMA list_of_arguments)? RIGHT_PARENTHESIS SEMICOLON
	| file_output_task_name LEFT_PARENTHESIS fd (COMMA list_of_arguments)? RIGHT_PARENTHESIS SEMICOLON
	;

file_output_task_name
	: DOLLAR_FDISPLAY
	| DOLLAR_FDISPLAYB
	| DOLLAR_FDISPLAYH
	| DOLLAR_FDISPLAYO
	| DOLLAR_FWRITE
	| DOLLAR_FWRITEB
	| DOLLAR_FWRITEH
	| DOLLAR_FWRITEO
	| DOLLAR_FSTROBE
	| DOLLAR_FSTROBEB
	| DOLLAR_FSTROBEH
	| DOLLAR_FSTROBEO
	| DOLLAR_FMONITOR
	| DOLLAR_FMONITORB
	| DOLLAR_FMONITORH
	| DOLLAR_FMONITORO
	;

// 17.2.9 Loading memory data from a file

load_memory_tasks
	: DOLLAR_READMEMB LEFT_PARENTHESIS filename COMMA memory_name (COMMA start_addr (COMMA finish_addr)?)? RIGHT_PARENTHESIS SEMICOLON
	| DOLLAR_READMEMH LEFT_PARENTHESIS filename COMMA memory_name (COMMA start_addr (COMMA finish_addr)?)? RIGHT_PARENTHESIS SEMICOLON
	;

memory_name
	: variable_identifier
	;

start_addr
	: DECIMAL_NUMBER
	;

finish_addr
	: DECIMAL_NUMBER
	;

filename
	: STRING
	| variable_identifier
	;

// 17.4 Simulation control system tasks
// 17.4.1 $finish

finish_task
	: DOLLAR_FINISH (LEFT_PARENTHESIS finish_number RIGHT_PARENTHESIS)? SEMICOLON
	;

finish_number
	: DECIMAL_NUMBER
	;

// 17.4.2 $stop

stop_task
	: DOLLAR_STOP (LEFT_PARENTHESIS finish_number RIGHT_PARENTHESIS)? SEMICOLON
	;

// 17.7 Simulation time system functions

time_function
	: TIME
	;

// 17.7.2 $stime

stime_function
	: DOLLAR_STIME
	;

// 17.7.3 $realtime

realtime_function
	: REALTIME
	;

// 17.8 Conversion functions

conversion_functions
	: conversion_function_name LEFT_PARENTHESIS constant_argument RIGHT_PARENTHESIS
	;

conversion_function_name
	: DOLLAR_RTOI
	| DOLLAR_ITOR
	| DOLLAR_REALTOBITS
	| DOLLAR_BITSTOREAL
	| DOLLAR_SIGNED
	| DOLLAR_UNSIGNED
	;

constant_argument
	: constant_expression
	;

// 17.9 Probabilistic distribution functions
// 17.9.1 $random function

random_function
	: DOLLAR_RANDOM (LEFT_PARENTHESIS seed RIGHT_PARENTHESIS)?
	;

seed
	: variable_identifier
	;

// 17.9.2 $dist_ functions

dist_functions
	: DOLLAR_DIST_UNIFORM LEFT_PARENTHESIS seed COMMA start_ COMMA end RIGHT_PARENTHESIS
	| DOLLAR_DIST_NORMAL LEFT_PARENTHESIS seed COMMA mean COMMA standard_deviation RIGHT_PARENTHESIS
	| DOLLAR_DIST_EXPONENTIAL LEFT_PARENTHESIS seed COMMA mean RIGHT_PARENTHESIS
	| DOLLAR_DIST_POISSON LEFT_PARENTHESIS seed COMMA mean RIGHT_PARENTHESIS
	| DOLLAR_DIST_CHI_SQUARE LEFT_PARENTHESIS seed COMMA degree_of_freedom RIGHT_PARENTHESIS
	| DOLLAR_DIST_T LEFT_PARENTHESIS seed COMMA degree_of_freedom RIGHT_PARENTHESIS
	| DOLLAR_DIST_ERLANG LEFT_PARENTHESIS seed COMMA k_stage COMMA mean RIGHT_PARENTHESIS
	;

start_
	: DECIMAL_NUMBER
	;

end
	: DECIMAL_NUMBER
	;

mean
	: DECIMAL_NUMBER
	;

standard_deviation
	: DECIMAL_NUMBER
	;

degree_of_freedom
	: DECIMAL_NUMBER
	;

k_stage
	: DECIMAL_NUMBER
	;

// 17.11 Math functions

math_functions
	: integer_math_functions
	| real_math_functions
	;

integer_math_functions
	: DOLLAR_CLOG2 LEFT_PARENTHESIS constant_argument RIGHT_PARENTHESIS
	;

real_math_functions
	: single_argument_real_math_function_name LEFT_PARENTHESIS constant_argument RIGHT_PARENTHESIS
	| double_argument_real_math_function_name LEFT_PARENTHESIS constant_argument COMMA constant_argument RIGHT_PARENTHESIS
	;

single_argument_real_math_function_name
	: DOLLAR_LN
	| DOLLAR_LOG10
	| DOLLAR_EXP
	| DOLLAR_SQRT
	| DOLLAR_FLOOR
	| DOLLAR_CEIL
	| DOLLAR_SIN
	| DOLLAR_COS
	| DOLLAR_TAN
	| DOLLAR_ASIN
	| DOLLAR_ACOS
	| DOLLAR_ATAN
	| DOLLAR_SINH
	| DOLLAR_COSH
	| DOLLAR_TANH
	| DOLLAR_ASINH
	| DOLLAR_ACOSH
	| DOLLAR_ATANH
	;

double_argument_real_math_function_name
	: DOLLAR_POW
	| DOLLAR_ATAN2
	| DOLLAR_HYPOT
	;

// 18. Value change dump (VCD) files
// 18.1 Creating four-state VCD file
// 18.1.1 Specifying name of dump file ($dumpfile)

dumpfile_task
	: DOLLAR_DUMPFILE LEFT_PARENTHESIS filename RIGHT_PARENTHESIS SEMICOLON
	;

// 18.1.2 Specifying variables to be dumped ($dumpvars)

dumpvars_task
	: DOLLAR_DUMPVARS SEMICOLON
	| DOLLAR_DUMPVARS LEFT_PARENTHESIS levels (COMMA list_of_modules_or_variables)? RIGHT_PARENTHESIS SEMICOLON
	;

list_of_modules_or_variables
	: module_or_variable (COMMA module_or_variable)*
	;

module_or_variable
	: module_identifier
	| variable_identifier
	;

levels
	: DECIMAL_NUMBER
	;

// 18.1.3 Stopping and resuming the dump ($dumpoff/$dumpon)

dumpoff_task
	: DOLLAR_DUMPOFF SEMICOLON
	;

dumpon_task
	: DOLLAR_DUMPON SEMICOLON
	;

// 18.1.4 Generating a checkpoint ($dumpall)

dumpall_task
	: DOLLAR_DUMPALL SEMICOLON
	;

// 18.1.5 Limiting size of dump file ($dumplimit)

dumplimit_task
	: DOLLAR_DUMPLIMIT LEFT_PARENTHESIS file_size RIGHT_PARENTHESIS SEMICOLON
	;

file_size
	: DECIMAL_NUMBER
	;

// 18.1.6 Reading dump file during simulation ($dumpflush)

dumpflush_task
	: DOLLAR_DUMPFLUSH SEMICOLON
	;

// 18.3 Creating extended VCD file
// 18.3.1 Specifying dump file name and ports to be dumped ($dumpports)

dumpports_task
	: DOLLAR_DUMPPORTS LEFT_PARENTHESIS scope_list COMMA file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

scope_list
	: module_identifier (COMMA module_identifier)*
	;

file_pathname
	: STRING
	| variable_identifier
	| expression
	;

// 18.3.2 Stopping and resuming the dump ($dumpportsoff/$dumpportson)

dumpportsoff_task
	: DOLLAR_DUMPPORTSOFF LEFT_PARENTHESIS file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

dumpportson_task
	: DOLLAR_DUMPPORTSON LEFT_PARENTHESIS file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

// 18.3.3 Generating a checkpoint ($dumpportsall)

dumpportsall_task
	: DOLLAR_DUMPPORTSALL LEFT_PARENTHESIS file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

// 18.3.4 Limiting size of dump file ($dumpportslimit)

dumpportslimit_task
	: DOLLAR_DUMPPORTSLIMIT LEFT_PARENTHESIS file_size COMMA file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

// 18.3.5 Reading dump file during simulation ($dumpportsflush)

dumpportsflush_task
	: DOLLAR_DUMPPORTSFLUSH LEFT_PARENTHESIS file_pathname RIGHT_PARENTHESIS SEMICOLON
	;

// A.1 Source text
// A.1.1 Library source text

library_text
	: library_description*
	;

library_description
	: library_declaration
	| include_statement
	| config_declaration
	;

library_declaration
	: LIBRARY library_identifier FILE_PATH_SPEC (COMMA FILE_PATH_SPEC)* (MINUS_INCDIR FILE_PATH_SPEC (COMMA FILE_PATH_SPEC)*)? SEMICOLON
	;

include_statement
	: INCLUDE FILE_PATH_SPEC SEMICOLON
	;

// A.1.2 Verilog source text
// START SYMBOL
source_text
	: description* EOF
	;

description
	: module_declaration
	| config_declaration
	;

module_declaration
	: attribute_instance* module_keyword module_identifier module_parameter_port_list? list_of_ports SEMICOLON module_item* ENDMODULE
	| attribute_instance* module_keyword module_identifier module_parameter_port_list? list_of_port_declarations? SEMICOLON non_port_module_item* ENDMODULE
	;

module_keyword
	: MODULE
	| MACROMODULE
	;

// A.1.3 Module parameters and ports

module_parameter_port_list
	: HASH LEFT_PARENTHESIS parameter_declaration (COMMA parameter_declaration)* RIGHT_PARENTHESIS
	;

list_of_ports
	: LEFT_PARENTHESIS port (COMMA port)* RIGHT_PARENTHESIS
	;

list_of_port_declarations
	: LEFT_PARENTHESIS port_declaration (COMMA port_declaration)* RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS RIGHT_PARENTHESIS
	;

port
	: port_expression?
	| DOT port_identifier LEFT_PARENTHESIS port_expression? RIGHT_PARENTHESIS
	;

port_expression
	: port_reference
	| LEFT_BRACE port_reference (COMMA port_reference)* RIGHT_BRACE
	;

port_reference
	: port_identifier (LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	;

port_declaration
	: attribute_instance* inout_declaration
	| attribute_instance* input_declaration
	| attribute_instance* output_declaration
	;

// A.1.4 Module items

module_item
	: port_declaration SEMICOLON
	| non_port_module_item
	;

module_or_generate_item
	: attribute_instance* module_or_generate_item_declaration
	| attribute_instance* local_parameter_declaration SEMICOLON
	| attribute_instance* parameter_override
	| attribute_instance* continuous_assign
	| attribute_instance* gate_instantiation
	| attribute_instance* module_instantiation
	| attribute_instance* initial_construct
	| attribute_instance* always_construct
	| attribute_instance* loop_generate_construct
	| attribute_instance* conditional_generate_construct
	;

module_or_generate_item_declaration
	: net_declaration
	| reg_declaration
	| integer_declaration
	| real_declaration
	| time_declaration
	| realtime_declaration
	| event_declaration
	| genvar_declaration
	| task_declaration
	| function_declaration
	;

non_port_module_item
	: module_or_generate_item
	| generate_region
	| specify_block
	| attribute_instance* parameter_declaration SEMICOLON
	| attribute_instance* specparam_declaration
	;

parameter_override
	: DEFPARAM list_of_param_assignments SEMICOLON
	;

// A.1.5 Configuration source text

config_declaration
	: CONFIG config_identifier SEMICOLON design_statement config_rule_statement* ENDCONFIG
	;

design_statement
	: DESIGN ((library_identifier DOT)? cell_identifier)* SEMICOLON
	;

config_rule_statement
	: default_clause liblist_clause
	| inst_clause liblist_clause
	| inst_clause use_clause
	| cell_clause liblist_clause
	| cell_clause use_clause
	;

default_clause
	: DEFAULT
	;

inst_clause
	: INSTANCE inst_name
	;

inst_name
	: topmodule_identifier (DOT instance_identifier)*
	;

cell_clause
	: CELL (library_identifier DOT)? cell_identifier
	;

liblist_clause
	: LIBLIST library_identifier*
	;

use_clause
	: USE (library_identifier DOT)? cell_identifier (COLON CONFIG)?
	;

// A.2 Declarations
// A.2.1 Declaration types
// A.2.1.1 Module parameter declarations

local_parameter_declaration
	: LOCALPARAM SIGNED? range_? list_of_param_assignments
	| LOCALPARAM parameter_type list_of_param_assignments
	;

parameter_declaration
	: PARAMETER SIGNED? range_? list_of_param_assignments
	| PARAMETER parameter_type list_of_param_assignments
	;

specparam_declaration
	: SPECPARAM range_? list_of_specparam_assignments SEMICOLON
	;

parameter_type
	: INTEGER
	| REAL
	| REALTIME
	| TIME
	;

// A.2.1.2 Port declarations

inout_declaration
	: INOUT net_type? SIGNED? range_? list_of_port_identifiers
	;

input_declaration
	: INPUT net_type? SIGNED? range_? list_of_port_identifiers
	;

output_declaration
	: OUTPUT net_type? SIGNED? range_? list_of_port_identifiers
	| OUTPUT REG SIGNED? range_? list_of_variable_port_identifiers
	| OUTPUT output_variable_type list_of_variable_port_identifiers
	;

// A.2.1.3 Type declarations

event_declaration
	: EVENT list_of_event_identifiers SEMICOLON
	;

integer_declaration
	: INTEGER list_of_variable_identifiers SEMICOLON
	;

net_declaration
	: net_type SIGNED? delay3? list_of_net_identifiers SEMICOLON
	| net_type drive_strength? SIGNED? delay3? list_of_net_decl_assignments SEMICOLON
	| net_type (VECTORED | SCALARED)? SIGNED? range_ delay3? list_of_net_identifiers SEMICOLON
	| net_type drive_strength? (VECTORED | SCALARED)? SIGNED? range_ delay3? list_of_net_decl_assignments SEMICOLON
	| TRIREG charge_strength? SIGNED? delay3? list_of_net_identifiers SEMICOLON
	| TRIREG drive_strength? SIGNED? delay3? list_of_net_decl_assignments SEMICOLON
	| TRIREG charge_strength? (VECTORED | SCALARED)? SIGNED? range_ delay3? list_of_net_identifiers SEMICOLON
	| TRIREG drive_strength? (VECTORED | SCALARED)? SIGNED? range_ delay3? list_of_net_decl_assignments SEMICOLON
	;

real_declaration
	: REAL list_of_real_identifiers SEMICOLON
	;

realtime_declaration
	: REALTIME list_of_real_identifiers SEMICOLON
	;

reg_declaration
	: REG SIGNED? range_? list_of_variable_identifiers SEMICOLON
	;

time_declaration
	: TIME list_of_variable_identifiers SEMICOLON
	;

// A.2.2 Declaration data types
// A.2.2.1 Net and variable types

net_type
	: SUPPLY0
	| SUPPLY1
	| TRI
	| TRIAND
	| TRIOR
	| TRI0
	| TRI1
	| WIRE
	| WAND
	| WOR
	;

output_variable_type
	: INTEGER
	| TIME
	;

real_type
	: real_identifier dimension*
	| real_identifier EQUAL constant_expression
	;

variable_type
	: variable_identifier dimension*
	| variable_identifier EQUAL constant_expression
	;

// A.2.2.2 Strengths

drive_strength
	: LEFT_PARENTHESIS strength0 COMMA strength1 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength1 COMMA strength0 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength0 COMMA HIGHZ1 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength1 COMMA HIGHZ0 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS HIGHZ0 COMMA strength1 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS HIGHZ1 COMMA strength0 RIGHT_PARENTHESIS
	;

strength0
	: SUPPLY0
	| STRONG0
	| PULL0
	| WEAK0
	;

strength1
	: SUPPLY1
	| STRONG1
	| PULL1
	| WEAK1
	;

charge_strength
	: LEFT_PARENTHESIS SMALL RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS MEDIUM RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS LARGE RIGHT_PARENTHESIS
	;

// A.2.2.3 Delays

delay3
	: HASH delay_value
	| HASH LEFT_PARENTHESIS mintypmax_expression (COMMA mintypmax_expression (COMMA mintypmax_expression)?)? RIGHT_PARENTHESIS
	;

delay2
	: HASH delay_value
	| HASH LEFT_PARENTHESIS mintypmax_expression (COMMA mintypmax_expression)? RIGHT_PARENTHESIS
	;

delay_value
	: DECIMAL_NUMBER
	| REAL_NUMBER
	| identifier
	;

// A.2.3 Declaration lists

list_of_defparam_assignments
	: defparam_assignment (COMMA defparam_assignment)*
	;

list_of_event_identifiers
	: event_identifier dimension*? (COMMA event_identifier dimension*?)*
	;

list_of_net_decl_assignments
	: net_decl_assignment (COMMA net_decl_assignment)*
	;

list_of_net_identifiers
	: net_identifier dimension*? (COMMA net_identifier dimension*?)*
	;

list_of_param_assignments
	: param_assignment (COMMA param_assignment)*
	;

list_of_port_identifiers
	: port_identifier (COMMA port_identifier)*
	;

list_of_real_identifiers
	: real_type (COMMA real_type)*
	;

list_of_specparam_assignments
	: specparam_assignment (COMMA specparam_assignment)*
	;

list_of_variable_identifiers
	: variable_type (COMMA variable_type)*
	;

list_of_variable_port_identifiers
	: port_identifier (EQUAL constant_expression)? (COMMA port_identifier (EQUAL constant_expression)?)*
	;

// A.2.4 Declaration assignments

defparam_assignment
	: hierarchical_parameter_identifier EQUAL constant_mintypmax_expression
	;

net_decl_assignment
	: net_identifier EQUAL expression
	;

param_assignment
	: parameter_identifier EQUAL constant_mintypmax_expression
	;

specparam_assignment
	: specparam_identifier EQUAL constant_mintypmax_expression
	| pulse_control_specparam
	;

pulse_control_specparam
	: PATHPULSE_DOLLAR EQUAL LEFT_PARENTHESIS reject_limit_value (COMMA error_limit_value)? RIGHT_PARENTHESIS
	| PATHPULSE_DOLLAR specify_input_terminal_descriptor DOT specify_output_terminal_descriptor EQUAL LEFT_PARENTHESIS reject_limit_value (COMMA error_limit_value)? RIGHT_PARENTHESIS
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

// A.2.5 Declaration ranges

dimension
	: LEFT_BRACKET dimension_constant_expression COLON dimension_constant_expression RIGHT_BRACKET
	;

range_
	: LEFT_BRACKET msb_constant_expression COLON lsb_constant_expression RIGHT_BRACKET
	;

// A.2.6 Function declarations

function_declaration
	: FUNCTION AUTOMATIC? function_range_or_type? function_identifier SEMICOLON function_item_declaration function_item_declaration* function_statement ENDFUNCTION
	| FUNCTION AUTOMATIC? function_range_or_type? function_identifier LEFT_PARENTHESIS function_port_list RIGHT_PARENTHESIS SEMICOLON block_item_declaration* function_statement ENDFUNCTION
	;

function_item_declaration
	: block_item_declaration
	| attribute_instance* tf_input_declaration SEMICOLON
	;

function_port_list
	: attribute_instance* tf_input_declaration (COMMA attribute_instance* tf_input_declaration)*
	;

function_range_or_type
	: SIGNED? range_
	| INTEGER
	| REAL
	| REALTIME
	| TIME
	;

// A.2.7 Task declarations

task_declaration
	: TASK AUTOMATIC? task_identifier SEMICOLON task_item_declaration* statement_or_null ENDTASK
	| TASK AUTOMATIC? task_identifier LEFT_PARENTHESIS task_port_list? RIGHT_PARENTHESIS SEMICOLON block_item_declaration* statement_or_null ENDTASK
	;

task_item_declaration
	: block_item_declaration
	| attribute_instance* tf_input_declaration SEMICOLON
	| attribute_instance* tf_output_declaration SEMICOLON
	| attribute_instance* tf_inout_declaration SEMICOLON
	;

task_port_list
	: task_port_item (COMMA task_port_item)*
	;

task_port_item
	: attribute_instance* tf_input_declaration
	| attribute_instance* tf_output_declaration
	| attribute_instance* tf_inout_declaration
	;

tf_input_declaration
	: INPUT REG? SIGNED? range_? list_of_port_identifiers
	| INPUT task_port_type list_of_port_identifiers
	;

tf_output_declaration
	: OUTPUT REG? SIGNED? range_? list_of_port_identifiers
	| OUTPUT task_port_type list_of_port_identifiers
	;

tf_inout_declaration
	: INOUT REG? SIGNED? range_? list_of_port_identifiers
	| INOUT task_port_type list_of_port_identifiers
	;

task_port_type
	: INTEGER
	| REAL
	| REALTIME
	| TIME
	;

// A.2.8 Block item declarations

block_item_declaration
	: attribute_instance* REG SIGNED? range_? list_of_block_variable_identifiers SEMICOLON
	| attribute_instance* INTEGER list_of_block_variable_identifiers SEMICOLON
	| attribute_instance* TIME list_of_block_variable_identifiers SEMICOLON
	| attribute_instance* REAL list_of_block_real_identifiers SEMICOLON
	| attribute_instance* REALTIME list_of_block_real_identifiers SEMICOLON
	| attribute_instance* event_declaration
	| attribute_instance* local_parameter_declaration SEMICOLON
	| attribute_instance* parameter_declaration SEMICOLON
	;

list_of_block_variable_identifiers
	: block_variable_type (COMMA block_variable_type)*
	;

list_of_block_real_identifiers
	: block_real_type (COMMA block_real_type)*
	;

block_variable_type
	: variable_identifier dimension*
	;

block_real_type
	: real_identifier dimension*
	;

// A.3 Primitive instances
// A.3.1 Primitive instantiation and instances

gate_instantiation
	: cmos_switchtype delay3? cmos_switch_instance (COMMA cmos_switch_instance)* SEMICOLON
	| enable_gatetype drive_strength? delay3? enable_gate_instance (COMMA enable_gate_instance)* SEMICOLON
	| mos_switchtype delay3? mos_switch_instance (COMMA mos_switch_instance)* SEMICOLON
	| n_input_gatetype drive_strength? delay2? n_input_gate_instance (COMMA n_input_gate_instance)* SEMICOLON
	| n_output_gatetype drive_strength? delay2? n_output_gate_instance (COMMA n_output_gate_instance)* SEMICOLON
	| pass_en_switchtype delay2? pass_enable_switch_instance (COMMA pass_enable_switch_instance)* SEMICOLON
	| pass_switchtype pass_switch_instance (COMMA pass_switch_instance)* SEMICOLON
	| PULLDOWN pulldown_strength? pull_gate_instance (COMMA pull_gate_instance)* SEMICOLON
	| PULLUP pullup_strength? pull_gate_instance (COMMA pull_gate_instance)* SEMICOLON
	;

cmos_switch_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal COMMA input_terminal COMMA ncontrol_terminal COMMA pcontrol_terminal RIGHT_PARENTHESIS
	;

enable_gate_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal COMMA input_terminal COMMA enable_terminal RIGHT_PARENTHESIS
	;

mos_switch_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal COMMA input_terminal COMMA enable_terminal RIGHT_PARENTHESIS
	;

n_input_gate_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal COMMA input_terminal (COMMA input_terminal)* RIGHT_PARENTHESIS
	;

n_output_gate_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal (COMMA output_terminal)* COMMA input_terminal RIGHT_PARENTHESIS
	;

pass_switch_instance
	: name_of_gate_instance? LEFT_PARENTHESIS inout_terminal COMMA inout_terminal RIGHT_PARENTHESIS
	;

pass_enable_switch_instance
	: name_of_gate_instance? LEFT_PARENTHESIS inout_terminal COMMA inout_terminal COMMA enable_terminal RIGHT_PARENTHESIS
	;

pull_gate_instance
	: name_of_gate_instance? LEFT_PARENTHESIS output_terminal RIGHT_PARENTHESIS
	;

name_of_gate_instance
	: gate_instance_identifier range_?
	;

// A.3.2 Primitive strengths

pulldown_strength
	: LEFT_PARENTHESIS strength0 COMMA strength1 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength1 COMMA strength0 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength0 RIGHT_PARENTHESIS
	;

pullup_strength
	: LEFT_PARENTHESIS strength0 COMMA strength1 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength1 COMMA strength0 RIGHT_PARENTHESIS
	| LEFT_PARENTHESIS strength1 RIGHT_PARENTHESIS
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
	: CMOS
	| RCMOS
	;

enable_gatetype
	: BUFIF0
	| BUFIF1
	| NOTIF0
	| NOTIF1
	;

mos_switchtype
	: NMOS
	| PMOS
	| RNMOS
	| RPMOS
	;

n_input_gatetype
	: AND
	| NAND
	| OR
	| NOR
	| XOR
	| XNOR
	;

n_output_gatetype
	: BUF
	| NOT
	;

pass_en_switchtype
	: TRANIF0
	| TRANIF1
	| RTRANIF1
	| RTRANIF0
	;

pass_switchtype
	: TRAN
	| RTRAN
	;

// A.4 Module instantiation and generate construct
// A.4.1 Module instantiation

module_instantiation
	: module_identifier parameter_value_assignment? module_instance (COMMA module_instance)* SEMICOLON
	;

parameter_value_assignment
	: HASH LEFT_PARENTHESIS list_of_parameter_assignments RIGHT_PARENTHESIS
	;

list_of_parameter_assignments
	: ordered_parameter_assignment (COMMA ordered_parameter_assignment)*
	| named_parameter_assignment (COMMA named_parameter_assignment)*
	;

ordered_parameter_assignment
	: expression
	;

named_parameter_assignment
	: DOT parameter_identifier LEFT_PARENTHESIS mintypmax_expression? RIGHT_PARENTHESIS
	;

module_instance
	: name_of_module_instance LEFT_PARENTHESIS list_of_port_connections RIGHT_PARENTHESIS
	;

name_of_module_instance
	: module_instance_identifier range_?
	;

list_of_port_connections
	: ordered_port_connection (COMMA ordered_port_connection)*
	| named_port_connection (COMMA named_port_connection)*
	;

ordered_port_connection
	: attribute_instance* expression?
	;

named_port_connection
	: attribute_instance* DOT port_identifier LEFT_PARENTHESIS expression? RIGHT_PARENTHESIS
	;

// A.4.2 Generate construct

generate_region
	: GENERATE module_or_generate_item* ENDGENERATE
	;

genvar_declaration
	: GENVAR list_of_genvar_identifiers SEMICOLON
	;

list_of_genvar_identifiers
	: genvar_identifier (COMMA genvar_identifier)*
	;

loop_generate_construct
	: FOR LEFT_PARENTHESIS genvar_initialization SEMICOLON genvar_expression SEMICOLON genvar_iteration RIGHT_PARENTHESIS generate_block
	;

genvar_initialization
	: genvar_identifier EQUAL constant_expression
	;

genvar_expression
	: genvar_primary
	| unary_operator attribute_instance* genvar_primary
	| genvar_expression binary_operator attribute_instance* genvar_expression
	| genvar_expression QUESTION_MARK attribute_instance* genvar_expression COLON genvar_expression
	;

genvar_iteration
	: genvar_identifier EQUAL genvar_expression
	;

genvar_primary
	: constant_primary
	| genvar_identifier
	;

conditional_generate_construct
	: if_generate_construct
	| case_generate_construct
	;

if_generate_construct
	: IF LEFT_PARENTHESIS constant_expression RIGHT_PARENTHESIS generate_block_or_null (ELSE generate_block_or_null)?
	;

case_generate_construct
	: constant_expression (COMMA constant_expression)* COLON generate_block_or_null
	| DEFAULT COLON? generate_block_or_null
	;

generate_block
	: module_or_generate_item
	| BEGIN (COLON generate_block_identifier)? module_or_generate_item* END
	;

generate_block_or_null
	: generate_block
	| SEMICOLON
	;

// A.5 UDP declaration and instantiation
// A.5.1 UDP declaration
/*
udp_declaration
	: attribute_instance* PRIMITIVE udp_identifier LEFT_PARENTHESIS udp_port_list RIGHT_PARENTHESIS SEMICOLON udp_port_declaration udp_port_declaration* udp_body ENDPRIMITIVE
	| attribute_instance* PRIMITIVE udp_identifier LEFT_PARENTHESIS udp_declaration_port_list RIGHT_PARENTHESIS SEMICOLON udp_body ENDPRIMITIVE
	;
*/
// A.5.2 UDP ports
/*
udp_port_list
	: output_port_identifier COMMA input_port_identifier (COMMA input_port_identifier)*
	;

udp_declaration_port_list
	: udp_output_declaration COMMA udp_input_declaration (COMMA udp_input_declaration)*
	;

udp_port_declaration
	: udp_output_declaration SEMICOLON
	| udp_input_declaration SEMICOLON
	| udp_reg_declaration SEMICOLON
	;

udp_output_declaration
	: attribute_instance* OUTPUT port_identifier
	| attribute_instance* OUTPUT REG port_identifier (EQUAL constant_expression)?
	;

udp_input_declaration
	: attribute_instance* INPUT list_of_port_identifiers
	;

udp_reg_declaration
	: attribute_instance* REG variable_identifier
	;
*/
// A.5.3 UDP body
/*
udp_body
	: combinational_body
	| sequential_body
	;

combinational_body
	: TABLE combinational_entry combinational_entry* ENDTABLE
	;

combinational_entry
	: level_input_list COLON OUTPUT_SYMBOL SEMICOLON
	;

sequential_body
	: udp_initial_statement? TABLE sequential_entry sequential_entry* ENDTABLE
	;

udp_initial_statement
	: INITIAL output_port_identifier EQUAL INIT_VAL SEMICOLON
	;

sequential_entry
	: seq_input_list COLON current_state COLON next_state SEMICOLON
	;

seq_input_list
	: level_input_list
	| edge_input_list
	;

level_input_list
	: LEVEL_SYMBOL LEVEL_SYMBOL*
	;

edge_input_list
	: LEVEL_SYMBOL* edge_indicator LEVEL_SYMBOL*
	;

edge_indicator
	: LEFT_PARENTHESIS LEVEL_SYMBOL LEVEL_SYMBOL RIGHT_PARENTHESIS
	| EDGE_SYMBOL
	;

current_state
	: LEVEL_SYMBOL
	;

next_state
	: OUTPUT_SYMBOL
	| MINUS
	;
*/
// A.5.4 UDP instantiation
/*
udp_instantiation
	: udp_identifier drive_strength? delay2? udp_instance (COMMA udp_instance)* SEMICOLON
	;

udp_instance
	: name_of_udp_instance? LEFT_PARENTHESIS output_terminal COMMA input_terminal (COMMA input_terminal)* RIGHT_PARENTHESIS
	;

name_of_udp_instance
	: udp_instance_identifier range_?
	;
*/
// A.6 Behavioral statements
// A.6.1 Continuous assignment statements

continuous_assign
	: ASSIGN drive_strength? delay3? list_of_net_assignments SEMICOLON
	;

list_of_net_assignments
	: net_assignment (COMMA net_assignment)*
	;

net_assignment
	: net_lvalue EQUAL expression
	;

// A.6.2 Procedural blocks and assignments

initial_construct
	: INITIAL statement
	;

always_construct
	: ALWAYS statement
	;

blocking_assignment
	: variable_lvalue EQUAL delay_or_event_control? expression
	;

nonblocking_assignment
	: variable_lvalue LESS_THAN_EQUAL delay_or_event_control? expression
	;

procedural_continuous_assignments
	: ASSIGN variable_assignment
	| DEASSIGN variable_lvalue
	| FORCE variable_assignment
	| FORCE net_assignment
	| RELEASE variable_lvalue
	| RELEASE net_lvalue
	;

variable_assignment
	: variable_lvalue EQUAL expression
	;

// A.6.3 Parallel and sequential blocks

par_block
	: FORK (COLON block_identifier block_item_declaration*)? statement* JOIN
	;

seq_block
	: BEGIN (COLON block_identifier block_item_declaration*)? statement* END
	;

// A.6.4 Statements

statement
	: attribute_instance* blocking_assignment SEMICOLON
	| attribute_instance* case_statement
	| attribute_instance* conditional_statement
	| attribute_instance* disable_statement
	| attribute_instance* event_trigger
	| attribute_instance* loop_statement
	| attribute_instance* nonblocking_assignment SEMICOLON
	| attribute_instance* par_block
	| attribute_instance* procedural_continuous_assignments SEMICOLON
	| attribute_instance* procedural_timing_control_statement
	| attribute_instance* seq_block
	| attribute_instance* system_task_enable
	| attribute_instance* task_enable
	| attribute_instance* wait_statement
	| display_tasks
	| strobe_tasks
	| monitor_tasks
	| file_open_function
	| file_close_task
	| file_output_tasks
	| load_memory_tasks
	| finish_task
	| stop_task
	| dumpall_task
	| dumpfile_task
	| dumpflush_task
	| dumplimit_task
	| dumpoff_task
	| dumpon_task
	| dumpports_task
	| dumpportsall_task
	| dumpportsflush_task
	| dumpportslimit_task
	| dumpportsoff_task
	| dumpportson_task
	| dumpvars_task
	;

statement_or_null
	: statement
	| attribute_instance* SEMICOLON
	;

function_statement
	: statement
	;

// A.6.5 Timing control statements

delay_control
	: HASH delay_value
	| HASH LEFT_PARENTHESIS mintypmax_expression RIGHT_PARENTHESIS
	;

delay_or_event_control
	: delay_control
	| event_control
	| REPEAT LEFT_PARENTHESIS expression RIGHT_PARENTHESIS event_control
	;

disable_statement
	: DISABLE hierarchical_task_identifier SEMICOLON
	| DISABLE hierarchical_block_identifier SEMICOLON
	;

event_control
	: AT hierarchical_event_identifier
	| AT LEFT_PARENTHESIS event_expression RIGHT_PARENTHESIS
	| AT ASTERISK
	| AT LEFT_PARENTHESIS ASTERISK RIGHT_PARENTHESIS
	;

event_trigger
	: MINUS_GREATER_THAN hierarchical_event_identifier expression* SEMICOLON
	;

event_expression
	: expression
	| POSEDGE expression
	| NEGEDGE expression
	| event_expression OR event_expression
	| event_expression COMMA event_expression
	;

event_primary
	: expression
	| POSEDGE expression
	| NEGEDGE expression
	;

procedural_timing_control
	: delay_control
	| event_control
	;

procedural_timing_control_statement
	: procedural_timing_control statement_or_null
	;

wait_statement
	: WAIT LEFT_PARENTHESIS expression RIGHT_PARENTHESIS statement_or_null
	;

// A.6.6 Conditional statements

conditional_statement
	: IF LEFT_PARENTHESIS expression RIGHT_PARENTHESIS statement_or_null (ELSE IF LEFT_PARENTHESIS expression RIGHT_PARENTHESIS statement_or_null)* (ELSE statement_or_null)?
	;

// A.6.7 Case statements

case_statement
	: CASE LEFT_PARENTHESIS expression RIGHT_PARENTHESIS case_item case_item* ENDCASE
	| CASEZ LEFT_PARENTHESIS expression RIGHT_PARENTHESIS case_item case_item* ENDCASE
	| CASEX LEFT_PARENTHESIS expression RIGHT_PARENTHESIS case_item case_item* ENDCASE
	;

case_item
	: expression (COMMA expression)* COLON statement_or_null
	| DEFAULT COLON? statement_or_null
	;

// A.6.8 Looping statements

loop_statement
	: FOREVER statement
	| REPEAT LEFT_PARENTHESIS expression RIGHT_PARENTHESIS statement
	| WHILE LEFT_PARENTHESIS expression RIGHT_PARENTHESIS statement
	| FOR LEFT_PARENTHESIS variable_assignment SEMICOLON expression SEMICOLON variable_assignment RIGHT_PARENTHESIS statement
	;

// A.6.9 Task enable statements

system_task_enable
	: system_task_identifier (LEFT_PARENTHESIS expression? (COMMA expression?)* RIGHT_PARENTHESIS)? SEMICOLON
	;

task_enable
	: hierarchical_task_identifier (LEFT_PARENTHESIS expression (COMMA expression)* RIGHT_PARENTHESIS)? SEMICOLON
	;

// A.7 Specify section
// A.7.1 Specify block declaration

specify_block
	: SPECIFY specify_item* ENDSPECIFY
	;

specify_item
	: specparam_declaration
	| pulsestyle_declaration
	| showcancelled_declaration
	| path_declaration
	;

pulsestyle_declaration
	: PULSESTYLE_ONEVENT list_of_path_outputs SEMICOLON
	| PULSESTYLE_ONDETECT list_of_path_outputs SEMICOLON
	;

showcancelled_declaration
	: SHOWCANCELLED list_of_path_outputs SEMICOLON
	| NOSHOWCANCELLED list_of_path_outputs SEMICOLON
	;

// A.7.2 Specify path declarations

path_declaration
	: simple_path_declaration SEMICOLON
	| edge_sensitive_path_declaration SEMICOLON
	| state_dependent_path_declaration SEMICOLON
	;

simple_path_declaration
	: parallel_path_description EQUAL path_delay_value
	| full_path_description EQUAL path_delay_value
	;

parallel_path_description
	: LEFT_PARENTHESIS specify_input_terminal_descriptor polarity_operator? EQUAL_GREATER_THAN specify_output_terminal_descriptor RIGHT_PARENTHESIS
	;

full_path_description
	: LEFT_PARENTHESIS list_of_path_inputs polarity_operator? ASTERISK_GREATER_THAN list_of_path_outputs RIGHT_PARENTHESIS
	;

list_of_path_inputs
	: specify_input_terminal_descriptor (COMMA specify_input_terminal_descriptor)*
	;

list_of_path_outputs
	: specify_output_terminal_descriptor (COMMA specify_output_terminal_descriptor)*
	;

// A.7.3 Specify block terminals

specify_input_terminal_descriptor
	: input_identifier (LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	;

specify_output_terminal_descriptor
	: output_identifier (LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	;

input_identifier
	: input_port_identifier
	| inout_port_identifier
	;

output_identifier
	: output_port_identifier
	| inout_port_identifier
	;

// A.7.4 Specify path delays

path_delay_value
	: list_of_path_delay_expressions
	| LEFT_PARENTHESIS list_of_path_delay_expressions RIGHT_PARENTHESIS
	;

list_of_path_delay_expressions
	: t_path_delay_expression
	| trise_path_delay_expression COMMA tfall_path_delay_expression
	| trise_path_delay_expression COMMA tfall_path_delay_expression COMMA tz_path_delay_expression
	| t01_path_delay_expression COMMA t10_path_delay_expression COMMA t0z_path_delay_expression COMMA tz1_path_delay_expression COMMA t1z_path_delay_expression COMMA tz0_path_delay_expression
	| t01_path_delay_expression COMMA t10_path_delay_expression COMMA t0z_path_delay_expression COMMA tz1_path_delay_expression COMMA t1z_path_delay_expression COMMA tz0_path_delay_expression COMMA t0x_path_delay_expression COMMA tx1_path_delay_expression COMMA t1x_path_delay_expression COMMA tx0_path_delay_expression COMMA txz_path_delay_expression COMMA tzx_path_delay_expression
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
	: parallel_edge_sensitive_path_description EQUAL path_delay_value
	| full_edge_sensitive_path_description EQUAL path_delay_value
	;

parallel_edge_sensitive_path_description
	: LEFT_PARENTHESIS edge_identifier? specify_input_terminal_descriptor EQUAL_GREATER_THAN LEFT_PARENTHESIS specify_output_terminal_descriptor polarity_operator? COLON data_source_expression RIGHT_PARENTHESIS RIGHT_PARENTHESIS
	;

full_edge_sensitive_path_description
	: LEFT_PARENTHESIS edge_identifier? list_of_path_inputs ASTERISK_GREATER_THAN LEFT_PARENTHESIS list_of_path_outputs polarity_operator? COLON data_source_expression RIGHT_PARENTHESIS RIGHT_PARENTHESIS
	;

data_source_expression
	: expression
	;

edge_identifier
	: POSEDGE
	| NEGEDGE
	;

state_dependent_path_declaration
	: IF LEFT_PARENTHESIS module_path_expression RIGHT_PARENTHESIS simple_path_declaration
	| IF LEFT_PARENTHESIS module_path_expression RIGHT_PARENTHESIS edge_sensitive_path_declaration
	| IFNONE simple_path_declaration
	;

polarity_operator
	: PLUS
	| MINUS
	;

// A.7.5 System timing checks
// A.7.5.1 System timing check commands
/*
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
	: DOLLAR_SETUP LEFT_PARENTHESIS data_event COMMA reference_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

hold_timing_check
	: DOLLAR_HOLD LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

setuphold_timing_check
	: DOLLAR_SETUPHOLD LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit COMMA timing_check_limit (COMMA notifier? (COMMA timestamp_condition? (COMMA timecheck_condition? (COMMA delayed_reference? (COMMA delayed_data?)?)?)?)?)? RIGHT_PARENTHESIS SEMICOLON
	;

recovery_timing_check
	: DOLLAR_RECOVERY LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

removal_timing_check
	: DOLLAR_REMOVAL LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

recrem_timing_check
	: DOLLAR_RECREM LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit COMMA timing_check_limit (COMMA notifier? (COMMA timestamp_condition? (COMMA timecheck_condition? (COMMA delayed_reference? (COMMA delayed_data?)?)?)?)?)? RIGHT_PARENTHESIS SEMICOLON
	;

skew_timing_check
	: DOLLAR_SKEW LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

timeskew_timing_check
	: DOLLAR_TIMESKEW LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit (COMMA notifier? (COMMA event_based_flag? (COMMA remain_active_flag?)?)?)? RIGHT_PARENTHESIS SEMICOLON
	;

fullskew_timing_check
	: DOLLAR_FULLSKEW LEFT_PARENTHESIS reference_event COMMA data_event COMMA timing_check_limit COMMA timing_check_limit (COMMA notifier? (COMMA event_based_flag? (COMMA remain_active_flag?)?)?)? RIGHT_PARENTHESIS SEMICOLON
	;

period_timing_check
	: DOLLAR_PERIOD LEFT_PARENTHESIS controlled_reference_event COMMA timing_check_limit (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

width_timing_check
	: DOLLAR_WIDTH LEFT_PARENTHESIS controlled_reference_event COMMA timing_check_limit COMMA threshold (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;

nochange_timing_check
	: DOLLAR_NOCHANGE LEFT_PARENTHESIS reference_event COMMA data_event COMMA start_edge_offset COMMA end_edge_offset (COMMA notifier?)? RIGHT_PARENTHESIS SEMICOLON
	;
*/
// A.7.5.2 System timing check command arguments
/*
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
	: terminal_identifier (LEFT_BRACKET constant_mintypmax_expression RIGHT_BRACKET)?
	;

delayed_reference
	: terminal_identifier (LEFT_BRACKET constant_mintypmax_expression RIGHT_BRACKET)?
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
*/
// A.7.5.3 System timing check event definitions
/*
timing_check_event
	: timing_check_event_control? specify_terminal_descriptor (TRIPLE_AMPERSAND timing_check_condition)?
	;

controlled_timing_check_event
	: timing_check_event_control specify_terminal_descriptor (TRIPLE_AMPERSAND timing_check_condition)?
	;

timing_check_event_control
	: POSEDGE
	| NEGEDGE
	| EDGE
	| edge_control_specifier
	;

specify_terminal_descriptor
	: specify_input_terminal_descriptor
	| specify_output_terminal_descriptor
	;

edge_control_specifier
	: EDGE LEFT_BRACKET EDGE_DESCRIPTOR (COMMA EDGE_DESCRIPTOR)* RIGHT_BRACKET
	;

timing_check_condition
	: scalar_timing_check_condition
	| LEFT_PARENTHESIS scalar_timing_check_condition RIGHT_PARENTHESIS
	;

scalar_timing_check_condition
	: TILDE? expression
	| expression (DOUBLE_EQUAL | TRIPLE_EQUAL | EXCLAMATION_MARK_EQUAL | EXCLAMATION_MARK_DOUBLE_EQUAL) SCALAR_CONSTANT
	;
*/
// A.8 Expressions
// A.8.1 Concatenations

concatenation
	: LEFT_BRACE expression (COMMA expression)* RIGHT_BRACE
	;

constant_concatenation
	: LEFT_BRACE constant_expression (COMMA constant_expression)* RIGHT_BRACE
	;

constant_multiple_concatenation
	: LEFT_BRACE constant_expression constant_concatenation RIGHT_BRACE
	;

module_path_concatenation
	: LEFT_BRACE module_path_expression (COMMA module_path_expression)* RIGHT_BRACE
	;

module_path_multiple_concatenation
	: LEFT_BRACE constant_expression module_path_concatenation RIGHT_BRACE
	;

multiple_concatenation
	: LEFT_BRACE constant_expression concatenation RIGHT_BRACE
	;

// A.8.2 Function calls

constant_function_call
	: function_identifier attribute_instance* LEFT_PARENTHESIS constant_expression (COMMA constant_expression)* RIGHT_PARENTHESIS
	;

constant_system_function_call
	: system_function_identifier LEFT_PARENTHESIS constant_expression (COMMA constant_expression)* RIGHT_PARENTHESIS
	;

function_call
	: hierarchical_function_identifier attribute_instance* LEFT_PARENTHESIS expression (COMMA expression)* RIGHT_PARENTHESIS
	;

system_function_call
	: system_function_identifier (LEFT_PARENTHESIS expression (COMMA expression)* RIGHT_PARENTHESIS)?
	;

// A.8.3 Expressions

base_expression
	: expression
	;
/*
conditional_expression
	: expression QUESTION_MARK attribute_instance* expression COLON expression
	;
*/
constant_base_expression
	: constant_expression
	;

constant_expression
	: constant_primary
	| unary_operator attribute_instance* constant_primary
	| constant_expression binary_operator attribute_instance* constant_expression
	| constant_expression QUESTION_MARK attribute_instance* constant_expression COLON constant_expression
	;

constant_mintypmax_expression
	: constant_expression
	| constant_expression COLON constant_expression COLON constant_expression
	;

constant_range_expression
	: constant_expression
	| msb_constant_expression COLON lsb_constant_expression
	| constant_base_expression PLUS_COLON width_constant_expression
	| constant_base_expression MINUS_COLON width_constant_expression
	;

dimension_constant_expression
	: constant_expression
	;

expression
	: primary
	| unary_operator attribute_instance* primary
	| expression binary_operator attribute_instance* expression
	| expression QUESTION_MARK attribute_instance* expression COLON expression // = conditional_expression
	;

lsb_constant_expression
	: constant_expression
	;

mintypmax_expression
	: expression
	| expression COLON expression COLON expression
	;
/*
module_path_conditional_expression
	: module_path_expression QUESTION_MARK attribute_instance* module_path_expression COLON module_path_expression
	;
*/
module_path_expression
	: module_path_primary
	| unary_module_path_operator attribute_instance* module_path_primary
	| module_path_expression binary_module_path_operator attribute_instance* module_path_expression
	| module_path_expression QUESTION_MARK attribute_instance* module_path_expression COLON module_path_expression // = module_path_conditional_expression
	;

module_path_mintypmax_expression
	: module_path_expression
	| module_path_expression COLON module_path_expression COLON module_path_expression
	;

msb_constant_expression
	: constant_expression
	;

range_expression
	: expression
	| msb_constant_expression COLON lsb_constant_expression
	| base_expression PLUS_COLON width_constant_expression
	| base_expression MINUS_COLON width_constant_expression
	;

width_constant_expression
	: constant_expression
	;

// A.8.4 Primaries

constant_primary
	: number
	| parameter_identifier (LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	| specparam_identifier (LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	| constant_concatenation
	| constant_multiple_concatenation
	| constant_function_call
	| constant_system_function_call
	| LEFT_PARENTHESIS constant_mintypmax_expression RIGHT_PARENTHESIS
	| STRING
	| conversion_functions
	| random_function
	| dist_functions
	| math_functions
	;

module_path_primary
	: number
	| identifier
	| module_path_concatenation
	| module_path_multiple_concatenation
	| function_call
	| system_function_call
	| LEFT_PARENTHESIS module_path_mintypmax_expression RIGHT_PARENTHESIS
	;

primary
	: number
	| hierarchical_identifier ((LEFT_BRACKET expression RIGHT_BRACKET)* LEFT_BRACKET range_expression RIGHT_BRACKET)?
	| concatenation
	| multiple_concatenation
	| function_call
	| system_function_call
	| LEFT_PARENTHESIS mintypmax_expression RIGHT_PARENTHESIS
	| STRING
	| conversion_functions
	| random_function
	| dist_functions
	| math_functions
	;

// A.8.5 Expression left-side values

net_lvalue
	: hierarchical_net_identifier ((LEFT_BRACKET constant_expression RIGHT_BRACKET)* LEFT_BRACKET constant_range_expression RIGHT_BRACKET)?
	| LEFT_BRACE net_lvalue (COMMA net_lvalue)* RIGHT_BRACE
	;

variable_lvalue
	: hierarchical_variable_identifier ((LEFT_BRACKET expression RIGHT_BRACKET)* LEFT_BRACKET range_expression RIGHT_BRACKET)?
	| LEFT_BRACE variable_lvalue (COMMA variable_lvalue)* RIGHT_BRACE
	;

// A.8.6 Operators

unary_operator
	: PLUS
	| MINUS
	| EXCLAMATION_MARK
	| TILDE
	| AMPERSAND
	| TILDE_AMPERSAND
	| VERTICAL_BAR
	| TILDE_VERTICAL_BAR
	| CARET
	| TILDE_CARET
	| CARET_TILDE
	;

binary_operator
	: PLUS
	| MINUS
	| ASTERISK
	| SLASH
	| PERCENT
	| DOUBLE_EQUAL
	| EXCLAMATION_MARK_EQUAL
	| TRIPLE_EQUAL
	| EXCLAMATION_MARK_DOUBLE_EQUAL
	| DOUBLE_AMPERSAND
	| DOUBLE_VERTICAL_BAR
	| DOUBLE_ASTERISK
	| LESS_THAN
	| LESS_THAN_EQUAL
	| GREATER_THAN
	| GREATER_THAN_EQUAL
	| AMPERSAND
	| VERTICAL_BAR
	| CARET
	| CARET_TILDE
	| TILDE_CARET
	| DOUBLE_GREATER_THAN
	| DOUBLE_LESS_THAN
	| TRIPLE_GREATER_THAN
	| TRIPLE_LESS_THAN
	;

unary_module_path_operator
	: EXCLAMATION_MARK
	| TILDE
	| AMPERSAND
	| TILDE_AMPERSAND
	| VERTICAL_BAR
	| TILDE_VERTICAL_BAR
	| CARET
	| TILDE_CARET
	| CARET_TILDE
	;

binary_module_path_operator
	: DOUBLE_EQUAL
	| EXCLAMATION_MARK_EQUAL
	| DOUBLE_AMPERSAND
	| DOUBLE_VERTICAL_BAR
	| AMPERSAND
	| VERTICAL_BAR
	| CARET
	| TILDE_CARET
	| CARET_TILDE
	;

// A.8.7 Numbers

number
	: DECIMAL_NUMBER
	| OCTAL_NUMBER
	| BINARY_NUMBER
	| HEX_NUMBER
	| REAL_NUMBER
	;

// A.9 General
// A.9.1 Attributes

attribute_instance
	: LEFT_PARENTHESIS ASTERISK attr_spec (COMMA attr_spec)* ASTERISK RIGHT_PARENTHESIS
	;

attr_spec
	: attr_name (EQUAL constant_expression)?
	;

attr_name
	: identifier
	;

// A.9.3 Identifiers

block_identifier
	: identifier
	;

cell_identifier
	: identifier
	;

config_identifier
	: identifier
	;

event_identifier
	: identifier
	;

function_identifier
	: identifier
	;

gate_instance_identifier
	: identifier
	;

generate_block_identifier
	: identifier
	;

genvar_identifier
	: identifier
	;

hierarchical_block_identifier
	: hierarchical_identifier
	;

hierarchical_event_identifier
	: hierarchical_identifier
	;

hierarchical_function_identifier
	: hierarchical_identifier
	;

hierarchical_identifier
	: (identifier (LEFT_BRACKET constant_expression RIGHT_BRACKET)? DOT)* identifier
	;

hierarchical_net_identifier
	: hierarchical_identifier
	;

hierarchical_parameter_identifier
	: hierarchical_identifier
	;

hierarchical_variable_identifier
	: hierarchical_identifier
	;

hierarchical_task_identifier
	: hierarchical_identifier
	;

identifier
	: SIMPLE_IDENTIFIER
	| ESCAPED_IDENTIFIER
	;

inout_port_identifier
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

module_identifier
	: identifier
	;

module_instance_identifier
	: identifier
	;

net_identifier
	: identifier
	;

output_port_identifier
	: identifier
	;

parameter_identifier
	: identifier
	;

port_identifier
	: identifier
	;

real_identifier
	: identifier
	;

specparam_identifier
	: identifier
	;

system_function_identifier
	: SYSTEM_TF_IDENTIFIER
	;

system_task_identifier
	: SYSTEM_TF_IDENTIFIER
	;

task_identifier
	: identifier
	;

terminal_identifier
	: identifier
	;

topmodule_identifier
	: identifier
	;

udp_identifier
	: identifier
	;

udp_instance_identifier
	: identifier
	;

variable_identifier
	: identifier
	;
