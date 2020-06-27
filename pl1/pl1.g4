/*
BSD License

Copyright (c) 2020, Tom Everett
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
grammar pl1;

program
   : statements
   ;

statements
   : ('(' (condition_prefix ',')+ ')' ':')* (label_prefix ':')* statement_body? ';'
   ;

percent_statements
   : '%' (label_prefix ':')* statement_body ';'
   ;

attribute
   : bindec_attribute
   | task_attribute
   | condition_attribute
   | print_attribute
   | keyed_attribute
   | exclusive_attribute
   | environment_attribute
   | backwards_attribute
   | buffered_attribute
   | direct_attribute
   | input_attribute
   | record_attribute
   | file_attribute
   | generic_attribute
   | builtin_attribute
   | reducible_attribute
   | returns_attribute
   | options_attribute
   | entry_attribute
   | initial_attribute
   | connected_attribute
   | position_attribute
   | defined_attribute
   | area_attribute
   | area_attribute
   | pointer_attribute
   | based_attribute
   | controlled_attribute
   | automatic_attribute
   | static_attribute
   | internal_attribute
   | like_attribute
   | parenth_attribute
   | aligned_attribute
   | variable_attribute
   | label_attribute
   | event_attribute
   | fixed_attribute
   | digits_attribute
   | real_attribute
   | bit_attribute
   | character_attribute
   | graphic_attribute
   | varying_attribute
   | picture_attribute
   | offset_attribute
   ;

bindec_attribute
   : ('BINARY' | 'DECIMAL')
   ;

task_attribute
   : 'TASK'
   ;

condition_attribute
   : 'CONDITION'
   ;

print_attribute
   : 'PRINT'
   ;

keyed_attribute
   : 'KEYED'
   ;

exclusive_attribute
   : 'EXCLUSIVE'
   ;

environment_attribute
   : 'ENVIRONMENT' '(' characteristic_list ')'
   ;

backwards_attribute
   : 'BACKWARDS'
   ;

buffered_attribute
   : ('BUFFERED' | 'UNBUFFERED')
   ;

direct_attribute
   : ('DIRECT' | 'SEQUENTIAL' | 'TRANSIENT')
   ;

input_attribute
   : ('INPUT' | 'OUTPUT' | 'UPDATE')
   ;

record_attribute
   : ('RECORD' | 'STREAM')
   ;

file_attribute
   : 'FILE'
   ;

generic_attribute
   : generic_name 'GENERIC' '(' (entry_reference 'WHEN' '(' (generic_descriptor ',')* ')' ',')+ ')'
   ;

builtin_attribute
   : 'BUILTIN'
   ;

reducible_attribute
   : ('IRREDUCIBLE' | 'REDUCIBLE')?
   ;

returns_attribute
   : 'RETURNS' '(' (attribute)+ ')'
   ;

options_attribute
   : 'OPTIONS' '(' attribute_characteristic_list ')'
   ;

entry_attribute
   : 'ENTRY' ('(' attribute_parameter_descriptor_list? ')')?
   ;

initial_attribute
   : 'INITIAL' '(' (item ',')+ ')'
   ;

connected_attribute
   : 'CONNECTED'
   ;

position_attribute
   : 'POSITION' '(' expression ')'
   ;

defined_attribute
   : 'DEFINED' (reference | '(' reference ')')
   ;

offset_attribute
   : 'OFFSET' ('(' area_variable ')')?
   ;

area_attribute
   : 'AREA' (('(' '*' ')' | '(' expression ('REFER' '(' variable ')')? ')'))?
   ;

pointer_attribute
   : 'POINTER'
   ;

based_attribute
   : 'BASED' ('(' locator_reference ')')?
   ;

controlled_attribute
   : 'CONTROLLED'
   ;

automatic_attribute
   : 'AUTOMATIC'
   ;

static_attribute
   : 'STATIC'
   ;

internal_attribute
   : ('INTERNAL' | 'EXTERNAL')
   ;

like_attribute
   : 'LIKE' structure_variable
   ;

parenth_attribute
   : '(' (attribute_bound ',')+ ')'
   ;

aligned_attribute
   : ('ALIGNED' | 'UNALIGNED')
   ;

variable_attribute
   : 'VARIABLE'
   ;

label_attribute
   : 'LABEL' ('(' (label_constant ',')+ ')')?
   ;

event_attribute
   : 'EVENT'
   ;

fixed_attribute
   : ('FIXED' | 'FLOAT')
   ;

digits_attribute
   : '(' number_of_digits (',' scaling_factor)? ')'
   ;

real_attribute
   : ('REAL' | 'COMPLEX')
   ;

bit_attribute
   : 'BIT' ('(' length ('REFER' '(' variable ')')? ')')?
   ;

character_attribute
   : 'CHARACTER' ('(' length ('REFER' '(' variable ')')? ')')?
   ;

graphic_attribute
   : 'GRAPHIC' ('(' length ('REFER' '(' variable ')')? ')')?
   ;

varying_attribute
   : 'VARYING'
   ;

picture_attribute
   : 'PICTURE' '\'' picture_specification '\''
   ;

constant
   : character_constant
   | hex_constant
   | binary_constant
   | hex2_constant
   | graphic_constant
   | hex3_constant
   | graphic2_constant
   ;

character_constant
   : '\'' ((character | '"'))* '\''
   ;

hex_constant
   : '\'' (hex_digit hex_digit)* '\'X'
   ;

binary_constant
   : '\'' (('0' | '1'))* '\'B'
   ;

hex2_constant
   : '\'' (hex_digit)* ('\'BX' | '\'B4')
   ;

graphic_constant
   : '\'' (graphic_symbol)* '\'G'
   ;

hex3_constant
   : '\'' (hex_digit hex_digit hex_digit hex_digit)* '\'GX'
   ;

graphic2_constant
   : '\'' ((character | '\'' '\'' | '<' graphic_symbol+ '>'))* '\'M'
   ;

attribute_bound
   : ((attribute_lower_bound ':')? attribute_upper_bound | '*')
   ;

attribute_upper_bound
   : expression ('REFER' '(' variable ')')?
   ;

attribute_lower_bound
   : expression ('REFER' '(' variable ')')?
   ;

expression
   : expression_unary_expression (infix_operator expression_unary_expression)*
   ;

expression_unary_expression
   : (prefix_operator)* expression_elementary_expression
   ;

expression_elementary_expression
   : ('(' expression ')' | expression_reference | constant | isub6)
   ;

expression_reference
   : expression_locator_qualifier? expression_basic_reference ('(' expression_subscript_list ')')? ('(' expression_argument_list? ')')?
   ;

expression_locator_qualifier
   : expression_reference '->'
   ;

expression_subscript_list
   : ((expression | '*') ',')+
   ;

expression_argument_list
   : (expression ',')+
   ;

expression_basic_reference
   : expression_structure_qualification? name
   ;

expression_structure_qualification
   : expression_basic_reference ('(' expression_subscript_list ')')?
   ;

statement
   : fetch_statement
   | fetch_statement
   | note_statement
   | if_statement
   | goto_statement
   | end_statement
   | do1_statement
   | do2_statement
   | declare_statement
   | deactivate_statement
   | assign_statement
   | activate_statement
   | return_statement
   | procedure_statement
   | exit_statement
   | skip_statement
   | print_statement
   | page_statement
   | noprint_statement
   | include_statement
   | signal_statement
   | revert_statement
   | on_statement
   | format_statement
   | put_statement
   | putstring_statement
   | put_statement
   | getstring_statement
   | get_statement
   | unlock_statement
   | delete_statement
   | locate_statement
   | rewritefile_statement
   | writefile_statement
   | readfile_statement
   | close_statement
   | open_statement
   | return2_statement
   | call_statement
   | cobol_statement
   | main_statement
   | entry_statement
   | procedure2_statement
   | main_statement
   | wait_statement
   | stop_statement
   | select_statement
   | wait_statement
   | leave_statement
   | if2_statement
   | goto2_statement
   | end2_statement
   | do3_statement
   | do4_statement
   | do5_statement
   | display1_statement
   | delay_statement
   | display1_statement
   | display2_statement
   | free_statement
   | free2_statement
   | allocate_statement
   | allocate2_statement
   | release_statement
   | begin_statement
   | declare_statement
   | declare2_statement
   | default_statement
   | null_statement
   | percent_statement
   ;

fetch_statement
   : 'FETCH' (entry_constant ',')+ ';'
   ;

percent_statement
   : '%' ';'
   ;

note_statement
   : '%' (label ':')* 'NOTE' '(' message (',' code)? ')' ';'
   ;

if_statement
   : '%' (label ':')* 'IF' preprocessor_expression '%' 'THEN' preprocessor_unit1 ('%' 'ELSE' preprocessor_unit2)?
   ;

goto_statement
   : '%' (label ':')* 'GO_TO' label ';'
   ;

end_statement
   : '%' (label ':')* 'END' label? ';'
   ;

do1_statement
   : '%' (label ':')* 'DO' preprocessor_do_specification ';'
   ;

do2_statement
   : '%' (label ':')* 'DO' ';'
   ;

declare_statement
   : '%' (label ':')* 'DECLARE' (identifier ('FIXED' | 'CHARACTER' | 'ENTRY' | 'BUILTIN' ','))+ ';'
   ;

deactivate_statement
   : '%' (label ':')* 'DEACTIVATE' (identifier ',')+ ';'
   ;

assign_statement
   : '%' (label ':')* preprocessor_variable '=' preprocessor_expression ';'
   ;

activate_statement
   : '%' (label ':')* 'ACTIVATE' (identifier (('RESCAN' | 'NORESCAN'))? ',')+ ';'
   ;

return_statement
   : (label ':')* 'RETURN' '(' preprocessor_expression ')' ';'
   ;

procedure_statement
   : '%' (entry_name ':')+ 'PROCEDURE' ('(' (parameter ',')+ ')')? 'STATEMENT'? 'RETURNS' '(' ('CHARACTER' | 'FIXED') ')' ';'
   ;

exit_statement
   : 'EXIT' ';'
   ;

skip_statement
   : '%' 'SKIP' ('(' expression ')')? ';'
   ;

print_statement
   : '%' 'PRINT' ';'
   ;

page_statement
   : '%' 'PAGE' ';'
   ;

noprint_statement
   : '%' 'NOPRINT' ';'
   ;

include_statement
   : '%' 'INCLUDE' ((dataset '(' member_name ')' | member_name) ',')+ ';'
   ;

signal_statement
   : 'SIGNAL' condition ';'
   ;

revert_statement
   : 'REVERT' condition ';'
   ;

on_statement
   : 'ON' condition 'SNAP'? ('SYSTEM' ';' | on_unit)
   ;

format_statement
   : (label ':')+ 'FORMAT' '(' format_list ')' ';'
   ;

putstring_statement
   : 'PUT-STRING' '(' character_reference ')' data_specification ';'
   ;

put_statement
   : 'PUT' ('FILE' '(' file_reference ')')? data_specification? (('PAGE' ('LINE' '(' expression ')')? | 'SKIP' ('(' expression ')')? | 'LINE' '(' expression ')'))? ';'
   ;

getstring_statement
   : 'GET-STRING' '(' expression ')' data_specification ';'
   ;

get_statement
   : 'GET' ('FILE' '(' file_reference ')')? data_specification? ('COPY' '(' file_reference ')')? ('SKIP' ('(' expression ')')?)? ';'
   ;

unlock_statement
   : 'UNLOCK' 'FILE' '(' file_reference ')' 'KEY' '(' expression ')' ';'
   ;

delete_statement
   : 'DELETE' 'FILE' '(' file_reference ')' ('KEY' '(' expression ')')? ('EVENT' '(' event_reference ')')? ';'
   ;

locate_statement
   : 'LOCATE' based_variable ('FILE' '(' file_reference ')' | 'SET' '(' pointer_reference | 'KEYFROM' '(' expression ')') ';'
   ;

rewritefile_statement
   : 'REWRITE_FILE' '(' file_reference ')' ('FROM' '(' reference ')')? ('KEY' '(' expression ')')? ('EVENT' '(' event_reference)? ';'
   ;

writefile_statement
   : 'WRITE' 'FILE' '(' file_reference ')' 'FROM' '(' reference ')' (('KEYFROM' '(' expression ')' | 'KEYTO' '(' reference ')'))? ('EVENT' '(' event_reference)? ';'
   ;

readfile_statement
   : 'READ' 'FILE' '(' file_reference ')' ('INTO' '(' ref ')' (('KEY' '(' expression ')' 'NOLOCK'? | 'KEYTO' '(' reference ')'))? | 'SET' '(' pointer_ref ')' (('KEY' '(' expression ')' | 'KEYTO' '(' reference ')'))? | 'IGNORE' '(' expression ')') ('EVENT' '(' reference ')')? ';'
   ;

close_statement
   : 'CLOSE' ('FILE' '(' file_reference ')' ('ENVIRONMENT' '(' ('LEAVE' | 'REREAD') ')')? ',')+ ';'
   ;

open_statement
   : 'OPEN' (options_group ',')+ ';'
   ;

return2_statement
   : 'RETURN' ('(' expression ')')? ';'
   ;

call_statement
   : 'CALL' (entry_reference | generic_name | built_in_name) ('(' (argument ',')* ')')? ('TASK' ('(' task_reference ')')?)? ('EVENT' '(' event_reference ')')? ('PRIORITY' '(' expression ')')? ';'
   ;

cobol_statement
   : ('COBOL' | 'FORTRAN') ('NOMAP' ('(' parameter_list ')')?)? ('NOMAPIN' ('(' parameter_list ')')?)? ('NOMAPOUT' ('(' parameter_list ')')?)?
   ;

main_statement
   : ('MAIN' 'NOEXECOPS'? 'REENTRANT'? 'TASK'? | 'COBOL' statement_additional_options | 'FORTRAN' statement_additional_options)
   ;

entry_statement
   : (entry_constant ':')+ 'ENTRY' ('(' (parameter ',')+ ')')? ('RETURNS' '(' attribute+ ')')? ('OPTIONS' '(' characteristic_list ')')? (('REDUCIBLE' | 'IRREDUCIBLE'))? ';'
   ;

procedure2_statement
   : (entry_constant ':')+ 'PROCEDURE' ('(' (parameter ',')+ ')')? ('RETURNS' '(' attribute+ ')')? ('OPTIONS' '(' characteristic_list ')')? (('REDUCIBLE' | 'IRREDUCIBLE'))? 'RECURSIVE'? (('ORDER' | 'REORDER'))? (('CHARGRAPHIC' | 'NOCHARGRAPHIC'))? ';'
   ;

wait_statement
   : 'WAIT' '(' (event_reference ',')+ ')' ('(' expression ')')? ';'
   ;

stop_statement
   : 'STOP' ';'
   ;

select_statement
   : 'SELECT' ('(' exp1 ')')? ';' ('WHEN' '(' (exp2 ',')+ ')' unit)* ('OTHERWISE' unit)?
   ;

null_statement
   : ';'
   ;

leave_statement
   : 'LEAVE' label_constant? ';'
   ;

if2_statement
   : 'IF' expression 'THEN' unit1 ('ELSE' unit2)?
   ;

goto2_statement
   : ('GO' 'TO' | 'GOTO') label_reference ';'
   ;

end2_statement
   : 'END' statement_label? ';'
   ;

do3_statement
   : 'DO' reference '=' (specification ',')+ ';'
   ;

do4_statement
   : 'DO' ('WHILE' '(' exp4 ')' ('UNTIL' '(' exp5 ')')? | 'UNTIL' '(' exp5 ')' ('WHILE' '(' exp4 ')')?) ';'
   ;

do5_statement
   : 'DO' ';'
   ;

display1_statement
   : 'DISPLAY' '(' expression ')' ('REPLY' '(' character_reference ')' ('EVENT' '(' event_reference ')')? | 'EVENT' '(' event_reference ')' reply '(' character_reference ')') ';'
   ;

display2_statement
   : 'DISPLAY' '(' expression ')' ';'
   ;

delay_statement
   : 'DELAY' '(' expression ')' ';'
   ;

assign2_statement
   : (reference ',')+ '=' expression (',' 'BY' 'NAME')? ';'
   ;

free_statement
   : 'FREE' (option ',')+ ';'
   ;

allocate_statement
   : 'ALLOCATE' (based_variable reference? ',')+ ';'
   ;

free2_statement
   : 'FREE' (controlled_variable ',')+ ';'
   ;

allocate2_statement
   : 'ALLOCATE' (level? controlled_variable attribute? ',')+
   ;

release_statement
   : 'RELEASE' entry_constant+ ';'
   ;

begin_statement
   : 'BEGIN' ('ORDER' | 'REORDER')? ('CHARGRAPHIC' | 'NOCHARGRAPHIC')? ';'
   ;

declare2_statement
   : 'DECLARE' (level? name attribute* 'SYSTEM'? ',')+ ';'
   ;

default_statement
   : 'DEFAULT' ((statement_simple_specification | statement_factored_specification) ',')+ ';'
   ;

statement_simple_specification
   : ('RANGE' '(' ((letter+ | letter ':' letter))+ ')' statement_attribute_specification? | 'RANGE' '(' '*' ')' statement_attribute_specification? | 'DESCRIPTORS' statement_attribute_specification?)
   ;

statement_factored_specification
   : '(' ((statement_simple_specification | statement_factored_specification) ',')+ ')' statement_attribute_specification?
   ;

statement_attribute_specification
   : (attribute+ ('VALUE' '(' (statement_value_specification ',')+ ')')? | 'VALUE' '(' (statement_value_specification ',')+ ')')
   ;

statement_value_specification
   : ('AREA' '(' size ')' | 'BIT' '(' length ')' | 'CHARACTER' '(' length ')' | 'GRAPHIC' '(' length ')' | statement_additional_options)
   ;

statement_additional_options
   : (('REAL' | 'COMPLEX'))? (('FIXED' | 'FLOAT'))? (('BINARY' | 'DECIMAL'))? ('(' precision (',' scaling_factor)? ')')?
   ;

statement_additional_options
   : ('NOMAP' ('(' parameter_list ')')?)? ('NOMAPIN' ('(' parameter_list ')')?)? ('NOMAPOUT' ('(' parameter_list ')')?)?
   ;

statement_attribute
   : (dimension | 'CHARACTER' '(' length | 'BIT' '(' length ')' | 'GRAPHIC' '(' length ')' | 'AREA' '(' size ')' | 'INITIAL' '(' ('ITEM' ',')+ ')' | initial_call_entry_reference ('(' (argument ',')+ ')')?)
   ;

locator_qualification
   : locator_reference '->' (based_locator_reference '->')* based_reference
   ;

statement_reference
   : ('IN' '(' area_reference ')')? ('SET' '(' locator_reference ')')?
   ;

statement_option
   : (locator_qualifier '>')? based_variable ('IN' '(' area_reference ')')?
   ;

option
   : refer_option
   | priority_option
   | event_option
   | task_option
   ;

refer_option
   : expression 'REFER' '(' variable ')'
   ;

priority_option
   : 'PRIORITY' '(' expression ')'
   ;

event_option
   : 'EVENT' '(' event_reference ')'
   ;

task_option
   : 'TASK' ('(' task_reference ')')?
   ;

attribute_item
   : ('*' | initial_constant | reference | '(' expression ')' | attribute_iteration_specification)
   ;

attribute_iteration_specification
   : '(' iteration_factor ')' attribute_iteration_item
   ;

attribute_iteration_item
   : ('*' | initial_constant | reference | '(' (item ',')+ ')')
   ;

attribute_initial_constant1
   : ((('+' | '-'))? arithmetic_constant | bit_constant | character_constant | graphic_constant | entry_constant | file_constant | label_constant | ('+' | '-')? real_constant ('+' | '-') imaginary_constant)
   ;

attribute_initial_constant2
   : 'INITIAL' 'CALL' entry_reference ('(' (argument ',')+ ')')?
   ;

statement_specificationstatement_specification
   : exp1 (('TO' exp2 ('BY' exp3)? | 'BY' exp3 ('TO' exp2)? | 'REPEAT' exp6))? (('WHILE' '(' exp4 ')' ('(' 'UNTIL' '(' exp5 ')')? | 'UNTIL' '(' exp5 ')' ('(' 'WHILE' '(' exp4 ')')?))?
   ;

entry_data1
   : 'ENTRY' ('(' attribute_parameter_descriptor_list ')')? ('RETURNS' '(' attribute_list ')')? ('OPTIONS' '(' attribute_characteristic_list ')')? 'EXTERNAL'? (('IRREDUCIBLE' | 'REDUCIBLE'))?
   ;

entry_data2
   : 'ENTRY' ('(' attribute_parameter_descriptor_list ')')? 'VARIABLE'? ('RETURNS' '(' attribute_list ')')? ('OPTIONS' '(' attribute_characteristic_list ')')? (('IRREDUCIBLE' | 'REDUCIBLE'))? scope_attribute? storage_attribute? alignment_attribute?? parameter? 'DEFINED' 'INITIAL'?
   ;

attribute_parameter_descriptor_list
   : (attribute_parameter_descriptor ',')+
   ;

attribute_parameter_descriptor
   : (((attribute)+ | '*' | attribute_structure_parameter_descriptor))?
   ;

attribute_structure_parameter_descriptor
   : '1' attribute* ',' (level attribute* ',')+
   ;

attribute_characteristic_list
   : ('COBOL' | 'FORTRAN' | 'ASSEMBLER') attribute_additional_options?
   ;

attribute_additional_options
   : ('NOMAP' ('(' (argi ',')+ ')')?)? ('NOMAPIN' ('(' (argi)+ ')')?)? ('NOMAPOUT' ('(' (argi ',')+ ')')?)? 'INTER'? 'RETCODE'?
   ;

built_in
   : built_in_function
   | parmset_built_in_function
   | counter_built_in_function
   | compiletime_built_in_function
   | verify_built_in_function
   | unspec_built_in_function
   | trunc_built_in_function
   | translate_built_in_function
   | time_built_in_function
   | tanh_built_in_function
   | tand_built_in_function
   | tan_built_in_function
   | sysnull_built_in_function
   | sum_built_in_function
   | substr_built_in_function
   | string_built_in_function
   | storage_built_in_function
   | status_built_in_function
   | sqrt_built_in_function
   | sinh_built_in_function
   | sind_built_in_function
   | sin_built_in_function
   | sin_built_in_function
   | samekey_built_in_function
   | round_built_in_function
   | repeat_built_in_function
   | real_built_in_function
   | prod_built_in_function
   | priority_built_in_function
   | precision_built_in_function
   | poly_built_in_function
   | pointer_built_in_function
   | pointeradd_built_in_function
   | pointervalue_built_in_function
   | onchar_built_in_function
   | oncode_built_in_function
   | oncount_built_in_function
   | onfile_built_in_function
   | onkey_built_in_function
   | onfile_built_in_function
   | onloc_built_in_function
   | onsource_built_in_function
   | onloc_built_in_function
   | offset_built_in_function
   | null_built_in_function
   | mpstr_built_in_function
   | mod_built_in_function
   | min_built_in_function
   | max_built_in_function
   | log2_built_in_function
   | log10_built_in_function
   | log_built_in_function
   | low_built_in_function
   | lineno_built_in_function
   | length_built_in_function
   | lbound_built_in_function
   | index_built_in_function
   | imag_built_in_function
   | high_built_in_function
   | hbound_built_in_function
   | graphic_built_in_function
   | float_built_in_function
   | float_built_in_function
   | fixed_built_in_function
   | exp_built_in_function
   | erfc_built_in_function
   | erf_built_in_function
   | entryaddr_built_in_function
   | empty_built_in_function
   | divide_built_in_function
   | dim_built_in_function
   | decimal_built_in_function
   | date_built_in_function
   | datetime_built_in_function
   | datafield_built_in_function
   | currentstorage_built_in_function
   | count_built_in_function
   | cosh_built_in_function
   | cos_built_in_function
   | cosd_built_in_function
   | complex_built_in_function
   | conjg_built_in_function
   | completion_built_in_function
   | chat_built_in_function
   | ceil_built_in_function
   | bool_built_in_function
   | bit_built_in_function
   | binaryvalue_built_in_function
   | binary_built_in_function
   | atan_built_in_function
   | atand_built_in_function
   | atanh_built_in_function
   | asin_built_in_function
   | any_built_in_function
   | allocation_built_in_function
   | all_built_in_function
   | addr_built_in_function
   | add_built_in_function
   | acos_built_in_function
   | abs_built_in_function
   | multiply_built_in_function
   ;

built_in_function
   : name ('(' (argument ',')* ')')?
   ;

parmset_built_in_function
   : 'PARMSET' '(' expression ')'
   ;

counter_built_in_function
   : 'COUNTER'
   ;

compiletime_built_in_function
   : 'COMPILETIME'
   ;

verify_built_in_function
   : 'VERIFY' '(' expression ',' expression ')'
   ;

unspec_built_in_function
   : 'UNSPEC' '(' expression ')'
   ;

trunc_built_in_function
   : 'TRUNC' '(' expression ')'
   ;

translate_built_in_function
   : 'TRANSLATE' '(' expression ',' expression (',' expression)? ')'
   ;

time_built_in_function
   : 'TIME' ('(' ')')?
   ;

tanh_built_in_function
   : 'TANH' '(' expression ')'
   ;

tand_built_in_function
   : 'TAND' '(' expression ')'
   ;

tan_built_in_function
   : 'TAN' '(' expression ')'
   ;

sysnull_built_in_function
   : 'SYSNULL' ('(' ')')?
   ;

sum_built_in_function
   : 'SUM' '(' expression ')'
   ;

substr_built_in_function
   : 'SUBSTR' '(' expression ',' expression (',' expression)? ')'
   ;

string_built_in_function
   : 'STRING' '(' expression ')'
   ;

storage_built_in_function
   : 'STORAGE' '(' expression ')'
   ;

status_built_in_function
   : 'STATUS' ('(' expression ')')?
   ;

sqrt_built_in_function
   : 'SQRT' '(' expression ')'
   ;

sinh_built_in_function
   : 'SINH' '(' expression ')'
   ;

sind_built_in_function
   : 'SIND' '(' expression ')'
   ;

sin_built_in_function
   : 'SIN' '(' expression ')'
   ;

sign_built_in_function
   : 'SIGN' '(' expression ')'
   ;

samekey_built_in_function
   : 'SAMEKEY' '(' expression ')'
   ;

round_built_in_function
   : 'ROUND' '(' expression ',' expression ')'
   ;

repeat_built_in_function
   : 'REPEAT' '(' expression ',' expression ')'
   ;

real_built_in_function
   : 'REAL' '(' expression ')'
   ;

prod_built_in_function
   : 'PROD' '(' expression ')'
   ;

priority_built_in_function
   : 'PRIORITY' '(' expression ')'
   ;

precision_built_in_function
   : 'PRECISION' '(' expression ',' expression (',' expression)? ')'
   ;

poly_built_in_function
   : 'POLY' '(' expression ',' expression ')'
   ;

pointervalue_built_in_function
   : 'POINTERVALUE' '(' expression ')'
   ;

pointeradd_built_in_function
   : 'POINTERADD' '(' expression ',' expression ')'
   ;

pointer_built_in_function
   : 'POINTER' '(' expression ',' expression ')'
   ;

pliretv_built_in_function
   : 'PLIRETV' ('(' ')')?
   ;

onsource_built_in_function
   : 'ONSOURCE' ('(' ')')?
   ;

onloc_built_in_function
   : 'ONLOC' ('(' ')')?
   ;

onkey_built_in_function
   : 'ONKEY' ('(' ')')?
   ;

onfile_built_in_function
   : 'ONFILE' ('(' ')')?
   ;

oncount_built_in_function
   : 'ONCOUNT' ('(' ')')
   ;

oncode_built_in_function
   : 'ONCODE' ('(' ')')
   ;

onchar_built_in_function
   : 'ONCHAR' ('(' ')')
   ;

offset_built_in_function
   : 'OFFSET' '(' expression ',' expression ')'
   ;

null_built_in_function
   : 'NULL' ('(' ')')
   ;

multiply_built_in_function
   : 'MULTIPLY' '(' expression ',' expression ',' expression (',' expression)? ')'
   ;

mpstr_built_in_function
   : 'MPSTR' '(' expression ',' expression (',' expression)? ')'
   ;

mod_built_in_function
   : 'MOD' '(' expression ',' expression ')'
   ;

min_built_in_function
   : 'MIN' '(' (expression ',')+ ')'
   ;

max_built_in_function
   : 'MAX' '(' (expression ',')+ ')'
   ;

low_built_in_function
   : 'LOW' '(' expression ')'
   ;

log10_built_in_function
   : 'LOG10' '(' expression ')'
   ;

log2_built_in_function
   : 'LOG2' '(' expression ')'
   ;

log_built_in_function
   : 'LOG' '(' expression ')'
   ;

lineno_built_in_function
   : 'LINENO' '(' expression ')'
   ;

length_built_in_function
   : 'LENGTH' '(' expression ')'
   ;

lbound_built_in_function
   : 'LBOUND' '(' expression ',' expression ')'
   ;

index_built_in_function
   : 'INDEX' '(' expression ',' expression ')'
   ;

imag_built_in_function
   : 'IMAG' '(' expression ')'
   ;

high_built_in_function
   : 'HIGH' '(' expression ')'
   ;

hbound_built_in_function
   : 'HBOUND' '(' expression ',' expression ')'
   ;

graphic_built_in_function
   : 'GRAPHIC' '(' expression (',' expression)? ')'
   ;

floor_built_in_function
   : 'FLOOR' '(' expression ')'
   ;

float_built_in_function
   : 'FLOAT' '(' expression (',' expression)? ')'
   ;

fixed_built_in_function
   : 'FIXED' '(' expression (',' expression (',' expression)?)? ')'
   ;

exp_built_in_function
   : 'EXP' '(' expression ')'
   ;

erfc_built_in_function
   : 'ERFC' '(' expression ')'
   ;

erf_built_in_function
   : 'ERF' '(' expression ')'
   ;

entryaddr_built_in_function
   : 'ENTRYADDR' '(' expression ')'
   ;

empty_built_in_function
   : 'EMPTY' ('(' ')')?
   ;

divide_built_in_function
   : 'DIVIDE' '(' expression ',' expression ',' expression (',' expression)? ')'
   ;

dim_built_in_function
   : 'DIM' '(' expression ',' expression ')'
   ;

decimal_built_in_function
   : 'DECIMAL' '(' expression (',' expression (',' expression)?)? ')'
   ;

datetime_built_in_function
   : 'DATETIME' ('(' ')')?
   ;

date_built_in_function
   : 'DATE' ('(' ')')?
   ;

datafield_built_in_function
   : 'DATAFIELD' ('(' ')')?
   ;

currentstorage_built_in_function
   : 'CURRENTSTORAGE' '(' expression ')'
   ;

count_built_in_function
   : 'COUNT' '(' expression ')'
   ;

cosh_built_in_function
   : 'COSH' '(' expression ')'
   ;

cosd_built_in_function
   : 'COSD' '(' expression ')'
   ;

cos_built_in_function
   : 'COS' '(' expression ')'
   ;

conjg_built_in_function
   : 'CONJG' '(' expression ')'
   ;

complex_built_in_function
   : 'COMPLEX' '(' expression ',' expression ')'
   ;

completion_built_in_function
   : 'COMPLETION' '(' expression ')'
   ;

chat_built_in_function
   : 'CHAR' '(' expression (',' expression)? ')'
   ;

ceil_built_in_function
   : 'CEIL' '(' expression ')'
   ;

bool_built_in_function
   : 'BOOL' '(' expression ',' expression ',' expression ')'
   ;

bit_built_in_function
   : 'BIT' '(' expression (',' expression)? ')'
   ;

binaryvalue_built_in_function
   : 'BINARYVALUE' '(' expression ')'
   ;

binary_built_in_function
   : 'BINARY' '(' expression (',' expression (',' expression)?)? ')'
   ;

atanh_built_in_function
   : 'ATANH' '(' expression ')'
   ;

atand_built_in_function
   : 'ATAND' '(' expression (',' expression)? ')'
   ;

atan_built_in_function
   : 'ATAN' '(' expression (',' expression)? ')'
   ;

asin_built_in_function
   : 'ASIN' '(' expression ')'
   ;

any_built_in_function
   : 'ANY' '(' expression ')'
   ;

allocation_built_in_function
   : 'ALLOCATION' '(' expression ')'
   ;

all_built_in_function
   : 'ALL' '(' expression ')'
   ;

addr_built_in_function
   : 'ADDR' '(' expression ')'
   ;

add_built_in_function
   : 'ADD' '(' expression ',' expression ',' expression (',' expression)? ')'
   ;

acos_built_in_function
   : 'ACOS' '(' expression ')'
   ;

abs_built_in_function
   : 'ABS' '(' expression ')'
   ;

built_in_subroutine
   : call_built_in_subroutine
   | plitest_built_in_subroutine
   | plitdli_built_in_subroutine
   | plisrtd_built_in_subroutine
   | plisrtc_built_in_subroutine
   | plisrbbuilt_in_subroutine
   | plistra_built_in_subroutine
   | pliretc_built_in_subroutine
   | plirest_built_in_subroutine
   | plidump_built_in_subroutine
   | plickpt_built_in_subroutine
   | plicanc_built_in_subroutine
   | asmtdli_built_in_subroutine
   ;

call_built_in_subroutine
   : call_name ('(' (argument ',')* ')')? ';'
   ;

plitest_built_in_subroutine
   : 'PLITEST' ('(' command_list ')')?
   ;

plitdli_built_in_subroutine
   : 'PLITDLI' '(' (argument ',')+ ')'
   ;

plisrtd_built_in_subroutine
   : 'PLISRTD' '(' (argument ',')+ ')'
   ;

plisrtc_built_in_subroutine
   : 'PLISRTC' '(' (argument ',')+ ')'
   ;

plisrbbuilt_in_subroutine
   : 'PLISRTB' '(' (argument ',')+ ')'
   ;

plistra_built_in_subroutine
   : 'PLISRTA' '(' (argument ',')+ ')'
   ;

pliretc_built_in_subroutine
   : 'PLIRETC' '(' return_code ')'
   ;

plirest_built_in_subroutine
   : 'PLIREST'
   ;

plidump_built_in_subroutine
   : 'PLIDUMP' '(' (argument ',')+ ')'
   ;

plickpt_built_in_subroutine
   : 'PLICKPT' '(' (argument ',')+ ')'
   ;

plicanc_built_in_subroutine
   : 'PLICANC'
   ;

asmtdli_built_in_subroutine
   : 'ASMTDLI' '(' (argument ',')+ ')'
   ;

pseudovariable
   : completion_pseudovariable
   | complex_pseudovariable
   | entryaddr_pseudovariable
   | entryaddr_pseudovariable
   | onchar_pseudovariable
   | onsource_pseudovariable
   | priority_pseudovariable
   | real_pseudovariable
   | status_pseudovariable
   | string_pseudovariable
   | substr_pseudovariable
   | unspec_pseudovariable
   | imag_pseudovariable
   ;

completion_pseudovariable
   : 'COMPLETION' '(' expression ')'
   ;

complex_pseudovariable
   : 'COMPLEX' '(' expression ',' expression ')'
   ;

entryaddr_pseudovariable
   : 'ENTRYADDR' '(' expression ')'
   ;

imag_pseudovariable
   : 'IMAG' '(' expression ')'
   ;

onchar_pseudovariable
   : 'ONCHAR' ('(' ')')?
   ;

onsource_pseudovariable
   : 'ONSOURCE' ('(' ')')?
   ;

priority_pseudovariable
   : 'PRIORITY' ('(' expression ')')?
   ;

real_pseudovariable
   : 'REAL' '(' expression ')'
   ;

status_pseudovariable
   : 'STATUS' ('(' expression ')')?
   ;

string_pseudovariable
   : 'STRING' '(' expression ')'
   ;

substr_pseudovariable
   : 'SUBSTR' '(' expression ',' expression (',' expression)? ')'
   ;

unspec_pseudovariable
   : 'UNSPEC' '(' expression ')'
   ;

statement_options_group
   : 'FILE' '(' file_reference ')' ('STREAM' | 'RECORD')? (('INPUT' | 'OUTPUT' | 'UPDATE'))? (('DIRECT' | 'SEQUENTIAL' | 'TRANSIENT'))? ('BUFFERED' | 'UNBUFFERED')? 'BACKWARDS'? 'EXCLUSIVE'? 'KEYED'? 'PRINT'? ('TITLE' '(' expression ')')? ('LINESIZE' '(' expression ')')? ('PAGESIZE' '(' expression ')')?
   ;

data_specification
   : ('LIST'? '(' data_specification_data_list ')' | 'DATA' ('(' (data_specification_data_list ',')+ ')')? | 'EDIT' ('(' data_specification_data_list ')' '(' data_specification_format_list ')')+)
   ;

data_specification_data_list
   : ((data_list_item ',')+ | ('(' data_list_item_type_3_do ')')+)
   ;

data_specification_format_list
   : ((format_item | n_format_item | n '(' format_list ')') ',')+
   ;

list_directed_data_value
   : ((('+' | '-'))? arithmetic_constant | (('+' | '-'))? real_constant ('+' | '-') imaginary_constant | character_constant | bit_constant | graphic_constant)
   ;

data_directed_data_specification
   : 'DATA' ('(' data_list ')')?
   ;

data_directed_element_assignment
   : ((element_variable '=' data_value)+ ',')+ ';'
   ;

edit_directed_data_specification
   : 'EDIT' ('(' data_list ')' '(' format_list ')')+
   ;

edit_directed_data_specification_format_list
   : ((format_item ',')+ | (n_format_item ',')+ | (n '(' format_list ')') ','+)
   ;

format_item
   : format_item_a
   | format_item_b
   | format_item_c
   | format_item_column
   | format_item_e
   | format_item_f
   | format_item_g
   | format_item_line
   | format_item_p
   | format_item_page
   | format_item_r
   | format_item_skip
   | format_item_x
   ;

format_item_a
   : 'A' ('(' field_width ')')?
   ;

format_item_b
   : 'B' ('(' field_width ')')?
   ;

format_item_c
   : 'C' '(' (real_format_item ',')+ ')'
   ;

format_item_column
   : 'COLUMN' '(' character_position ')'
   ;

format_item_e
   : 'E' '(' field_width ',' fractional_digits (',' significant_digits)? ')'
   ;

format_item_f
   : 'F' '(' field_width (',' fractional_digits (',' scaling_factor)?)? ')'
   ;

format_item_g
   : 'G' ('(' field_width ')')?
   ;

format_item_line
   : 'LINE' '(' line_number ')'
   ;

format_item_p
   : 'P' '\'' picture_specification '\''
   ;

format_item_page
   : 'PAGE'
   ;

format_item_r
   : 'R' '(' label_reference ')'
   ;

format_item_skip
   : 'SKIP' ('(' relative_line ')')?
   ;

format_item_x
   : 'X' '(' field_width ')'
   ;

scaling_factor_character
   : 'F' '(' (('+' | '-'))? integer ')'
   ;

condition
   : condition_area
   | condition_attention
   | condition_check
   | condition_condition
   | condition_conversion
   | condition_endfile
   | condition_endpage
   | condition_error
   | condition_finish
   | condition_fixedoverflow
   | condition_key
   | condition_name
   | condition_overflow
   | condition_pending
   | condition_record
   | condition_size
   | condition_stringrange
   | condition_stringsize
   | condition_subscriptrange
   | condition_transmit
   | condition_undefinedfile
   | condition_underflow
   | condition_zeodivide
   ;

condition_area
   : 'AREA'
   ;

condition_attention
   : 'ATTENTION'
   ;

condition_check
   : 'CHECK' ('(' (name ',')+ ')')?
   ;

condition_condition
   : 'CONDITION' '(' name ')'
   ;

condition_conversion
   : 'CONVERSION'
   ;

condition_endfile
   : 'ENDFILE' '(' file_reference ')'
   ;

condition_endpage
   : 'ENDPAGE' '(' file_reference ')'
   ;

condition_error
   : 'ERROR'
   ;

condition_finish
   : 'FINISH'
   ;

condition_fixedoverflow
   : 'FIXEDOVERFLOW'
   ;

condition_key
   : 'KEY' '(' file_reference ')'
   ;

condition_name
   : 'NAME' '(' file_reference ')'
   ;

condition_overflow
   : 'OVERFLOW'
   ;

condition_pending
   : 'PENDING' '(' file_reference ')'
   ;

condition_record
   : 'RECORD' '(' file_reference ')'
   ;

condition_size
   : 'SIZE'
   ;

condition_stringrange
   : 'STRINGRANGE'
   ;

condition_stringsize
   : 'STRINGSIZE'
   ;

condition_subscriptrange
   : 'SUBSCRIPTRANGE'
   ;

condition_transmit
   : 'TRANSMIT' '(' file_reference ')'
   ;

condition_undefinedfile
   : 'UNDEFINEDFILE' '(' file_reference ')'
   ;

condition_underflow
   : 'UNDERFLOW'
   ;

condition_zeodivide
   : 'ZERODIVIDE'
   ;

statement_preprocessor_do_specification
   : preprocessor_variable '=' preprocessor_exp1 (('TO' preprocessor_exp2 ('BY' preprocessor_exp3)? | 'BY' preprocessor_exp3 ('TO' preprocessor_exp2)?))?
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

STRING
   : [a-zA-Z] [a-zA-Z]*
   ;

NUMBER
   : [0-9]+
   ;

WS
   : [ \t\r\n] -> skip
   ;

