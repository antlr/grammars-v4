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
   : 'OPTIONS' '(' characteristic_list ')'
   ;

entry_attribute
   : 'ENTRY' ('(' parameter_descriptor_list? ')')?
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
   : '(' (bound ',')+ ')'
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

charcter_constant
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
   : '\'' ((character | '\'' '\'' | '<' (graphic_symbol)+ '>'))* '\'M'
   ;

attribute_bound
   : ((lower_bound ':')? upper_bound | '*')
   ;

attribute_upper_bound
   : expression ('REFER' '(' variable ')')?
   ;

attribute_lower_bound
   : expression ('REFER' '(' variable ')')?
   ;

expression
   : unary_expression (infix_operator unary_expression)*
   ;

expression_unary_expression
   : (prefix_operator)* elementary_expression
   ;

expression_elementary_expression
   : ('(' expression ')' | reference | constant | isub6)
   ;

expression_reference
   : locator_qualifier? basic_reference ('(' subscript_list ')')? ('(' argument_list? ')')?
   ;

expression_locator_qualifier
   : reference '->'
   ;

expression_subscript_list
   : ((expression | '*') ',')+
   ;

expression_argument_list
   : (expression ',')+
   ;

expression_basic_reference
   : structure_qualification? name
   ;

expression_structure_qualification
   : basic_reference ('(' subscript_list ')')?
   ;

statement
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
   : '%' 'SKIP' ('(' n ')')? ';'
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
   : ('MAIN' 'NOEXECOPS'? 'REENTRANT'? 'TASK'? | 'COBOL' additional_options | 'FORTRAN' additional_options)
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

begine_statement
   : 'BEGIN' ('ORDER' | 'REORDER')? ('CHARGRAPHIC' | 'NOCHARGRAPHIC')? ';'
   ;

declare2_statement
   : 'DECLARE' (level? name attribute* 'SYSTEM'? ',')+ ';'
   ;

default_statement
   : 'DEFAULT' ((simple_specification | factored_specification) ',')+ ';'
   ;

statement_simple_specification
   : ('RANGE' '(' ((letter+ | letter ':' letter))+ ')' attribute_specification? | 'RANGE' '(' '*' ')' attribute_specification? | 'DESCRIPTORS' attribute_specifications?)
   ;

statement_factored_specification
   : '(' ((simple_specification | factored_specification) ',')+ ')' attribute_specification?
   ;

statement_attribute_specification
   : (attribute+ ('VALUE' '(' (value_specification ',')+ ')')? | 'VALUE' '(' (value_specification ',')+ ')')
   ;

statement_value_specification
   : ('AREA' '(' size ')' | 'BIT' '(' length ')' | 'CHARACTER' '(' length ')' | 'GRAPHIC' '(' length ')' | additional_options)
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
   : expression 'REFER' '(' variable ')'
   ;

option
   : 'PRIORITY' '(' expression ')'
   ;

option
   : 'EVENT' '(' event_reference ')'
   ;

option
   : 'TASK' ('(' task_reference ')')?
   ;

attribute_item
   : ('*' | initial_constant | reference | '(' expression ')' | iteration_specification)
   ;

attribute_iteration_specification
   : '(' iteration_factor ')' iteration_item
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

statement_specification
   : exp1 (('TO' exp2 ('BY' exp3)? | 'BY' exp3 ('TO' exp2)? | 'REPEAT' exp6))? (('WHILE' '(' exp4 ')' ('(' 'UNTIL' '(' exp5 ')')? | 'UNTIL' '(' exp5 ')' ('(' 'WHILE' '(' exp4 ')')?))?
   ;

entry_data1
   : 'ENTRY' ('(' parameter_descriptor_list ')')? ('RETURNS' '(' attribute_list ')')? ('OPTIONS' '(' characteristic_list ')')? 'EXTERNAL'? (('IRREDUCIBLE' | 'REDUCIBLE'))?
   ;

entry_data2
   : 'ENTRY' ('(' parameter_descriptor_list ')')? 'VARIABLE'? ('RETURNS' '(' attribute_list ')')? ('OPTIONS' '(' characteristic_list ')')? (('IRREDUCIBLE' | 'REDUCIBLE'))? scope_attribute? storage_attribute? alignment_attribute?? parameter?? 'DEFINED' 'INITIAL'?
   ;

attribute_parameter_descriptor_list
   : (parameter_descriptor ',')+
   ;

attribute_parameter_descriptor
   : (((attribute)+ | '*' | structure_parameter_descriptor))?
   ;

attribute_structure_parameter_descriptor
   : '1' attribute* ',' (level attribute* ',')+
   ;

attribute_characteristic_list
   : ('COBOL' | 'FORTRAN' | 'ASSEMBLER') additional_options?
   ;

attribute_additional_options
   : ('NOMAP' ('(' (argi ',')+ ')')?)? ('NOMAPIN' ('(' (argi)+ ')')?)? ('NOMAPOUT' ('(' (argi ',')+ ')')?)? 'INTER'? 'RETCODE'?
   ;

built_in_function
   : name ('(' (argument ',')* ')')?
   ;

parmset_built_in_function
   : 'PARMSET' '(' x ')'
   ;

counter_built_in_function
   : 'COUNTER'
   ;

compiletime_built_in_function
   : 'COMPILETIME'
   ;

verify_built_in_function
   : 'VERIFY' '(' x ',' y ')'
   ;

nuspec_built_in_function
   : 'UNSPEC' '(' x ')'
   ;

trunc_built_in_function
   : 'TRUNC' '(' x ')'
   ;

translate_built_in_function
   : 'TRANSLATE' '(' x ',' y (',' z)? ')'
   ;

time_built_in_function
   : 'TIME' ('(' ')')?
   ;

tanh_built_in_function
   : 'TANH' '(' x ')'
   ;

tand_built_in_function
   : 'TAND' '(' x ')'
   ;

tan_built_in_function
   : 'TAN' '(' x ')'
   ;

sysnull_built_in_function
   : 'SYSNULL' ('(' ')')?
   ;

sum_built_in_function
   : 'SUM' '(' x ')'
   ;

substr_built_in_function
   : 'SUBSTR' '(' x ',' y (',' z)? ')'
   ;

string_built_in_function
   : 'STRING' '(' x ')'
   ;

storage_built_in_function
   : 'STORAGE' '(' x ')'
   ;

status_built_in_function
   : 'STATUS' ('(' x ')')?
   ;

sqrt_built_in_function
   : 'SQRT' '(' x ')'
   ;

sinh_built_in_function
   : 'SINH' '(' x ')'
   ;

sind_built_in_function
   : 'SIND' '(' x ')'
   ;

sin_built_in_function
   : 'SIN' '(' x ')'
   ;

sign_built_in_function
   : 'SIGN' '(' x ')'
   ;

samekey_built_in_function
   : 'SAMEKEY' '(' x ')'
   ;

round_built_in_function
   : 'ROUND' '(' x ',' y ')'
   ;

repeat_built_in_function
   : 'REPEAT' '(' x ',' y ')'
   ;

real_built_in_function
   : 'REAL' '(' x ')'
   ;

prod_built_in_function
   : 'PROD' '(' x ')'
   ;

priority_built_in_function
   : 'PRIORITY' '(' x ')'
   ;

precision_built_in_function
   : 'PRECISION' '(' x ',' p (',' q)? ')'
   ;

poly_built_in_function
   : 'POLY' '(' x ',' y ')'
   ;

pointervalue_built_in_function
   : 'POINTERVALUE' '(' x ')'
   ;

pointeradd_built_in_function
   : 'POINTERADD' '(' x ',' y ')'
   ;

pointer_built_in_function
   : 'POINTER' '(' x ',' y ')'
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
   : 'OFFSET' '(' x ',' y ')'
   ;

null_built_in_function
   : 'NULL' ('(' ')')
   ;

multiply_built_in_function
   : 'MULTIPLY' '(' x ',' y ',' p (',' q)? ')'
   ;

mpstr_built_in_function
   : 'MPSTR' '(' x ',' r (',' y)? ')'
   ;

mod_built_in_function
   : 'MOD' '(' x ',' y ')'
   ;

min_built_in_function
   : 'MIN' '(' (x ',')+ ')'
   ;

max_built_in_function
   : 'MAX' '(' (x ',')+ ')'
   ;

low_built_in_function
   : 'LOW' '(' x ')'
   ;

log10_built_in_function
   : 'LOG10' '(' x ')'
   ;

log2_built_in_function
   : 'LOG2' '(' x ')'
   ;

log_built_in_function
   : 'LOG' '(' x ')'
   ;

lineno_built_in_function
   : 'LINENO' '(' x ')'
   ;

length_built_in_function
   : 'LENGTH' '(' x ')'
   ;

lbound_built_in_function
   : 'LBOUND' '(' x ',' y ')'
   ;

index_built_in_function
   : 'INDEX' '(' x ',' y ')'
   ;

imag_built_in_function
   : 'IMAG' '(' x ')'
   ;

high_built_in_function
   : 'HIGH' '(' x ')'
   ;

hbound_built_in_function
   : 'HBOUND' '(' x ',' y ')'
   ;

graphic_built_in_function
   : 'GRAPHIC' '(' x (',' y)? ')'
   ;

floor_built_in_function
   : 'FLOOR' '(' x ')'
   ;

float_built_in_function
   : 'FLOAT' '(' x (',' p)? ')'
   ;

fixed_built_in_function
   : 'FIXED' '(' x (',' p (',' q)?)? ')'
   ;

exp_built_in_function
   : 'EXP' '(' x ')'
   ;

erfc_built_in_function
   : 'ERFC' '(' x ')'
   ;

erf_built_in_function
   : 'ERF' '(' x ')'
   ;

entryaddr_built_in_function
   : 'ENTRYADDR' '(' x ')'
   ;

empty_built_in_function
   : 'EMPTY' ('(' ')')?
   ;

devide_built_in_function
   : 'DIVIDE' '(' x ',' y ',' p (',' q)? ')'
   ;

dim_built_in_function
   : 'DIM' '(' x ',' y ')'
   ;

decimal_built_in_function
   : 'DECIMAL' '(' x (',' p (',' q)?)? ')'
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
   : 'CURRENTSTORAGE' '(' x ')'
   ;

count_built_in_function
   : 'COUNT' '(' x ')'
   ;

cosh_built_in_function
   : 'COSH' '(' x ')'
   ;

cosd_built_in_function
   : 'COSD' '(' x ')'
   ;

cos_built_in_function
   : 'COS' '(' x ')'
   ;

conjg_built_in_function
   : 'CONJG' '(' x ')'
   ;

complex_built_in_function
   : 'COMPLEX' '(' x ',' y ')'
   ;

completion_built_in_function
   : 'COMPLETION' '(' x ')'
   ;

chat_built_in_function
   : 'CHAR' '(' x (',' y)? ')'
   ;

ciel_built_in_function
   : 'CEIL' '(' x ')'
   ;

bool_built_in_function
   : 'BOOL' '(' x ',' y ',' z ')'
   ;

bit_built_in_function
   : 'BIT' '(' x (',' y)? ')'
   ;

binaryvalue_built_in_function
   : 'BINARYVALUE' '(' x ')'
   ;

binary_built_in_function
   : 'BINARY' '(' x (',' p (',' q)?)? ')'
   ;

atanh_built_in_function
   : 'ATANH' '(' x ')'
   ;

atand_built_in_function
   : 'ATAND' '(' x (',' y)? ')'
   ;

atan_built_in_function
   : 'ATAN' '(' x (',' y)? ')'
   ;

asin_built_in_function
   : 'ASIN' '(' x ')'
   ;

any_built_in_function
   : 'ANY' '(' x ')'
   ;

allocation_built_in_function
   : 'ALLOCATION' '(' x ')'
   ;

all_built_in_function
   : 'ALL' '(' x ')'
   ;

addr_built_in_function
   : 'ADDR' '(' x ')'
   ;

add_built_in_function
   : 'ADD' '(' x ',' y ',' p (',' q)? ')'
   ;

acos_built_in_function
   : 'ACOS' '(' x ')'
   ;

abs_built_in_function
   : 'ABS' '(' x ')'
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

plisRbuilt_in_subroutine
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

completion_pseudovariable
   : 'COMPLETION' '(' x ')'
   ;

complex_pseudovariable
   : 'COMPLEX' '(' x ',' y ')'
   ;

entryaddr_pseudovariable
   : 'ENTRYADDR' '(' x ')'
   ;

imag_pseudovariable
   : 'IMAG' '(' x ')'
   ;

onchar_pseudovariable
   : 'ONCHAR' ('(' ')')?
   ;

onsource_pseudovariable
   : 'ONSOURCE' ('(' ')')?
   ;

priority_pseudovariable
   : 'PRIORITY' ('(' x ')')?
   ;

real_pseudovariable
   : 'REAL' '(' x ')'
   ;

status_pseudovariable
   : 'STATUS' ('(' x ')')?
   ;

string_pseudovariable
   : 'STRING' '(' x ')'
   ;

substr_pseudovariable
   : 'SUBSTR' '(' x ',' y (',' z)? ')'
   ;

unspec_pseudovariable
   : 'UNSPEC' '(' x ')'
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

