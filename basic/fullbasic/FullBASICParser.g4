/*
 * Full BASIC
 *   ANSI X3.113-1987 (as adopted in FIPS PUB 68-2)
 *   ECMA-116, 1st edition, June 1986
 *   ISO/IEC 10279:1991
 * Section numbers (§) refer to ANSI X3.113-1987.
 * Modules: Core, Enhanced files, Exception handling, Graphics, Fixed decimal.
 *
 * Copyright (c) 2026 AJGB (ajgb.eu)
 * SPDX-License-Identifier: MIT
 */

parser grammar FullBASICParser;
options {
    tokenVocab = FullBASICLexer;
}

// §4.1.4, §4.4.4

program:
    program_name_line? main_program procedure_part* tail* EOF
;

// §4 PROGRAM ELEMENTS
// §4.1 CHARACTERS AND STRINGS
quoted_string: STRINGLITERAL;

// §4.2 Programs, lines and blocks
program_name_line:
    line_number PROGRAM program_name function_parm_list? tail
;
program_name: routine_identifier;
main_program: unit_block* end_line tail*;
unit_block: block | internal_proc_def;
internal_proc_def:
    internal_function_def
    | internal_sub_def
    | detached_handler
    | internal_picture_def
;
block:
    statement_line
    | remark_line
    | loop
    | if_block
    | select_block
    | image_line
    | protection_block
;
statement_line: line_number statement tail | line_number tail;
line_number: integer;
tail: TAIL_COMMENT? end_of_line;
end_of_line: END_LINE+;
end_line: line_number end_statement;
end_statement: END;
statement:
    declarative_statement
    | imperative_statement
    | conditional_statement
;
declarative_statement:
    data_statement
    | declare_statement
    | dimension_statement
    | option_statement
    | remark_statement
    // §8.4.2
    | template_statement
    // §9.1.2
;
imperative_statement:
    transform_call_assignment
    | array_assignment
    | array_input_statement
    | file_array_input_statement
    | array_line_input_statement
    | file_array_line_input_statement
    | array_print_statement
    | file_array_print_statement
    | file_print_statement
    | array_read_statement
    | file_array_read_statement
    | file_read_statement
    | array_write_statement
    | array_rewrite_statement
    | rewrite_statement
    | delete_statement
    | replace_statement
    | array_replace_statement
    | random_statement
    | next_file_statement
    | rename_statement
    | ask_statement
    | file_ask_statement
    | call_statement
    | cause_statement
    | chain_statement
    | close_statement
    | clear_statement
    | debug_statement
    | graphic_set_statement
    | graphic_ask_statement
    | graphic_output_statement
    | graphic_input_statement
    | draw_statement
    | exit_picture_statement
    | transform_assignment
    | break_statement
    | erase_statement
    | exit_do_statement
    | exit_for_statement
    | exit_function_statement
    | exit_handler_statement
    | exit_sub_statement
    | gosub_statement
    | goto_statement
    | handler_return_statement
    | input_statement
    | file_input_statement
    | line_input_statement
    | file_line_input_statement
    | numeric_let_statement
    | string_let_statement
    | numeric_function_let_statement
    | string_function_let_statement
    | open_statement
    | print_statement
    | write_statement
    | read_statement
    | restore_statement
    | return_statement
    | set_statement
    | file_set_statement
    | stop_statement
    | trace_statement
    | randomize_statement
;
stop_statement: STOP;
conditional_statement:
    if_statement
    | on_goto_statement
    | on_gosub_statement
;
procedure_part: remark_line* procedure;
// §4.2.2
procedure:
    external_function_def
    | external_sub_def
    | external_picture_def
;
// §4.2.2 prod. 21, §4.3.2 prod. 3
remark_line: line_number remark_statement? tail;

// §4.3 Program Annotation
remark_statement: REM | REMARK_STRING;

// §4.4 Identifiers
numeric_identifier: NUMERIC_IDENTIFIER | soft_syntax;
string_identifier: STRING_IDENTIFIER | soft_syntax_str;
routine_identifier: NUMERIC_IDENTIFIER | soft_syntax;

// §5 NUMBERS
// §5.1 Numeric Constants
integer: UNSIGNED_INT;
signed_integer: (PLUS_SIGN | MINUS_SIGN)? UNSIGNED_INT;
numeric_constant: UNSIGNED_SCI | UNSIGNED_FIXED | UNSIGNED_INT;

// §5.2 Numeric Variables
numeric_variable:
    simple_numeric_variable
    | numeric_array_element
;
simple_numeric_variable: numeric_identifier;
numeric_array_element: numeric_array subscript_part;
numeric_array: numeric_identifier;
subscript_part:
    LEFT_PARENTHESIS subscript (COMMA subscript)* RIGHT_PARENTHESIS
;
subscript: index;
index: numeric_expression;
simple_variable:
    simple_numeric_variable
    | simple_string_variable
;
// §7.1, §10.5, §10.5.4
array_name: numeric_array | string_array;
// §5.3 Numeric Expressions
numeric_expression: term ((PLUS_SIGN | MINUS_SIGN) term)*;
term: unary (multiplier unary)*;
multiplier: ASTERISK | SLANT;
unary: (PLUS_SIGN | MINUS_SIGN) unary | factor;
factor: primary (CIRCUMFLEX_ACCENT primary)*;
primary:
    numeric_supplied_function_ref
    | numeric_variable
    | numeric_constant
    | numeric_function_ref
    | LEFT_PARENTHESIS numeric_expression RIGHT_PARENTHESIS
;
numeric_supplied_function_ref:
    numeric_supplied_function function_arg_list
    | array_numeric_function_ref
    | array2_numeric_function_ref
;
numeric_numeric_function_ref:
    numeric_function_name function_arg_list?
;
numeric_function_name:
    numeric_defined_function
    | numeric_supplied_function
;
function_arg_list:
    LEFT_PARENTHESIS function_argument (COMMA function_argument)* RIGHT_PARENTHESIS
;
function_argument: expression | actual_array;
actual_array: array_name;

// §5.4 Implementation-Supplied Numeric Functions
numeric_supplied_function:
    ABS
    | ACOS
    | ANGLE
    | ASIN
    | ATN
    | CEIL
    | COS
    | COSH
    | COT
    | CSC
    | DATE
    | DEG
    | EPS
    | EXP
    | FP
    | MAXNUM
    | INT
    | IP
    | LOG
    | LOG10
    | LOG2
    | MAX
    | MIN
    | MOD
    | PI
    | RAD
    | REMAINDER
    | RND
    | ROUND
    | SEC
    | SGN
    | SIN
    | SINH
    | SQR
    | TAN
    | TANH
    | TIME
    | TRUNCATE
    | EXTYPE
    | EXLINE
    | string_numeric_functions
;
string_numeric_functions: LEN | ORD | POS | VAL | MAXLEN;

randomize_statement: RANDOMIZE;

// §5.5 Numeric Assignment Statement
numeric_let_statement:
    LET numeric_variable_list EQUALS_SIGN numeric_expression
;
numeric_variable_list:
    numeric_variable (COMMA numeric_variable)*
;

// §5.6 Numeric Arithmetic and Angle
option_statement: OPTION option_list;
numeric_option:
    ARITHMETIC (DECIMAL | NATIVE)
    | ANGLE (DEGREES | RADIANS)
;
declare_statement: DECLARE type_declaration;
numeric_type_declaration: numeric_type;
numeric_type:
    NUMERIC numeric_declaration (COMMA numeric_declaration)*
    | fixed_numeric_type
;

// §6 STRINGS
string_constant: quoted_string;

// §6.2 String Variables
variable: numeric_variable | string_variable;
string_variable: (simple_string_variable | string_array_element) substring_qualifier?
;
simple_string_variable: string_identifier;
string_array_element: string_array subscript_part;
string_array: string_identifier;
substring_qualifier:
    LEFT_PARENTHESIS index COLON index RIGHT_PARENTHESIS
;

// §6.3 String Expressions
string_expression:
    string_primary (concatenation string_primary)*
;
string_primary:
    string_supplied_function_ref
    | string_constant
    | string_variable
    | string_function_ref
    | LEFT_PARENTHESIS string_expression RIGHT_PARENTHESIS
;
string_supplied_function_ref:
    string_supplied_function function_arg_list
;
string_function_ref: string_function function_arg_list?;
string_function:
    string_defined_function
    | string_supplied_function
;
concatenation: AMPERSAND;

// §6.4 Implementation-Supplied String Functions
string_supplied_function:
    CHR_
    | DATE_
    | LCASE_
    | LTRIM_
    | REPEAT_
    | RTRIM_
    | STR_
    | TIME_
    | UCASE_
    | USING_
    | EXTEXT_
;

// §6.5 String Assignment Statements
string_let_statement:
    LET string_variable_list EQUALS_SIGN string_expression
;
string_variable_list: string_variable ( COMMA string_variable)*;

// §6.6, §7.1.2, §7.1.4
string_option: COLLATE (NATIVE | STANDARD);
string_type_declaration: string_type;
string_type:
    STRING length_max? string_declaration (
        COMMA string_declaration
    )*
;
length_max: ASTERISK integer;
string_declaration:
    simple_string_declaration
    | array_string_declaration
;
simple_string_declaration: simple_string_variable length_max?;

// §7 ARRAYS
// §7.1 Array declarations
dimension_statement: DIM dimension_list;
dimension_list: array_declaration ( COMMA array_declaration)*;
array_declaration:
    numeric_array_declaration
    | string_array_declaration
;
numeric_array_declaration: numeric_array bounds;
bounds:
    LEFT_PARENTHESIS bounds_range (COMMA bounds_range)* RIGHT_PARENTHESIS
;
bounds_range: signed_integer TO signed_integer | signed_integer;
string_array_declaration: string_array bounds;
array_option: BASE integer;
array_string_declaration: string_array_declaration length_max?;
array_numeric_function_ref:
    MAXSIZE maxsize_argument
    | SIZE bound_argument
    | LBOUND bound_argument
    | UBOUND bound_argument
;
maxsize_argument:
    LEFT_PARENTHESIS actual_array RIGHT_PARENTHESIS
;
bound_argument:
    LEFT_PARENTHESIS actual_array (COMMA index)? RIGHT_PARENTHESIS
;

// §7.2 Numeric Arrays
array_assignment:
    numeric_array_assignment
    | string_array_assignment
;
numeric_array_assignment:
    MAT numeric_array EQUALS_SIGN numeric_array_expression
;
numeric_array_expression: (numeric_array numeric_array_operator)? numeric_array
    | scalar_multiplier numeric_array
    | numeric_array_value
    | numeric_array_function_ref
;
numeric_array_operator: PLUS_SIGN | MINUS_SIGN | ASTERISK;
scalar_multiplier: primary ASTERISK;
numeric_array_value:
    scalar_multiplier? (CON | IDN | ZER) redim?
    | TRANSFORM
;
redim:
    LEFT_PARENTHESIS redim_bounds (COMMA redim_bounds)* RIGHT_PARENTHESIS
;
redim_bounds: (index TO)? index;
numeric_array_function_ref: (TRN | INV) LEFT_PARENTHESIS numeric_array RIGHT_PARENTHESIS
;
array2_numeric_function_ref:
    DET LEFT_PARENTHESIS numeric_array RIGHT_PARENTHESIS
    | DOT LEFT_PARENTHESIS numeric_array COMMA numeric_array RIGHT_PARENTHESIS
;

// §7.3 String Arrays
string_array_assignment:
    MAT string_array substring_qualifier? EQUALS_SIGN string_array_expression
;
string_array_expression:
    string_array_primary (concatenation string_array_primary)?
    | string_primary concatenation string_array_primary
    | string_array_primary concatenation string_primary
    | string_array_value
;
string_array_primary: string_array substring_qualifier?;
string_array_value: (string_primary concatenation)? NUL redim?;

// §8 CONTROL STRUCTURES
// §8.1 Relational Expressions
relational_expression: disjunction;
disjunction: conjunction ( OR conjunction)*;
conjunction: relational_term ( AND relational_term)*;
relational_term: NOT? relational_primary;
relational_primary:
    comparison
    | LEFT_PARENTHESIS relational_expression RIGHT_PARENTHESIS
;
comparison:
    numeric_expression relation numeric_expression
    | string_expression relation string_expression
;
relation:
    equality_relation
    | GREATER_THAN_SIGN
    | LESS_THAN_SIGN
    | NOT_GREATER
    | NOT_LESS
;
equality_relation: EQUALS_SIGN | NOT_EQUALS;

// §8.2 Control Statements
goto_statement: (GOTO | GO TO) line_number;
on_goto_statement:
    ON index (GOTO | GO TO) line_number (COMMA line_number)* (
        ELSE imperative_statement
    )?
;
gosub_statement: (GOSUB | GO SUB) line_number;
return_statement: RETURN;
on_gosub_statement:
    ON index (GOSUB | GO SUB) line_number (COMMA line_number)* (
        ELSE imperative_statement
    )?
;

// §8.3 Loop Structures
loop: do_loop | for_loop;
do_loop: do_line do_body;
do_line: line_number do_statement tail;
do_statement: DO exit_condition?;
exit_condition: (WHILE | UNTIL) relational_expression;
do_body: block* loop_line;
exit_do_statement: EXIT DO;
loop_line: line_number loop_statement tail;
loop_statement: LOOP exit_condition?;
for_loop: for_line for_body;
for_line: line_number for_statement tail;
for_statement:
    FOR control_variable EQUALS_SIGN initial_value TO limit (
        STEP increment
    )?
;
control_variable: simple_numeric_variable;
initial_value: numeric_expression;
limit: numeric_expression;
increment: numeric_expression;
for_body: block* next_line;
exit_for_statement: EXIT FOR;
next_line: line_number next_statement tail;
next_statement: NEXT control_variable;

// §8.4 Decision Structures
if_statement:
    IF relational_expression THEN if_clause (ELSE if_clause)?
;
if_clause: imperative_statement | line_number;
if_block:
    if_then_line then_block elseif_block* else_block? end_if_line
;
if_then_line: line_number IF relational_expression THEN tail;
then_block: block*;
elseif_block: elseif_then_line block*;
elseif_then_line:
    line_number ELSEIF relational_expression THEN tail
;
else_block: else_line block*;
else_line: line_number ELSE tail;
end_if_line: line_number END IF tail;
select_block:
    select_line remark_line* case_block case_block* case_else_block? end_select_line
;
select_line: line_number select_statement tail;
select_statement: SELECT CASE expression;
case_block: case_line block*;
case_line: line_number case_statement tail;
case_statement: CASE case_list;
case_list: case_item ( COMMA case_item)*;
case_item: constant | range;
range: (constant TO | IS relation) constant;
case_else_block: case_else_line block*;
case_else_line: line_number CASE ELSE tail;
end_select_line: line_number END SELECT tail;

// §9 PROGRAM SEGMENTATION
// §12.1.2, §4.2.2
internal_function_def:
    internal_def_line
    | internal_function_line (block | detached_handler)* end_function_line
;
internal_def_line: line_number def_statement tail;
def_statement: numeric_def_statement | string_def_statement;
// §15.2.2 prod. 5-9
numeric_def_statement:
    DEF numeric_defined_function fixed_point_type? function_parm_list? EQUALS_SIGN
        numeric_expression
;
// §9.1.2
numeric_defined_function: numeric_identifier;
string_def_statement:
    DEF string_defined_function length_max? function_parm_list? EQUALS_SIGN string_expression
;
string_defined_function: string_identifier;
function_parm_list:
    LEFT_PARENTHESIS function_parameter (
        COMMA function_parameter
    )* RIGHT_PARENTHESIS
;
function_parameter:
    simple_variable
    | formal_array
    | numeric_fixed_parameter
;
formal_array:
    array_name LEFT_PARENTHESIS COMMA* RIGHT_PARENTHESIS
;
internal_function_line:
    line_number FUNCTION (
        numeric_defined_function fixed_point_type?
        | string_defined_function length_max?
    ) function_parm_list? tail
;
end_function_line: line_number END FUNCTION tail;
external_function_def:
    external_function_line unit_block* end_function_line
;
external_function_line:
    line_number EXTERNAL FUNCTION (
        numeric_defined_function fixed_point_type?
        | string_defined_function length_max?
    ) function_parm_list? tail
;
numeric_function_let_statement:
    LET numeric_defined_function function_parm_list? EQUALS_SIGN numeric_expression
;
string_function_let_statement:
    LET string_defined_function function_parm_list? EQUALS_SIGN string_expression
;
exit_function_statement: EXIT FUNCTION;
def_type: DEF function_list;
internal_function_type: FUNCTION function_list;
function_type_declaration:
    def_type
    | internal_function_type
    | external_function_type
;
external_function_type: EXTERNAL FUNCTION function_list;
function_list: defined_function ( COMMA defined_function)*;
defined_function:
    numeric_defined_function
    | string_defined_function
    | fixed_defined_function
;

// §9.2 Subprograms
internal_sub_def:
    internal_sub_line (block | detached_handler)* end_sub_line
;
internal_sub_line: line_number sub_statement tail;
sub_statement: SUB subprogram_name procedure_parm_list?;
subprogram_name: routine_identifier;
procedure_parm_list:
    LEFT_PARENTHESIS procedure_parameter (
        COMMA procedure_parameter
    )* RIGHT_PARENTHESIS
;
procedure_parameter:
    simple_variable
    | formal_array
    | channel_number
    | numeric_fixed_parameter
;
channel_number: NUMBER_SIGN integer;
end_sub_line: line_number end_sub_statement tail;
end_sub_statement: END SUB;
exit_sub_statement: EXIT SUB;
external_sub_def: external_sub_line unit_block* end_sub_line;
external_sub_line: line_number EXTERNAL sub_statement tail;
call_statement: CALL subprogram_name procedure_argument_list?;
procedure_argument_list:
    LEFT_PARENTHESIS procedure_argument (
        COMMA procedure_argument
    )* RIGHT_PARENTHESIS
;
procedure_argument:
    expression
    | actual_array
    | channel_expression
;
sub_type_declaration: internal_sub_type | external_sub_type;
internal_sub_type: SUB sub_list;
external_sub_type: EXTERNAL SUB sub_list;
sub_list: subprogram_name ( COMMA subprogram_name)*;

// §9.3 Chaining
chain_statement:
    CHAIN program_designator (WITH function_arg_list)?
;
program_designator: string_expression;

// §10 INPUT AND OUTPUT
// §10.1 Internal Data
read_statement: READ (missing_recovery COLON)? variable_list;
variable_list: variable ( COMMA variable)*;
missing_recovery: IF MISSING THEN io_recovery_action;
io_recovery_action:
    exit_do_statement
    | exit_for_statement
    | line_number
;
restore_statement: RESTORE line_number?;
data_statement: DATA datum (COMMA datum)*;
datum: constant | unquoted_token+;
// $antlr-format off
unquoted_token: NUMERIC_IDENTIFIER | soft_syntax | DATE | MAXNUM | PI | RND | TIME | ELSE | NOT | PRINT | REM | EXLINE | EXTYPE | CON | IDN | ZER | TRANSFORM | UNSIGNED_INT | UNSIGNED_FIXED | UNSIGNED_SCI | PLUS_SIGN | MINUS_SIGN | PERIOD | OTHER_CHARACTER;
// $antlr-format on

// §10.2 Input
input_statement: INPUT input_modifier_list? variable_list;
input_modifier_list:
    input_modifier (COMMA input_modifier)* COLON
;
input_modifier:
    prompt_specifier
    | timeout_expression
    | time_inquiry
;
prompt_specifier: PROMPT string_expression;
timeout_expression: TIMEOUT numeric_time_expression;
numeric_time_expression: numeric_expression;
time_inquiry: ELAPSED numeric_variable;
line_input_statement:
    LINE INPUT input_modifier_list? string_variable_list
;

// §10.3 Output
print_statement: PRINT (print_list | formatted_print_list);
print_list: print_item? (print_separator print_item?)*;
print_item: tab_call | expression;
tab_call: TAB LEFT_PARENTHESIS index RIGHT_PARENTHESIS;
print_separator: COMMA | SEMICOLON;
set_statement: SET set_object;
set_object: (MARGIN | ZONEWIDTH) index | screen_set_object;
ask_statement: ASK ask_io_list;
ask_io_list: ask_io_item ( COMMA ask_io_item)*;
ask_io_item: (MARGIN | ZONEWIDTH) numeric_variable;

// §10.4 Formatted Output
formatted_print_list: USING image ( COLON output_list)?;
image: line_number | string_expression;
output_list: expression ( COMMA expression)* SEMICOLON?;
// §10.4.2
image_line: line_number IMAGE_COLON IMAGE_BODY? IMAGE_MODE_TAIL;

// §10.5 Array Input and Output
array_read_statement:
    MAT READ (missing_recovery COLON)? redim_array_list
;
redim_array_list: redim_array ( COMMA redim_array)*;
redim_array: array_name redim?;
array_input_statement:
    MAT INPUT input_modifier_list? (
        redim_array_list
        | variable_length_vector
    )
;
variable_length_vector:
    array_name LEFT_PARENTHESIS QUESTION_MARK RIGHT_PARENTHESIS
;
array_line_input_statement:
    MAT LINE INPUT input_modifier_list? redim_string_array_list
;
redim_string_array_list:
    redim_string_array (COMMA redim_string_array)*
;
redim_string_array: string_array redim?;
array_print_statement:
    MAT PRINT (
        array_print_list
        | USING image COLON array_output_list
    )
    | file_array_print_statement
;
array_print_list:
    array_name (print_separator array_name)* print_separator?
;
array_output_list: array_name ( COMMA array_name)* SEMICOLON?;

// §11 FILES
// §11.1 File Operations
open_statement:
    OPEN channel_setter NAME file_name file_attribute_list
;
channel_setter: channel_expression COLON;
channel_expression: NUMBER_SIGN index;
file_name: string_expression;
file_attribute_list: ( COMMA file_attribute)*;
file_attribute: core_file_attribute | enhanced_file_attribute;
core_file_attribute:
    access_mode
    | file_organization
    | record_type
    | record_size
;
access_mode:
    ACCESS (INPUT | OUTPUT | OUTIN | string_expression)
;
file_organization:
    ORGANIZATION (file_organization_value | string_expression)
;
file_organization_value:
    core_file_org_value
    | enhanced_file_org_value
;
core_file_org_value: SEQUENTIAL | STREAM;
record_type: RECTYPE (record_type_value | string_expression);
record_type_value:
    core_record_type_value
    | enhanced_record_type_value
;
core_record_type_value: DISPLAY | INTERNAL;
record_size:
    RECSIZE (VARIABLE | string_expression) (LENGTH index)?
;
close_statement: CLOSE channel_expression;
erase_statement: ERASE REST? channel_expression;
file_ask_statement: ASK channel_setter ask_item_list;
ask_item_list: ask_item ( COMMA ask_item)*;
ask_item: ask_attribute_name variable variable*;
ask_attribute_name:
    core_attribute_name
    | enhanced_attribute_name
;
core_attribute_name:
    ACCESS
    | DATUM
    | ERASABLE
    | FILETYPE
    | MARGIN
    | NAME
    | ORGANIZATION
    | POINTER
    | RECSIZE
    | RECTYPE
    | SETTER
    | ZONEWIDTH
;

enhanced_file_org_value: RELATIVE | KEYED;
enhanced_record_type_value: NATIVE;
enhanced_file_attribute: collate_sequence;
collate_sequence:
    COLLATE (STANDARD | NATIVE | string_expression)
;
enhanced_attribute_name: RECORD | KEY | COLLATE;

// §11.2 File Pointer Manipulation
pointer_items:
    pointer_control
    | io_recovery
    | pointer_control COMMA io_recovery
;
// §11.2.2
pointer_control: POINTER record_setter | enhanced_record_setter;
record_setter: core_record_setter | enhanced_record_setter;
core_record_setter: BEGIN | END | NEXT | SAME;
io_recovery: missing_recovery | not_missing_recovery;
not_missing_recovery: IF THERE THEN io_recovery_action;
enhanced_record_setter:
    RECORD index
    | KEY (exact_search | inexact_search)? string_expression
;
exact_search: EQUALS_SIGN;
inexact_search: GREATER_THAN_SIGN | NOT_LESS;

// §11.3 File Data Creation
file_print_statement:
    PRINT channel_expression print_control (
        COLON (print_list | output_list)
    )?
;
file_array_print_statement:
    MAT PRINT channel_expression print_control COLON (
        array_print_list
        | array_output_list
    )
;
print_control: ( COMMA print_control_item)*;
// §11.3.2
print_control_item:
    core_record_setter
    | not_missing_recovery
    | USING image
;
file_set_statement: SET channel_setter file_set_object;
file_set_object: (MARGIN | ZONEWIDTH) index | pointer_items;
write_statement:
    WRITE channel_expression write_control COLON expression_list
;
array_write_statement:
    MAT WRITE channel_expression write_control COLON array_list
;
write_control: ( COMMA write_control_item)*;
write_control_item:
    record_setter
    | not_missing_recovery
    | template_identifier
;
expression_list: expression ( COMMA expression)*;
array_list: array_name ( COMMA array_name)*;
template_identifier: WITH (line_number | string_expression);
template_statement: TEMPLATE COLON template_element_list;
template_element_list:
    template_element (COMMA template_element)*
;
template_element:
    fixed_field_count (
        field_specifier
        | LEFT_PARENTHESIS template_element_list RIGHT_PARENTHESIS
    )
    | variable_field_count field_specifier
;
fixed_field_count: SKIP_? (integer OF)?;
variable_field_count: QUESTION_MARK OF;
field_specifier: numeric_specifier | string_specifier;
numeric_specifier: NUMERIC ASTERISK numeric_field_size;
// §11.3.2
numeric_field_size:
    UNSIGNED_FIXED
    | fixed_point_size
    | NUMERIC_IDENTIFIER
;
fixed_point_size:
    integer_size PERIOD?
    | integer_size? PERIOD fraction_size
;
integer_size: integer;
fraction_size: integer;
string_specifier: STRING ASTERISK string_field_size;
string_field_size: integer;

// §11.4 File Data Retrieval
file_input_statement:
    INPUT channel_expression input_control COLON variable_list (
        COMMA SKIP_ REST
    )?
;
file_array_input_statement:
    MAT INPUT channel_expression input_control COLON (
        redim_array_list
        | variable_length_vector
    )
;
file_line_input_statement:
    LINE INPUT channel_expression input_control COLON string_variable_list
;
file_array_line_input_statement:
    MAT LINE INPUT channel_expression input_control COLON redim_string_array_list
;
input_control: ( COMMA input_control_item)*;
input_control_item:
    core_record_setter
    | missing_recovery
    | prompt_specifier
    | timeout_expression
    | time_inquiry
;
// §11.4.2
file_read_statement:
    READ channel_expression read_control COLON variable_list (
        COMMA SKIP_ REST
    )?
;
// §11.4.2
file_array_read_statement:
    MAT READ channel_expression read_control COLON redim_array_list
;
read_control: ( COMMA read_control_item)*;
read_control_item:
    record_setter
    | missing_recovery
    | template_identifier
;

// §11.5 File Data Modification
rewrite_statement:
    REWRITE channel_expression rewrite_control COLON expression_list
;
array_rewrite_statement:
    MAT REWRITE channel_expression rewrite_control COLON array_list
;
rewrite_control: ( COMMA rewrite_control_item)*;
rewrite_control_item:
    missing_recovery
    | record_setter
    | template_identifier
;
delete_statement: DELETE channel_expression delete_control;
delete_control: ( COMMA delete_control_item)*;
delete_control_item: missing_recovery | record_setter;

// §11.5 File Data Modification - BASIC-2
replace_statement:
    REPLACE channel_expression replace_control COLON expression_list
;
array_replace_statement:
    MAT REPLACE channel_expression replace_control COLON array_list
;
replace_control: ( COMMA replace_control_item)*;
replace_control_item:
    missing_recovery
    | record_setter
    | template_identifier
;
random_statement: RANDOM channel_expression record_setter;
next_file_statement:
    NEXT channel_expression (COMMA? record_setter)?
;
rename_statement: RENAME file_name TO file_name;

// §12 EXCEPTION HANDLING AND DEBUGGING
// §12.1 Exception Handling
protection_block: when_use_block | when_use_name_block;
when_use_block:
    when_line when_block use_line exception_handler end_when_line
;
when_line: line_number WHEN EXCEPTION IN tail;
when_block: block*;
use_line: line_number USE tail;
exception_handler: block*;
end_when_line: line_number END WHEN tail;
when_use_name_block:
    when_use_name_line when_block end_when_line
;
when_use_name_line:
    line_number WHEN EXCEPTION USE handler_name tail
;
handler_name: routine_identifier;
handler_return_statement: RETRY | CONTINUE;
exit_handler_statement: EXIT HANDLER;
cause_statement: CAUSE EXCEPTION exception_type;
exception_type: index;
detached_handler:
    handler_line exception_handler end_handler_line
;
handler_line: line_number HANDLER handler_name tail;
end_handler_line: line_number END HANDLER tail;

// §12.2 Debugging
debug_statement: DEBUG_KEYWORD (ON | OFF);
break_statement: BREAK;
trace_statement: TRACE ON (TO channel_expression)? | TRACE OFF;

// §13 GRAPHICS
// §13.1 Coordinate System
graphic_set_statement: SET graphic_set_object;
graphic_set_object:
    WINDOW boundaries
    | VIEWPORT boundaries
    | DEVICE WINDOW boundaries
    | DEVICE VIEWPORT boundaries
    | CLIP string_expression
;
boundaries:
    boundary COMMA boundary COMMA boundary COMMA boundary
;
boundary: numeric_expression;
graphic_ask_statement: ASK ask_object status_clause?;
status_clause: STATUS numeric_variable;
ask_object:
    WINDOW boundary_variables
    | VIEWPORT boundary_variables
    | DEVICE WINDOW boundary_variables
    | DEVICE VIEWPORT boundary_variables
    | DEVICE SIZE numeric_variable COMMA numeric_variable COMMA string_variable
    | CLIP string_variable
    | screen_ask_object
    | graphic_ask_object
;
boundary_variables:
    numeric_variable COMMA numeric_variable COMMA numeric_variable COMMA numeric_variable
;

// §13.2 Attributes and Screen Control
clear_statement: CLEAR;

screen_set_object:
    primitive_1 STYLE index
    | primitive_2 COLOR index
    | TEXT HEIGHT numeric_expression
    | TEXT ANGLE numeric_expression
    | TEXT JUSTIFY string_expression COMMA string_expression
    | COLOR MIX LEFT_PARENTHESIS index RIGHT_PARENTHESIS rgb_list
;
primitive_2: primitive_1 | TEXT | AREA;
primitive_1: POINT | LINE;

rgb_list:
    numeric_expression COMMA numeric_expression COMMA numeric_expression
;

screen_ask_object:
    primitive_1 STYLE numeric_variable
    | primitive_2 COLOR numeric_variable
    | TEXT HEIGHT numeric_variable
    | TEXT ANGLE numeric_variable
    | TEXT JUSTIFY string_variable COMMA string_variable
    | MAX primitive_1 STYLE numeric_variable
    | MAX COLOR numeric_variable
    | COLOR MIX LEFT_PARENTHESIS index RIGHT_PARENTHESIS mix_list
;

mix_list:
    numeric_variable COMMA numeric_variable COMMA numeric_variable
;

// §13.3 Graphic Output
graphic_output_statement:
    geometric_statement
    | array_geometric_statement
    | graphic_text_statement
    | array_cells_statement
;

geometric_statement:
    graphic_verb geometric_object COLON point_list
    | PLOT LINES
    | PLOT LINES COLON point_list SEMICOLON
;
graphic_verb: GRAPH | PLOT;
geometric_object: POINTS | LINES | AREA;
point_list: coordinate_pair (SEMICOLON coordinate_pair)*;
coordinate_pair: numeric_expression COMMA numeric_expression;
graphic_text_statement:
    graphic_verb TEXT initial_point (
        COMMA USING image COLON expression_list
        | COLON string_expression
    )
;
initial_point: COMMA? AT coordinate_pair;

array_geometric_statement:
    MAT graphic_verb geometric_object (COMMA size_select)? COLON array_point_list
;
array_point_list:
    numeric_array COMMA numeric_array
    | numeric_array
;
size_select: LIMIT index;

array_cells_statement:
    MAT graphic_verb CELLS COMMA IN point_pair COLON numeric_array
;

// §13.4 Graphic Input
graphic_input_statement:
    locate_statement
    | array_locate_statement
    | GET point_select COLON coordinate_variables
    | MAT GET point_select COLON array_locate_object
;
locate_statement:
    LOCATE (
        point_select COLON coordinate_variables
        | value_select COLON numeric_variable
    )
;
array_locate_statement:
    MAT LOCATE point_select COLON array_locate_object
;
point_select: POINT device_select? initial_point?;
coordinate_variables: numeric_variable COMMA numeric_variable;
value_select:
    CHOICE device_select? start_value?
    | VALUE device_select? range_select? start_value?
;
array_locate_object:
    redim_numeric_array (COMMA redim_numeric_array)?
    | numeric_variable_vector COMMA numeric_variable_vector
    | numeric_variable_matrix
;
redim_numeric_array: numeric_array redim?;
numeric_variable_vector:
    numeric_array LEFT_PARENTHESIS QUESTION_MARK RIGHT_PARENTHESIS
;
// §13.4.2
numeric_variable_matrix:
    numeric_array LEFT_PARENTHESIS QUESTION_MARK COMMA RIGHT_PARENTHESIS
;
range_select:
    COMMA RANGE numeric_expression TO numeric_expression
;
device_select: LEFT_PARENTHESIS index RIGHT_PARENTHESIS;
start_value: COMMA AT numeric_expression;
graphic_ask_object:
    MAX device_type DEVICE numeric_variable
    | PIXEL SIZE LEFT_PARENTHESIS point_pair RIGHT_PARENTHESIS numeric_variable COMMA
        numeric_variable
    | PIXEL ARRAY point_location numeric_array (
        COMMA string_variable
    )?
    | PIXEL VALUE point_location numeric_variable
;
point_pair: coordinate_pair SEMICOLON coordinate_pair;
point_location:
    LEFT_PARENTHESIS coordinate_pair RIGHT_PARENTHESIS
;
device_type: POINT | MULTIPOINT | CHOICE | VALUE;

// §13.5 Graphic Pictures and Moving Point Output
draw_statement: DRAW picture_invocation (WITH transform)?;
picture_invocation: picture_name procedure_argument_list?;
picture_name: routine_identifier;
transform: transform_term (ASTERISK transform_term)*;
transform_term:
    transform_function function_arg_list
    | numeric_array
    | current_transform
;
current_transform: TRANSFORM;
transform_function: ROTATE | SHEAR | SHIFT | SCALE;
exit_picture_statement: EXIT PICTURE;
transform_assignment: MAT numeric_array EQUALS_SIGN transform;
transform_call_assignment:
    MAT numeric_array EQUALS_SIGN transform_function function_arg_list (
        ASTERISK transform_term
    )*
;
internal_picture_def:
    internal_picture_line (block | detached_handler)* end_picture_line
;
internal_picture_line: line_number picture_statement tail;
picture_statement: PICTURE picture_name procedure_parm_list?;
end_picture_line: line_number END PICTURE tail;
external_picture_def:
    external_picture_line unit_block* end_picture_line
;
// §13.5.2
external_picture_line:
    line_number EXTERNAL picture_statement tail
;
internal_picture_type: PICTURE picture_name_list;
picture_name_list: picture_name (COMMA picture_name)*;
external_picture_type: EXTERNAL PICTURE picture_name_list;

option_list: option ( COMMA option)*;
option:
    numeric_option
    | array_option
    | string_option
    | fixed_option
;
type_declaration:
    numeric_type_declaration
    | string_type_declaration
    | function_type_declaration
    | sub_type_declaration
    | internal_picture_type
    | external_picture_type
;
// §6.6
numeric_declaration:
    simple_numeric_variable
    | numeric_array_declaration
;
numeric_function_ref: numeric_numeric_function_ref;
// §5.3.2, §7.2.2
expression: numeric_expression | string_expression;
// §5.1.2, §6.1.2, §8.4.2
constant: (PLUS_SIGN | MINUS_SIGN)? numeric_constant
    | string_constant
;

// §15 FIXED DECIMAL NUMBERS (BASIC-2 only)
// §15.1 Fixed Decimal Precision
fixed_option: ARITHMETIC FIXED fixed_point_type;
fixed_point_type: ASTERISK (UNSIGNED_FIXED | fixed_point_size);
fixed_numeric_type:
    NUMERIC fixed_point_type? fixed_declaration (
        COMMA fixed_declaration
    )*
;
fixed_declaration:
    simple_numeric_variable fixed_point_type?
    | numeric_array_declaration fixed_point_type?
;

// §15.2 Fixed Decimal Program Segmentation
numeric_fixed_parameter:
    simple_numeric_variable fixed_point_type
    | fixed_formal_array
;
fixed_formal_array: formal_array fixed_point_type;
fixed_defined_function:
    numeric_defined_function fixed_point_type
;

// $antlr-format off
soft_syntax: OPTION | BASE | DECIMAL | NATIVE | FIXED | DEGREES | RADIANS | COLLATE | STANDARD | ACCESS | AND | AREA | ARITHMETIC | ASK | AT | BEGIN | BREAK | CALL | CASE | CHAIN | CLEAR | CLIP | CLOSE | COLOR | MIX | DATA | DATUM | DEBUG_KEYWORD | DECLARE | DEF | DEVICE | DIM | DISPLAY | DO | ELAPSED | ELSEIF | END | ERASE | ERASABLE | EXIT | EXTERNAL | FILETYPE | FOR | FUNCTION | GO | GOSUB | GOTO | GRAPH | IF | IMAGE | INPUT | INTERNAL | IS | LENGTH | LET | LINE | LINES | LOOP | MARGIN | MAT | MISSING | NAME | NEXT | NUMERIC | OFF | ON | OPEN | OR | ORGANIZATION | OUTIN | OUTPUT | POINT | POINTER | POINTS | PROGRAM | PROMPT | RANDOMIZE | READ | RECSIZE | RECTYPE | REST | RESTORE | RETURN | SAME | SELECT | SEQUENTIAL | SET | SETTER | SKIP_ | STATUS | STEP | STOP | STREAM | STRING | STYLE | SUB | TEXT | HEIGHT | JUSTIFY | THEN | THERE | TIMEOUT | TO | TRACE | UNTIL | USING | VARIABLE | VIEWPORT | WHILE | WINDOW | WITH | WRITE | ZONEWIDTH | LCASE | LTRIM | REPEAT | RTRIM | STR | UCASE | RELATIVE | KEYED | RECORD | KEY | TEMPLATE | OF | REWRITE | DELETE | REPLACE | RANDOM | RENAME | WHEN | EXCEPTION | IN | USE | RETRY | CONTINUE | HANDLER | CAUSE | LIMIT | DRAW | PICTURE | PLOT | CELLS | GET | LOCATE | CHOICE | VALUE | RANGE | PIXEL | ARRAY | MULTIPOINT | ABS | ACOS | ASIN | ATN | CEIL | COS | COSH | COT | CSC | DEG | EPS | EXP | FP | INT | IP | LOG | LOG10 | LOG2 | MIN | MOD | RAD | REMAINDER | ROUND | SEC | SGN | SIN | SINH | SQR | TAN | TANH | TRUNCATE | TAB | LEN | ORD | POS | VAL | MAXLEN | MAXSIZE | LBOUND | UBOUND | TRN | INV | DET | DOT | ROTATE | SHEAR | SHIFT | SCALE | ANGLE | MAX | SIZE;
// $antlr-format on
// $antlr-format off
soft_syntax_str: CHR_ | LCASE_ | LTRIM_ | REPEAT_ | RTRIM_ | STR_ | UCASE_ | USING_ | EXTEXT_;
// $antlr-format on