// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License.

//*******************************************************************/
//* Company:    Microsoft Corporation                               */
//* Engineer:   Richard Neil Pittman                                */
//*                                                                 */
//* Revision:                                                       */
//* Revision    0.1.0   -   Internal Beta Release                   */
//* Revision    0.2.0   -   ANTLR GitHub Release                    */
//*                                                                 */
//* Additional Comments:                                            */
//*                                                                 */
//*******************************************************************/

grammar SysVerilogHDL;

/**********LEXER**********/

// Channels
fragment
COMMENT_TEXT : .*? ;

Carriage_return : '\r' -> channel(HIDDEN);
Forward_slash_forward_slash : '//'  -> channel(3);
Forward_slash_star : '/*'  -> channel(3);
New_line : '\n' -> channel(HIDDEN);
Star_forward_slash : '*/'  -> channel(3);

Block_comment : Forward_slash_star COMMENT_TEXT Star_forward_slash -> channel(3);
//Line directive emitted by preprocessor before and after include file insertion.
Line_directive : '`line' .*? Carriage_return? New_line -> channel(2) ;
One_line_comment : Forward_slash_forward_slash COMMENT_TEXT Carriage_return? New_line -> channel(3) ;
WHITE_SPACE : [ \t\r\n]+ -> channel(HIDDEN) ;


// numbers
fragment
Binary_base : Quote [sS]? [bB] ;
fragment
Binary_digit : X_digit | Z_digit | [01] ;
fragment
Binary_value : Binary_digit ( '_' | Binary_digit )* ;
fragment
Decimal_base : Quote [sS]? [dD] ;
fragment
Decimal_digit : [0-9] ;
fragment
Exp : 'e' | 'E' ;
fragment
Hex_base : Quote [sS]? [hH];
fragment
Hex_digit : X_digit | Z_digit | [0-9a-fA-F] ;
fragment
Hex_value : Hex_digit ( '_' | Hex_digit )* ;
fragment
No_base : Quote [sS]? ;
fragment
Non_zero_decimal_digit : [1-9] ;
fragment
Non_zero_unsigned_number : Non_zero_decimal_digit ( '_' | Decimal_digit)* ;
fragment
Octal_base : Quote [sS]? [oO] ;
fragment
Octal_digit : X_digit | Z_digit | [0-7] ;
fragment
Octal_value : Octal_digit ( '_' | Octal_digit )* ;
fragment
Size : Non_zero_unsigned_number ;
fragment
Unsigned_number : Decimal_digit ( '_' | Decimal_digit )* ;
fragment
X_digit : [xX] ;
fragment
Z_digit : [zZ?] ;

Binary_number : ( Size )? Binary_base Binary_value ;
Decimal_number : Unsigned_number
               | ( Size )? Decimal_base Unsigned_number
               | ( Size )? Decimal_base X_digit ( '_' )*
               | ( Size )? Decimal_base Z_digit ( '_' )*
               | No_base Unsigned_number
               | No_base X_digit ( '_' )*
               | No_base Z_digit ( '_' )*
               ;
Fixed_point_number : Unsigned_number Dot Unsigned_number ;
Hex_number : ( Size )? Hex_base Hex_value ;
Octal_number : ( Size )? Octal_base Octal_value ;
Real_exp_form : Unsigned_number ( Dot Unsigned_number )? Exp ( '+' | '-' )? Unsigned_number ;
Unbased_unsized_literal : '\'0' | '\'1' | Quote Z_or_x ;


// Keywords
Always : 'always' ;
Always_comb : 'always_comb' ;
Always_ff : 'always_ff' ;
And : 'and' ;
Assert : 'assert' ;
Assign : 'assign' ;
Automatic : 'automatic' ;
Begin : 'begin' ;
Bit : 'bit' ;
Buf : 'buf' ;
Bufif0 : 'bufif0' ;
Bufif1 : 'bufif1' ;
Byte : 'byte' ;
Case_keyword : 'case' ;
Casez: 'casez' ;
Casex: 'casex' ;
Cell : 'cell' ;
Cmos : 'cmos' ;
Config : 'config' ;
Const : 'const' ;
Deassign : 'deassign' ;
Default : 'default' ;
Default_nettype : '`default_nettype' ;
Defparam : 'defparam' ;
Design : 'design' ;
Disable : 'disable' ;
Do : 'do' ;
Edge : 'edge' ;
Else : 'else' ;
End : 'end' ;
Endcase : 'endcase' ;
Endconfig : 'endconfig' ;
Endfunction : 'endfunction' ;
Endgenerate : 'endgenerate' ;
Endmodule: 'endmodule' ;
Endpackage : 'endpackage' ;
Endproperty : 'endproperty' ;
Endspecify : 'endspecify' ;
Endtask : 'endtask' ;
Enum : 'enum' ;
Event_keyword : 'event' ;
Final : 'final' ;
For : 'for' ;
Force : 'force' ;
Forever : 'forever' ;
Fork : 'fork' ;
Function : 'function' ;
Generate : 'generate' ;
Genvar : 'genvar' ;
Highz0 : 'highz0' ;
Highz1 : 'highz1' ;
If : 'if' ;
Iff : 'iff' ;
Ifnone : 'ifnone' ;
Import : 'import' ;
Incdir : '-incdir' ;
Initial : 'initial' ;
Inout : 'inout' ;
Input : 'input' ;
Instance : 'instance' ;
Int : 'int' ;
Integer : 'integer' ;
Join : 'join' ;
Join_any : 'join_any' ;
Join_none : 'join_none' ;
Large : 'large' ;
Liblist : 'liblist' ;
Library : '`library' ;
Localparam : 'localparam' ;
Logic : 'logic' ;
Macromodule : 'macromodule' ;
Medium : 'medium' ;
Module_keyword_only : 'module' ;
Nand : 'nand' ;
Negedge : 'negedge' ;
Nmos : 'nmos' ;
NONE : 'none' ;
Nor : 'nor' ;
Not : 'not' ;
Notif0 : 'notif0' ;
Notif1 : 'notif1' ;
Noshowcancelled : 'noshowcancelled' ;
Or : 'or ' ;
Output : 'output' ;
Parameter : 'parameter' ;
Path_pulse_dollar : 'PATHPULSE$' ;
Posedge : 'posedge' ;
Package : 'package' ;
Packed : 'packed' ;
Pmos : 'pmos' ;
Property : 'property' ;
Pull0 : 'pull0' ;
Pull1 : 'pull1' ;
Pullup : 'pullup' ;
Pulldown : 'pulldown' ;
Pulsestyle_ondetect : 'pulsestyle_ondetect' ;
Pulsestyle_onevent : 'pulsestyle_onevent' ;
Rcmos : 'rcmos' ;
Real : 'real' ;
Realtime : 'realtime' ;
Ref : 'ref' ;
Reg : 'reg' ;
Release : 'release' ;
Repeat : 'repeat' ;
Return : 'return' ;
Rnmos : 'rnmos' ;
Rpmos : 'rpmos' ;
Rtran : 'rtran' ;
Rtranif0 : 'rtranif0' ;
Rtranif1 : 'rtranif1' ;
Scalared : 'scalared' ;
Showcancelled : 'showcancelled' ;
Signed : 'signed' ;
Small : 'small' ;
Specify : 'specify' ;
Specparam : 'specparam' ;
Static : 'static' ;
String : 'string' ;
Strong0 : 'strong0' ;
Strong1 : 'strong1' ;
Struct : 'struct' ;
Supply0 : 'supply0' ;
Supply1 : 'supply1' ;
Task : 'task' ;
Tick_timescale : '`timescale' ;
Time : 'time' ;
Timeprecision : 'timeprecision' ;
Timeunit : 'timeunit' ;
Tran : 'tran' ;
Tranif0 : 'tranif0' ;
Tranif1 : 'tranif1' ;
Tri : 'tri' ;
Tri_and : 'triand' ;
Tri_or : 'trior' ;
Tri_reg : 'trireg' ;
Tri0 : 'tri0' ;
Tri1 : 'tri1' ;
Typedef : 'typedef' ;
UnionStruct : 'union' ;
Unsigned : 'unsigned' ;
Use : 'use' ;
Uwire : 'uwire' ;
Vectored : 'vectored' ;
Wait : 'wait' ;
Wand : 'wand' ;
Weak0 : 'weak0' ;
Weak1 : 'weak1' ;
While : 'while' ;
Wire : 'wire' ;
Wor : 'wor' ;
Xnor : 'xnor' ;
Xor : 'xor' ;

module_keyword  :   Module_keyword_only | Macromodule ;
struct_keyword  :   Struct | UnionStruct ;
any_case_keyword    :   Case_keyword | Casez | Casex ;

// literals
fragment
ALPHA : [a-zA-Z_] ;
fragment
DIGIT : [0-9] ;

Dollar_Identifier : '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]* ;
Escaped_identifier : '\\' ~[ \r\t\n]* ;
Simple_identifier : ALPHA (ALPHA | DIGIT)* ;
String_literal : '"' (~('"'|'\n'|'\r') | '""')* '"'  ;


// punctuation
At : '@' ;
Close_parenthesis : ')' ;
Colon : ':' ;
Comma : ',' ;
Dash_right_angle : '->' ;
Dot : '.' ;
Dollar : '$' ;
Double_colon : '::' ;
Equal : '=' ;
Equals_right_angle : '=>' ;
Forward_slash : '/' ;
Hash : '#' ;
Left_angle_equals : '<=' ;
Left_bracket : '[' ;
Left_curly_bracket : '{' ;
Minus_colon : '-:' ;
Open_parenthesis : '(' ;
Plus_colon : '+:' ;
Question_mark : '?' ;
Quote : '\'' ;
Right_bracket : ']' ;
Right_curly_bracket : '}' ;
Semicolon : ';' ;
Star : '*' ;
Star_right_angle : '*>' ;
Tilde : '~' ;

semicolon : ';'
          | ';;'
          | ';' ';'
          ;

//Operators
//polarity_operator : '+' | '-' ;
unary_operator : '+' | '-' | '!' | '~' | '&' | '~&' | '|' | '~|' | '^' | '~^' | '^~' ;
binary_operator  : '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '===' | '!==' | '&&' | '||' | '**' | '<' | '<=' | '>' | '>=' | '&' | '|' | '^' | '^~' | '~^' | '>>' | '<<' | '>>>' | '<<<' ;
//unary_module_path_operator : '!' | '~' | '&' | '~&' | '|' | '~|' | '^' | '~^' | '^~' ;
//binary_module_path_operator : '==' | '!=' | '&&' | '||' | '&' | '|' | '^' | '^~' | '~^' ;
unary_assign_operator : '++' | '--' ;
binary_assign_operator : '+=' | '-=' | '&=' | '|=' ;


//Time
fragment
Time_unit : 's ' | 'ms' | 'us' | 'ns' | 'ps' | ' fs' ;

Time_literal : Decimal_number ' '? Time_unit
             | Fixed_point_number ' '? Time_unit
             ;


//Edge
fragment
Edge_descriptor : '01' | '10' | Z_or_x Zero_or_one | Zero_or_one Z_or_x ;
fragment
Zero_or_one : [01] ;
fragment
Z_or_x : [xXzZ] ;

Edge_control_specifier : Edge Right_bracket Edge_descriptor ( Comma Edge_descriptor )* Left_bracket ;

/**********LEXER**********/
/**********PARSER**********/

//Source Text
source_text :   description_star EOF ;

//Description
description_star    :   ( description )* ;

header_text : compiler_directive
            | design_attribute
            | import_package
            ;

design_attribute    :   attribute_instance ;

compiler_directive  :   timescale_compiler_directive
                    |   default_nettype_statement
                    ;

description :   header_text
            |   package_declaration semicolon?
            |   module_declaration semicolon?
            |   function_declaration semicolon?
            |   enum_declaration semicolon?
            |   typedef_declaration semicolon?
            ;

/**********PARSER**********/
/**********MODULES**********/

module_declaration  :   attribute_instance_star
                        module_keyword module_identifier
                        module_interface
                        semicolon
                        module_item_star
                        Endmodule (colon_module_identifier)?
                    ;

module_identifier   :   identifier ;

module_interface : (module_parameter_interface)? (module_port_interface)? ;

module_parameter_interface : Hash Open_parenthesis (list_of_interface_parameters)? Close_parenthesis ;

module_port_interface : Open_parenthesis (list_of_interface_ports)? Close_parenthesis ;

module_item_star    :   (module_item)*;

module_item :   import_package
            |   parameter_item_semicolon
            |   attr_port_item_semicolon
            //| variable_item
            |   attr_variable_item_semicolon
            |   subroutine_item_semicolon
            |   attr_construct_item
            |   attr_generated_instantiation
            |   attr_component_item
            |   compiler_item
            |   type_item
            //| verification_item
            ;

colon_module_identifier : Colon module_identifier ;

/**********MODULES**********/
/**********PACKAGES**********/

package_declaration :   attribute_instance_star
                        Package package_identifier semicolon
                        package_item_star
                        Endpackage (colon_package_identifier)?
                    ;

package_identifier : identifier ;

colon_package_identifier    :   Colon package_identifier ;

package_item_star   :   (package_item)* ;

package_item    :   import_package
                |   parameter_item_semicolon
                //| attr_port_item_semicolon
                //| variable_item
                |   attr_variable_item_semicolon
                |   subroutine_item_semicolon
                //| attr_construct_item
                //| attr_generated_instantiation
                |   attr_component_item
                |   compiler_item
                |   type_item
                //| verification_item
                ;

import_package  :   Import package_identifier Double_colon Star semicolon
                |   Import package_identifier Double_colon package_item_identifier semicolon
                ;

package_item_identifier : identifier ;

/**********PACKAGES**********/
/**********ITEMS**********/

parameter_item_semicolon    :   parameter_item semicolon ;

parameter_item  :   parameter_declaration
                |   local_parameter_declaration
                |   parameter_override
                ;

attr_port_item_semicolon    :   attribute_instance_star port_declaration semicolon ;
//attr_port_item_semicolon  :   attr_port_declaration_semicolon ;

attr_variable_item_semicolon    :   attribute_instance_star variable_item semicolon ;

variable_item   :   net_declaration
                |   reg_declaration
                |   logic_declaration
                |   bits_declaration
                |   integer_declaration
                |   int_declaration
                |   real_declaration
                |   time_declaration
                |   realtime_declaration
                |   event_declaration
                |   genvar_declaration
                |   usertype_variable_declaration
                |   string_declaration
                |   struct_declaration
                |   enum_declaration
                ;

subroutine_item_semicolon   :   subroutine_item semicolon? ;

subroutine_item :   task_declaration
                |   function_declaration
                ;

attr_construct_item :   attribute_instance_star construct_item ;

construct_item  :   continuous_assign
                |   initial_construct
                |   final_construct
                |   always_construct
                ;

attr_component_item :   attribute_instance_star component_item ;

component_item  :   module_instantiation
                |   gate_instantiation
                ;

compiler_item   :   timescale_compiler_directive
                |   timeunit_directive semicolon
                |   timeprecision_directive semicolon
                ;

type_item   :   default_nettype_statement
            |   typedef_declaration semicolon
            ;

//verification_item :   assertion_property_block
//                  |   specify_block
//                  |   specparam_declaration
//                  |   property_block
//                  ;

null_item   :   semicolon ;

/**********ITEMS**********/
/**********PARAMETERS**********/

list_of_interface_parameters    :   list_of_parameter_declarations
                                |   list_of_parameter_descriptions
                                ;

list_of_parameter_declarations  :   parameter_declaration comma_parameter_declaration_star ;

comma_parameter_declaration_star    :   (  comma_parameter_declaration )*;

comma_parameter_declaration :   Comma parameter_declaration ;

list_of_parameter_descriptions  :   list_of_variable_descriptions ;

param_declaration   :   (Signed | Unsigned)? (dimension_plus)? list_of_hierarchical_variable_descriptions ;

param_description   :   param_declaration
                    //| net_declaration
                    //| reg_declaration
                    |   logic_declaration
                    //| bits_declaration
                    |   integer_declaration
                    |   int_declaration
                    |   real_declaration
                    |   time_declaration
                    |   realtime_declaration
                    //| event_declaration
                    //| genvar_declaration
                    |   usertype_variable_declaration
                    |   string_declaration
                    //| struct_declaration
                    //| enum_declaration
                    ;

parameter_declaration   :   Parameter param_description ;

local_parameter_declaration :   Localparam param_description ;

parameter_override : Defparam param_description ;

/**********PARAMETERS**********/
/**********PORTS**********/

list_of_tf_interface_ports  :   list_of_port_identifiers
                            |   list_of_tf_port_declarations
                            ;

list_of_tf_port_declarations    :   list_of_tf_port_declarations_comma
                                |   list_of_tf_port_declarations_semicolon
                                ;

list_of_tf_port_declarations_comma  :   attr_tf_port_declaration comma_attr_tf_port_declaration_star ;

comma_attr_tf_port_declaration_star :   ( comma_attr_tf_port_declaration )* ;

comma_attr_tf_port_declaration  :   Comma attr_tf_port_declaration ;

list_of_tf_port_declarations_semicolon  :   attr_tf_port_declaration_semicolon_plus ;

attr_tf_port_declaration_semicolon_plus :   (attr_tf_port_declaration_semicolon)+ ;

attr_tf_port_declaration_semicolon_star :   (attr_tf_port_declaration_semicolon)* ;

attr_tf_port_declaration_semicolon  :   attr_tf_port_declaration semicolon ;

attr_tf_port_declaration    :   attribute_instance_star tf_port_declaration ;

tf_port_declaration :   inout_declaration
                    |   input_declaration
                    |   output_declaration
                    |   ref_declaration
                    |   tf_declaration
                    ;

list_of_interface_ports :   list_of_port_identifiers
                        |   list_of_port_declarations
                        ;

list_of_port_identifiers    :   port_identifier comma_port_identifier_star (Comma)?;

comma_port_identifier_star : ( comma_port_identifier )* ;

comma_port_identifier   :   Comma port_identifier ;

port_identifier : identifier ;

list_of_port_declarations   :   list_of_port_declarations_comma
                            |   list_of_port_declarations_semicolon
                            ;

list_of_port_declarations_comma :   attr_port_declaration comma_attr_port_declaration_star ;

comma_attr_port_declaration_star    :   ( comma_attr_port_declaration )* ;

comma_attr_port_declaration :   Comma attr_port_declaration ;

list_of_port_declarations_semicolon :   attr_port_declaration_semicolon_plus ;

attr_port_declaration_semicolon_plus    :   (attr_port_declaration_semicolon)+ ;

attr_port_declaration_semicolon_star    :   (attr_port_declaration_semicolon)* ;

attr_port_declaration_semicolon :   attr_port_declaration semicolon ;

attr_port_declaration   :   attribute_instance_star port_declaration ;

port_declaration    :   inout_declaration
                    |   input_declaration
                    |   output_declaration
                    |   ref_declaration
                    //| tf_declaration
                    ;

port_description    :   (Signed | Unsigned)? (dimension_plus)? list_of_variable_descriptions ;

inout_description   :   port_description
                    |   net_declaration
                    ;

input_description   :   port_description
                    |   net_declaration
                    |   reg_declaration
                    |   logic_declaration
                    |   bits_declaration
                    |   int_declaration
                    |   integer_declaration
                    |   real_declaration
                    |   time_declaration
                    |   usertype_variable_declaration
                    |   string_declaration
                    ;

output_description  :   port_description
                    |   net_declaration
                    |   reg_declaration
                    |   logic_declaration
                    |   integer_declaration
                    |   time_declaration
                    |   usertype_variable_declaration
                    |   string_declaration
                    ;

ref_description :   port_description
                |   net_declaration
                |   reg_declaration
                |   logic_declaration
                |   integer_declaration
                |   time_declaration
                |   usertype_variable_declaration
                |   string_declaration
                ;

tf_declaration  :   port_description
                |   real_declaration
                |   net_declaration
                |   reg_declaration
                |   logic_declaration
                |   bits_declaration
                |   int_declaration
                |   integer_declaration
                |   time_declaration
                |   usertype_variable_declaration
                |   string_declaration
                ;

inout_declaration   :   Inout inout_description ;

input_declaration   :   Input input_description ;

output_declaration  :   Output output_description ;

ref_declaration :   Ref ref_description ;

/**********PORTS**********/
/**********DECLARATIONS**********/

user_type   :   user_type_identifer;

user_type_identifer : identifier ;

dimension_plus  :   ( dimension )+;

dimension_star  :   ( dimension )* ;

dimension   :   Left_bracket range_expression Right_bracket ;

range_expression    :   index_expression
                    |   sb_range
                    |   base_increment_range
                    |   base_decrement_range
                    ;

index_expression    :   expression
                    |   Dollar
                    |   Star
                    ;

sb_range : base_expression Colon expression ;

base_increment_range : base_expression Plus_colon expression ;

base_decrement_range : base_expression Minus_colon expression ;

base_expression : expression ;


net_type    :   Supply0
            |   Supply1
            |   Tri
            |   Tri_and
            |   Tri_or
            |   Tri_reg
            |   Tri0
            |   Tri1
            |   Uwire
            |   Wire
            |   Wand
            |   Wor
            |   NONE
            ;

drive_strength  :   Open_parenthesis drive_strength_value_0 Comma drive_strength_value_1 Close_parenthesis ;

drive_strength_value_0  :   strength0
                        |   strength1
                        |   highz0
                        |   highz1
                        ;

drive_strength_value_1  :   strength0
                        |   strength1
                        |   highz0
                        |   highz1
                        ;

strength0   :   Supply0
            |   Strong0
            |   Pull0
            |   Weak0
            ;

strength1   :   Supply1
            |   Strong1
            |   Pull1
            |   Weak1
            ;

highz0  :   Highz0 ;

highz1  :   Highz1 ;

charge_strength :   Open_parenthesis charge_size Close_parenthesis ;

charge_size :   Small
            |   Medium
            |   Large
            ;

list_of_variable_descriptions   :   variable_description comma_variable_description_star ;

comma_variable_description_star :   (comma_variable_description)* ;

comma_variable_description  :   Comma variable_description ;

variable_description    :   variable_identifier (dimension_plus)? ( Equal expression )? ;

variable_identifier : identifier ;

list_of_hierarchical_variable_descriptions  :   hierarchical_variable_description comma_hierarchical_variable_description_star ;

comma_hierarchical_variable_description_star    :   (comma_hierarchical_variable_description)* ;

comma_hierarchical_variable_description :   Comma hierarchical_variable_description ;

hierarchical_variable_description   :   hierarchical_variable_identifier (dimension_plus)? ( Equal expression )? ;

hierarchical_variable_identifier    :   hierarchical_identifier ;

net_declaration : net_type (user_type)? (drive_strength)? (charge_strength)? (Vectored | Scalared)? (Signed | Unsigned)? (dimension_plus)? (delay)? list_of_variable_descriptions ;

reg_declaration : Reg (Signed | Unsigned)? (dimension_plus)? list_of_variable_descriptions ;

logic_declaration   :   Logic (Signed | Unsigned)? (dimension_plus)? list_of_variable_descriptions ;

bits_type   :   Bit
            |   Byte
            ;

bits_declaration    :   bits_type (Signed | Unsigned)? (dimension_plus)? list_of_variable_descriptions ;

integer_declaration :   (Automatic)? Integer (Signed | Unsigned)? list_of_variable_descriptions ;

int_declaration :   (Automatic | Static | Const)? Int (Signed | Unsigned)? list_of_variable_descriptions ;

real_declaration    :   Real list_of_variable_descriptions ;

time_declaration    :   Time list_of_variable_descriptions ;

realtime_declaration    :   Realtime list_of_variable_descriptions ;

event_declaration   :   Event_keyword list_of_variable_descriptions ;

genvar_declaration  :   Genvar list_of_variable_descriptions ;

usertype_variable_declaration   : (Automatic)? user_type (dimension)? list_of_variable_descriptions ;

string_declaration  :   String list_of_variable_descriptions ;

struct_declaration  :   struct_type list_of_variable_descriptions ;

enum_declaration    :   enumerated_type list_of_variable_descriptions ;

/**********DECLARATIONS**********/
/**********FUNCTIONS**********/

function_declaration    :   Function (Automatic)? (Signed | Unsigned)? (function_type)? (dimension)? function_identifier
                            (function_interface)? semicolon
                            function_item_declaration_star function_statement
                            Endfunction (colon_function_identifier)?
                        ;

function_type   : Logic
                | Integer
                | Int
                | Real
                | Realtime
                | Time
                | Reg
                | String
                | bits_type
                | user_type
                ;

function_identifier :   identifier ;

function_interface : Open_parenthesis (list_of_tf_interface_ports)? Close_parenthesis ;

function_item_declaration_star : ( function_item_declaration_semicolon )* ;

function_item_declaration_semicolon :   function_item_declaration semicolon ;

function_item_declaration   :   block_item_declaration
                            |   port_declaration
                            ;

function_statement  :   statement_star ;

colon_function_identifier   :   Colon function_identifier ;

/**********FUNCTIONS**********/
/**********TASKS**********/

task_declaration    :   Task (Automatic)? task_identifier
                        (task_interface)? semicolon
                        task_item_declaration_star task_statement
                        Endtask
                    ;

task_identifier :   identifier ;

task_interface : Open_parenthesis (list_of_tf_interface_ports)? Close_parenthesis ;

task_item_declaration_semicolon :   task_item_declaration semicolon ;

task_item_declaration   :   block_item_declaration
                        |   port_declaration
                        ;

task_item_declaration_star : (  task_item_declaration_semicolon )* ;

task_statement : statement_star ;

/**********TASKS**********/
/**********STRUCTS**********/

struct_item_semicolon   :   struct_item semicolon ;

struct_item_star    :   (struct_item_semicolon)* ;

struct_item : logic_declaration
            | bits_declaration
            | int_declaration
            | integer_declaration
            | usertype_variable_declaration
            | time_declaration
            ;

struct_type : struct_keyword (Packed)? Left_curly_bracket struct_item_star Right_curly_bracket ;

/**********STRUCTS**********/
/**********ENUM**********/

enum_type   : Integer
            | Logic
            | bits_type
            | Int
            ;

list_of_enum_items : enum_item comma_enum_item_star ;

enum_item   : enum_identifier
            | enum_identifier Equal expression
            ;

enum_identifier : identifier ;

comma_enum_item_star : (comma_enum_item)* ;

comma_enum_item : Comma enum_item ;

enumerated_type : Enum (enum_type)? (Signed | Unsigned)? (dimension)? Left_curly_bracket list_of_enum_items Right_curly_bracket ;

/**********ENUM**********/
/**********MODULE INST**********/

module_instantiation    :   module_identifier (parameter_interface_assignments)? list_of_module_instances semicolon ;

parameter_interface_assignments :   Hash Open_parenthesis (list_of_interface_assignments)? Close_parenthesis ;

list_of_interface_assignments   :   list_of_ordered_interface_assignments
                                |   list_of_named_interface_assignments
                                ;

list_of_ordered_interface_assignments : ordered_interface_assignment comma_ordered_interface_assignment_star ;

comma_ordered_interface_assignment_star     :   (comma_ordered_interface_assignment)* ;

comma_ordered_interface_assignment  :   Comma (ordered_interface_assignment)? ;

ordered_interface_assignment : expression ;

list_of_named_interface_assignments : named_interface_assignment comma_named_interface_assignment_star ;

comma_named_interface_assignment_star   :   (comma_named_interface_assignment)* ;

comma_named_interface_assignment    :   Comma named_interface_assignment ;

named_interface_assignment  :   Dot identifier (Open_parenthesis (expression)? Close_parenthesis)?
                            |   Dot Star;

list_of_module_instances    :   module_instance comma_module_instance_star ;

comma_module_instance_star  :   ( comma_module_instance )* ;

comma_module_instance   :   Comma module_instance ;

module_instance :   module_instance_identifier (port_interface_assignments)? ;

module_instance_identifier : arrayed_identifier ;

arrayed_identifier  : simple_arrayed_identifier | escaped_arrayed_identifier ;

simple_arrayed_identifier : Simple_identifier (dimension)? ;

escaped_arrayed_identifier : Escaped_identifier (dimension)? ;

port_interface_assignments  :   Open_parenthesis (list_of_interface_assignments)? Close_parenthesis ;

/**********MODULE INST**********/
/**********GATE INST**********/

delay   :   Hash delay_value
        |   Hash Open_parenthesis list_of_delay_values Close_parenthesis
        ;

list_of_delay_values    :   delay_value comma_delay_value_star ;

comma_delay_value_star  :   (comma_delay_value)* ;

comma_delay_value   :   Comma delay_value ;

delay_value :   expression ;

pulldown_strength   :   Open_parenthesis strength0 Comma strength1 Close_parenthesis
                    |   Open_parenthesis strength1 Comma strength0 Close_parenthesis
                    |   Open_parenthesis strength0 Close_parenthesis
                    ;

pullup_strength :   Open_parenthesis strength0 Comma strength1 Close_parenthesis
                |   Open_parenthesis strength1 Comma strength0 Close_parenthesis
                |   Open_parenthesis strength1 Close_parenthesis
                ;

gate_instance_identifier : arrayed_identifier ;

gate_instantiation  :   cmos_instantiation
                    |   mos_instantiation
                    |   pass_instantiation
                    |   pulldown_instantiation
                    |   pullup_instantiation
                    |   enable_instantiation
                    |   n_input_instantiation
                    |   n_output_instantiation
                    |   pass_enable_instantiation
                    ;

enable_gatetype         :   Bufif0 | Bufif1 | Notif0 | Notif1 ;
mos_switchtype          :   Nmos | Pmos | Rnmos | Rpmos ;
cmos_switchtype         :   Cmos | Rcmos ;
n_output_gatetype       :   Buf | Not ;
n_input_gatetype        :   And | Nand | Or | Nor | Xor | Xnor ;
pass_switchtype         :   Tran | Rtran ;
pass_enable_switchtype  :   Tranif0 | Tranif1 | Rtranif1 | Rtranif0 ;

pulldown_instantiation      :   Pulldown                ( pulldown_strength )?          list_of_pull_gate_instance              semicolon ;
pullup_instantiation        :   Pullup                  ( pullup_strength )?            list_of_pull_gate_instance              semicolon ;
enable_instantiation        :   enable_gatetype         ( drive_strength )? ( delay )?  list_of_enable_gate_instance            semicolon ;
mos_instantiation           :   mos_switchtype          ( delay )?                      list_of_mos_switch_instance             semicolon ;
cmos_instantiation          :   cmos_switchtype         ( delay )?                      list_of_cmos_switch_instance            semicolon ;
n_output_instantiation      :   n_output_gatetype       ( drive_strength )? ( delay )?  list_of_n_output_gate_instance          semicolon ;
n_input_instantiation       :   n_input_gatetype        ( drive_strength )? ( delay )?  list_of_n_input_gate_instance           semicolon ;
pass_instantiation          :   pass_switchtype                                         list_of_pass_switch_instance            semicolon ;
pass_enable_instantiation   :   pass_enable_switchtype  ( delay )?                      list_of_pass_enable_switch_instance     semicolon ;

list_of_pull_gate_instance          :   pull_gate_instance              comma_pull_gate_instance_star ;
list_of_enable_gate_instance        :   enable_gate_instance            comma_enable_gate_instance_star ;
list_of_mos_switch_instance         :   mos_switch_instance             comma_mos_switch_instance_star ;
list_of_cmos_switch_instance        :   cmos_switch_instance            comma_cmos_switch_instance_star ;
list_of_n_input_gate_instance       :   n_input_gate_instance           comma_n_input_gate_instance_star ;
list_of_n_output_gate_instance      :   n_output_gate_instance          comma_n_output_gate_instance_star ;
list_of_pass_switch_instance        :   pass_switch_instance            comma_pass_switch_instance_star ;
list_of_pass_enable_switch_instance :   pass_enable_switch_instance     comma_pass_enable_switch_instance_star ;

comma_pull_gate_instance_star           :   (comma_pull_gate_instance)* ;
comma_enable_gate_instance_star         :   (comma_enable_gate_instance)* ;
comma_mos_switch_instance_star          :   (comma_mos_switch_instance)* ;
comma_cmos_switch_instance_star         :   (comma_cmos_switch_instance)* ;
comma_n_input_gate_instance_star        :   (comma_n_input_gate_instance)* ;
comma_n_output_gate_instance_star       :   (comma_n_output_gate_instance)* ;
comma_pass_switch_instance_star         :   (comma_pass_switch_instance)* ;
comma_pass_enable_switch_instance_star  :   (comma_pass_enable_switch_instance)* ;

comma_pull_gate_instance            :   Comma   pull_gate_instance ;
comma_enable_gate_instance          :   Comma   enable_gate_instance ;
comma_mos_switch_instance           :   Comma   mos_switch_instance ;
comma_cmos_switch_instance          :   Comma   cmos_switch_instance ;
comma_n_input_gate_instance         :   Comma   n_input_gate_instance ;
comma_n_output_gate_instance        :   Comma   n_output_gate_instance ;
comma_pass_switch_instance          :   Comma   pass_switch_instance ;
comma_pass_enable_switch_instance   :   Comma   pass_enable_switch_instance ;

pull_gate_instance              :   ( gate_instance_identifier )?   pull_gate_interface ;
enable_gate_instance            :   ( gate_instance_identifier )?   enable_gate_interface ;
mos_switch_instance             :   ( gate_instance_identifier )?   mos_switch_interface ;
cmos_switch_instance            :   ( gate_instance_identifier )?   cmos_switch_interface ;
n_input_gate_instance           :   ( gate_instance_identifier )?   n_input_gate_interface ;
n_output_gate_instance          :   ( gate_instance_identifier )?   n_output_gate_interface ;
pass_switch_instance            :   ( gate_instance_identifier )?   pass_switch_interface ;
pass_enable_switch_instance     :   ( gate_instance_identifier )?   pass_enable_switch_interface ;

pull_gate_interface             :   Open_parenthesis    output_terminal             Close_parenthesis ;
enable_gate_interface           :   Open_parenthesis    output_terminal             Comma   input_terminal          Comma   enable_terminal Close_parenthesis ;
mos_switch_interface            :   Open_parenthesis    output_terminal             Comma   input_terminal          Comma   enable_terminal Close_parenthesis ;
cmos_switch_interface           :   Open_parenthesis    output_terminal             Comma   input_terminal          Comma   ncontrol_terminal Comma pcontrol_terminal   Close_parenthesis ;
n_input_gate_interface          :   Open_parenthesis    output_terminal             Comma   list_of_input_terminals Close_parenthesis ;
n_output_gate_interface         :   Open_parenthesis    list_of_output_terminals    Comma   input_terminal          Close_parenthesis ;
pass_switch_interface           :   Open_parenthesis    inout_terminal              Comma   inout_terminal          Close_parenthesis ;
pass_enable_switch_interface    :   Open_parenthesis    inout_terminal              Comma   inout_terminal          Comma   enable_terminal Close_parenthesis ;

list_of_input_terminals     :   input_terminal comma_input_terminal_star ;
list_of_output_terminals    :   output_terminal comma_output_terminal_star ;

comma_input_terminal_star   :   (comma_input_terminal)* ;
comma_output_terminal_star  :   (comma_output_terminal)* ;

comma_input_terminal    :   Comma input_terminal ;
comma_output_terminal   :   Comma output_terminal ;

enable_terminal     :   expression ;
input_terminal      :   expression ;
inout_terminal      :   expression ;
ncontrol_terminal   :   expression ;
output_terminal     :   expression ;
pcontrol_terminal   :   expression ;

/**********GATE INST**********/
/**********STATEMENT**********/

statement_star : (  statement_semicolon )* ;

statement_semicolon :   attribute_instance_star statement semicolon?
                    |   null_statement
                    ;

statement   :   assignment_statement
            |   flow_control_statement
            |   block_statement
            |   task_call_statement
            |   event_statement
            |   procedural_statement
            |   expression_statement
            |   subroutine_statement
            ;

assignment_statement    :   blocking_assignment
                        |   nonblocking_assignment
                        |   prefix_assignment
                        |   postfix_assignment
                        |   operator_assignment
                        |   declarative_assignment
                        ;


flow_control_statement  :   case_statement
                        |   conditional_statement
                        |   loop_statement
                        ;

block_statement :   par_block
                |   seq_block
                ;

task_call_statement :   task_enable
                    |   system_task_enable
                    |   disable_statement
                    ;


event_statement :   event_trigger
                |   wait_statement
                ;

procedural_statement    :   procedural_continuous_assignments
                        |   procedural_timing_control_statement
                        |   procedural_assertion_statement
                        |   property_statement
                        ;

expression_statement    :   expression ;

subroutine_statement    :   return_statement ;

return_statement    : Return expression
                    | Return
                    ;

null_statement  :   semicolon ;

/**********STATEMENT**********/
/**********PROCEDURAL**********/

procedural_continuous_assignments : assign_statement
                                  | deassign_statement
                                  | force_statement
                                  | release_statement
                                  ;

assign_statement    :   Assign assignment_statement ;

deassign_statement  :   Deassign variable_lvalue ;

force_statement :   Force assignment_statement ;

release_statement   :   Release variable_lvalue ;

procedural_timing_control_statement : delay_or_event_control statement_semicolon ;

property_statement  : disable_condition_statement ;

disable_condition_statement : Disable Iff Open_parenthesis expression Close_parenthesis property_expression ;

property_expression : expression ;

procedural_assertion_statement : assert_statement ( assert_else_statement )? ;

assert_else_statement : Else statement ;

assert_statement : (hierarchical_identifier Colon)? Assert Open_parenthesis expression Close_parenthesis ;

/**********PROCEDURAL**********/
/**********TASKENABLE**********/

system_task_enable  :   system_task_identifier ( task_interface_assignments )? ;

system_task_identifier : Dollar_Identifier ;

task_interface_assignments  :   Open_parenthesis (list_of_interface_assignments)? Close_parenthesis ;

task_enable :   hierarchical_task_identifier ( task_interface_assignments )?  ;

hierarchical_task_identifier : hierarchical_identifier ;

disable_statement : Disable hierarchical_task_identifier
                  | Disable hierarchical_block_identifier
                  ;

hierarchical_block_identifier : hierarchical_identifier ;

/**********TASKENABLE**********/
/**********ASSIGNMENTS**********/

variable_lvalue :   hierarchical_variable_lvalue
                |   variable_concatenation
                ;

hierarchical_variable_lvalue    :   primary_hierarchical_identifier ;

variable_concatenation  :   Left_curly_bracket variable_concatenation_value comma_vcv_star Right_curly_bracket ;

variable_concatenation_value    :   primary_hierarchical_identifier
                                |   variable_concatenation
                                ;

comma_vcv_star : ( Comma variable_concatenation_value )* ;

blocking_assignment : variable_lvalue Equal ( delay_or_event_control )? expression ;

nonblocking_assignment : variable_lvalue Left_angle_equals ( delay_or_event_control )? expression ;

prefix_assignment : unary_assign_operator variable_lvalue;

postfix_assignment : variable_lvalue unary_assign_operator;

operator_assignment : variable_lvalue binary_assign_operator expression ;

declarative_assignment  :   reg_declaration
                        |   logic_declaration
                        |   bits_declaration
                        |   integer_declaration
                        |   int_declaration
                        |   genvar_declaration
                        ;

/**********ASSIGNMENTS**********/
/**********DELAY_EVENT**********/

delay_or_event_control  :   delay_control
                        |   event_control
                        |   repeat_event_control
                        ;

delay_control   :   Hash delay_value
                |   Hash Open_parenthesis delay_value Close_parenthesis
                |   Hash Open_parenthesis mintypmax_expression Close_parenthesis
                ;

event_control : event_control_identifier
              | event_control_expression
              | event_control_wildcard
              ;

event_control_identifier : At event_identifier ;

event_control_expression    : At Open_parenthesis event_expression Close_parenthesis ;

event_expression    :   single_event_expression
                    |   event_expression_or
                    ;

single_event_expression : expression
                        | hierarchical_identifier
                        | event_expression_edgespec expression
                        ;

event_expression_edgespec : Posedge | Negedge ;

event_expression_or :   list_of_event_expression_comma
                    |   list_of_event_expression_or
                    ;

list_of_event_expression_comma  :   single_event_expression comma_event_expression_star ;
comma_event_expression_star :   (comma_event_expression)* ;
comma_event_expression  :   Comma single_event_expression ;

list_of_event_expression_or :   single_event_expression or_event_expression_star ;
or_event_expression_star    :   (or_event_expression)* ;
or_event_expression :   Or single_event_expression ;

event_control_wildcard  :   At Star
                        |   At Open_parenthesis Star Close_parenthesis
                        ;

repeat_event_control : Repeat Open_parenthesis expression Close_parenthesis event_control ;

event_trigger : Dash_right_angle hierarchical_event_identifier ;

hierarchical_event_identifier : hierarchical_identifier ;

event_identifier : identifier ;

wait_statement : Wait Open_parenthesis expression Close_parenthesis statement_semicolon ;

/**********DELAY_EVENT**********/
/**********GENERATES**********/

attr_generated_instantiation    :   attribute_instance_star generated_instantiation ;

generated_instantiation :   Generate generate_item_star Endgenerate semicolon? ;

generate_item_star  :   ( generate_item )* ;

generate_item   :   generate_conditional_statement
                |   generate_case_statement
                |   generate_loop_statement
                |   generate_block
                //| import_package
                |   parameter_item_semicolon
                //| attr_port_item_semicolon
                |   attr_variable_item_semicolon
                |   subroutine_item_semicolon
                |   attr_construct_item
                //| attr_generated_instantiation
                |   attr_component_item
                //| compiler_item
                //| type_item
                //| verification_item
                |   null_item
                ;

generate_block  :   Begin (generate_colon_block_identifier0)? generate_item_star End (generate_colon_block_identifier1)? semicolon? ;

generate_colon_block_identifier0    :   generate_colon_block_identifier ;
generate_colon_block_identifier1    :   generate_colon_block_identifier ;
generate_colon_block_identifier :   Colon generate_block_identifier ;

generate_block_identifier : identifier ;

generate_conditional_statement  : generate_if_statement (generate_else_statement)? ;

generate_if_statement   : If Open_parenthesis conditional_expression Close_parenthesis generate_item ;

generate_else_statement : Else generate_item ;

generate_loop_statement :   generate_forever_loop_statement
                        |   generate_repeat_loop_statement
                        |   generate_while_loop_statement
                        |   generate_do_loop_statement
                        |   generate_for_loop_statement
                        ;

generate_forever_loop_statement :   Forever generate_item ;

generate_repeat_loop_statement  :   Repeat Open_parenthesis loop_terminate_expression Close_parenthesis generate_item ;

generate_while_loop_statement   :   While Open_parenthesis loop_terminate_expression Close_parenthesis generate_item ;

generate_do_loop_statement  :   Do generate_item While Open_parenthesis loop_terminate_expression Close_parenthesis semicolon ;

generate_for_loop_statement :   For Open_parenthesis loop_init_assignment semicolon loop_terminate_expression semicolon (loop_step_assignment)? Close_parenthesis generate_item ;

generate_case_statement :   any_case_keyword Open_parenthesis case_switch Close_parenthesis generate_case_item_star Endcase ;

generate_case_item_star :   ( generate_case_item )* ;

generate_case_item  :   (case_item_key) Colon generate_item
                    |   Default (Colon)? generate_item
                    ;

/**********GENERATES**********/
/**********CONDITIONAL STATEMENT**********/

conditional_statement   : if_statement (else_statement)? ;

if_statement    : If Open_parenthesis conditional_expression Close_parenthesis statement_semicolon ;

else_statement : Else statement_semicolon ;

conditional_expression  :   expression ;

/**********CONDITIONAL STATEMENT**********/
/**********LOOP STATEMENT**********/

loop_statement  :   forever_loop_statement
                |   repeat_loop_statement
                |   while_loop_statement
                |   do_loop_statement
                |   for_loop_statement
                ;

forever_loop_statement  :   Forever statement_semicolon ;

repeat_loop_statement   :   Repeat Open_parenthesis loop_terminate_expression Close_parenthesis statement_semicolon ;

while_loop_statement    :   While Open_parenthesis loop_terminate_expression Close_parenthesis statement_semicolon ;

do_loop_statement   :   Do statement_semicolon While Open_parenthesis loop_terminate_expression Close_parenthesis semicolon ;

for_loop_statement  :   For Open_parenthesis loop_init_assignment semicolon loop_terminate_expression semicolon (loop_step_assignment)? Close_parenthesis statement_semicolon ;

loop_init_assignment    :   declarative_assignment
                        |   blocking_assignment
                        ;

loop_terminate_expression   :   expression ;

loop_step_assignment    :   blocking_assignment
                        |   postfix_assignment
                        |   prefix_assignment
                        |   operator_assignment
                        ;

/**********LOOP STATEMENT**********/
/**********CASE STATEMENT**********/

case_statement  :   any_case_keyword Open_parenthesis case_switch Close_parenthesis case_item_star Endcase ;

case_item_star  :   ( case_item )* ;

case_item   :   (case_item_key) Colon statement_semicolon
            |   Default (Colon)? statement_semicolon
            ;

case_switch :   expression ;

case_item_key   :   case_item_key_expression comma_case_item_key_expression_star ;

case_item_key_expression    :   expression ;
comma_case_item_key_expression  :   Comma case_item_key_expression ;
comma_case_item_key_expression_star :   (comma_case_item_key_expression)* ;

/**********CASE STATEMENT**********/
/**********EXPRESSION**********/

expression  :   unary_expression
            |   unary_post_assign_expression
            |   unary_pre_assign_expression
            |   binary_expression
            |   ternary_expression
            |   mintypmax_expression
            |   single_expression
            ;

single_expression   :   String_literal
                    //| primary_range
                    |   primary
                    |   arrayed_structured_value
                    |   structured_value
                    ;

primary_range   :   primary dimension ;

primary :   number
        |   concatenation
        |   multiple_concatenation
        |   function_call
        |   system_function_call
        |   constant_function_call
        |   imported_function_call
        |   primary_imported_hierarchical_identifier
        |   primary_hierarchical_identifier
        |   type_cast_expression
        |   parenthesis_expression
        ;

unary_expression    :   unary_operator expression ;
unary_post_assign_expression    :   single_expression unary_assign_operator ;
unary_pre_assign_expression :   unary_assign_operator single_expression  ;
binary_expression   :   single_expression binary_operator expression ;
ternary_expression  :   single_expression Question_mark expression Colon expression ;
mintypmax_expression    :   single_expression Colon expression Colon expression ;

structured_value    :   Quote Left_curly_bracket expression (Comma expression)* Right_curly_bracket
                    |   Quote Left_curly_bracket expression Right_curly_bracket
                    |   Left_curly_bracket Right_curly_bracket
                    ;

arrayed_structured_value    :   Quote Left_curly_bracket arrayed_structure_item_plus  Right_curly_bracket
                            ;

arrayed_structure_item  :   Default Colon expression
                        |   hierarchical_identifier Colon expression
                        ;

comma_arrayed_structure_item    :   Comma arrayed_structure_item ;
comma_arrayed_structure_item_star   :   (comma_arrayed_structure_item)* ;

arrayed_structure_item_plus :   arrayed_structure_item comma_arrayed_structure_item_star ;

variable_type_cast  : variable_type Quote expression;
width_type_cast     : number Quote expression;
sign_type_cast      : (Signed | Unsigned) Quote expression;
null_type_cast      : Quote expression;

variable_type   : Int
                | user_type
                //| Logic
                //| Integer
                //| Real
                //| Realtime
                //| Time
                //| Reg
                //| String
                //| bits_type
                ;

type_cast_identifier : identifier ;

type_cast_expression    :   variable_type_cast
                        |   width_type_cast
                        |   sign_type_cast
                        |   null_type_cast
                        ;

function_call : hierarchical_function_identifier attribute_instance_star function_interface_assignments ;

hierarchical_function_identifier : hierarchical_identifier ;

function_interface_assignments  :   Open_parenthesis (list_of_interface_assignments)? Close_parenthesis ;

system_function_call : system_function_identifier ( function_interface_assignments )?  ;

system_function_identifier : Dollar_Identifier ;

constant_function_call : function_call ;

imported_function_call : imported_function_hierarchical_identifier attribute_instance_star function_interface_assignments ;

imported_function_hierarchical_identifier : imported_hierarchical_identifier ;

primary_hierarchical_identifier :   hierarchical_identifier (dimension_plus)? ;

primary_imported_hierarchical_identifier    :   imported_hierarchical_identifier (dimension_plus)? ;

imported_hierarchical_identifier : identifier Double_colon hierarchical_identifier ;

parenthesis_expression  :   Open_parenthesis expression Close_parenthesis ;

concatenation   :   Left_curly_bracket expression comma_expression_star Right_curly_bracket ;

multiple_concatenation  :   Left_curly_bracket expression concatenation Right_curly_bracket ;

comma_expression_plus   :   ( Comma expression )+ ;

comma_expression_star   :   ( Comma expression )* ;

/**********EXPRESSION**********/
/**********TYPEDEF**********/

typedef_declaration :   Typedef typedef_definition typedef_identifier ;

typedef_identifier : identifier ;

typedef_definition  :   typedef_definition_type
                    |   enumerated_type
                    |   struct_type
                    ;

typedef_definition_type :   complex_type
                        |   typedef_type
                        ;

complex_type    : typedef_type (Signed | Unsigned)? (dimension_plus)?
                ;

typedef_type    : Reg
                | Logic
                | bits_type
                | net_type
                | user_type
                ;

/**********TYPEDEF**********/
/**********BLOCK**********/

par_block   :   Fork ( Colon block_identifier )?
                block_item_declaration_star
                statement_star
                join_keyword ( colon_block_identifier )?
            ;
seq_block   :   Begin ( Colon block_identifier )?
                block_item_declaration_star
                statement_star
                End ( colon_block_identifier )?
            ;

block_identifier    :   identifier ;

colon_block_identifier  :   Colon block_identifier ;

block_item_declaration_star : (  block_item_declaration_semicolon )* ;

block_item_declaration_semicolon    :   block_item_declaration semicolon ;

block_item_declaration : reg_declaration
                       | event_declaration
                       | logic_declaration
                       | bits_declaration
                       | integer_declaration
                       | int_declaration
                       | local_parameter_declaration
                       | parameter_declaration
                       | real_declaration
                       | realtime_declaration
                       | time_declaration
                       | string_declaration
                       | usertype_variable_declaration
                       ;

join_keyword : Join | Join_none | Join_any ;

/**********BLOCK**********/
/**********CONSTRUCTS**********/

continuous_assign : Assign ( drive_strength )? ( delay )? list_of_variable_assignments semicolon ;

list_of_variable_assignments : variable_assignment comma_variable_assignment_star ;

comma_variable_assignment_star : ( comma_variable_assignment )* ;

comma_variable_assignment   :   Comma variable_assignment ;

variable_assignment : variable_lvalue Equal expression ;

initial_construct : Initial statement_semicolon ;

final_construct : Final statement_semicolon ;

always_keyword  : Always | Always_comb | Always_ff ;

always_construct : always_keyword statement_semicolon ;

/**********CONSTRUCTS**********/
/**********ATTRIBUTES**********/

attribute_instance_star :   (  attribute_instance )* ;

attribute_instance  :   Open_parenthesis Star attr_spec attr_spec_star Star Close_parenthesis ;

attr_spec_star  :   ( Comma attr_spec )* ;

attr_spec   :   attr_name Equal expression
            |   attr_name
            ;

attr_name : identifier ;

/**********ATTRIBUTES**********/
/**********LISTS**********/
/**********LISTS**********/
/**********IDENTIFIERS**********/

identifier  :   Simple_identifier
            |   Escaped_identifier
            ;

/**********IDENTIFIERS**********/
/**********HIERARCHICAL IDENTIFIERS**********/

hierarchical_identifier :   hierarchical_identifier_branch_item dot_hierarchical_identifier_branch_item_star ;
dot_hierarchical_identifier_branch_item_star    :   (dot_hierarchical_identifier_branch_item)* ;
dot_hierarchical_identifier_branch_item :   Dot hierarchical_identifier_branch_item ;
hierarchical_identifier_branch_item :   identifier (dimension_plus)? ;

/**********HIERARCHICAL IDENTIFIERS**********/
/**********TIME DIRECTIVES**********/

timescale_compiler_directive    :   Tick_timescale Time_literal Forward_slash Time_literal ;

timeunit_directive  :   Timeunit Time_literal ;

timeprecision_directive :   Timeprecision Time_literal ;

/**********TIME DIRECTIVES**********/
/**********NETTYPE DIRECTIVES**********/

default_nettype_statement   :   Default_nettype net_type ;

/**********NETTYPE DIRECTIVES**********/
/**********NUMBERS**********/

number  :   integral_number
        |   real_number
        ;

integral_number :   Decimal_number
                |   Octal_number
                |   Binary_number
                |   Hex_number
                ;

real_number : Fixed_point_number
            | Real_exp_form
            ;

/**********NUMBERS**********/
/**********VERIFICATION**********/

//assertion_property_block : (assert_identifier_colon)? Assert Property Open_parenthesis statement Close_parenthesis (assert_property_statement)? ( assert_else_statement )? (semicolon)? ;

//assert_identifier_colon : assertion_identifier Colon ;

//assert_property_statement : statement ;

//specify_block :   Specify ( specify_item )* Endspecify ;

//specify_item  :   specparam_declaration
//              |   pulsestyle_declaration
//              |   showcancelled_declaration
//              |   path_declaration
//              ;

//specparam_declaration :   Specparam (dimension)? list_of_specparam_assignments ;

//pulsestyle_declaration    :   Pulsestyle_onevent list_of_path_outputs semicolon
//                      |   Pulsestyle_ondetect list_of_path_outputs semicolon
//                      ;

//showcancelled_declaration :   Showcancelled list_of_path_outputs semicolon
//                          |   Noshowcancelled list_of_path_outputs semicolon
//                          ;

//path_declaration  :   simple_path_declaration semicolon
//                  |   edge_sensitive_path_declaration semicolon
//                  |   state_dependent_path_declaration semicolon
//                  ;

//list_of_specparam_assignments :   specparam_assignment ( Comma specparam_assignment )* ;

//state_dependent_path_declaration  :   If Open_parenthesis module_path_expression Close_parenthesis simple_path_declaration
//                                  |   If Open_parenthesis module_path_expression Close_parenthesis edge_sensitive_path_declaration
//                                  |   Ifnone simple_path_declaration
//                                  ;

//edge_sensitive_path_declaration   :   parallel_edge_sensitive_path_description Equal path_delay_value
//                              |   full_edge_sensitive_path_description Equal path_delay_value
//                              ;

//parallel_edge_sensitive_path_description  :   Open_parenthesis ( edge_identifier )? specify_input_terminal_descriptor Equals_right_angle specify_output_terminal_descriptor ( polarity_operator )? Colon data_source_expression Close_parenthesis ;

//simple_path_declaration   :   parallel_path_description Equal path_delay_value
//                      |   full_path_description Equal path_delay_value
//                      ;

//full_path_description : Open_parenthesis list_of_path_inputs ( polarity_operator )? Star_right_angle list_of_path_outputs Close_parenthesis ;

//specparam_assignment  :   specparam_identifier Equal constant_mintypmax_expression
//                      |   pulse_control_specparam
//                      ;

//parallel_path_description :   ( specify_input_terminal_descriptor ( polarity_operator )? Equals_right_angle specify_output_terminal_descriptor ) ;

//path_delay_value  :   list_of_path_delay_expressions
//                  |   Open_parenthesis list_of_path_delay_expressions Close_parenthesis
//                  ;

//full_edge_sensitive_path_description  :   Open_parenthesis ( edge_identifier )? list_of_path_inputs Star_right_angle list_of_path_outputs ( polarity_operator )? Colon data_source_expression Close_parenthesis ;

//list_of_path_outputs  :   specify_output_terminal_descriptor ( Comma specify_output_terminal_descriptor )* ;

//specparam_identifier : identifier ;

//pulse_control_specparam   :   Path_pulse_dollar Equal Open_parenthesis reject_limit_value ( Comma error_limit_value )? Close_parenthesis semicolon
//                        | Path_pulse_dollar specify_input_terminal_descriptor Dollar specify_output_terminal_descriptor Equal
//                          Open_parenthesis reject_limit_value ( Comma error_limit_value )? Close_parenthesis semicolon
//                        ;

//specify_output_terminal_descriptor    :   output_identifier
//                                  |   output_identifier Left_bracket constant_expression Right_bracket
//                                  |   output_identifier Left_bracket range_expression Right_bracket
//                                  ;

//output_identifier :   output_port_identifier
//                  |   inout_port_identifier
//                  ;

//list_of_path_delay_expressions    :   t_path_delay_expression
//                              |   trise_path_delay_expression Comma tfall_path_delay_expression
//                              |   trise_path_delay_expression Comma tfall_path_delay_expression Comma tz_path_delay_expression
//                              |   t01_path_delay_expression Comma t10_path_delay_expression Comma t0z_path_delay_expression Comma tz1_path_delay_expression Comma t1z_path_delay_expression Comma tz0_path_delay_expression
//                              |   t01_path_delay_expression Comma t10_path_delay_expression Comma t0z_path_delay_expression Comma tz1_path_delay_expression Comma t1z_path_delay_expression Comma tz0_path_delay_expression Comma t0x_path_delay_expression Comma tx1_path_delay_expression Comma t1x_path_delay_expression Comma tx0_path_delay_expression Comma txz_path_delay_expression Comma tzx_path_delay_expression
//                              ;

//edge_identifier   :   Posedge | Negedge ;

//list_of_path_inputs : specify_input_terminal_descriptor ( Comma specify_input_terminal_descriptor )* ;

//specify_input_terminal_descriptor :   input_identifier
//                                  |   input_identifier Left_bracket constant_expression Right_bracket
//                                  |   input_identifier Left_bracket range_expression Right_bracket
//                                  ;

//data_source_expression : expression ;

//reject_limit_value : limit_value ;

//error_limit_value : limit_value ;

//output_port_identifier : identifier ;

//input_identifier  :   input_port_identifier
//                  |   inout_port_identifier
//                  ;

//inout_port_identifier : identifier ;

//t_path_delay_expression   :   path_delay_expression ;

//trise_path_delay_expression   :   path_delay_expression ;

//tfall_path_delay_expression   :   path_delay_expression ;

//tz_path_delay_expression  :   path_delay_expression ;

//t01_path_delay_expression :   path_delay_expression ;

//t10_path_delay_expression :   path_delay_expression ;

//t0z_path_delay_expression :   path_delay_expression ;

//tz1_path_delay_expression :   path_delay_expression ;

//t1z_path_delay_expression :   path_delay_expression ;

//tz0_path_delay_expression :   path_delay_expression ;

//t0x_path_delay_expression :   path_delay_expression ;

//tx1_path_delay_expression :   path_delay_expression ;

//t1x_path_delay_expression :   path_delay_expression ;

//tx0_path_delay_expression :   path_delay_expression ;

//txz_path_delay_expression :   path_delay_expression ;

//tzx_path_delay_expression :   path_delay_expression ;

//recursive set of rules
//module_path_primary   :   number
//                    | identifier
//                    | module_path_concatenation
//                    | module_path_multiple_concatenation
//                    | function_call
//                    | system_function_call
//                    | constant_function_call
//                    | Open_parenthesis module_path_mintypmax_expression Close_parenthesis
//                    ;

//module_path_multiple_concatenation    :   Left_curly_bracket constant_expression module_path_concatenation Right_curly_bracket ;

//module_path_concatenation :   Left_curly_bracket module_path_expression ( Comma module_path_expression )* Right_curly_bracket ;

//module_path_mintypmax_expression  :   module_path_expression (Colon module_path_expression Colon module_path_expression)? ;

//module_path_expression    :   ( module_path_primary | unary_module_path_operator attribute_instance_star module_path_primary )
//                          ( binary_module_path_operator attribute_instance_star module_path_expression | Question_mark attribute_instance_star module_path_expression Colon module_path_expression )*
//                      ;

//limit_value : constant_mintypmax_expression ;

//input_port_identifier : identifier ;

//path_delay_expression : constant_mintypmax_expression ;

//constant_mintypmax_expression :   constant_expression
//                              |   constant_expression Colon constant_expression Colon constant_expression
//                              ;

//property_block : Property assertion_identifier semicolon statement Endproperty ;

//assertion_identifier : identifier ;

/**********VERIFICATION**********/
/**********LIBRARY**********/

//library_descriptions  :   library_declaration
//                      |   config_declaration
//                      ;

//library_declaration   :   Library library_identifier file_path_spec  ( Comma file_path_spec )*  ( Incdir file_path_spec ( Comma file_path_spec )* )? semicolon ;

//library_identifier : identifier ;

//file_path_spec : String_literal ;

//config_declaration : Config config_identifier semicolon design_statement ( config_rule_statement )* Endconfig semicolon? ;

//config_identifier : identifier ;

//design_statement : Design ( ( library_identifier Dot )? cell_identifier )* semicolon ;

//config_rule_statement : Default liblist_clause
//                      | inst_clause liblist_clause
//                      | inst_clause use_clause
//                      | cell_clause liblist_clause
//                      | cell_clause use_clause
//                      ;

//liblist_clause : Liblist library_identifier* ;

//inst_clause : Instance inst_name ;

//use_clause : Use ( library_identifier Dot )? cell_identifier ( Colon Config )? ;

//cell_clause : Cell ( library_identifier Dot )? cell_identifier ;

//cell_identifier : identifier ;

//inst_name : topmodule_identifier ( Dot instance_identifier )* ;

//topmodule_identifier : identifier ;

//instance_identifier : identifier ;

/**********LIBRARY**********/
