/*
BSD License

Copyright (c) 2023, Michał Lorek
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

parser grammar PTXParser;

options { tokenVocab=PTXLexer; }

prog
    : version_directive
        target_directive+
        line* EOF
    ;

version_directive
    : VERSION VER
    ;

target_directive
    : TARGET target_list
    ;

target_list
    : target_specifier (',' target_specifier)*
    ;

target_specifier
    : SM_10
    | SM_11
    | SM_12
    | SM_13
    | COMPUTE_10
    | COMPUTE_11
    | MAP_F64_TO_F32
    ;

line
    : directive ';'
    | instruction_template ';'
    | debugging_directive ';'
    | VISIBLE? ENTRY id_ array_spec+ param_list? body
    | FUNC ret_param? id_ reg_list? body
    ;

directive
    : ( CONST
      | GLOBAL
      | LOCAL
      | SHARED) var_decl
    | REG vector_type? built_in_type id_list initializer?
    | SREG vector_type? built_in_type? special_register
    | state_space? STRUCT alignment? id_ ('{' struct_field_list '}' | id_)
    | UNION alignment? id_ '{' struct_field_list '}'
    | SURF
    | DOT_TEX
    | EXTERN id_
    ;

var_decl
    : alignment? vector_type? built_in_type id_list array_spec* initializer?
    ;

special_register
    : TID
    | NTID
    | LANEID
    | WARPID
    | CTAID
    | NCTAID
    | SMID
    | NSMID
    | GRIDID
    | CLOCK
    | PM
    ;

debugging_directive
    : SECTION /*section_type, section_name*/
    | FILE filename=IDENTIFIER
    | LOC line_number=DECIMAL_LITERAL
    | BYTE data_list
    ;

data_list
    : HEXADECIMAL_LITERAL (',' HEXADECIMAL_LITERAL)+
    ;

alignment
    : ALIGN integer_constant
    ;

array_spec
    : '[' (integer_constant | IDENTIFIER)? ']'
    ;

id_list
    : id_ (',' id_)*
    ;

initializer
    : '=' initial_value
    ;

const_value
    : integer_constant
    | floating_point_constant
    // struct
    ;

initial_value
    : ('+' | '-')? const_value
    | '{' initial_value (',' initial_value)* '}'
    ;

instruction_template
    : LABEL? guard? instruction
    ;

ret_param
    : '(' state_space built_in_type id_ ')'
    ;

param_list
    : '(' param_decl (',' param_decl)* ')'
    ;

param_decl
    : PARAM alignment? built_in_type id_ array_spec*
    ;

reg_list
    : '(' reg_decl (',' reg_decl)* ')'
    ;

reg_decl
    : REG built_in_type id_
    ;

body
    : '{' line* '}'
    ;

guard
    : '@' '!'? pred_reg
    ;

pred_reg
    : id_
    ;

data_type
    : built_in_type
    | vector_type
    | vector_type built_in_type
    | id_
    ;

instruction
    : ABS data_type d=operand ',' a=operand
    | ADD rounding_mode? SAT? data_type d=operand ',' a=operand ',' b=operand
    | AND data_type d=operand ',' a=operand ',' b=operand
    | ATOM space '.' op=operation data_type d=operand ',' a=operand ',' b=operand (',' c=operand)?
    | BAR SYNC? d=operand
    | BRA UNI? tgt=id_
    | BRKPT
    | CALL UNI? ('(' r=operand ')' ',')? fn=id_ (',' '(' operand (',' operand)* ')')?
    | CNOT data_type d=operand ',' a=operand
    | COS data_type d=operand ',' a=operand
    | CROSS
    | CVT rounding_mode? SAT? dt=data_type at=data_type d=operand ',' a=operand
    | DIV WIDE? rounding_mode? SAT? data_type d=operand ',' a=operand ',' b=operand
    | DOT
    | EX2 data_type d=operand ',' a=operand
    | EXIT
    | EXTRACT
    | FRC data_type d=operand ',' a=operand
    | INSERT
    | LD space t=data_type d=operand ',' a=operand
    | LD space vec t=data_type d=operand ',' a=operand
    | LG2 data_type d=operand ',' a=operand
    | MAD (HI | LO | WIDE)? rounding_mode? SAT? data_type d=operand ',' a=operand ',' b=operand ',' c=operand
    | MAD24 (HI | LO)? SAT? data_type d=operand ',' a=operand ',' b=operand ',' c=operand
    | MAX data_type d=operand ',' a=operand ',' b=operand
    | MEMBAR
    | MIN data_type d=operand ',' a=operand ',' b=operand
    | MOV data_type d=operand ',' a=operand
    | MUL (HI | LO | WIDE)? rounding_mode? SAT? data_type d=operand ',' a=operand ',' b=operand
    | MUL24 (HI | LO)? data_type d=operand ',' a=operand ',' b=operand
    | NEG data_type d=operand ',' a=operand
    | NOP
    | NOT data_type d=operand ',' a=operand
    | OR data_type d=operand ',' a=operand ',' b=operand
    | RCP data_type d=operand ',' a=operand
    | REM WIDE? data_type d=operand ',' a=operand ',' b=operand
    | RET UNI?
    | RSQRT data_type d=operand ',' a=operand
    | SAD rounding_mode? data_type d=operand ',' a=operand ',' b=operand ',' c=operand
    | SELP data_type d=operand ',' a=operand ',' b=operand ',' c=operand
    | SET cmp_op dt=data_type st=data_type d=operand ',' a=operand ',' b=operand
    | SET cmp_op bool_op dt=data_type st=data_type d=operand ',' a=operand ',' b=operand ',' '!'? c=operand
    | SETP cmp_op data_type p=operand ('|' q=operand)? ',' a=operand ',' b=operand
    | SETP cmp_op bool_op data_type p=operand ('|' q=operand)? ',' a=operand ',' b=operand ',' '!'? c=operand
    | SHL data_type d=operand ',' a=operand ',' b=operand
    | SHR data_type d=operand ',' a=operand ',' b=operand
    | SIN data_type d=operand ',' a=operand
    | SLCT dt=data_type ct=data_type d=operand ',' a=operand ',' b=operand ',' c=operand
    | SQRT data_type d=operand ',' a=operand
    | ST space t=data_type d=operand ',' a=operand
    | ST space vec t=data_type d=operand ',' a=operand
    | SUB rounding_mode? SAT? data_type d=operand ',' a=operand ',' b=operand
    | TEX geom dt=data_type bt=data_type d=operand ',' a=operand ',' b=operand
    | TRAP
    | VOTE
    | VRED
    | XOR data_type d=operand ',' a=operand ',' b=operand
    | '{' instruction_list?  '}'
    ;

instruction_list
    : (instruction_template ';')+
    ;

operation
    : AND
    | OR
    | XOR
    | INC
    | DEC
    | ADD
    | MIN
    | MAX
    | CAS //compare and swap
    | EXCH //exchange
    ;

rounding_mode
    : floating_point_rounding_mode
    | integer_rounding_mode
    ;

floating_point_rounding_mode
    : RN
    | RZ
    | RM
    | RP
    ;

integer_rounding_mode
    : RNI
    | RZI
    | RMI
    | RPI
    ;

cmp_op
    : EQ
    | NE
    | LT
    | LE
    | GT
    | GE
    | LO
    | LS
    | HI
    | HS
    | EQU
    | NEU
    | LTU
    | LEU
    | GTU
    | GEU
    | NUM
    | NAN
    ;

bool_op
    : DOT_AND
    | DOT_OR
    | DOT_XOR
    ;

operand
    : id_
    | ('+' | '-')? const_value
    | '{' id_list '}'
    | '[' addr_expr ']'
    | id_ '[' arr_ix ']'
    | id_ '.' id_
    ;

arr_ix
    : integer_constant
    | id_
    | id_ ('+' | '-') integer_constant
    ;

addr_expr
    : integer_constant
    | id_
    | id_ '+' '-'? integer_constant
    ;

geom
    : G1D
    | G2D
    | G3D
    ;

space
    : CONST
    | GLOBAL
    | LOCAL
    | PARAM
    | SHARED
    ;

vec
    : V2
    | V3
    | V4
    ;

state_space
    : REG
    | SREG
    | CONST
    | GLOBAL
    | LOCAL
    | PARAM
    | SHARED
    | SURF
    | DOT_TEX
    ;

struct_field_list
    : (field_decl ';')*
    ;

field_decl
    : built_in_type id_list
    ;

built_in_type
    : S8
    | S16
    | S32
    | S64
    | U8
    | U16
    | U32
    | U64
    | F16
    | F32
    | F64
    | B8
    | B16
    | B32
    | B64
    | PRED
    ;

vector_type
    : V2
    | V3
    | V4
    ;

id_
    : IDENTIFIER
    | PARAM_VAR_NAME
    | special_register
    ;

integer_constant
    : HEXADECIMAL_LITERAL
    | OCTAL_LITERAL
    | BINARY_LITERAL
    | DECIMAL_LITERAL
    ;

floating_point_constant
    : SINGLE_PRECISION_FLOATING_POINT_LITERAL
    | DOUBLE_PRECISION_FLOATING_POINT_LITERAL
    | REAL_LITERAL
    ;
