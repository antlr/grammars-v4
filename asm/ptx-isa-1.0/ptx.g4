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

grammar ptx;

prog
   : version_directive ';'
     (target_directive ';')+
        (line ';')+ EOF
   ;

version_directive
    : VERSION VER//major=DECIMAL_LITERAL '.' minor=DECIMAL_LITERAL
    ;

target_directive
    : TARGET target_list
    ;

line
   : directive
   | instruction
   ;

directive
    : CONST alignment? vector_type? built_in_type id_ array_spec* initializer?
    | ENTRY id_ param_list? body
    | FUNC ret_param? id_ reg_list? body
    | GLOBAL alignment? vector_type? built_in_type id_ array_spec* initializer?
    | LOCAL alignment? vector_type? built_in_type id_ array_spec* initializer?
    | REG vector_type? built_in_type id_list initializer?
    | SHARED alignment? vector_type? built_in_type? id_list array_spec* initializer?
    | SREG vector_type? built_in_type? special_register //components
    | STRUCT alignment? id_ '{' struct_field_list '}'
    | SURF
    | TEX
    | UNION alignment? id_ '{' struct_field_list '}'
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

performance_tuning_directive
    : MAXNREG DECIMAL_LITERAL //entry
    | MAXNTID DECIMAL_LITERAL ((',' DECIMAL_LITERAL)? ',' DECIMAL_LITERAL)? //entry
    | MAXNCATPERSM (DECIMAL_LITERAL | target_list) //entry
    ;

target_list
    : target_specifier (',' target_specifier)* // ,,,
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

debugging_directive
    : SECTION //section_type, section_name
    | FILE filename=IDENTIFIER
    | LOC line_number=DECIMAL_LITERAL
    | BYTE data_list
    ;

data_list
    : HEXADECIMAL_LITERAL (',' HEXADECIMAL_LITERAL)+
    ;

other_directives
    : EXTERN id_
    | VISIBLE id_ //entry, func
    ;

alignment
    : ALIGN integer_constant
    ;

array_spec
    : '[' (integer_constant|IDENTIFIER)? ']'
    ;

id_list
    : id_ (',' id_)*
    ;

initializer
    : '=' (expr | '{' '}')
    ;

instruction
    : lbl? guard? opcode operands*
    ;

lbl
    : id_
    ;

ret_param
    : '(' state_space built_in_type id_ ')'
    ;

param_list
    : '(' param_decl (',' param_decl)* ')'
    ;

param_decl
    : PARAM alignment? built_in_type id_
    ;

reg_list
    : '(' reg_decl (',' reg_decl)* ')'
    ;

reg_decl
    : REG built_in_type id_
    ;

body
    : '{' (line ';')* '}'
    ;

label_name
    : LABEL
    ;

guard
    : '@' '!'? pred_reg
    ;

pred_reg
    :
    ;

opcode
    : ABS
    | ADD
    | AND
    | ATOM
    | BAR
    | BRA
    | BRKPT
    | CALL
    | CNOT
    | COS
    | CROSS
    | CVT
    | DIV
    | DOT
    | EX2
    | EXIT
    | EXTRACT
    | FRA
    | INSERT
    | LD
    | LG2
    | MAD
    | MAD24
    | MAX
    | MEMBAR
    | MIN
    | MOV
    | MUL
    | MUL24
    | NEG
    | NOP
    | NOT
    | OR
    | RCP
    | REM
    | RET
    | RSQRT
    | SAD
    | SELP
    | SHL
    | SHR
    | SIN
    | SLCT
    | SQRT
    | ST
    | SUB
    | TEX
    | TRAP
    | VOTE
    | VRED
    | XOR
    ;

operands
    : reg_var
    | const_expr
    | addr_expr
    | label_name2
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
    | TEX
    ;

todo
    : ';'
    ;

reg_var
    : todo
    ;

const_expr
    : todo
    ;

addr_expr
    : todo
    ;

expr
    : IDENTIFIER
    | integer_constant
    | floating_point_constant
    | '(' expr ')'
    | expr ('*' | '/') expr
    | expr ('+' | '-') expr
    | expr ('&' | '|' | '^') expr
    ;

label_name2
    : todo
    ;

struct_field_list
    : (field_decl ';')*
    ;

field_decl
    : built_in_type id_list
    ;

var_decl
    : state_space built_in_type var_id_list ';'
    ;

var_id_list
    :
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
    : V4
    | V2
    ;

id_
    : IDENTIFIER
    | PARAM_VAR_NAME
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
    ;

//------------------
ALIGN          : '.align';
BYTE           : '.byte';
CONST          : '.const';
ENTRY          : '.entry';
EXTERN         : '.extern';
FILE           : '.file';
FUNC           : '.func';
GLOBAL         : '.global';
LOCAL          : '.local';
LOC            : '.loc';
MAXNCATPERSM   : '.maxncatpersm';
MAXNREG        : '.maxnreg';
MAXNTID        : '.maxntid';
PARAM          : '.param';
REG            : '.reg';
SECTION        : '.section';
SHARED         : '.shared';
SREG           : '.sreg';
STRUCT         : '.struct';
SURF           : '.surf';
TARGET         : '.target';
TEX            : '.tex';
UNION          : '.union';
VERSION        : '.version';
VISIBLE        : '.visible';

//---------------------
S8             : '.s8';
S16            : '.s16';
S32            : '.s32';
S64            : '.s64';
U8             : '.u8';
U16            : '.u16';
U32            : '.u32';
U64            : '.u64';
F16            : '.f16';
F32            : '.f32';
F64            : '.f64';
B8             : '.b8';
B16            : '.b16';
B32            : '.b32';
B64            : '.b64';
PRED           : '.pred';
V2             : '.v2';
V4             : '.v4';

//ADDRESS_SIZE : '.address_size';

SM_10      : 'sm_10';
SM_11      : 'sm_11';
SM_12      : 'sm_12';
SM_13      : 'sm_13';
COMPUTE_10 : 'compute_10';
COMPUTE_11 : 'compute_11';

// texturing mode
TEXMODE_UNIFIED: 'texmode_unified';
TEXMODE_INDEPENDENT: 'texmode_independent';
// platform option
DEBUG: 'debug';
MAP_F64_TO_F32: 'map_f64_to_f32';

// instruction keyword
ABS           : 'abs';
ADD           : 'add';
ADDC          : 'addc';
AND           : 'and';
ATOM          : 'atom';
BAR           : 'bar';
BRA           : 'bra';
BRKPT         : 'brkpt';
CALL          : 'call';
CNOT          : 'cnot';
COS           : 'cos';
CROSS         : 'cross';
CVT           : 'cvt';
DIV           : 'div';
DOT           : 'dot';
EX2           : 'ex2';
EXIT          : 'exit';
EXTRACT       : 'extract';
FMA           : 'fma';
FRA           : 'fra';
INSERT        : 'insert';
LD            : 'ld';
LG2           : 'lg2';
MAD           : 'mad';
MAD24         : 'mad24';
MAX           : 'max';
MEMBAR        : 'membar';
MIN           : 'min';
MOV           : 'mov';
MUL           : 'mul';
MUL24         : 'mul24';
NEG           : 'neg';
NOP           : 'nop';
NOT           : 'not';
OR            : 'or';
PMEVENT       : 'pmevent';
RCP           : 'rcp';
RED           : 'red';
REM           : 'rem';
RET           : 'ret';
RSQRT         : 'rsqrt';
SAD           : 'sad';
SELP          : 'selp';
SET           : 'set';
SETP          : 'setp';
SHL           : 'shl';
SHR           : 'shr';
SIN           : 'sin';
SLCT          : 'slct';
SQRT          : 'sqrt';
ST            : 'st';
SUB           : 'sub';
SUBC          : 'subc';
TRAP          : 'trap';
VOTE          : 'vote';
VRED          : 'vred';
XOR           : 'xor';

// predefined identifiers
CLOCK         : '%clock';
LANEID        : '%laneid';
PM            : '%pm'[0-3];
NCTAID        : '%nctaid';
SMID          : '%smid';
CTAID         : '%ctaid';
NTID          : '%ntid';
TID           : '%tid';
NSMID         : '%nsmid';
WARPID        : '%warpid';
GRIDID        : '%gridid';
WARP_SZ       : 'WARP_SZ';

//#include, #define, #if, #ifdef, #else, #endif, #line, #file

//

LABEL
    : [a-zA-Z0-9.] + ':'
    ;

IDENTIFIER
    : [a-zA-Z] [a-zA-Z0-9_$]*
    | [_$%] [a-zA-Z0-9_$]+
    ;

PARAM_VAR_NAME
    : '%' IDENTIFIER '<' DECIMAL_LITERAL '>'
    ;

VER
    : DECIMAL_LITERAL '.' DECIMAL_LITERAL;

fragment BIT
    : [01]
    ;

fragment HEX_DIGIT
    : [0-9a-fA-F]
    ;

HEXADECIMAL_LITERAL
    : '0' [xX] HEX_DIGIT+ 'U'?
    ;

DECIMAL_LITERAL
    : ([1-9][0-9]* | '0') 'U'? //TODO
    ;

BINARY_LITERAL
    : '0' [bB] BIT+ 'U'?
    ;

OCTAL_LITERAL
    : '0' [0-7]+ 'U'?
    ;

SINGLE_PRECISION_FLOATING_POINT_LITERAL
    : '0' [fF] HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
               HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

DOUBLE_PRECISION_FLOATING_POINT_LITERAL
    : '0' [dD] HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
               HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
               HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
               HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

STRING
    : '<' [a-zA-Z0-9$*,%/:?#@.]*
    ;

CHAR
    : [a-zA-Z0-9>.] '>'
    ;

LCB            : '{';
RCB            : '}';
LRP            : '(';
RRP            : ')';
LSB            : '[';
RSB            : ']';
PLUS           : '+';
MINUS          : '-';
NEG_OP         : '~';
SEMI           : ';';
AT_OP          : '@';
EM_OP          : '!';
MOD_OP         : '%';
STAR           : '*';
DIV_OP         : '/';
AND_OP         : '&';
OR_OP          : '|';
XOR_OP         : '^';
QM_OP          : '?';
COLON          : ':';
LT             : '<';
GT             : '>';
LE             : '<=';
GE             : '>=';
NE             : '!=';
EQ             : '==';
ASSIGN         : '=';
BIT_BUCKET     : '_';
COMMA          : ',';

WS                      : [ \n\r\t]                                        -> channel(HIDDEN);
fragment NEWLINE        : '\r'? '\n';
fragment NEWLINE_EOF    : NEWLINE | EOF;
EOL                     : [\r\n]+;
SINGLE_LINE_COMMENT     : '//' ~('\r' | '\n')* NEWLINE_EOF                 -> channel(HIDDEN);
MULTI_LINE_COMMENT      : '/*' .*? '*/'                                    -> channel(HIDDEN);
