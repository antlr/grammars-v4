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

lexer grammar PTXLexer;

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
PARAM          : '.param';
REG            : '.reg';
SECTION        : '.section';
SHARED         : '.shared';
SREG           : '.sreg';
STRUCT         : '.struct';
SURF           : '.surf';
TARGET         : '.target';
DOT_TEX        : '.tex';
UNION          : '.union';
VERSION        : '.version' -> mode(V);
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
V3             : '.v3';
V4             : '.v4';

SM_10          : 'sm_10';
SM_11          : 'sm_11';
SM_12          : 'sm_12';
SM_13          : 'sm_13';
COMPUTE_10     : 'compute_10';
COMPUTE_11     : 'compute_11';

// platform option
DEBUG          : 'debug';
MAP_F64_TO_F32 : 'map_f64_to_f32';

// instruction keyword
ABS            : 'abs';
ADD            : 'add';
ADDC           : 'addc';
AND            : 'and';
ATOM           : 'atom';
BAR            : 'bar';
BRA            : 'bra';
BRKPT          : 'brkpt';
CALL           : 'call';
CNOT           : 'cnot';
COS            : 'cos';
CROSS          : 'cross';
CVT            : 'cvt';
DIV            : 'div';
DOT            : 'dot';
EX2            : 'ex2';
EXIT           : 'exit';
EXTRACT        : 'extract';
FMA            : 'fma';
FRC            : 'frc';
INSERT         : 'insert';
LD             : 'ld';
LG2            : 'lg2';
MAD            : 'mad';
MAD24          : 'mad24';
MAX            : 'max';
MEMBAR         : 'membar';
MIN            : 'min';
MOV            : 'mov';
MUL            : 'mul';
MUL24          : 'mul24';
NEG            : 'neg';
NOP            : 'nop';
NOT            : 'not';
OR             : 'or';
RCP            : 'rcp';
REM            : 'rem';
RET            : 'ret';
RSQRT          : 'rsqrt';
SAD            : 'sad';
SELP           : 'selp';
SET            : 'set';
SETP           : 'setp';
SHL            : 'shl';
SHR            : 'shr';
SIN            : 'sin';
SLCT           : 'slct';
SQRT           : 'sqrt';
ST             : 'st';
SUB            : 'sub';
SUBC           : 'subc';
TEX            : 'tex';
TRAP           : 'trap';
VOTE           : 'vote';
VRED           : 'vred';
XOR            : 'xor';

// predefined identifiers
CLOCK          : '%clock';
LANEID         : '%laneid';
PM             : '%pm'[0-3];
NCTAID         : '%nctaid.' [012];
SMID           : '%smid';
CTAID          : '%ctaid.' [012];
NTID           : '%ntid.' [012];
TID            : '%tid.' [012];
NSMID          : '%nsmid';
WARPID         : '%warpid';
GRIDID         : '%gridid';
WARP_SZ        : 'WARP_SZ';

RN             : '.rn';
RZ             : '.rz';
RM             : '.rm';
RP             : '.rp';

RNI            : '.rni';
RZI            : '.rzi';
RMI            : '.rmi';
RPI            : '.rpi';

SAT            : '.sat';
UNI            : '.uni';

LT             : '.lt';
GT             : '.gt';
LE             : '.le';
GE             : '.ge';
NE             : '.ne';
EQ             : '.eq';
LS             : '.ls';
HS             : '.hs';
EQU            : '.equ';
NEU            : '.neu';
LTU            : '.ltu';
LEU            : '.leu';
GTU            : '.gtu';
GEU            : '.geu';
NUM            : '.num';
NAN            : '.nan';
HI             : '.hi';
LO             : '.lo';
WIDE           : '.wide';

G1D            : '.1d';
G2D            : '.2d';
G3D            : '.3d';

SYNC           : '.sync';
INC            : '.inc';
DEC            : '.dec';
CAS            : '.cas';
EXCH           : '.exch';

DOT_AND        : '.and';
DOT_OR         : '.or';
DOT_XOR        : '.xor';

//#include, #define, #if, #ifdef, #else, #endif, #line, #file

LABEL
    : [a-zA-Z0-9$_] + ':'
    ;

IDENTIFIER
    : [a-zA-Z] [a-zA-Z0-9_$]*
    | [_$%] [a-zA-Z0-9_$]+
    ;

PARAM_VAR_NAME
    : '%' IDENTIFIER '<' DECIMAL_LITERAL '>'
    ;

fragment DIGIT
    : [0-9]
    ;

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
    : ([1-9] DIGIT* | '0') 'U'?
    ;

BINARY_LITERAL
    : '0' [bB] BIT+ 'U'?
    ;

OCTAL_LITERAL
    : '0' [0-7]+ 'U'?
    ;

REAL_LITERAL
    : ([1-9] DIGIT* | '0') '.' DIGIT*
    | '.' DIGIT+
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
ASSIGN         : '=';
BIT_BUCKET     : '_';
COMMA          : ',';
DOT_OP         : '.';

WS                      : [ \n\r\t]                                       -> channel(HIDDEN);
SINGLE_LINE_COMMENT     : '//' ~[\r\n]*                                   -> channel(HIDDEN);
MULTI_LINE_COMMENT      : '/*' .*? '*/'                                   -> channel(HIDDEN);

mode V;

DECIMAL_LITERAL_V
    : ([1-9] DIGIT* | '0') 'U'? -> type(DECIMAL_LITERAL)
    ;

VER
    : DECIMAL_LITERAL '.' DECIMAL_LITERAL -> mode(DEFAULT_MODE)
    ;

V_WS                    : [ \n\r\t]                                       -> type(WS), channel(HIDDEN);
