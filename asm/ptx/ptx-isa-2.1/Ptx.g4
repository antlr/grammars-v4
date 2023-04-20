/*
   Copyright 2023 Ken Domino

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

   This is an Antlr v4 grammar for Nvidia's CUDA
   (Compute Unified Device Architecture) PTX (Parallel Thread Execution)
   assembly language, version 2.1 (April 21, 2010).  A description
   (informal) can be found in the CUDA SDK (see
   http://www.nvidia.com/object/cuda_home_new.html and
   http://developer.nvidia.com/object/cuda_3_1_downloads.html).

*/

grammar Ptx;

prog
    : version target address_size? statement+ EOF
    ;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Individual statements and directives of a PTX assembly language file.
//
////////////////////////////////////////////////////////////////////////////////////////////////////

version
    : K_VERSION float_
    ;

target
    : K_TARGET target_list
    ;

address_size
    : K_ADDRESS_SIZE integer
    ;

target_list
    : T_WORD (T_COMMA T_WORD)*
    ;

statement
    : label_decl
    | semicolon_terminated_statement
    | unterminated_statement
    ;

label_decl
    :
    i=T_WORD
    T_COLON
    ;

/**************************************************************************************
**
** PTX has a bizarre mix of semicolon-terminated and -untermininated statements.
** This requires a bit of messiness in the grammar.
**
***************************************************************************************/

semicolon_terminated_statement
    : (
        semicolon_terminated_directive
        | instruction
        | linking_directive
      )
      T_SEMICOLON
    | pragma
    ;

unterminated_statement
    : unterminated_directive
    ;

semicolon_terminated_directive
    : control_flow_directive
    | identifier_decl
    ;

unterminated_directive
    : entry
    | func T_SEMICOLON?
    | debugging_directive
    | T_OC statement+ T_CC
    ;

entry
    :
    a=entry_aux
    ;

entry_aux
    :
    (K_VISIBLE | K_EXTERN )?
    K_ENTRY kernel_name ( T_OP entry_param_list? T_CP )? performance_tuning_directives? entry_body
    ;

kernel_name
    : T_WORD
    ;

entry_param_list
    : entry_param (T_COMMA entry_param)*
    ;

entry_param
    : entry_space align? entry_param_type T_WORD array_spec?
    ;

entry_space
    : K_PARAM
    ;

align
    : K_ALIGN b=byte_count
    ;

texref
    : K_TEXREF
    ;

byte_count
    : base_10_integer | base_8_integer | base_16_integer
    ;

entry_param_type
    : fundamental_type
    | opaque_type
    ;

entry_body
    :
    T_OC statement* T_CC
    ;

fundamental_type
    :
    a=fundamental_type_aux
    ;

fundamental_type_aux
    : K_S8
    | K_S16
    | K_S32
    | K_S64
    | K_U8
    | K_U16
    | K_U32
    | K_U64
    | K_F16
    | K_F32
    | K_F64
    | K_B8
    | K_B16
    | K_B32
    | K_B64
    | K_PRED
    ;

vector_type
    :
    v=vector_type_aux
    f=fundamental_type
    ;

vector_type_aux
    :
    K_V4
    | K_V2
    ;

opaque_type
    :
    a=opaque_type_aux
    ;

opaque_type_aux
    : K_TEXREF
    | K_SAMPLERREF
    | K_SURFREF
    ;

func
    : a=func_aux
    ;

func_aux
    :
    (K_VISIBLE | K_EXTERN )?
    K_FUNC
    ( T_OP func_ret_list? T_CP )?
    func_name
    ( T_OP func_param_list? T_CP )?
    func_body?
    ;

func_name
    : T_WORD
    ;

func_ret_list
    : func_ret ( T_COMMA func_ret )*
    ;

func_ret
    : func_ret_space align? func_ret_type T_WORD array_spec?
    ;

func_ret_space
    : K_PARAM
    | K_REG
    ;

func_ret_type
    : fundamental_type
    ;

func_param_list
    : func_param ( T_COMMA func_param )* ( T_ELLIPSIS )?
    ;

func_param
    : func_param_space align? func_param_type T_WORD array_spec?
    ;

func_param_space
    : K_PARAM
    | K_REG
    ;

func_param_type
    : fundamental_type
    ;

func_body
    : T_OC statement* T_CC
    ;

control_flow_directive
    : branch_targets
    | call_prototype
    | call_targets
    ;

branch_targets
    : label_decl K_BRANCHTARGETS list_of_labels T_SEMICOLON
    ;

list_of_labels
    : opr ( T_COMMA opr )*
    ;

call_targets
    : label_decl K_CALLTARGETS list_of_labels
    ;

call_prototype
    : label_decl K_CALLPROTOTYPE ( T_OP call_param_list? T_CP)? T_UNDERSCORE ( T_OP call_param_list? T_CP )?
    ;

call_param_list
    : call_param ( T_COMMA call_param )*
    ;

call_param
    : call_param_space align? call_param_type T_UNDERSCORE array_spec?
    ;

call_param_space
    : K_PARAM
    | K_REG
    ;

call_param_type
    : fundamental_type
    ;

performance_tuning_directives
    : ( performance_tuning_directive )+
    ;

performance_tuning_directive
    : maxnreg
    | maxntid
    | reqntid
    | minnctapersm
    | maxnctapersm
    | pragma
    ;

maxnreg
    : K_MAXNREG integer
    ;

maxntid
    : K_MAXNTID integer ( T_COMMA integer ( T_COMMA integer )? )?
    ;

reqntid
    : K_REQNTID integer ( T_COMMA integer ( T_COMMA integer )? )?
    ;

minnctapersm
    : K_MINNCTAPERSM integer
    ;

maxnctapersm
    : K_MINNCTAPERSM integer
    ;

pragma
    : K_PRAGMA list_of_strings T_SEMICOLON
    ;

list_of_strings
    : K_NOUNROLL
    ;

debugging_directive
    : dwarf
    | file
    | section
    | loc
    ;

dwarf
    : a=K_DWARF
    ;

file
    : a=K_FILE b=integer c=T_STRING (T_COMMA integer)*
    ;

section
    : a=K_SECTION
    section_name
    T_OC
    data_declarator_list*
    T_CC
    ;

section_name
    : T_WORD | U_DEBUG_ABBREV | U_DEBUG_INFO | U_DEBUG_LINE | (U_DEBUG_LOC (T_PLUS integer)?) | U_DEBUG_PUBNAMES | U_DEBUG_RANGES
    ;

data_declarator_list
    : type
    (integer | T_WORD | U_DEBUG_ABBREV | U_DEBUG_INFO | U_DEBUG_LINE | (U_DEBUG_LOC (T_PLUS integer)?) | U_DEBUG_PUBNAMES | U_DEBUG_RANGES)
    ;

loc
    : a=K_LOC b=integer c=integer d=integer
    ;

linking_directive
    : extern_
    | visible
    ;

extern_
    :
    K_EXTERN
    i=identifier_decl
    ;

visible
    : K_VISIBLE identifier_decl
    ;

identifier_decl
    :
    a=identifier_decl_aux
    ;

identifier_decl_aux
    : (state_space_specifier | global_space_specifier | const_space_specifier)
	align? type? texref?
	(variable_declarator_list_ | variable_declarator+) T_SEMICOLON?
    ;

variable_declarator_list_
    :   type (variable_declarator | variable_declarator_with_initializer) ( T_COMMA (variable_declarator | variable_declarator_with_initializer) )*
    ;

variable_declarator
    : id_or_opcode
    (
        array_spec
        | parameterized_register_spec
    )?
    ;

array_spec
    :
    i=array_spec_aux
    ;

array_spec_aux
    :
    ( T_OB integer? T_CB )+
    ;

parameterized_register_spec
    :
    /// Parameterized register names 1.4 spec, page 28. Only a constant is allow.
    T_LT integer T_GT?
    ;

id_or_opcode
    : T_WORD
    | opcode
    ;

opcode
    : KI_ABS
    | KI_ADD
    | KI_ADDC
    | KI_AND
    | KI_ATOM
    | KI_BAR
    | KI_BFE
    | KI_BFI
    | KI_BFIND
    | KI_BRA
    | KI_BREV
    | KI_BRKPT
    | KI_CALL
    | KI_CLZ
    | KI_CNOT
    | KI_COPYSIGN
    | KI_COS
    | KI_CVT
    | KI_CVTA
    | KI_DIV
    | KI_EX2
    | KI_EXIT
    | KI_FMA
    | KI_ISSPACEP
    | KI_LD
    | KI_LDU
    | KI_LG2
    | KI_MAD
    | KI_MAD24
    | KI_MAX
    | KI_MEMBAR
    | KI_MIN
    | KI_MOV
    | KI_MUL
    | KI_MUL24
    | KI_NEG
    | KI_NOT
    | KI_OR
    | KI_PMEVENT
    | KI_POPC
    | KI_PREFETCH
    | KI_PREFETCHU
    | KI_PRMT
    | KI_RCP
    | KI_RED
    | KI_REM
    | KI_RET
    | KI_RSQRT
    | KI_SAD
    | KI_SELP
    | KI_SET
    | KI_SETP
    | KI_SHL
    | KI_SHR
    | KI_SIN
    | KI_SLCT
    | KI_SQRT
    | KI_ST
    | KI_SUB
    | KI_SUBC
    | KI_SULD
    | KI_SURED
    | KI_SUST
    | KI_SUQ
    | KI_TESTP
    | KI_TEX
    | KI_TXQ
    | KI_TRAP
    | KI_VABSDIFF
    | KI_VADD
    | KI_VMAD
    | KI_VMAX
    | KI_VMIN
    | KI_VSET
    | KI_VSHL
    | KI_VSHR
    | KI_VSUB
    | KI_VOTE
    | KI_XOR
    ;

variable_declarator_with_initializer
    : variable_declarator equal_initializer
    ;

equal_initializer
        :
        T_EQ variable_initializer
        ;

variable_initializer
    : ( aggregate_initializer | constant_expression | id_or_opcode )
    ;

aggregate_initializer
    :   T_OC
            (variable_initializer
                (T_COMMA variable_initializer
                )*
            )?
        T_CC
    ;

type
    : fundamental_type
    | vector_type
    | opaque_type
    ;

id
    : opr
    | array_spec
    ;

state_space_specifier
    :
    a=state_space_specifier_aux
    ;

state_space_specifier_aux
    : align
    | local
    | param
    | reg
    | shared
    | sreg
    | tex
    ;

global_space_specifier
    : a=global
    ;

const_space_specifier
    : a=const_
    ;

const_
    : K_CONST
    ;

global
    : K_GLOBAL
    ;

local
    : K_LOCAL
    ;

param
    : K_PARAM
    ;

reg
    : K_REG
    ;

shared
    : K_SHARED
    ;

sreg
    : K_SREG
    ;

tex
    : K_TEX
    ;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// PTX Instruction set.
//
// Each instruction is complicated by the fact that each has different type qualifiers (the stuff
// after the instruction name), and operands.  In order to construct the tree, Antlr requires
// a lot of auxiliary rules to be added, instead of tree variables for subrules, e.g.,
// 
// i_abs
//     : i=KI_ABS
//     t=(
//         (
//             K_S16 | K_S32 | K_S64
//         ) |
//         (
//             K_FTZ? K_F32
//         ) |
//         (
//             K_F64
//         )
//     )
//     o=(
//          opr_register
//          T_COMMA
//          opr_register_or_constant
//     )
//     ;
//
////////////////////////////////////////////////////////////////////////////////////////////////////

instruction
    :
    a=instruction_aux
    ;

instruction_aux
    : predicate?
    (
        i_abs
        | i_add
        | i_addc
        | i_and
        | i_atom
        | i_bar
        | i_bfe
        | i_bfi
        | i_bfind
        | i_bra
        | i_brev
        | i_brkpt
        | i_call
        | i_clz
        | i_cnot
        | i_copysign
        | i_cos
        | i_cvt
        | i_cvta
        | i_div
        | i_ex2
        | i_exit
        | i_fma
        | i_isspacep
        | i_ld
        | i_ldu
        | i_lg2
        | i_mad
        | i_mad24
        | i_max
        | i_membar
        | i_min
        | i_mov
        | i_mul
        | i_mul24
        | i_neg
        | i_not
        | i_or
        | i_pmevent
        | i_popc
        | i_prefetch
        | i_prefetchu
        | i_prmt
        | i_rcp
        | i_red
        | i_rem
        | i_ret
        | i_rsqrt
        | i_sad
        | i_selp
        | i_set
        | i_setp
        | i_shl
        | i_shr
        | i_sin
        | i_slct
        | i_sqrt
        | i_st
        | i_sub
        | i_subc
        | i_suld
        | i_sured
        | i_sust
        | i_suq
        | i_testp
        | i_tex
        | i_txq
        | i_trap
        | i_vabsdiff
        | i_vadd
        | i_vmad
        | i_vmax
        | i_vmin
        | i_vset
        | i_vshl
        | i_vshr
        | i_vsub
        | i_vote
        | i_xor
    )
    ;

predicate
    : T_AT T_NOT? T_WORD
    ;

i_abs
    :
    i=KI_ABS
    t=i_abs_type
    o=i_abs_opr
    ;

i_abs_type
    :
    (
        (
            K_S16 | K_S32 | K_S64
        ) |
        (
            K_FTZ? K_F32
        ) |
        (
            K_F64
        )
    )
    ;

i_abs_opr
    :
    (
        opr_register
        T_COMMA
        opr_register_or_constant
    )
    ;

i_add
    :
    i=KI_ADD
    t=i_add_type
    o=i_add_opr
    ;

i_add_type:
    (
        (
            ( ( K_SAT K_S32 ) | ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64) ) |
            ( K_CC ( K_S32 | K_U32 ) )
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )? K_FTZ? K_SAT? K_F32
        ) |
        ( ( K_RN | K_RZ | K_RM | K_RP )? K_F64 )
    )
    ;

i_add_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_addc
    :
    i=KI_ADDC
    t=i_addc_type
    o=i_addc_opr
    ;

i_addc_type:
    (
        (
            K_CC? ( K_S32 | K_U32 | K_S16 | K_U16 | K_U64 )
        )
    )
    ;

i_addc_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_and
    :
    i=KI_AND
    t=i_and_type
    o=i_and_opr
    ;

i_and_type:
    (
        K_PRED | K_B16 | K_B32 | K_B64
    )
    ;

i_and_opr
    :
    opr3
    ;

i_atom
    :
    i=KI_ATOM
    t=i_atom_type
    o=i_atom_opr
    ;

i_atom_type
    :
    (
        (
            ( K_GLOBAL | K_SHARED )?
            ( K_AND | K_OR | K_XOR | K_CAS | K_EXCH | K_ADD | K_INC | K_DEC | K_MIN | K_MAX )
            ( K_B32 | K_B64 | K_U32 | K_U64 | K_S32 | K_F32 )
        )
    )
    ;

i_atom_opr
    :
    opr T_COMMA T_OB opr T_CB T_COMMA opr ( T_COMMA opr )?
    ;

i_bar
    : i_bar1
    | i_bar2
    | i_bar3
    | i_bar4
    ;

i_bar1
    :
    i=KI_BAR
    t=i_bar1_type
    o=i_bar1_opr
    ;

i_bar1_type
    :
    K_SYNC
    ;

i_bar1_opr
    :
    opr ( T_COMMA opr )?
    ;


i_bar2
    :
    i=KI_BAR
    t=i_bar2_type
    o=i_bar2_opr
    ;

i_bar2_type
    :
    K_ARRIVE
    ;

i_bar2_opr
    :
    opr2
    ;

i_bar3
    :
    i=KI_BAR
    t=i_bar3_type
    o=i_bar3_opr
    ;

i_bar3_type
    :
    (
        K_RED
        K_POPC
        K_U32
    )
    ;

i_bar3_opr
    :
    opr T_COMMA opr
    ( T_COMMA opr )?
    T_COMMA T_NOT? opr
    ;

i_bar4
    :
    i=KI_BAR
    t=i_bar4_type
    o=i_bar4_opr
    ;

i_bar4_type
    :
    (
        K_RED
        ( K_AND | K_OR )
        K_PRED
    )
    ;

i_bar4_opr
    :
    opr T_COMMA opr
    ( T_COMMA opr )?
    T_COMMA T_NOT? opr
    ;

i_bfe
    :
    i=KI_BFE
    t=i_bfe_type
    o=i_bfe_opr
    ;

i_bfe_type
    :
    ( K_S32 | K_U32 | K_S64 | K_U64 )
    ;

i_bfe_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant3
    ;

i_bfi
    :
    i=KI_BFI
    t=i_bfi_type
    o=i_bfi_opr
    ;

i_bfi_type
    :
    ( K_B32 | K_B64 )
    ;

i_bfi_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant4
    ;

i_bfind
    :
    i=KI_BFIND
    t=i_bfind_type
    o=i_bfind_opr
    ;

i_bfind_type
    :
    ( K_SHIFTAMT )? ( K_S32 | K_U32 | K_S64 | K_U64 )
    ;

i_bfind_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant
    ;

i_bra
    :
    KI_BRA
    i_bra_type
    i_bra_opr
    ;

i_bra_type
    :
    K_UNI
    |
    ;

i_bra_opr
    :
    (
        opr_label
        | opr_register T_COMMA opr_label
    )
    ;

i_brev
    :
    i=KI_BREV
    t=i_brev_type
    o=i_brev_opr
    ;

i_brev_type
    :
    ( K_B32 | K_B64 )
    ;

i_brev_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant
    ;

i_brkpt
    :
    i=KI_BRKPT
    ;

i_call
    :
        KI_CALL
        i_call_type
        ( T_OP opr ( T_COMMA opr )* T_CP T_COMMA )?
        func_name
        ( T_COMMA T_OP opr ( T_COMMA opr )* T_CP )?
        ( T_COMMA flist | T_COMMA fproto )?
    ;

i_call_type
    :
    t=K_UNI
    |
    ;

flist
    : T_WORD
    ;

fproto
    : T_WORD
    ;

i_clz
    :
    i=KI_CLZ
    t=i_clz_type
    o=i_clz_opr
    ;

i_clz_type
    :
    ( K_B32 | K_B64 )
    ;

i_clz_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant
    ;

i_cnot
    :
    i=KI_CNOT
    t=i_cnot_type
    o=i_cnot_opr
    ;

i_cnot_type
    :
    (
        K_B16 | K_B32 | K_B64
    )
    ;

i_cnot_opr
    :
    opr2
    ;

i_copysign
    :
    i=KI_COPYSIGN
    t=i_copysign_type
    o=i_copysign_opr
    ;

i_copysign_type
    :
    ( K_F32 | K_F64 )
    ;

i_copysign_opr
    :
    opr_register
    T_COMMA
    opr_register
    T_COMMA
    opr_register
    ;

i_cos
    :
    i=KI_COS
    t=i_cos_type
    o=i_cos_opr
    ;

i_cos_type
    :
    K_APPROX K_FTZ? K_F32
    ;

i_cos_opr
    :
    opr2
    ;

i_cvt
    :
    i=KI_CVT
    t=i_cvt_type
    o=i_cvt_opr
    ;

i_cvt_type
    :
    (
        (
	    ( i_cvt_irnd | i_cvt_frnd )
            K_FTZ?
            K_SAT?
            ( K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F16 | K_F32 | K_F64 )
            ( K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F16 | K_F32 | K_F64 )
	    ( i_cvt_irnd | i_cvt_frnd )
            K_FTZ?
            K_SAT?
        )
    )
    ;


i_cvt_irnd
    :
    i=i_cvt_irnd_aux
    |
    ;

i_cvt_irnd_aux
    :
    ( K_RNI | K_RZI | K_RMI | K_RPI )
    ;

i_cvt_frnd
    :
    i=i_cvt_frnd_aux
    |
    ;

i_cvt_frnd_aux
    :
    ( K_RN | K_RZ | K_RM | K_RP )
    ;

i_cvt_opr
    :
    opr2
    ;

i_cvta
    :
    i=KI_CVTA
    t=i_cvta_type
    o=i_cvta_opr
    ;

i_cvta_type
    :
    K_TO?
    ( K_GLOBAL | K_LOCAL | K_SHARED | K_CONST )
    ( K_U32 | K_U64 )
    ;

i_cvta_opr
    :
    opr T_COMMA opr ( T_PLUS integer )?
    ;

i_div
    :
    i=KI_DIV
    t=i_div_type
    o=i_div_opr
    ;

i_div_type
    :
    (
        (
            K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64
        ) |
        (
            K_APPROX K_FTZ? K_F32
        ) |
        (
            K_FULL K_FTZ? K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP ) K_FTZ? K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP ) K_F64
        )
    )
    ;

i_div_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_ex2
    :
    i=KI_EX2
    t=i_ex2_type
    o=i_ex2_opr
    ;

i_ex2_type
    :
    K_APPROX K_FTZ? K_F32
    ;

i_ex2_opr
    :
    opr2
    ;

i_exit
    :
    i=KI_EXIT
    ;

i_fma
    :
    i=KI_FMA
    t=i_fma_type
    o=i_fma_opr
    ;

i_fma_type
    :
    ( K_RN | K_RZ | K_RM | K_RP )
    K_FTZ?
    K_SAT?
    K_F32
    ;

i_fma_opr
    :
    opr4
    ;

i_isspacep
    :
    i=KI_ISSPACEP
    t=i_isspacep_type
    o=i_isspacep_opr
    ;

i_isspacep_type
    :
    ( K_GLOBAL | K_LOCAL | K_SHARED )
    ;

i_isspacep_opr
    :
    opr2
    ;

i_ld
    :
    i=KI_LD
    t=i_ld_type
    o=i_ld_opr
    ;

i_ld_type
    :
    (
        (
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_CA | K_CG | K_CS | K_LU | K_CV )?
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_CA | K_CG | K_CS | K_LU | K_CV )?
            ( K_V2 | K_V4 )
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            K_VOLATILE
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            K_VOLATILE
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_V2 | K_V4 )
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_ld_opr
    : opr T_COMMA T_OB opr T_CB
    ;

i_ldu
    :
    i=KI_LDU
    t=i_ldu_type
    o=i_ldu_opr
    ;

i_ldu_type
    :
    (
        (
            ( K_GLOBAL )?
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            ( K_GLOBAL )?
            ( K_V2 | K_V4 )
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_ldu_opr
    :
    opr T_COMMA T_OB opr T_CB
    ;

i_lg2
    :
    i=KI_LG2
    t=i_lg2_type
    o=i_lg2_opr
    ;

i_lg2_type
    :
    K_APPROX K_FTZ? K_F32
    ;

i_lg2_opr
    :
    opr2
    ;

i_mad
    :
    i=KI_MAD
    t=i_mad_type
    o=i_mad_opr
    ;

i_mad_type
    :
    (
        (
            ( K_HI | K_LO | K_WIDE )
            ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 )
        ) |
        ( K_HI K_SAT K_S32 ) |
        (
            K_FTZ?
            K_SAT?
            K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP)
            K_FTZ?
            K_SAT?
            K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP)
            K_F64
        )
    )
    ;

i_mad_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant3
    ;

i_mad24
    :
    i=KI_MAD24
    t=i_mad24_type
    o=i_mad24_opr
    ;

i_mad24_type
    :
    (
        (
            ( K_HI | K_LO )
            ( K_U32 | K_S32 )
        ) |
        (
            K_HI K_SAT K_S32
        )
    )
    ;

i_mad24_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant3
    ;

i_max
    :
    i=KI_MAX
    t=i_max_type
    o=i_max_opr
    ;

i_max_type
    :
    (
        (
            K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64
        ) |
        (
            K_FTZ? K_F32
        ) |
        (
            K_F64
        )
    )
    ;

i_max_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_membar
    :
    i=KI_MEMBAR
    t=i_membar_type
    ;

i_membar_type
    :
    ( K_CTA | K_GL | K_SYS )
    ;

i_min
    :
    i=KI_MIN
    t=i_min_type
    o=i_min_opr
    ;

i_min_type
    :
    (
        (
            K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64
        ) |
        (
            K_FTZ? K_F32
        ) |
        (
            K_F64
        )
    )
    ;

i_min_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_mov
    :
    i=KI_MOV
    t=i_mov_type
    o=i_mov_opr
    ;

i_mov_type
    :
    (
        K_PRED | K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64
    )
    ;

i_mov_opr
    :
    opr2
    ;

i_mul
    :
    i=KI_MUL
    t=i_mul_type
    o=i_mul_opr
    ;

i_mul_type
    :
    (
        (
            ( K_HI | K_LO | K_WIDE )? ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 )
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )? K_FTZ? K_SAT? K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )? K_F64
        )
    )
    ;

i_mul_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_mul24
    :
    i=KI_MUL24
    t=i_mul24_type
    o=i_mul24_opr
    ;

i_mul24_type
    :
    ( ( K_HI | K_LO ) ( K_U32 | K_S32 ) )
    ;

i_mul24_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_neg
    :
    i=KI_NEG
    t=i_neg_type
    o=i_neg_opr
    ;

i_neg_type
    :
    (
        (
            K_S16 | K_S32 | K_S64
        ) |
        (
            K_FTZ? K_F32
        ) |
        (
            K_F64
        )
    )
    ;

i_neg_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant
    ;

i_not
    :
    i=KI_NOT
    t=i_not_type
    o=i_not_opr
    ;

i_not_type
    :
    (
        K_PRED | K_B16 | K_B32 | K_B64
    )
    ;

i_not_opr
    :
    opr2
    ;

i_or
    :
    i=KI_OR
    t=i_or_type
    o=i_or_opr
    ;

i_or_type
    :
    (
        K_PRED | K_B16 | K_B32 | K_B64
    )
    ;

i_or_opr
    :
    opr3
    ;

i_pmevent
    :
    i=KI_PMEVENT
    o=i_pmevent_opr
    ;

i_pmevent_opr
    :
    opr
    ;

i_popc
    :
    i=KI_POPC
    t=i_popc_type
    o=i_popc_opr
    ;

i_popc_type
    :
    ( K_B32 | K_B64 )
    ;

i_popc_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant
    ;

i_prefetch
    :
    i=KI_PREFETCH
    t=i_prefetch_type
    o=i_prefetch_opr
    ;

i_prefetch_type
    :
    (
        (
            ( K_GLOBAL | K_LOCAL )?
            ( K_L1 | K_L2 )
        )
    )
    ;

i_prefetch_opr
    :
    T_OB opr T_CB
    ;

i_prefetchu
    :
    i=KI_PREFETCHU
    t=i_prefetchu_type
    o=i_prefetchu_opr
    ;

i_prefetchu_type
    :
    K_L1
    ;

i_prefetchu_opr
    :
    T_OB opr T_CB
    ;

i_prmt
    :
    i=KI_PRMT
    t=i_prmt_type
    o=i_prmt_opr
    ;

i_prmt_type
    :
    K_B32
    ( K_F4E | K_B4E | K_RC8 | K_ECL | K_ECR | K_RC16 )?
    ;

i_prmt_opr
    :
    opr_register
    T_COMMA
    opr_register
    T_COMMA
    opr_register
    T_COMMA
    opr_register
    ;

i_rcp
    :
    i=KI_RCP
    t=i_rcp_type
    o=i_rcp_opr
    ;

i_rcp_type
    :
    (
        (
            K_APPROX K_FTZ? K_F32 K_FTZ?
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )
            K_FTZ?
            K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )
            K_F64
        ) |
        (
            K_APPROX K_FTZ K_F64
        )
    )
    ;

i_rcp_opr
    :
    opr2
    ;

i_red
    :
    i=KI_RED
    t=i_red_type
    o=i_red_opr
    ;

i_red_type
    :
    ( K_GLOBAL | K_SHARED )?
    ( K_AND | K_OR | K_XOR | K_ADD | K_INC | K_DEC | K_MIN | K_MAX )
    ( K_B32 | K_B64 | K_U32 | K_U64 | K_S32 | K_F32 )
    ;

i_red_opr
    :
    T_OB opr T_CB T_COMMA opr
    ;

i_rem
    :
    i=KI_REM
    t=i_rem_type
    o=i_rem_opr
    ;

i_rem_type
    :
    ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 )
    ;

i_rem_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_ret
    :
    i=KI_RET
    t=i_ret_type
    ;

i_ret_type
    : K_UNI
    |
    ;

i_rsqrt
    :
    i=KI_RSQRT
    t=i_rsqrt_type
    o=i_rsqrt_opr
    ;

i_rsqrt_type
    :
    (
        (
            K_APPROX K_FTZ? K_F32
        ) |
        (
            K_APPROX K_F64
        )
    )
    ;

i_rsqrt_opr
    :
    opr2
    ;

i_sad
    :
    i=KI_SAD
    t=i_sad_type
    o=i_sad_opr
    ;

i_sad_type
    :
    ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 )
    ;

i_sad_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant3
    ;

i_selp
    :
    i=KI_SELP
    t=i_selp_type
    o=i_selp_opr
    ;

i_selp_type
    :
    ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
    ;

i_selp_opr
    :
    opr4
    ;

i_set
    : i_set1
    | i_set2
    ;

i_set1
    :
    i=KI_SET
    t=i_set1_type
    o=i_set1_opr
    ;

i_set1_type
    :
    (
        (
            ( K_EQ | K_NE | K_LT | K_LE | K_GT | K_GE | K_LO | K_LS | K_HI | K_HS
              | K_EQU | K_NEU | K_LTU | K_LEU | K_GTU | K_GEU | K_NUM | K_NAN )
            K_FTZ?
            ( K_U32 | K_S32 | K_F32 )
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_set1_opr
    :
    opr3
    ;

i_set2
    :
    i=KI_SET
    t=i_set2_type
    o=i_set2_opr
    ;

i_set2_type
    :
    (
        (
            ( K_EQ | K_NE | K_LT | K_LE | K_GT | K_GE | K_LO | K_LS | K_HI | K_HS
              | K_EQU | K_NEU | K_LTU | K_LEU | K_GTU | K_GEU | K_NUM | K_NAN )
            ( K_AND | K_OR | K_XOR )
            K_FTZ?
            ( K_U32 | K_S32 | K_F32 )
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_set2_opr
    :
    opr T_COMMA opr T_COMMA opr T_COMMA T_NOT? opr
    ;

i_setp
    : i_setp1
    | i_setp2
    ;

i_setp1
    :
    i=KI_SETP
    t=i_setp1_type
    o=i_setp1_opr
    ;

i_setp1_type
    :
    (
        (
            ( K_EQ | K_NE | K_LT | K_LE | K_GT | K_GE | K_LO | K_LS | K_HI | K_HS
              | K_EQU | K_NEU | K_LTU | K_LEU | K_GTU | K_GEU | K_NUM | K_NAN )
            K_FTZ?
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_setp1_opr
    :
    opr3
    ;

i_setp2
    :
    i=KI_SETP
    t=i_setp2_type
    o=i_setp2_opr
    ;

i_setp2_type
    :
    (
        (
            ( K_EQ | K_NE | K_LT | K_LE | K_GT | K_GE | K_LO | K_LS | K_HI | K_HS
              | K_EQU | K_NEU | K_LTU | K_LEU | K_GTU | K_GEU | K_NUM | K_NAN )
            ( K_AND | K_OR | K_XOR )
            K_FTZ?
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_setp2_opr
    :
    opr T_COMMA opr T_COMMA opr T_COMMA T_NOT? opr
    ;

i_shl
    :
    i=KI_SHL
    t=i_shl_type
    o=i_shl_opr
    ;

i_shl_type
    :
    (
        K_B16 | K_B32 | K_B64
    )
    ;

i_shl_opr
    :
    opr3
    ;

i_shr
    :
    i=KI_SHR
    t=i_shr_type
    o=i_shr_opr
    ;

i_shr_type
    :
    (
        K_B16 | K_B32 | K_B64 |
        K_U16 | K_U32 | K_U64 |
        K_S16 | K_S32 | K_S64
    )
    ;

i_shr_opr
    :
    opr3
    ;

i_sin
    :
    i=KI_SIN
    t=i_sin_type
    o=i_sin_opr
    ;

i_sin_type
    :
    K_APPROX K_FTZ? K_F32
    ;

i_sin_opr
    :
    opr2
    ;

i_slct
    :
    i=KI_SLCT
    t=i_slct_type
    o=i_slct_opr
    ;

i_slct_type
    :
    (
        (
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
            K_S32
        ) |
        (
            K_FTZ?
            ( K_B16 | K_B32 | K_B64 | K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
            K_F32
        )
    )
    ;

i_slct_opr
    :
    opr4
    ;

i_sqrt
    :
    i=KI_SQRT
    t=i_sqrt_type
    o=i_sqrt_opr
    ;

i_sqrt_type
    :
    (
        (
            K_APPROX K_FTZ? K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )
            K_FTZ?
            K_F32
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )
            K_F64
        )
    )
    ;

i_sqrt_opr
    :
    opr2
    ;

i_st
    :
    i=KI_ST
    t=i_st_type
    o=i_st_opr
    ;

i_st_type
    :
    (
        (
            ( K_GLOBAL | K_LOCAL | K_SHARED | K_PARAM )?
            ( K_WB | K_CG | K_CS | K_WT )?
            ( K_V2 | K_V4 )?
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            K_VOLATILE
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        ) |
        (
            K_VOLATILE
            ( K_CONST | K_GLOBAL | K_LOCAL | K_PARAM | K_SHARED )?
            ( K_V2 | K_V4 )
            ( K_B8 | K_B16 | K_B32 | K_B64 | K_U8 | K_U16 | K_U32 | K_U64 | K_S8 | K_S16 | K_S32 | K_S64 | K_F32 | K_F64 )
        )
    )
    ;

i_st_opr
    :
    T_OB opr T_CB T_COMMA opr
    ;

i_sub
    :
    i=KI_SUB
    t=i_sub_type
    o=i_sub_opr
    ;

i_sub_type
    :
    (
        (
            ( ( K_SAT K_S32 ) | ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64) ) |
            ( K_CC ( K_S32 | K_U32 ) )
        ) |
        (
            ( K_RN | K_RZ | K_RM | K_RP )? K_FTZ? K_SAT? K_F32
        ) |
        ( ( K_RN | K_RZ | K_RM | K_RP )? K_F64 )
    )
    ;

i_sub_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_subc
    :
    i=KI_SUBC
    t=i_subc_type
    o=i_subc_opr
    ;

i_subc_type
    :
    (
        ( ( K_SAT K_S32 ) | ( K_U16 | K_U32 | K_U64 | K_S16 | K_S32 | K_S64) ) |
        ( K_CC ( K_S32 | K_U32 ) )
    )
    ;

i_subc_opr
    :
    opr_register
    T_COMMA
    opr_register_or_constant2
    ;

i_suld
    :
    i=KI_SULD
    t=i_suld_type
    o=i_suld_opr
    ;

i_suld_type
    :
    (
        (
            K_B
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B8 | K_B16 | K_B32 | K_B64 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        ) |
        (
            K_P
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B32 | K_U32 | K_S32 | K_F32 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        )
    )
    ;

i_suld_opr
    :
    opr T_COMMA T_OB opr T_COMMA opr T_CB
    ;

i_sured
    :
    i=KI_SURED
    t=i_sured_type
    o=i_sured_opr
    ;

i_sured_type
    :
    (
        (
            K_B
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B8 | K_B16 | K_B32 | K_B64 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        ) |
        (
            K_P
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B32 | K_U32 | K_S32 | K_F32 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        )
    )
    ;

i_sured_opr
    :
    T_OB opr T_COMMA opr T_CB T_COMMA opr
    ;

i_sust
    :
    i=KI_SUST
    t=i_sust_type
    o=i_sust_opr
    ;

i_sust_type
    :
    (
        (
            K_B
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B8 | K_B16 | K_B32 | K_B64 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        ) |
        (
            K_P
            ( K_1D | K_2D | K_3D )
            ( K_CA | K_CG | K_CS | K_CV )?
            ( K_V2 | K_V4 )?
            ( K_B32 | K_U32 | K_S32 | K_F32 )
            ( K_TRAP | K_CLAMP | K_ZERO )
        )
    )
    ;

i_sust_opr
    :
    T_OB opr T_COMMA opr T_CB T_COMMA opr
    ;

i_suq
    :
    i=KI_SUQ
    t=i_suq_type
    o=i_suq_opr
    ;

i_suq_type
    :
    ( K_WIDTH | K_HEIGHT | K_DEPTH )
    K_B32
    ;

i_suq_opr
    :
    opr T_COMMA T_OB opr T_CB
    ;

i_testp
    :
    i=KI_TESTP
    t=i_testp_type
    o=i_testp_opr
    ;

i_testp_type
    :
    ( K_FINITE | K_INFINITE | K_NUMBER | K_NOTANUMBER | K_NORMAL | K_SUBNORMAL )
    ( K_F32 | K_F64 )
    ;

i_testp_opr
    :
    opr_register
    T_COMMA
    opr_register
    ;

i_tex
    :
    i=KI_TEX
    t=i_tex_type
    o=i_tex_opr
    ;

i_tex_type
    :
    (
        (
            ( K_1D | K_2D | K_3D )
            K_V4
            ( K_U32 | K_S32 | K_F32 )
            ( K_S32 | K_F32 )
        )
    )
    ;

i_tex_opr
    :
    opr T_COMMA T_OB opr T_COMMA opr ( T_COMMA opr )? T_CB
    ;

i_txq
    :
    i=KI_TXQ
    t=i_txq_type
    o=i_txq_opr
    ;

i_txq_type
    :
    (
        (
            ( K_WIDTH | K_HEIGHT | K_DEPTH | K_CHANNEL_DATA_TYPE | K_CHANNEL_ORDER | K_NORMALIZED_COORDS )
        ) |
        (
            ( K_FILTER_MODE | K_ADDR_MODE_0 | K_ADDR_MODE_1 | K_ADDR_MODE_2 )
        )
    ) 
    K_B32
    ;

i_txq_opr
    :
    opr T_COMMA T_OB opr T_CB
    ;

i_trap
    :
    i=KI_TRAP
    ;

i_vabsdiff
    :
    i=KI_VABSDIFF
    ;

i_vadd
    :
    i=KI_VADD
    ;

i_vmad
    :
    i=KI_VMAD
    ;

i_vmax
    :
    i=KI_VMAX
    ;

i_vmin
    :
    i=KI_VMIN
    ;

i_vset
    :
    i=KI_VSET
    ;

i_vshl
    :
    i=KI_VSHL
    ;

i_vshr
    :
    i=KI_VSHR
    ;

i_vsub
    :
    i=KI_VSUB
    ;

i_vote
    :
    i=KI_VOTE
    t=i_vote_type
    o=i_vote_opr
    ;

i_vote_type
    :
    (
        (
            ( K_ALL | K_ANY | K_UNI )
            K_PRED
        ) |
        (
            K_BALLOT
            K_B32
        )
    )
    ;

i_vote_opr
    :
    opr T_COMMA T_NOT? opr
    ;

i_xor
    :
    i=KI_XOR
    t=i_xor_type
    o=i_xor_opr
    ;

i_xor_type
    :    
    (
        K_PRED | K_B16 | K_B32 | K_B64
    )
    ;

i_xor_opr
    :
    opr3
    ;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Restricted operands.
//
////////////////////////////////////////////////////////////////////////////////////////////////////

opr_register
    : a=id_or_opcode
    ;

opr_register_or_constant
    : a=id_or_opcode
    | b=constant_expression
    ;

opr_register_or_constant2
    : opr_register_or_constant T_COMMA opr_register_or_constant
    ;

opr_register_or_constant3
    : opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant
    ;

opr_register_or_constant4
    : opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant
    ;

opr_register_or_constant5
    : opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant T_COMMA opr_register_or_constant
    ;

opr_label
    : a=T_WORD
    ;

////////////////////////////////////////////////////////////////////////////////////////////////////
//
// General operands, including aggregate data.
//
////////////////////////////////////////////////////////////////////////////////////////////////////

opr
    :
    a=opr_aux
    ;

// This probably needs a lot of work...
opr_aux
    : (
        (
            ( id_or_opcode ( K_X | K_Y | K_Z | K_W | K_A | K_R | K_G | K_B )?
                                | constant_expression )
            ( T_PLUS constant_expression )?
            ( T_LT opr T_GT )?
        ) |
        ( // aggregate
            T_OC
	    (id_or_opcode | T_UNDERSCORE)
            ( T_COMMA (id_or_opcode | T_UNDERSCORE))*
            T_CC
        ) |
        T_UNDERSCORE
    )
    ;

opr2
    : opr T_COMMA opr
    ;

opr3
    : opr T_COMMA opr T_COMMA opr
    ;

opr4
    : opr T_COMMA opr T_COMMA opr T_COMMA opr
    ;

opr5
    : opr T_COMMA opr T_COMMA opr T_COMMA opr T_COMMA opr
    ;


////////////////////////////////////////////////////////////////////////////////////////////////////
//
// Constant literal expressions.
//
////////////////////////////////////////////////////////////////////////////////////////////////////

constant_expression
    :
    e=constant_expression_aux
    ;

constant_expression_aux
    :
    conditional_expression
    ;

conditional_expression
    :   conditional_or_expression
        ( T_QUESTION constant_expression_aux T_COLON conditional_expression
        )?
    ;

conditional_or_expression
    :   conditional_and_expression
        ( T_OROR conditional_and_expression
        )*
    ;

conditional_and_expression
    :   inclusive_or_expression
        ( T_ANDAND inclusive_or_expression
        )*
    ;

inclusive_or_expression
    :   exclusive_or_expression
        ( T_OR exclusive_or_expression
        )*
    ;

exclusive_or_expression
    :   and_expression
        ( T_XOR and_expression
        )*
    ;

and_expression
    :   equality_expression
        ( T_AND equality_expression
        )*
    ;

equality_expression
    :   relational_expression
        (
            (   T_EQEQ
            |   T_NOTEQ
            )
            relational_expression
        )*
    ;

relational_expression
    :   shift_expression
        (relational_op shift_expression
        )*
    ;

relational_op
    :    T_LE
    |    T_GE
    |   T_LT
    |   T_GT
    ;

shift_expression
    :   additive_expression
        (shift_op additive_expression
        )*
    ;

shift_op
    :    T_LTLT
    |    T_GTGT
    ;

additive_expression
    :   multiplicative_expression
        (   
            ( T_PLUS
            | T_MINUS
            )
            multiplicative_expression
         )*
    ;

multiplicative_expression 
    :
        unary_expression
        (   
            ( T_STAR
            | T_SLASH
            | T_PERCENT
            )
            unary_expression
        )*
    ;

unary_expression
    :   T_PLUS unary_expression
    |   T_MINUS unary_expression
    |   unary_expression_not_plus_minus
    ;

unary_expression_not_plus_minus
    :   T_TILDE unary_expression
    |   T_NOT unary_expression
    |   cast_expression
    |   primary
    ;

cast_expression
    :
    e=cast_expression_aux
    ;

cast_expression_aux
    :
    T_OP (K_S64 | K_U64) T_CP unary_expression
    ;

primary
    :   par_expression
    |   integer
    |   float_
    ;

par_expression
    :   T_OP constant_expression_aux T_CP
    ;

integer
    : base_10_integer
    | base_8_integer
    | base_16_integer
    ;

float_
    : T_FLT_LITERAL
    ;

base_10_integer
    : T_DEC_LITERAL
    ;

base_8_integer
    : T_OCT_LITERAL
    ;

base_16_integer
    : T_HEX_LITERAL
    ;

KI_ABS: 'abs';
KI_ADD: 'add';
KI_ADDC: 'addc';
KI_AND: 'and';
KI_ATOM: 'atom';
KI_BAR: 'bar';
KI_BFE: 'bfe';
KI_BFI: 'bfi';
KI_BFIND: 'bfind';
KI_BRA: 'bra';
KI_BREV: 'brev';
KI_BRKPT: 'brkpt';
KI_CALL: 'call';
KI_CLZ: 'clz';
KI_CNOT: 'cnot';
KI_COPYSIGN: 'copysign';
KI_COS: 'cos';
KI_CVT: 'cvt';
KI_CVTA: 'cvta';
KI_DIV: 'div';
KI_EX2: 'ex2';
KI_EXIT: 'exit';
KI_FMA: 'fma';
KI_ISSPACEP: 'isspacep';
KI_LD: 'ld';
KI_LDU: 'ldu';
KI_LG2: 'lg2';
KI_MAD24: 'mad24';
KI_MAD: 'mad';
KI_MAX: 'max';
KI_MEMBAR: 'membar';
KI_MIN: 'min';
KI_MOV: 'mov';
KI_MUL24: 'mul24';
KI_MUL: 'mul';
KI_NEG: 'neg';
KI_NOT: 'not';
KI_OR: 'or';
KI_PMEVENT: 'pmevent';
KI_POPC: 'popc';
KI_PREFETCH: 'prefetch';
KI_PREFETCHU: 'prefetchu';
KI_PRMT: 'prmt';
KI_RCP: 'rcp';
KI_RED: 'red';
KI_REM: 'rem';
KI_RET: 'ret';
KI_RSQRT: 'rsqrt';
KI_SAD: 'sad';
KI_SELP: 'selp';
KI_SETP: 'setp';
// Note order after SETP
KI_SET: 'set';
KI_SHL: 'shl';
KI_SHR: 'shr';
KI_SIN: 'sin';
KI_SLCT: 'slct';
KI_SQRT: 'sqrt';
KI_ST: 'st';
KI_SUB: 'sub';
KI_SUBC: 'subc';
KI_SULD: 'suld';
KI_SUQ: 'suq';
KI_SURED: 'sured';
KI_SUST: 'sust';
KI_TESTP: 'testp';
KI_TEX: 'tex';
KI_TRAP: 'trap';
KI_TXQ: 'txq';
KI_VABSDIFF: 'vabsdiff';
KI_VADD: 'vadd';
KI_VMAD: 'vmad';
KI_VMAX: 'vmax';
KI_VMIN: 'vmin';
KI_VOTE: 'vote';
KI_VSET: 'vset';
KI_VSHL: 'vshl';
KI_VSHR: 'vshr';
KI_VSUB: 'vsub';
KI_XOR: 'xor';

T_QUESTION: '?';
T_OROR: '||';
T_ANDAND: '&&';
T_OR: '|';
T_XOR: '^';
T_AND: '&';
T_EQEQ: '==';
T_LE: '<=';
T_GE: '>=';
T_LTLT: '<<';
T_GTGT: '>>';
T_STAR: '*';
T_TILDE: '~';
T_FLT_LITERAL: ( '0' ('f' | 'F' | 'd' | 'D') ('0' .. '9' | 'a' .. 'f' |  'A' .. 'F' )+ | '.' ('0' .. '9')+ | ('0' .. '9')+ '.' | ('0' .. '9')+ '.' ('0' .. '9')+ );
T_HEX_LITERAL: '0' ('x' | 'X') ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )+ 'U'?;
T_OCT_LITERAL: '0' ('0' .. '7' )+ 'U'?;
T_DEC_LITERAL: ('0' .. '9') ( '0' .. '9')* 'U'?;
K_3D: '.3d';
K_2D: '.2d';
K_1D: '.1d';

U_DEBUG_ABBREV: '.debug_abbrev';
U_DEBUG_INFO: '.debug_info';
U_DEBUG_LINE: '.debug_line';
U_DEBUG_LOC: '.debug_loc';
U_DEBUG_PUBNAMES: '.debug_pubnames';
U_DEBUG_RANGES: '.debug_ranges';
U_BYTE: '.byte';
U_4BYTE: '.4byte';
T_EQ: '=';
T_SEMICOLON: ';';
T_PLUS: '+';
T_OP: '(';
T_OC: '{';
T_OB: '[';
T_NOTEQ: '!=';
T_NOT: '!';
T_MINUS: '-';
T_GT: '>';
T_LT: '<';
T_ELLIPSIS: '...';
T_CP: ')';
T_COMMA:',';
T_COLON: ':';
T_CC: '}';
T_CB: ']';
LINE_COMMENT: '//' .*? ('\n' | '\r') -> channel(HIDDEN);
K_ZERO: '.zero';
K_XOR: '.xor';
K_WT: '.wt';
K_WIDTH: '.width';
K_WIDE: '.wide';
K_WB: '.wb';
K_VOLATILE: '.volatile';
K_VISIBLE: '.visible';
K_VERSION: '.version';
K_VB: '.vb';
K_V4: '.v4';
K_V2: '.v2';
K_UNI: '.uni';
K_U8: '.u8';
K_U64: '.u64';
K_U32: '.u32';
K_U16: '.u16';
K_TRAP: '.trap';
K_TO: '.to';
K_TEXREF: '.texref';
K_TEX: '.tex';
K_TARGET: '.target';
K_SYS: '.sys';
K_SYNC: '.sync';
K_SURFREF: '.surfref';
K_SUBNORMAL: '.subnormal';
K_SREG: '.sreg';
K_SHIFTAMT: '.shiftamt';
K_SHARED: '.shared';
K_SECTION: '.section';
K_SAT: '.sat';
K_SAMPLERREF: '.samplerref';
K_S8: '.s8';
K_S64: '.s64';
K_S32: '.s32';
K_S16: '.s16';
K_RZI: '.rzi';
K_RZ: '.rz';
K_RPI: '.rpi';
K_RP: '.rp';
K_RNI: '.rni';
K_RN: '.rn';
K_RMI: '.rmi';
K_RM: '.rm';
K_REQNTID: '.reqntid';
K_REG: '.reg';
K_RED: '.red';
K_RCP: '.rcp';
K_RC8: '.rc8';
K_RC16: '.rc16';
K_PRED: '.pred';
K_PRAGMA: '.pragma';
K_POPC: '.popc';
K_PARAM: '.param';
K_P: '.p';
K_OR: '.or';
K_OC: '.oc';
K_NUMBER: '.number';
K_NUM: '.num';
K_NS: '.ns';
K_NOUNROLL: '"nounroll"';
T_STRING:  '"' ( ~('"') )* '"';
K_NOTANUMBER: '.notanumber';
K_NORMALIZED_COORDS: '.normalized_coords';
K_NORMAL: '.normal';
K_NEU: '.neu';
K_NE: '.ne';
K_NAN: '.nan';
K_MINNCTAPERSM: '.minnctapersm';
K_MIN: '.min';
K_MAXNTID: '.maxntid';
K_MAXNREG: '.maxnreg';
K_MAXNCTAPERSM: '.maxnctapersm';
K_MAX: '.max';
K_LU: '.lu';
K_LTU: '.ltu';
K_LT: '.lt';
K_LS: '.ls';
K_LOCAL: '.local';
K_LOC: '.loc';
K_LO: '.lo';
K_LEU: '.leu';
K_LE: '.le';
K_L2: '.L2';
K_L1: '.L1';
K_INFINITE: '.infinite';
K_INC: '.inc';
K_HS: '.hs';
K_HI: '.hi';
K_HEIGHT: '.height';
K_GTU: '.gtu';
K_GT: '.gt';
K_GLOBAL: '.global';
K_GL: '.gl';
K_GEU: '.geu';
K_GE: '.ge';
K_FUNC: '.func';
K_FULL: '.full';
K_FTZ: '.ftz';
K_FINITE: '.finite';
K_FILTER_MODE: '.filter_mode';
K_FILE: '.file';
K_F64: '.f64';
K_F4E: '.f4e';
K_F32: '.f32';
K_F16: '.f16';
K_EXTERN: '.extern';
K_EXCH: '.exch';
K_EQU: '.equ';
K_EQ: '.eq';
K_ENTRY: '.entry';
K_ECR: '.ecr';
K_ECL: '.ecl';
K_DWARF: '@@DWARF' .*? ('\n' | '\r');
K_DEPTH: '.depth';
K_DEC: '.dec';
K_CV: '.cv';
K_CTA: '.cta';
K_CS: '.cs';
K_CONST: '.const';
K_CLAMP: '.clamp';
K_CHANNEL_ORDER: '.channel_order';
K_CHANNEL_DATA_TYPE: '.channel_data_type';
K_CHANNEL_DATA: '.channel_data';
K_CG: '.cg';
K_CC: '.cc';
K_CAS: '.cas';
K_CALLTARGETS: '.calltargets';
K_CALLPROTOTYPE: '.callprototype';
K_CA: '.ca';
K_BRANCHTARGETS: '.branchtargets';
K_BALLOT: '.ballot';
K_B8: '.b8';
K_B64: '.b64';
K_B4E: '.b4e';
K_B32: '.b32';
K_B16: '.b16';
K_ARRIVE: '.arrive';
K_APPROX: '.approx';
K_ANY: '.any';
K_AND: '.and';
K_ALL: '.all';
K_ALIGN: '.align';
K_ADDR_MODE_2: '.addr_mode_2';
K_ADDR_MODE_1: '.addr_mode_1';
K_ADDR_MODE_0: '.addr_mode_0';
K_ADDRESS_SIZE: '.address_size';
K_ADD: '.add';
K_X: '.x';
K_Y: '.y';
K_Z: '.z';
K_W: '.w';
K_A: '.a';
K_R: '.r';
K_G: '.g';
K_B: '.b';
COMMENT:   '/*' .*? '*/' -> channel(HIDDEN);
WS: (' '| '\t' | '\r' | '\n')+ -> channel(HIDDEN);
T_WORD: (('a'..'z' | 'A'..'Z' ) FollowSym*) | (('_' | '%' | '$' ) FollowSym+ );
T_UNDERSCORE: '_';
T_AT: '@';
T_PERCENT: '%';
T_SLASH: '/';
T_DOT: '.';

fragment
EscapeSequence
    :   '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
    |   OctalEscape
    ;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;

fragment
FollowSym
    : ( 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '$' )
    ;

