// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar nasm_x86_64_Parser;

options {
    caseInsensitive = true;
    tokenVocab = nasm_x86_64_Lexer;
}

program
    : line* EOF
    ;

line
    : label? (directive | times_prefix? (pseudoinstruction | instruction))? EOL
    ;

label
    : name COLON
    ;

directive
    : bits decimal_integer
    | use16
    | use32
    | default default_perfix
    | section section_params
    | absolute (integer | name)
    | (extern | required) extern_params
    | global global_params
    | common common_params
    | static name
    | cpu (decimal_integer | name)
    | float_name float_params
    | LEFT_BRACKET (warning warning_state? warning_class | map map_type name) RIGHT_BRACKET
    | org integer
    | (group name name | import_rule) name+
    | uppercase
    | export export_params
    | safeseh name
    | osabi decimal_integer
    ;

bits
    : BITS
    ;

decimal_integer
    : DECIMAL_INTEGER
    ;

use16
    : USE16
    ;

use32
    : USE32
    ;

default
    : DEFAULT
    ;

default_perfix
    : REL
    | ABS
    | BND
    | NOBND
    ;

section
    : SECTION
    | SEGMENT
    ;

section_params
    : name attribute? section_type? class? overlay? designation? allocation? execution? writing? starting_possition? follow? (
        use16
        | use32
    )? flat? (absolute_seg | alingment)? comdat? tls?
    ;

name
    : NAME
    ;

attribute
    : PRIVATE
    | PUBLIC
    | COMMON
    | STACK
    ;

section_type
    : CODE
    | TEXT
    | DATA
    | BSS
    | RDATA
    | INFO
    | MIXED
    | ZEROFILL
    | NO_DEAD_STRIP
    | LIVE_SUPPORT
    | STRIP_STATIC_SYMS
    | DEBUG
    ;

class
    : CLASS_ EQUAL_1 name
    ;

overlay
    : OVERLAY EQUAL_1 name
    ;

designation
    : PROGBITS
    | NOBITS
    | NOTE
    | PREINIT_ARRAY
    | INIT_ARRAY
    | FINI_ARRAY
    ;

allocation
    : ALLOC
    | NOALLOC
    ;

execution
    : EXEC
    | NOEXEC
    ;

writing
    : WRITE
    | NOWRITE
    ;

starting_possition
    : VSTART EQUAL_1 integer
    ;

follow
    : (FOLLOWS | VFOLLOWS) EQUAL_1 name
    ;

flat
    : FLAT
    ;

absolute_seg
    : ABSOLUTE EQUAL_1 integer
    ;

alingment
    : (ALIGN | START) EQUAL_1 integer
    | POINTER
    ;

comdat
    : COMDAT EQUAL_1 integer COLON name
    ;

tls
    : TLS
    ;

absolute
    : ABSOLUTE
    ;

integer
    : DECIMAL_INTEGER
    | OCT_INTEGER
    | HEX_INTEGER
    | BIN_INTEGER
    ;

extern
    : EXTERN
    ;

extern_params
    : name (COLON (wrt | weak))? (COMMA extern_params)*
    ;

wrt
    : name? WRT name (COLON integer)?
    ;

weak
    : WEAK
    ;

required
    : REQUIRED
    ;

global
    : GLOBAL
    ;

global_params
    : name (COLON global_type)? visibility? binding? expression? (COMMA global_params)*
    ;

global_type
    : FUNCTION
    | DATA
    | OBJECT
    ;

visibility
    : DEFAULT
    | INTERNAL
    | HIDDEN_
    | PROTECTED
    ;

binding
    : WEAK
    | STRONG
    ;

common
    : COMMON
    ;

common_params
    : name (integer (COLON (near | far | integer | wrt))?)+ integer?
    ;

near
    : NEAR
    ;

far
    : FAR
    ;

static
    : STATIC
    ;

cpu
    : CPU
    ;

float_name
    : FLOAT_NAME
    ;

float_params
    : DAZ
    | NODAZ
    | NEAR
    | UP
    | DOWN
    | ZERO
    | DEFAULT
    ;

warning
    : WARNING
    ;

warning_state
    : PLUS
    | MINUS
    | MULTIPLICATION
    ;

warning_class
    : warning_name
    | push
    | pop
    ;

warning_name
    : WARNING_NAME
    | NAME
    ;

push
    : PUSH
    ;

pop
    : POP
    ;

org
    : ORG
    ;

map
    : MAP
    ;

map_type
    : ALL
    | BRIEF
    | SECTIONS
    | SEGMENTS
    | SYMBOLS
    ;

group
    : GROUP
    ;

uppercase
    : UPPERCASE
    ;

import_rule
    : IMPORT
    ;

export
    : EXPORT
    ;

export_params
    : name name? (resident | nodata | parm | integer)*
    ;

resident
    : RESIDENT
    ;

nodata
    : NODATA
    ;

parm
    : PARM EQUAL_1 integer
    ;

safeseh
    : SAFESEH
    ;

osabi
    : OSABI
    ;

times_prefix
    : times (expression | integer)
    ;

times
    : TIMES
    ;

pseudoinstruction
    : name? (
        dx value (COMMA value)*
        | resx integer
        | incbin atom (COMMA atom)*
        | equ (integer | expression)
    )
    ;

dx
    : DB
    | DW
    | DD
    | DQ
    | DT
    | DO
    | DY
    | DZ
    ;

float_number
    : FLOAT_NUMBER
    ;

question
    : QUESTION
    ;

resx
    : RESB
    | RESW
    | RESD
    | RESQ
    | REST
    | RESO
    | RESY
    | RESZ
    ;

incbin
    : INCBIN
    ;

string
    : STRING
    ;

value
    : atom
    | size value
    | list
    | macro_call
    ;

atom
    : integer
    | float_number
    | string
    | name
    | question
    | expression
    ;

size
    : BYTE
    | WORD
    | DWORD
    | QWORD
    | TWORD
    | OWORD
    | YWORD
    | ZWORD
    ;

list
    : duplist
    | PERCENT parlist
    | size PERCENT? parlist
    ;

duplist
    : expression DUP size? PERCENT? parlist
    ;

parlist
    : LEFT_PARENTHESIS value (COMMA value)* RIGHT_PARENTHESIS
    ;

unaryExpression
    : unaryOperator castExpression
    | LEFT_PARENTHESIS expression RIGHT_PARENTHESIS
    ;

unaryOperator
    : PLUS
    | MINUS
    | BITWISE_NOT
    | BOOLEAN_NOT
    ;

castExpression
    : unaryExpression
    | integer
    | register
    | (register COLON)? name
    | string
    | float_number
    | DOLLAR
    | DOUBLE_DOLLAR
    ;

multiplicativeExpression
    : castExpression (
        (MULTIPLICATION | UNSIGNED_DIVISION | SIGNED_DIVISION | PERCENT | SIGNED_MODULE) castExpression
    )*
    ;

additiveExpression
    : multiplicativeExpression ((PLUS | MINUS) multiplicativeExpression)*
    ;

shiftExpression
    : additiveExpression (
        (LEFT_SHIFT | RIGHT_SHIFT | LEFT_SHIFT_COMPLETENESS | RIGHT_SHIFT_COMPLETENESS) additiveExpression
    )*
    ;

relationalExpression
    : shiftExpression (
        (LESS_THAN | LESS_THAN_EQUAL | GREATER_THAN | GREATER_THAN | SIGNED_COMPARISON) shiftExpression
    )*
    ;

equalityExpression
    : relationalExpression ((EQUAL_1 | EQUAL_2 | NOT_EQUAL_1 | NOT_EQUAL_2) relationalExpression)*
    ;

andExpression
    : equalityExpression (BITWISE_AND equalityExpression)*
    ;

exclusiveOrExpression
    : andExpression (BITWISE_XOR andExpression)*
    ;

inclusiveOrExpression
    : exclusiveOrExpression (BITWISE_OR exclusiveOrExpression)*
    ;

booleanAndExpression
    : inclusiveOrExpression (BOOLEAN_AND inclusiveOrExpression)*
    ;

booleanOrExpression
    : booleanAndExpression (BOOLEAN_OR booleanAndExpression)*
    ;

conditionalExpression
    : booleanOrExpression (QUESTION integer COLON conditionalExpression)?
    ;

expression
    : conditionalExpression
    ;

equ
    : EQU
    ;

instruction
    : opcode operand? (COMMA operand)*
    | macro_call
    ;

opcode
    : AAA
    | AAD
    | AAM
    | AAS
    | ADC
    | ADD
    | AND
    | ARPL
    | BB0_RESET
    | BB1_RESET
    | BOUND
    | BSF
    | BSR
    | BSWAP
    | BT
    | BTC
    | BTR
    | BTS
    | CALL
    | CBW
    | CDQ
    | CDQE
    | CLC
    | CLD
    | CLI
    | CLTS
    | CMC
    | CMOVA
    | CMOVAE
    | CMOVB
    | CMOVBE
    | CMOVC
    | CMOVE
    | CMOVGE
    | CMOVL
    | CMOVLE
    | CMOVNA
    | CMOVNAE
    | CMOVNB
    | CMOVNBE
    | CMOVNC
    | CMOVNE
    | CMOVNG
    | CMOVNGE
    | CMOVNL
    | CMOVNO
    | CMOVNP
    | CMOVNS
    | CMOVNZ
    | CMOVO
    | CMOVP
    | CMOVPE
    | CMOVPO
    | CMOVS
    | CMOVZ
    | CMP
    | CMPSB
    | CMPSD
    | CMPSQ
    | CMPSW
    | CMPXCHG
    | CMPXCHG16B
    | CMPXCHG486
    | CMPXCHG8B
    | CPU_READ
    | CPU_WRITE
    | CPUID
    | CQO
    | CWD
    | CWDE
    | DAA
    | DAS
    | DEC
    | DIV
    | DMINT
    | EMMS
    | ENTER
    | EQU
    | F2XM1
    | FABS
    | FADD
    | FADDP
    | FBLD
    | FBSTP
    | FCHS
    | FCLEX
    | FCMOVB
    | FCMOVBE
    | FCMOVE
    | FCMOVNB
    | FCMOVNBE
    | FCMOVNE
    | FCMOVNU
    | FCMOVU
    | FCOM
    | FCOMI
    | FCOMIP
    | FCOMP
    | FCOMPP
    | FCOS
    | FDECSTP
    | FDISI
    | FDIV
    | FDIVP
    | FDIVR
    | FDIVRP
    | FEMMS
    | FENI
    | FFREE
    | FFREEP
    | FIADD
    | FICOM
    | FICOMP
    | FIDIV
    | FIDIVR
    | FILD
    | FIMUL
    | FINCSTP
    | FINIT
    | FIST
    | FISTP
    | FISTTP
    | FISUB
    | FISUBR
    | FLD
    | FLD1
    | FLDCW
    | FLDENV
    | FLDL2E
    | FLDL2T
    | FLDLG2
    | FLDLN2
    | FLDPI
    | FLDZ
    | FMUL
    | FMULP
    | FNCLEX
    | FNDISI
    | FNENI
    | FNINIT
    | FNOP
    | FNSAVE
    | FNSTCW
    | FNSTENV
    | FNSTSW
    | FPATAN
    | FPREM
    | FPREM1
    | FPTAN
    | FRNDINT
    | FRSTOR
    | FSAVE
    | FSCALE
    | FSETPM
    | FSIN
    | FSINCOS
    | FSQRT
    | FST
    | FSTCW
    | FSTENV
    | FSTP
    | FSTSW
    | FSUB
    | FSUBP
    | FSUBR
    | FSUBRP
    | FTST
    | FUCOM
    | FUCOMI
    | FUCOMIP
    | FUCOMP
    | FUCOMPP
    | FWAIT
    | FXAM
    | FXCH
    | FXTRACT
    | FYL2X
    | FYL2XP1
    | HLT
    | IBTS
    | ICEBP
    | IDIV
    | IMUL
    | IN
    | INC
    | INSB
    | INSD
    | INSW
    | INT
    | INT01
    | INT03
    | INT1
    | INT3
    | INTO
    | INVD
    | INVLPG
    | INVLPGA
    | INVPCID
    | IRET
    | IRETD
    | IRETQ
    | IRETW
    | JA
    | JAE
    | JB
    | JBE
    | JC
    | JCXZ
    | JE
    | JECXZ
    | JG
    | JGE
    | JL
    | JLE
    | JMP
    | JMPE
    | JNA
    | JNAE
    | JNB
    | JNBE
    | JNC
    | JNE
    | JNG
    | JNGE
    | JNL
    | JNLE
    | JNO
    | JNP
    | JNS
    | JNZ
    | JO
    | JP
    | JPE
    | JPO
    | JRCXZ
    | JS
    | JZ
    | LAHF
    | LAR
    | LDS
    | LEA
    | LEAVE
    | LES
    | LFENCE
    | LFS
    | LGDT
    | LGS
    | LIDT
    | LLDT
    | LMSW
    | LOADALL
    | LOADALL286
    | LODSB
    | LODSD
    | LODSQ
    | LODSW
    | LOOP
    | LOOPE
    | LOOPNE
    | LOOPNZ
    | LOOPZ
    | LSL
    | LSS
    | LTR
    | MFENCE
    | MONITOR
    | MONITORX
    | MOV
    | MOVD
    | MOVQ
    | MOVSB
    | MOVSD
    | MOVSQ
    | MOVSW
    | MOVSX
    | MOVSXD
    | MOVZX
    | MUL
    | MWAIT
    | MWAITX
    | NEG
    | NOP
    | NOT
    | OR
    | OUT
    | OUTSB
    | OUTSD
    | OUTSW
    | PACKSSDW
    | PACKSSWB
    | PACKUSWB
    | PADDB
    | PADDD
    | PADDSB
    | PADDSIW
    | PADDSW
    | PADDUSB
    | PADDUSW
    | PADDW
    | PAND
    | PANDN
    | PAUSE
    | PAVEB
    | PAVGUSB
    | PCMPEQB
    | PCMPEQD
    | PCMPEQW
    | PCMPGTB
    | PCMPGTD
    | PCMPGTW
    | PDISTIB
    | PF2ID
    | PFACC
    | PFADD
    | PFCMPEQ
    | PFCMPGE
    | PFCMPGT
    | PFMAX
    | PFMIN
    | PFMUL
    | PFRCP
    | PFRCPIT1
    | PFRCPIT2
    | PFRSQIT1
    | PFRSQRT
    | PFSUB
    | PFSUBR
    | PI2FD
    | PMACHRIW
    | PMADDWD
    | PMAGW
    | PMULHRIW
    | PMULHRWA
    | PMULHRWC
    | PMULHW
    | PMULLW
    | PMVGEZB
    | PMVLZB
    | PMVNZB
    | PMVZB
    | POP
    | POPA
    | POPAD
    | POPAW
    | POPF
    | POPFD
    | POPFQ
    | POPFW
    | POR
    | PREFETCH
    | PREFETCHW
    | PSLLD
    | PSLLQ
    | PSLLW
    | PSRAD
    | PSRAW
    | PSRLD
    | PSRLQ
    | PSRLW
    | PSUBB
    | PSUBD
    | PSUBSB
    | PSUBSIW
    | PSUBSW
    | PSUBUSB
    | PSUBUSW
    | PSUBW
    | PUNPCKHBW
    | PUNPCKHDQ
    | PUNPCKHWD
    | PUNPCKLBW
    | PUNPCKLDQ
    | PUNPCKLWD
    | PUSH
    | PUSHA
    | PUSHAD
    | PUSHAW
    | PUSHF
    | PUSHFD
    | PUSHFQ
    | PUSHFW
    | PXOR
    | RCL
    | RCR
    | RDM
    | RDMSR
    | RDPMC
    | RDSHR
    | RDTSC
    | RDTSCP
    | RET
    | RETD
    | RETF
    | RETFD
    | RETFQ
    | RETFW
    | RETN
    | RETND
    | RETNQ
    | RETNW
    | RETQ
    | RETW
    | ROL
    | ROR
    | RSDC
    | RSLDT
    | RSM
    | RSTS
    | SAHF
    | SAL
    | SALC
    | SAR
    | SBB
    | SCASB
    | SCASD
    | SCASQ
    | SCASW
    | SETA
    | SETAE
    | SETB
    | SETBE
    | SETC
    | SETE
    | SETG
    | SETGE
    | SETL
    | SETLE
    | SETNA
    | SETNAE
    | SETNB
    | SETNBE
    | SETNC
    | SETNE
    | SETNG
    | SETNGE
    | SETNL
    | SETNLE
    | SETNO
    | SETNP
    | SETNS
    | SETNZ
    | SETO
    | SETP
    | SETPE
    | SETPO
    | SETS
    | SETZ
    | SFENCE
    | SGDT
    | SHL
    | SHLD
    | SHR
    | SHRD
    | SIDT
    | SKINIT
    | SLDT
    | SMI
    | SMINT
    | SMINTOLD
    | SMSW
    | STC
    | STD
    | STI
    | STOSB
    | STOSD
    | STOSQ
    | STOSW
    | STR
    | SUB
    | SVDC
    | SVLDT
    | SVTS
    | SWAPGS
    | SYSCALL
    | SYSENTER
    | SYSEXIT
    | SYSRET
    | TEST
    | UD0
    | UD1
    | UD2
    | UD2A
    | UD2B
    | UMOV
    | VERR
    | VERW
    | WBINVD
    | WRMSR
    | WRSHR
    | XADD
    | XBTS
    | XCHG
    | XLAT
    | XLATB
    | XOR
    | AADD
    | AAND
    | ADCX
    | ADDPD
    | ADDPS
    | ADDSD
    | ADDSS
    | ADDSUBPD
    | ADDSUBPS
    | ADOX
    | AESDEC
    | AESDECLAST
    | AESENC
    | AESENCLAST
    | AESIMC
    | AESKEYGENASSIST
    | ANDN
    | ANDNPD
    | ANDNPS
    | ANDPD
    | ANDPS
    | AXOR
    | BEXTR
    | BLCFILL
    | BLCI
    | BLCIC
    | BLCMSK
    | BLCS
    | BLENDPD
    | BLENDPS
    | BLENDVPD
    | BLENDVPS
    | BLSFILL
    | BLSI
    | BLSIC
    | BLSMSK
    | BLSR
    | BNDCL
    | BNDCN
    | BNDCU
    | BNDLDX
    | BNDMK
    | BNDMOV
    | BNDSTX
    | BZHI
    | CLAC
    | CLDEMOTE
    | CLFLUSH
    | CLFLUSHOPT
    | CLGI
    | CLRSSBSY
    | CLUI
    | CLWB
    | CLZERO
    | CMPEQPD
    | CMPEQPS
    | CMPEQSD
    | CMPEQSS
    | CMPLEPD
    | CMPLEPS
    | CMPLESD
    | CMPLESS
    | CMPLTPD
    | CMPLTPS
    | CMPLTSD
    | CMPLTSS
    | CMPNEQPD
    | CMPNEQPS
    | CMPNEQSD
    | CMPNEQSS
    | CMPNLEPD
    | CMPNLEPS
    | CMPNLESD
    | CMPNLESS
    | CMPNLTPD
    | CMPNLTPS
    | CMPNLTSD
    | CMPNLTSS
    | CMPNPXADD
    | CMPNSXADD
    | CMPNZXADD
    | CMPORDPD
    | CMPORDPS
    | CMPORDSD
    | CMPORDSS
    | CMPOXADD
    | CMPPD
    | CMPPS
    | CMPPXADD
    | CMPSS
    | CMPSXADD
    | CMPUNORDPD
    | CMPUNORDPS
    | CMPUNORDSD
    | CMPUNORDSS
    | CMPZXADD
    | COMISD
    | COMISS
    | CRC32
    | CVTDQ2PD
    | CVTDQ2PS
    | CVTPD2DQ
    | CVTPD2PI
    | CVTPD2PS
    | CVTPI2PD
    | CVTPI2PS
    | CVTPS2DQ
    | CVTPS2PD
    | CVTPS2PI
    | CVTSD2SI
    | CVTSD2SS
    | CVTSI2SD
    | CVTSI2SS
    | CVTSS2SD
    | CVTSS2SI
    | CVTTPD2DQ
    | CVTTPD2PI
    | CVTTPS2DQ
    | CVTTPS2PI
    | CVTTSD2SI
    | CVTTSS2SI
    | DIVPD
    | DIVPS
    | DIVSD
    | DIVSS
    | DPPD
    | DPPS
    | ENCLS
    | ENCLU
    | ENCLV
    | ENDBR32
    | ENDBR64
    | ENQCMD
    | ENQCMDS
    | EXTRACTPS
    | EXTRQ
    | FXRSTOR
    | FXRSTOR64
    | FXSAVE
    | FXSAVE64
    | GETSEC
    | GF2P8AFFINEINVQB
    | GF2P8AFFINEQB
    | GF2P8MULB
    | HADDPD
    | HADDPS
    | HINT_NOP0
    | HINT_NOP1
    | HINT_NOP10
    | HINT_NOP11
    | HINT_NOP12
    | HINT_NOP13
    | HINT_NOP14
    | HINT_NOP15
    | HINT_NOP16
    | HINT_NOP17
    | HINT_NOP18
    | HINT_NOP19
    | HINT_NOP2
    | HINT_NOP20
    | HINT_NOP21
    | HINT_NOP22
    | HINT_NOP23
    | HINT_NOP24
    | HINT_NOP25
    | HINT_NOP26
    | HINT_NOP27
    | HINT_NOP28
    | HINT_NOP29
    | HINT_NOP3
    | HINT_NOP30
    | HINT_NOP31
    | HINT_NOP32
    | HINT_NOP33
    | HINT_NOP34
    | HINT_NOP35
    | HINT_NOP36
    | HINT_NOP37
    | HINT_NOP38
    | HINT_NOP39
    | HINT_NOP4
    | HINT_NOP40
    | HINT_NOP41
    | HINT_NOP42
    | HINT_NOP43
    | HINT_NOP44
    | HINT_NOP45
    | HINT_NOP46
    | HINT_NOP47
    | HINT_NOP48
    | HINT_NOP49
    | HINT_NOP5
    | HINT_NOP50
    | HINT_NOP51
    | HINT_NOP52
    | HINT_NOP53
    | HINT_NOP54
    | HINT_NOP55
    | HINT_NOP56
    | HINT_NOP57
    | HINT_NOP58
    | HINT_NOP59
    | HINT_NOP6
    | HINT_NOP60
    | HINT_NOP61
    | HINT_NOP62
    | HINT_NOP63
    | HINT_NOP7
    | HINT_NOP8
    | HINT_NOP9
    | HRESET
    | HSUBPD
    | HSUBPS
    | INCSSPD
    | INCSSPQ
    | INSERTPS
    | INSERTQ
    | INVEPT
    | INVVPID
    | KADD
    | KADDB
    | KADDD
    | KADDQ
    | KADDW
    | KAND
    | KANDB
    | KANDD
    | KANDN
    | KANDNB
    | KANDND
    | KANDNQ
    | KANDNW
    | KANDQ
    | KANDW
    | KMOV
    | KMOVB
    | KMOVD
    | KMOVQ
    | KMOVW
    | KNOT
    | KNOTB
    | KNOTD
    | KNOTQ
    | KNOTW
    | KOR
    | KORB
    | KORD
    | KORQ
    | KORTEST
    | KORTESTB
    | KORTESTD
    | KORTESTQ
    | KORTESTW
    | KORW
    | KSHIFTL
    | KSHIFTLB
    | KSHIFTLD
    | KSHIFTLQ
    | KSHIFTLW
    | KSHIFTR
    | KSHIFTRB
    | KSHIFTRD
    | KSHIFTRQ
    | KSHIFTRW
    | KTEST
    | KTESTB
    | KTESTD
    | KTESTQ
    | KTESTW
    | KUNPCK
    | KUNPCKBW
    | KUNPCKDQ
    | KUNPCKWD
    | KXNOR
    | KXNORB
    | KXNORD
    | KXNORQ
    | KXNORW
    | KXOR
    | KXORB
    | KXORD
    | KXORQ
    | KXORW
    | LDDQU
    | LDMXCSR
    | LDTILECFG
    | LLWPCB
    | LWPINS
    | LWPVAL
    | LZCNT
    | MASKMOVDQU
    | MASKMOVQ
    | MAXPD
    | MAXPS
    | MAXSD
    | MAXSS
    | MINPD
    | MINPS
    | MINSD
    | MINSS
    | MONTMUL
    | MOVAPD
    | MOVAPS
    | MOVBE
    | MOVDDUP
    | MOVDIR64B
    | MOVDIRI
    | MOVDQ2Q
    | MOVDQA
    | MOVDQU
    | MOVHLPS
    | MOVHPD
    | MOVHPS
    | MOVLHPS
    | MOVLPD
    | MOVLPS
    | MOVMSKPD
    | MOVMSKPS
    | MOVNTDQ
    | MOVNTDQA
    | MOVNTI
    | MOVNTPD
    | MOVNTPS
    | MOVNTQ
    | MOVNTSD
    | MOVNTSS
    | MOVQ2DQ
    | MOVSHDUP
    | MOVSLDUP
    | MOVSS
    | MOVUPD
    | MOVUPS
    | MPSADBW
    | MULPD
    | MULPS
    | MULSD
    | MULSS
    | MULX
    | ORPD
    | ORPS
    | PABSB
    | PABSD
    | PABSW
    | PACKUSDW
    | PADDQ
    | PALIGNR
    | PAVGB
    | PAVGW
    | PBLENDVB
    | PBLENDW
    | PCLMULHQHQDQ
    | PCLMULHQLQDQ
    | PCLMULLQHQDQ
    | PCLMULLQLQDQ
    | PCLMULQDQ
    | PCMPEQQ
    | PCMPESTRI
    | PCMPESTRM
    | PCMPGTQ
    | PCMPISTRI
    | PCMPISTRM
    | PCOMMIT
    | PCONFIG
    | PDEP
    | PEXT
    | PEXTRB
    | PEXTRD
    | PEXTRQ
    | PEXTRW
    | PF2IW
    | PFNACC
    | PFPNACC
    | PFRCPV
    | PFRSQRTV
    | PHADDD
    | PHADDSW
    | PHADDW
    | PHMINPOSUW
    | PHSUBD
    | PHSUBSW
    | PHSUBW
    | PI2FW
    | PINSRB
    | PINSRD
    | PINSRQ
    | PINSRW
    | PMADDUBSW
    | PMAXSB
    | PMAXSD
    | PMAXSW
    | PMAXUB
    | PMAXUD
    | PMAXUW
    | PMINSB
    | PMINSD
    | PMINSW
    | PMINUB
    | PMINUD
    | PMINUW
    | PMOVMSKB
    | PMOVSXBD
    | PMOVSXBQ
    | PMOVSXBW
    | PMOVSXDQ
    | PMOVSXWD
    | PMOVSXWQ
    | PMOVZXBD
    | PMOVZXBQ
    | PMOVZXBW
    | PMOVZXDQ
    | PMOVZXWD
    | PMOVZXWQ
    | PMULDQ
    | PMULHRSW
    | PMULHUW
    | PMULLD
    | PMULUDQ
    | POPCNT
    | PREFETCHIT0
    | PREFETCHIT1
    | PREFETCHNTA
    | PREFETCHT0
    | PREFETCHT1
    | PREFETCHT2
    | PREFETCHWT1
    | PSADBW
    | PSHUFB
    | PSHUFD
    | PSHUFHW
    | PSHUFLW
    | PSHUFW
    | PSIGNB
    | PSIGND
    | PSIGNW
    | PSLLDQ
    | PSRLDQ
    | PSUBQ
    | PSWAPD
    | PTEST
    | PTWRITE
    | PUNPCKHQDQ
    | PUNPCKLQDQ
    | PVALIDATE
    | RCPPS
    | RCPSS
    | RDFSBASE
    | RDGSBASE
    | RDMSRLIST
    | RDPID
    | RDPKRU
    | RDRAND
    | RDSEED
    | RDSSPD
    | RDSSPQ
    | RMPADJUST
    | RORX
    | ROUNDPD
    | ROUNDPS
    | ROUNDSD
    | ROUNDSS
    | RSQRTPS
    | RSQRTSS
    | RSTORSSP
    | SARX
    | SAVEPREVSSP
    | SENDUIPI
    | SERIALIZE
    | SETSSBSY
    | SHA1MSG1
    | SHA1MSG2
    | SHA1NEXTE
    | SHA1RNDS4
    | SHA256MSG1
    | SHA256MSG2
    | SHA256RNDS2
    | SHLX
    | SHRX
    | SHUFPD
    | SHUFPS
    | SLWPCB
    | SQRTPD
    | SQRTPS
    | SQRTSD
    | SQRTSS
    | STAC
    | STGI
    | STMXCSR
    | STTILECFG
    | STUI
    | SUBPD
    | SUBPS
    | SUBSD
    | SUBSS
    | T1MSKC
    | TDPBF16PS
    | TDPBSSD
    | TDPBSUD
    | TDPBUSD
    | TDPBUUD
    | TESTUI
    | TILELOADD
    | TILELOADDT1
    | TILERELEASE
    | TILESTORED
    | TILEZERO
    | TPAUSE
    | TZCNT
    | TZMSK
    | UCOMISD
    | UCOMISS
    | UIRET
    | UMONITOR
    | UMWAIT
    | UNPCKHPD
    | UNPCKHPS
    | UNPCKLPD
    | UNPCKLPS
    | V4DPWSSD
    | V4DPWSSDS
    | V4FMADDPS
    | V4FMADDSS
    | V4FNMADDPS
    | V4FNMADDSS
    | VADDPD
    | VADDPH
    | VADDPS
    | VADDSD
    | VADDSH
    | VADDSS
    | VADDSUBPD
    | VADDSUBPS
    | VAESDEC
    | VAESDECLAST
    | VAESENC
    | VAESENCLAST
    | VAESIMC
    | VAESKEYGENASSIST
    | VALIGND
    | VALIGNQ
    | VANDNPD
    | VANDNPS
    | VANDPD
    | VANDPS
    | VBCSTNEBF16PS
    | VBCSTNESH2PS
    | VBLENDMPD
    | VBLENDMPS
    | VBLENDPD
    | VBLENDPS
    | VBLENDVPD
    | VBLENDVPS
    | VBROADCASTF128
    | VBROADCASTF32X2
    | VBROADCASTF32X4
    | VBROADCASTF32X8
    | VBROADCASTF64X2
    | VBROADCASTF64X4
    | VBROADCASTI128
    | VBROADCASTI32X2
    | VBROADCASTI32X4
    | VBROADCASTI32X8
    | VBROADCASTI64X2
    | VBROADCASTI64X4
    | VBROADCASTSD
    | VBROADCASTSS
    | VCMPEQ_OQPD
    | VCMPEQ_OQPS
    | VCMPEQ_OQSD
    | VCMPEQ_OQSS
    | VCMPEQ_OSPD
    | VCMPEQ_OSPS
    | VCMPEQ_OSSD
    | VCMPEQ_OSSS
    | VCMPEQ_UQPD
    | VCMPEQ_UQPS
    | VCMPEQ_UQSD
    | VCMPEQ_UQSS
    | VCMPEQ_USPD
    | VCMPEQ_USPS
    | VCMPEQ_USSD
    | VCMPEQ_USSS
    | VCMPEQPD
    | VCMPEQPS
    | VCMPEQSD
    | VCMPEQSS
    | VCMPFALSE_OQPD
    | VCMPFALSE_OQPS
    | VCMPFALSE_OQSD
    | VCMPFALSE_OQSS
    | VCMPFALSE_OSPD
    | VCMPFALSE_OSPS
    | VCMPFALSE_OSSD
    | VCMPFALSE_OSSS
    | VCMPFALSEPD
    | VCMPFALSEPS
    | VCMPFALSESD
    | VCMPFALSESS
    | VCMPGE_OQPD
    | VCMPGE_OQPS
    | VCMPGE_OQSD
    | VCMPGE_OQSS
    | VCMPGE_OSPD
    | VCMPGE_OSPS
    | VCMPGE_OSSD
    | VCMPGE_OSSS
    | VCMPGEPD
    | VCMPGEPS
    | VCMPGESD
    | VCMPGESS
    | VCMPGT_OQPD
    | VCMPGT_OQPS
    | VCMPGT_OQSD
    | VCMPGT_OQSS
    | VCMPGT_OSPD
    | VCMPGT_OSPS
    | VCMPGT_OSSD
    | VCMPGT_OSSS
    | VCMPGTPD
    | VCMPGTPS
    | VCMPGTSD
    | VCMPGTSS
    | VCMPLE_OQPD
    | VCMPLE_OQPS
    | VCMPLE_OQSD
    | VCMPLE_OQSS
    | VCMPLE_OSPD
    | VCMPLE_OSPS
    | VCMPLE_OSSD
    | VCMPLE_OSSS
    | VCMPLEPD
    | VCMPLEPS
    | VCMPLESD
    | VCMPLESS
    | VCMPLT_OQPD
    | VCMPLT_OQPS
    | VCMPLT_OQSD
    | VCMPLT_OQSS
    | VCMPLT_OSPD
    | VCMPLT_OSPS
    | VCMPLT_OSSD
    | VCMPLT_OSSS
    | VCMPLTPD
    | VCMPLTPS
    | VCMPLTSD
    | VCMPLTSS
    | VCMPNEQ_OQPD
    | VCMPNEQ_OQPS
    | VCMPNEQ_OQSD
    | VCMPNEQ_OQSS
    | VCMPNEQ_OSPD
    | VCMPNEQ_OSPS
    | VCMPNEQ_OSSD
    | VCMPNEQ_OSSS
    | VCMPNEQ_UQPD
    | VCMPNEQ_UQPS
    | VCMPNEQ_UQSD
    | VCMPNEQ_UQSS
    | VCMPNEQ_USPD
    | VCMPNEQ_USPS
    | VCMPNEQ_USSD
    | VCMPNEQ_USSS
    | VCMPNEQPD
    | VCMPNEQPS
    | VCMPNEQSD
    | VCMPNEQSS
    | VCMPNGE_UQPD
    | VCMPNGE_UQPS
    | VCMPNGE_UQSD
    | VCMPNGE_UQSS
    | VCMPNGE_USPD
    | VCMPNGE_USPS
    | VCMPNGE_USSD
    | VCMPNGE_USSS
    | VCMPNGEPD
    | VCMPNGEPS
    | VCMPNGESD
    | VCMPNGESS
    | VCMPNGT_UQPD
    | VCMPNGT_UQPS
    | VCMPNGT_UQSD
    | VCMPNGT_UQSS
    | VCMPNGT_USPD
    | VCMPNGT_USPS
    | VCMPNGT_USSD
    | VCMPNGT_USSS
    | VCMPNGTPD
    | VCMPNGTPS
    | VCMPNGTSD
    | VCMPNGTSS
    | VCMPNLE_UQPD
    | VCMPNLE_UQPS
    | VCMPNLE_UQSD
    | VCMPNLE_UQSS
    | VCMPNLE_USPD
    | VCMPNLE_USPS
    | VCMPNLE_USSD
    | VCMPNLE_USSS
    | VCMPNLEPD
    | VCMPNLEPS
    | VCMPNLESD
    | VCMPNLESS
    | VCMPNLT_UQPD
    | VCMPNLT_UQPS
    | VCMPNLT_UQSD
    | VCMPNLT_UQSS
    | VCMPNLT_USPD
    | VCMPNLT_USPS
    | VCMPNLT_USSD
    | VCMPNLT_USSS
    | VCMPNLTPD
    | VCMPNLTPS
    | VCMPNLTSD
    | VCMPNLTSS
    | VCMPORD_QPD
    | VCMPORD_QPS
    | VCMPORD_QSD
    | VCMPORD_QSS
    | VCMPORD_SPD
    | VCMPORD_SPS
    | VCMPORD_SSD
    | VCMPORD_SSS
    | VCMPORDPD
    | VCMPORDPS
    | VCMPORDSD
    | VCMPORDSS
    | VCMPPD
    | VCMPPH
    | VCMPPS
    | VCMPSD
    | VCMPSH
    | VCMPSS
    | VCMPTRUE_UQPD
    | VCMPTRUE_UQPS
    | VCMPTRUE_UQSD
    | VCMPTRUE_UQSS
    | VCMPTRUE_USPD
    | VCMPTRUE_USPS
    | VCMPTRUE_USSD
    | VCMPTRUE_USSS
    | VCMPTRUEPD
    | VCMPTRUEPS
    | VCMPTRUESD
    | VCMPTRUESS
    | VCMPUNORD_QPD
    | VCMPUNORD_QPS
    | VCMPUNORD_QSD
    | VCMPUNORD_QSS
    | VCMPUNORD_SPD
    | VCMPUNORD_SPS
    | VCMPUNORD_SSD
    | VCMPUNORD_SSS
    | VCMPUNORDPD
    | VCMPUNORDPS
    | VCMPUNORDSD
    | VCMPUNORDSS
    | VCOMISD
    | VCOMISH
    | VCOMISS
    | VCOMPRESSPD
    | VCOMPRESSPS
    | VCVTDQ2PD
    | VCVTDQ2PH
    | VCVTDQ2PS
    | VCVTNE2PS2BF16
    | VCVTNEEBF162PS
    | VCVTNEEPH2PS
    | VCVTNEOBF162PS
    | VCVTNEOPH2PS
    | VCVTNEPS2BF16
    | VCVTPD2DQ
    | VCVTPD2PH
    | VCVTPD2PS
    | VCVTPD2QQ
    | VCVTPD2UDQ
    | VCVTPD2UQQ
    | VCVTPH2DQ
    | VCVTPH2PD
    | VCVTPH2PS
    | VCVTPH2PSX
    | VCVTPH2QQ
    | VCVTPH2UDQ
    | VCVTPH2UQQ
    | VCVTPH2UW
    | VCVTPH2W
    | VCVTPS2DQ
    | VCVTPS2PD
    | VCVTPS2PH
    | VCVTPS2QQ
    | VCVTPS2UDQ
    | VCVTPS2UQQ
    | VCVTQQ2PD
    | VCVTQQ2PH
    | VCVTQQ2PS
    | VCVTSD2SH
    | VCVTSD2SI
    | VCVTSD2SS
    | VCVTSD2USI
    | VCVTSH2SD
    | VCVTSH2SI
    | VCVTSH2SS
    | VCVTSH2USI
    | VCVTSI2SD
    | VCVTSI2SH
    | VCVTSI2SS
    | VCVTSS2SD
    | VCVTSS2SH
    | VCVTSS2SI
    | VCVTSS2USI
    | VCVTTPD2DQ
    | VCVTTPD2QQ
    | VCVTTPD2UDQ
    | VCVTTPD2UQQ
    | VCVTTPH2DQ
    | VCVTTPH2QQ
    | VCVTTPH2UDQ
    | VCVTTPH2UQQ
    | VCVTTPH2UW
    | VCVTTPH2W
    | VCVTTPS2DQ
    | VCVTTPS2QQ
    | VCVTTPS2UDQ
    | VCVTTPS2UQQ
    | VCVTTSD2SI
    | VCVTTSD2USI
    | VCVTTSH2SI
    | VCVTTSH2USI
    | VCVTTSS2SI
    | VCVTTSS2USI
    | VCVTUDQ2PD
    | VCVTUDQ2PH
    | VCVTUDQ2PS
    | VCVTUQQ2PD
    | VCVTUQQ2PH
    | VCVTUQQ2PS
    | VCVTUSI2SD
    | VCVTUSI2SH
    | VCVTUSI2SS
    | VCVTUW2PH
    | VCVTW2PH
    | VDBPSADBW
    | VDIVPD
    | VDIVPH
    | VDIVPS
    | VDIVSD
    | VDIVSH
    | VDIVSS
    | VDPBF16PS
    | VDPPD
    | VDPPS
    | VENDSCALEPH
    | VENDSCALESH
    | VEXP2PD
    | VEXP2PS
    | VEXPANDPD
    | VEXPANDPS
    | VEXTRACTF128
    | VEXTRACTF32X4
    | VEXTRACTF32X8
    | VEXTRACTF64X2
    | VEXTRACTF64X4
    | VEXTRACTI128
    | VEXTRACTI32X4
    | VEXTRACTI32X8
    | VEXTRACTI64X2
    | VEXTRACTI64X4
    | VEXTRACTPS
    | VFCMADDCPH
    | VFCMADDCSH
    | VFCMULCPCH
    | VFCMULCSH
    | VFIXUPIMMPD
    | VFIXUPIMMPS
    | VFIXUPIMMSD
    | VFIXUPIMMSS
    | VFMADD123PD
    | VFMADD123PS
    | VFMADD123SD
    | VFMADD123SS
    | VFMADD132PD
    | VFMADD132PH
    | VFMADD132PS
    | VFMADD132SD
    | VFMADD132SS
    | VFMADD213PD
    | VFMADD213PH
    | VFMADD213PS
    | VFMADD213SD
    | VFMADD213SS
    | VFMADD231PD
    | VFMADD231PH
    | VFMADD231PS
    | VFMADD231SD
    | VFMADD231SS
    | VFMADD312PD
    | VFMADD312PS
    | VFMADD312SD
    | VFMADD312SS
    | VFMADD321PD
    | VFMADD321PS
    | VFMADD321SD
    | VFMADD321SS
    | VFMADDCPH
    | VFMADDCSH
    | VFMADDPD
    | VFMADDPS
    | VFMADDSD
    | VFMADDSS
    | VFMADDSUB123PD
    | VFMADDSUB123PS
    | VFMADDSUB132PD
    | VFMADDSUB132PH
    | VFMADDSUB132PS
    | VFMADDSUB213PD
    | VFMADDSUB213PH
    | VFMADDSUB213PS
    | VFMADDSUB231PD
    | VFMADDSUB231PH
    | VFMADDSUB231PS
    | VFMADDSUB312PD
    | VFMADDSUB312PS
    | VFMADDSUB321PD
    | VFMADDSUB321PS
    | VFMADDSUBPD
    | VFMADDSUBPS
    | VFMSUB123PD
    | VFMSUB123PS
    | VFMSUB123SD
    | VFMSUB123SS
    | VFMSUB132PD
    | VFMSUB132PH
    | VFMSUB132PS
    | VFMSUB132SD
    | VFMSUB132SS
    | VFMSUB213PD
    | VFMSUB213PH
    | VFMSUB213PS
    | VFMSUB213SD
    | VFMSUB213SS
    | VFMSUB231PD
    | VFMSUB231PH
    | VFMSUB231PS
    | VFMSUB231SD
    | VFMSUB231SS
    | VFMSUB312PD
    | VFMSUB312PS
    | VFMSUB312SD
    | VFMSUB312SS
    | VFMSUB321PD
    | VFMSUB321PS
    | VFMSUB321SD
    | VFMSUB321SS
    | VFMSUBADD123PD
    | VFMSUBADD123PS
    | VFMSUBADD132PD
    | VFMSUBADD132PH
    | VFMSUBADD132PS
    | VFMSUBADD213PD
    | VFMSUBADD213PH
    | VFMSUBADD213PS
    | VFMSUBADD231PD
    | VFMSUBADD231PH
    | VFMSUBADD231PS
    | VFMSUBADD312PD
    | VFMSUBADD312PS
    | VFMSUBADD321PD
    | VFMSUBADD321PS
    | VFMSUBADDPD
    | VFMSUBADDPS
    | VFMSUBPD
    | VFMSUBPS
    | VFMSUBSD
    | VFMSUBSS
    | VFMULCPCH
    | VFMULCSH
    | VFNMADD123PD
    | VFNMADD123PS
    | VFNMADD123SD
    | VFNMADD123SS
    | VFNMADD132PD
    | VFNMADD132PS
    | VFNMADD132SD
    | VFNMADD132SS
    | VFNMADD213PD
    | VFNMADD213PS
    | VFNMADD213SD
    | VFNMADD213SS
    | VFNMADD231PD
    | VFNMADD231PS
    | VFNMADD231SD
    | VFNMADD231SS
    | VFNMADD312PD
    | VFNMADD312PS
    | VFNMADD312SD
    | VFNMADD312SS
    | VFNMADD321PD
    | VFNMADD321PS
    | VFNMADD321SD
    | VFNMADD321SS
    | VFNMADDPD
    | VFNMADDPS
    | VFNMADDSD
    | VFNMADDSS
    | VFNMSUB123PD
    | VFNMSUB123PS
    | VFNMSUB123SD
    | VFNMSUB123SS
    | VFNMSUB132PD
    | VFNMSUB132PS
    | VFNMSUB132SD
    | VFNMSUB132SS
    | VFNMSUB213PD
    | VFNMSUB213PS
    | VFNMSUB213SD
    | VFNMSUB213SS
    | VFNMSUB231PD
    | VFNMSUB231PS
    | VFNMSUB231SD
    | VFNMSUB231SS
    | VFNMSUB312PD
    | VFNMSUB312PS
    | VFNMSUB312SD
    | VFNMSUB312SS
    | VFNMSUB321PD
    | VFNMSUB321PS
    | VFNMSUB321SD
    | VFNMSUB321SS
    | VFNMSUBPD
    | VFNMSUBPS
    | VFNMSUBSD
    | VFNMSUBSS
    | VFPCLASSPD
    | VFPCLASSPH
    | VFPCLASSPS
    | VFPCLASSSD
    | VFPCLASSSH
    | VFPCLASSSS
    | VFRCZPD
    | VFRCZPS
    | VFRCZSD
    | VFRCZSS
    | VGATHERDPD
    | VGATHERDPS
    | VGATHERPF0DPD
    | VGATHERPF0DPS
    | VGATHERPF0QPD
    | VGATHERPF0QPS
    | VGATHERPF1DPD
    | VGATHERPF1DPS
    | VGATHERPF1QPD
    | VGATHERPF1QPS
    | VGATHERQPD
    | VGATHERQPS
    | VGETEXPPD
    | VGETEXPPH
    | VGETEXPPS
    | VGETEXPSD
    | VGETEXPSH
    | VGETEXPSS
    | VGETMANTPD
    | VGETMANTPH
    | VGETMANTPS
    | VGETMANTSD
    | VGETMANTSH
    | VGETMANTSS
    | VGETMAXPH
    | VGETMAXSH
    | VGETMINPH
    | VGETMINSH
    | VGF2P8AFFINEINVQB
    | VGF2P8AFFINEQB
    | VGF2P8MULB
    | VHADDPD
    | VHADDPS
    | VHSUBPD
    | VHSUBPS
    | VINSERTF128
    | VINSERTF32X4
    | VINSERTF32X8
    | VINSERTF64X2
    | VINSERTF64X4
    | VINSERTI128
    | VINSERTI32X4
    | VINSERTI32X8
    | VINSERTI64X2
    | VINSERTI64X4
    | VINSERTPS
    | VLDDQU
    | VLDMXCSR
    | VLDQQU
    | VMASKMOVDQU
    | VMASKMOVPD
    | VMASKMOVPS
    | VMAXPD
    | VMAXPS
    | VMAXSD
    | VMAXSS
    | VMCALL
    | VMCLEAR
    | VMFUNC
    | VMGEXIT
    | VMINPD
    | VMINPS
    | VMINSD
    | VMINSS
    | VMLAUNCH
    | VMLOAD
    | VMMCALL
    | VMOVAPD
    | VMOVAPS
    | VMOVD
    | VMOVDDUP
    | VMOVDQA
    | VMOVDQA32
    | VMOVDQA64
    | VMOVDQU
    | VMOVDQU16
    | VMOVDQU32
    | VMOVDQU64
    | VMOVDQU8
    | VMOVHLPS
    | VMOVHPD
    | VMOVHPS
    | VMOVLHPS
    | VMOVLPD
    | VMOVLPS
    | VMOVMSKPD
    | VMOVMSKPS
    | VMOVNTDQ
    | VMOVNTDQA
    | VMOVNTPD
    | VMOVNTPS
    | VMOVNTQQ
    | VMOVQ
    | VMOVQQA
    | VMOVQQU
    | VMOVSD
    | VMOVSH
    | VMOVSHDUP
    | VMOVSLDUP
    | VMOVSS
    | VMOVUPD
    | VMOVUPS
    | VMOVW
    | VMPSADBW
    | VMPTRLD
    | VMPTRST
    | VMREAD
    | VMRESUME
    | VMRUN
    | VMSAVE
    | VMULPD
    | VMULPH
    | VMULPS
    | VMULSD
    | VMULSH
    | VMULSS
    | VMWRITE
    | VMXOFF
    | VMXON
    | VORPD
    | VORPS
    | VP2INTERSECTD
    | VPABSB
    | VPABSD
    | VPABSQ
    | VPABSW
    | VPACKSSDW
    | VPACKSSWB
    | VPACKUSDW
    | VPACKUSWB
    | VPADDB
    | VPADDD
    | VPADDQ
    | VPADDSB
    | VPADDSW
    | VPADDUSB
    | VPADDUSW
    | VPADDW
    | VPALIGNR
    | VPAND
    | VPANDD
    | VPANDN
    | VPANDND
    | VPANDNQ
    | VPANDQ
    | VPAVGB
    | VPAVGW
    | VPBLENDD
    | VPBLENDMB
    | VPBLENDMD
    | VPBLENDMQ
    | VPBLENDMW
    | VPBLENDVB
    | VPBLENDW
    | VPBROADCASTB
    | VPBROADCASTD
    | VPBROADCASTMB2Q
    | VPBROADCASTMW2D
    | VPBROADCASTQ
    | VPBROADCASTW
    | VPCLMULHQHQDQ
    | VPCLMULHQLQDQ
    | VPCLMULLQHQDQ
    | VPCLMULLQLQDQ
    | VPCLMULQDQ
    | VPCMOV
    | VPCMPB
    | VPCMPD
    | VPCMPEQB
    | VPCMPEQD
    | VPCMPEQQ
    | VPCMPEQUB
    | VPCMPEQUD
    | VPCMPEQUQ
    | VPCMPEQUW
    | VPCMPEQW
    | VPCMPESTRI
    | VPCMPESTRM
    | VPCMPGEB
    | VPCMPGED
    | VPCMPGEQ
    | VPCMPGEUB
    | VPCMPGEUD
    | VPCMPGEUQ
    | VPCMPGEUW
    | VPCMPGEW
    | VPCMPGTB
    | VPCMPGTD
    | VPCMPGTQ
    | VPCMPGTUB
    | VPCMPGTUD
    | VPCMPGTUQ
    | VPCMPGTUW
    | VPCMPGTW
    | VPCMPISTRI
    | VPCMPISTRM
    | VPCMPLEB
    | VPCMPLED
    | VPCMPLEQ
    | VPCMPLEUB
    | VPCMPLEUD
    | VPCMPLEUQ
    | VPCMPLEUW
    | VPCMPLEW
    | VPCMPLTB
    | VPCMPLTD
    | VPCMPLTQ
    | VPCMPLTUB
    | VPCMPLTUD
    | VPCMPLTUQ
    | VPCMPLTUW
    | VPCMPLTW
    | VPCMPNEQB
    | VPCMPNEQD
    | VPCMPNEQQ
    | VPCMPNEQUB
    | VPCMPNEQUD
    | VPCMPNEQUQ
    | VPCMPNEQUW
    | VPCMPNEQW
    | VPCMPNGTB
    | VPCMPNGTD
    | VPCMPNGTQ
    | VPCMPNGTUB
    | VPCMPNGTUD
    | VPCMPNGTUQ
    | VPCMPNGTUW
    | VPCMPNGTW
    | VPCMPNLEB
    | VPCMPNLED
    | VPCMPNLEQ
    | VPCMPNLEUB
    | VPCMPNLEUD
    | VPCMPNLEUQ
    | VPCMPNLEUW
    | VPCMPNLEW
    | VPCMPNLTB
    | VPCMPNLTD
    | VPCMPNLTQ
    | VPCMPNLTUB
    | VPCMPNLTUD
    | VPCMPNLTUQ
    | VPCMPNLTUW
    | VPCMPNLTW
    | VPCMPQ
    | VPCMPUB
    | VPCMPUD
    | VPCMPUQ
    | VPCMPUW
    | VPCMPW
    | VPCOMB
    | VPCOMD
    | VPCOMPRESSB
    | VPCOMPRESSD
    | VPCOMPRESSQ
    | VPCOMPRESSW
    | VPCOMQ
    | VPCOMUB
    | VPCOMUD
    | VPCOMUQ
    | VPCOMUW
    | VPCOMW
    | VPCONFLICTD
    | VPCONFLICTQ
    | VPDPBSSD
    | VPDPBSSDS
    | VPDPBSUD
    | VPDPBSUDS
    | VPDPBUSD
    | VPDPBUSDS
    | VPDPBUUD
    | VPDPBUUDS
    | VPDPWSSD
    | VPDPWSSDS
    | VPERM2F128
    | VPERM2I128
    | VPERMB
    | VPERMD
    | VPERMI2B
    | VPERMI2D
    | VPERMI2PD
    | VPERMI2PS
    | VPERMI2Q
    | VPERMI2W
    | VPERMILPD
    | VPERMILPS
    | VPERMPD
    | VPERMPS
    | VPERMQ
    | VPERMT2B
    | VPERMT2D
    | VPERMT2PD
    | VPERMT2PS
    | VPERMT2Q
    | VPERMT2W
    | VPERMW
    | VPEXPANDB
    | VPEXPANDD
    | VPEXPANDQ
    | VPEXPANDW
    | VPEXTRB
    | VPEXTRD
    | VPEXTRQ
    | VPEXTRW
    | VPGATHERDD
    | VPGATHERDQ
    | VPGATHERQD
    | VPGATHERQQ
    | VPHADDBD
    | VPHADDBQ
    | VPHADDBW
    | VPHADDD
    | VPHADDDQ
    | VPHADDSW
    | VPHADDUBD
    | VPHADDUBQ
    | VPHADDUBW
    | VPHADDUDQ
    | VPHADDUWD
    | VPHADDUWQ
    | VPHADDW
    | VPHADDWD
    | VPHADDWQ
    | VPHMINPOSUW
    | VPHSUBBW
    | VPHSUBD
    | VPHSUBDQ
    | VPHSUBSW
    | VPHSUBW
    | VPHSUBWD
    | VPINSRB
    | VPINSRD
    | VPINSRQ
    | VPINSRW
    | VPLZCNTD
    | VPLZCNTQ
    | VPMACSDD
    | VPMACSDQH
    | VPMACSDQL
    | VPMACSSDD
    | VPMACSSDQH
    | VPMACSSDQL
    | VPMACSSWD
    | VPMACSSWW
    | VPMACSWD
    | VPMACSWW
    | VPMADCSSWD
    | VPMADCSWD
    | VPMADD132PH
    | VPMADD132SH
    | VPMADD213PH
    | VPMADD213SH
    | VPMADD231PH
    | VPMADD231SH
    | VPMADD52HUQ
    | VPMADD52LUQ
    | VPMADDUBSW
    | VPMADDWD
    | VPMASKMOVD
    | VPMASKMOVQ
    | VPMAXSB
    | VPMAXSD
    | VPMAXSQ
    | VPMAXSW
    | VPMAXUB
    | VPMAXUD
    | VPMAXUQ
    | VPMAXUW
    | VPMINSB
    | VPMINSD
    | VPMINSQ
    | VPMINSW
    | VPMINUB
    | VPMINUD
    | VPMINUQ
    | VPMINUW
    | VPMOVB2M
    | VPMOVD2M
    | VPMOVDB
    | VPMOVDW
    | VPMOVM2B
    | VPMOVM2D
    | VPMOVM2Q
    | VPMOVM2W
    | VPMOVMSKB
    | VPMOVQ2M
    | VPMOVQB
    | VPMOVQD
    | VPMOVQW
    | VPMOVSDB
    | VPMOVSDW
    | VPMOVSQB
    | VPMOVSQD
    | VPMOVSQW
    | VPMOVSWB
    | VPMOVSXBD
    | VPMOVSXBQ
    | VPMOVSXBW
    | VPMOVSXDQ
    | VPMOVSXWD
    | VPMOVSXWQ
    | VPMOVUSDB
    | VPMOVUSDW
    | VPMOVUSQB
    | VPMOVUSQD
    | VPMOVUSQW
    | VPMOVUSWB
    | VPMOVW2M
    | VPMOVWB
    | VPMOVZXBD
    | VPMOVZXBQ
    | VPMOVZXBW
    | VPMOVZXDQ
    | VPMOVZXWD
    | VPMOVZXWQ
    | VPMSUB132PH
    | VPMSUB132SH
    | VPMSUB213PH
    | VPMSUB213SH
    | VPMSUB231PH
    | VPMSUB231SH
    | VPMULDQ
    | VPMULHRSW
    | VPMULHUW
    | VPMULHW
    | VPMULLD
    | VPMULLQ
    | VPMULLW
    | VPMULTISHIFTQB
    | VPMULUDQ
    | VPNMADD132SH
    | VPNMADD213SH
    | VPNMADD231SH
    | VPNMSUB132SH
    | VPNMSUB213SH
    | VPNMSUB231SH
    | VPOPCNTB
    | VPOPCNTD
    | VPOPCNTQ
    | VPOPCNTW
    | VPOR
    | VPORD
    | VPORQ
    | VPPERM
    | VPROLD
    | VPROLQ
    | VPROLVD
    | VPROLVQ
    | VPRORD
    | VPRORQ
    | VPRORVD
    | VPRORVQ
    | VPROTB
    | VPROTD
    | VPROTQ
    | VPROTW
    | VPSADBW
    | VPSCATTERDD
    | VPSCATTERDQ
    | VPSCATTERQD
    | VPSCATTERQQ
    | VPSHAB
    | VPSHAD
    | VPSHAQ
    | VPSHAW
    | VPSHLB
    | VPSHLD
    | VPSHLDD
    | VPSHLDQ
    | VPSHLDVD
    | VPSHLDVQ
    | VPSHLDVW
    | VPSHLDW
    | VPSHLQ
    | VPSHLW
    | VPSHRDD
    | VPSHRDQ
    | VPSHRDVD
    | VPSHRDVQ
    | VPSHRDVW
    | VPSHRDW
    | VPSHUFB
    | VPSHUFBITQMB
    | VPSHUFD
    | VPSHUFHW
    | VPSHUFLW
    | VPSIGNB
    | VPSIGND
    | VPSIGNW
    | VPSLLD
    | VPSLLDQ
    | VPSLLQ
    | VPSLLVD
    | VPSLLVQ
    | VPSLLVW
    | VPSLLW
    | VPSRAD
    | VPSRAQ
    | VPSRAVD
    | VPSRAVQ
    | VPSRAVW
    | VPSRAW
    | VPSRLD
    | VPSRLDQ
    | VPSRLQ
    | VPSRLVD
    | VPSRLVQ
    | VPSRLVW
    | VPSRLW
    | VPSUBB
    | VPSUBD
    | VPSUBQ
    | VPSUBSB
    | VPSUBSW
    | VPSUBUSB
    | VPSUBUSW
    | VPSUBW
    | VPTERNLOGD
    | VPTERNLOGQ
    | VPTEST
    | VPTESTMB
    | VPTESTMD
    | VPTESTMQ
    | VPTESTMW
    | VPTESTNMB
    | VPTESTNMD
    | VPTESTNMQ
    | VPTESTNMW
    | VPUNPCKHBW
    | VPUNPCKHDQ
    | VPUNPCKHQDQ
    | VPUNPCKHWD
    | VPUNPCKLBW
    | VPUNPCKLDQ
    | VPUNPCKLQDQ
    | VPUNPCKLWD
    | VPXOR
    | VPXORD
    | VPXORQ
    | VRANGEPD
    | VRANGEPS
    | VRANGESD
    | VRANGESS
    | VRCP14PD
    | VRCP14PS
    | VRCP14SD
    | VRCP14SS
    | VRCP28PD
    | VRCP28PS
    | VRCP28SD
    | VRCP28SS
    | VRCPPH
    | VRCPPS
    | VRCPSH
    | VRCPSS
    | VREDUCEPD
    | VREDUCEPH
    | VREDUCEPS
    | VREDUCESD
    | VREDUCESH
    | VREDUCESS
    | VRNDSCALEPD
    | VRNDSCALEPS
    | VRNDSCALESD
    | VRNDSCALESS
    | VROUNDPD
    | VROUNDPS
    | VROUNDSD
    | VROUNDSS
    | VRSQRT14PD
    | VRSQRT14PS
    | VRSQRT14SD
    | VRSQRT14SS
    | VRSQRT28PD
    | VRSQRT28PS
    | VRSQRT28SD
    | VRSQRT28SS
    | VRSQRTPH
    | VRSQRTPS
    | VRSQRTSH
    | VRSQRTSS
    | VSCALEFPD
    | VSCALEFPH
    | VSCALEFPS
    | VSCALEFSD
    | VSCALEFSH
    | VSCALEFSS
    | VSCATTERDPD
    | VSCATTERDPS
    | VSCATTERPF0DPD
    | VSCATTERPF0DPS
    | VSCATTERPF0QPD
    | VSCATTERPF0QPS
    | VSCATTERPF1DPD
    | VSCATTERPF1DPS
    | VSCATTERPF1QPD
    | VSCATTERPF1QPS
    | VSCATTERQPD
    | VSCATTERQPS
    | VSHUFF32X4
    | VSHUFF64X2
    | VSHUFI32X4
    | VSHUFI64X2
    | VSHUFPD
    | VSHUFPS
    | VSQRTPD
    | VSQRTPH
    | VSQRTPS
    | VSQRTSD
    | VSQRTSH
    | VSQRTSS
    | VSTMXCSR
    | VSUBPD
    | VSUBPH
    | VSUBPS
    | VSUBSD
    | VSUBSH
    | VSUBSS
    | VTESTPD
    | VTESTPS
    | VUCOMISD
    | VUCOMISH
    | VUCOMISS
    | VUNPCKHPD
    | VUNPCKHPS
    | VUNPCKLPD
    | VUNPCKLPS
    | VXORPD
    | VXORPS
    | VZEROALL
    | VZEROUPPER
    | WBNOINVD
    | WRFSBASE
    | WRGSBASE
    | WRMSRLIST
    | WRMSRNS
    | WRPKRU
    | WRSSD
    | WRSSQ
    | WRUSSD
    | WRUSSQ
    | XABORT
    | XBEGIN
    | XCRYPTCBC
    | XCRYPTCFB
    | XCRYPTCTR
    | XCRYPTECB
    | XCRYPTOFB
    | XEND
    | XGETBV
    | XORPD
    | XORPS
    | XRESLDTRK
    | XRSTOR
    | XRSTOR64
    | XRSTORS
    | XRSTORS64
    | XSAVE
    | XSAVE64
    | XSAVEC
    | XSAVEC64
    | XSAVEOPT
    | XSAVEOPT64
    | XSAVES
    | XSAVES64
    | XSETBV
    | XSHA1
    | XSHA256
    | XSTORE
    | XSUSLDTRK
    | XTEST
    ;

operand
    : (register COLON)? (register | name)
    | (strict? size)? (
        string
        | float_number
        | integer
        | LEFT_BRACKET expression (COMMA expression)* RIGHT_BRACKET
    )
    | expression
    ;

register
    : AL
    | AH
    | AX
    | EAX
    | RAX
    | BL
    | BH
    | BX
    | EBX
    | RBX
    | CL
    | CH
    | CX
    | ECX
    | RCX
    | DL
    | DH
    | DX
    | EDX
    | RDX
    | SPL
    | SP
    | ESP
    | RSP
    | BPL
    | BP
    | EBP
    | RBP
    | SIL
    | SI
    | ESI
    | RSI
    | DIL
    | DI
    | EDI
    | RDI
    | R8B
    | R9B
    | R10B
    | R11B
    | R12B
    | R13B
    | R14B
    | R15B
    | R8W
    | R9W
    | R10W
    | R11W
    | R12W
    | R13W
    | R14W
    | R15W
    | R8D
    | R9D
    | R10D
    | R11D
    | R12D
    | R13D
    | R14D
    | R15D
    | R8
    | R9
    | R10
    | R11
    | R12
    | R13
    | R14
    | R15
    | IP
    | EIP
    | RIP
    | ES
    | CS
    | SS
    | DS
    | FS
    | GS
    | SEGR6
    | SEGR7
    | CR0
    | CR1
    | CR2
    | CR3
    | CR4
    | CR5
    | CR6
    | CR7
    | CR8
    | CR9
    | CR10
    | CR11
    | CR12
    | CR13
    | CR14
    | CR15
    | DR0
    | DR1
    | DR2
    | DR3
    | DR4
    | DR5
    | DR6
    | DR7
    | DR8
    | DR9
    | DR10
    | DR11
    | DR12
    | DR13
    | DR14
    | DR15
    | TR0
    | TR1
    | TR2
    | TR3
    | TR4
    | TR5
    | TR6
    | TR7
    | ST0
    | ST1
    | ST2
    | ST3
    | ST4
    | ST5
    | ST6
    | ST7
    | MM0
    | MM1
    | MM2
    | MM3
    | MM4
    | MM5
    | MM6
    | MM7
    | XMM0
    | XMM1
    | XMM2
    | XMM3
    | XMM4
    | XMM5
    | XMM6
    | XMM7
    | XMM8
    | XMM9
    | XMM10
    | XMM11
    | XMM12
    | XMM13
    | XMM14
    | XMM15
    | XMM16
    | XMM17
    | XMM18
    | XMM19
    | XMM20
    | XMM21
    | XMM22
    | XMM23
    | XMM24
    | XMM25
    | XMM26
    | XMM27
    | XMM28
    | XMM29
    | XMM30
    | XMM31
    | YMM0
    | YMM1
    | YMM2
    | YMM3
    | YMM4
    | YMM5
    | YMM6
    | YMM7
    | YMM8
    | YMM9
    | YMM10
    | YMM11
    | YMM12
    | YMM13
    | YMM14
    | YMM15
    | YMM16
    | YMM17
    | YMM18
    | YMM19
    | YMM20
    | YMM21
    | YMM22
    | YMM23
    | YMM24
    | YMM25
    | YMM26
    | YMM27
    | YMM28
    | YMM29
    | YMM30
    | YMM31
    | ZMM0
    | ZMM1
    | ZMM2
    | ZMM3
    | ZMM4
    | ZMM5
    | ZMM6
    | ZMM7
    | ZMM8
    | ZMM9
    | ZMM10
    | ZMM11
    | ZMM12
    | ZMM13
    | ZMM14
    | ZMM15
    | ZMM16
    | ZMM17
    | ZMM18
    | ZMM19
    | ZMM20
    | ZMM21
    | ZMM22
    | ZMM23
    | ZMM24
    | ZMM25
    | ZMM26
    | ZMM27
    | ZMM28
    | ZMM29
    | ZMM30
    | ZMM31
    | TMM0
    | TMM1
    | TMM2
    | TMM3
    | TMM4
    | TMM5
    | TMM6
    | TMM7
    | K0
    | K1
    | K2
    | K3
    | K4
    | K5
    | K6
    | K7
    | BND0
    | BND1
    | BND2
    | BND3
    ;

strict
    : STRICT
    ;

macro_call
    : name (
        macro_param? (COMMA macro_param)*
        | LEFT_PARENTHESIS macro_param? (COMMA macro_param)* RIGHT_PARENTHESIS
    )
    ;

macro_param
    : string
    | name
    | integer
    | float_number
    ;