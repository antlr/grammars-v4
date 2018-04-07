
lexer grammar SHARCLexer;

//tokens

StringLiteral
   :
   //'"' (ID | EscapeSequence | NormalChar | WS )* '"' '"' ~ ( '\n' | '\r' | '"' )* '"'
   ;


CharLiteral
   :
   // '\'' (ID | EscapeSequence | NormalChar | WS )* '\'' '\'' ~ ( '\n' | '\r' | '\'' )* '\''
   ;


fragment HexPrefix
   : '0x' | '0X'
   ;


fragment HexDigit
   : ( '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' )
   ;


INT
   : ( '0' .. '9' )+ '.' ( '0' .. '9' )* Exponent? | '.' ( '0' .. '9' )+ Exponent? | ( '0' .. '9' )+ Exponent | ( '0' .. '9' )+ | HexPrefix ( HexDigit )+
   ;


fragment Exponent
   : ( 'e' | 'E' ) ( '+' | '-' )? ( '0' .. '9' )+
   ;


fragment LCHAR
   : CHAR | '_'
   ;


fragment CHAR
   : LC | UC
   ;


fragment LC
   : 'a' .. 'z'
   ;


fragment UC
   : 'A' .. 'Z'
   ;


WS
   : ( ' ' | '\t' | '\r' | '\n' ) -> skip
   ;


DOT_ADI_
   : '_ADI_'
   ;


DOT_DATE_
   : '_DATE_'
   ;


DOT_FILE_
   : '_FILE'
   ;


DOT_ALGIGN
   : '.align'
   ;


DOT_COMPRESS
   : '.compress'
   ;


DOT_ELIF
   : '.elif'
   ;


DOT_ELSE
   : '.else'
   ;


DOT_ENDIF
   : '.endif'
   ;


DOT_EXTERN
   : '.extern'
   ;


DOT_FILE
   : '.file'
   ;


DOT_FILE_ATTR
   : '.file_attr'
   ;


DOT_FORCECOMPRESS
   : '.forcecompress'
   ;


DOT_GLOBAL
   : '.global'
   ;


DOT_IF
   : '.if'
   ;


DOT_IMPORT
   : '.import'
   ;


DOT_INCBINARY
   : '.inc/binary'
   ;


DOT_LEFTMARGIN
   : '.leftmargin'
   ;


DOT_LIST
   : '.list'
   ;


DOT_LIST_DATA
   : '.list_data'
   ;


DOT_LIST_DATFILE
   : '.list_datfile'
   ;


DOT_LIST_DEFTAB
   : '.list_deftab'
   ;


DOT_LIST_LOCTAB
   : '.list_loctab'
   ;


DOT_LIST_WRAPDATA
   : '.list_wrapdata'
   ;


DOT_NEWPAGE
   : '.newpage'
   ;


DOT_NOCOMPRESS
   : '.nocompress'
   ;


DOT_NOLIST_DATA
   : '.nolist_data'
   ;


DOT_NOLIST_DATFILE
   : '.nolist_datfile'
   ;


DOT_NOLIST_WRAPDATA
   : '.nolist_wrapdata'
   ;


DOT_PAGELENGTH
   : '.pagelength'
   ;


DOT_PAGEWIDTH
   : '.pagewidth'
   ;


DOT_PRECISION
   : '.precision'
   ;


DOT_ROUND_MINUS
   : '.round_minus'
   ;


DOT_ROUND_NEAREST
   : '.round_nearest'
   ;


DOT_ROUND_PLUS
   : '.round_plus'
   ;


DOT_ROUND_ZERO
   : '.round_zero'
   ;


DOT_PREVIOUS
   : '.previous'
   ;


DOT_SECTION
   : '.section'
   ;


DOT_SEGMENT
   : '.segment'
   ;


DOT_ENDSEG
   : '.endseg'
   ;


DOT_STRUCT
   : '.struct'
   ;


DOT_TYPE
   : '.type'
   ;


DOT_VAR
   : '.var'
   ;


DOT_WEAK
   : '.weak'
   ;


ABS
   : 'abs'
   ;


AC
   : 'ac'
   ;


ACS
   : 'acs'
   ;


ACT
   : 'act'
   ;


ADDRESS
   : 'address'
   ;


AND
   : 'and'
   ;


ASHIFT
   : 'ashift'
   ;


ASTAT
   : 'astat'
   ;


AV
   : 'av'
   ;


B0
   : 'b0'
   ;


B1
   : 'b1'
   ;


B2
   : 'b2'
   ;


B3
   : 'b3'
   ;


B4
   : 'b4'
   ;


B5
   : 'b5'
   ;


B6
   : 'b6'
   ;


B7
   : 'b7'
   ;


B8
   : 'b8'
   ;


B9
   : 'b9'
   ;


B10
   : 'b10'
   ;


B11
   : 'b11'
   ;


B12
   : 'b12'
   ;


B13
   : 'b13'
   ;


B14
   : 'b14'
   ;


B15
   : 'b15'
   ;


BB
   : 'bb'
   ;


BCLR
   : 'bclr'
   ;


BF
   : 'bf'
   ;


BIT
   : 'bit'
   ;


BITREV
   : 'bitrev'
   ;


BM
   : 'bm'
   ;


BSET
   : 'bset'
   ;


BTGL
   : 'btgl'
   ;


BTST
   : 'btst'
   ;


BY
   : 'by'
   ;


CA
   : 'ca'
   ;


CACHE
   : 'cache'
   ;


CALL
   : 'call'
   ;


CH
   : 'ch'
   ;


CI
   : 'ci'
   ;


CJUMP
   : 'cjump'
   ;


CL
   : 'cl'
   ;


CLR
   : 'clr'
   ;


CLIP
   : 'clip'
   ;


COMP
   : 'comp'
   ;


COPYSIGN
   : 'copysign'
   ;


COS
   : 'cos'
   ;


CURLCNTR
   : 'curlcntr'
   ;


DADDR
   : 'daddr'
   ;


DB
   : 'db'
   ;


DEC
   : 'dec'
   ;


DEF
   : 'def'
   ;


DIM
   : 'dim'
   ;


DM
   : 'dm'
   ;


DMA1E
   : 'dm1e'
   ;


DMA1s
   : 'dm1s'
   ;


DMA2E
   : 'dm2e'
   ;


DMA2s
   : 'dm2s'
   ;


DMADR
   : 'dmadr'
   ;


DMABANK1
   : 'dmabank1'
   ;


DMABANK2
   : 'dmabank2'
   ;


DMABANK3
   : 'dmabank3'
   ;


DMAWAIT
   : 'dmawait'
   ;


DO
   : 'do'
   ;


DOVL
   : 'dovl'
   ;


EB
   : 'eb'
   ;


ECE
   : 'ece'
   ;


EF
   : 'ef'
   ;


ELSE
   : 'else'
   ;


EMUCLK
   : 'emuclk'
   ;


EMUCLK2
   : 'emuclk2'
   ;


EMUIDLE
   : 'emuidle'
   ;


EMUN
   : 'emun'
   ;


EOS
   : 'eos'
   ;


EQ
   : 'eq'
   ;


EX
   : 'ex'
   ;


EXP
   : 'exp'
   ;


EXP2
   : 'exp2'
   ;


F0
   : 'f0'
   ;


F1
   : 'f1'
   ;


F2
   : 'f2'
   ;


F3
   : 'f3'
   ;


F4
   : 'f4'
   ;


F5
   : 'f5'
   ;


F6
   : 'f6'
   ;


F7
   : 'f7'
   ;


F8
   : 'f8'
   ;


F9
   : 'f9'
   ;


F10
   : 'f10'
   ;


F11
   : 'f11'
   ;


F12
   : 'f12'
   ;


F13
   : 'f13'
   ;


F14
   : 'f14'
   ;


F15
   : 'f15'
   ;


FADDR
   : 'faddr'
   ;


FDEP
   : 'fdep'
   ;


FEXT
   : 'fext'
   ;


FILE
   : 'file'
   ;


FIX
   : 'fix'
   ;


FLAG0_IN
   : 'flag0_in'
   ;


FLAG1_IN
   : 'flag1_in'
   ;


FLAG2_IN
   : 'flag2_in'
   ;


FLAG3_IN
   : 'flag3_in'
   ;


FLOAT
   : 'float'
   ;


FLUSH
   : 'flush'
   ;


FMERG
   : 'fmerg'
   ;


FOREVER
   : 'forever'
   ;


FPACK
   : 'fpack'
   ;


FRACTIONAL
   : 'fractional'
   ;


FTA
   : 'fta'
   ;


FTB
   : 'ftb'
   ;


FTC
   : 'ftc'
   ;


FUNPACK
   : 'funpack'
   ;


GCC_COMPILED
   : 'gcc_compiled'
   ;


GE
   : 'ge'
   ;


GT
   : 'gt'
   ;


I0
   : 'i0'
   ;


I1
   : 'i1'
   ;


I2
   : 'i2'
   ;


I3
   : 'i3'
   ;


I4
   : 'i4'
   ;


I5
   : 'i5'
   ;


I6
   : 'i6'
   ;


I7
   : 'i7'
   ;


I8
   : 'i8'
   ;


I9
   : 'i9'
   ;


I10
   : 'i10'
   ;


I11
   : 'i11'
   ;


I12
   : 'i12'
   ;


I13
   : 'i13'
   ;


I14
   : 'i14'
   ;


I15
   : 'i15'
   ;


IDLE
   : 'idle'
   ;


IDLE16
   : 'idle16'
   ;


IDLEI15
   : 'idlei15'
   ;


IDLEI16
   : 'idlei16'
   ;


IF
   : 'if'
   ;


IMASK
   : 'imask'
   ;


IMASKP
   : 'imaskp'
   ;


INC
   : 'inc'
   ;


IRPTL
   : 'irptl'
   ;


JUMP
   : 'jump'
   ;


L0
   : 'l0'
   ;


L1
   : 'l1'
   ;


L2
   : 'l2'
   ;


L3
   : 'l3'
   ;


L4
   : 'l4'
   ;


L5
   : 'l5'
   ;


L6
   : 'l6'
   ;


L7
   : 'l7'
   ;


L8
   : 'l8'
   ;


L9
   : 'l9'
   ;


L10
   : 'l10'
   ;


L11
   : 'l11'
   ;


L12
   : 'l12'
   ;


L13
   : 'l13'
   ;


L14
   : 'l14'
   ;


L15
   : 'l15'
   ;


LA
   : 'la'
   ;


LADDR
   : 'laddr'
   ;


LCE
   : 'lce'
   ;


LCNTR
   : 'lcntr'
   ;


LE
   : 'le'
   ;


LEFTO
   : 'lefto'
   ;


LEFTZ
   : 'leftz'
   ;


LENGTH
   : 'length'
   ;


LINE
   : 'line'
   ;


LN
   : 'ln'
   ;


LOAD
   : 'load'
   ;


LOG2
   : 'log2'
   ;


LOGB
   : 'logb'
   ;


LOOP
   : 'loop'
   ;


LR
   : 'lr'
   ;


LSHIFT
   : 'lshift'
   ;


LT
   : 'lt'
   ;


M0
   : 'm0'
   ;


M1
   : 'm1'
   ;


M2
   : 'm2'
   ;


M3
   : 'm3'
   ;


M4
   : 'm4'
   ;


M5
   : 'm5'
   ;


M6
   : 'm6'
   ;


M7
   : 'm7'
   ;


M8
   : 'm8'
   ;


M9
   : 'm9'
   ;


M10
   : 'm10'
   ;


M11
   : 'm11'
   ;


M12
   : 'm12'
   ;


M13
   : 'm13'
   ;


M14
   : 'm14'
   ;


M15
   : 'm15'
   ;


MANT
   : 'mant'
   ;


MAX
   : 'max'
   ;


MBM
   : 'mbm'
   ;


MIN
   : 'min'
   ;


MOD
   : 'mod'
   ;


MODE1
   : 'mode1'
   ;


MODE2
   : 'mode2'
   ;


MODIFY
   : 'modify'
   ;


MR0B
   : 'mr0b'
   ;


MR0F
   : 'mr0f'
   ;


MR1B
   : 'mr1b'
   ;


MR1F
   : 'mr1f'
   ;


MR2B
   : 'mr2b'
   ;


MR2F
   : 'mr2f'
   ;


MRB
   : 'mrb'
   ;


MRF
   : 'mrf'
   ;


MS
   : 'ms'
   ;


MV
   : 'mv'
   ;


NBM
   : 'nbm'
   ;


NE
   : 'ne'
   ;


NOFO
   : 'nofo'
   ;


NOFZ
   : 'nofz'
   ;


NOP
   : 'nop'
   ;


NOPSPECIAL
   : 'nopspecial'
   ;


NOT
   : 'not'
   ;


NU
   : 'nu'
   ;


NW
   : 'nw'
   ;


OFFSETOF
   : 'offsetof'
   ;


OR
   : 'or'
   ;


P20
   : 'p20'
   ;


P32
   : 'p32'
   ;


P40
   : 'p40'
   ;


PACK
   : 'pack'
   ;


PAGE
   : 'page'
   ;


PASS
   : 'pass'
   ;


PC
   : 'pc'
   ;


PCSTK
   : 'pcstk'
   ;


PCSTKP
   : 'pcstkp'
   ;


PM
   : 'pm'
   ;


PMADR
   : 'pmadr'
   ;


PMBANK1
   : 'pmbank1'
   ;


PMDAE
   : 'pmdae'
   ;


PMDAS
   : 'pmdas'
   ;


POP
   : 'pop'
   ;


POVL0
   : 'povl0'
   ;


POVL1
   : 'povl1'
   ;


PSA1E
   : 'psa1e'
   ;


PSA1S
   : 'psa1s'
   ;


PSA2E
   : 'psa2e'
   ;


PSA3E
   : 'psa3e'
   ;


PSA3S
   : 'psa3s'
   ;


PSA4E
   : 'psa4e'
   ;


PSA4S
   : 'psa4s'
   ;


PUSH
   : 'push'
   ;


PX
   : 'px'
   ;


PX1
   : 'px1'
   ;


PX2
   : 'px2'
   ;


RETAIN_NAME
   : 'retain_name'
   ;


R0
   : 'r0'
   ;


R1
   : 'r1'
   ;


R2
   : 'r2'
   ;


R3
   : 'r3'
   ;


R4
   : 'r4'
   ;


R5
   : 'r5'
   ;


R6
   : 'r6'
   ;


R7
   : 'r7'
   ;


R8
   : 'r8'
   ;


R9
   : 'r9'
   ;


R10
   : 'r10'
   ;


R11
   : 'r11'
   ;


R12
   : 'r12'
   ;


R13
   : 'r13'
   ;


R14
   : 'r14'
   ;


R15
   : 'r15'
   ;


READ
   : 'read'
   ;


RECIPS
   : 'recips'
   ;


RFRAME
   : 'rframe'
   ;


RND
   : 'rnd'
   ;


ROT
   : 'rot'
   ;


RS
   : 'rs'
   ;


RSQRTS
   : 'rsqrts'
   ;


RTI
   : 'rti'
   ;


RTS
   : 'rts'
   ;


SAT
   : 'sat'
   ;


SCALB
   : 'scalb'
   ;


SCL
   : 'scl'
   ;


SE
   : 'se'
   ;


SET
   : 'set'
   ;


SF
   : 'sf'
   ;


SI
   : 'si'
   ;


SIN
   : 'sin'
   ;


SIZE
   : 'size'
   ;


SIZEOF
   : 'sizeof'
   ;


SQR
   : 'sqr'
   ;


SR
   : 'sr'
   ;


SSF
   : 'ssf'
   ;


SSFR
   : 'ssfr'
   ;


SSI
   : 'ssi'
   ;


SSIR
   : 'ssir'
   ;


ST
   : 'st'
   ;


STEP
   : 'step'
   ;


STKY
   : 'stky'
   ;


STRUCT
   : 'struct'
   ;


STS
   : 'sts'
   ;


SUF
   : 'suf'
   ;


SUFR
   : 'sufr'
   ;


SUI
   : 'sui'
   ;


SV
   : 'sv'
   ;


SW
   : 'sw'
   ;


SZ
   : 'sz'
   ;


TAG
   : 'tag'
   ;


TCOUNT
   : 'tcount'
   ;


TF
   : 'tf'
   ;


TGL
   : 'tgl'
   ;


TPERIOD
   : 'tperiod'
   ;


TRUE
   : 'true'
   ;


TRUNC
   : 'trunc'
   ;


TST
   : 'tst'
   ;


TYPE
   : 'type'
   ;


TRAP
   : 'trap'
   ;


UF
   : 'uf'
   ;


UI
   : 'ui'
   ;


UNPACK
   : 'unpack'
   ;


UNTIL
   : 'until'
   ;


UR
   : 'ur'
   ;


USF
   : 'usf'
   ;


USFR
   : 'usfr'
   ;


USI
   : 'usi'
   ;


USIR
   : 'usir'
   ;


USTAT1
   : 'ustat1'
   ;


USTAT2
   : 'ustat2'
   ;


UUF
   : 'uuf'
   ;


UUFR
   : 'uufr'
   ;


UUI
   : 'uui'
   ;


UUIR
   : 'uuir'
   ;


VAL
   : 'val'
   ;


WITH
   : 'with'
   ;


XOR
   : 'xor'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


MULT
   : '*'
   ;


DIV
   : '/'
   ;


DIV_MOD
   : '%'
   ;


EQU
   : '='
   ;


I_OR
   : '|'
   ;


I_XOR
   : '^'
   ;


COMMA
   : ','
   ;


COLON
   : ':'
   ;


SEMICOLON
   : ';'
   ;


LPARENTHESE
   : '('
   ;


RPARENTHESE
   : ')'
   ;


LBRACKET
   : '['
   ;


RBRACKET
   : ']'
   ;


LBRACE
   : '{'
   ;


RBRACE
   : '}'
   ;


AT
   : '@'
   ;


NO_INIT
   : 'no_init'
   ;


ZERO_INIT
   : 'zero_init'
   ;


RUNTIME_INIT
   : 'runtime_init'
   ;


CODE
   : 'code'
   ;


DATA
   : 'data'
   ;


DATA64
   : 'data64'
   ;


DMAONLY
   : 'dmaonly'
   ;


SECTION
   : 'SECTION'
   ;


SECTION_INFO
   : 'SECTION_INFO'
   ;


STMT
   : 'STMT'
   ;


ADDR
   : 'ADDR'
   ;


BIT_DATA
   : 'BIT_DATA'
   ;


JUMP_INT
   : 'JUMP_INT'
   ;


JUMP_PC
   : 'JUMP_PC'
   ;


JUMP_MD
   : 'JUMP_MD'
   ;


MODIFIER
   : 'MODIFIER'
   ;


MULTI_MOD
   : 'MULTI_MOD'
   ;


LABLE
   : 'LABLE'
   ;


VARDEF
   : 'VARDEF'
   ;


ARRDEF
   : 'ARRDEF'
   ;


DM_ACCESS
   : 'DM_ACCESS'
   ;


PM_ACCESS
   : 'PM_ACCESS'
   ;


CONDITION
   : 'CONDITION'
   ;


IF_STMT
   : 'IF_STMT'
   ;


VALUE_EXP
   : 'VALUE_EXP'
   ;


NULL
   : 'NULL'
   ;


CHAR_LITERAL
   : 'CHAR_LITERAL'
   ;


STR_LITERAL
   : 'STR_LITERAL'
   ;


DIRECTIVE
   : 'DIRECTIVE'
   ;


NEGATE
   : 'NEGATE'
   ;


ID
   : ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '.') ( 'a' .. 'z' | 'A' .. 'Z' | '_' | '.'  | '0' .. '9' )*
   ;


COMMENT
   : ( '//' ~ ( '\n' | '\r' )* '\r'? '\n' | '/*' .*? '*/' ) -> skip
   ;
