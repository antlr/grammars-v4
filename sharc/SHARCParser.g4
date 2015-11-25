
parser grammar SHARCParser;

options { tokenVocab=SHARCLexer; }

prog
   : ( statement SEMICOLON )+
   ;

statement
   : stmt_atom | ( ID COLON )+ stmt_atom
   ;

stmt_atom
   : stmt | sec | seg | end_seg | directive_exp
   ;

//segment
sec
   : DOT_SECTION seg_qualifier ID
   ;

seg
   : DOT_SEGMENT seg_qualifier ID
   ;

end_seg
   : DOT_ENDSEG
   ;

seg_qualifier
   : seg_qualifier1 ( seg_qualifier2 | seg_qualifier3 )? | seg_qualifier2 ( seg_qualifier1 | seg_qualifier3 )? | seg_qualifier3 ( seg_qualifier1 | seg_qualifier2 )?
   ;

seg_qualifier1
   : ( DIV ( seg_qualifier_1 | seg_qualifier_2 ) )
   ;

seg_qualifier2
   : ( DIV seg_qualifier_3 )
   ;

seg_qualifier3
   : ( DIV DMAONLY )
   ;

seg_qualifier_1
   : PM | CODE
   ;

seg_qualifier_2
   : DM | DATA | DATA64
   ;

seg_qualifier_3
   : NO_INIT | ZERO_INIT | RUNTIME_INIT
   ;

stmt
   : compute | flow_control_exp | imm_mov_exp | misc_exp | declaration | if_compute_mov | compute_mov_exp
   ;

//.VAR foo[N]="123.dat";
declaration
   : DOT_VAR ( declaration_exp1 | declaration_exp2 | declaration_exp3 | declaration_exp4 | declaration_exp5 )
   ;

declaration_exp1
   : ID ( COMMA ID )*
   ;

declaration_exp2
   : EQU initExpression ( COMMA initExpression )*
   ;

declaration_exp3
   : ID LBRACKET RBRACKET ( EQU declaration_exp_f2 )?
   ;

declaration_exp4
   : ID LBRACKET value_exp RBRACKET ( EQU declaration_exp_f2 )?
   ;

declaration_exp5
   : ID EQU value_exp
   ;

declaration_exp_f1
   : initExpression ( COMMA initExpression )* | StringLiteral
   ;

declaration_exp_f2
   : LBRACE declaration_exp_f1 RBRACE | declaration_exp_f1
   ;

initExpression
   : value_exp | CharLiteral
   ;

//get addr
var_addr
   : AT ID | LENGTH LPARENTHESE ID RPARENTHESE
   ;

value_exp
   : value_exp2
   ;

value_exp2
   : term ( ( PLUS | MINUS | MULT | DIV | DIV_MOD | I_OR | I_XOR ) term )*
   ;

term
   : ( op = MINUS )? factor
   ;

factor
   : atom | LPARENTHESE value_exp2 RPARENTHESE
   ;

atom
   : INT | var_addr | ID
   ;

compute
   : dual_op | fixpoint_alu_op | floating_point_alu_op | multi_op | shifter_op
   ;

//====================================================================================================
if_compute_mov
   : IF condition if_compute_mov_exp
   ;

if_compute_mov_exp
   : compute_mov_exp | compute
   ;

//move or modify instructions
compute_mov_exp
   : ( compute COMMA )? ( mov_exp_1 | mov_exp_3a | mov_exp_3b | mov_exp_3c | mov_exp_3d | mov_exp_4a | mov_exp_4b | mov_exp_4c | mov_exp_4d | mov_exp_5 | mov_exp_7 )
   ;

mov_exp_1
   : mov_exp_1_1 COMMA mov_exp_1_2
   ;

//DM(Ia, Mb) = dreg
//dreg = DM(Ia, Mb)
mov_exp_1_1
   : mem_addr_dm_ia_mb EQU d_reg | d_reg EQU mem_addr_dm_ia_mb
   ;

//PM(Ic, Md) = dreg
//dreg = PM(Ic, Md)
mov_exp_1_2
   : mem_addr_pm_ic_md EQU d_reg | d_reg EQU mem_addr_pm_ic_md
   ;

//DM(Ia, Mb) = ureg
//PM(Ic, Md)
mov_exp_3a
   : ( mem_addr_dm_ia_mb | mem_addr_pm_ic_md ) EQU u_reg
   ;

//DM(Mb, Ia) = ureg
//PM(Md, Ic)
mov_exp_3b
   : ( mem_addr_dm_mb_ia | mem_addr_pm_md_ic ) EQU u_reg
   ;

//u_reg = DM(Ia, Mb)	
//        PM(Ic, Md)
mov_exp_3c
   : u_reg EQU ( mem_addr_dm_ia_mb | mem_addr_pm_ic_md )
   ;

//u_reg = DM(Mb, Ia)	
//        PM(Md, Ic)
mov_exp_3d
   : u_reg EQU ( mem_addr_dm_mb_ia | mem_addr_pm_md_ic )
   ;

//DM(Ia, <data6>) = dreg
//PM(Ic, <data6>)
mov_exp_4a
   : ( mem_addr_dm_ia_int | mem_addr_pm_ic_int ) EQU d_reg
   ;

//DM(<data6>, Ia) = dreg
//PM(Ic, <data6>)
mov_exp_4b
   : imm_mov_15a
   ;

//d_reg = DM(Ia, <data6>)
//        PM(Ic, <data6>)
mov_exp_4c
   : d_reg EQU ( mem_addr_dm_ia_int | mem_addr_pm_ic_int )
   ;

//d_reg = DM(<data6>, Ia)
//        PM(<data6>, Ic)
mov_exp_4d
   : imm_mov_15b
   ;

//ureg1 = ureg2
mov_exp_5
   : u_reg2 EQU u_reg
   ;

//DM(Ia, Mb) = dreg
//PM(Ic, Md)
mov_exp_6a
   : ( mem_addr_dm_ia_mb | mem_addr_pm_ic_md ) EQU d_reg
   ;

//dreg = DM(Ia, Mb)
//       PM(Ic, Md)
mov_exp_6b
   : d_reg EQU ( mem_addr_dm_ia_mb | mem_addr_pm_ic_md )
   ;

//MODIFY (Ia, Mb)
//  	 (Ic, Md)
mov_exp_7
   : MODIFY ( LPARENTHESE ia COMMA mb RPARENTHESE | LPARENTHESE ic COMMA md RPARENTHESE )
   ;

mem_addr_ia_mb
   : LPARENTHESE ia COMMA mb RPARENTHESE
   ;

mem_addr_ic_md
   : LPARENTHESE ic COMMA md RPARENTHESE
   ;

mem_addr_md_ic
   : LPARENTHESE md COMMA ic RPARENTHESE
   ;

mem_addr_mb_ia
   : LPARENTHESE mb COMMA ia RPARENTHESE
   ;

mem_addr_ia_int
   : LPARENTHESE ia COMMA value_exp RPARENTHESE
   ;

mem_addr_ic_int
   : LPARENTHESE ic COMMA value_exp RPARENTHESE
   ;

mem_addr_int_ia
   : LPARENTHESE value_exp COMMA ia RPARENTHESE
   ;

mem_addr_int_ic
   : LPARENTHESE value_exp COMMA ic RPARENTHESE
   ;

mem_addr_int
   :
   //LPARENTHESE value_exp RPARENTHESE
   //^(DIRECT value_exp) LPARENTHESE mem_addr_int_ RPARENTHESE
   ;

mem_addr_int_
   : atom | atom ( PLUS | MINUS ) atom
   ;

mem_addr_dm_ia_mb
   : DM mem_addr_ia_mb
   ;

mem_addr_pm_ic_md
   : PM mem_addr_ic_md
   ;

mem_addr_dm_mb_ia
   : DM mem_addr_mb_ia
   ;

mem_addr_pm_md_ic
   : PM mem_addr_md_ic
   ;

mem_addr_dm_ia_int
   : DM mem_addr_ia_int
   ;

mem_addr_pm_ic_int
   : PM mem_addr_ic_int
   ;

mem_addr_dm_int_ia
   : DM mem_addr_int_ia
   ;

mem_addr_pm_int_ic
   : PM mem_addr_int_ic
   ;

mem_addr_dm_int
   : DM mem_addr_int
   ;

mem_addr_pm_int
   : PM mem_addr_int
   ;

//====================================================================================================		
fixpoint_alu_op
   : r_reg EQU r_exp | COMP LPARENTHESE r_reg COMMA r_reg RPARENTHESE
   ;

r_exp
   : r_reg add_or_sub r_reg | r_reg PLUS r_reg PLUS CI | r_reg PLUS r_reg PLUS CI MINUS INT | LPARENTHESE r_reg PLUS r_reg RPARENTHESE DIV INT | r_reg PLUS CI | r_reg PLUS CI MINUS INT | r_reg PLUS INT | r_reg MINUS INT | MINUS r_reg | ABS r_reg | PASS r_reg | r_reg AND r_reg | r_reg OR r_reg | r_reg XOR r_reg | NOT r_reg | MIN LPARENTHESE r_reg COMMA r_reg RPARENTHESE | MAX LPARENTHESE r_reg COMMA r_reg RPARENTHESE | CLIP r_reg BY r_reg | MANT f_reg | LOGB f_reg | FIX f_reg ( BY r_reg )? | TRUNC f_reg ( BY r_reg )?
   ;

//====================================================================================================	
floating_point_alu_op
   : f_reg EQU f_exp | COMP LPARENTHESE f_reg COMMA f_reg RPARENTHESE
   ;

f_exp
   : f_reg PLUS f_reg | f_reg MINUS f_reg | ABS LPARENTHESE f_reg PLUS f_reg RPARENTHESE | ABS LPARENTHESE f_reg MINUS f_reg RPARENTHESE | LPARENTHESE f_reg PLUS f_reg RPARENTHESE DIV INT | MINUS f_reg | ABS f_reg | PASS f_reg | RND f_reg | SCALB f_reg BY r_reg | FLOAT r_reg ( BY r_reg )? | RECIPS f_reg | RSQRTS f_reg | f_reg COPYSIGN f_reg | MIN LPARENTHESE f_reg COMMA f_reg RPARENTHESE | MAX LPARENTHESE f_reg COMMA f_reg RPARENTHESE | CLIP f_reg BY f_reg | f_reg MULT f_reg
   ;

//====================================================================================================
multi_op
   : r_reg EQU multi_exp_r | MRF EQU multi_exp_mrf | MRB EQU multi_exp_mrb | mr EQU INT | ( mrf | mrb ) EQU r_reg | r_reg EQU ( mrf | mrb )
   ;

multi_r
   : r_reg MULT r_reg multi_mod2?
   ;

multi_exp_r
   : multi_r | mr add_or_sub multi_r | SAT mr multi_mod1? | RND mr multi_mod1? | mr
   ;

multi_exp_mrf
   : multi_r | MRF add_or_sub multi_r | SAT MRF multi_mod1? | RND MRF multi_mod1?
   ;

multi_exp_mrb
   : multi_r | MRB add_or_sub multi_r | SAT MRB multi_mod1? | RND MRB multi_mod1?
   ;

mr
   : MRB | MRF
   ;

//====================================================================================================	
shifter_op
   : r_reg EQU shifter_exp | BTST r_reg BY sec_op | f_reg EQU FUNPACK r_reg
   ;

shifter_exp
   : LSHIFT r_reg BY sec_op | r_reg OR LSHIFT r_reg BY sec_op | ASHIFT r_reg BY sec_op | r_reg OR ASHIFT r_reg BY sec_op | ROT r_reg BY sec_op | BCLR r_reg BY sec_op | BSET r_reg BY sec_op | BTGL r_reg BY sec_op | FDEP r_reg BY sec_op2 ( LPARENTHESE SE RPARENTHESE )? | FEXT r_reg BY sec_op2 ( LPARENTHESE SE RPARENTHESE )? | r_reg OR FDEP r_reg BY sec_op2 | EXP r_reg ( LPARENTHESE EX RPARENTHESE )? | LEFTZ r_reg | LEFTO r_reg | FPACK f_reg
   ;

sec_op
   : r_reg | atom | MINUS atom
   ;

sec_op2
   : r_reg | bit_data
   ;

bit_data
   : INT COLON INT
   ;

add_or_sub
   : PLUS | MINUS
   ;

dual_op
   : dual_add_r | parallel_multi
   ;

dual_add_r
   : r_reg EQU r_reg PLUS r_reg COMMA r_reg EQU r_reg MINUS r_reg
   ;

parallel_multi
   : multi_op ( COMMA fixpoint_alu_op )+ | floating_point_alu_op ( COMMA floating_point_alu_op )+
   ;

//====================================================================================================
/*
dual_op	:	dual_add_r
	|	dual_add_f
	|	parallel_multi_1
	|	parallel_multi_2
	|	parallel_multi_3
	|	parallel_multi_4
	//|	parallel_multi_add_f
	|	parallel_multi_add_r
	;
	

	
//-------------
parallel_multi_1
	:	parallel_multi_1_1 COMMA! ( parallel_multi_1_2 | parallel_multi_1_3 )
	;
parallel_multi_1_1
	:	r_reg EQU r3_0 MULT r7_4 LPARENTHESE SSFR RPARENTHESE
		-> ^(EQU r_reg ^(MULT r3_0 r7_4 SSFR))
	;
parallel_multi_1_2
	:	r_reg EQU r11_8 add_or_sub r15_12
		-> ^(EQU r_reg ^(add_or_sub r11_8 r15_12))
	;
parallel_multi_1_3
	:	r_reg EQU LPARENTHESE r11_8 PLUS r15_12 RPARENTHESE DIV INT
		-> ^(EQU r_reg ^(DIV ^(PLUS r11_8 r15_12) INT))
	;
//-------------
parallel_multi_2
	:	parallel_multi_2_1 COMMA! ( parallel_multi_1_2 | parallel_multi_1_3 )
	;
parallel_multi_2_1
	:	MRF EQU MRF add_or_sub r3_0 MULT r7_4 LPARENTHESE SSF RPARENTHESE
		-> ^(EQU MRF ^(add_or_sub MRF ^(MULT r3_0 r7_4 SSF)))
	;
//-------------
parallel_multi_3
	: 	parallel_multi_3_1 COMMA! ( parallel_multi_1_2 | parallel_multi_1_3 )
	;	
parallel_multi_3_1
	:	r_reg EQU MRF add_or_sub r3_0 MULT r7_4 LPARENTHESE SSFR RPARENTHESE
		-> ^(EQU r_reg ^(add_or_sub MRF ^(MULT r3_0 r7_4 SSFR)))
	;
//--------------
parallel_multi_4
	:	f_reg EQU f_exp (COMMA f_reg EQU f_exp)+
		-> ^(EQU f_reg f_exp) (^(EQU f_reg f_exp))+
	;
	/*parallel_multi_4_1 COMMA! (
	|	parallel_multi_4_1_1
	|	parallel_multi_4_1_2
	|	parallel_multi_4_1_3
	|	parallel_multi_4_1_4
	|	parallel_multi_4_1_5
	|	parallel_multi_4_1_6
	|	parallel_multi_4_1_7
	)
	;
parallel_multi_4_1
	:	f_reg EQU f3_0 MULT f7_4
		-> ^(EQU f_reg ^(MULT f3_0 f7_4))
	;
parallel_multi_4_1_1
	:	f_reg EQU f11_8 add_or_sub f15_12
		-> ^(EQU f_reg ^(add_or_sub f11_8 f15_12))
	;
parallel_multi_4_1_2
	:	f_reg EQU FLOAT r11_8 BY r15_12
		-> ^(EQU f_reg ^(FLOAT r11_8 r15_12))
	;
parallel_multi_4_1_3
	:	f_reg EQU FIX f11_8 BY r15_12
		-> ^(EQU f_reg ^(FIX f11_8 r15_12))
	;

parallel_multi_4_1_4
	:	f_reg EQU LPARENTHESE f11_8 PLUS f15_12 RPARENTHESE DIV INT
		-> ^(EQU f_reg ^(DIV ^(PLUS f11_8 f15_12) INT))
	;
parallel_multi_4_1_5
	:	f_reg EQU ABS f11_8
		-> ^(EQU f_reg ^(ABS f11_8))
	;

parallel_multi_4_1_6
	:	f_reg EQU MAX LPARENTHESE f11_8 COMMA f15_12 RPARENTHESE
		-> ^(EQU f_reg ^(MAX f11_8 f15_12))
	;

parallel_multi_4_1_7
	:	f_reg EQU MIN LPARENTHESE f11_8 COMMA f15_12 RPARENTHESE
		-> ^(EQU f_reg ^(MIN f11_8 f15_12))
	;

parallel_multi_add_r
	:	r_reg EQU r3_0 MULT r7_4 LPARENTHESE SSFR RPARENTHESE COMMA r_reg EQU r11_8 PLUS r15_12 COMMA r_reg EQU r11_8 MINUS r15_12
		-> ^(EQU r_reg ^(MULT r3_0 r7_4 SSFR)) ^(EQU r_reg ^(PLUS r11_8 r15_12)) ^(EQU r_reg ^(MINUS r11_8 r15_12))
	;
//parallel_multi_add_f
//	:	f_reg EQU f3_0 MULT f7_4 COMMA f_reg EQU f11_8 PLUS f15_12 COMMA f_reg EQU f11_8 MINUS f15_12
//		-> ^(EQU f_reg ^(MULT f3_0 f7_4)) ^(EQU f_reg ^(PLUS f11_8 f15_12)) ^(EQU f_reg ^(MINUS f11_8 f15_12))
//	;
*/
//====================================================================================================
// program flow control instructions
flow_control_exp
   : flow_contorl_8 | flow_control_9_and_11 | flow_control_10 | flow_control_8a | flow_control_8b | flow_control_9a | flow_control_9b | flow_control_11a | flow_control_11b | flow_control_12 | flow_control_13
   ;

/////////////////////////////////
flow_contorl_8
   : IF condition flow_contorl_8_exp
   ;

flow_contorl_8_exp
   : flow_control_8a | flow_control_8b
   ;

flow_control_9_and_11
   : IF condition flow_control_9_and_11_exp COMMA ELSE compute
   ;

flow_control_9_and_11_exp
   : flow_control_9a | flow_control_9b | flow_control_11a | flow_control_11b
   ;

flow_control_10
   : IF condition JUMP flow_control_10_frag COMMA ELSE ( compute COMMA )? mov_exp_1_1
   ;

flow_control_10_frag
   : mem_addr_md_ic | jump_addr_pc
   ;

flow_control_12
   : LCNTR EQU lcntr_v ( COMMA DO jump_addr_int_or_pc UNTIL LCE )
   ;

lcntr_v
   : value_exp | u_reg
   ;

//DO <addr24>          UNTIL termination
//   (PC, <reladdr24>)	
flow_control_13
   : DO jump_addr_int_or_pc UNTIL condition
   ;

flow_control_8a
   : JUMP jump_addr_int jump_modifier?
   ;

flow_control_8b
   : CALL jump_addr_int jump_modifier2?
   ;

flow_control_9a
   : JUMP flow_control_10_frag jump_modifier? ( COMMA compute )?
   ;

flow_control_9b
   : CALL flow_control_10_frag jump_modifier2? ( COMMA compute )?
   ;

flow_control_11a
   : RTS jump_modifier3? ( COMMA compute )?
   ;

flow_control_11b
   : RTI jump_modifier2? ( COMMA compute )?
   ;

//////////////////////////////////
jump_addr_int_or_pc
   : jump_addr_int | jump_addr_pc
   ;

jump_addr_md_or_pc
   : mem_addr_md_ic | jump_addr_pc
   ;

jump_addr_pc
   : LPARENTHESE PC COMMA value_exp RPARENTHESE
   ;

jump_addr_int
   : value_exp
   ;

jump_modifier
   : jump_modifier_
   ;

jump_modifier_
   : LPARENTHESE ( jump_modifier_1 | LA | CI ) RPARENTHESE
   ;

jump_modifier_1
   : DB ( COMMA ( LA | CI ) )?
   ;

jump_modifier2
   : LPARENTHESE DB RPARENTHESE
   ;

jump_modifier3
   : jump_modifier3_
   ;

jump_modifier3_
   : LPARENTHESE ( jump_modifier3_1 | LR ) RPARENTHESE
   ;

jump_modifier3_1
   : DB ( COMMA LR )?
   ;

//====================================================================================================
//
imm_mov_exp
   : imm_mov_14a | imm_mov_14b | imm_mov_16 | imm_mov_17
   ;

imm_mov_14a
   : ( mem_addr_dm_int | mem_addr_pm_int ) EQU u_reg
   ;

imm_mov_15a
   : ( mem_addr_dm_int_ia | mem_addr_pm_int_ic ) EQU u_reg
   ;

imm_mov_14b
   : u_reg EQU ( mem_addr_dm_int | mem_addr_pm_int )
   ;

imm_mov_15b
   : u_reg EQU ( mem_addr_dm_int_ia | mem_addr_pm_int_ic )
   ;

imm_mov_16
   : ( mem_addr_dm_ia_mb | mem_addr_pm_ic_md ) EQU value_exp
   ;

imm_mov_17
   : u_reg2 EQU value_exp
   ;

u_reg2
   : d_reg | PC | PCSTK | PCSTKP | FADDR | DADDR | LADDR | CURLCNTR | dag_reg | PX1 | PX2 | PX | TPERIOD | TCOUNT | s_reg
   ;

misc_exp
   : BIT ( SET | CLR | TGL | TST | XOR ) s_reg value_exp | BITREV ( mem_addr_ia_int | mem_addr_ic_int ) | MODIFY LPARENTHESE ia COMMA value_exp RPARENTHESE | MODIFY LPARENTHESE ic COMMA value_exp RPARENTHESE | misc_20 ( COMMA misc_20 )* | FLUSH CACHE | NOP | IDLE | IDLE16 | CJUMP jump_addr_int_or_pc jump_modifier2? | RFRAME
   ;

misc_20
   : ( PUSH | POP ) ( LOOP | STS | PCSTK )
   ;

//====================================================================================================
directive_exp
   : DOT_ALGIGN INT | DOT_COMPRESS | DOT_EXTERN ID ( COMMA ID )* | DOT_FILE StringLiteral | DOT_FILE_ATTR . | DOT_FORCECOMPRESS | DOT_GLOBAL ID ( COMMA ID )* | DOT_IMPORT StringLiteral ( COMMA StringLiteral )* | DOT_LEFTMARGIN value_exp | DOT_LIST | DOT_LIST_DATA | DOT_LIST_DATFILE | DOT_LIST_DEFTAB value_exp | DOT_LIST_LOCTAB value_exp | DOT_LIST_WRAPDATA | DOT_NEWPAGE | DOT_NOCOMPRESS | DOT_NOLIST_DATA | DOT_NOLIST_DATFILE | DOT_NOLIST_WRAPDATA | DOT_PAGELENGTH value_exp | DOT_PAGEWIDTH value_exp | DOT_PRECISION ( EQU? ) INT | DOT_ROUND_MINUS | DOT_ROUND_NEAREST | DOT_ROUND_PLUS | DOT_ROUND_ZERO | DOT_PREVIOUS | DOT_WEAK ID
   ;

//====================================================================================================
b_reg
   : B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8 | B9 | B10 | B11 | B12 | B13 | B14 | B15
   ;

l_reg
   : L0 | L1 | L2 | L3 | L4 | L5 | L6 | L7 | L8 | L9 | L10 | L11 | L12 | L13 | L14 | L15
   ;

r_reg
   : R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
   ;

f_reg
   : F0 | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | F13 | F14 | F15
   ;

// system register
s_reg
   : MODE1 | MODE2 | IRPTL | IMASK | IMASKP | ASTAT | STKY | USTAT1 | USTAT2
   ;

ia
   : I0 | I1 | I2 | I3 | I4 | I5 | I6 | I7
   ;

mb
   : M0 | M1 | M2 | M3 | M4 | M5 | M6 | M7
   ;

ic
   : I8 | I9 | I10 | I11 | I12 | I13 | I14 | I15
   ;

md
   : M8 | M9 | M10 | M11 | M12 | M13 | M14 | M15
   ;

i_reg
   : ia | ic
   ;

m_reg
   : mb | md
   ;

dag_reg
   : i_reg | m_reg | b_reg | l_reg
   ;

d_reg
   : r_reg | f_reg
   ;

// universal register
u_reg
   : d_reg | PC | PCSTK | PCSTKP | FADDR | DADDR | LADDR | CURLCNTR | LCNTR | dag_reg | PX1 | PX2 | PX | TPERIOD | TCOUNT | s_reg
   ;

condition
   : ccondition
   ;

ccondition
   : EQ | LT | LE | AC | AV | MV | MS | SV | SZ | FLAG0_IN | FLAG1_IN | FLAG2_IN | FLAG3_IN | TF | BM | LCE | NOT LCE | NE | GE | GT | NOT AC | NOT AV | NOT MV | NOT MS | NOT SV | NOT SZ | NOT FLAG0_IN | NOT FLAG1_IN | NOT FLAG2_IN | NOT FLAG3_IN | NOT TF | NBM | FOREVER | TRUE
   ;

multi_mod1
   : multi_mod1_
   ;

multi_mod1_
   : LPARENTHESE ( SI | UI | SF | UF ) RPARENTHESE
   ;

multi_mod2
   : multi_mod2_
   ;

multi_mod2_
   : LPARENTHESE ( SSI | SUI | USI | UUI | SSF | SUF | USF | UUF | SSFR | SUFR | USFR | UUFR ) RPARENTHESE
   ;

r3_0
   : R0 | R2 | R3
   ;

r7_4
   : R4 | R5 | R6 | R7
   ;

r11_8
   : R8 | R9 | R10 | R11
   ;

r15_12
   : R12 | R13 | R14 | R15
   ;

f3_0
   : F0 | F2 | F3
   ;

f7_4
   : F4 | F5 | F6 | F7
   ;

f11_8
   : F8 | F9 | F10 | F11
   ;

f15_12
   : F12 | F13 | F14 | F15
   ;

addr
   : ID | INT
   ;

mrf
   : MR0F | MR1F | MR2F
   ;

mrb
   : MR0B | MR1B | MR2B
   ;
