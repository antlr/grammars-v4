/*
BSD License

Copyright (c) 2018, Tom Everett
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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Riscv64G;

options {
   caseInsensitive = true;
}

prog
    : line* EOF
    ;

line
    : lbl? (assemblerdirective | instruction)? EOL
    ;

assemblerdirective
   :  directives
   ;

directives
   : '.file' label
   | '.global' label
   | '.globl' label
   | '.weak' label
   | '.type' label ',' '@'label
   | '.align' number
   | '.balign' number
   | '.zero' number
   | '.2byte' expressionlist
   | '.4byte' expressionlist
   | '.8byte' expressionlist
   | '.half' expressionlist
   | '.word' expressionlist
   | '.dword' expressionlist
   | '.string' string
   | '.include' string 
   | '.assicz' label
   | '.assic' label
   | '.float' expressionlist
   | '.double' expressionlist
   | '.comm' label ',' number ',' number
   | '.common' label ',' number ',' number
   | '.ident' label
   | '.option' OPTIONARG
   | '.option arch' ',' (extensionsorfullarch | label)
   | '.section' ('.text' | '.data' | '.rodata' | '.bss')?
   | '.text'
   | '.data'
   | '.rodata'
   | '.bss'
   | '.pushsection' label
   | '.popsection' label
   | '.zero' number
   | '.space' number (',' number)*
   | '.skip' number (',' number)*  
   | '.set' label ',' expression
   | '.equ' label ',' (number | label)
   | '.size' label ',' (number | label)
   | '.macro' label (',' label)*
   | '.endm'
   | '.quad' expressionlist
   | '.dtprelword' expressionlist
   | '.dtpreldword' expressionlist
   | '.sleb' expression
   | '.uleb' expression
   | '.p2align' 'p2' ',' number (',' number)?
   | '.variant_cc' label
   | '.attribute' label ',' (number | label)
   ;

lbl
   : label COLON?
   ;

expressionlist
   : expression (',' expression)*
   ;

expression
   : expression_ ((OP|'*') expression_)*
   ;

expression_
   : register
   | number
   | label
   ;

extensionsorfullarch
   : extensions_
   | fullarchstr
   ;

extensions_
   : extension ',' extension
   | extension
   ;

fullarchstr
   : label
   ;

extension
   : OP EXTENSIONNAME (number | number 'p' number)?
   ;

OP
   : '+'
   | '-'
   ;

instruction
   : rtype
   | itype
   | stype
   | utype
   | jtype
   | btype
   | csrtype
   ;

register
   : FREGS
   | XREGS
   | CSRREGS
   ;

rtype
   : 'add' xregs ',' xregs ',' xregs
   | 'mv' xregs ',' xregs
   | 'la' xregs ',' label
   | 'sub' xregs ',' xregs ',' xregs
   | 'sll' xregs ',' xregs ',' xregs
   | 'slt' xregs ',' xregs ',' xregs
   | 'sltu' xregs ',' xregs ',' xregs
   | 'xor' xregs ',' xregs ',' xregs
   | 'srl' xregs ',' xregs ',' xregs
   | 'sra' xregs ',' xregs ',' xregs
   | 'or' xregs ',' xregs ',' xregs
   | 'and' xregs ',' xregs ',' xregs
   | 'addw' xregs ',' xregs ',' xregs
   | 'subw' xregs ',' xregs ',' xregs
   | 'sllw' xregs ',' xregs ',' xregs
   | 'srlw' xregs ',' xregs ',' xregs
   | 'sraw' xregs ',' xregs ',' xregs
   | 'mul' xregs ',' xregs ',' xregs
   | 'mulh' xregs ',' xregs ',' xregs
   | 'mulhsu' xregs ',' xregs ',' xregs
   | 'mulhu' xregs ',' xregs ',' xregs
   | 'div' xregs ',' xregs ',' xregs
   | 'divu' xregs ',' xregs ',' xregs
   | 'rem' xregs ',' xregs ',' xregs
   | 'remu' xregs ',' xregs ',' xregs
   | 'mulw' xregs ',' xregs ',' xregs
   | 'divw' xregs ',' xregs ',' xregs
   | 'divuw' xregs ',' xregs ',' xregs
   | 'remw' xregs ',' xregs ',' xregs
   | 'remuw' xregs ',' xregs ',' xregs
   | 'lr.w' xregs ',' '(' xregs ')'
   | 'sc.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amoswap.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amoadd.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amoxor.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amoand.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amoor.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amomin.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amomax.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amominu.w' xregs ',' xregs ',' '(' xregs ')'
   | 'amomaxu.w' xregs ',' xregs ',' '(' xregs ')'
   | 'lr.d' xregs ',' '(' xregs ')'
   | 'sc.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amoswap.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amoadd.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amoxor.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amoand.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amoor.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amomin.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amomax.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amominu.d' xregs ',' xregs ',' '(' xregs ')'
   | 'amomaxu.d' xregs   xregs ',' '(' xregs ')'
   | 'fadd.s' fregs ',' fregs ',' fregs
   | 'fsub.s' fregs ',' fregs ',' fregs
   | 'fmul.s' fregs ',' fregs ',' fregs
   | 'fdiv.s' fregs ',' fregs ',' fregs
   | 'fsqrt.s' fregs ',' fregs
   | 'fsgnj.s' fregs ',' fregs ',' fregs
   | 'fsgnjn.s' fregs ',' fregs ',' fregs
   | 'fsgnjx.s' fregs ',' fregs ',' fregs
   | 'fmin.s' fregs ',' fregs ',' fregs
   | 'fmax.s' fregs ',' fregs ',' fregs
   | 'fcvt.w.s' xregs ',' fregs
   | 'fcvt.wu.s' xregs ',' fregs
   | 'fmv.x.w' xregs ',' fregs
   | 'feq.s' xregs ',' fregs ',' fregs
   | 'flt.s' xregs ',' fregs ',' fregs
   | 'fle.s' xregs ',' fregs ',' fregs
   | 'fclass.s' xregs ',' fregs
   | 'fcvt.s.w' fregs ',' xregs
   | 'fcvt.s.wu' fregs ',' xregs
   | 'fmv.w.x' fregs ',' xregs
   | 'fmadd.s' fregs ',' xregs ',' fregs ',' fregs
   | 'fmsub.s' fregs ',' fregs ',' fregs ',' fregs
   | 'fnmsub.s' fregs ',' fregs ',' fregs ',' fregs
   | 'fnmadd.s' fregs xregs fregs ',' fregs ',' fregs
   | 'fcvt.l.s' xregs ',' fregs
   | 'fcvt.lu.s' xregs ',' fregs
   | 'fcvt.s.l' fregs ',' xregs
   | 'fcvt.s.lu' fregs ',' xregs
   | 'fadd.d' fregs ',' fregs ',' fregs
   | 'fsub.d' fregs ',' fregs ',' fregs
   | 'fmul.d' fregs ',' fregs ',' fregs
   | 'fdiv.d' fregs ',' fregs ',' fregs
   | 'fsqrt.d' fregs ',' fregs
   | 'fsgnj.d' fregs ',' fregs ',' fregs
   | 'fsgnjn.d' fregs ',' fregs ',' fregs
   | 'fsgnjx.d' fregs ',' fregs ',' fregs
   | 'fmin.d' fregs ',' fregs ',' fregs
   | 'fmax.d' fregs ',' fregs ',' fregs
   | 'fcvt.d.s' fregs ',' fregs
   | 'fcvt.s.d' fregs ',' fregs
   | 'feq.d' xregs ',' fregs ',' fregs
   | 'flt.d' xregs ',' fregs ',' fregs
   | 'fle.d' xregs ',' fregs ',' fregs
   | 'fclass.d' xregs ',' fregs
   | 'fcvt.w.d' xregs ',' fregs
   | 'fcvt.wu.d' xregs ',' fregs
   | 'fcvt.d.w' fregs ',' xregs
   | 'fcvt.d.wu' fregs ',' xregs
   | 'fmadd.d' fregs ',' fregs ',' fregs ',' fregs
   | 'fmsub.d' fregs ',' fregs ',' fregs ',' fregs
   | 'fnmsub.d' fregs ',' fregs ',' fregs ',' fregs
   | 'fnmadd.d' fregs ',' fregs ',' fregs ',' fregs
   | 'fcvt.l.d' xregs ',' fregs
   | 'fcvt.lu.d' xregs ',' fregs
   | 'fmv.x.d' xregs ',' fregs
   | 'fcvt.d.l' fregs ',' xregs
   | 'fcvt.d.lu' fregs ',' xregs
   | 'fmv.d.x' fregs ',' xregs
   | 'fadd.q' fregs ',' fregs ',' fregs
   | 'fsub.q' fregs ',' fregs ','fregs
   | 'fmul.q' fregs ',' fregs ',' fregs
   | 'fdiv.q' fregs ',' fregs ',' fregs
   | 'fsqrt.q' fregs ',' fregs
   | 'fsgnj.q' fregs ',' fregs ',' fregs
   | 'fsgnjn.q' fregs ',' fregs ',' fregs
   | 'fsgnjx.q' fregs ',' fregs ',' fregs
   | 'fmin.q' fregs ',' fregs ',' fregs
   | 'fmax.q' fregs ',' fregs ',' fregs
   | 'fcvt.q.s' fregs ',' fregs
   | 'fcvt.s.q' fregs ',' fregs
   | 'fcvt.q.d' fregs ',' fregs
   | 'fcvt.d.q' fregs ',' fregs
   | 'feq.q' xregs ',' fregs ',' fregs
   | 'flt.q' xregs ',' fregs ',' fregs
   | 'fle.q' xregs ',' fregs ',' fregs
   | 'fclass.q' xregs ',' fregs
   | 'fcvt.wu.q'  xregs ',' fregs
   | 'fcvt.w.q' xregs ',' fregs
   | 'fcvt.q.w' fregs ',' xregs
   | 'fcvt.q.wu' fregs ',' xregs
   | 'fmadd.q' fregs ',' fregs ',' fregs ',' fregs
   | 'fmsub.q'  fregs ',' fregs ',' fregs ',' fregs
   | 'fnmsub.q' fregs ',' fregs ',' fregs ',' fregs
   | 'fnmadd.q' fregs ',' fregs ',' fregs ',' fregs
   | 'fcvt.l.q' xregs ',' fregs
   | 'fcvt.lu.q' xregs ',' fregs
   | 'fmv.x.q' xregs ',' fregs
   | 'fcvt.q.l' fregs ',' xregs
   | 'fcvt.q.lu' fregs ',' xregs
   | 'fmv.q.x' fregs ',' xregs
   | 'wfi'
   ;

itype
   : 'jalr' xregs ',' number '(' xregs ')'
   | 'lb' xregs ',' number '(' xregs ')'
   | 'lh' xregs ',' number '(' xregs ')'
   | 'lw' xregs ',' number '(' xregs ')'
   | 'lbu' xregs ',' number? '(' xregs ')'
   | 'lhu' xregs ',' number '(' xregs ')'
   | 'slli' xregs ',' xregs ',' number
   | 'srli' xregs ',' xregs ',' number
   | 'srai' xregs ',' xregs ',' number
   | 'addi' xregs ',' xregs ',' number
   | 'slti' xregs ',' xregs ',' number
   | 'sltiu' xregs ',' xregs ',' number
   | 'xori' xregs ',' xregs ',' number
   | 'ori' xregs ',' xregs ',' number
   | 'andi' xregs ',' xregs ',' number
   | 'lwu' xregs ',' number '(' xregs ')'
   | 'ld' xregs ',' number '(' xregs ')'
   | 'slli' xregs ',' xregs ',' number
   | 'srli' xregs ',' xregs ',' number
   | 'srai' xregs ',' xregs ',' number
   | 'addiw' xregs ',' xregs ',' number
   | 'slliw' xregs ',' xregs ',' number
   | 'srliw' xregs ',' xregs ',' number
   | 'sraiw' xregs ',' xregs ',' number
   | 'flw' fregs ',' number '(' xregs ')'
   | 'fld' fregs ',' number '(' xregs ')'
   | 'flq' fregs ',' number '(' xregs ')'
   | 'li' xregs ',' label
   | 'fence'
   | 'fence.i'
   | 'sfence.vma' xregs ',' xregs
   | 'ecall'
   | 'ebreak'
   | 'mret'
   | 'sret'
   | 'uret'
   | 'ret'
   | 'nop'
   ;

stype
   : 'sb' xregs ',' number '(' xregs ')'
   | 'sh' xregs ',' number '(' xregs ')'
   | 'sw' xregs ',' number '(' xregs ')'
   | 'sd' xregs ',' number '(' xregs ')'
   | 'fsw' fregs ',' number '(' xregs ')'
   | 'fsd' fregs ',' number '(' xregs ')'
   | 'fsq' fregs ',' number '(' xregs ')'
   ;

btype
   : 'beq' xregs ',' xregs ',' label
   | 'bne' xregs ',' xregs ',' label
   | 'bnez' xregs ',' label
   | 'blt' xregs ',' xregs ',' label
   | 'bge' xregs ',' xregs ',' label
   | 'bltu' xregs ',' xregs ',' label
   | 'bgeu' xregs ',' xregs ',' label
   | 'beqz' xregs ',' label
   | 'bltz' xregs ',' label
   ;

utype
   : 'lui' xregs ',' number
   | 'auipc' xregs ',' number
   ;

jtype
   : 'jal' (xregs ',')? label (',' xregs)?
   | 'j' label
   ;

csrtype
   : 'csrrw' xregs ',' csrregs ',' xregs
   | 'csrr' xregs ',' csrregs
   | 'csrrs' xregs ',' csrregs ',' xregs
   | 'csrrc' xregs ',' csrregs ',' xregs
   | 'csrrwi' xregs ',' csrregs ',' number
   | 'csrrsi' xregs ',' csrregs ',' number
   | 'csrrci' xregs ',' csrregs ',' number
   ;

xregs
   : XREGS
   ;

fregs
   : FREGS
   ;

csrregs
   : CSRREGS
   ;

comment
   : COMMENT
   ;

label
   : NAME
   | NUMBER
   ;

string
   : STRING
   ;

number
   : NUMBER
   ;

OPTIONARG
   : 'rvc' 
   | 'norvc' 
   | 'pic' 
   | 'nopic' 
   | 'relax' 
   | 'norelax' 
   | 'push' 
   | 'pop'
   ;

EXTENSIONNAME
   : 'A'
   | 'G'
   | 'C'
   | 'M'
   | 'F'
   | 'D'
   | 'Q'
   | 'L'
   | 'B'
   | 'J'
   | 'T'
   | 'P'
   | 'V'
   | 'N'
   ;

XREGS
   : 'zero'
   | 'ra'
   | 'sp'
   | 'gp'
   | 'tp'
   | 't0'
   | 't1'
   | 't2'
   | 's0'
   | 's1'
   | 'a0'
   | 'a1'
   | 'a2'
   | 'a3'
   | 'a4'
   | 'a5'
   | 'a6'
   | 'a7'
   | 's2'
   | 's3'     
   | 's4'
   | 's5'
   | 's6'
   | 's7'
   | 's8'
   | 's9'
   | 's10' 
   | 's11' 
   | 't3'
   | 't4'
   | 't5'
   | 't6'
   ;

FREGS
   : 'ft0'
   | 'ft1'
   | 'ft2'
   | 'ft3'
   | 'ft4'
   | 'ft5'
   | 'ft6'
   | 'ft7'
   | 'fs0'
   | 'fs1'
   | 'fa0'
   | 'fa1'
   | 'fa2'
   | 'fa3'
   | 'fa4'
   | 'fa5'
   | 'fa6'
   | 'fa7'
   | 'fs2'
   | 'fs3'
   | 'fs4'
   | 'fs5'
   | 'fs6'
   | 'fs7'
   | 'fs8'
   | 'fs9'
   | 'fs10'
   | 'fs11'
   | 'ft8'
   | 'ft9'
   | 'ft10' 
   | 'ft11'
   ;

CSRREGS
   :'fflags'
   |'frm'
   |'fcsr'
   |'sstatus'
   |'sie'
   |'sscratch'
   |'sepc'
   |'scause'
   |'stval'
   |'sip'
   |'satp'
   |'mhartid'
   |'mstatus'
   |'medeleg'
   |'mie'
   |'mscratch'
   |'mepc'
   |'mcause'
   |'mtval'
   |'mip'
   |'pmpcfg0'
   |'pmpaddr0'
   |'pmpaddr1'
   |'pmpaddr2'
   |'pmpaddr3'
   |'pmpaddr4'
   |'pmpaddr5' 
   |'pmpaddr6'
   |'pmpaddr7'
   ;

COMMENT
   : '#' ~ [\r\n]* -> channel(HIDDEN)
   ;

BLOCKCOMMENT
   : '/*' .*? '*/' -> channel(HIDDEN)
   ;

STRING
   : ["][A-Z_0-9.]*["]
   ;

NAME
   : [.A-Z_][A-Z0-9."_]*
   ;

COLON
   : ':'
   ;

NUMBER
   : '0x'? [0-9A-F]+
   | '-'? [0-9]+
   ;

EOL
   : [\r\n]+
   ;

WS
   : [ \t] -> skip
   ;