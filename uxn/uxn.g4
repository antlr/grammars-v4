/*
BSD License

Copyright (c) 2026, Tom Everett
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

// Grammar for Uxntal, the concatenative assembly language of the Uxn
// virtual machine. See https://wiki.xxiivv.com/site/uxntal.html and the
// formal grammar published at https://wiki.xxiivv.com/etc/uxntal.bnf.txt.
//
// A Uxntal program is a whitespace-separated sequence of words. The
// leading rune of a word (@ & $ | # " ! ? . , ; - _ = ( % { } [ ]) selects
// its category: opcodes, labels, padding, literals/raw addressing modes,
// strings, comments, macros, and the bracket/ignore tokens used purely
// for readability. A bare word with no leading rune is either a macro or
// label reference (an "immediate" invocation) or raw hex data.

grammar uxn;

program
   : statement* EOF
   ;

statement
   : opcode
   | parentLabel
   | childLabel
   | padRel
   | padAbs
   | literalHex
   | rawString
   | immJmi
   | immJci
   | literalZep
   | literalRel
   | literalAbs
   | rawZep
   | rawRel
   | rawAbs
   | ignore
   | comment
   | macroDef
   | anonLabel
   | immJsi
   ;

opcode
   : OPCODE
   ;

parentLabel
   : PARENT_LABEL
   ;

childLabel
   : CHILD_LABEL
   ;

padRel
   : PAD_REL
   ;

padAbs
   : PAD_ABS
   ;

literalHex
   : HEX_LIT
   ;

rawString
   : STRING
   ;

immJsi
   : NAME
   ;

immJmi
   : JMI
   ;

immJci
   : JCI
   ;

literalZep
   : LIT_ZEP
   ;

literalRel
   : LIT_REL
   ;

literalAbs
   : LIT_ABS
   ;

rawZep
   : RAW_ZEP
   ;

rawRel
   : RAW_REL
   ;

rawAbs
   : RAW_ABS
   ;

ignore
   : IGNORE
   ;

comment
   : COMMENT
   ;

anonLabel
   : LBRACE statement* RBRACE NAME?
   ;

macroDef
   : MACRO_START NAME comment? LBRACE statement* RBRACE
   ;

COMMENT
   : '(' (COMMENT | ~[()])*? ')'
   ;

MACRO_START
   : '%'
   ;

LBRACE
   : '{'
   ;

RBRACE
   : '}'
   ;

STRING
   : '"' ~[ \t\r\n]+
   ;

PAD_REL
   : '$' WORDCHAR*
   ;

PAD_ABS
   : '|' WORDCHAR*
   ;

HEX_LIT
   : '#' WORDCHAR*
   ;

PARENT_LABEL
   : '@' WORDCHAR*
   ;

CHILD_LABEL
   : '&' WORDCHAR*
   ;

JMI
   : '!' WORDCHAR*
   ;

JCI
   : '?' WORDCHAR*
   ;

LIT_ZEP
   : '.' WORDCHAR*
   ;

LIT_REL
   : ',' WORDCHAR*
   ;

LIT_ABS
   : ';' WORDCHAR*
   ;

RAW_ZEP
   : '-' WORDCHAR*
   ;

RAW_REL
   : '_' WORDCHAR*
   ;

RAW_ABS
   : '=' WORDCHAR*
   ;

IGNORE
   : (')' | '[' | ']') WORDCHAR*
   ;

OPCODE
   : BASEOP MODE*
   ;

NAME
   : NAMESTART WORDCHAR*
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

fragment MODE
   : [2kr]
   ;

fragment BASEOP
   : 'BRK' | 'INC' | 'POP' | 'DUP' | 'NIP' | 'SWP' | 'OVR' | 'ROT'
   | 'EQU' | 'NEQ' | 'GTH' | 'LTH' | 'JMP' | 'JCN' | 'JSR' | 'STH'
   | 'LDZ' | 'STZ' | 'LDR' | 'STR' | 'LDA' | 'STA' | 'DEI' | 'DEO'
   | 'ADD' | 'SUB' | 'MUL' | 'DIV' | 'AND' | 'ORA' | 'EOR' | 'SFT'
   | 'LIT'
   ;

fragment RUNECHAR
   : [|$@&,_.\-;=!?#"~%{}()[\]]
   ;

fragment NAMESTART
   : ~[ \t\r\n|$@&,_.\-;=!?#"~%{}()[\]]
   ;

fragment WORDCHAR
   : ~[ \t\r\n()[\]{}]
   ;
