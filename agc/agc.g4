/*
BSD License

Copyright (c) 2013, Tom Everett
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
/*
    http://www.ibiblio.org/apollo/
*/
/*
    thtp://www.ibiblio.org/apollo/hrst/archive/1689.pdf
*/
/*
    http://www.ibiblio.org/apollo/assembly_language_manual.html
*/

grammar agc;

prog
    : line+
    ;

line
    : comment_line
    | blank_line
    | instruction_line
    | erase_line
    | assignment_line
    ;

blank_line
    : label? eol
    ;

comment_line
    : ws? comment eol
    ;

// an instruction "line" can span many lines in the file, and can have comment lines in the middle of it
instruction_line
    : label? ws opcodes (eol comment_line)? argument (eol argument)* eol
    ;

// erase can be specified with no variable
erase_line
    : variable? ws 'ERASE' (ws? expression)* (ws comment)? eol
    ;

// assignment with no RHS is legal
assignment_line
    : variable ws? ('=' | 'EQUALS') (ws? expression)* (ws comment)? eol
    ;

opcodes
    : opcode (ws opcode)?
    ;

argument
    : (ws expression)* (ws comment)?
    ;

ws
    : WS
    ;

eol
    : WS? EOL
    ;

comment
    : COMMENT
    ;

label
    : LABEL
    ;

variable
    : LABEL
    | (LPAREN LABEL RPAREN)
    ;

expression 
    : multiplyingExpression ((PLUS|MINUS) multiplyingExpression)*
    ;

multiplyingExpression  
    : atom ((TIMES|DIV) atom)*
    ;

atom 
    : inte
    | decimal
    | variable
    | label
    | register
    ;

inte
    : INTE
    ;

decimal
    : ('+' | '-')? DECIMAL
    ;

register
    :'A'
    | 'L'
    | 'Q'
    | 'EB'
    | 'FB'
    | 'Z'
    | 'BB'
    | 'ARUPT'
    | 'LRUPT'
    | 'QRUPT'
    | 'QRUPT'
    | 'BBRUPT'
    | 'BRUPT'
    | 'CYR'
    | 'SR'
    | 'CYL'
    | 'EDOP'
    | 'TIME2'
    | 'TIME1'
    | 'TIME3'
    | 'TIME4'
    | 'TIME5'
    | 'TIME6'
    | 'CDUX'
    | 'CDUY'
    | 'CDUZ'
    | 'OPTY'
    | 'OPTX'
    | 'PIPAX'
    | 'PIPAY'
    | 'PIPAZ'
    | 'Q-RHCCTR'
    | 'RHCP'
    | 'P-RHCCTR'
    | 'RHCY'
    | 'R-RHCCTR'
    | 'RHCR'
    | 'INLINK'
    | 'RNRAD'
    | 'GYROCTR'
    | 'GYROCMD'
    | 'CDUXCMD'
    | 'CDUYCMD'
    | 'CDUZCMD'
    | 'OPTYCMD'
    | 'OPTXCMD'
    | 'THRUST'
    | 'LEMONM'
    | 'OUTLINK'
    | 'ALTM'
    ;

opcode
    : standard_opcode
    | pseudo_opcode
    | axt_opcode
    ;

// Address to Index
axt_opcode
    : 'AXT,1'
    | 'AXT,2'
    ;

pseudo_opcode
    : '1DNADR'
    | '2DNADR'  
    | '3DNADR'  
    | '4DNADR'  
    | '5DNADR'  
    | '6DNADR'         
    | 'DNCHAN'
    | 'DNPTR'
    | '-1DNADR'
    | '-2DNADR'  
    | '-3DNADR'  
    | '-4DNADR'  
    | '-5DNADR'  
    | '-6DNADR'         
    | '-DNCHAN'
    | '-DNPTR'
    | '2DEC'
    | '2DEC*'
    | '2DNADR'
    | '-2DNADR'
    | '2FCADR' // embed a double-word constant
    | '3DNADR'
    | '-3DNADR'
    | '4DNADR'
    | '-4DNADR'
    | '5DNADR'
    | '-5DNADR'
    | '6DNADR'
    | '-6DNADR'
    | 'BANK'
    | 'BLOCK'
    | 'BNKSUM'
    | 'COUNT'
    | 'COUNT*'
    | 'DEC'     // embed a single-precision (SP) constant
    | '2DEC'    // embed a double precision constant
    | '2FCADR'   // embed a double-word constant, to be used later by the DTCF instruction
    | 'OCT'     // embed an octal constant
    | 'SETLOC'
    | 'SUBRO'
    ;
      
standard_opcode
    : 'TC'      // transfer control   
    | 'TCR'     // transfer control
    | 'CCS'     // Count, Compare, and Skip      
    | 'TCF'     // Transfer Control to Fixed
    | 'DAS'     // Double Add to Storage
    | 'LXCH'    // Exchange L and K
    | 'INCR'    // Increment
    | 'AD'      // add to accumulator
    | 'ADS'     // Add to Storage
    | 'CA'      // Clear and Add
    | 'CS'      // Clear and Subtract
    | 'INDEX'   // Index
    | 'DXCH'    // double exchange
    | 'TS'      // Transfer to Storage
    | 'XCH'     // Exchange A and K
    | 'AD'      // AD
    | 'MASK'    // Mask A by K
    | 'MSK'     // Mask A by K
    | 'READ'    // Read Channel KC
    | 'WRITE'   // Write Channel KC
    | 'RAND'    // Read and Mask
    | 'WAND'    // Write and Mask
    | 'ROR'     // Read and Superimpose
    | 'WOR'     // Write and Superimpose
    | 'RXOR'    // Read and Invert
    | 'EDRUPT'  // for machine checkout only
    | 'BZF'     // Branch Zero to Fixed
    | 'MSU'     // Modular Subtract
    | 'QXCH'    // Exchange Q and K
    | 'AUG'     // augment
    | 'DIM'     // diminish
    | 'DCA'     // Double Clear and Add
    | 'DCS'     // Double Clear and Subtract
    | 'SU'      // subtract
    | 'BZMF'    // Branch Zero or Minus to Fixed
    | 'MP'      // Multiply
    | 'XXALQ'   // Execute Extracode Using A, L, and Q
    | 'XLQ'     // Execute Using L and Q
    | 'RETURN'  // Return from Subroutine
    | 'RELINT'  // Enable Interrupts
    | 'INHINT'  // Disable Interrupts
    | 'EXTEND'  // extend
    | 'NOOP'    // No-operation
    | 'DDOUBL'  // Double Precision Double
    | 'DTCF'    // Double Transfer Control, Switching F Bank
    | 'COM'     // Complement the Contents of A
    | 'ZL'      // Zero L
    | 'RESUME'  // Resume Interrupted Program
    | 'DTCB'    // Double Transfer Control, Switching Both Banks
    | 'OVSK'    // Overflow Skip
    | 'TCAA'    // Transfer Control to Address in A
    | 'DOUBLE'  // Double the Contents of A
    | 'ZQ'      // Zero Q
    | 'DCOM'    // Double Complement
    | 'SQUARE'  // Square the Contents of A
    | 'PINC'    // Add +1 in 1's-complement fashion to a counter
    | 'PCDU'    // Add +1 in 2's-complement fashion to a counter.  
    | 'MINC'    // Add -1 in 1's-complement fashion to a counter.
    | 'MCDU'    // Add -1 in 2's-complement fashion to a counter.
    | 'DINC'    // look at the docs
    | 'SHINC'   // look at the docs
    | 'SHANC'   // look at the docs
    | 'INOTRD'  // look at the docs
    | 'INOTLD'  // look at the docs
    | 'FETCH'   // look at the docs
    | 'STORE'   // look at the docs
    | 'GOJ'     // Jump to location 04000 octal.
    | 'TCSAJ'   // look at the docs
    | 'CAF'     // Clear and Add Fixed
    | 'CAE'     // Clear and Add Erasable
    | 'CADR'
    | 'DMOVE'
    | 'VMOVE'
    | 'SMOVE'
    | 'DSU'
    | 'RTB'
    | 'ITC'
    | 'NOLOD'
    | 'EXIT'
    | 'BPL'
    | 'SIN'
    | 'COS'
    | 'CAD'
    | 'TEST'
    | 'VXSC'
    | 'ITC'  
    | 'DAD'
    | 'VXV'
    | 'VAD'
    | 'DAD'
    | 'BPL'
    | 'DMP'
    | 'BOV'
    | 'VXV'
    | 'VAD'
    | 'UNIT'
    | 'OCTAL'
    | 'ADRES'
    | 'ABVAL'   
    | 'COMP'
    | 'DV'      // Divide
    | 'NDX'     // INDEX (alternative syntax)
    | 'POUT'    // look at the docs 
    | 'MOUT'    // look at the docs
    | 'ZOUT'    // look at the docs
    | 'LODON'   // this is used in a couple places in a "2-opcodes on a line" format
    | 'TSLT'    // this is used in a couple places in a "2-opcodes on a line" format
    ;

//
// labels can begin with + or -, letters or digits 
//
// labels can contain "&" as well as math symbols "+-*/" and "."
//
LABEL
    : [a-zA-Z0-9_.+\\\-/*=&]+
    ; 

INTE
    : [0-9]+ ('DEC' | 'D')
    ;

DECIMAL
    : ([0-9]+ ('.' [0-9]+)?)
    | ('.' [0-9]+)
    ;

COMMENT
    : '#' ~[\r\n]*
    ;

PLUS 
    : '+'
    ;

MINUS 
    : '-'
    ;

TIMES 
    : '*'
    ;

DIV 
    : '/'
    ;

COMMA
    : ','
    ;

LPAREN 
    : '('
    ;

RPAREN 
    : ')'
    ;

EOL
    : [\r\n]+
    ;

WS
    : [ \t]+
    ;
