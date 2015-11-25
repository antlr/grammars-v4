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
    http://www.ibiblio.org/apollo/hrst/archive/1689.pdf
*/
/*
    http://www.ibiblio.org/apollo/assembly_language_manual.html
*/

grammar agc;

prog
    : instruction+
    ;

instruction
    : label? comment EOL                                    // comment
    | label? EOL                                            // blank line
    | label? opcode expression* comment? EOL                // opcode and operand on same line
    | label? (opcode expression? (EOL expression)*) comment? EOL        // opcode with operands on new lines
    | label? (opcode opcode (EOL expression)*) comment? EOL // opcode followed by opcode
    | assignment comment? EOL                               // assignment
    ;

assignment
    : variable '=' expression
    ;

opcode
    : 'SETLOC'
    | 'TC'      // transfer control   
    | 'TCR'     // transfer control
    | 'CCS'     // Count, Compare, and Skip      
    | 'TCF'     // Transfer Control to Fixed
    | 'DAS'     // Double Add to Storage
    | 'LXCH'    // Exchange L and K
    | 'INCR'    // Increment
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
    | 'PINC'
    | 'PCDU'
    | 'MINC'
    | 'MCDU'
    | 'DINC'
    | 'SHINC'
    | 'INOTRD'
    | 'INOTLD'
    | 'FETCH'
    | 'STORE'
    | 'GOJ'
    | 'TCSAJ'
    | 'CAF'     // Clear and Add Fixed
    | 'CAE'     // Clear and Add Erasable
    | 'OCT'
    | 'CADR'
    | 'BANK'
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
    | 'DEC'
    | 'TEST'
    | 'VXSC'
    | 'ITC'  
    | '2DEC'
    | 'EQUALS'
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
    | 'DV'      // Divide
    | 'NDX'     // INDEX
    ;

label
    : STRING
    ;

comment
    : COMMENT
    ;

variable
    : STRING (COMMA number)?
    | STRING LPAREN variable RPAREN
    ;

expression 
    : multiplyingExpression ((PLUS|MINUS) multiplyingExpression)*
    ;

multiplyingExpression  
    : atom ((TIMES|DIV) atom)*
    ;

atom 
    : number
    | variable
    ;

number
    : FLOAT
    | DECIMAL
    ;

STRING
    : [-a-zA-Z0-9] [a-zA-Z0-9_.+\-/*]*
    ; 

DECIMAL
    : [0-9]+ 'DEC'
    ;

FLOAT
    : ('+' | '-')? [0-9]+ ('.' [0-9]*)?
    | ('+' | '-')? '.' [0-9]+
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
    : '\r'? '\n'
    ;

WS
    : [ \t\r\n]->skip
    ;