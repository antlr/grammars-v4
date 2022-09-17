/*
 BSD License
 
 Copyright (c) 2021, Tom Everett All rights reserved.
 
 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:
 
 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
grammar karel;

karel
   : 'BEGINNING-OF-PROGRAM' definition* 'BEGINNING-OF-EXECUTION' statement* 'END-OF-EXECUTION' 'END-OF-PROGRAM' EOF
   ;

definition
   : 'DEFINE' IDENTIFIER 'AS' statement
   ;

statement
   : block
   | iteration
   | loop
   | conditional
   | instruction
   ;

block
   : 'BEGIN' statement* 'END'
   ;

iteration
   : 'ITERATE' number 'TIMES' statement
   ;

loop
   : 'WHILE' condition 'DO' statement
   ;

conditional
   : 'IF' condition 'THEN' statement ('ELSE' statement)?
   ;

instruction
   : 'MOVE'
   | 'TURNLEFT'
   | 'PICKBEEPER'
   | 'PUTBEEPER'
   | 'TURNOFF'
   | IDENTIFIER
   ;

condition
   : 'FRONT-IS-CLEAR'
   | 'FRONT-IS-BLOCKED'
   | 'LEFT-IS-CLEAR'
   | 'LEFT-IS-BLOCKED'
   | 'RIGHT-IS-CLEAR'
   | 'RIGHT-IS-BLOCKED'
   | 'BACK-IS-CLEAR'
   | 'BACK-IS-BLOCKED'
   | 'NEXT-TO-A-BEEPER'
   | 'NOT-NEXT-TO-A-BEEPER'
   | 'ANY-BEEPERS-IN-BEEPER-BAG'
   | 'NO-BEEPERS-IN-BEEPER-BAG'
   | 'FACING-NORTH'
   | 'NOT-FACING-NORTH'
   | 'FACING-SOUTH'
   | 'NOT-FACING-SOUTH'
   | 'FACING-EAST'
   | 'NOT-FACING-EAST'
   | 'FACING-WEST'
   | 'NOT-FACING-WEST'
   ;

IDENTIFIER
   : LETTER (LETTER | DIGIT)*
   ;

number
   : DIGIT+
   ;

LETTER
   : [A-Za-z]
   | '-'
   ;

DIGIT
   : [0-9]
   ;

WS
   : [ \t\r\n] -> skip
   ;

