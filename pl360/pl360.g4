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

// Transcribed from the "PL360 Syntactic Grammar" (Appendix I of the Stanford
// PL360 Reference Manual): https://www.slac.stanford.edu/spires/explain/manuals/PL360TXT.HTML#:I
//
// That appendix is a "precedence grammar" written for a 1970s single-pass,
// operator-precedence compiler: most of its productions are long chains of
// tiny one-token "builder" rules (e.g. FUNC-DC1 through FUNC-DC7 together
// just spell out `FUNCTION id ( number , number )`, one symbol at a time).
// Those chains are collapsed here into ordinary EBNF (`?`, `*`, `+`, and
// comma/paren-separated lists) rather than transcribed rule-for-rule, since a
// literal transcription both reads poorly top-down and is riddled with
// indirect left recursion that ANTLR4 cannot auto-resolve (it only rewrites
// *direct* self-recursion within a single rule). A handful of rarely-used,
// purely decorative constructs from the reference (statement sequences
// threaded in front of the IF/WHILE keywords themselves, used by the
// original precedence parser to admit stray comment-like statements between
// tokens) are intentionally dropped, since BEGIN...END blocks already give
// every statement position a way to hold more than one statement.
grammar pl360;

program
   : (globHd ';')? statement '.' EOF
   ;

globHd
   : 'GLOBAL' 'PROCEDURE' identifier '(' kReg ')' ('BASE' kReg)?
   ;

// A PL360 "statement" (the reference's STATEMENT / STATEMENT+ / STATEMENT*,
// which are all just aliases of the same thing once the decorative
// statement-threading noted above is dropped).
statement
   : gotoStatement
   | 'IF' condThen statement
   | 'IF' condThen truePart statement
   | repeatBlock 'UNTIL' primCond
   | simpleStatement
   | 'WHILE' condDo statement
   | 'FOR' assStep limit 'DO' statement
   ;

gotoStatement
   : 'GOTO' identifier
   ;

truePart
   : (gotoStatement | simpleStatement) 'ELSE'
   ;

// REPEAT really does carry a statement list of its own (the loop body),
// unlike IF/WHILE above.
repeatBlock
   : 'REPEAT' (labelDef | statement ';')*
   ;

simpleStatement
   : block
   | caseStatement
   | cellAssignment
   | functionCall
   | procedureCall
   | kRegAss
   | identifier
   | 'NULL'
   ;

block
   : blockBody 'END'
   ;

blockBody
   : blockHead (labelDef | statement ';')*
   ;

blockHead
   : 'BEGIN' (decl ';')*
   ;

caseStatement
   : caseSeq 'END'
   ;

caseSeq
   : caseHead (labelDef | statement ';')*
   ;

caseHead
   : 'CASE' kReg 'OF' 'BEGIN' (decl ';')*
   ;

labelDef
   : identifier ':'
   ;

// Declarations, allowed at the head of a block/case/procedure body.
decl
   : dsegType 'BASE' kReg
   | equateDeclaration
   | functionDecl
   | procedureDecl
   | procSynDecl
   | synDecl
   | tDecl
   | 'CLOSE' 'BASE'
   ;

dsegType
   : 'COMMON'
   | 'COMMON' 'DATA' identifier
   | 'DUMMY'
   | 'EXTERNAL' 'DATA' identifier
   | 'GLOBAL' 'DATA' identifier
   | 'SEGMENT'
   ;

functionDecl
   : 'FUNCTION' functionDeclItem (',' functionDeclItem)*
   ;

functionDeclItem
   : identifier '(' tNumber ',' tNumber ')'
   ;

procedureDecl
   : procedureHeader statement
   ;

procedureHeader
   : ('EXTERNAL' | 'GLOBAL' | 'SEGMENT' | 'COMMON')? 'PROCEDURE' identifier '(' kReg ')' ('BASE' kReg)? ';'
   ;

procSynDecl
   : 'PROCEDURE' identifier 'SYN' identifier
   ;

equateDeclaration
   : ('EQUATE' | 'SHORT' 'EQUATE') equSynItem (',' equSynItem)*
   ;

equSynItem
   : identifier 'SYN' equSynValue (equSynOp equSynOperand)*
   ;

equSynValue
   : kReg
   | STRING
   | tNumber
   | unaryNum
   | tCell
   ;

equSynOp
   : arithOp
   | logOp
   | shiftOp
   ;

equSynOperand
   : kReg
   | STRING
   | tNumber
   | tCell
   ;

synDecl
   : synDeclHead (',' identifier 'SYN' synDeclValue)*
   ;

synDeclHead
   : (siType 'REGISTER' | tType) identifier 'SYN' synDeclValue
   ;

synDeclValue
   : kReg
   | tCell
   | tNumber
   ;

tDecl
   : tDeclItem (',' tDeclItem)*
   ;

tDeclItem
   : tType identifier? ('=' fill)?
   | identifier ('=' fill)?
   ;

// A replicated/initializer "fill" value; the parenthesized form can itself
// hold a comma-separated, optionally count-prefixed list of fills (e.g.
// `111(" ")`, a 111-fold repeated space string, from the reference manual's
// sample program).
fill
   : adrOp (identifier | tCell)
   | (tNumber)? '(' fill (',' fill)* ')'
   | STRING
   | tNumber
   ;

// Assignment to a register.
kRegAss
   : kReg (':=' kRegAssRhs)? (kRegAssOp kRegAssOperand)*
   ;

kRegAssRhs
   : adrOp (identifier | tCell)
   | kReg
   | STRING
   | tCell
   | tNumber
   | unaryCell
   | unaryNum
   | unaryReg
   ;

kRegAssOp
   : arithOp
   | logOp
   | shiftOp
   ;

kRegAssOperand
   : kReg
   | STRING
   | tCell
   | tNumber
   ;

// Assignment to a memory cell.
cellAssignment
   : tCell ':=' cellAssignmentRhs (logOp cellAssignmentOperand)*
   ;

cellAssignmentRhs
   : kReg
   | STRING
   | tCell
   | tNumber
   ;

cellAssignmentOperand
   : STRING
   | tCell
   | tNumber
   ;

functionCall
   : identifier '(' functionArg (',' functionArg)* ')'
   ;

functionArg
   : functionCall
   | kReg
   | STRING
   | tCell
   | tNumber
   ;

procedureCall
   : identifier '(' kReg ')'
   ;

assStep
   : kRegAss 'STEP' tNumber
   ;

limit
   : 'UNTIL' (kReg | tCell | tNumber)
   ;

// Conditions.
condThen
   : compoundCondition 'THEN'
   ;

condDo
   : compoundCondition 'DO'
   ;

primCond
   : compoundCondition
   ;

compoundCondition
   : (statement ';')* conditionPrimary (('AND' | 'OR') (statement ';')* conditionPrimary)*
   ;

conditionPrimary
   : kReg brindxOp kReg
   | kReg relOp (kReg | STRING | tCell | tNumber)
   | tCell relOp (STRING | tCell | tNumber)
   | tCell
   | tNumber
   | unaryReg
   | relOp
   | '~' (tCell | tNumber | unaryReg)
   | '(' (statement ';')* compoundCondition ')'
   ;

// Operators.
adrOp
   : '@@'
   | '@'
   ;

arithOp
   : '--'
   | '++'
   | '-'
   | '+'
   | '*'
   | '/'
   | '=:'
   ;

brindxOp
   : 'GT'
   | 'LE'
   ;

logOp
   : 'AND'
   | 'OR'
   | 'XOR'
   ;

relOp
   : '='
   | '~='
   | '>='
   | '<='
   | '>'
   | '<'
   ;

shiftOp
   : 'SHLA'
   | 'SHLL'
   | 'SHRA'
   | 'SHRL'
   ;

unaryOp
   : 'ABS'
   | 'DEC'
   | 'HALF'
   | 'NEG'
   ;

unaryCell
   : unaryOp+ tCell
   ;

unaryNum
   : unaryOp+ tNumber
   ;

unaryReg
   : unaryOp+ kReg
   ;

// Types.
siType
   : 'BYTE'
   | 'CHARACTER'
   | 'INTEGER'
   | 'LOGICAL'
   | 'LONG' 'REAL'
   | 'REAL'
   | 'SHORT' 'INTEGER'
   ;

tType
   : siType
   | 'ARRAY' tNumber siType
   ;

// Cells, registers and numbers.
kReg
   : identifier
   ;

tCell
   : identifier
   | identifier '(' cellIndex ')'
   ;

cellIndex
   : kReg (arithOp (kReg | tNumber))?
   | (STRING | tNumber) (arithOp (STRING | tNumber))*
   ;

tNumber
   : identifier
   | '_' NUMBER
   | NUMBER
   ;

identifier
   : IDENTIFIER
   ;

// Lexer.

// Reserved words and operator-like keywords (kept ahead of IDENTIFIER below
// only by file order of first use above -- ANTLR resolves equal-length
// lexical ties by the literal's first point of definition in the grammar
// file, and every quoted keyword above textually precedes IDENTIFIER).

STRING
   : '"' (~'"' | '""')* '"'
   ;

NUMBER
   : (HEXVAL | DIGITS ('.' DIGITS?)?) ('\'' '_'? DIGITS)? [LRSX]?
   ;

fragment HEXVAL
   : '#' HEXDIGIT+
   ;

fragment HEXDIGIT
   : [0-9A-F]
   ;

fragment DIGITS
   : [0-9]+
   ;

IDENTIFIER
   : [A-Z] [A-Z0-9]*
   ;

COMMENT1
   : 'COMMENT' .*? ';' -> skip
   ;

COMMENT2
   : '|' ~'|'* '|' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;
