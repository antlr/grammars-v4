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

// A grammar for the APL programming language (Iverson notation).


//


// References:


//  - https://en.wikipedia.org/wiki/APL_(programming_language)


//  - https://softwarepreservation.computerhistory.org/apl/book/APROGRAMMINGLANGUAGE.pdf


//


// APL has no operator precedence: every primitive function/operator binds


// at the same level and expressions are evaluated strictly right to left,


// e.g. "2+3*4" is "2+(3*4)". Whether a symbol is used monadically (one,


// right, argument) or dyadically (left and right arguments) is determined


// by whether a value appears to its left -- the grammar models this with


// the "expr : operand (function expr)? | function expr" rule below.


//


// A bare name can denote either a variable or a (user defined) function;


// resolving that requires a symbol table and is outside the scope of a


// context-free grammar, so this grammar prefers the dyadic reading


// ("A FN B") when a value already precedes the name, and otherwise falls


// back to the monadic reading ("FN B").

grammar apl;

program
   : line* EOF
   ;

line
   : functionDefinition
   | statementLine
   ;
   // -------------------------------------------------------------------
   
   // User defined functions: DEL header body DEL, as described in the
   
   // "function definition" section of the APL language.
   
   // -------------------------------------------------------------------
   
functionDefinition
   : DEL header NEWLINE statementLine* DEL NEWLINE?
   ;

header
   : NAME LARROW NAME NAME NAME localVars? // R<-A FN B  (dyadic, with result)
   | NAME NAME NAME localVars? // A FN B     (dyadic, no result)
   | NAME LARROW NAME NAME localVars? // R<-FN B    (monadic, with result)
   | NAME NAME localVars? // FN B       (monadic, no result)
   | NAME LARROW NAME localVars? // R<-FN      (niladic, with result)
   | NAME localVars? // FN         (niladic, no result)
   
   ;

localVars
   : (SEMI NAME)+
   ;

statementLine
   : label? (statement (DIAMOND statement)*)? NEWLINE
   ;

label
   : NAME COLON
   ;

statement
   : target LARROW statement
   | branch
   | expr
   ;

target
   : NAME index?
   ;

branch
   : RARROW expr?
   ;
   // -------------------------------------------------------------------
   
   // Expressions: right-to-left, flat (no precedence).
   
   // -------------------------------------------------------------------
   
expr
   : operand (function expr)?
   | function expr
   ;

operand
   : strand
   | NAME index?
   | ALPHA
   | OMEGA
   | sysVar
   | ZILDE
   | dfn
   ;
   // Strand (vector) notation: juxtaposed literal scalars, e.g. "1 2 3".
   
strand
   : scalar+
   ;

scalar
   : NUMBER
   | CHARLITERAL
   | LPAREN expr RPAREN
   ;

index
   : LBRACKET indexItem (SEMI indexItem)* RBRACKET
   ;

indexItem
   : expr?
   ;

sysVar
   : SYSNAME
   | QUAD
   | QUOTEQUAD
   ;
   // A "dfn": an anonymous function, e.g. {(+/omega)/rho omega}, using the
   
   // implicit arguments ALPHA (left) and OMEGA (right).
   
dfn
   : LBRACE (statement (DIAMOND statement)*)? RBRACE
   ;

function
   : primitive
   | NAME
   | dfn
   | LPAREN function RPAREN
   | function operator
   | function DOT function
   ;

operator
   : SLASH
   | SLASHBAR
   | BACKSLASH
   | BACKSLASHBAR
   | EACH
   | POWEROP
   | COMMUTE
   ;

primitive
   : PLUS
   | MINUS
   | TIMES
   | DIVIDE
   | POWER
   | CEILING
   | FLOOR
   | RESIDUE
   | COMMA
   | DOMINO
   | CIRCLE
   | LOG
   | REVERSE
   | REVERSEFIRST
   | GRADEUP
   | GRADEDOWN
   | EXECUTE
   | FORMAT
   | TRANSPOSE
   | FACTORIAL
   | MATCH
   | NOMATCH
   | TABLE
   | ROLL
   | MEMBERSHIP
   | FIND
   | SHAPE
   | TAKE
   | DROP
   | DECODE
   | ENCODE
   | LT
   | LE
   | EQ
   | GE
   | GT
   | NE
   | OR
   | AND
   | NOR
   | NAND
   | NOT
   | LEFTTACK
   | RIGHTTACK
   | ENCLOSE
   | DISCLOSE
   | IOTA
   | SLASH
   | SLASHBAR
   | BACKSLASH
   | BACKSLASHBAR
   | JOT
   ;
   // -------------------------------------------------------------------
   
   // Lexer
   
   // -------------------------------------------------------------------
   
DEL
   : '∇'
   ; // nabla: begin/end a function definition
   
LARROW
   : '←'
   ; // assignment
   
RARROW
   : '→'
   ; // branch
   
COMMENT
   : '⍝' ~ [\r\n]* -> skip
   ; // lamp
   
DIAMOND
   : '⋄'
   ; // statement separator
   
LPAREN
   : '('
   ;

RPAREN
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

SEMI
   : ';'
   ;

DOT
   : '.'
   ;

COLON
   : ':'
   ;
   // scalar dyadic/monadic functions
   
PLUS
   : '+'
   ;

MINUS
   : '−'
   | '-'
   ;

TIMES
   : '×'
   ;

DIVIDE
   : '÷'
   ;

POWER
   : '*'
   ;

CEILING
   : '⌈'
   ;

FLOOR
   : '⌊'
   ;

RESIDUE
   : '|'
   ;

COMMA
   : ','
   ;

DOMINO
   : '⌹'
   ;

CIRCLE
   : '○'
   ;

LOG
   : '⍟'
   ;

REVERSE
   : '⌽'
   ;

REVERSEFIRST
   : '⊖'
   ;

GRADEUP
   : '⍋'
   ;

GRADEDOWN
   : '⍒'
   ;

EXECUTE
   : '⍎'
   ;

FORMAT
   : '⍕'
   ;

TRANSPOSE
   : '⍉'
   ;

FACTORIAL
   : '!'
   ;

MATCH
   : '≡'
   ;

NOMATCH
   : '≢'
   ;

TABLE
   : '⍪'
   ;

ROLL
   : '?'
   ;

MEMBERSHIP
   : '∈'
   ;

FIND
   : '⍷'
   ;

SHAPE
   : '⍴'
   ;

TAKE
   : '↑'
   ;

DROP
   : '↓'
   ;

DECODE
   : '⊥'
   ;

ENCODE
   : '⊤'
   ;

LT
   : '<'
   ;

LE
   : '≤'
   ;

EQ
   : '='
   ;

GE
   : '≥'
   ;

GT
   : '>'
   ;

NE
   : '≠'
   ;

OR
   : '∨'
   ;

AND
   : '∧'
   ;

NOR
   : '⍱'
   ;

NAND
   : '⍲'
   ;

NOT
   : '∼'
   | '~'
   ;

LEFTTACK
   : '⊣'
   ;

RIGHTTACK
   : '⊢'
   ;

ENCLOSE
   : '⊂'
   ;

DISCLOSE
   : '⊃'
   ;

IOTA
   : '⍳'
   ;
   // operators (modify a function)
   
SLASH
   : '/'
   ;

SLASHBAR
   : '⌿'
   ;

BACKSLASH
   : '\\'
   ;

BACKSLASHBAR
   : '⍀'
   ;

EACH
   : '¨'
   ;

POWEROP
   : '⍣'
   ;

COMMUTE
   : '⍨'
   ;

JOT
   : '∘'
   ;
   // implicit dfn arguments, empty vector, system names
   
ALPHA
   : '⍺'
   ;

OMEGA
   : '⍵'
   ;

ZILDE
   : '⍬'
   ;

QUAD
   : '⎕'
   ;

QUOTEQUAD
   : '⍞'
   ;

SYSNAME
   : '⎕' [A-Za-z]+
   ;

fragment DIGITS
   : [0-9]+
   ;

fragment EXPONENT
   : [eE] '¯'? DIGITS
   ;

fragment REALPART
   : '¯'? DIGITS ('.' DIGITS)? EXPONENT?
   ;

NUMBER
   : REALPART ([Jj] REALPART)?
   ;

CHARLITERAL
   : '\'' ('\'\'' | ~ ['\r\n])* '\''
   ;

fragment NAMESTART
   : [A-Za-z_]
   | '∆'
   | '⍙'
   ;

fragment NAMECHAR
   : [A-Za-z0-9_]
   | '∆'
   | '⍙'
   ;

NAME
   : NAMESTART NAMECHAR*
   ;

NEWLINE
   : '\r'? '\n'
   ;

WS
   : [ \t]+ -> skip
   ;

