/*
 BSD License

 Copyright (c) 2026, Tom Everett All rights reserved.

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
grammar bliss;

module_
   : 'MODULE' name '(' parameters? ')' '=' e 'ELUDOM'
   ;

parameters
   : parameter
   | parameter ',' parameters
   ;

parameter
   : name
   | name '=' e
   ;

block
   : 'BEGIN' blockbody 'END'
   | (blockbody)
   ;

compoundexpression
   : 'BEGIN' expressionsequence 'END'
   | '(' expressionsequence ')'
   ;

blockbody
   : declarations (';' expressionsequence | ';')?
   ;

declarations
   : declaration
   | declaration ';' declarations
   ;

expressionsequence
   : e (';' e)*
   ;

literal
   : number
   | quotedstring
   ;

number
   : DECIMAL
   | OCTAL
   | FLOATING
   ;

quotedstring
   : LEFTADJUSTEDSTRING
   | RIGHTADJUSTEDSTRING
   ;

e
   : simpleexpression
   | controlexpression
   ;

simpleexpression
   : pl1 '=' e
   | pl1
   ;

pl1
   : pl0
   | pl1 'XOR' pl0
   | pl1 'EQV' pl0
   ;

pl0
   : p9
   | pl0 'OR' p9
   ;

p9
   : p8
   | p9 'AND' p8
   ;

p8
   : p7
   | 'NOT' p7
   ;

p7
   : p6
   | p6 relation p6
   ;

p6
   : p5
   | '-' p5
   | p6 '+' p5
   | p6 '-' p5
   ;

p5
   : p4
   | p5 '*' p4
   | p5 '/' p4
   | p5 'MOD' p4
   ;

p4
   : p3
   | p4 't' p3
   ;

p3
   : p2
   | '.' p3
   | '@' p3
   | '\\' p3
   ;

p2
   : p1
   | p1 '<' pointerparameters '>'
   ;

p1
   : literal
   | name
   | name '[' elist ']'
   | p1 '(' elist ')'
   | p1 '(' ')'
   | block
   | compoundexpression
   ;

relation
   : 'EQL'
   | 'NEQ'
   | 'LSS'
   | 'LEQ'
   | 'GTR'
   | 'GEQ'
   ;

pointerparameters
   : position? ',' size modification?
   ;

modification
   : ',' index
   | ',' index ',' indirect
   ;

position
   : e
   ;

size
   : e
   ;

index
   : e
   ;

indirect
   : e
   ;

controlexpression
   : conditionalexpression
   | loopexpression
   | choiceexpression
   | escapeexpression
   | coroutineexpression
   ;

conditionalexpression
   : 'IF' e 'THEN' e
   | 'IF' e 'THEN' e 'ELSE' e
   ;

loopexpression
   : 'WHILE' e 'DO' e
   | 'UNTIL' e 'DO' e
   | 'DO' e 'WHILE' e
   | 'DO' e 'UNTIL' e
   | 'INCR' name 'FROM' e 'TO' e 'BY' e 'DO' e
   | 'DECR' name 'FROM' e 'TO' e 'BY' e 'DO' e
   ;

escapeexpression
   : environment level escapevalue
   | 'RETURN' escapevalue
   ;

environment
   : 'EXIT'
   | 'EXITBLOCK'
   | 'EXITCOMPOUND'
   | 'EXITLOOP'
   | 'EXITCONDITIONAL' 'EXITCASE'
   | 'EXITSET'
   | 'EXITSELECT'
   ;

level
   : ('[' e ']')?
   ;

escapevalue
   : e?
   ;

choiceexpression
   : 'CASE' elist 'OF' 'SET' expressionset 'TES'
   | 'SELECT' elist 'OF' 'NSET' nexpressionset 'TESN'
   ;

elist
   : e
   | elist ',' e
   ;

expressionset
   : e
   | ';' expressionset
   | e ';' expressionset
   ;

nexpressionset
   : ne
   | ne ';' nexpressionset
   ;

ne
   : e ':' e
   ;

coroutineexpression
   : 'CREATE' e '(' elist ')' 'AT' e 'LENGTH' e 'THEN' e
   | 'EXCHJ' '(' e ',' e ')'
   ;

declaration
   : functiondeclaration
   | structuredeclaration
   | binddeclaration
   | macrodeclaration
   | allocationdeclaration
   | mapdeclaration
   | 'MACHOP' mlist
   | 'ALLMACHOP'
   | 'SWITCHES' switchlist
   ;

allocationdeclaration
   : allocatetype msidlist
   ;

allocatetype
   : 'GLOBAL'
   | 'REGISTER'
   | 'OWN'
   | 'LOCAL'
   | 'EXTERNAL'
   ;

msidlist
   : msidelement
   | msidelement ',' msidlist
   ;

msidelement
   : structure sizedchunks
   ;

structure
   : name?
   ;

sizedchunks
   : sizedchunk
   | sizedchunk ':' sizedchunks
   ;

sizedchunk
   : idchunk
   | idchunk '[' elist ']'
   ;

idchunk
   : name
   | name ':' idchunk
   ;

mapdeclaration
   : 'MAP' msidlist
   ;

binddeclaration
   : 'BIND' equivalencelist
   ;

equivalencelist
   : equivalence
   | equivalence ',' equivalencelist
   ;

equivalence
   : msidelement '=' e
   ;

structuredeclaration
   : 'STRUCTURE' name structureformallist '=' structuresize
   ;

structureformallist
   : ('[' namelist ']')?
   ;

structuresize
   : ('[' e ']')?
   ;

functiondeclaration
   : 'FUNCTION' name '(' namelist ')' '=' e
   | 'FUNCTION' name '=' e
   | 'ROUTINE' name '(' namelist ')' '=' e
   | 'ROUTINE' name '=' e
   | 'GLOBAL' 'ROUTINE' name '(' namelist ')' '=' e
   | 'GLOBAL' 'ROUTINE' name '=' e
   | 'EXTERNAL' 'ROUTINE' namelist
   | 'FORWARD' 'ROUTINE' namelist
   ;

namelist
   : name
   | namelist ',' name
   ;

macrodeclaration
   : 'MACRO' definitionlist
   ;

definitionlist
   : definition
   | definitionlist ',' definition
   ;

definition
   : name '(' namelist ')' '=' e
   | name '=' e
   ;

mlist
   : name '=' e
   | mlist ',' name '=' e
   ;

switchlist
   : switch
   | switch ',' switchlist
   ;

switch
   : name
   | name '=' e
   ;

name
   : NAME
   ;

COMMENT1
   : '!' ~ [\r\n]* -> skip
   ;

COMMENT2
   : '%' ~ '%'* '%' -> skip
   ;

DECIMAL
   : DIGIT+
   ;

OCTAL
   : '#' [0-7]+
   ;

FLOATING
   : DECIMAL '.' DECIMAL EXPONENT?
   ;

fragment EXPONENT
   : 'E' DECIMAL
   | 'E' '+' DECIMAL
   | 'E' '-' DECIMAL
   ;

LEFTADJUSTEDSTRING
   : '\'' ~ ['\r\n]* '\''
   ;

RIGHTADJUSTEDSTRING
   : '"' ~ ["\r\n]* '"'
   ;

NAME
   : [A-Za-z] [A-Za-z0-9_$]*
   ;

fragment DIGIT
   : [0-9]
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

