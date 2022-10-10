/*
BSD License

Copyright (c) 2022, Tom Everett
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
grammar structuredtext;

expression_
   : xor_expression ('OR' xor_expression)* EOF
   ;

xor_expression
   : and_expression ('XOR' and_expression)*
   ;

and_expression
   : comparison (('&' | 'AND') comparison)*
   ;

comparison
   : equexpression (('=' | '<>') equexpression)*
   ;

equexpression
   : add_expression (COMPARISONOPERATOR add_expression)*
   ;

add_expression
   : term (ADDOPERATOR term)*
   ;

term
   : powerexpression (MULTIPLYOPERATOR PowerExpowerexpressionression)*
   ;

powerexpression
   : unaryexpression ('**' unaryexpression)*
   ;

unaryexpression
   : UNARYOPERATOR? primaryexpression
   ;

primaryexpression
   : constant
   | variablename
   | '(' expression_ ')'
   | functionname '(' functioninputs? ')'
   ;

functioninputs
   : functioninput (',' functioninput)*
   ;

functioninput
   : (variablename ':=')? expression_
   ;

statementlist
   : statement ';' (statement ';')*
   ;

statement
   : NIL
   | assignmentstatement
   | subprogramcontrolstatement
   | selectionstatement
   | iterationstatement
   ;

assignmentstatement
   : variablename ':=' expression_
   ;

subprogramcontrolstatement
   : invocation
   | 'RETURN'
   ;

invocation
   : name '(' (inputassignment (',' inputassignment)*)? ')'
   ;

inputassignment
   : variablename ':=' expression_
   ;

selectionstatement
   : ifstatement
   | casestatement
   ;

ifstatement
   : 'IF' expression_ 'THEN' statementlist ('ELSIF' expression_ 'THEN' statementlist)* ('ELSE' statementlist)? 'END_IF'
   ;

casestatement
   : 'CASE' expression_ 'OF' CaseElement caseelement* ('ELSE' statementlist)? 'END_CASE'
   ;

caseelement
   : caselist ':' statementlist
   ;

caselist
   : caselistelement (',' caselistelement)*
   ;

caselistelement
   : subrange
   | signedinteger
   ;

iterationstatement
   : forstatement
   | whilestatement
   | repeatstatement
   | exitstatement
   ;

forstatement
   : 'FOR' controlvariable ':=' ForList 'DO' statementlist 'END_FOR'
   ;

controlvariable
   : identifier
   ;

forlist
   : expression_ 'TO' expression_ ('BY' expression_)?
   ;

whilestatement
   : 'WHILE' expression_ 'DO' statementlist 'END_WHILE'
   ;

repeatstatement
   : 'REPEAT' statementlist 'UNTIL' expression_ 'END_REPEAT'
   ;

exitstatement
   : 'EXIT'
   ;

identifier
   : IDENTIFIER
   ;

functionname
   : IDENTIFIER
   ;

variablename
   : IDENTIFIER
   ;

name
   : IDENTIFIER
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

MULTIPLYOPERATOR
   : '*'
   | '/'
   | 'MOD'
   ;

UNARYOPERATOR
   : '-'
   | 'NOT'
   ;

ADDOPERATOR
   : '+'
   | '-'
   ;

COMPARISONOPERATOR
   : '<'
   | '>'
   | '<='
   | '>='
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

