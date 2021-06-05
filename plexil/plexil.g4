/*
BSD License

Copyright (c) 2020, Tom Everett
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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar plexil;

plexilPlan
   : node
   ;

node
   : (nodeId ':')? '{' nodeAttributes? nodeBody? '}'
   ;

nodeId
   : String
   ;

nodeBody
   : nodeList
   | command
   | assignment
   ;

nodeList
   : 'nodeList' ':' '{' node* '}'
   ;

nodeAttributes
   : /* here the xml schema specifies any order */

   startCondition? repeatUntilCondition? preCondition? postCondition? invariantCondition? endCondition? priority? interface? variableDeclarations?
   ;

priority
   : 'priority' ':' NONNEGATIVEINTEGER ';'
   ;

startCondition
   : 'startCondition' ':' booleanExpression ';'
   ;

repeatUntilCondition
   : 'repeatUntilCondition' ':' booleanExpression ';'
   ;

preCondition
   : 'preCondition' ':' booleanExpression ';'
   ;

postCondition
   : 'postCondition' ':' booleanExpression ';'
   ;

invariantCondition
   : 'invariantCondition' ':' booleanExpression ';'
   ;

endCondition
   : 'endCondition' ':' booleanExpression ';'
   ;

interface
   : /* here the xml schema specifies any order */

   in? inOut?
   ;

in
   : 'In' ':' '{' (declaredVariable ';')* '}'
   ;

inOut
   : 'InOut' ':' (declaredVariable ';')* '}'
   ;

declaredVariable
   : integerVariable
   | realVariable
   | booleanVariable
   | stringVariable
   | pointerVariable
   ;

integerVariable
   : NCName
   ;

realVariable
   : NCName
   ;

booleanVariable
   : NCName
   ;

stringVariable
   : NCName
   ;

pointerVariable
   : NCName
   ;

variableDeclarations
   : (variableDeclaration)*
   ;

variableDeclaration
   : ('Boolean' ':' '{' (booleanVariable ('=' booleanValue)? ';')* '}')
   | ('Integer' ':' '{' (integerVariable ('=' integerValue)? ';')* '}')
   | ('Real' ':' '{' (realVariable ('=' realValue)? ';')* '}')
   | ('String' ':' '{' (stringVariable ('=' stringValue)? ';')* '}')
   | ('Pointer' ':' '{' (pointerVariable ('=' pointsTo)? ';')* '}')
   ;

booleanValue
   : BOOLEAN
   | 'UNKNOWN'
   ;

integerValue
   : INTEGER
   | 'UNKNOWN'
   ;

realValue
   : DOUBLE
   | 'UNKNOWN'
   ;

stringValue
   : STRING
   | 'UNKNOWN'
   ;

pointsTo
   : externalStructName initialValue?
   ;

externalStructName
   : STRING
   ;

initialValue
   : INTEGER
   ;

command
   : 'command' ':' (declaredVariable '=')? commandName '(' arguments? ')' ';'
   ;

arguments
   : ((integerValue | realValue | booleanValue | declaredVariable) ';'?)*
   ;

assignment
   : 'assignment' ':' (booleanassignment | numericassignment)
   ;

booleanassignment
   : booleanVariable '=' booleanRHS ';'
   ;

numericassignment
   : (integerVariable | realVariable) '=' numericRHS ';'
   ;

booleanRHS
   : booleanExpression
   ;

numericRHS
   : numericExpression
   ;

booleanExpression
   : or
   | and
   | not
   | gt
   | ge
   | lt
   | le
   | eq
 //  | ne
   | booleanVariable
   | booleanValue
   | lookup
   | '(' booleanExpression ')'
   ;

or
   : 'OR' (booleanExpression)+
   ;

and
   : 'AND' (booleanExpression)+
   ;

not
   : 'NOT' booleanExpression
   ;

gt
   : numericExpression '>' numericExpression
   ;

ge
   : numericExpression '>=' numericExpression
   ;

lt
   : numericExpression '<' numericExpression
   ;

le
   : numericExpression '<=' numericExpression
   ;

eq
   : eqBoolean
   | eqNumeric
   | eqInternal
   ;

eqBoolean
   : booleanExpression '==' booleanExpression
   ;

eqNumeric
   : numericExpression '==' numericExpression
   ;

eqInternal
   : nodeState '==' nodeState
   | nodeOutcome '==' nodeOutcome
   ;

neExpression
   : neBoolean
   | neNumeric
   | neInternal
   ;

neBoolean
   : booleanExpression '!=' booleanExpression
   ;

neNumeric
   : numericExpression '!=' numericExpression
   ;

neInternal
   : nodeState '!=' nodeState
   | nodeOutcome '!=' nodeOutcome
   ;

nodeState
   : nodeStateVariable
   | nodeStateValue
   ;

nodeStateVariable
   : nodeId '.state'
   ;

nodeStateValue
   : 'WAITING'
   | 'EXECUTING'
   | 'FINISHING'
   | 'FAILING'
   | 'FINISHED'
   ;

nodeOutcome
   : nodeOutcomeVariable
   | nodeOutcomeValue
   ;

nodeOutcomeVariable
   : nodeId '.outcome'
   ;

nodeOutcomeValue
   : 'SUCCESS'
   | 'FAILURE'
   | 'SKIPPED'
   | 'INFINITE_LOOP'
   ;

nodeTimepointValue
   : nodeId '.' nodeStateValue '.' TIMEPOINT
   ;

numericExpression
   : add
   | sub
   | mul
   | div
   | integerVariable
   | realVariable
   | integerValue
   | realValue
   | lookup
   | nodeTimepointValue
   | PLUSINFINITY
   | MINUSINFINITY
   | '(' numericExpression ')'
   ;

add
   : numericExpression '+' numericExpression
   ;

sub
   : numericExpression '-' numericExpression
   ;

mul
   : numericExpression '*' numericExpression
   ;

div
   : numericExpression '/' numericExpression
   ;

lookup
   : lookupWithfrequency
   | lookupOnChange
   | lookupNow
   ;

lookupWithfrequency
   : 'lookupWithfrequency' ':' stateName ',' parameter* ',' frequency ';'
   ;

stateName
   : STRING
   ;

frequency
   : realValue
   | declaredVariable
   ;

lookupOnChange
   : 'lookupOnChange' ':' stateName ',' parameter* (',' tolerance)? ';'
   ;

tolerance
   : Value
   | declaredVariable
   ;

lookupNow
   : 'lookupNow' ':' stateName ';'
   ;

parameter
   : STRING
   ;

STRING
   : [a-zA-Z]+
   ;

NONNEGATIVEINTEGER
   : [0-9]+
   ;

INTEGER
   : ('+' | '-')? NONNEGATIVEINTEGER
   ;

DOUBLE
   : [0-9]+ '.' [0-9]+
   ;

BOOLEAN
   : 'true'
   | 'false'
   ;

TIMEPOINT
   : 'START'
   | 'END'
   ;

PLUSINFINITY
   : 'INF'
   ;

MINUSINFINITY
   : '-INF'
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

