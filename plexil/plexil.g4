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

plan
   : node
   ;

node
   : (nodeId ':')? '{' nodeAttributes? nodeBody? '}'
   ;

nodeId
   : STRING
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
   : 'priority' ':' INTEGER ';'
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

   inClause? inOut?
   ;

inClause
   : 'In' ':' '{' (declaredVariable ';')* '}'
   ;

inOut
   : 'InOut' ':' '{' (declaredVariable ';')* '}'
   ;

declaredVariable
   : integerVariable
   | realVariable
   | booleanVariable
   | stringVariable
   | pointerVariable
   ;

integerVariable
   : STRING
   ;

realVariable
   : STRING
   ;

booleanVariable
   : STRING
   ;

stringVariable
   : STRING
   ;

pointerVariable
   : STRING
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


// this is missing in the PDF from NASA
commandName: STRING;

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
   : orExpr
   | andExpr
   | notExpr
   | gt
   | ge
   | lt
   | le
   | booleanVariable
   | booleanValue
   | lookup
   | '(' booleanExpression ')'
   | booleanExpression '==' booleanExpression
   | booleanExpression '!=' booleanExpression
   | numericExpression '==' numericExpression
   | numericExpression '!=' numericExpression
   | nodeState '==' nodeState
   | nodeState '!=' nodeState
   | nodeOutcome '==' nodeOutcome
   | nodeOutcome '!=' nodeOutcome
   ;

orExpr
   : 'OR' (booleanExpression)+
   ;

andExpr
   : 'AND' (booleanExpression)+
   ;

notExpr
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
   : integerVariable
   | realVariable
   | integerValue
   | realValue
   | lookup
   | nodeTimepointValue
   | PLUSINFINITY
   | MINUSINFINITY
   | '(' numericExpression ')'
   | numericExpression '*' numericExpression
   | numericExpression '/' numericExpression
   | numericExpression '+' numericExpression
   | numericExpression '-' numericExpression
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
   : realValue
   | declaredVariable
   ;

lookupNow
   : 'lookupNow' ':' stateName ';'
   ;

parameter
   : STRING
   ;

// NONNEGATIVEINTEGER is a fragment (rather than its own token) so that
// a plain unsigned number always lexes as INTEGER; if it were its own
// token, a bare "0" would tie in length with INTEGER's match of "0"
// and, by rule-order tie-breaking, always lex as NONNEGATIVEINTEGER
// instead, which integerValue (and other rules expecting INTEGER)
// could then never match.
fragment NONNEGATIVEINTEGER
   : [0-9]+
   ;

INTEGER
   : ('+' | '-')? NONNEGATIVEINTEGER
   ;

DOUBLE
   : [0-9]+ '.' [0-9]+
   ;

// BOOLEAN, TIMEPOINT and PLUSINFINITY must be declared before STRING:
// ANTLR breaks a same-length lexer tie in favor of whichever rule is
// declared first, so if STRING ([a-zA-Z]+) came first, plain-letter
// words like "true", "START" or "INF" would always lex as STRING
// instead of the keyword they are meant to be.
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

STRING
   : [a-zA-Z]+
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

