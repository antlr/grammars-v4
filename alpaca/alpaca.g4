/*
BSD License

Copyright (c) 2019, Tom Everett
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
'AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar alpaca;

prog
   : defns ('.' | 'begin') EOF
   ;

defns
   : defn (';' defn)*
   ;

defn
   : stateDefn
   | classDefn
   | nbhdDefn
   ;

stateDefn
   : 'state' stateID QUOTEDCHAR? membershipDecl* rules?
   ;

classDefn
   : 'class' classID membershipDecl* rules?
   ;

nbhdDefn
   : 'neighbourhood' nbhdID neigbourhood
   ;

classID
   : identifier
   ;

stateID
   : identifier
   ;

nbhdID
   : identifier
   ;

membershipDecl
   : classRef
   ;

classRef
   : 'is' classID
   ;

rules
   : rule_ (',' rule_)*
   ;

rule_
   : 'to' stateRef ('when' expression)?
   ;

stateRef
   : stateID
   | arrowchain
   | 'me'
   ;

expression
   : term (('and' | 'or' | 'xor') term)*
   ;

term
   : adjacencyPred
   | '(' expression ')'
   | 'not' term
   | boolPrimitive
   | relationalPred
   ;

relationalPred
   : stateRef ('='? stateRef | classRef)
   ;

adjacencyPred
   : naturalnumber ('in' (neigbourhood | nbhdID))? (stateRef | classRef)
   ;

boolPrimitive
   : 'true'
   | 'false'
   | 'guess'
   ;

neigbourhood
   : '(' arrowchain* ')'
   ;

identifier
   : ALPHA (ALPHA | DIGIT)*
   ;

naturalnumber
   : DIGIT+
   ;

arrowchain
   : ARROW+
   ;

QUOTEDCHAR
   : '"' ~ '"'* '"'
   ;

QUOTE
   : '"'
   ;

ALPHA
   : [a-zA-Z]
   ;

DIGIT
   : [0-9]
   ;

ARROW
   : [^v<>]
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \t\r\n] -> skip
   ;

