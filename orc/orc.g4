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
grammar orc;

program
   : expression EOF
   ;

expression
   : literal # literalExpr
   | variable # variableExpr
   | 'stop' # stopExpr
   | '[' (expression (',' expression)*)? ']' # listExpr
   | '{' '.' key '=' expression (',' key '=' expression)* '.' '}' # recordExpr
   | '(' expression ')' # parenExpr
   | '(' expression (',' expression)+ ')' # tupleExpr
   | 'lambda' typeParameters? parameters returnType? '=' expression # lambdaExpr
   | declaration expression # withDeclarationExpr
   | 'if' expression 'then' expression 'else' expression # conditionalExpr
   | expression '.' key # dotAccessExpr
   | expression typeArguments? arguments # callExpr
   | expression '?' # derefExpr
   | '-' expression # negateExpr
   | expression op = ('*' | '/') expression # mulDivExpr
   | expression op = ('+' | '-') expression # addSubExpr
   | expression ':' expression # consExpr
   | expression op = ('<:' | ':>' | '=') expression # compareExpr
   | expression ':=' expression # assignExpr
   | expression '&&' expression # andExpr
   | expression '||' expression # orExpr
   | expression '::' type # withTypeExpr
   | expression ':!:' type # typeOverrideExpr
   | expression '>' pattern? '>' expression # sequenceExpr
   | expression '<' pattern? '<' expression # pruneExpr
   | expression '|' expression # parallelExpr
   | expression ';' expression # otherwiseExpr
   ;

declaration
   : declareVal
   | declareDefinition
   | declareDefclass
   | declareSignature
   | declareSite
   | declareClass
   | declareInclude
   | declareType
   ;

declareVal
   : 'val' pattern '=' expression
   ;

declareDefinition
   : 'def' variable typeParameters? parameters returnType? guard? '=' expression
   ;

declareDefclass
   : 'def' 'class' variable typeParameters? parameters returnType? guard? '=' expression
   ;

guard
   : 'if' '(' expression ')'
   ;

declareSignature
   : 'def' variable typeParameters? argumentTypes returnType
   ;

declareSite
   : 'import' 'site' variable '=' className
   ;

declareClass
   : 'import' 'class' variable '=' className
   ;

declareInclude
   : 'include' fileName
   ;

declareType
   : declareTypeAlias
   | declareTypeImport
   | declareDatatype
   ;

declareTypeAlias
   : 'type' typeVariable typeParameters? '=' type
   ;

declareTypeImport
   : 'import' 'type' typeVariable '=' className
   ;

declareDatatype
   : 'type' typeVariable typeParameters? '=' dataConstructor ('|' dataConstructor)*
   ;

dataConstructor
   : variable '(' (slot (',' slot)*)? ')'
   ;

slot
   : type
   | '_'
   ;

pattern
   : '_' # wildcardPattern
   | literal # literalPattern
   | variable '(' (pattern (',' pattern)*)? ')' # callPattern
   | variable # variablePattern
   | '(' pattern ')' # parenPattern
   | '(' pattern (',' pattern)+ ')' # tuplePattern
   | '[' (pattern (',' pattern)*)? ']' # listPattern
   | '{' '.' key '=' pattern (',' key '=' pattern)* '.' '}' # recordPattern
   | pattern ':' pattern # consPattern
   | pattern 'as' variable # asPattern
   | pattern '::' type # typedPattern
   ;

type
   : typeVariable # typeVariableType
   | '(' type ')' # parenType
   | '(' type (',' type)+ ')' # tupleType
   | '{' '.' key '=' type (',' key '=' type)* '.' '}' # recordType
   | 'lambda' typeParameters? argumentTypes returnType # functionType
   | type typeArguments # typeApplicationType
   ;

literal
   : SIGNALLITERAL
   | BOOLEANLITERAL
   | NUMBERLITERAL
   | CHARACTERSTRINGLITERAL
   ;

variable
   : IDENTIFIER
   ;

typeVariable
   : IDENTIFIER
   ;

key
   : IDENTIFIER
   ;

className
   : CHARACTERSTRINGLITERAL
   ;

fileName
   : CHARACTERSTRINGLITERAL
   ;

parameters
   : '(' (pattern (',' pattern)*)? ')'
   ;

typeParameters
   : '[' typeVariable (',' typeVariable)* ']'
   ;

argumentTypes
   : '(' (type (',' type)*)? ')'
   ;

returnType
   : '::' type
   ;

arguments
   : '(' (expression (',' expression)*)? ')'
   ;

typeArguments
   : '[' type (',' type)* ']'
   ;

BOOLEANLITERAL
   : 'true'
   | 'false'
   ;

SIGNALLITERAL
   : 'signal'
   ;

NUMBERLITERAL
   : INTEGERLITERAL ('.' INTEGERLITERAL)? EXPONENTPART?
   ;

fragment EXPONENTPART
   : [eE] [+-]? INTEGERLITERAL
   ;

fragment INTEGERLITERAL
   : DECIMALDIGIT+
   ;

fragment DECIMALDIGIT
   : [0-9]
   ;

CHARACTERSTRINGLITERAL
   : '"' (~ ["\r\n])* '"'
   ;

IDENTIFIER
   : [a-zA-Z_] [a-zA-Z_0-9]*
   ;

LINE_COMMENT
   : '--' ~ [\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '{-' .*? '-}' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

