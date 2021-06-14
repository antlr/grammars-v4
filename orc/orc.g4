/*
BSD License

Copyright (c) 2021, Tom Everett
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

expression
   : literal
   | variable
   | stop
   | tuple
   | list
   | record
   | call
   | dotAccess
   | prefixOperation
   | infixOperation
   | postfixOperation
   | parallel
   | sequence
   | prune
   | otherwise
   | conditional
   | lambda
   | withDeclaration
   | withTypeInformation
   | typeOverride
   | '(' expression ')'
   ;

stop
   : 'stop'
   ;

tuple
   : expression ',' expression (',' expression)*
   ;

list
   : '[' expression (',' expression)* ']'
   ;

record
   : '{' '.' (key '=' expression) (',' key '=' expression)* '.' '}'
   ;

call
   : expression typeArguments? arguments
   ;

dotAccess
   : expression '.' key
   ;

prefixOperation
   : PREFIXOPERATOR expression
   ;

infixOperation
   : expression INFIXOPERATOR expression
   ;

postfixOperation
   : expression POSTFIXOPERATOR
   ;

parallel
   : expression '|' expression
   ;

sequence
   : expression '>' pattern? '>' expression
   ;

prune
   : expression '<' pattern? '<' expression
   ;

otherwise
   : expression ';' expression
   ;

conditional
   : 'if' expression 'then' expression 'else' expression
   ;

lambda
   : 'lambda' typeParameters? parameters returnType? '=' expression
   ;

withDeclaration
   : declaration expression
   ;

withTypeInformation
   : expression '::' type
   ;

typeOverride
   : expression ':!:' type
   ;

declaration
   : declareVal '#?'
   | declareDefinition '#?'
   | declareDefclass '#?'
   | declareSignature '#?'
   | declareSite '#?'
   | declareClass '#?'
   | declareInclude '#?'
   | declareType '#?'
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
   : 'type' typeVariable typeParameters? '=' constructor+
   ;

constructor
   : variable '(' slot (',' slot)* ')'
   ;

slot
   : type
   | '_'
   ;

pattern
   : wildcardPattern
   | literalPattern
   | variablePattern
   | tuplePattern
   | listPattern
   | recordPattern
   | consPattern
   | callPattern
   | asPattern
   | patternWithTypeInformation
   | '(' Pattern ')'
   ;

wildcardPattern
   : '_'
   ;

literalPattern
   : Literal
   ;

variablePattern
   : Variable
   ;

tuplePattern
   : '(' pattern (',' pattern)* ')'
   ;

listPattern
   : '[' pattern (',' pattern)* ']'
   ;

recordPattern
   : '{' '.' key '=' pattern (',' key '=' pattern)* '.' '}'
   ;

consPattern
   : pattern ':' pattern
   ;

callPattern
   : variable '(' pattern (',' pattern)* ')'
   ;

asPattern
   : pattern 'as' variable
   ;

patternWithTypeInformation
   : pattern '::' type
   ;

type
   : typeVariable
   | tupleType
   | recordType
   | functionType
   | typeApplication
   | '(' type ')'
   ;

tupleType
   : '(' type (',' type)* ')'
   ;

recordType
   : '{' '.' key '=' type (',' key '=' type)* '.' '}'
   ;

functionType
   : 'lambda' typeParameters? argumentTypes returnType
   ;

typeApplication
   : type typeArguments
   ;

literal
   : SIGNALLITERAL
   | BOOLEANLITERAL
   | NUMBERLITERAL
   | CHARACTERSTRINGLITERAL
   | 'null'
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
   : '(' pattern (',' pattern)* ')'
   ;

typeParameters
   : '[' typeVariable (',' typeVariable)* ']'
   ;

argumentTypes
   : '(' type (',' type)* ')'
   ;

returnType
   : '::' type
   ;

arguments
   : '(' expression (',' expression)* ')'
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
   : INTEGERLITERAL DECIMALPART? EXPONENTPART?
   ;

EXPONENTPART
   : 'E' INTEGERLITERAL
   | 'E+' INTEGERLITERAL
   | 'E-' INTEGERLITERAL
   | 'e' INTEGERLITERAL
   | 'e+' INTEGERLITERAL
   | 'e-' INTEGERLITERAL
   ;

DECIMALPART
   : '.' INTEGERLITERAL
   ;

INTEGERLITERAL
   : DECIMALDIGIT+
   ;

DECIMALDIGIT
   : [0-9]
   ;

CHARACTERSTRINGLITERAL
   : '"' ~ '"'* '"'
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

