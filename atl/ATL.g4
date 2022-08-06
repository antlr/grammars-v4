/*
Copyright 2015 Spikes N.V.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

Initially developed in the context of ARTIST EU project www.artist-project.eu
*/

grammar ATL;

/*
 * Parser Rules
 */
unit
   : ( module | library_ | query ) EOF
   ;

module
   : 'module' (STRING | IDENTIFIER) ';' 'create' targetModelPattern transformationMode sourceModelPattern ';' libraryRef* moduleElement*
   ;

// Small change introducing targetModelPattern, sourceModelPattern, transformationMode to facilitate fetching the information via the parser
targetModelPattern
   : oclModel (',' oclModel)*
   ;

sourceModelPattern
   : oclModel (',' oclModel)*
   ;

transformationMode
   : 'refining'
   | 'from'
   ;

library_
   : 'library' (STRING | IDENTIFIER) ';' libraryRef* helper*
   ;

query
   : 'query' (STRING | IDENTIFIER) '=' oclExpression ';' libraryRef* helper*
   ;

libraryRef
   : 'uses' STRING ';'
   ;

moduleElement
   : helper
   | arule
   ;

helper
   : 'helper' oclFeatureDefinition ';'
   ;

oclFeatureDefinition
   : oclContextDefinition? 'def' ':' oclFeature
   ;

oclContextDefinition
   : 'context' oclType
   ;

oclFeature
   : operation
   | attribute
   ;

operation
   : IDENTIFIER '(' (parameter (',' parameter)*)? ')' ':' oclType '=' oclExpression
   ;

parameter
   : IDENTIFIER ':' oclType
   ;

attribute
   : IDENTIFIER ':' oclType '=' oclExpression
   ;

arule
   : calledRule
   | matchedRule
   ;

matchedRule
   : lazyMatchedRule
   | matchedRule_abstractContents
   ;

lazyMatchedRule
   : 'unique'? 'lazy' 'abstract'? 'refining'? 'rule' IDENTIFIER ('extends' IDENTIFIER)? '{' inPattern ('using' '{' ruleVariableDeclaration* '}')? outPattern? actionBlock? '}'
   ;

ruleVariableDeclaration
   : IDENTIFIER ':' oclType '=' oclExpression ';'
   ;

calledRule
   : 'entrypoint'? 'endpoint'? 'rule' IDENTIFIER '(' (parameter (',' parameter)*)? ')' '{' ('using' '{' ruleVariableDeclaration* '}')? outPattern? actionBlock? '}'
   ;

inPattern
   : 'from' inPatternElement (',' inPatternElement)* ('(' oclExpression ')')?
   ;

inPatternElement
   : simpleInPatternElement
   ;

simpleInPatternElement
   : IDENTIFIER ':' oclType ('in' IDENTIFIER (',' IDENTIFIER)*)?
   ;

outPattern
   : 'to' outPatternElement (',' outPatternElement)*
   ;

outPatternElement
   : simpleOutPatternElement
   | forEachOutPatternElement
   ;

simpleOutPatternElement
   : IDENTIFIER ':' oclType ('in' IDENTIFIER)? ('mapsTo' IDENTIFIER)? ('(' (binding (',' binding)*)? ')')?
   ;

forEachOutPatternElement
   : IDENTIFIER ':' 'distinct' oclType 'foreach' '(' iterator 'in' oclExpression ')' ('mapsTo' IDENTIFIER)? ('(' (binding (',' binding)*)? ')')?
   ;

binding
   : IDENTIFIER '<-' oclExpression
   ;

actionBlock
   : 'do' '{' statement* '}'
   ;

statement
   : ifStat
   | expressionStat
   | bindingStat
   | forStat
   ;

bindingStat
   : oclExpression '<-' oclExpression ';'
   ;

expressionStat
   : oclExpression ';'
   ;

ifStat
   : 'if' '(' oclExpression ')' (statement | '{' statement* '}') ('else' (statement | '{' statement* '}'))?
   ;

forStat
   : 'for' '(' iterator 'in' oclExpression ')' '{' statement* '}'
   ;

oclModel
   : IDENTIFIER ':' IDENTIFIER
   ;

oclModelElement
   : IDENTIFIER '!' (STRING | IDENTIFIER)
   ;

oclExpression
   : priority_5
   | letExp
   ;

iteratorExp
   : IDENTIFIER '(' iterator (',' iterator)* '|' oclExpression ')'
   ;

iterateExp
   : 'iterate' '(' iterator (',' iterator)* ';' variableDeclaration '|' oclExpression ')'
   ;

collectionOperationCallExp
   : IDENTIFIER '(' (oclExpression (',' oclExpression)*)? ')'
   ;

operationCallExp
   : IDENTIFIER '(' (oclExpression (',' oclExpression)*)? ')'
   ;

navigationOrAttributeCallExp
   : IDENTIFIER
   ;

iterator
   : IDENTIFIER
   ;

oclUndefinedExp
   : 'OclUndefined'
   ;

primitiveExp
   : numericExp
   | booleanExp
   | stringExp
   ;

numericExp
   : integerExp
   | realExp
   ;

booleanExp
   : 'true'
   | 'false'
   ;

integerExp
   : INTEGER
   ;

realExp
   : FLOAT
   ;

stringExp
   : STRING
   ;

ifExp
   : 'if' oclExpression 'then' oclExpression 'else' oclExpression 'endif'
   ;

variableExp
   : IDENTIFIER
   ;

superExp
   : 'super'
   ;

letExp
   : 'let' variableDeclaration 'in' oclExpression
   ;

variableDeclaration
   : IDENTIFIER ':' oclType '=' oclExpression
   ;

enumLiteralExp
   : '#' IDENTIFIER
   ;

collectionExp
   : bagExp
   | setExp
   | orderedSetExp
   | sequenceExp
   ;

bagExp
   : 'Bag' '{' (oclExpression (',' oclExpression)*)? '}'
   ;

setExp
   : 'Set' '{' (oclExpression (',' oclExpression)*)? '}'
   ;

orderedSetExp
   : 'OrderedSet' '{' (oclExpression (',' oclExpression)*)? '}'
   ;

sequenceExp
   : 'Sequence' '{' (oclExpression (',' oclExpression)*)? '}'
   ;

mapExp
   : 'Map' '{' (mapElement (',' mapElement)*)? '}'
   ;

mapElement
   : '(' oclExpression ',' oclExpression ')'
   ;

tupleExp
   : 'Tuple' '{' (tuplePart (',' tuplePart)*)? '}'
   ;

tuplePart
   : IDENTIFIER (':' oclType)? '=' oclExpression
   ;

oclType
   : oclModelElement
   | oclAnyType
   | tupleType
   | mapType
   | primitive
   | collectionType
   | oclType_abstractContents
   ;

oclAnyType
   : oclAnyType_abstractContents
   ;

tupleType
   : 'TupleType' '(' (tupleTypeAttribute (',' tupleTypeAttribute)*)? ')'
   ;

tupleTypeAttribute
   : IDENTIFIER ':' oclType
   ;

mapType
   : 'Map' '(' oclType ',' oclType ')'
   ;

primitive
   : numericType
   | booleanType
   | stringType
   ;

numericType
   : integerType
   | realType
   ;

integerType
   : 'Integer'
   ;

realType
   : 'Real'
   ;

booleanType
   : 'Boolean'
   ;

stringType
   : 'String'
   ;

collectionType
   : bagType
   | setType
   | orderedSetType
   | sequenceType
   | collectionType_abstractContents
   ;

bagType
   : 'Bag' '(' oclType ')'
   ;

setType
   : 'Set' '(' oclType ')'
   ;

orderedSetType
   : 'OrderedSet' '(' oclType ')'
   ;

sequenceType
   : 'Sequence' '(' oclType ')'
   ;

priority_0
   : primary_oclExpression ('.' (operationCallExp | navigationOrAttributeCallExp) | '->' (iteratorExp | iterateExp | collectionOperationCallExp))*
   ;

priority_1
   : 'not' priority_0
   | '-' priority_0
   | priority_0
   ;

priority_2
   : priority_1 ('*' priority_1 | '/' priority_1 | 'div' priority_1 | 'mod' priority_1)*
   ;

priority_3
   : priority_2 ('+' priority_2 | '-' priority_2)*
   ;

priority_4
   : priority_3 ('=' priority_3 | '>' priority_3 | '<' priority_3 | '>=' priority_3 | '<=' priority_3 | '<>' priority_3)*
   ;

priority_5
   : priority_4 ('and' priority_4 | 'or' priority_4 | 'xor' priority_4 | 'implies' priority_4)*
   ;

matchedRule_abstractContents
   : 'nodefault'? 'abstract'? 'refining'? 'rule' IDENTIFIER ('extends' IDENTIFIER)? '{' inPattern ('using' '{' ruleVariableDeclaration* '}')? outPattern? actionBlock? '}'
   ;

oclType_abstractContents
   : 'OclType'
   ;

oclAnyType_abstractContents
   : 'OclAny'
   ;

collectionType_abstractContents
   : 'Collection' '(' oclType ')'
   ;

primary_oclExpression
   : variableExp
   | oclUndefinedExp
   | primitiveExp
   | ifExp
   | superExp
   | enumLiteralExp
   | collectionExp
   | mapExp
   | tupleExp
   | oclType
   | '(' oclExpression ')'
   ;


STRING
   : '"' DoubleStringCharacters? '"' | '\'' SingleStringCharacters? '\''
   ;


fragment DoubleStringCharacters
   : DoubleStringCharacter +
   ;


fragment DoubleStringCharacter
   : ~ ["\\] | '\\' [btnfr"'\\]
   ;


fragment SingleStringCharacters
   : SingleStringCharacter +
   ;


fragment SingleStringCharacter
   : ~ ['\\] | '\\' [btnfr"'\\]
   ;

// Integer Literals

INTEGER
   : DecimalIntegerLiteral
   ;


fragment DecimalIntegerLiteral
   : DecimalNumeral
   ;


fragment DecimalNumeral
   : '0' | NonZeroDigit Digits?
   ;


fragment Digits
   : Digit Digit*
   ;


fragment Digit
   : '0' | NonZeroDigit
   ;


fragment NonZeroDigit
   : [1-9]
   ;


FLOAT
   : DecimalFloatingPointLiteral
   ;


fragment DecimalFloatingPointLiteral
   : Digits '.' Digits? ExponentPart? FloatTypeSuffix? | '.' Digits ExponentPart? FloatTypeSuffix? | Digits ExponentPart FloatTypeSuffix? | Digits FloatTypeSuffix
   ;


fragment ExponentPart
   : ExponentIndicator SignedInteger
   ;


fragment ExponentIndicator
   : [eE]
   ;


fragment SignedInteger
   : Sign? Digits
   ;


fragment Sign
   : [+-]
   ;


fragment FloatTypeSuffix
   : [fFdD]
   ;


IDENTIFIER
   : LetterOrDigit LetterOrDigit*
   ;


fragment Letter
   : [a-zA-Z$_]
   ;


fragment LetterOrDigit
   : [a-zA-Z0-9$_]
   ;

//
// Whitespace and comments
//

WS
   : [ \t\r\n\u000C] + -> skip
   ;


COMMENT
   : '/*' .*? '*/' -> skip
   ;


LINE_COMMENT
   : '--' ~ [\r\n]* -> skip
   ;
