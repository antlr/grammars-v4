/*
Objective-C grammar.
The MIT License (MIT).
Copyright (c) 2016, Alex Petuschak (alex@swiftify.io).
Copyright (c) 2016, Ivan Kochurkin (kvanttt@gmail.com).
Converted to ANTLR 4 by Terence Parr; added @property and a few others.
Updated June 2014, Carlos Mejia.  Fix try-catch, add support for @( @{ @[ and blocks
June 2008 Cedric Cuche

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar ObjectiveCParser;

options { tokenVocab=ObjectiveCLexer; }

translationUnit
    : externalDeclaration* EOF
    ;

externalDeclaration
    : importDeclaration
    | functionDefinition
    | declaration
    | classInterface
    | classImplementation
    | categoryInterface
    | categoryImplementation
    | protocolDeclaration
    | protocolDeclarationList
    | classDeclarationList
    | implementationDefinitionList
    ;

importDeclaration
    : '@import' identifier ';'
    ;

classInterface
    : '@interface'
       classNameGeneric (':' superclassName)? protocolReferenceList? instanceVariables? interfaceDeclarationList?
      '@end'
    ;

categoryInterface
    : '@interface'
       classNameGeneric LP categoryName? RP protocolReferenceList? instanceVariables? interfaceDeclarationList?
      '@end'
    ;

classImplementation
    : '@implementation'
       classNameGeneric (':' superclassName)? instanceVariables? implementationDefinitionList?
      '@end'
    ;

categoryImplementation
    : '@implementation'
       classNameGeneric LP categoryName RP implementationDefinitionList?
      '@end'
    ;

protocolDeclaration
    : '@protocol'
       protocolName protocolReferenceList? ('@required' | interfaceDeclarationList | '@optional')*
      '@end'
    ;

protocolDeclarationList
    : '@protocol' protocolList ';'
    ;

classDeclarationList
    : '@class' classList ';'
    ;

classList
    : className (',' className)*
    ;

protocolReferenceList
    : LT protocolList GT
    ;

protocolList
    : protocolName (',' protocolName)*
    ;

propertyDeclaration
    : '@property' (LP propertyAttributesList RP)? ibOutletSpecifier? structDeclaration
    ;

propertyAttributesList
    : propertyAttribute (',' propertyAttribute)*
    ;

propertyAttribute
    : 'nonatomic' | 'assign' | 'weak' | 'strong' | 'retain' | 'readonly' | 'readwrite'
    | 'getter' '=' identifier
    | 'setter' '=' identifier ':'
    | nullabilitySpecifier
    | identifier
    ;

className
    : identifier
    ;

superclassName
    : identifier
    ;

categoryName
    : identifier
    ;

protocolName
    : protocolReferenceList
    | ('__covariant' | '__contravariant')?  identifier
    ;

instanceVariables
    : '{' structDeclaration* '}'
    | '{' visibilitySpecification structDeclaration+ '}'
    | '{' structDeclaration+ instanceVariables '}'
    | '{' visibilitySpecification structDeclaration+ instanceVariables '}'
    ;

visibilitySpecification
    : '@private'
    | '@protected'
    | '@package'
    | '@public'
    ;

interfaceDeclarationList
    : (declaration
    | classMethodDeclaration
    | instanceMethodDeclaration
    | propertyDeclaration)+
    ;

classMethodDeclaration
    : '+' methodDeclaration
    ;

instanceMethodDeclaration
    : '-' methodDeclaration
    ;

methodDeclaration
    : methodType? methodSelector macros? ';'
    ;

implementationDefinitionList
    : (functionDefinition
    | declaration
    | classMethodDefinition
    | instanceMethodDefinition
    | propertyImplementation
    )+
    ;

classMethodDefinition
    : '+' methodDefinition
    ;

instanceMethodDefinition
    : '-' methodDefinition
    ;

methodDefinition
    : methodType? methodSelector initDeclaratorList? ';'? compoundStatement
    ;

methodSelector
    : selector
    | keywordDeclarator+ (',' '...')?
    ;

keywordDeclarator
    : selector? ':' methodType* arcBehaviourSpecifier? identifier
    ;

selector
    : identifier
    | 'return'
    ;

methodType
    : LP typeName RP
    ;

propertyImplementation
    : '@synthesize' propertySynthesizeList ';'
    | '@dynamic' propertySynthesizeList ';'
    ;

propertySynthesizeList
    : propertySynthesizeItem (',' propertySynthesizeItem)*
    ;

propertySynthesizeItem
    : identifier ('=' identifier)?
    ;

blockType
    : typeSpecifier LP '^' typeSpecifier? RP blockParameters?
    ;

genericsSpecifier
    : LT typeSpecifier? (',' typeSpecifier)* GT
    ;

protocolQualifier
    : 'in' | 'out' | 'inout' | 'bycopy' | 'byref' | 'oneway'
    ;

dictionaryExpression
    : '@' '{' dictionaryPair? (',' dictionaryPair)* ','? '}'
    ;

dictionaryPair
    : castExpression ':' conditionalExpression
    ;

arrayExpression
    : '@' '[' conditionalExpression? (',' conditionalExpression)* ','? ']'
    ;

boxExpression
    : '@' LP expression RP
    | '@' (constant | identifier)
    ;

blockParameters
    : LP (typeVariableDeclarator | typeName | 'void')? (',' (typeVariableDeclarator | typeName))* RP
    ;

blockExpression
    : '^' typeSpecifier? blockParameters? compoundStatement
    ;

messageExpression
    : '[' receiver messageSelector ']'
    ;

receiver
    : expression
    | typeSpecifier
    ;

messageSelector
    : selector
    | keywordArgument+
    ;

keywordArgument
    : selector? ':' expression ('{' initializerList '}')? (',' expression ('{' initializerList '}')?)*
    ;

selectorExpression
    : '@selector' LP selectorName RP
    ;

selectorName
    : selector
    | (selector? ':')+
    ;

protocolExpression
    : '@protocol' LP protocolName RP
    ;

encodeExpression
    : '@encode' LP typeName RP
    ;

typeVariableDeclarator
    : declarationSpecifiers declarator
    ;

throwStatement
    : '@throw' LP identifier RP
    | '@throw' expression
    ;

tryBlock
    : '@try' tryStatement=compoundStatement catchStatement* ('@finally' finallyStatement=compoundStatement)?
    ;

catchStatement
    : '@catch' LP typeVariableDeclarator RP compoundStatement
    ;

synchronizedStatement
    : '@synchronized' LP expression RP compoundStatement
    ;

autoreleaseStatement
    : '@autoreleasepool'  compoundStatement
    ;

// TODO: if official doc: declarator instead of identifier ( LP parameterList? RP )?
functionDefinition
    : declarationSpecifiers? attributeSpecifier? declarationSpecifiers? attributeSpecifier? identifier (LP parameterList? RP)? attributeSpecifier? compoundStatement
    ;

attributeSpecifier
    : '__attribute__' (LP LP attribute (',' attribute)* RP RP)?
    ;

attribute
    : attributeName attributeParameters?
    ;

attributeName
    : 'const'
    | IDENTIFIER
    ;

attributeParameters
    : LP attributeParameterList? RP
    ;

attributeParameterList
    : attributeParameter (',' attributeParameter)*
    ;

attributeParameter
    : attribute
    | constant
    | stringLiteral
    | attributeParameterAssignment
    ;

attributeParameterAssignment
    : attributeName '=' (constant | attributeName | stringLiteral)
    ;

declaration
    : functionCallDeclaration
    | enumDeclaration
    | varDeclaration
    ;

functionCallDeclaration
    : attributeSpecifier? className attributeSpecifier? LP directDeclarator RP ';'
    ;

enumDeclaration
    : attributeSpecifier? storageClassSpecifier? enumSpecifier className? ';'
    ;

varDeclaration
    : attributeSpecifier? declarationSpecifiers attributeSpecifier? initDeclaratorList? ';'
    ;

// TODO: replace with declarationSpecifier and repetition.
declarationSpecifiers
    : (arcBehaviourSpecifier
    | storageClassSpecifier
    | typeSpecifier
    | typeQualifier
    | ibOutletSpecifier)+
    ;

ibOutletSpecifier
    : 'IBOutletCollection' LP className RP
    | 'IBOutlet'
    ;

initDeclaratorList
    : initDeclarator (',' initDeclarator)*
    ;

initDeclarator
    : declarator ('=' initializer)?
    ;

structOrUnionSpecifier
    : ('struct' | 'union') (identifier | identifier? '{' structDeclaration+ '}')
    ;

structDeclaration
    : specifierQualifierList structDeclaratorList macros? ';'
    ;

specifierQualifierList
    : (arcBehaviourSpecifier | nullabilitySpecifier | typeSpecifier | typeQualifier)+
    ;

arcBehaviourSpecifier
    : '__autoreleasing'
    | '__deprecated'
    | '__unsafe_unretained'
    | '__unused'
    | '__weak'
    ;

nullabilitySpecifier
    : 'nullable'
    | 'nonnull'
    ;

storageClassSpecifier
    : 'auto'
    | 'register'
    | 'static'
    | 'extern'
    | 'typedef'
    ;

typeSpecifier
    : 'void'
    | 'char'
    | 'short'
    | 'int'
    | 'long'
    | 'float'
    | 'double'
    | 'instancetype'
    | 'signed'
    | 'unsigned'
    | typeofExpression
    | (className (protocolReferenceList | genericsSpecifier)?)
    | structOrUnionSpecifier
    | enumSpecifier
    | identifier pointer?
    ;

typeQualifier
    : 'const'
    | 'volatile'
    | protocolQualifier
    ;

typeofExpression
    : ('typeof' | '__typeof' | '__typeof__') (LP expression RP)
    ;

classNameGeneric
    : className (protocolReferenceList | genericsSpecifier)?
    ;

structDeclaratorList
    : structDeclarator (',' structDeclarator)*
    ;

structDeclarator
    : declarator
    | declarator? ':' constant
    ;

enumSpecifier
    : 'enum' (identifier? ':' typeName)? (identifier ('{' enumeratorList '}')? | '{' enumeratorList '}')
    | ('NS_OPTIONS' | 'NS_ENUM') LP typeName ',' identifier RP '{' enumeratorList '}'
    ;

enumeratorList
    : enumerator (',' enumerator)* ','?
    ;

enumerator
    : enumeratorIdentifier ('=' binaryExpression)?
    ;

enumeratorIdentifier
    : identifier
    | 'default'
    ;

directDeclarator
    : (identifier | LP declarator RP) declaratorSuffix*
    | LP '^' identifier? RP blockParameters
    ;

declaratorSuffix
    : '[' constantExpression? ']'
    | LP parameterList RP
    ;

parameterList
    : parameterDeclarationList (',' '...')?
    ;

pointer
    : '*' declarationSpecifiers? pointer?
    ;

macros
    : identifier (LP primaryExpression (',' primaryExpression)* RP)?
    ;

initializer
    : conditionalExpression
    | arrayInitializer
    | structInitializer
    ;

arrayInitializer
    : '{' (conditionalExpression (',' conditionalExpression)* ','?)? '}'
    ;

structInitializer
    : '{' ('.' assignmentExpression (',' '.' assignmentExpression)* ','?)? '}'
    ;

initializerList
    : initializer (',' initializer)* ','?
    ;

typeName
    : specifierQualifierList abstractDeclarator?
    | blockType
    ;

abstractDeclarator
    : pointer abstractDeclarator?
    | LP abstractDeclarator? RP abstractDeclaratorSuffix+
    | ('[' constantExpression? ']')+
    ;

abstractDeclaratorSuffix
    : '[' constantExpression? ']'
    | LP parameterDeclarationList? RP
    ;

parameterDeclarationList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    : declarationSpecifiers declarator
    | 'void'
    ;

declarator
    : pointer? directDeclarator
    ;

statement
    : labeledStatement ';'?
    | compoundStatement ';'?
    | selectionStatement ';'?
    | iterationStatement ';'?
    | jumpStatement ';'?
    | synchronizedStatement ';'?
    | autoreleaseStatement ';'?
    | throwStatement ';'?
    | tryBlock ';'?
    | expression ';'?
    | ';'
    ;

labeledStatement
    : identifier ':' statement
    ;

rangeExpression
    :  constantExpression ('...' constantExpression)?
    ;

compoundStatement
    : '{' (declaration | statement)* '}'
    ;

selectionStatement
    : IF LP expression RP statement (ELSE statement)?
    | switchStatement
    ;

switchStatement
    : 'switch' LP expression RP switchBlock
    ;

switchBlock
    : '{' switchSection* '}'
    ;

switchSection
    : switchLabel+ statement+
    ;

switchLabel
    : 'case' (rangeExpression | LP rangeExpression RP) ':'
    | 'default' ':'
    ;

forLoopInitializer
    : declarationSpecifiers initDeclaratorList
    | expression
    ;

iterationStatement
    : whileStatement
    | doStatement
    | forStatement
    | forInStatement
    ;

whileStatement
    : 'while' LP expression RP statement
    ;

doStatement
    : 'do' statement 'while' LP expression RP ';'
    ;

forStatement
    : 'for' LP forLoopInitializer? ';' expression? ';' expression? RP statement
    ;

forInStatement
    : 'for' LP typeVariableDeclarator 'in' expression? RP statement
    ;

jumpStatement
    : 'goto' identifier ';'
    | 'continue' ';'
    | 'break' ';'
    | 'return' expression? ';'
    ;

expression
    : assignmentExpression (',' assignmentExpression)*
    ;

assignmentExpression
    : conditionalExpression
    | unaryExpression assignmentOperator assignmentExpression
    ;

conditionalExpression
    : binaryExpression
    | binaryExpression '?' expression? ':' conditionalExpression
    ;

binaryExpression
    : castExpression
    | binaryExpression ('*' | DIV | '%') binaryExpression
    | binaryExpression ('+' | '-') binaryExpression
    | binaryExpression (LT LT | GT GT) binaryExpression
    | binaryExpression (LE | GE | LT | GT) binaryExpression
    | binaryExpression (NOTEQUAL | EQUAL) binaryExpression
    | binaryExpression '&' binaryExpression
    | binaryExpression '^' binaryExpression
    | binaryExpression '|' binaryExpression
    | binaryExpression AND binaryExpression
    | binaryExpression OR binaryExpression
    ;

castExpression
    : LP typeName RP (castExpression | initializer)
    | unaryExpression
    ;

assignmentOperator
    : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

constantExpression
    : identifier
    | constant
    ;

unaryExpression
    : postfixExpression
    | '++' unaryExpression
    | '--' unaryExpression
    | unaryOperator castExpression
    ;

unaryOperator
    : '&' | '*' | '+' | '-' | '~' | BANG
    ;

postfixExpression
    : primaryExpression                                  #primaryInPostfixExpression
    | postfixExpression '[' expression ']'               #indexerExpression
    | postfixExpression LP argumentExpressionList? RP    #functionCallExpression
    | postfixExpression ('.' | '->') identifier          #propertyExpression
    | postfixExpression ('++' | '--')                    #incDecExpression
    ;

argumentExpressionList
    : argumentExpression (',' argumentExpression)*
    ;

argumentExpression
    : assignmentExpression
    | typeSpecifier
    ;

primaryExpression
    : identifier
    | constant
    | stringLiteral
    | LP expression RP
    | messageExpression
    | selectorExpression
    | protocolExpression
    | encodeExpression
    | dictionaryExpression
    | arrayExpression
    | boxExpression
    | blockExpression
    ;

constant
    : HEX_LITERAL
    | OCTAL_LITERAL
    | BINARY_LITERAL
    | ('+' | '-')? DECIMAL_LITERAL
    | ('+' | '-')? FLOATING_POINT_LITERAL
    | CHARACTER_LITERAL
    ;

stringLiteral
    : STRING ('\\'? STRING)*
    | QUOTE_STRING+
    ;

identifier
    : IDENTIFIER
    | NULLABLE
    | WWEAK
    | TYPEOF | TYPEOF__ | TYPEOF____ | KINDOF__ | SIZEOF
    | ASSIGNPA | GETTER | NONATOMIC | SETTER | STRONG | RETAIN | READONLY | READWRITE | WEAK
    | ID
    | COVARIANT | CONTRAVARIANT
    | WUNUSED
    ;
