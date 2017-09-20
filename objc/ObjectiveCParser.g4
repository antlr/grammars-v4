/*
Objective-C grammar.
The MIT License (MIT).
Copyright (c) 2016-2017, Alex Petuschak (alex@swiftify.io).
Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com).
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
    : topLevelDeclaration* EOF
    ;

topLevelDeclaration
    : importDeclaration
    | functionDeclaration
    | declaration
    | classInterface
    | classImplementation
    | categoryInterface
    | categoryImplementation
    | protocolDeclaration
    | protocolDeclarationList
    | classDeclarationList
    | functionDefinition
    ;

importDeclaration
    : '@import' identifier ';'
    ;

classInterface
    : IB_DESIGNABLE?
      '@interface'
       className=genericTypeSpecifier (':' superclassName=identifier)? (LT protocolList GT)? instanceVariables? interfaceDeclarationList?
      '@end'
    ;

categoryInterface
    : '@interface'
       categoryName=genericTypeSpecifier LP className=identifier? RP (LT protocolList GT)? instanceVariables? interfaceDeclarationList?
      '@end'
    ;

classImplementation
    : '@implementation'
       className=genericTypeSpecifier (':' superclassName=identifier)? instanceVariables? implementationDefinitionList?
      '@end'
    ;

categoryImplementation
    : '@implementation'
       categoryName=genericTypeSpecifier LP className=identifier RP implementationDefinitionList?
      '@end'
    ;

genericTypeSpecifier
    : identifier ((LT protocolList GT) | genericsSpecifier)?
    ;

protocolDeclaration
    : '@protocol'
       protocolName (LT protocolList GT)? protocolDeclarationSection*
      '@end'
    ;

protocolDeclarationSection
    : modifier=(REQUIRED | OPTIONAL) interfaceDeclarationList*
    | interfaceDeclarationList+
    ;

protocolDeclarationList
    : '@protocol' protocolList ';'
    ;

classDeclarationList
    : '@class' identifier (',' identifier)* ';'
    ;

protocolList
    : protocolName (',' protocolName)*
    ;

propertyDeclaration
    : '@property' (LP propertyAttributesList RP)? ibOutletQualifier? IB_INSPECTABLE? fieldDeclaration
    ;

propertyAttributesList
    : propertyAttribute (',' propertyAttribute)*
    ;

propertyAttribute
    : ATOMIC
    | NONATOMIC
    | STRONG
    | WEAK
    | RETAIN
    | ASSIGN
    | UNSAFE_UNRETAINED
    | COPY
    | READONLY
    | READWRITE
    | GETTER '=' identifier
    | SETTER '=' identifier ':'
    | nullabilitySpecifier
    | identifier
    ;

protocolName
    : LT protocolList GT
    | ('__covariant' | '__contravariant')?  identifier
    ;

instanceVariables
    : '{' visibilitySection* '}'
    ;

visibilitySection
    : accessModifier fieldDeclaration*
    | fieldDeclaration+
    ;

accessModifier
    : PRIVATE
    | PROTECTED
    | PACKAGE
    | PUBLIC
    ;

interfaceDeclarationList
    : (declaration
    | classMethodDeclaration
    | instanceMethodDeclaration
    | propertyDeclaration
    | functionDeclaration)+
    ;

classMethodDeclaration
    : '+' methodDeclaration
    ;

instanceMethodDeclaration
    : '-' methodDeclaration
    ;

methodDeclaration
    : methodType? methodSelector macro? ';'
    ;

implementationDefinitionList
    : (functionDefinition
    | declaration
    | classMethodDefinition
    | instanceMethodDefinition
    | propertyImplementation
    )+;

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
    : nullabilitySpecifier? typeSpecifier nullabilitySpecifier? LP '^' (nullabilitySpecifier | typeSpecifier)? RP blockParameters?
    ;

genericsSpecifier
    : LT (typeSpecifierWithPrefixes (',' typeSpecifierWithPrefixes)*)? GT
    ;

typeSpecifierWithPrefixes
    : typePrefix* typeSpecifier
    ;

dictionaryExpression
    : '@' '{' (dictionaryPair (',' dictionaryPair)* ','?)? '}'
    ;

dictionaryPair
    : castExpression ':' expression
    ;

arrayExpression
    : '@' '[' (expressions ','?)? ']'
    ;

boxExpression
    : '@' LP expression RP
    | '@' (constant | identifier)
    ;

blockParameters
    : LP ((typeVariableDeclaratorOrName | 'void') (',' typeVariableDeclaratorOrName)*)? RP
    ;

typeVariableDeclaratorOrName
    : typeVariableDeclarator
    | typeName
    ;

blockExpression
    : '^' typeSpecifier? nullabilitySpecifier? blockParameters? compoundStatement
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
    : selector? ':' keywordArgumentType (',' keywordArgumentType)*
    ;

keywordArgumentType
    : expressions nullabilitySpecifier? ('{' initializerList '}')?
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
    : '@autoreleasepool' compoundStatement
    ;

functionDeclaration
    : functionSignature ';'
    ;

functionDefinition
    : functionSignature compoundStatement
    ;

functionSignature
    : declarationSpecifiers? identifier (LP parameterList? RP) attributeSpecifier?
    ;

attribute
    : attributeName attributeParameters?
    ;

attributeName
    : 'const'
    | identifier
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
    : functionCallExpression
    | enumDeclaration
    | varDeclaration
    | typedefDeclaration
    ;

functionCallExpression
    : attributeSpecifier? identifier attributeSpecifier? LP directDeclarator RP ';'
    ;

enumDeclaration
    : attributeSpecifier? TYPEDEF? enumSpecifier identifier? ';'
    ;

varDeclaration
    : (declarationSpecifiers initDeclaratorList | declarationSpecifiers) ';'
    ;

typedefDeclaration
    : attributeSpecifier? TYPEDEF (declarationSpecifiers typeDeclaratorList | declarationSpecifiers) ';'
    ;

typeDeclaratorList
    : typeDeclarator (',' typeDeclarator)*
    ;

typeDeclarator
    : pointer? directDeclarator
    ;

declarationSpecifiers
    : (storageClassSpecifier
    | attributeSpecifier
    | arcBehaviourSpecifier
    | nullabilitySpecifier
    | ibOutletQualifier
    | typePrefix
    | typeQualifier
    | typeSpecifier)+
    ;

attributeSpecifier
    : '__attribute__' LP LP attribute (',' attribute)* RP RP
    ;

initDeclaratorList
    : initDeclarator (',' initDeclarator)*
    ;

initDeclarator
    : declarator ('=' initializer)?
    ;

structOrUnionSpecifier
    : ('struct' | 'union') (identifier | identifier? '{' fieldDeclaration+ '}')
    ;

fieldDeclaration
    : specifierQualifierList fieldDeclaratorList macro? ';'
    ;

specifierQualifierList
    : (arcBehaviourSpecifier
    | nullabilitySpecifier
    | ibOutletQualifier
    | typePrefix
    | typeQualifier
    | typeSpecifier)+
    ;

ibOutletQualifier
    : IB_OUTLET_COLLECTION LP identifier RP
    | IB_OUTLET
    ;

arcBehaviourSpecifier
    : WEAK_QUALIFIER
    | STRONG_QUALIFIER
    | AUTORELEASING_QUALIFIER
    | UNSAFE_UNRETAINED_QUALIFIER
    ;

nullabilitySpecifier
    : NULL_UNSPECIFIED
    | NULLABLE
    | NONNULL
    | NULL_RESETTABLE
    ;

storageClassSpecifier
    : AUTO
    | REGISTER
    | STATIC
    | EXTERN
    ;

typePrefix
    : BRIDGE
    | BRIDGE_TRANSFER
    | BRIDGE_RETAINED
    | BLOCK
    | INLINE
    | NS_INLINE
    | KINDOF
    ;

typeQualifier
    : CONST
    | VOLATILE
    | RESTRICT
    | protocolQualifier
    ;

protocolQualifier
    : 'in'
    | 'out'
    | 'inout'
    | 'bycopy'
    | 'byref'
    | 'oneway'
    ;

typeSpecifier
    : 'void'
    | 'char'
    | 'short'
    | 'int'
    | 'long'
    | 'float'
    | 'double'
    | 'signed'
    | 'unsigned'
    | typeofExpression
    | genericTypeSpecifier
    | structOrUnionSpecifier
    | enumSpecifier
    | identifier pointer?
    ;

typeofExpression
    : TYPEOF (LP expression RP)
    ;

fieldDeclaratorList
    : fieldDeclarator (',' fieldDeclarator)*
    ;

fieldDeclarator
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
    : enumeratorIdentifier ('=' expression)?
    ;

enumeratorIdentifier
    : identifier
    | 'default'
    ;

directDeclarator
    : (identifier | LP declarator RP) declaratorSuffix*
    | LP '^' nullabilitySpecifier? identifier? RP blockParameters
    ;

declaratorSuffix
    : '[' constantExpression? ']'
    ;

parameterList
    : parameterDeclarationList (',' '...')?
    ;

pointer
    : '*' declarationSpecifiers? pointer?
    ;

macro
    : identifier (LP primaryExpression (',' primaryExpression)* RP)?
    ;

arrayInitializer
    : '{' (expressions ','?)? '}'
    ;

structInitializer
    : '{' ('.' expression (',' '.' expression)* ','?)? '}'
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
    | expressions ';'?
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
    : IF LP expression RP ifBody=statement (ELSE elseBody=statement)?
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
    : 'for' LP forLoopInitializer? ';' expression? ';' expressions? RP statement
    ;

forLoopInitializer
    : declarationSpecifiers initDeclaratorList
    | expressions
    ;

forInStatement
    : 'for' LP typeVariableDeclarator 'in' expression? RP statement
    ;

jumpStatement
    : GOTO identifier
    | CONTINUE
    | BREAK
    | RETURN expression?
    ;

expressions
    : expression (',' expression)*
    ;

expression
    : castExpression

    | expression op=(MUL | DIV | MOD) expression
    | expression op=(ADD | SUB) expression
    | expression (LT LT | GT GT) expression
    | expression op=(LE | GE | LT | GT) expression
    | expression op=(NOTEQUAL | EQUAL) expression
    | expression op=BITAND expression
    | expression op=BITXOR expression
    | expression op=BITOR expression
    | expression op=AND expression
    | expression op=OR expression

    | expression QUESTION trueExpression=expression? COLON falseExpression=expression

    | unaryExpression assignmentOperator assignmentExpression=expression
    ;

assignmentOperator
    : '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

castExpression
    : unaryExpression
    | (LP typeName RP) (castExpression | initializer)
    ;

initializer
    : expression
    | arrayInitializer
    | structInitializer
    ;

constantExpression
    : identifier
    | constant
    ;

unaryExpression
    : postfixExpression
    | SIZEOF (unaryExpression | LP typeSpecifier RP)
    | op=(INC | DEC) unaryExpression
    | unaryOperator castExpression
    ;

unaryOperator
    : '&'
    | '*'
    | '+'
    | '-'
    | '~'
    | BANG
    ;

postfixExpression
    : primaryExpression postfix*
    | postfixExpression (DOT | STRUCTACCESS) identifier postfix*  // TODO: get rid of property and postfix expression.
    ;

postfix
    : LBRACK expression RBRACK
    | LP argumentExpressionList? RP
    | LP (COMMA | macroArguments+=~RP)+ RP
    | op=(INC | DEC)
    ;

argumentExpressionList
    : argumentExpression (',' argumentExpression)*
    ;

argumentExpression
    : expression
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
    | NIL
    | NULL
    | YES
    | NO
    | TRUE
    | FALSE
    ;

stringLiteral
    : (STRING_START (STRING_VALUE | STRING_NEWLINE)* STRING_END)+
    ;

identifier
    : IDENTIFIER

    | BOOL
    | Class
    | BYCOPY
    | BYREF
    | ID
    | IMP
    | IN
    | INOUT
    | ONEWAY
    | OUT
    | PROTOCOL_
    | SEL
    | SELF
    | SUPER
    | ATOMIC
    | NONATOMIC
    | RETAIN

    | AUTORELEASING_QUALIFIER
    | BLOCK
    | BRIDGE_RETAINED
    | BRIDGE_TRANSFER
    | COVARIANT
    | CONTRAVARIANT
    | DEPRECATED
    | KINDOF
    | UNUSED

    | NS_INLINE
    | NS_ENUM
    | NS_OPTIONS

    | NULL_UNSPECIFIED
    | NULLABLE
    | NONNULL
    | NULL_RESETTABLE

    | ASSIGN
    | COPY
    | GETTER
    | SETTER
    | STRONG
    | READONLY
    | READWRITE
    | WEAK
    | UNSAFE_UNRETAINED

    | IB_OUTLET
    | IB_OUTLET_COLLECTION
    | IB_INSPECTABLE
    | IB_DESIGNABLE
    ;
