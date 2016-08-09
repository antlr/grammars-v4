/*
Objective-C grammar.
The MIT License (MIT).
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
    | topLevelFunctionDefinition
    | declaration
    | classInterface
    | classImplementation
    | categoryInterface
    | categoryImplementation
    | protocolDeclaration
    | protocolDeclarationList
    | classDeclarationList
    | macros
    ;

topLevelFunctionDefinition
    : functionDefinition
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
      classNameGeneric '(' categoryName? ')' protocolReferenceList? instanceVariables? interfaceDeclarationList?
      '@end'
    ;

classImplementation
    : '@implementation'
       classNameGeneric (':' superclassName)? instanceVariables? implementationDefinitionList?
      '@end'
    ;

categoryImplementation
    : '@implementation'
       classNameGeneric '(' categoryName ')' implementationDefinitionList?
      '@end'
    ;

protocolDeclaration
    : '@protocol'
       protocolName protocolReferenceList? '@required'? interfaceDeclarationList? '@optional'? interfaceDeclarationList?
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
    : '<' protocolList '>'
    ;

protocolList
    : protocolName (',' protocolName)*
    ;

propertyDeclaration
    : '@property' ('(' propertyAttributesList ')')? structDeclaration
    ;

propertyAttributesList
    : propertyAttribute (',' propertyAttribute)*
    ;

propertyAttribute
    : 'nonatomic' | 'assign' | 'weak' | 'strong' | 'retain' | 'readonly' | 'readwrite'
    | 'getter' '=' identifier //  getter
    | 'setter' '=' identifier ':' // setter
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
    | identifier
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
    : interfaceDeclarationListItem+
    ;

interfaceDeclarationListItem
    : declaration
    | classMethodDeclaration
    | instanceMethodDeclaration
    | propertyDeclaration
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
    : implementationDefinitionListItem+
    ;

implementationDefinitionListItem
    : functionDefinition
    | declaration
    | classMethodDefinition
    | instanceMethodDefinition
    | propertyImplementation
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
    : selector? ':' methodType* identifier
    ;

selector
    : identifier
    ;

methodType
    : '(' typeName ')'
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
    : typeofTypeSpecifier '(' '^' typeofTypeSpecifier? ')' blockParameters?
    ;

genericsSpecifier
    : '<' typeofTypeSpecifier? (',' typeofTypeSpecifier)* '>'
    ;

signedUnsigned
    : ('unsigned' | 'signed')? ('char' | 'short' | 'int' | 'long' 'long'?)
    | 'unsigned' | 'signed'
    ;

protocolQualifier
    : 'in' | 'out' | 'inout' | 'bycopy' | 'byref' | 'oneway'
    ;

primaryExpression
    : identifier
    | vaArgExpression
    | constant
    | stringLiteral
    | '(' expression ')'
    | 'self'
    | 'super'
    | messageExpression
    | selectorExpression
    | protocolExpression
    | encodeExpression
    | dictionaryExpression
    | arrayExpression
    | boxExpression
    | blockExpression
    | structExpression
    ;

structExpression
    : '{' (structPair ','?)+ '}'
    ;

structPair
    : '.' identifier '=' postfixExpression
    ;

vaArgExpression
    : 'va_arg' '(' postfixExpression ',' typeSpecifier ')'
    ;

dictionaryExpression
    : '@' '{' dictionaryPair? (',' dictionaryPair)* ','? '}'
    ;

dictionaryPair
    : castExpression ':' castExpression
    ;

arrayExpression
    : '@' '[' castExpression? (',' castExpression)* ','? ']'
    ;

boxExpression
    : '@' '('expression')'
    | '@' (constant | identifier)
    ;

blockParameters
    : '(' (typeVariableDeclarator | typeName | 'void')? (',' (typeVariableDeclarator | typeName))* ')'
    ;

blockExpression
    :'^' typeofTypeSpecifier? blockParameters? compoundStatement
    ;

messageExpression
    : '[' receiver messageSelector ']'
    ;

receiver
    : expression
    | className
    | 'super'
    ;

messageSelector
    : selector
    | keywordArgument+
    ; 

// initializer_list?
keywordArgument
    : selector? ':' expression ('{' initializer_list ','? '}')? (',' expression ('{' initializer_list ','? '}')?)*
    ;

selectorExpression
    : '@selector' '(' selectorName ')'
    ;

selectorName
    : selector
    | (selector? ':')+
    ;

protocolExpression
    : '@protocol' '(' protocolName ')'
    ;

encodeExpression
    : '@encode' '(' typeName ')'
    ;

typeVariableDeclarator
    : declarationSpecifier+ declarator
    ;

tryStatement
    : '@try' compoundStatement
    ;

catchStatement
    : '@catch' '(' typeVariableDeclarator ')' compoundStatement
    ;

finallyStatement
    : '@finally' compoundStatement
    ;

throwStatement
    : '@throw' '(' identifier ')'
    | '@throw' expression
    ;

tryBlock
    : tryStatement catchStatement* finallyStatement?
    ;

synchronizedStatement
    : '@synchronized' '(' expression ')' compoundStatement
    ;

autoreleaseStatement
    : '@autoreleasepool'  compoundStatement
    ;

functionDefinition
    : declarationSpecifier* functionSignature compoundStatement
    ;

functionSignature
    : declarator
    ;

declaration
    : declarationSpecifier* initDeclaratorList? macros? ';'
    ;

declarationSpecifier
    : arcBehaviourSpecifier
    | storageClassSpecifier
    | typeofTypeSpecifier
    | typeQualifier
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
    : (arcBehaviourSpecifier | typeofTypeSpecifier | typeQualifier)+
    ;

arcBehaviourSpecifier
    : '__unsafe_unretained'
    | '__weak'
    ;

storageClassSpecifier
    : 'auto'
    | 'register'
    | 'static'
    | 'extern'
    | 'typedef'
    ;

typeofTypeSpecifier
    : ('typeof' | '__typeof' | '__typeof__') '(' (className | 'self') ')'
    | '__kindof'? typeSpecifier
    ;

typeSpecifier
    : 'nullable'?
    ( 'void'
    | 'char'
    | 'float'
    | 'double'
    | signedUnsigned
    | 'instancetype'
    | 'id' protocolReferenceList?
    | classNameGeneric
    | structOrUnionSpecifier
    | enumSpecifier
    | identifier)
    pointer?
    ;

typeQualifier
    : 'const'
    | 'volatile'
    | protocolQualifier
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
    : 'enum' (identifier (':' typeName)? ('{' enumeratorList '}')? | (':' typeName)? '{' enumeratorList '}')
    | 'NS_OPTIONS' '(' typeName ',' identifier ')' '{' enumeratorList '}'
    | 'NS_ENUM' '(' typeName ',' identifier ')' '{' enumeratorList '}'
    ;

enumeratorList
    : enumerator (',' enumerator)* ','?
    ;

enumerator
    : identifier ('=' binaryExpression)?
    ;

pointer
    : '*' declarationSpecifier* pointer?
    ;

declarator
    : pointer? directDeclarator
    ;

directDeclarator
    : identifier declaratorSuffix*
    | '(' declarator ')' declaratorSuffix* 
    | '(''^' identifier? ')' blockParameters
    ;

declaratorSuffix
    : '[' constantExpression? ']'
    | '(' (parameterDeclarationList (',' '...')?)? ')'
    ;

macros
    : identifier ('(' primaryExpression (',' primaryExpression)* ')')?
    ;

initializer
    : assignmentExpression
    | '{' (initializer_list ','?)? '}'
    ;

initializer_list
    : designation? initializer (',' designation? initializer)*
    ;

designation
    : designator+ '='
    ;

designator
    : '[' constantExpression ']'
    | '.' identifier
    ;

typeName
    : specifierQualifierList abstractDeclarator?
    | blockType
    ;

abstractDeclarator
    : pointer abstractDeclarator
    | '(' abstractDeclarator ')' abstractDeclaratorSuffix+
    | ('[' constantExpression? ']')+
    ;

abstractDeclaratorSuffix
    : '[' constantExpression? ']'
    | '(' parameterDeclarationList? ')'
    ;

parameterDeclarationList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    : declarationSpecifier+ (declarator? | abstractDeclarator)
    ;

statementList
    : statement+
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
    ;

labeledStatement
    : identifier ':' statement
    | 'case' constantExpression ':' statement
    | 'default' ':' statement
    ;

compoundStatement
    : '{' (declaration | statementList)* '}'
    ;

selectionStatement
    : IF '(' expression ')' statement (ELSE statement)?
    | 'switch' '(' expression ')' statement
    ;

forInStatement
    : 'for' '(' typeVariableDeclarator 'in' expression? ')' statement
    ;

forStatement
    : 'for' '(' ((declarationSpecifier+ initDeclaratorList) | expression)? ';' expression? ';' expression? ')' statement
    ;

whileStatement
    : 'while' '(' expression ')' statement
    ;

doStatement
    : 'do' statement 'while' '(' expression ')' ';'
    ;

iterationStatement
    : whileStatement
    | doStatement
    | forStatement
    | forInStatement
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

binaryExpression     // TODO: optimize binaryExpression for deep expressions handling.
    : castExpression
    | binaryExpression ('*' | DIV | '%') binaryExpression
    | binaryExpression ('+' | '-') binaryExpression
    | binaryExpression ('<' '<' | '>' '>' | '<') binaryExpression
    | binaryExpression ('<=' | '>=' | '>') binaryExpression
    | binaryExpression ('!=' | '==') binaryExpression
    | binaryExpression '&' binaryExpression
    | binaryExpression '^' binaryExpression
    | binaryExpression '|' binaryExpression
    | binaryExpression '&&' binaryExpression
    | binaryExpression '||' binaryExpression
    ;

castExpression
    : '(' typeName ')' castExpression
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
    | 'sizeof' ('(' typeName ')' | unaryExpression)
    ;

unaryOperator
    : '&' | '*' | '+' | '-' | '~' | '!'
    ;

postfixExpression
  : primaryExpression
    ('[' expression ']'
    | '(' argumentExpressionList? ')'
    | dataMemberAccess
    | '++'
    | '--'
    | '*'
    )* ;

dataMemberAccess
    : ('.' | '->') identifier
    ;

argumentExpressionList
    : expression (',' expression)*
    ; // Support variadic functions.

constant
    : ('+' | '-')? DECIMAL_LITERAL
    | HEX_LITERAL
    | OCTAL_LITERAL
    | CHARACTER_LITERAL
    | ('+' | '-')? FLOATING_POINT_LITERAL
    ;

stringLiteral
    : (L_STR | '@') STRING+
    | STRING+
    | QUOTE_STRING+
    ;

identifier
    : IDENTIFIER
    | L_STR
    | NULLABLE
    | WWEAK
    | TYPEOF | TYPEOF__ | TYPEOF____ | KINDOF__
    | ASSIGNPA | GETTER | NONATOMIC | SETTER | STRONG | RETAIN | READONLY | READWRITE | WEAK
    | SELF
    ;
