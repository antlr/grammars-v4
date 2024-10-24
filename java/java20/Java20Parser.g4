// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar Java20Parser;

options {
    tokenVocab = Java20Lexer;
}

//=============

start_
    : compilationUnit EOF
    ;

// Paragraph 3.8 Identifiers
// -------------

identifier
    : Identifier
    | contextualKeyword
    ;

typeIdentifier
    : Identifier
    | contextualKeywordMinusForTypeIdentifier
    ;

unqualifiedMethodIdentifier
    : Identifier
    | contextualKeywordMinusForUnqualifiedMethodIdentifier
    ;

// 3.9 Keywords

contextualKeyword
    : 'exports'
    | 'module'
    | 'non-sealed'
    | 'open'
    | 'opens'
    | 'permits'
    | 'provides'
    | 'record'
    | 'requires'
    | 'sealed'
    | 'to'
    | 'transitive'
    | 'uses'
    | 'var'
    | 'with'
    | 'yield'
    ;

contextualKeywordMinusForTypeIdentifier
    : 'exports'
    | 'module'
    | 'non-sealed'
    | 'open'
    | 'opens'
//  | 'permits'
    | 'provides'
//  | 'record'
    | 'requires'
//  | 'sealed'
    | 'to'
    | 'transitive'
    | 'uses'
//  | 'var'
    | 'with'
//  | 'yield'
    ;


contextualKeywordMinusForUnqualifiedMethodIdentifier
    : 'exports'
    | 'module'
    | 'non-sealed'
    | 'open'
    | 'opens'
    | 'permits'
    | 'provides'
    | 'record'
    | 'requires'
    | 'sealed'
    | 'to'
    | 'transitive'
    | 'uses'
    | 'var'
    | 'with'
//  | 'yield'
    ;

// Paragraph 3.10
// --------------

literal
    : IntegerLiteral
    | FloatingPointLiteral
    | BooleanLiteral
    | CharacterLiteral
    | StringLiteral
    | TextBlock
    | NullLiteral
    ;

// Paragraph 4.1 // Type is not used.
// Type    ::=      primitiveType
//          |       referenceType
//          ;

// Paragraph 4.2
// -------------

primitiveType
    : annotation* (numericType | 'boolean')
    ;

numericType
    : integralType
    | floatingPointType
    ;

integralType
    : 'byte'
    | 'short'
    | 'int'
    | 'long'
    | 'char'
    ;

floatingPointType
    : 'float'
    | 'double'
    ;

// Paragraph 4.3
// -------------

referenceType
    : classOrInterfaceType
    | typeVariable
    | arrayType
    ;

// replace classType in classOrInterfaceType

// classOrInterfaceType
//         : classType
//         | interfaceType
//         ;
//

// classOrInterfaceType
//         :                                      annotation* typeIdentifier typeArguments? coit
//         |             packageName          '.' annotation* typeIdentifier typeArguments? coit
//         |             classOrInterfaceType '.' annotation* typeIdentifier typeArguments?
//         | interfaceType coit
//         ;
//

coit
    : '.' annotation* typeIdentifier typeArguments? coit?
    ;

classOrInterfaceType
    : (packageName '.')? annotation* typeIdentifier typeArguments? coit?
    ;

classType
    : annotation* typeIdentifier typeArguments?
    | packageName '.' annotation* typeIdentifier typeArguments?
    | classOrInterfaceType '.' annotation* typeIdentifier typeArguments?
    ;

interfaceType
    : classType
    ;

typeVariable
    : annotation* typeIdentifier
    ;

arrayType
    : primitiveType dims
    | classType dims
    | typeVariable dims
    ;

dims
    : annotation* '[' ']' (annotation* '[' ']')*
    ;

// Paragraph 4.4
// -------------

typeParameter
    : typeParameterModifier* typeIdentifier typeBound?
    ;

typeParameterModifier
    : annotation
    ;

typeBound
    : 'extends' (typeVariable | classOrInterfaceType additionalBound*)
    ;

additionalBound
    : '&' interfaceType
    ;

// Paragraph 4.5.1
// ---------------

typeArguments
    : '<' typeArgumentList '>'
    ;

typeArgumentList
    : typeArgument (',' typeArgument)*
    ;

typeArgument
    : referenceType
    | wildcard
    ;

wildcard
    : annotation* '?' wildcardBounds?
    ;

wildcardBounds
    : 'extends' referenceType
    | 'super' referenceType
    ;

// Paragraph 6.5
// -------------

moduleName
    : identifier ('.' moduleName)?
    // left recursion --> right recursion
    ;

packageName
    : identifier ('.' packageName)?
    // left recursion --> right recursion
    ;

typeName
    : packageName ('.' typeIdentifier)?
    ;

packageOrTypeName
    : identifier ('.' packageOrTypeName)?
    // left recursion --> right recursion
    ;

expressionName
    : (ambiguousName '.')? identifier
    ;

methodName
    : unqualifiedMethodIdentifier
    ;

ambiguousName
    : identifier ('.' ambiguousName)?
    // left recursion --> right recursion
    ;

// Paragraph 7.3
// -------------

compilationUnit
    : ordinaryCompilationUnit
    | modularCompilationUnit
    ;

ordinaryCompilationUnit
    : packageDeclaration? importDeclaration* topLevelClassOrInterfaceDeclaration*
    ;

modularCompilationUnit
    : importDeclaration* moduleDeclaration
    ;

// Paragraph 7.4
// -------------

packageDeclaration
    : packageModifier* 'package' identifier ('.' identifier)* ';'
    ;

packageModifier
    : annotation
    ;

// Paragraph 7.5
// -------------

importDeclaration
    : singleTypeImportDeclaration
    | typeImportOnDemandDeclaration
    | singleStaticImportDeclaration
    | staticImportOnDemandDeclaration
    ;

singleTypeImportDeclaration
    : 'import' typeName ';'
    ;

typeImportOnDemandDeclaration
    : 'import' packageOrTypeName '.' '*' ';'
    ;

singleStaticImportDeclaration
    : 'import' 'static' typeName '.' identifier ';'
    ;

staticImportOnDemandDeclaration
    : 'import' 'static' typeName '.' '*' ';'
    ;

// Paragraph 7.6
// -------------

topLevelClassOrInterfaceDeclaration
    : classDeclaration
    | interfaceDeclaration
    | ';'
    ;

// Paragraph 7.7
// -------------

moduleDeclaration
    : annotation* 'open'? 'module' identifier ('.' identifier)* '{' moduleDirective* '}'
    ;

moduleDirective
    : 'requires' requiresModifier* moduleName ';'
    | 'exports' packageName ('to' moduleName ( ',' moduleName)*)? ';'
    | 'opens' packageName ('to' moduleName ( ',' moduleName)*)? ';'
    | 'uses' typeName ';'
    | 'provides' typeName 'with' typeName ( ',' typeName)* ';'
    ;

requiresModifier
    : 'transitive'
    | 'static'
    ;

// Paragraph 8.1
// -------------

classDeclaration
    : normalClassDeclaration
    | enumDeclaration
    | recordDeclaration
    ;

normalClassDeclaration
    : classModifier* 'class' typeIdentifier typeParameters? classExtends? classImplements? classPermits? classBody
    ;

classModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'final'
    | 'sealed'
    | 'non-sealed'
    | 'strictfp'
    ;

typeParameters
    : '<' typeParameterList '>'
    ;

typeParameterList
    : typeParameter (',' typeParameter)*
    ;

classExtends
    : 'extends' classType
    ;

classImplements
    : 'implements' interfaceTypeList
    ;

interfaceTypeList
    : interfaceType (',' interfaceType)*
    ;

classPermits
    : 'permits' typeName (',' typeName)*
    ;

classBody
    : '{' classBodyDeclaration* '}'
    ;

classBodyDeclaration
    : classMemberDeclaration
    | instanceInitializer
    | staticInitializer
    | constructorDeclaration
    ;

classMemberDeclaration
    : fieldDeclaration
    | methodDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

// Paragraph 8.3
// -------------

fieldDeclaration
    : fieldModifier* unannType variableDeclaratorList ';'
    ;

fieldModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'static'
    | 'final'
    | 'transient'
    | 'volatile'
    ;

variableDeclaratorList
    : variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    : identifier dims?
    ;

variableInitializer
    : expression
    | arrayInitializer
    ;

unannType
    : unannPrimitiveType
    | unannReferenceType
    ;

unannPrimitiveType
    : numericType
    | 'boolean'
    ;

unannReferenceType
    : unannClassOrInterfaceType
    | unannTypeVariable
    | unannArrayType
    ;

// Replace unannClassType in unannClassOrInterfaceType

// unannClassOrInterfaceType
//         : unannClassType
//         | unannInterfaceType
//         ;
//

unannClassOrInterfaceType
    : (packageName '.' annotation*)? typeIdentifier typeArguments? uCOIT?
    ;

uCOIT
    : '.' annotation* typeIdentifier typeArguments? uCOIT?
    ;

unannClassType
    : typeIdentifier typeArguments?
    | (packageName | unannClassOrInterfaceType) '.' annotation* typeIdentifier typeArguments?
    ;

unannInterfaceType
    : unannClassType
    ;

unannTypeVariable
    : typeIdentifier
    ;

unannArrayType
    : (unannPrimitiveType | unannClassOrInterfaceType | unannTypeVariable) dims
    ;

// Paragraph 8.4
// -------------

methodDeclaration
    : methodModifier* methodHeader methodBody
    ;

methodModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'final'
    | 'synchronized'
    | 'native'
    | 'strictfp'
    ;

methodHeader
    : (typeParameters annotation*)? result methodDeclarator throwsT?
    ;

result
    : unannType
    | 'void'
    ;

methodDeclarator
    : identifier '(' (receiverParameter ',')? formalParameterList? ')' dims?
    ;

receiverParameter
    : annotation* unannType (identifier '.')? 'this'
    ;

formalParameterList
    : formalParameter (',' formalParameter)*
    ;

formalParameter
    : variableModifier* unannType variableDeclaratorId
    | variableArityParameter
    ;

variableArityParameter
    : variableModifier* unannType annotation* '...' identifier
    ;

variableModifier
    : annotation
    | 'final'
    ;

throwsT
    : 'throws' exceptionTypeList
    ;

exceptionTypeList
    : exceptionType (',' exceptionType)*
    ;

exceptionType
    : classType
    | typeVariable
    ;

methodBody
    : block
    | ';'
    ;

// Paragraph 8.6
// -------------

instanceInitializer
    : block
    ;

// Paragraph 8.7
// -------------

staticInitializer
    : 'static' block
    ;

// Paragraph 8.8
// -------------

constructorDeclaration
    : constructorModifier* constructorDeclarator throwsT? constructorBody
    ;

constructorModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    ;

constructorDeclarator
    : typeParameters? simpleTypeName '(' (receiverParameter ',')? formalParameterList? ')'
    ;

simpleTypeName
    : typeIdentifier
    ;

constructorBody
    : '{' explicitConstructorInvocation? blockStatements? '}'
    ;

explicitConstructorInvocation
    : typeArguments? ('this' | 'super') '(' argumentList? ')' ';'
    | (expressionName | primary) '.' typeArguments? 'super' '(' argumentList? ')' ';'
    ;

// Paragraph 8.9
// -------------

enumDeclaration
    : classModifier* 'enum' typeIdentifier classImplements? enumBody
    ;

enumBody
    : '{' enumConstantList? ','? enumBodyDeclarations? '}'
    // It is not my  grammarmistake! It is based on //docs.oracle.com/javase/specs/jls/se20/jls20.pdf.
    // Notice, javac accepts "enum One { ,  }" and also "enum Two { , ; {} }"
    ;

enumConstantList
    : enumConstant (',' enumConstant)*
    ;

enumConstant
    : enumConstantModifier* identifier ('(' argumentList? ')')? classBody?
    ;

enumConstantModifier
    : annotation
    ;

enumBodyDeclarations
    : ';' classBodyDeclaration*
    ;

// Paragraph 8.10
// --------------

recordDeclaration
    : classModifier* 'record' typeIdentifier typeParameters? recordHeader classImplements? recordBody
    ;

recordHeader
    : '(' recordComponentList? ')'
    ;

recordComponentList
    : recordComponent (',' recordComponent)*
    ;

recordComponent
    : recordComponentModifier* unannType identifier
    | variableArityRecordComponent
    ;

variableArityRecordComponent
    : recordComponentModifier* unannType annotation* '...' identifier
    ;

recordComponentModifier
    : annotation
    ;

recordBody
    : '{' recordBodyDeclaration* '}'
    ;

recordBodyDeclaration
    : classBodyDeclaration
    | compactConstructorDeclaration
    ;

compactConstructorDeclaration
    : constructorModifier* simpleTypeName constructorBody
    ;

// Paragraph 9.1
// -------------

interfaceDeclaration
    : normalInterfaceDeclaration
    | annotationInterfaceDeclaration
    ;

normalInterfaceDeclaration
    : interfaceModifier* 'interface' typeIdentifier typeParameters? interfaceExtends? interfacePermits? interfaceBody
    ;

interfaceModifier
    : annotation
    | 'public'
    | 'protected'
    | 'private'
    | 'abstract'
    | 'static'
    | 'sealed'
    | 'non-sealed'
    | 'strictfp'
    ;

interfaceExtends
    : 'extends' interfaceTypeList
    ;

interfacePermits
    : 'permits' typeName (',' typeName)*
    ;

interfaceBody
    : '{' interfaceMemberDeclaration* '}'
    ;

interfaceMemberDeclaration
    : constantDeclaration
    | interfaceMethodDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

// Paragraph 9.3
// -------------

constantDeclaration
    : constantModifier* unannType variableDeclaratorList ';'
    ;

constantModifier
    : annotation
    | 'public'
    | 'static'
    | 'final'
    ;

// Paragraph 9.4
// -------------

interfaceMethodDeclaration
    : interfaceMethodModifier* methodHeader methodBody
    ;

interfaceMethodModifier
    : annotation
    | 'public'
    | 'private'
    | 'abstract'
    | 'default'
    | 'static'
    | 'strictfp'
    ;

// Paragraph 9.6
// -------------

annotationInterfaceDeclaration
    : interfaceModifier* '@' 'interface' typeIdentifier annotationInterfaceBody
    ;

annotationInterfaceBody
    : '{' annotationInterfaceMemberDeclaration* '}'
    ;

annotationInterfaceMemberDeclaration
    : annotationInterfaceElementDeclaration
    | constantDeclaration
    | classDeclaration
    | interfaceDeclaration
    | ';'
    ;

annotationInterfaceElementDeclaration
    : annotationInterfaceElementModifier* unannType identifier '(' ')' dims? defaultValue? ';'
    ;

annotationInterfaceElementModifier
    : annotation
    | 'public'
    | 'abstract'
    ;

defaultValue
    : 'default' elementValue
    ;

// Paragraph 9.7
// -------------

annotation
    : normalAnnotation
    | markerAnnotation
    | singleElementAnnotation
    ;

normalAnnotation
    : '@' typeName '(' elementValuePairList? ')'
    ;

elementValuePairList
    : elementValuePair (',' elementValuePair)*
    ;

elementValuePair
    : identifier '=' elementValue
    ;

elementValue
    : conditionalExpression
    | elementValueArrayInitializer
    | annotation
    ;

elementValueArrayInitializer
    : '{' elementValueList? ','? '}'
    ;

elementValueList
    : elementValue (',' elementValue)*
    ;

markerAnnotation
    : '@' typeName
    ;

singleElementAnnotation
    : '@' typeName '(' elementValue ')'
    ;

// Paragraph 10.6
// --------------

arrayInitializer
    : '{' variableInitializerList? ','? '}'
    // Strange  ','  ?! staat ook in antlr_java.g4
    ;

variableInitializerList
    : variableInitializer (',' variableInitializer)*
    ;

// Paragraph 14.2
// --------------

block
    : '{' blockStatements? '}'
    ;

blockStatements
    : blockStatement blockStatement*
    ;

blockStatement
    : localClassOrInterfaceDeclaration
    | localVariableDeclarationStatement
    | statement
    ;

// Paragraph 14.3
// --------------

localClassOrInterfaceDeclaration
    : classDeclaration
    | normalInterfaceDeclaration
    ;

// Paragraph 14.4
// --------------

localVariableDeclaration
    : variableModifier* localVariableType variableDeclaratorList?
    ;

localVariableType
    : unannType
    | 'var'
    ;

localVariableDeclarationStatement
    : localVariableDeclaration ';'
    ;

// Paragraph 14.5
// --------------

statement
    : statementWithoutTrailingSubstatement
    | labeledStatement
    | ifThenStatement
    | ifThenElseStatement
    | whileStatement
    | forStatement
    ;

statementNoShortIf
    : statementWithoutTrailingSubstatement
    | labeledStatementNoShortIf
    | ifThenElseStatementNoShortIf
    | whileStatementNoShortIf
    | forStatementNoShortIf
    ;

statementWithoutTrailingSubstatement
    : block
    | emptyStatement_
    | expressionStatement
    | assertStatement
    | switchStatement
    | doStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | synchronizedStatement
    | throwStatement
    | tryStatement
    | yieldStatement
    ;

// Paragraph 14.6
// --------------

emptyStatement_
    : ';'
    ;

// Paragraph 14.7
// --------------

labeledStatement
    : identifier ':' statement
    ;

labeledStatementNoShortIf
    : identifier ':' statementNoShortIf
    ;

// Paragraph 14.8
// --------------

expressionStatement
    : statementExpression ';'
    ;

statementExpression
    : assignment
    | preIncrementExpression
    | preDecrementExpression
    | postIncrementExpression
    | postDecrementExpression
    | methodInvocation
    | classInstanceCreationExpression
    ;

// Paragraph 14.9
// --------------

ifThenStatement
    : 'if' '(' expression ')' statement
    ;

ifThenElseStatement
    : 'if' '(' expression ')' statementNoShortIf 'else' statement
    ;

ifThenElseStatementNoShortIf
    : 'if' '(' expression ')' statementNoShortIf 'else' statementNoShortIf
    ;

// Paragraph 14.10
// ---------------

assertStatement
    : 'assert' expression (':' expression)? ';'
    ;

// Paragraph 14.11
// --------------

switchStatement
    : 'switch' '(' expression ')' switchBlock
    ;

switchBlock
    : '{' switchRule switchRule* '}'
    | '{' switchBlockStatementGroup* ( switchLabel ':')* '}'
    ;

switchRule
    : switchLabel '->' (expression ';' | block | throwStatement)
    ;

switchBlockStatementGroup
    : switchLabel ':' (switchLabel ':')* blockStatements
    ;

switchLabel
    : 'case' caseConstant (',' caseConstant)*
    | 'default'
    ;

caseConstant
    : conditionalExpression
    ;

// Paragraph 14.12
// ---------------

whileStatement
    : 'while' '(' expression ')' statement
    ;

whileStatementNoShortIf
    : 'while' '(' expression ')' statementNoShortIf
    ;

// Paragraph 14.13
// ---------------

doStatement
    : 'do' statement 'while' '(' expression ')' ';'
    ;

// Paragraph 14.14
// ---------------

forStatement
    : basicForStatement
    | enhancedForStatement
    ;

forStatementNoShortIf
    : basicForStatementNoShortIf
    | enhancedForStatementNoShortIf
    ;

basicForStatement
    : 'for' '(' forInit? ';' expression? ';' forUpdate? ')' statement
    ;

basicForStatementNoShortIf
    : 'for' '(' forInit? ';' expression? ';' forUpdate? ')' statementNoShortIf
    ;

forInit
    : statementExpressionList
    | localVariableDeclaration
    ;

forUpdate
    : statementExpressionList
    ;

statementExpressionList
    : statementExpression (',' statementExpression)*
    ;

enhancedForStatement
    : 'for' '(' localVariableDeclaration ':' expression ')' statement
    ;

enhancedForStatementNoShortIf
    : 'for' '(' localVariableDeclaration ':' expression ')' statementNoShortIf
    ;

// Paragraph 14.15
// ---------------

breakStatement
    : 'break' identifier? ';'
    ;

// Paragraph 14.16
// ---------------

continueStatement
    : 'continue' identifier? ';'
    ;

// Paragraph 14.17
// ---------------

returnStatement
    : 'return' expression? ';'
    ;

// Paragraph 14.18
// ---------------

throwStatement
    : 'throw' expression ';'
    ;

// Paragraph 14.19
// ---------------

synchronizedStatement
    : 'synchronized' '(' expression ')' block
    ;

// Paragraph 14.20
// ---------------

tryStatement
    : 'try' block catches
    | 'try' block finallyBlock
    | 'try' block catches? finallyBlock
    | tryWithResourcesStatement
    ;

catches
    : catchClause catchClause*
    ;

catchClause
    : 'catch' '(' catchFormalParameter ')' block
    ;

catchFormalParameter
    : variableModifier* catchType variableDeclaratorId
    ;

catchType
    : unannClassType ('|' classType)*
    ;

finallyBlock
    : 'finally' block
    ;

tryWithResourcesStatement
    : 'try' resourceSpecification block catches? finallyBlock?
    ;

resourceSpecification
    : '(' resourceList ';'? ')'
    ;

resourceList
    : resource (';' resource)*
    ;

resource
    : localVariableDeclaration
    | variableAccess
    ;

variableAccess
    : expressionName
    | fieldAccess
    ;

// Paragraph 14.21
//----------------

yieldStatement
    : 'yield' expression ';'
    ;

// Paragraph 14.30
// --------------

pattern
    : typePattern
    ;

typePattern
    : localVariableDeclaration
    ;

// Paragraph 15.2
// --------------

expression
    : lambdaExpression
    | assignmentExpression
    ;

// Paragraph 15.8
// --------------

primary
    : primaryNoNewArray
    | arrayCreationExpression
    ;

// Replace classInstanceCreationExpression, fieldAccess, arrayAccess, methodInvocation, and
// methodReference in primaryNoNewArray.
// Replace in these two rules primary by primaryNoNewArray.

// primaryNoNewArray
//         : literal
//         | classLiteral
//         | 'this'
//         | typeName '.' 'this'
//         | '(' expression ')'
//         | classInstanceCreationExpression
//         | fieldAccess
//         | arrayAccess
//         | methodInvocation
//         | methodReference
//         ;
//

// primaryNoNewArray
//         : literal
//         | classLiteral
//         | 'this'
//         | typeName '.' 'this'
//         | '(' expression ')'
//         |                                  unqualifiedClassInstanceCreationExpression
//         | expressionName              '.'  unqualifiedClassInstanceCreationExpression
//
//         | primaryNoNewArray           '.'  unqualifiedClassInstanceCreationExpression
//         | arrayCreationExpression     '.'  unqualifiedClassInstanceCreationExpression
//
//         | primaryNoNewArray           '.'  Identifier
//         | arrayCreationExpression     '.'  Identifier
//
//         | 'super'  '.'                     Identifier
//         | typeName '.' 'super'        '.'  Identifier
//
//         | expressionName                         '[' expression ']'
//         | primaryNoNewArray                      '[' expression ']'
//         | arrayCreationExpressionWithInitializer '[' expression ']'
//
//         | methodName                                                 '(' argumentList? ')'
//         | typeName                    '.'  typeArguments? Identifier '(' argumentList? ')'
//         | expressionName              '.'  typeArguments? Identifier '(' argumentList? ')'
//
//         | primaryNoNewArray           '.'  typeArguments? Identifier '(' argumentList? ')'
//         | arrayCreationExpression     '.'  typeArguments? Identifier '(' argumentList? ')'
//
//         | 'super'                     '.'  typeArguments? Identifier '(' argumentList? ')'
//         | typeName       '.' 'super'  '.'  typeArguments? Identifier '(' argumentList? ')'
//
//         | expressionName              '::' typeArguments? Identifier
//
//         | primaryNoNewArray           '::' typeArguments? Identifier
//         | arrayCreationExpression     '::' typeArguments? Identifier
//
//
//         | referenceType               '::' typeArguments? Identifier
//         | 'super'                     '::' typeArguments? Identifier
//         | typeName '.' 'super'        '::' typeArguments? Identifier
//         | classType                   '::' typeArguments? 'new'
//         | arrayType                   '::'                'new'
//         ;
//

primaryNoNewArray
    : literal pNNA?
    | classLiteral pNNA?
    | 'this' pNNA?
    | typeName '.' 'this' pNNA?
    | '(' expression ')' pNNA?
    | unqualifiedClassInstanceCreationExpression pNNA?
    | expressionName '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' unqualifiedClassInstanceCreationExpression pNNA?
    | arrayCreationExpression '.' identifier pNNA?
    | 'super' '.' identifier pNNA?
    | typeName '.' 'super' '.' identifier pNNA?
    | expressionName '[' expression ']' pNNA?
    | arrayCreationExpressionWithInitializer '[' expression ']' pNNA?
    | methodName '(' argumentList? ')' pNNA?
    | typeName '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | expressionName '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | arrayCreationExpression '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | typeName '.' 'super' '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | expressionName '::' typeArguments? identifier pNNA?
    | arrayCreationExpression '::' typeArguments? identifier pNNA?
    | referenceType '::' typeArguments? identifier pNNA?
    | 'super' '::' typeArguments? identifier pNNA?
    | typeName '.' 'super' '::' typeArguments? identifier pNNA?
    | classType '::' typeArguments? 'new' pNNA?
    | arrayType '::' 'new' pNNA?
    ;

pNNA
    : '.' unqualifiedClassInstanceCreationExpression pNNA?
    | '.' identifier pNNA?
    | '[' expression ']' pNNA?
    | '.' typeArguments? identifier '(' argumentList? ')' pNNA?
    | '::' typeArguments? identifier pNNA?
    ;

classLiteral
    : typeName ('[' ']')* '.' 'class'
    | numericType ( '[' ']')* '.' 'class'
    | 'boolean' ( '[' ']')* '.' 'class'
    | 'void' '.' 'class'
    ;

// Paragraph 15.9
// --------------

classInstanceCreationExpression
    : unqualifiedClassInstanceCreationExpression
    | expressionName '.' unqualifiedClassInstanceCreationExpression
    | primary '.' unqualifiedClassInstanceCreationExpression
    ;

unqualifiedClassInstanceCreationExpression
    : 'new' typeArguments? classOrInterfaceTypeToInstantiate '(' argumentList? ')' classBody?
    ;

classOrInterfaceTypeToInstantiate
    : annotation* identifier ('.' annotation* identifier)* typeArgumentsOrDiamond?
    ;

typeArgumentsOrDiamond
    : typeArguments
    | '<>'
    ;

// Paragraph 15.10
// ---------------

arrayCreationExpression
    : arrayCreationExpressionWithoutInitializer
    | arrayCreationExpressionWithInitializer
    ;

arrayCreationExpressionWithoutInitializer
    : 'new' primitiveType dimExprs dims?
    | 'new' classType dimExprs dims?
    ;

arrayCreationExpressionWithInitializer
    : 'new' primitiveType dims arrayInitializer
    | 'new' classOrInterfaceType dims arrayInitializer
    ;

dimExprs
    : dimExpr dimExpr*
    ;

dimExpr
    : annotation* '[' expression ']'
    ;

arrayAccess
    : expressionName '[' expression ']'
    | primaryNoNewArray '[' expression ']'
    | arrayCreationExpressionWithInitializer '[' expression ']'
    ;

// Paragraph 15.11
// ---------------

fieldAccess
    : primary '.' identifier
    | 'super' '.' identifier
    | typeName '.' 'super' '.' identifier
    ;

// Paragraph 15.12
// ---------------

methodInvocation
    : methodName '(' argumentList? ')'
    | typeName '.' typeArguments? identifier '(' argumentList? ')'
    | expressionName '.' typeArguments? identifier '(' argumentList? ')'
    | primary '.' typeArguments? identifier '(' argumentList? ')'
    | 'super' '.' typeArguments? identifier '(' argumentList? ')'
    | typeName '.' 'super' '.' typeArguments? identifier '(' argumentList? ')'
    ;

argumentList
    : expression (',' expression)*
    ;

// Paragraph 15.13
// ---------------

methodReference
    : expressionName '::' typeArguments? identifier
    | primary '::' typeArguments? identifier
    | referenceType '::' typeArguments? identifier
    | 'super' '::' typeArguments? identifier
    | typeName '.' 'super' '::' typeArguments? identifier
    | classType '::' typeArguments? 'new'
    | arrayType '::' 'new'
    ;

// Paragraph 15.14
// ---------------

// Replace postIncrementExpression and postDecrementExpression by postfixExpression.

// postfixExpression
//         : primary
//         | expressionName
//         | postIncrementExpression
//         | postDecrementExpression
//         ;
//

// postfixExpression
//         : primary
//         | expressionName
//         | postfixExpression '++'
//         | postfixExpression '--'
//         ;
//

postfixExpression
    : primary pfE?
    | expressionName pfE?
    ;

pfE
    : '++' pfE?
    | '--' pfE?
    ;

postIncrementExpression
    : postfixExpression '++'
    ;

postDecrementExpression
    : postfixExpression '--'
    ;

// Paragraph 15.15
// ---------------

unaryExpression
    : preIncrementExpression
    | preDecrementExpression
    | '+' unaryExpression
    | '-' unaryExpression
    | unaryExpressionNotPlusMinus
    ;

preIncrementExpression
    : '++' unaryExpression
    ;

preDecrementExpression
    : '--' unaryExpression
    ;

unaryExpressionNotPlusMinus
    : postfixExpression
    | '~' unaryExpression
    | '!' unaryExpression
    | castExpression
    | switchExpression
    ;

// Paragraph 15.16
// ---------------

castExpression
    : '(' primitiveType ')' unaryExpression
    | '(' referenceType additionalBound* ')' unaryExpressionNotPlusMinus
    | '(' referenceType additionalBound* ')' lambdaExpression
    ;

// Paragraph 15.17
// ---------------

multiplicativeExpression
    : unaryExpression
    | multiplicativeExpression '*' unaryExpression
    | multiplicativeExpression '/' unaryExpression
    | multiplicativeExpression '%' unaryExpression
    ;

// Paragraph 15.18
// ---------------

additiveExpression
    : multiplicativeExpression
    | additiveExpression '+' multiplicativeExpression
    | additiveExpression '-' multiplicativeExpression
    ;

// Paragraph 15.19
// ---------------

shiftExpression
    : additiveExpression
    | shiftExpression '<' '<' additiveExpression
    | shiftExpression '>' '>' additiveExpression
    | shiftExpression '>' '>' '>' additiveExpression
    ;

// Paragraph 15.20
// ---------------

relationalExpression
    : shiftExpression
    | relationalExpression '<' shiftExpression
    | relationalExpression '>' shiftExpression
    | relationalExpression '<=' shiftExpression
    | relationalExpression '>=' shiftExpression
    //      | instanceofExpression
    | relationalExpression 'instanceof' (referenceType | pattern)
    // Solves left recursion with instanceofExpression.
    ;

// instanceofExpression
//        : relationalExpression 'instanceof' (referenceType | pattern)
//        ;
// Resulted to left recursion with relationalExpression.

// Paragraph 15.21
// ---------------

equalityExpression
    : relationalExpression
    | equalityExpression '==' relationalExpression
    | equalityExpression '!=' relationalExpression
    ;

// Paragraph 15.22
// ---------------

andExpression
    : equalityExpression
    | andExpression '&' equalityExpression
    ;

exclusiveOrExpression
    : andExpression
    | exclusiveOrExpression '^' andExpression
    ;

inclusiveOrExpression
    : exclusiveOrExpression
    | inclusiveOrExpression '|' exclusiveOrExpression
    ;

// Paragraph 15.23
// ---------------

conditionalAndExpression
    : inclusiveOrExpression
    | conditionalAndExpression '&&' inclusiveOrExpression
    ;

// Paragraph 15.24
// ---------------

conditionalOrExpression
    : conditionalAndExpression
    | conditionalOrExpression '||' conditionalAndExpression
    ;

// Paragraph 15.25
// ---------------

conditionalExpression
    : conditionalOrExpression
    | conditionalOrExpression '?' expression ':' conditionalExpression
    | conditionalOrExpression '?' expression ':' lambdaExpression
    ;

// Paragraph 15.26
// ---------------

assignmentExpression
    : conditionalExpression
    | assignment
    ;

assignment
    : leftHandSide assignmentOperator expression
    ;

leftHandSide
    : expressionName
    | fieldAccess
    | arrayAccess
    ;

assignmentOperator
    : '='
    | '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    ;

// Paragraph 15.27
// ---------------

lambdaExpression
    : lambdaParameters '->' lambdaBody
    ;

lambdaParameters
    : '(' lambdaParameterList? ')'
    | identifier
    ;

lambdaParameterList
    : lambdaParameter (',' lambdaParameter)*
    | identifier ( ',' identifier)*
    ;

lambdaParameter
    : variableModifier* lambdaParameterType variableDeclaratorId
    | variableArityParameter
    ;

lambdaParameterType
    : unannType
    | 'var'
    ;

lambdaBody
    : expression
    | block
    ;

// Paragraph 15.28
// ---------------

switchExpression
    : 'switch' '(' expression ')' switchBlock
    ;

// Paragraph 15.29
// ---------------

constantExpression
    : expression
    ;
