/*******************************************************************************
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Camilo Sanchez (Camiloasc1) 2020 Martin Mirchev (Marti2203)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ****************************************************************************
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar CPP20Parser;

options {
    tokenVocab = CPP20Lexer;
}

// Insert here @header for C++ parser.


/* A.3 Lexical conventions */

/*
preprocessingToken
	: headerName
	| Import
	| Module
	| Export
	| Identifier
	| CharacterLiteral
	| UserDefinedCharacterLiteral
	| StringLiteral
	| UserDefinedStringLiteral
	| preprocessingOpOrPunc
	// each non-white-space character that cannot be one of the above
	;

preprocessingOpOrPunc
	: preprocessingOperator
	| operatorOrPunctuator
	;

preprocessingOperator
	: Sharp+
	| (Mod Colon)+
	;

operatorOrPunctuator
	: LeftBrace
	| RightBrace
	| LeftBracket
	| RightBracket
	| LeftParen
	| RightParen
	| Less Colon
	| Colon Greater
	| Less Mod
	| Mod Greater
	| Semi
	| Colon
	| Ellipsis
	| Question
	| Doublecolon
	| Dot
	| DotStar
	| Arrow
	| ArrowStar
	| Tilde
	| Not
	| Plus
	| Minus
	| Star
	| Div
	| Mod
	| Caret
	| And
	| Or
	| Assign
	| PlusAssign
	| MinusAssign
	| StarAssign
	| DivAssign
	| ModAssign
	| XorAssign
	| AndAssign
	| OrAssign
	| Equal
	| NotEqual
	| Less
	| Greater
	| LessEqual
	| GreaterEqual
	| LessEqualGreater
	| AndAnd
	| OrOr
	| Less Less
	| Greater Greater
	| LeftShiftAssign
	| RightShiftAssign
	| PlusPlus
	| MinusMinus
	| Comma
	;
*/

headerName
	: Less hCharSequence Greater
	| Quotes qCharSequence Quotes
	;

hCharSequence
	: hChar+
	;

hChar
	: ~ (Newline | Greater) 
	// any member of the source character set except new-line and >
	;

qCharSequence
	: qChar+
	;

qChar
	:  ~ (Newline | '"') 
	// any member of the source character set except new-line and "
	;

literal
    : IntegerLiteral
    | CharacterLiteral
    | FloatingPointLiteral
    | StringLiteral
    | BooleanLiteral
    | PointerLiteral
    | UserDefinedLiteral
    ;


/* A.4 Basics */

translationUnit
	: declarationseq? /* EOF */
	| globalModuleFragment? moduleDeclaration declarationseq? privateModuleFragment?
	;


/* A.5 Expressions */

primaryExpression
    : literal
    | This
    | LeftParen expression RightParen
    | idExpression
    | lambdaExpression
	| foldExpression
	| requiresExpression
    ;

idExpression
    : unqualifiedId
    | qualifiedId
	| typeSpecifier
    ;

unqualifiedId
    : Identifier
    | operatorFunctionId
    | conversionFunctionId
    | literalOperatorId
    | Tilde (theTypeName | decltypeSpecifier)
    | templateId
    ;

qualifiedId
    : nestedNameSpecifier Template? unqualifiedId
    ;

nestedNameSpecifier
	: (theTypeName | namespaceName | decltypeSpecifier)? Doublecolon
    | nestedNameSpecifier ( Identifier | Template? simpleTemplateId) Doublecolon
    ;

lambdaExpression
    : lambdaIntroducer (Less templateparameterList Greater requiresClause?)? lambdaDeclarator? compoundStatement
    ;

lambdaIntroducer
    : LeftBracket lambdaCapture? RightBracket
    ;

lambdaDeclarator
    : LeftParen parameterDeclarationClause RightParen declSpecifierSeq? noexceptSpecifier? attributeSpecifierSeq? trailingReturnType? requiresClause?
    ;

lambdaCapture
    : captureList
    | captureDefault (Comma captureList)?
    ;

captureDefault
    : And
    | Assign
    ;

captureList
	: capture (Comma capture)*
    ;

capture
    : simpleCapture
    | initcapture
    ;

simpleCapture
    : And? Identifier Ellipsis?
    | Star? This
    ;

initcapture
    : And? Ellipsis? Identifier initializer
    ;

foldExpression
	: LeftParen (castExpression foldOperator)? Ellipsis (foldOperator castExpression)? RightParen
	;

foldOperator
    : Plus
	| Minus
	| Star
	| Div
	| Mod
	| Caret
	| And
	| Or
	| Less Less
	| Greater Greater
	| assignmentOperator
	| Equal
	| NotEqual
	| Less
	| Greater
	| LessEqual
	| GreaterEqual
	| AndAnd
	| OrOr
	| Comma
	| DotStar
	| ArrowStar
    ;

requiresExpression
	: Requires requirementParameterList? requirementBody
	;

requirementParameterList
	: LeftParen parameterDeclarationClause RightParen
	| LeftParen RightParen
	;

requirementBody
	: LeftBrace requirementSeq RightBrace
	;

requirementSeq
	: requirement+
	;

requirement
	: simpleRequirement
	| typeRequirement
	| compoundRequirement
	| nestedRequirement
	;

simpleRequirement
	: expression Semi
	;

typeRequirement
	: Typename_ nestedNameSpecifier? theTypeName Semi
	;
	
compoundRequirement
	: LeftBrace expression RightBrace Noexcept? returnTypeRequirement? Semi
	;
	
returnTypeRequirement
	: Arrow typeConstraint
	;

nestedRequirement
	: Requires constraintExpression Semi
	;

postfixExpression
    : primaryExpression
	| postfixExpression ( (LeftBracket exprOrBracedInitList RightBracket | LeftParen expressionList? RightParen) |
	                      (PlusPlus | MinusMinus | (Dot | Arrow) Template? idExpression) )
	| (simpleTypeSpecifier | typeNameSpecifier) (LeftParen expressionList? RightParen | bracedInitList)
	| (Dynamic_cast | Static_cast | Reinterpret_cast | Const_cast) Less theTypeId Greater LeftParen expression RightParen
	| Typeid_ LeftParen (expression | theTypeId) RightParen
    ;

expressionList
    : initializerList
    ;

unaryExpression
    : postfixExpression
	| (unaryOperator | PlusPlus | MinusMinus) castExpression
	| awaitExpression
    | Sizeof (unaryExpression | LeftParen theTypeId RightParen | Ellipsis LeftParen Identifier RightParen)
	| Alignof LeftParen theTypeId RightParen
    | noExceptExpression
    | newExpression_
    | deleteExpression
    ;

unaryOperator
    : Star
	| And
	| Plus
	| Minus
	| Not
    | Tilde
    ;

awaitExpression
	: Co_await castExpression
	;

noExceptExpression
    : Noexcept LeftParen expression RightParen
    ;

newExpression_
    : Doublecolon? New newPlacement? (newTypeId | LeftParen theTypeId RightParen) newInitializer_?
    ;

newPlacement
    : LeftParen expressionList RightParen
    ;

newTypeId
    : typeSpecifierSeq newDeclarator_?
    ;

newDeclarator_
    : pointerOperator newDeclarator_?
    | noPointerNewDeclarator
    ;

noPointerNewDeclarator
    : LeftBracket expression? RightBracket attributeSpecifierSeq?
    | noPointerNewDeclarator LeftBracket constantExpression RightBracket attributeSpecifierSeq?
    ;

newInitializer_
    : LeftParen expressionList? RightParen
    | bracedInitList
    ;

deleteExpression
    : Doublecolon? Delete (LeftBracket RightBracket)? castExpression
    ;

castExpression
    : unaryExpression
    | LeftParen theTypeId RightParen castExpression
    ;

pointerMemberExpression
	: castExpression ((DotStar | ArrowStar) castExpression)*
    ;

multiplicativeExpression
    : pointerMemberExpression
	| multiplicativeExpression (Star | Div | Mod) pointerMemberExpression
    ;

additiveExpression
    : multiplicativeExpression
	| additiveExpression (Plus | Minus) multiplicativeExpression
    ;

shiftExpression
    : additiveExpression 
	| shiftExpression (Less Less | Greater Greater) additiveExpression
    ;

compareExpression
	: shiftExpression
	| compareExpression LessEqualGreater shiftExpression
	;

relationalExpression
    : compareExpression
	| relationalExpression (Less | Greater | LessEqual | GreaterEqual) compareExpression
    ;

equalityExpression
    : relationalExpression 
	| equalityExpression (Equal | NotEqual) relationalExpression
    ;

andExpression
    : equalityExpression
	| andExpression And equalityExpression
    ;

exclusiveOrExpression
    : andExpression
	| exclusiveOrExpression Caret andExpression
    ;

inclusiveOrExpression
    : exclusiveOrExpression
	| inclusiveOrExpression Or exclusiveOrExpression
    ;

logicalAndExpression
    : inclusiveOrExpression 
	| logicalAndExpression AndAnd inclusiveOrExpression
    ;

logicalOrExpression
    : logicalAndExpression
	| logicalOrExpression OrOr logicalAndExpression
    ;

conditionalExpression
    : logicalOrExpression (Question expression Colon assignmentExpression)?
    ;

yieldExpression
	: Co_yeild (assignmentExpression | bracedInitList)
	;

throwExpression
    : Throw assignmentExpression?
    ;

assignmentExpression
    : conditionalExpression
	| yieldExpression	
	| throwExpression
    | logicalOrExpression assignmentOperator initializerClause
    ;

assignmentOperator
    : Assign
    | StarAssign
    | DivAssign
    | ModAssign
    | PlusAssign
    | MinusAssign
    | RightShiftAssign
    | LeftShiftAssign
    | AndAssign
    | XorAssign
    | OrAssign
    ;

expression
    : assignmentExpression
	| expression Comma assignmentExpression
    ;

constantExpression
    : conditionalExpression
    ;


/* A.6 Statements */

statement
    : labeledStatement
    | attributeSpecifierSeq? (expressionStatement | compoundStatement | selectionStatement | iterationStatement | jumpStatement | tryBlock)
	| declarationStatement
    ;

forInitStatement
    : expressionStatement
    | simpleDeclaration
    ;

condition
    : expression
	| attributeSpecifierSeq? declSpecifierSeq declarator braceOrEqualInitializer
    ;

labeledStatement
    : attributeSpecifierSeq? (Identifier | Case constantExpression | Default) Colon statement
    ;

expressionStatement
    : expression? Semi
    ;

compoundStatement
    : LeftBrace statementSeq? RightBrace
    ;

statementSeq
	: statement+
    ;

selectionStatement
    : If Constexpr? LeftParen forInitStatement? condition RightParen statement (Else statement)?
    | Switch LeftParen forInitStatement? condition RightParen statement
    ;

iterationStatement
    : While LeftParen condition RightParen statement
    | Do statement While LeftParen expression RightParen Semi
	| For LeftParen forInitStatement? (condition? Semi expression? | forRangeDeclaration Colon forRangeInitializer) RightParen statement
    ;

forRangeDeclaration
    : attributeSpecifierSeq? declSpecifierSeq (declarator | refqualifier? LeftBracket identifierList RightBracket)
    ;

forRangeInitializer
    : exprOrBracedInitList
    ;

jumpStatement
    : (Break | Continue | Return exprOrBracedInitList? | Goto Identifier) Semi
	| coroutineReturnStatement
    ;

coroutineReturnStatement
	: Co_return exprOrBracedInitList? Semi
	;

declarationStatement
    : blockDeclaration
    ;


/* A.7 Declarations */

declarationseq
	: declaration+
    ;

declaration
    : blockDeclaration
	| nodeclspecFunctionDeclaration
    | functionDefinition
    | templateDeclaration
	| deductionGuide	
    | explicitInstantiation	
    | explicitSpecialization
	| exportDeclaration
    | linkageSpecification
    | namespaceDefinition
    | emptyDeclaration_
    | attributeDeclaration
	| moduleImportDeclaration
    ;

blockDeclaration
    : simpleDeclaration
    | asmDeclaration
    | namespaceAliasDefinition
    | usingDeclaration
	| usingEnumDeclaration
    | usingDirective
    | staticAssertDeclaration
    | aliasDeclaration
    | opaqueEnumDeclaration
    ;

nodeclspecFunctionDeclaration
	: attributeSpecifierSeq? declarator Semi
	;

aliasDeclaration
    : Using Identifier attributeSpecifierSeq? Assign definingTypeId Semi
    ;

simpleDeclaration
    : declSpecifierSeq initDeclaratorList? Semi
	| attributeSpecifierSeq? declSpecifierSeq (initDeclaratorList | refqualifier? LeftBracket identifierList RightBracket initializer) Semi
    ;

staticAssertDeclaration
	: Static_assert LeftParen constantExpression (Comma StringLiteral)? RightParen Semi
    ;

emptyDeclaration_
    : Semi
    ;

attributeDeclaration
    : attributeSpecifierSeq Semi
    ;

declSpecifier
    : storageClassSpecifier
    | definingTypeSpecifier
    | functionSpecifier
    | Friend
    | Typedef
    | Constexpr
	| Consteval
	| Constinit
	| Inline
    ;

declSpecifierSeq
	: declSpecifier (attributeSpecifierSeq? | declSpecifierSeq)
    ;

storageClassSpecifier
    : Static
    | Thread_local
    | Extern
    | Mutable
    ;

functionSpecifier
    : Virtual
    | explicitSpecifier
    ;

explicitSpecifier
	: Explicit (LeftParen constantExpression RightParen)?
	;

typedefName
    : Identifier
	| simpleTemplateId
    ;

typeSpecifier
    : simpleTypeSpecifier
	| elaboratedTypeSpecifier
	| typeNameSpecifier
	| cvQualifier
    ;

typeSpecifierSeq
	: typeSpecifier+ attributeSpecifierSeq?
    ;

definingTypeSpecifier
	: typeSpecifier
	| classSpecifier
	| enumSpecifier
	;

definingTypeSpecifierSeq
	: definingTypeSpecifier (attributeSpecifierSeq? | definingTypeSpecifierSeq)
	;

simpleTypeSpecifier
	: nestedNameSpecifier? (theTypeName | templateName | Template simpleTemplateId)
	| decltypeSpecifier
	| placeholderTypeSpecifier
	| Char
    | Char8
    | Char16
    | Char32
    | Wchar
    | Bool
    | Short
    | Int
    | Long
    | Signed
    | Unsigned	
    | Float
    | Double
    | Void
    ;

theTypeName
    : className
    | enumName
    | typedefName
    ;

elaboratedTypeSpecifier
	: classKey (attributeSpecifierSeq? nestedNameSpecifier? Identifier | simpleTemplateId | nestedNameSpecifier Template? simpleTemplateId)
	| elaboratedEnumSpecifier
	;
	
elaboratedEnumSpecifier
	: Enum nestedNameSpecifier? Identifier
	;

decltypeSpecifier
    : Decltype LeftParen expression RightParen
    ;

placeholderTypeSpecifier
	: typeConstraint? (Auto | Decltype LeftParen Auto RightParen)
	;

initDeclaratorList
	: initDeclarator (Comma initDeclarator)*
    ;

initDeclarator
	: declarator (initializer? | requiresClause)
	;

declarator
    : pointerDeclarator
    | noPointerDeclarator parametersAndQualifiers trailingReturnType
    ;

pointerDeclarator
    : noPointerDeclarator
	| pointerOperator pointerDeclarator
    ;

noPointerDeclarator
    : declaratorid attributeSpecifierSeq?
	| noPointerDeclarator (parametersAndQualifiers | LeftBracket constantExpression? RightBracket attributeSpecifierSeq?)
    | LeftParen pointerDeclarator RightParen
    ;

parametersAndQualifiers
    : LeftParen parameterDeclarationClause RightParen cvqualifierseq? refqualifier? noexceptSpecifier? attributeSpecifierSeq?
    ;

trailingReturnType
    : theTypeId
    ;

pointerOperator
	: nestedNameSpecifier? (Star | And | AndAnd) attributeSpecifierSeq? cvqualifierseq?
    ;

cvqualifierseq
    : cvQualifier cvqualifierseq?
    ;

cvQualifier
    : Const
    | Volatile
    ;

refqualifier
    : And
    | AndAnd
    ;

declaratorid
    : Ellipsis? idExpression
    ;

theTypeId
    : typeSpecifierSeq abstractDeclarator?
    ;

definingTypeId
	: definingTypeSpecifierSeq abstractDeclarator?
	;

abstractDeclarator
    : pointerAbstractDeclarator
    | noPointerAbstractDeclarator? parametersAndQualifiers trailingReturnType
    | abstractPackDeclarator
    ;

pointerAbstractDeclarator
    : noPointerAbstractDeclarator
    | pointerOperator pointerAbstractDeclarator?
    ;

noPointerAbstractDeclarator
    : noPointerAbstractDeclarator (parametersAndQualifiers | LeftBracket constantExpression? RightBracket attributeSpecifierSeq?)
    | LeftParen pointerAbstractDeclarator RightParen
    ;

abstractPackDeclarator
    : noPointerAbstractPackDeclarator
	| pointerOperator abstractPackDeclarator
    ;

noPointerAbstractPackDeclarator
	: noPointerAbstractPackDeclarator (parametersAndQualifiers | LeftBracket constantExpression? RightBracket attributeSpecifierSeq?)
    | Ellipsis
    ;

parameterDeclarationClause
    : parameterDeclarationList? Comma? Ellipsis?
    ;

parameterDeclarationList
	: parameterDeclaration (Comma parameterDeclaration)*
    ;

parameterDeclaration
    : attributeSpecifierSeq? declSpecifierSeq (declarator | abstractDeclarator?) (Assign initializerClause)?
    ;

initializer
    : braceOrEqualInitializer
    | LeftParen expressionList RightParen
    ;

braceOrEqualInitializer
    : Assign initializerClause
    | bracedInitList
    ;

initializerClause
    : assignmentExpression
    | bracedInitList
    ;

bracedInitList
    : LeftBrace (initializerList | designatedInitializerList) Comma? RightBrace
    | LeftBrace RightBrace
    ;

initializerList
	: initializerClause Ellipsis? (Comma initializerClause Ellipsis?)*
    ;

designatedInitializerList
    : designatedInitializerClause (Comma designatedInitializerClause)*
	;

designatedInitializerClause
	: designator braceOrEqualInitializer
	;

designator
	: Dot Identifier
	;

exprOrBracedInitList
	: expression
	| bracedInitList
	;

functionDefinition
	: attributeSpecifierSeq? declSpecifierSeq? declarator (virtualSpecifierSeq? | requiresClause) functionBody
    ;

functionBody
    : constructorInitializer? compoundStatement
    | functionTryBlock
    | Assign (Default | Delete) Semi
    ;

enumName
    : Identifier
    ;

enumSpecifier
    : enumHead LeftBrace (enumeratorList Comma?)? RightBrace
    ;

enumHead
    : enumkey attributeSpecifierSeq? enumHeadName? enumbase?
    ;

enumHeadName
	: nestedNameSpecifier? Identifier
	;

opaqueEnumDeclaration
    : enumkey attributeSpecifierSeq? enumHeadName enumbase? Semi
    ;

enumkey
    : Enum (Class | Struct)?
    ;

enumbase
    : Colon typeSpecifierSeq
    ;

enumeratorList
    : enumeratorDefinition
	| enumeratorList Comma enumeratorDefinition
    ;

enumeratorDefinition
    : enumerator (Assign constantExpression)?
    ;

enumerator
    : Identifier attributeSpecifierSeq?
    ;

usingEnumDeclaration
	: Using elaboratedEnumSpecifier Semi
	;

namespaceName
    : Identifier
    | namespaceAlias
    ;

namespaceDefinition
	: namedNamespaceDefinition
	| unnamedNamespaceDefinition
	| nestedNamespaceDefinition
	;

namedNamespaceDefinition
    : Inline? Namespace attributeSpecifierSeq? Identifier LeftBrace namespaceBody RightBrace
    ;

unnamedNamespaceDefinition
    : Inline? Namespace attributeSpecifierSeq? LeftBrace namespaceBody RightBrace
    ;

nestedNamespaceDefinition
	: Namespace enclosingNamespaceSpecifier Doublecolon Inline? Identifier LeftBrace namespaceBody RightBrace
	;

enclosingNamespaceSpecifier
	: Identifier
	| enclosingNamespaceSpecifier Doublecolon Inline? Identifier
	;

namespaceBody
	: declarationseq?
	;

namespaceAlias
    : Identifier
    ;

namespaceAliasDefinition
    : Namespace Identifier Assign qualifiednamespacespecifier Semi
    ;

qualifiednamespacespecifier
    : nestedNameSpecifier? namespaceName
    ;

usingDirective
    : attributeSpecifierSeq? Using Namespace nestedNameSpecifier? namespaceName Semi
    ;

usingDeclaration
    : Using usingDeclaratorList Semi
    ;

usingDeclaratorList
	: usingDeclarator Ellipsis?
	| usingDeclaratorList Comma usingDeclarator Ellipsis?
	;

usingDeclarator
	: Typename_? nestedNameSpecifier unqualifiedId
	;

asmDeclaration
    : attributeSpecifierSeq? Asm LeftParen StringLiteral RightParen Semi
    ;

linkageSpecification
    : Extern StringLiteral (LeftBrace declarationseq? RightBrace | declaration)
    ;

attributeSpecifierSeq
    : attributeSpecifier+
    ;

attributeSpecifier
    : LeftBracket LeftBracket attributeUsingPrefix? attributeList RightBracket RightBracket
    | alignmentspecifier
    ;

alignmentspecifier
    : Alignas LeftParen (theTypeId | constantExpression) Ellipsis? RightParen
    ;

attributeUsingPrefix
	: Using attributeNamespace Colon
	;

attributeList
    : attribute?
	| attributeList Comma attribute?
	| attribute Ellipsis
	| attributeList Comma attribute Ellipsis
    ;

attribute
    : attributeToken attributeArgumentClause?
    ;

attributeToken
	: Identifier
	| attributeScopedToken
	;

attributeScopedToken
	: attributeNamespace Doublecolon Identifier
	;
	
attributeNamespace
    : Identifier
    ;

attributeArgumentClause
    : LeftParen balancedTokenSeq? RightParen
    ;

balancedTokenSeq
    : balancedtoken
	| balancedTokenSeq balancedtoken
    ;

balancedtoken
    : LeftParen balancedTokenSeq RightParen
    | LeftBracket balancedTokenSeq RightBracket
    | LeftBrace balancedTokenSeq RightBrace
    | ~(LeftParen | RightParen | LeftBrace | RightBrace | LeftBracket | RightBracket)+
	// any token other than a parenthesis, a bracket, or a brace
    ;


/* A.8 Modules */

moduleDeclaration
	: Export? Module moduleName modulePartition? attributeSpecifierSeq? Semi
	;

moduleName
	: moduleNameQualifier? Identifier
	;

modulePartition
	: Colon moduleNameQualifier? Identifier
	;

moduleNameQualifier
	: Identifier Dot
	| moduleNameQualifier Identifier Dot
	;

exportDeclaration
	: Export (declaration | LeftBrace declarationseq? RightBrace | moduleImportDeclaration)
	;

moduleImportDeclaration
	: Import (moduleName | modulePartition | headerName) attributeSpecifierSeq? Semi
	;

globalModuleFragment
	: Module Semi declarationseq?
	;

privateModuleFragment
	: Module Colon Private Semi declarationseq?
	;


/* A.9 Classes */

className
    : Identifier
    | simpleTemplateId
    ;

classSpecifier
    : classHead LeftBrace memberSpecification? RightBrace
    ;

classHead
    : classKey attributeSpecifierSeq? (classHeadName classVirtSpecifier?)? baseClause?
    ;

classHeadName
    : nestedNameSpecifier? className
    ;

classVirtSpecifier
    : Final
    ;

classKey
    : Class
    | Struct
	| Union
    ;

memberSpecification
    : Identifier? Colon? (memberdeclaration | accessSpecifier Identifier? Colon) memberSpecification?
    ;

memberdeclaration
    : attributeSpecifierSeq? declSpecifierSeq? memberDeclaratorList? Semi
    | functionDefinition
    | usingDeclaration
	| usingEnumDeclaration
    | staticAssertDeclaration
    | templateDeclaration
	| explicitSpecialization
    | deductionGuide
    | aliasDeclaration
	| opaqueEnumDeclaration	
    | emptyDeclaration_
    ;

memberDeclaratorList
	: memberDeclarator (Comma memberDeclarator)*
    ;

memberDeclarator
	: declarator (virtualSpecifierSeq? pureSpecifier? | requiresClause | braceOrEqualInitializer?)
	| Identifier? attributeSpecifierSeq? Colon constantExpression braceOrEqualInitializer?
	;

virtualSpecifierSeq
	: virtualSpecifier+
    ;

virtualSpecifier
    : Override
    | Final
    ;

// purespecifier: Assign '0' conflicts with the lexer ;
pureSpecifier
    : Assign IntegerLiteral
    ;

conversionFunctionId
	: Operator conversionTypeId
	;

conversionTypeId
	: typeSpecifierSeq conversionDeclarator?
	;

conversionDeclarator
	: pointerOperator conversionDeclarator?
	;

baseClause
    : Colon baseSpecifierList
    ;

baseSpecifierList
	: baseSpecifier Ellipsis? (Comma baseSpecifier Ellipsis?)*
    ;

baseSpecifier
	: attributeSpecifierSeq? (Virtual accessSpecifier? | accessSpecifier Virtual?)? classOrDeclType
    ;

classOrDeclType
    : nestedNameSpecifier? (theTypeName | Template simpleTemplateId)
    | decltypeSpecifier
    ;

accessSpecifier
    : Private
    | Protected
    | Public
    ;

constructorInitializer
    : Colon memInitializerList
    ;

memInitializerList
	: memInitializer Ellipsis? (Comma memInitializer Ellipsis?)*
    ;

memInitializer
    : meminitializerid (LeftParen expressionList? RightParen | bracedInitList)
    ;

meminitializerid
    : classOrDeclType
    | Identifier
    ;


/* A.10 Overloading*/

operatorFunctionId
    : Operator theOperator
    ;

theOperator
    : New (LeftBracket RightBracket)?
    | Delete (LeftBracket RightBracket)?
	| Co_await
	| LeftParen RightParen
    | LeftBracket RightBracket
	| Arrow
	| ArrowStar
	| Tilde
    | Not	
    | Plus
    | Minus
    | Star
    | Div
    | Mod
    | Caret
    | And
    | Or
    | Assign
    | PlusAssign
    | MinusAssign
    | StarAssign
	| DivAssign
    | ModAssign
    | XorAssign
    | AndAssign
    | OrAssign
    | Equal
    | NotEqual
    | Less
    | Greater
    | LessEqual
    | GreaterEqual
	| LessEqualGreater
    | AndAnd
    | OrOr
    | Less Less
    | Greater Greater
    | LeftShiftAssign
	| RightShiftAssign
    | PlusPlus
    | MinusMinus
    | Comma
    ;

literalOperatorId
    : Operator (StringLiteral Identifier | UserDefinedStringLiteral)
    ;


/* A.11 Templates */

templateDeclaration
	: templateHead (declaration | conceptDefinition)
    ;

templateHead
	: Template Less templateparameterList Greater requiresClause?
	;

templateparameterList
	: templateParameter (Comma templateParameter)*
    ;

requiresClause
	: Requires constraintLogicalOrExpression
	;

constraintLogicalOrExpression
	: constraintLogicalAndExpression
	| constraintLogicalOrExpression OrOr constraintLogicalAndExpression
	;

constraintLogicalAndExpression
	: primaryExpression
	| constraintLogicalAndExpression AndAnd primaryExpression
	;

templateParameter
    : typeParameter
    | parameterDeclaration
    ;

typeParameter
	: typeParameterKey Ellipsis? Identifier? (Assign theTypeId)?
	| typeConstraint Ellipsis? Identifier? (Assign theTypeId)?
	| templateHead typeParameterKey Ellipsis? Identifier? (Assign idExpression)?
	;

typeParameterKey
	: Class
	| Typename_
	;

typeConstraint
	: nestedNameSpecifier? conceptName (Less templateArgumentList? Greater)?
	;

simpleTemplateId
    : templateName Less templateArgumentList? Greater
    ;

templateId
    : simpleTemplateId
    | (operatorFunctionId | literalOperatorId) Less templateArgumentList? Greater
    ;

templateName
    : Identifier
    ;

templateArgumentList
	: templateArgument Ellipsis? (Comma templateArgument Ellipsis?)*
    ;

templateArgument
    : constantExpression
	| theTypeId
    | idExpression
    ;

constraintExpression
	: logicalOrExpression
	;

deductionGuide
	: explicitSpecifier? templateName LeftParen parameterDeclarationClause RightParen Arrow simpleTemplateId Semi
	;

conceptDefinition
	: Concept conceptName Assign constraintExpression Semi
	;

conceptName
	: Identifier
	;

typeNameSpecifier
    : Typename_ nestedNameSpecifier (Identifier | Template? simpleTemplateId)
    ;

explicitInstantiation
    : Extern? Template declaration
    ;

explicitSpecialization
    : Template Less Greater declaration
    ;


/* A.12 Exception handling */

tryBlock
    : Try compoundStatement handlerSeq
    ;

functionTryBlock
    : Try constructorInitializer? compoundStatement handlerSeq
    ;

handlerSeq
    : handler handlerSeq?
    ;

handler
    : Catch LeftParen exceptionDeclaration RightParen compoundStatement
    ;

exceptionDeclaration
    : attributeSpecifierSeq? typeSpecifierSeq (declarator | abstractDeclarator)?
    | Ellipsis
    ;

noexceptSpecifier
    : Noexcept (LeftParen constantExpression RightParen)?
    ;


/* A.13 Preprocessing directives */

/*
preprocessingFile
	: group?
	| moduleFile
	;

moduleFile
	: ppGlobalModuleFragment? ppModule group? ppPrivateModuleFragment?
	;

ppGlobalModuleFragment
	: Module Semi Newline group?
	;

ppPrivateModuleFragment
	: Module Colon Private Semi Newline group?
	;

group
	: groupPart
	| group groupPart
	;
	
groupPart
	: controlLine
	| ifSection
	| textLine
	| Sharp conditionallySupportedDirective
	;

controlLine
	: Sharp Include ppTokens Newline
	| ppImport
	| Define Identifier replacementList Newline
	| Define Identifier lparen identifierList? RightParen replacementList Newline
	| Define Identifier lparen Ellipsis RightParen replacementList Newline
	| Define Identifier lparen identifierList Comma Ellipsis RightParen replacementList Newline
	| Undef Identifier Newline
	| Line_ ppTokens Newline
	| Error_  ppTokens? Newline
	| Pragma ppTokens? Newline
	| Sharp Newline
	;

ifSection
	:	ifGroup elifGroups? elseGroup? endifLine
	;

ifGroup
	: Sharp If constantExpression Newline group?
	| (Ifdef | Ifndef) Identifier Newline group?
	;

elifGroups
	: elifGroup+
	;

elifGroup
	: Sharp Elif constantExpression Newline group?
	;

elseGroup
	: Sharp Else Newline group?
	;

endifLine
	: Endif Newline
	;

textLine
	: ppTokens? Newline
	;

conditionallySupportedDirective
	: ppTokens Newline
	;

lparen
	: LeftParen
	// a ( character not immediately preceded by white-space
	;
*/

identifierList
	: Identifier (Comma Identifier)*
	;

/*
replacementList
	: ppTokens?
	;

ppTokens
	: preprocessingToken
	| ppTokens preprocessingToken
	;

definedMacroExpression
	: Defined (Identifier | LeftParen Identifier RightParen)
	;

hPreprocessingToken
	: ~ ('>')
	// any preprocessing-token other than >
	;
	
hppTokens
	: hPreprocessingToken
	| hppTokens hPreprocessingToken
	;

headerNameTokens
	: StringLiteral
	| Less hppTokens Greater
	;

hasIncludeExpression
	: Has_include LeftParen (headerName | headerNameTokens) RightParen
	;

hasAttributeExpression
	: Has_cpp_attribute LeftParen ppTokens RightParen
	;

ppModule
	: Export? Module ppTokens? Semi Newline
	;

ppImport
	: Export? Import (headerName | headerNameTokens)? ppTokens? Semi Newline
	;

vaOptReplacement
	: Va_opt LeftParen ppTokens? RightParen
	;
*/
