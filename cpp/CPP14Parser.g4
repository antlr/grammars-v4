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
parser grammar CPP14Parser;
options {
	tokenVocab = CPP14Lexer;
}
/*Basic concepts*/

translationUnit: declarationSeq? EOF;
/*Expressions*/

idExpression: (nestedNameSpecifier Template?)? unqualifiedId;

unqualifiedId:
	Identifier
	| operatorFunctionId
	| conversionFunctionId
	| literalOperatorId
	| Tilde (className | decltypeSpecifier)
	| templateId;

nestedNameSpecifier:
	(typeName | namespaceName | decltypeSpecifier)? Doublecolon (
		(Identifier | Template? simpleTemplateId) Doublecolon
	)*;

lambdaIntroducer: LeftBracket lambdaCapture? RightBracket;

lambdaCapture:
	captureList
	| captureDefault (Comma captureList)?;

captureDefault: And | Equal;

captureList: capture Ellipsis? (Comma capture Ellipsis?)*;

capture: simpleCapture | initcapture;

simpleCapture: And? Identifier | This;

initcapture: And? Identifier initializer;

lambdaDeclarator:
	LeftParen parameterDeclarationClause? RightParen Mutable? exceptionSpecification?
		attributeSpecifierSeq? trailingReturnType?;

expressionList: initializerList;

pseudoDestructorName:
	nestedNameSpecifier? (typeName Doublecolon)? Tilde typeName
	| nestedNameSpecifier Template simpleTemplateId Doublecolon Tilde typeName
	| Tilde decltypeSpecifier;

//TODO CHECK THIS
unaryOperator: Or | Star | And | Plus | Tilde | Minus | Not;

newPlacement: LeftParen expressionList RightParen;

newTypeId: typeSpecifierSeq newDeclarator?;

newDeclarator:
	pointerOperator newDeclarator?
	| noPointerNewDeclarator;

noPointerNewDeclarator:
	LeftBracket expression RightBracket attributeSpecifierSeq?
	| noPointerNewDeclarator LeftBracket expression RightBracket attributeSpecifierSeq?;

newInitializer:
	LeftParen expressionList? RightParen
	| bracedInitList;

shiftOperator: Greater Greater | Less Less;
comparisonOperator: Greater | Less | GreaterEqual | LessEqual;

assignmentOperator:
	Assign
	| StarAssign
	| DivAssign
	| ModAssign
	| PlusAssign
	| MinusAssign
	| RightShiftAssign
	| LeftShiftAssign
	| AndAssign
	| XorAssign
	| OrAssign;

expression:
	literal																# literalExpression
	| This																# thisExpression
	| LeftParen expression RightParen									# parenthesisedExpression
	| idExpression														# idExpressionAlternative
	| lambdaIntroducer lambdaDeclarator? compoundStatement				# lambdaExpression
	| expression LeftBracket (expression | bracedInitList) RightBracket	# subscriptExpression
	| expression LeftParen expressionList? RightParen					# callExpression
	| (
		simpleTypeSpecifier
		| typeNameSpecifier /*| templateName */
	) (LeftParen expressionList? RightParen | bracedInitList)					# initializationExpression
	| LeftParen (simpleTypeSpecifier | typeNameSpecifier) RightParen expression	# cCastExpression
	| expression (Dot | Arrow) (
		Template? idExpression
		| pseudoDestructorName
	)										# memberAccessExpression
	| expression (PlusPlus | MinusMinus)	# suffixIncrementDecrementExpression
	| (
		Dynamic_cast
		| Static_cast
		| Reinterpret_cast
		| Const_cast
	) Less typeId Greater LeftParen expression RightParen					# castExpression
	| Typeid_ LeftParen (expression | typeId) RightParen					# typeIdExpression
	| <assoc = right> (PlusPlus | MinusMinus | unaryOperator) expression	# unaryExpression
	| <assoc = right> Sizeof (
		LeftParen typeId RightParen
		| Ellipsis LeftParen Identifier RightParen
		| expression
	)															# sizeofExpression
	| <assoc = right> Alignof LeftParen typeId RightParen		# alignExpression
	| <assoc = right> Noexcept LeftParen expression RightParen	# noExceptExpression
	| <assoc = right> Doublecolon? New newPlacement? (
		newTypeId
		| (LeftParen typeId RightParen)
	) newInitializer? # newExpression
	| <assoc = right> Doublecolon? Delete (
		LeftBracket RightBracket
	)? expression														# deleteExpression
	| expression (DotStar | ArrowStar) expression						# pointerMemberExpression
	| expression (Star | Div | Mod) expression							# multiplicativeExpression
	| expression (Plus | Minus) expression								# additiveExpression
	| expression shiftOperator expression								# shiftExpression
	| expression comparisonOperator expression							# relationalExpression
	| expression (Equal | NotEqual) expression							# equalityExpression
	| expression And expression											# andExpression
	| expression Caret expression										# exclusiveOrExpression
	| expression Or expression											# inclusiveOrExpression
	| expression AndAnd expression										# logicalAndExpression
	| expression OrOr expression										# logicalOrExpression
	| <assoc = right> Throw expression									# throwExpression
	| <assoc = right> expression Question expression Colon expression	# conditionalExpression
	| <assoc = right> expression assignmentOperator initializerClause	# assignmentExpression
	| expression Comma expression										# commaExpression;

/*Statements*/

statement:
	labeledStatement
	| declarationStatement
	| attributeSpecifierSeq? (
		expressionStatement
		| compoundStatement
		| selectionStatement
		| iterationStatement
		| jumpStatement
		| tryBlock
	);

labeledStatement:
	attributeSpecifierSeq? (
		Identifier
		| Case expression
		| Default
	) Colon statement;

expressionStatement: expression? Semi;

compoundStatement: LeftBrace statementSeq? RightBrace;

statementSeq: statement+;

selectionStatement:
	If LeftParen condition RightParen statement (Else statement)?
	| Switch LeftParen condition RightParen statement;

condition:
	expression
	| attributeSpecifierSeq? declSpecifierSeq declarator (
		Assign initializerClause
		| bracedInitList
	);

iterationStatement:
	While LeftParen condition RightParen statement
	| Do statement While LeftParen expression RightParen Semi
	| For LeftParen (
		forInitStatement condition? Semi expression?
		| forRangeDeclaration Colon forRangeInitializer
	) RightParen statement;

forInitStatement: expressionStatement | simpleDeclaration;

forRangeDeclaration:
	attributeSpecifierSeq? declSpecifierSeq declarator;

forRangeInitializer: expression | bracedInitList;

jumpStatement:
	(
		Break
		| Continue
		| Return (expression | bracedInitList)?
		| Goto Identifier
	) Semi;

declarationStatement: blockDeclaration;
/*Declarations*/

declarationSeq: declaration+;

declaration:
	blockDeclaration
	| functionDefinition
	| templateDeclaration
	| explicitInstantiation
	| explicitSpecialization
	| linkageSpecification
	| namespaceDefinition
	| emptyDeclaration
	| attributeDeclaration;

blockDeclaration:
	simpleDeclaration
	| asmDeclaration
	| namespaceAliasDefinition
	| usingDeclaration
	| usingDirective
	| staticAssertDeclaration
	| aliasDeclaration
	| opaqueEnumDeclaration;

aliasDeclaration:
	(Template Less templateParameterList Greater)? Using Identifier attributeSpecifierSeq? Assign
		typeId Semi;

simpleDeclaration:
	(
		declSpecifierSeq? initDeclaratorList?
		| attributeSpecifierSeq declSpecifierSeq? initDeclaratorList
	) Semi;

staticAssertDeclaration:
	Static_assert LeftParen expression Comma StringLiteral RightParen Semi;

emptyDeclaration: Semi;

attributeDeclaration: attributeSpecifierSeq Semi;

declSpecifier:
	storageClassSpecifier
	| typeSpecifier
	| functionSpecifier
	| Friend
	| Typedef
	| Constexpr;

declSpecifierSeq: declSpecifier+ attributeSpecifierSeq?;

storageClassSpecifier:
	Register
	| Static
	| Thread_local
	| Extern
	| Mutable;

functionSpecifier: Inline | Virtual | Explicit;

typeDefName: Identifier;

typeSpecifier:
	trailingTypeSpecifier
	| classSpecifier
	| enumSpecifier;

trailingTypeSpecifier:
	simpleTypeSpecifier
	| elaboratedTypeSpecifier
	| typeNameSpecifier
	| cvQualifier;

typeSpecifierSeq: typeSpecifier+ attributeSpecifierSeq?;

trailingTypeSpecifierSeq:
	trailingTypeSpecifier+ attributeSpecifierSeq?;

simpleTypeSpecifier:
	nestedNameSpecifier? typeName
	| nestedNameSpecifier Template simpleTemplateId
	| Char
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
	| Auto
	| decltypeSpecifier;

typeName: className | enumName | typeDefName | simpleTemplateId;

decltypeSpecifier:
	Decltype LeftParen (expression | Auto) RightParen;

elaboratedTypeSpecifier:
	classKey (
		attributeSpecifierSeq? nestedNameSpecifier? Identifier
		| simpleTemplateId
		| nestedNameSpecifier Template? simpleTemplateId
	)
	| Enum nestedNameSpecifier? Identifier;

enumName: Identifier;

enumSpecifier:
	enumHead LeftBrace (enumeratorList Comma?)? RightBrace;

enumHead:
	enumkey attributeSpecifierSeq? (
		nestedNameSpecifier? Identifier
	)? enumbase?;

opaqueEnumDeclaration:
	enumkey attributeSpecifierSeq? Identifier enumbase? Semi;

enumkey: Enum (Class | Struct)?;

enumbase: Colon typeSpecifierSeq;

enumeratorList:
	enumeratorDefinition (Comma enumeratorDefinition)*;

enumeratorDefinition: enumerator (Assign expression)?;

enumerator: Identifier;

namespaceName: Identifier | namespaceAlias;

namespaceDefinition:
	Inline? Namespace (Identifier)? LeftBrace namespaceBody = declarationSeq? RightBrace;

namespaceAlias: Identifier;

namespaceAliasDefinition:
	Namespace Identifier Assign qualifiednamespacespecifier Semi;

qualifiednamespacespecifier: nestedNameSpecifier? namespaceName;

usingDeclaration:
	Using (Typename_? nestedNameSpecifier | Doublecolon) unqualifiedId Semi;

usingDirective:
	attributeSpecifierSeq? Using Namespace nestedNameSpecifier? namespaceName Semi;

asmDeclaration: Asm LeftParen StringLiteral RightParen Semi;

linkageSpecification:
	Extern StringLiteral (
		LeftBrace declarationSeq? RightBrace
		| declaration
	);

attributeSpecifierSeq: attributeSpecifier+;

attributeSpecifier:
	LeftBracket LeftBracket attributeList? RightBracket RightBracket
	| alignmentspecifier;

alignmentspecifier:
	Alignas LeftParen (typeId | expression) Ellipsis? RightParen;

attributeList: attribute Ellipsis? (Comma attribute Ellipsis?)*;

attribute: (attributeNamespace Doublecolon)? Identifier attributeArgumentClause?;

attributeNamespace: Identifier;

attributeArgumentClause: LeftParen balancedTokenSeq? RightParen;

balancedTokenSeq: balancedtoken+;

balancedtoken:
	LeftParen balancedTokenSeq RightParen
	| LeftBracket balancedTokenSeq RightBracket
	| LeftBrace balancedTokenSeq RightBrace
	| ~(
		LeftParen
		| RightParen
		| LeftBrace
		| RightBrace
		| LeftBracket
		| RightBracket
	)+;
/*Declarators*/

initDeclaratorList: initDeclarator (Comma initDeclarator)*;

initDeclarator: declarator initializer?;

declarator:
	pointerDeclarator
	| noPointerDeclarator parametersAndQualifiers trailingReturnType;

pointerDeclarator: (pointerOperator Const?)* noPointerDeclarator;

noPointerDeclarator:
	(
		declaratorId attributeSpecifierSeq?
		| LeftParen pointerDeclarator RightParen
	) (
		parametersAndQualifiers
		| LeftBracket expression? RightBracket attributeSpecifierSeq?
	)*;

parametersAndQualifiers:
	LeftParen parameterDeclarationClause? RightParen cvQualifierSeq? refqualifier?
		exceptionSpecification? attributeSpecifierSeq?;

trailingReturnType:
	Arrow trailingTypeSpecifierSeq abstractDeclarator?;

pointerOperator:
	(And | AndOperator) attributeSpecifierSeq?
	| nestedNameSpecifier? Star attributeSpecifierSeq? cvQualifierSeq?;

cvQualifierSeq: cvQualifier+;

cvQualifier: Const | Volatile;

refqualifier: And | AndOperator;

declaratorId: Ellipsis? idExpression;

typeId: typeSpecifierSeq abstractDeclarator?;

abstractDeclarator:
	pointerAbstractDeclarator
	| noPointerAbstractDeclarator? parametersAndQualifiers trailingReturnType
	| abstractPackDeclarator;

pointerAbstractDeclarator:
	noPointerAbstractDeclarator
	| pointerOperator+ noPointerAbstractDeclarator?;

noPointerAbstractDeclarator:
	(
		parametersAndQualifiers
		| LeftBracket expression? RightBracket attributeSpecifierSeq?
		| LeftParen pointerAbstractDeclarator RightParen
	) (
		parametersAndQualifiers
		| LeftBracket expression? RightBracket attributeSpecifierSeq?
	)*;

abstractPackDeclarator:
	pointerOperator* noPointerAbstractPackDeclarator;

noPointerAbstractPackDeclarator:
	noPointerAbstractPackDeclarator (
		parametersAndQualifiers
		| LeftBracket expression? RightBracket attributeSpecifierSeq?
	)
	| Ellipsis;

parameterDeclarationClause:
	parameterDeclarationList (Comma? Ellipsis)?
	| Ellipsis;

parameterDeclarationList:
	parameterDeclaration (Comma parameterDeclaration)*;

parameterDeclaration:
	attributeSpecifierSeq? declSpecifierSeq (
		declarator
		| abstractDeclarator
	)? (Assign initializerClause)?;

functionDefinition:
	attributeSpecifierSeq? declSpecifierSeq? declarator virtualSpecifierSeq? functionBody;

functionBody:
	constructorInitializer? compoundStatement
	| functionTryBlock
	| Assign (Default | Delete) Semi;

initializer:
	braceOrEqualInitializer
	| LeftParen expressionList RightParen;

braceOrEqualInitializer:
	Assign initializerClause
	| bracedInitList;

initializerClause:
	bracedInitList
	| (Dot Identifier Assign)? expression;

initializerList:
	initializerClause Ellipsis? (
		Comma initializerClause Ellipsis?
	)*;

bracedInitList: LeftBrace (initializerList Comma?)? RightBrace;

/*Classes*/

className: Identifier | simpleTemplateId;

classSpecifier:
	classHead LeftBrace memberSpecification? RightBrace;

classHead:
	classKey attributeSpecifierSeq? (
		classHeadName classVirtSpecifier?
	)? baseClause?
	| Union attributeSpecifierSeq? (
		classHeadName classVirtSpecifier?
	)?;

classHeadName: nestedNameSpecifier? className;

classVirtSpecifier: Final;

classKey: Class | Struct;

memberSpecification:
	(memberDeclaration | accessSpecifier Colon)+;

memberDeclaration:
	attributeSpecifierSeq? declSpecifierSeq? memberDeclaratorList? Semi
	| functionDefinition
	| usingDeclaration
	| staticAssertDeclaration
	| explicitSpecialization
	| templateDeclaration
	| aliasDeclaration
	| emptyDeclaration;

memberDeclaratorList:
	memberDeclarator (Comma memberDeclarator)*;

memberDeclarator:
	declarator (
		virtualSpecifierSeq? pureSpecifier?
		| braceOrEqualInitializer?
	)
	| Identifier? attributeSpecifierSeq? Colon expression;

virtualSpecifierSeq: virtualSpecifier+;

virtualSpecifier: Override | Final;
/*
 purespecifier: Assign '0'//Conflicts with the lexer ;
 */

pureSpecifier:
	Assign val = OctalLiteral {if($val.text.compareTo("0")!=0) throw new InputMismatchException(this);
		};
/*Derived classes*/

baseClause: Colon baseSpecifierList;

baseSpecifierList:
	baseSpecifier Ellipsis? (Comma baseSpecifier Ellipsis?)*;

baseSpecifier:
	attributeSpecifierSeq? (
		Virtual accessSpecifier?
		| accessSpecifier Virtual?
	)? baseTypeSpecifier;

classOrDeclType:
	nestedNameSpecifier? className
	| decltypeSpecifier;

baseTypeSpecifier: classOrDeclType;

accessSpecifier: Private | Protected | Public;
/*Special member functions*/

conversionFunctionId: Operator conversionTypeId;

conversionTypeId: typeSpecifierSeq conversionDeclarator?;

conversionDeclarator: pointerOperator conversionDeclarator?;

constructorInitializer: Colon memInitializerList;

memInitializerList:
	memInitializer Ellipsis? (Comma memInitializer Ellipsis?)*;

memInitializer:
	meminitializerid (
		LeftParen expressionList? RightParen
		| bracedInitList
	);

meminitializerid: classOrDeclType | Identifier;
/*Overloading*/

operatorFunctionId: Operator operator;

literalOperatorId:
	Operator (
		StringLiteral Identifier
		| UserDefinedStringLiteral
	);
/*Templates*/

templateDeclaration:
	Template Less templateParameterList Greater declaration;

templateParameterList:
	templateParameter (Comma templateParameter)*;

templateParameter: typeParameter | parameterDeclaration;

typeParameter:
	(
		(Template Less templateParameterList Greater)? Class
		| Typename_
	) ((Ellipsis? Identifier?) | (Identifier? Assign typeId));

simpleTemplateId:
	templateName Less templateArgumentList? Greater;

templateId:
	simpleTemplateId
	| (operatorFunctionId | literalOperatorId) Less templateArgumentList? Greater;

templateName: Identifier;

templateArgumentList:
	templateArgument Ellipsis? (Comma templateArgument Ellipsis?)*;

templateArgument: typeId | expression;

typeNameSpecifier:
	Typename_ nestedNameSpecifier (
		Identifier
		| Template? simpleTemplateId
	);

explicitInstantiation: Extern? Template declaration;

explicitSpecialization: Template Less Greater declaration;
/*Exception handling*/

tryBlock: Try compoundStatement handlerSeq;

functionTryBlock:
	Try constructorInitializer? compoundStatement handlerSeq;

handlerSeq: handler+;

handler:
	Catch LeftParen exceptionDeclaration RightParen compoundStatement;

exceptionDeclaration:
	attributeSpecifierSeq? typeSpecifierSeq (
		declarator
		| abstractDeclarator
	)?
	| Ellipsis;

//throwExpression: Throw expression?;

exceptionSpecification:
	dynamicExceptionSpecification
	| noeExceptSpecification;

dynamicExceptionSpecification:
	Throw LeftParen typeIdList? RightParen;

typeIdList: typeId Ellipsis? (Comma typeId Ellipsis?)*;

noeExceptSpecification:
	Noexcept (LeftParen expression RightParen)?;
/*Preprocessing directives*/

literal:
	IntegerLiteral
	| CharacterLiteral
	| FloatingLiteral
	| StringLiteral
	| BooleanLiteral
	| PointerLiteral
	| UserDefinedLiteral;

operator:
	New (LeftBracket RightBracket)?
	| Delete (LeftBracket RightBracket)?
	| Plus
	| Minus
	| Star
	| Div
	| Mod
	| Caret
	| And
	| Or
	| Tilde
	| Not
	| Assign
	| Less
	| GreaterEqual
	| PlusAssign
	| MinusAssign
	| StarAssign
	| Assign
	| ModAssign
	| XorAssign
	| AndAssign
	| OrAssign
	| Greater Greater
	| Less Less
	| RightShiftAssign
	| LeftShiftAssign
	| Equal
	| NotEqual
	| LessEqual
	| GreaterEqual
	| AndAnd
	| OrOr
	| PlusPlus
	| MinusMinus
	| Comma
	| ArrowStar
	| Arrow
	| LeftParen RightParen
	| LeftBracket RightBracket;