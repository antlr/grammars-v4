/* Generated Mon, Jun 13, 2022 8:11:58 AM EST
 *
 * Copyright (c) 2022, 2023 Ken Domino
 * Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
 * for details. All rights reserved. Use of this source code is governed by a
 * BSD-style license that can be found in the LICENSE file.
 *
 * This grammar is generated from the CFG contained in:
 * https://github.com/dart-lang/language/blob/70eb85cf9a6606a9da0de824a5d55fd06de1287f/specification/dartLangSpec.tex
 *
 * The bash script used to scrape and the refactor the gramamr is here:
 * https://github.com/kaby76/ScrapeDartSpec/blob/master/refactor.sh
 *
 * Note: the CFG in the Specification is in development, and is for approximately
 * Dart version 2.15. The Specification is not up-to-date vis-a-vis the actual
 * compiler code, located here:
 * https://github.com/dart-lang/sdk/tree/main/pkg/_fe_analyzer_shared/lib/src/parser
 * Some of the refactorings that are applied are to bring the code into a working
 * Antlr4 parser. Other refactorings replace some of the rules in the Spec because
 * the Spec is incorrect, or incomplete.
 *
 * This grammar has been checked against a large subset (~370 Dart files) of the Dart SDK:
 * https://github.com/dart-lang/sdk/tree/main/sdk/lib
 * A copy of the SDK is provided in the examples for regression testing.
 */
parser grammar Dart2Parser;

options { tokenVocab=Dart2Lexer; }

additiveExpression : multiplicativeExpression ( additiveOperator multiplicativeExpression )* | SUPER_ ( additiveOperator multiplicativeExpression )+ ;
additiveOperator : PL | MINUS ;
argumentList : namedArgument ( C namedArgument )* | expressionList ( C namedArgument )* ;
argumentPart : typeArguments? arguments ;
arguments : OP ( argumentList C? )? CP ;
asOperator : AS_ ;
assertion : ASSERT_ OP expression ( C expression )? C? CP ;
assertStatement : assertion SC ;
assignableExpression : primary assignableSelectorPart | SUPER_ unconditionalAssignableSelector | identifier ;
assignableSelector : unconditionalAssignableSelector | QUD identifier | QU OB expression CB ;
assignableSelectorPart : selector* assignableSelector ;
assignmentOperator : EQ | compoundAssignmentOperator ;
awaitExpression : AWAIT_ unaryExpression ;
binaryOperator : multiplicativeOperator | additiveOperator | shiftOperator | relationalOperator | EE | bitwiseOperator ;
bitwiseAndExpression : shiftExpression ( A shiftExpression )* | SUPER_ ( A shiftExpression )+ ;
bitwiseOperator : A | CIR | P ;
bitwiseOrExpression : bitwiseXorExpression ( P bitwiseXorExpression )* | SUPER_ ( P bitwiseXorExpression )+ ;
bitwiseXorExpression : bitwiseAndExpression ( CIR bitwiseAndExpression )* | SUPER_ ( CIR bitwiseAndExpression )+ ;
block : OBC statements CBC ;
booleanLiteral : TRUE_ | FALSE_ ;
breakStatement : BREAK_ identifier? SC ;
cascade : cascade DD cascadeSection | conditionalExpression ( QUDD | DD ) cascadeSection ;
cascadeAssignment : assignmentOperator expressionWithoutCascade ;
cascadeSection : cascadeSelector cascadeSectionTail ;
cascadeSectionTail : cascadeAssignment | selector* ( assignableSelector cascadeAssignment )? ;
cascadeSelector : OB expression CB | identifier ;
catchPart : CATCH_ OP identifier ( C identifier )? CP ;
classDeclaration : ABSTRACT_? CLASS_ typeIdentifier typeParameters? superclass? interfaces? OBC ( metadata classMemberDeclaration )* CBC | ABSTRACT_? CLASS_ mixinApplicationClass ;
classMemberDeclaration : declaration SC | methodSignature functionBody ;
combinator : SHOW_ identifierList | HIDE_ identifierList ;
compilationUnit: (libraryDeclaration | partDeclaration | expression | statement) EOF ;
compoundAssignmentOperator : STE | SE | SQSE | PE | PLE | ME | LTLTE | GT GT GT EQ | GT GT EQ | AE | CIRE | POE | QUQUEQ ;
conditionalExpression : ifNullExpression ( QU expressionWithoutCascade CO expressionWithoutCascade )? ;
configurableUri : uri configurationUri* ;
configurationUri : IF_ OP uriTest CP uri ;
constantConstructorSignature : CONST_ constructorName formalParameterList ;
constObjectExpression : CONST_ constructorDesignation arguments ;
constructorDesignation : typeIdentifier | qualifiedName | typeName typeArguments ( D identifier )? ;
constructorInvocation : typeName typeArguments D identifier arguments ;
constructorName : typeIdentifier ( D identifier )? ;
constructorSignature : constructorName formalParameterList ;
continueStatement : CONTINUE_ identifier? SC ;
declaration :ABSTRACT_? ( EXTERNAL_ factoryConstructorSignature | EXTERNAL_ constantConstructorSignature | EXTERNAL_ constructorSignature | ( EXTERNAL_ STATIC_? )? getterSignature | ( EXTERNAL_ STATIC_? )? setterSignature | ( EXTERNAL_ STATIC_? )? functionSignature | EXTERNAL_? operatorSignature | STATIC_ CONST_ type? staticFinalDeclarationList | STATIC_ FINAL_ type? staticFinalDeclarationList | STATIC_ LATE_ FINAL_ type? initializedIdentifierList | STATIC_ LATE_? varOrType initializedIdentifierList | COVARIANT_ LATE_ FINAL_ type? identifierList | COVARIANT_ LATE_? varOrType initializedIdentifierList | LATE_? FINAL_ type? initializedIdentifierList | LATE_? varOrType initializedIdentifierList | redirectingFactoryConstructorSignature | constantConstructorSignature ( redirection | initializers )? | constructorSignature ( redirection | initializers )? );
declaredIdentifier : COVARIANT_? finalConstVarOrType identifier ;
defaultCase : label* DEFAULT_ CO statements ;
defaultFormalParameter : normalFormalParameter ( EQ expression )? ;
defaultNamedParameter : metadata REQUIRED_? normalFormalParameterNoMetadata ( ( EQ | CO ) expression )? ;
doStatement : DO_ statement WHILE_ OP expression CP SC ;
dottedIdentifierList : identifier ( D identifier )* ;
element : expressionElement | mapElement | spreadElement | ifElement | forElement ;
elements : element ( C element )* C? ;
enumEntry : metadata identifier ;
enumType : ENUM_ identifier OBC enumEntry ( C enumEntry )* C? CBC ;
equalityExpression : relationalExpression ( equalityOperator relationalExpression )? | SUPER_ equalityOperator relationalExpression ;
equalityOperator : EE | NE ;
expression : assignableExpression assignmentOperator expression | conditionalExpression | cascade | throwExpression ;
expressionElement : expression ;
expressionList : expression ( C expression )* ;
expressionStatement : expression? SC ;
expressionWithoutCascade : assignableExpression assignmentOperator expressionWithoutCascade | conditionalExpression | throwExpressionWithoutCascade ;
extensionDeclaration : EXTENSION_ identifier? typeParameters? ON_ type OBC ( metadata classMemberDeclaration )* CBC ;
factoryConstructorSignature : CONST_? FACTORY_ constructorName formalParameterList ;
fieldFormalParameter : finalConstVarOrType? THIS_ D identifier ( formalParameterPart QU? )? ;
fieldInitializer : ( THIS_ D )? identifier EQ initializerExpression ;
finalConstVarOrType : LATE_? FINAL_ type? | CONST_ type? | LATE_? varOrType ;
finallyPart : FINALLY_ block ;
forElement : AWAIT_? FOR_ OP forLoopParts CP element ;
forInitializerStatement : localVariableDeclaration | expression? SC ;
forLoopParts : forInitializerStatement expression? SC expressionList? | metadata declaredIdentifier IN_ expression | identifier IN_ expression ;
formalParameterList : OP CP | OP normalFormalParameters C? CP | OP normalFormalParameters C optionalOrNamedFormalParameters CP | OP optionalOrNamedFormalParameters CP ;
formalParameterPart : typeParameters? formalParameterList ;
forStatement : AWAIT_? FOR_ OP forLoopParts CP statement ;
functionBody :NATIVE_ stringLiteral? SC |  ASYNC_? EG expression SC | ( ASYNC_ ST? | SYNC_ ST )? block ;
functionExpression : formalParameterPart functionExpressionBody ;
functionExpressionBody : ASYNC_? EG expression | ( ASYNC_ ST? | SYNC_ ST )? block ;
functionFormalParameter : COVARIANT_? type? identifier formalParameterPart QU? ;
functionPrefix : type? identifier ;
functionSignature : type? identifier formalParameterPart ;
functionType : functionTypeTails | typeNotFunction functionTypeTails ;
functionTypeAlias : functionPrefix formalParameterPart SC ;
functionTypeTail : FUNCTION_ typeParameters? parameterTypeList ;
functionTypeTails : functionTypeTail QU? functionTypeTails | functionTypeTail ;
getterSignature : type? GET_ identifier ;
identifier : IDENTIFIER | ABSTRACT_ | AS_ | COVARIANT_ | DEFERRED_ | DYNAMIC_ | EXPORT_ | EXTERNAL_ | EXTENSION_ | FACTORY_ | FUNCTION_ | GET_ | IMPLEMENTS_ | IMPORT_ | INTERFACE_ | LATE_ | LIBRARY_ | MIXIN_ | OPERATOR_ | PART_ | REQUIRED_ | SET_ | STATIC_ | TYPEDEF_ | FUNCTION_ | ASYNC_ | HIDE_ | OF_ | ON_ | SHOW_ | SYNC_ | AWAIT_ | YIELD_ | DYNAMIC_ | NATIVE_ ;
identifierList : identifier ( C identifier )* ;
ifElement : IF_ OP expression CP element ( ELSE_ element )? ;
ifNullExpression : logicalOrExpression ( QUQU logicalOrExpression )* ;
ifStatement : IF_ OP expression CP statement ( ELSE_ statement )? ;
importOrExport : libraryImport | libraryExport ;
importSpecification : IMPORT_ configurableUri ( DEFERRED_? AS_ identifier )? combinator* SC ;
incrementOperator : PLPL | MM ;
initializedIdentifier : identifier ( EQ expression )? ;
initializedIdentifierList : initializedIdentifier ( C initializedIdentifier )* ;
initializedVariableDeclaration : declaredIdentifier ( EQ expression )? ( C initializedIdentifier )* ;
initializerExpression : conditionalExpression | cascade ;
initializerListEntry : SUPER_ arguments | SUPER_ D identifier arguments | fieldInitializer | assertion ;
initializers : CO initializerListEntry ( C initializerListEntry )* ;
interfaces : IMPLEMENTS_ typeNotVoidList ;
isOperator : IS_ NOT? ;
label : identifier CO ;
letExpression : LET_ staticFinalDeclarationList IN_ expression ;
libraryDeclaration :  libraryName? importOrExport* partDirective* ( metadata topLevelDeclaration )*  ;

libraryExport : metadata EXPORT_ configurableUri combinator* SC ;
libraryImport : metadata importSpecification ;
libraryName : metadata LIBRARY_ dottedIdentifierList SC ;
listLiteral : CONST_? typeArguments? OB elements? CB ;
literal : nullLiteral | booleanLiteral | numericLiteral | stringLiteral | symbolLiteral | listLiteral | setOrMapLiteral ;
localFunctionDeclaration : metadata functionSignature functionBody ;
localVariableDeclaration : metadata initializedVariableDeclaration SC ;
logicalAndExpression : equalityExpression ( AA equalityExpression )* ;
logicalOrExpression : logicalAndExpression ( PP logicalAndExpression )* ;
mapElement : expression CO expression ;
metadata : ( AT metadatum )* ;
metadatum : identifier | qualifiedName | constructorDesignation arguments ;
methodSignature : constructorSignature initializers? | factoryConstructorSignature | STATIC_? functionSignature | STATIC_? getterSignature | STATIC_? setterSignature | operatorSignature ;
minusOperator : MINUS ;
mixinApplication : typeNotVoid mixins interfaces? ;
mixinApplicationClass : identifier typeParameters? EQ mixinApplication SC ;
mixinDeclaration : MIXIN_ typeIdentifier typeParameters? ( ON_ typeNotVoidList )? interfaces? OBC ( metadata classMemberDeclaration )* CBC ;
mixins : WITH_ typeNotVoidList ;
multilineString : MultiLineString;
multiplicativeExpression : unaryExpression ( multiplicativeOperator unaryExpression )* | SUPER_ ( multiplicativeOperator unaryExpression )+ ;
multiplicativeOperator : ST | SL | PC | SQS ;
namedArgument : label expression ;
namedFormalParameters : OBC defaultNamedParameter ( C defaultNamedParameter )* C? CBC ;
namedParameterType : metadata REQUIRED_? typedIdentifier ;
namedParameterTypes : OBC namedParameterType ( C namedParameterType )* C? CBC ;
negationOperator : NOT ;
newExpression : NEW_ constructorDesignation arguments ;
nonLabelledStatement : block | localVariableDeclaration | forStatement | whileStatement | doStatement | switchStatement | ifStatement | rethrowStatement | tryStatement | breakStatement | continueStatement | returnStatement | yieldStatement | yieldEachStatement | expressionStatement | assertStatement | localFunctionDeclaration ;
normalFormalParameter : metadata normalFormalParameterNoMetadata ;
normalFormalParameterNoMetadata : functionFormalParameter | fieldFormalParameter | simpleFormalParameter ;
normalFormalParameters : normalFormalParameter ( C normalFormalParameter )* ;
normalParameterType : metadata typedIdentifier | metadata type ;
normalParameterTypes : normalParameterType ( C normalParameterType )* ;
nullLiteral : NULL_ ;
numericLiteral : NUMBER | HEX_NUMBER ;
onPart : catchPart block | ON_ typeNotVoid catchPart? block ;
operator : SQUIG | binaryOperator | OB CB | OB CB EQ ;
operatorSignature : type? OPERATOR_ operator formalParameterList ;
optionalOrNamedFormalParameters : optionalPositionalFormalParameters | namedFormalParameters ;
optionalParameterTypes : optionalPositionalParameterTypes | namedParameterTypes ;
optionalPositionalFormalParameters : OB defaultFormalParameter ( C defaultFormalParameter )* C? CB ;
optionalPositionalParameterTypes : OB normalParameterTypes C? CB ;
parameterTypeList : OP CP | OP normalParameterTypes C optionalParameterTypes CP | OP normalParameterTypes C? CP | OP optionalParameterTypes CP ;
partDeclaration : partHeader  (metadata topLevelDeclaration)*  ;
partDirective : metadata PART_ uri SC ;
partHeader : metadata PART_ OF_ ( dottedIdentifierList | uri ) SC ;
postfixExpression : assignableExpression postfixOperator | primary selector* ;
postfixOperator : incrementOperator ;
prefixOperator : minusOperator | negationOperator | tildeOperator ;
primary : thisExpression | SUPER_ unconditionalAssignableSelector | SUPER_ argumentPart | functionExpression | literal | identifier | newExpression | constObjectExpression | constructorInvocation | OP expression CP ;
qualifiedName : typeIdentifier D identifier | typeIdentifier D typeIdentifier D identifier ;
redirectingFactoryConstructorSignature : CONST_? FACTORY_ constructorName formalParameterList EQ constructorDesignation ;
redirection : CO THIS_ ( D identifier )? arguments ;
relationalExpression : bitwiseOrExpression ( typeTest | typeCast | relationalOperator bitwiseOrExpression )? | SUPER_ relationalOperator bitwiseOrExpression ;
relationalOperator : GT EQ | GT | LTE | LT ;
reserved_word : ASSERT_ | BREAK_ | CASE_ | CATCH_ | CLASS_ | CONST_ | CONTINUE_ | DEFAULT_ | DO_ | ELSE_ | ENUM_ | EXTENDS_ | FALSE_ | FINAL_ | FINALLY_ | FOR_ | IF_ | IN_ | IS_ | NEW_ | NULL_ | RETHROW_ | RETURN_ | SUPER_ | SWITCH_ | THIS_ | THROW_ | TRUE_ | TRY_ | VAR_ | VOID_ | WHILE_ | WITH_ ;
rethrowStatement : RETHROW_ SC ;
returnStatement : RETURN_ expression? SC ;
selector : NOT | assignableSelector | argumentPart ;
setOrMapLiteral : CONST_? typeArguments? OBC elements? CBC ;
setterSignature : type? SET_ identifier formalParameterList ;
shiftExpression : additiveExpression ( shiftOperator additiveExpression )* | SUPER_ ( shiftOperator additiveExpression )+ ;
shiftOperator : LTLT | GT GT GT | GT GT ;
simpleFormalParameter : declaredIdentifier | COVARIANT_? identifier ;
singleLineString : SingleLineString;
spreadElement : ( DDD | DDDQ ) expression ;
statement : label* nonLabelledStatement ;
statements : statement* ;
staticFinalDeclaration : identifier EQ expression ;
staticFinalDeclarationList : staticFinalDeclaration ( C staticFinalDeclaration )* ;
stringLiteral : ( multilineString | singleLineString )+ ;
superclass : EXTENDS_ typeNotVoid mixins? | mixins ;
switchCase : label* CASE_ expression CO statements ;
switchStatement : SWITCH_ OP expression CP OBC switchCase* defaultCase? CBC ;
symbolLiteral : PO ( identifier ( D identifier )* | operator | VOID_ ) ;
thisExpression : THIS_ ;
throwExpression : THROW_ expression ;
throwExpressionWithoutCascade : THROW_ expressionWithoutCascade ;
tildeOperator : SQUIG ;
topLevelDeclaration : classDeclaration | mixinDeclaration | extensionDeclaration | enumType | typeAlias | EXTERNAL_ functionSignature SC | EXTERNAL_ getterSignature SC | EXTERNAL_ setterSignature SC | functionSignature functionBody | getterSignature functionBody | setterSignature functionBody | ( FINAL_ | CONST_ ) type? staticFinalDeclarationList SC | LATE_ FINAL_ type? initializedIdentifierList SC | LATE_? varOrType initializedIdentifierList SC ;
tryStatement : TRY_ block ( onPart+ finallyPart? | finallyPart ) ;
type : functionType QU? | typeNotFunction ;
typeAlias : TYPEDEF_ typeIdentifier typeParameters? EQ type SC | TYPEDEF_ functionTypeAlias ;
typeArguments : LT typeList GT ;
typeCast : asOperator typeNotVoid ;
typedIdentifier : type identifier ;
typeIdentifier : IDENTIFIER | ASYNC_ | HIDE_ | OF_ | ON_ | SHOW_ | SYNC_ | AWAIT_ | YIELD_ | DYNAMIC_ | NATIVE_ | FUNCTION_;
typeList : type ( C type )* ;
typeName : typeIdentifier ( D typeIdentifier )? ;
typeNotFunction : VOID_ | typeNotVoidNotFunction ;
typeNotVoid : functionType QU? | typeNotVoidNotFunction ;
typeNotVoidList : typeNotVoid ( C typeNotVoid )* ;
typeNotVoidNotFunction : typeName typeArguments? QU? | FUNCTION_ QU? ;
typeNotVoidNotFunctionList : typeNotVoidNotFunction ( C typeNotVoidNotFunction )* ;
typeParameter : metadata identifier ( EXTENDS_ typeNotVoid )? ;
typeParameters : LT typeParameter ( C typeParameter )* GT ;
typeTest : isOperator typeNotVoid ;
unaryExpression : prefixOperator unaryExpression | awaitExpression | postfixExpression | ( minusOperator | tildeOperator ) SUPER_ | incrementOperator assignableExpression ;
unconditionalAssignableSelector : OB expression CB | D identifier ;
uri : stringLiteral ;
uriTest : dottedIdentifierList ( EE stringLiteral )? ;
varOrType : VAR_ | type ;
whileStatement : WHILE_ OP expression CP statement ;
yieldEachStatement : YIELD_ ST expression SC ;
yieldStatement : YIELD_ expression SC ;
