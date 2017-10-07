/**
 * Kotlin Grammar for ANTLR v4
 *
 * Based on:
 * http://jetbrains.github.io/kotlin-spec/#_grammars_and_parsing
 * and
 * http://kotlinlang.org/docs/reference/grammar.html
 *
 * Tested on
 * https://github.com/JetBrains/kotlin/tree/master/compiler/testData/psi
 */

parser grammar KotlinParser;

options { tokenVocab = KotlinLexer; }

kotlinFile
    : NL* fileAnnotation? packageHeader importList topLevelObject* EOF
    ;

script
    : NL* fileAnnotation? packageHeader importList(expression semi?)* EOF
    ;

fileAnnotation
    : (FILE COLON (LSQUARE unescapedAnnotation+ RSQUARE | unescapedAnnotation) semi?)+
    ;

packageHeader
    : (PACKAGE identifier semi?)?
    ;

importList
    : importHeader*
    ;

importHeader
    : IMPORT identifier (DOT MULT | importAlias)? semi?
    ;

importAlias
    : AS simpleIdentifier
    ;

topLevelObject
    : (classDeclaration
    | functionDeclaration
    | objectDeclaration
    | propertyDeclaration
    | typeAlias) semi?
    ;

classDeclaration
    : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier
    (NL* typeParameters)? (NL* primaryConstructor)?
    (NL* COLON NL* delegationSpecifiers)?
    (NL* typeConstraints)?
    (NL* classBody | NL* enumClassBody)?
    ;

primaryConstructor
    : modifierList? (CONSTRUCTOR NL*)? classParameters
    ;

classParameters
    : LPAREN (classParameter (COMMA classParameter)*)? RPAREN
    ;

classParameter
    : modifierList? (VAL | VAR)? simpleIdentifier COLON type (ASSIGNMENT expression)?
    ;

delegationSpecifiers
    : annotations* delegationSpecifier (NL* COMMA NL* delegationSpecifier)*
    ;

delegationSpecifier
    : constructorInvocation
    | userType
    | explicitDelegation
    ;

constructorInvocation
    : userType callSuffix
    ;

explicitDelegation
    : userType NL* BY NL* expression
    ;

classBody
    : LCURL NL* classMemberDeclaration* NL* RCURL
    ;

classMemberDeclaration
    : (classDeclaration
    | functionDeclaration
    | objectDeclaration
    | companionObject
    | propertyDeclaration
    | anonymousInitializer
    | secondaryConstructor
    | typeAlias) semi?
    ;

anonymousInitializer
    : INIT NL* block
    ;

secondaryConstructor
    : modifierList? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block
    ;

constructorDelegationCall
    : THIS NL* valueArguments
    | SUPER NL* valueArguments
    ;

enumClassBody
    : LCURL NL* enumEntries? (NL* SEMICOLON NL* classMemberDeclaration*)? NL* RCURL
    ;

enumEntries
    : (enumEntry NL*)+ SEMICOLON?
    ;

enumEntry
    : simpleIdentifier (NL* valueArguments)? (NL* classBody)? (NL* COMMA)?
    ;

functionDeclaration
    : modifierList? FUN
    //(NL* typeParameters)?
    (NL* type NL* DOT)?
    (NL* typeParameters)?
    (NL* identifier)?
    NL* functionValueParameters
    (NL* COLON NL* type)?
    (NL* typeConstraints)?
    (NL* functionBody)?
    ;

functionValueParameters
    : LPAREN (functionValueParameter (COMMA functionValueParameter)*)? RPAREN
    ;

functionValueParameter
    : modifierList? parameter (ASSIGNMENT expression)?
    ;

parameter
    : simpleIdentifier COLON type
    ;

functionBody
    : block
    | ASSIGNMENT NL* expression
    ;

objectDeclaration
    : modifierList? OBJECT
    NL* simpleIdentifier
    (NL* primaryConstructor)?
    (NL* COLON NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

companionObject
    : modifierList? COMPANION NL* modifierList? OBJECT
    (NL* simpleIdentifier)?
    (NL* COLON NL* delegationSpecifiers)?
    (NL* classBody)?
    ;

propertyDeclaration
    : modifierList? (VAL | VAR)
    (NL* typeParameters)?
    (NL* type NL* DOT)?
    (NL* (multiVariableDeclaration | variableDeclaration))
    (NL* typeConstraints)?
    (NL* (BY | ASSIGNMENT) NL* expression)?
    semi? (getter? (NL* setter)? | setter? (NL* getter)?)
    ;

multiVariableDeclaration
    : LPAREN variableDeclaration (COMMA variableDeclaration)* RPAREN
    ;

variableDeclaration
    : simpleIdentifier (COLON type)?
    ;

getter
    : modifierList? GETTER
    | modifierList? GETTER NL* LPAREN RPAREN (NL* COLON NL* type)? NL* (block | ASSIGNMENT NL* expression)
    ;

setter
    : modifierList? SETTER
    | modifierList? SETTER NL* LPAREN (annotations | parameterModifier)* (simpleIdentifier | parameter) RPAREN NL* functionBody
    ;

typeAlias
    : modifierList? TYPE_ALIAS NL* simpleIdentifier (NL* typeParameters)? NL* ASSIGNMENT NL* type
    ;

typeParameters
    : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* NL* RANGLE
    ;

typeParameter
    : modifierList? NL* simpleIdentifier (NL* COLON NL* type)?
    ;

type
    : typeModifierList?
    ( parenthesizedType
    | nullableType
    | typeReference
    | functionType)
    ;

typeModifierList
    : (annotations | SUSPEND NL*)+
    ;

parenthesizedType
    : LPAREN type RPAREN
    ;

nullableType
    : (typeReference | parenthesizedType) NL* QUEST+
    ;

typeReference
    : LPAREN typeReference RPAREN
    | userType
    | DYNAMIC
    ;

functionType
    : (functionTypeReceiver NL* DOT NL*)? functionTypeParameters  NL* ARROW (NL* type)?
    ;

functionTypeReceiver
    : parenthesizedType
    | nullableType
    | typeReference
    ;

userType
    : simpleUserType (NL* DOT NL* simpleUserType)*
    ;

simpleUserType
    : simpleIdentifier (NL* typeArguments)?
    ;

//parameters for functionType
functionTypeParameters
    : LPAREN (parameter | type)? (COMMA (parameter | type))* RPAREN
    ;

typeConstraints
    : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*
    ;

typeConstraint
    : annotations* simpleIdentifier NL* COLON NL* type
    ;

block
    : LCURL NL* (statement semi)* (statement semi?)? NL* RCURL
    ;

statements
    : ((statement semi)* statement semi?)?
    ;

statement
    : declaration
    | assignment
    | expression
    ;

declaration
    : labelDefinition*
    ( classDeclaration
    | functionDeclaration
    | propertyDeclaration
    | typeAlias)
    ;

assignment
    : assignableExpression assignmentOperator NL* disjunction
    ;

expression
    : disjunction
    ;

disjunction
    : conjunction (NL* DISJ NL* conjunction)*
    ;

conjunction
    : equality (NL* CONJ NL* equality)*
    ;

equality
    : comparison (equalityOperator NL* comparison)*
    ;

comparison
    : infixOperation (comparisonOperator NL* infixOperation)?
    ;

infixOperation
    : elvisExpression (inOperator NL* elvisExpression)*
    | elvisExpression (isOperator NL* type)?
    ;

elvisExpression
    : infixFunctionCall (NL* ELVIS NL* infixFunctionCall)*
    ;

infixFunctionCall
    : rangeExpression (simpleIdentifier NL* rangeExpression)*
    ;

rangeExpression
    : additiveExpression (RANGE NL* additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
    ;

multiplicativeExpression
    : asExpression (multiplicativeOperator NL* asExpression)*
    ;

asExpression
    : prefixUnaryExpression asExpressionTail?
    ;

asExpressionTail
    : NL* asOperator NL* type asExpressionTail?
    ;

prefixUnaryExpression
    : prefixUnaryOperator* postfixUnaryExpression
    | annotations* postfixUnaryExpression
    ;

postfixUnaryExpression
    : assignableExpression
    | callExpression
    | labeledExpression
    | dotQualifiedExpression
    | assignableExpression postfixUnaryOperator*
    | LPAREN callableReference RPAREN postfixUnaryOperator+
    | callableReference
    ;

callExpression
    : assignableExpression typeArguments? valueArguments? annotatedLambda*
    ;

labeledExpression
    : labelDefinition postfixUnaryExpression
    ;

dotQualifiedExpression
    : assignableExpression (NL* memberAccessOperator postfixUnaryExpression)+
    ;

assignableExpression
    : primaryExpression
    | indexingExpression
    ;

indexingExpression
    : identifier arrayAccess+
    ;

callSuffix
    : typeArguments? valueArguments annotatedLambda*
    | typeArguments annotatedLambda*
    ;

annotatedLambda
    : unescapedAnnotation* LabelDefinition? NL* functionLiteral
    ;

arrayAccess
    : LSQUARE (expression (COMMA expression)*)? RSQUARE
    ;

valueArguments
    : LPAREN valueArgument? RPAREN
    | LPAREN valueArgument (COMMA valueArgument)* RPAREN
    ;

typeArguments
    : LANGLE NL* typeProjection (NL* COMMA typeProjection)* NL* RANGLE
    ;

typeProjection
    : typeProjectionModifierList? type | MULT
    ;

typeProjectionModifierList
    : varianceAnnotation+
    ;

valueArgument
    : (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression
    ;

primaryExpression
    : parenthesizedExpression
    | literalConstant
    | stringLiteral
    | simpleIdentifier
    | functionLiteral
    | objectLiteral
    | collectionLiteral
    | thisExpression
    | superExpression
    | conditionalExpression
    | tryExpression
    | loopExpression
    | jumpExpression
    ;

parenthesizedExpression
    : LPAREN expression RPAREN
    ;

literalConstant
    : BooleanLiteral
    | IntegerLiteral
    | HexLiteral
    | BinLiteral
    | CharacterLiteral
    | RealLiteral
    | NullLiteral
    | LongLiteral
    ;

stringLiteral
    : lineStringLiteral
    | multiLineStringLiteral
    ;

lineStringLiteral
    : QUOTE_OPEN (lineStringContent | lineStringExpression)* QUOTE_CLOSE
    ;

multiLineStringLiteral
    : TRIPLE_QUOTE_OPEN (multiLineStringContent | multiLineStringExpression | lineStringLiteral | MultiLineStringQuote)* TRIPLE_QUOTE_CLOSE
    ;

lineStringContent
    : LineStrText
    | LineStrEscapedChar
    | LineStrRef
    ;

lineStringExpression
    : LineStrExprStart expression RCURL
    ;

multiLineStringContent
    : MultiLineStrText
    | MultiLineStrEscapedChar
    | MultiLineStrRef
    ;

multiLineStringExpression
    : MultiLineStrExprStart expression RCURL
    ;

functionLiteral
    : annotations*
    ( LCURL NL* statements NL* RCURL
    | LCURL NL* lambdaParameters NL* ARROW NL* statements NL* RCURL )
    ;

lambdaParameters
    : lambdaParameter? (NL* COMMA NL* lambdaParameter)*
    ;

lambdaParameter
    : variableDeclaration
    | multiVariableDeclaration (NL* COLON NL* type)?
    ;

objectLiteral
    : OBJECT (NL* COLON NL* delegationSpecifiers)? NL* classBody
    ;

collectionLiteral
    : LSQUARE expression? (COMMA expression)* RSQUARE
    ;

thisExpression
    : THIS LabelReference?
    ;

superExpression
    : SUPER (LANGLE NL* type NL* RANGLE)? LabelReference?
    ;

conditionalExpression
    : ifExpression
    | whenExpression
    ;

ifExpression
    : IF NL* LPAREN expression RPAREN NL* controlStructureBody? SEMICOLON?
    (NL* ELSE NL* controlStructureBody?)?
    ;

controlStructureBody
    : block
    | expression
    ;

whenExpression
    : WHEN NL* (LPAREN expression RPAREN)? NL* LCURL NL* (whenEntry NL*)* NL* RCURL
    ;

whenEntry
    : whenCondition (NL* COMMA NL* whenCondition)* NL* ARROW NL* controlStructureBody semi?
    | ELSE NL* ARROW NL* controlStructureBody
    ;

whenCondition
    : expression
    | rangeTest
    | typeTest
    ;

rangeTest
    : inOperator NL* expression
    ;

typeTest
    : isOperator NL* type
    ;

tryExpression
    : TRY NL* block (NL* catchBlock)* (NL* finallyBlock)?
    ;

catchBlock
    : CATCH NL* LPAREN annotations* simpleIdentifier COLON userType RPAREN NL* block
    ;

finallyBlock
    : FINALLY NL* block
    ;

loopExpression
    : forExpression
    | whileExpression
    | doWhileExpression
    ;

forExpression
    : FOR NL* LPAREN annotations* (variableDeclaration | multiVariableDeclaration) IN expression RPAREN NL* controlStructureBody?
    ;

whileExpression
    : WHILE NL* LPAREN expression RPAREN NL* controlStructureBody?
    ;

doWhileExpression
    : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
    ;

jumpExpression
    : THROW NL* expression
    | (RETURN | RETURN_AT) expression?
    | CONTINUE | CONTINUE_AT
    | BREAK | BREAK_AT
    ;

callableReference
    : (userType (QUEST NL*)*)? NL* (COLONCOLON | Q_COLONCOLON) NL* (identifier | CLASS)
    ;

assignmentOperator
    : ASSIGNMENT
    | ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT
    ;

equalityOperator
    : EXCL_EQ
    | EXCL_EQEQ
    | EQEQ
    | EQEQEQ
    ;

comparisonOperator
    : LANGLE
    | RANGLE
    | LE
    | GE
    ;

inOperator
    : IN | NOT_IN
    ;

isOperator
    : IS | NOT_IS
    ;

additiveOperator
    : ADD | SUB
    ;

multiplicativeOperator
    : MULT
    | DIV
    | MOD
    ;

asOperator
    : AS
    | AS_SAFE
    | COLON
    ;

prefixUnaryOperator
    : INCR
    | DECR
    | ADD
    | SUB
    | EXCL
    ;

postfixUnaryOperator
    : INCR | DECR | EXCL EXCL
    ;

memberAccessOperator
    : DOT | QUEST DOT
    ;
    
modifierList
    : (annotations | modifier)+
    ;

modifier
    : (classModifier
    | memberModifier
    | visibilityModifier
    | varianceAnnotation
    | functionModifier
    | propertyModifier
    | inheritanceModifier
    | parameterModifier
    | typeParameterModifier) NL*
    ;

classModifier
    : ENUM
    | SEALED
    | ANNOTATION
    | DATA
    | INNER
    ;

memberModifier
    : OVERRIDE
    | LATEINIT
    ;

visibilityModifier
    : PUBLIC
    | PRIVATE
    | INTERNAL
    | PROTECTED
    ;

varianceAnnotation
    : IN | OUT
    ;

functionModifier
    : TAILREC
    | OPERATOR
    | INFIX
    | INLINE
    | EXTERNAL
    | SUSPEND
    ;

propertyModifier
    : CONST
    ;

inheritanceModifier
    : ABSTRACT
    | FINAL
    | OPEN
    ;

parameterModifier
    : VARARG
    | NOINLINE
    | CROSSINLINE
    ;

typeParameterModifier
    : REIFIED
    ;

labelDefinition
    : LabelDefinition NL*
    ;

annotations
    : (annotation | annotationList) NL*
    ;

annotation
    : annotationUseSiteTarget NL* COLON NL* unescapedAnnotation
    | LabelReference (NL* typeArguments)? (NL* valueArguments)?
    ;

annotationList
    : annotationUseSiteTarget COLON LSQUARE unescapedAnnotation+ RSQUARE
    | AT LSQUARE unescapedAnnotation+ RSQUARE
    ;

annotationUseSiteTarget
    : FIELD
    | FILE
    | PROPERTY
    | GET
    | SET
    | RECEIVER
    | PARAM
    | SETPARAM
    | DELEGATE
    ;

unescapedAnnotation
    : identifier typeArguments? valueArguments?
    ;

identifier
    : simpleIdentifier (NL* DOT simpleIdentifier)*
    ;

simpleIdentifier
    : Identifier
    //soft keywords:
    | ABSTRACT
    | ANNOTATION
    | BY
    | CATCH
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FINAL
    | FINALLY
    | GETTER
    | IMPORT
    | INFIX
    | INIT
    | INLINE
    | INNER
    | INTERNAL
    | LATEINIT
    | NOINLINE
    | OPEN
    | OPERATOR
    | OUT
    | OVERRIDE
    | PRIVATE
    | PROTECTED
    | PUBLIC
    | REIFIED
    | SEALED
    | TAILREC
    | SETTER
    | VARARG
    | WHERE
    //strong keywords
    | CONST
    | SUSPEND
    ;

semi: NL+ | SEMICOLON | SEMICOLON NL+;
