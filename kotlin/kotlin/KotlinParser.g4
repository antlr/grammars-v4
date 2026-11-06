/**
 * Kotlin Grammar for ANTLR v4
 *
 * Based on:
 * jetbrains.github.io/kotlin-spec/#_grammars_and_parsing
 * and
 * kotlinlang.org/docs/reference/grammar.html
 *
 * Tested on
 * github.com/JetBrains/kotlin/tree/master/compiler/testData/psi
 * (stale link)
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar KotlinParser;

options {
    tokenVocab = KotlinLexer;
}

kotlinFile
    : ShebangLine? NL* preamble anysemi* (topLevelObject (anysemi+ topLevelObject?)*)? EOF
    ;

script
    : ShebangLine? NL* preamble statements EOF
    ;

preamble
    : fileAnnotations? packageHeader importList
    ;

fileAnnotations
    : fileAnnotation+
    ;

fileAnnotation
    : (FILE_SITE NL* COLON NL* (LSQUARE NL* (unescapedAnnotation NL*)+ RSQUARE | unescapedAnnotation) semi?)+
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
    : classDeclaration
    | objectDeclaration
    | functionDeclaration
    | propertyDeclaration
    | fullMultiPropertyDeclaration
    | typeAlias
    ;

classDeclaration
    : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier (NL* typeParameters)? (
        NL* primaryConstructor
    )? (NL* COLON NL* delegationSpecifiers)? (NL* typeConstraints)? (
        NL* classBody
        | NL* enumClassBody
    )?
    ;

primaryConstructor
    : modifierList? (CONSTRUCTOR NL*)? classParameters
    ;

classParameters
    : LPAREN (classParameter (COMMA classParameter)* COMMA?)? RPAREN
    ;

classParameter
    : modifierList? (VAL | VAR)? simpleIdentifier COLON type (ASSIGNMENT expression)?
    ;

delegationSpecifiers
    : annotations* delegationSpecifier (NL* COMMA NL* annotations* delegationSpecifier)*
    ;

delegationSpecifier
    : constructorInvocation
    | userType
    | typeModifierList? functionType
    | explicitDelegation
    ;

constructorInvocation
    : userType callSuffix
    ;

explicitDelegation
    : (userType | typeModifierList? functionType) NL* BY NL* expression
    ;

classBody
    : LCURL NL* (classMemberDeclaration (anysemi+ classMemberDeclaration)* anysemi*)? NL* RCURL
    ;

classMemberDeclaration
    : (
        classDeclaration
        | functionDeclaration
        | objectDeclaration
        | companionObject
        | propertyDeclaration
        | anonymousInitializer
        | secondaryConstructor
        | typeAlias
    )
    ;

anonymousInitializer
    : INIT NL* block
    ;

secondaryConstructor
    : modifierList? CONSTRUCTOR NL* functionValueParameters (
        NL* COLON NL* constructorDelegationCall
    )? NL* block?
    ;

constructorDelegationCall
    : THIS NL* valueArguments
    | SUPER NL* valueArguments
    ;

enumClassBody
    : LCURL NL* enumEntries? (NL* SEMICOLON NL* (classMemberDeclaration (anysemi+ classMemberDeclaration)* anysemi*)?)? NL* RCURL
    ;

enumEntries
    : enumEntry (NL* COMMA NL* enumEntry)* (NL* COMMA)?
    ;

enumEntry
    : annotations* simpleIdentifier (NL* valueArguments)? (NL* classBody)?
    ;

functionDeclaration
    : functionModifierList? FUN (NL* typeParameters)? (NL* receiverType NL* DOT)? NL* simpleIdentifier NL* functionValueParameters (
        NL* COLON NL* type
    )? (NL* typeConstraints)? (NL* functionBody)?
    ;

functionValueParameters
    : LPAREN (functionValueParameter (COMMA functionValueParameter)* COMMA?)? RPAREN
    ;

functionValueParameter
    : modifierList? parameter (ASSIGNMENT expression)?
    ;

parameter
    : simpleIdentifier COLON type
    ;

receiverType
    : typeModifierList? (parenthesizedType | nullableType | typeReference)
    ;

functionBody
    : block
    | ASSIGNMENT NL* expression
    ;

objectDeclaration
    : modifierList? OBJECT NL* simpleIdentifier (
        NL* COLON NL* delegationSpecifiers
    )? (NL* classBody)?
    ;

companionObject
    : modifierList? COMPANION NL* modifierList? OBJECT (NL* simpleIdentifier)? (
        NL* COLON NL* delegationSpecifiers
    )? (NL* classBody)?
    ;

propertyDeclaration
    : propertyModifierList? (VAL | VAR) (NL* typeParameters)? (NL* type NL* DOT)? (
        NL* (multiVariableDeclaration | variableDeclaration)
    ) (NL* typeConstraints)? (NL* (BY | ASSIGNMENT) NL* expression)? (
        (NL* getter (semi setter)?
        | NL* setter (semi getter)?)
        | NL* explicitBackingField
    )?
    ;

fullMultiPropertyDeclaration
    : fullMultiVariableDeclaration NL* ASSIGNMENT NL* expression
    ;

explicitBackingField
    : FIELD (COLON type (ASSIGNMENT NL* expression)? | ASSIGNMENT NL* expression)
    ;

multiVariableDeclaration
    : multiParenVariableDeclaration
    | multiPositionalVariableDeclaration
    ;

multiParenVariableDeclaration
    : LPAREN NL* variableDeclarationWithRenaming (NL* COMMA NL* variableDeclarationWithRenaming)* (NL* COMMA)? NL* RPAREN
    ;

multiPositionalVariableDeclaration
    : LSQUARE NL* variableDeclaration (NL* COMMA NL* variableDeclaration)* (NL* COMMA)? NL* RSQUARE
    ;

variableDeclarationWithRenaming
    : variableDeclaration (NL* ASSIGNMENT NL* simpleIdentifier)?
    ;

fullMultiVariableDeclaration
    : fullMultiParenVariableDeclaration
    | fullMultiPositionalVariableDeclaration
    ;

fullMultiParenVariableDeclaration
    : LPAREN NL* fullVariableDeclarationWithRenaming (NL* COMMA NL* fullVariableDeclarationWithRenaming)* (NL* COMMA)? NL* RPAREN
    ;

fullMultiPositionalVariableDeclaration
    : LSQUARE NL* fullVariableDeclaration (NL* COMMA NL* fullVariableDeclaration)* (NL* COMMA)? NL* RSQUARE
    ;

fullVariableDeclaration
    : (VAL | VAR) NL* simpleIdentifier (NL* COLON NL* type)?
    ;

fullVariableDeclarationWithRenaming
    : fullVariableDeclaration (NL* ASSIGNMENT NL* simpleIdentifier)?
    ;

variableDeclaration
    : simpleIdentifier (COLON type)?
    ;

getter
    : modifierList? GETTER
    | modifierList? GETTER NL* LPAREN RPAREN (NL* COLON NL* type)? NL* (
        block
        | ASSIGNMENT NL* expression
    )
    ;

setter
    : modifierList? SETTER
    | modifierList? SETTER NL* LPAREN (annotations | parameterModifier)* (
        simpleIdentifier
        | parameter
    ) (NL* COMMA)? RPAREN (NL* COLON NL* type)? NL* functionBody
    ;

typeAlias
    : modifierList? TYPE_ALIAS NL* simpleIdentifier (NL* typeParameters)? NL* ASSIGNMENT NL* type
    ;

typeParameters
    : LANGLE NL* typeParameter (NL* COMMA NL* typeParameter)* (NL* COMMA)? NL* RANGLE
    ;

typeParameter
    : modifierList? NL* (simpleIdentifier | MULT) (NL* COLON NL* type)?
    ;

type
    : typeModifierList? (functionType | parenthesizedType | nullableType | typeReference)
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
    : (functionTypeReceiver NL* DOT NL*)? functionTypeParameters NL* ARROW (NL* type)
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
    : LPAREN NL* ((parameter | type) (NL* COMMA NL* (parameter | type))* (NL* COMMA)?)? NL* RPAREN
    ;

typeConstraints
    : WHERE NL* typeConstraint (NL* COMMA NL* typeConstraint)*
    ;

typeConstraint
    : annotations* simpleIdentifier NL* COLON NL* type
    ;

block
    : LCURL statements RCURL
    ;

statements
    : anysemi* (statement (anysemi+ statement?)*)?
    ;

statement
    : declaration
    | assignment
    | loopStatement
    | blockLevelExpression
    ;

blockLevelExpression
    : annotations* NL* expression
    ;

declaration
    : labelDefinition* (
        classDeclaration
        | objectDeclaration
        | functionDeclaration
        | propertyDeclaration
        | fullMultiPropertyDeclaration
        | typeAlias
    )
    ;

expression
    : expression equalityOperation NL* expression
    | expression NL* CONJ NL* expression
    | expression NL* DISJ NL* expression
    | comparison
    | infixExpression
    ;

assignment
    : prefixUnaryExpression assignmentOperator NL* expression
    ;

comparison
    : infixExpression comparisonOperator NL* infixExpression
    ;

infixExpression
    : infixExpression multiplicativeOperation NL* infixExpression
    | infixExpression additiveOperator NL* infixExpression
    | infixExpression RANGE NL* infixExpression
    | infixExpression simpleIdentifier NL* infixExpression
    | infixExpression NL* ELVIS NL* infixExpression
    | infixExpression inOperator NL* infixExpression
    | infixExpression isOperator NL* type
    | prefixUnaryExpression (NL* typeOperation NL* type)*
    ;

prefixUnaryExpression
    : (prefixUnaryOperation NL*)* postfixUnaryExpression
    ;

postfixUnaryExpression
    : atomicExpression postfixUnaryOperation* (postfixCallChain postfixUnaryOperation+)* postfixCallChain?
        postfixUnaryOperation*
    ;

atomicExpression
    : parenthesizedExpression
    | literalConstant
    | functionLiteral
    | thisExpression        // THIS labelReference?
    | superExpression       // SUPER (LANGLE type RANGLE)? labelReference?
    | ifExpression
    | whenExpression
    | tryExpression
    | objectLiteral
    | jumpExpression
    | collectionLiteral
    | callableReference
    | simpleIdentifier
    ;

parenthesizedExpression
    : LPAREN expression RPAREN
    ;

callSuffix
    : callSuffixWithoutLambda annotatedLambda?
    | annotatedLambda
    ;

postfixCallChain
    : callSuffixWithoutLambda* callSuffixWithLambda callSuffixWithoutLambda*
    | callSuffixWithoutLambda+
    ;

callSuffixWithoutLambda
    : typeArguments valueArguments?
    | valueArguments
    ;

callSuffixWithLambda
    : callSuffixWithoutLambda? annotatedLambda
    ;

annotatedLambda
    : unescapedAnnotation* LabelDefinition? NL* functionLiteral
    ;

arrayAccess
    : LSQUARE expression (COMMA expression)* COMMA? RSQUARE
    ;

valueArguments
    : LPAREN (valueArgument (COMMA valueArgument)* (NL* COMMA)?)? RPAREN
    ;

typeArguments
    : LANGLE NL* typeProjection (NL* COMMA typeProjection)* (NL* COMMA)? NL* RANGLE QUEST?
    ;

typeProjection
    : typeProjectionModifierList? type
    | MULT
    ;

typeProjectionModifierList
    : (varianceAnnotation NL* | annotations)+
    ;

valueArgument
    : (simpleIdentifier NL* ASSIGNMENT NL*)? MULT? NL* expression
    ;

literalConstant
    : BooleanLiteral
    | IntegerLiteral
    | stringLiteral
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
    : TRIPLE_QUOTE_OPEN (
        multiLineStringContent
        | multiLineStringExpression
        | lineStringLiteral
        | MultiLineStringQuote
    )* TRIPLE_QUOTE_CLOSE
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
    | MultiLineStrRef
    ;

multiLineStringExpression
    : MultiLineStrExprStart expression RCURL
    ;

functionLiteral
    : annotations* (
        LCURL NL* statements NL* RCURL
        | LCURL NL* lambdaParameters NL* ARROW NL* statements NL* RCURL
    )
    ;

lambdaParameters
    : (lambdaParameter (NL* COMMA NL* lambdaParameter)* (NL* COMMA)?)?
    ;

lambdaParameter
    : variableDeclaration
    | multiVariableDeclaration (NL* COLON NL* type)?
    | fullMultiVariableDeclaration (NL* COLON NL* type)?
    ;

// https://kotlinlang.org/docs/reference/grammar.html#objectLiteral
objectLiteral
    : OBJECT (NL* COLON NL* delegationSpecifiers)? NL* classBody?
    ;

collectionLiteral
    : LSQUARE (expression (COMMA expression)* COMMA?)? RSQUARE
    ;

thisExpression
    : THIS LabelReference?
    ;

superExpression
    : SUPER (LANGLE NL* type NL* RANGLE)? LabelReference?
    ;

ifExpression
    : IF NL* LPAREN expression RPAREN NL* controlStructureBody? SEMICOLON? (
        NL* ELSE NL* controlStructureBody?
    )?
    ;

controlStructureBody
    : block
    | statement
    ;

whenExpression
    : WHEN NL* (LPAREN whenSubject RPAREN)? NL* LCURL NL* (whenEntry NL*)* NL* RCURL
    ;

whenSubject
    : whenSubjectVariable
    | expression
    ;

whenSubjectVariable
    : annotations* VAL NL* variableDeclaration NL* ASSIGNMENT NL* expression
    ;

whenEntry
    : whenCondition (NL* COMMA NL* whenCondition)* (NL* COMMA)? NL* ARROW NL* controlStructureBody semi?
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
    : TRY NL* block ((NL* catchBlock)+ (NL* finallyBlock)? | NL* finallyBlock)
    ;

catchBlock
    : CATCH NL* LPAREN annotations* simpleIdentifier COLON type (NL* COMMA)? RPAREN NL* block
    ;

finallyBlock
    : FINALLY NL* block
    ;

loopStatement
    : labelDefinition* (forExpression | whileExpression | doWhileExpression)
    ;

forExpression
    : FOR NL* LPAREN annotations* (variableDeclaration | multiVariableDeclaration | fullMultiVariableDeclaration) IN expression RPAREN NL* controlStructureBody?
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
    | CONTINUE
    | CONTINUE_AT
    | BREAK
    | BREAK_AT
    ;

callableReference
    : (userType (QUEST NL*)*)? NL* (COLONCOLON | Q_COLONCOLON) NL* (simpleIdentifier | CLASS)
    ;

assignmentOperator
    : ASSIGNMENT
    | ADD_ASSIGNMENT
    | SUB_ASSIGNMENT
    | MULT_ASSIGNMENT
    | DIV_ASSIGNMENT
    | MOD_ASSIGNMENT
    ;

equalityOperation
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
    : IN
    | NOT_IN
    ;

isOperator
    : IS
    | NOT_IS
    ;

additiveOperator
    : ADD
    | SUB
    ;

multiplicativeOperation
    : MULT
    | DIV
    | MOD
    ;

typeOperation
    : AS
    | AS_SAFE
    ;

prefixUnaryOperation
    : INCR
    | DECR
    | ADD
    | SUB
    | EXCL
    | EXCL_EXCL
    | EXCL_WS
    | annotations
    | labelDefinition
    ;

postfixUnaryOperation
    : INCR
    | DECR
    | EXCL_EXCL
    | arrayAccess
    | NL* COLONCOLON NL* (simpleIdentifier | CLASS)
    | NL* memberAccessOperator NL* postfixUnaryExpression
    ;

memberAccessOperator
    : DOT
    | QUEST DOT
    ;

modifierList
    : (annotations | modifier)+
    ;

functionModifierList
    : (annotations | modifier | contextModifier)+
    ;

propertyModifierList
    : (annotations | modifier | contextModifier)+
    ;

contextParameters
    : LPAREN (contextParameter (COMMA contextParameter)* COMMA?)? RPAREN
    ;

contextParameter
    : annotations* parameter
    ;

contextModifier
    : CONTEXT
    contextParameters NL*
    ;

modifier
    : (
        classModifier
        | memberModifier
        | visibilityModifier
        | varianceAnnotation
        | functionModifier
        | propertyModifier
        | inheritanceModifier
        | parameterModifier
        | typeParameterModifier
    ) NL*
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
    : IN
    | OUT
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
    | annotationName (NL* DOT NL* simpleIdentifier)* (NL* typeArguments)? (NL* valueArguments)?
    ;

annotationName
    : LabelReference
    | FIELD_SITE
    | FILE_SITE
    | PROPERTY_SITE
    | GET_SITE
    | SET_SITE
    | RECEIVER_SITE
    | PARAM_SITE
    | SETPARAM_SITE
    | DELEGATE_SITE
    ;

annotationList
    : annotationUseSiteTarget NL* COLON NL* LSQUARE unescapedAnnotation+ RSQUARE
    | AT LSQUARE unescapedAnnotation+ RSQUARE
    ;

annotationUseSiteTarget
    : FIELD_SITE
    | FILE_SITE
    | PROPERTY_SITE
    | GET_SITE
    | SET_SITE
    | RECEIVER_SITE
    | PARAM_SITE
    | SETPARAM_SITE
    | DELEGATE_SITE
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
    | CONTEXT
    | COMPANION
    | CONSTRUCTOR
    | CROSSINLINE
    | DATA
    | DYNAMIC
    | ENUM
    | EXTERNAL
    | FIELD
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

semi
    : NL+
    | NL* SEMICOLON NL*
    ;

anysemi
    : NL
    | SEMICOLON
    ;