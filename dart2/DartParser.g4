// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// CHANGES:
//
// v0.18 Add support for enhanced `enum` declarations.
//
// v0.17 (58d917e7573c359580ade43845004dbbc62220d5) Correct `uri` to allow
// multi-line strings (raw and non-raw).
//
// v0.16 (284695f1937c262523a9a11b9084213f889c83e0) Correct instance variable
// declaration syntax such that `covariant late final` is allowed.
//
// v0.15 (6facd6dfdafa2953e8523348220d3129ea884678) Add support for
// constructor tearoffs and explicitly instantiated function tearoffs and
// type literals.
//
// v0.14 (f65c20124edd9e04f7b3a6f014f40c16f51052f6) Correct `partHeader`
// to allow uri syntax in a `PART OF` directive.
//
// v0.13 (bb5cb79a2fd57d6a480b922bc650d5cd15948753) Introduce non-terminals
// `builtinIdentifier` and `reservedWord`; update `typeAlias` to enable
// non-function type aliases; add missing `metadata` to formal parameter
// declarations; correct `symbolLiteral` to allow `VOID`;

// v0.12 (82403371ac00ddf004be60fa7b705474d2864509) Cf. language issue #1341:
// correct `metadata`. Change `qualifiedName` such that it only includes the
// cases with a '.'; the remaining case is added where `qualifiedName` is used.
//
// v0.11 (67c703063d5b68c9e132edbaf34dfe375851f5a6) Corrections, mainly:
// `fieldFormalParameter` now allows `?` on the parameter type; cascade was
// reorganized in the spec, it is now reorganized similarly here; `?` was
// removed from argumentPart (null-aware invocation was never added).
//
// v0.10 (8ccdb9ae796d543e4ad8f339c847c02b09018d2d) Simplify grammar by making
// `constructorInvocation` an alternative in `primary`.
//
// v0.9 (f4d7951a88e1b738e22b768c3bc72bf1a1062365) Introduce abstract and
// external variables.
//
// v0.8 (a9ea9365ad8a3e3b59115bd889a55b6aa2c5a5fa) Change null-aware
// invocations of `operator []` and `operator []=` to not have a period.
//
// v0.7 (6826faf583f6a543b1a0e2e85bd6a8042607ce00) Introduce extension and
// mixin declarations. Revise rules about string literals and string
// interpolation. Reorganize "keywords" (built-in identifiers, reserved words,
// other words that are specified in the grammar and not parsed as IDENTIFIER)
// into explicitly marked groups. Change the cascade syntax to be
// compositional.
//
// v0.6 (a58052974ec2b4b334922c5227b043ed2b9c2cc5) Introduce syntax associated
// with null safety.
//
// v0.5 (56793b3d4714d4818d855a72074d5295489aef3f) Stop treating `ASYNC` as a
// conditional reserved word (only `AWAIT` and `YIELD` get this treatment).
//
// v0.4 Added support for 'unified collections' (spreads and control flow
// in collection literals).
//
// v0.3 Updated to use ANTLR v4 rather than antlr3.
//
// v0.2 Changed top level variable declarations to avoid redundant and
// misleading occurrence of (FINAL|CONST).
//
// v0.1 First version available in the SDK github repository. Covers the
// Dart language as specified in the language specification based on the
// many grammar rule snippets. That grammar was then adjusted to remove
// known issues (e.g., misplaced metadata) and to resolve ambiguities.

parser grammar DartParser;

options { tokenVocab = DartLexer; superClass = DartParserBase; }

// ---------------------------------------- Grammar rules.

libraryDefinition
    :    FEFF? SCRIPT_TAG?
         libraryName?
         importOrExport*
         partDirective*
         (metadata topLevelDefinition)*
         EOF
    ;

topLevelDefinition
    :    classDeclaration
    |    mixinDeclaration
    |    extensionDeclaration
    |    enumType
    |    typeAlias
    |    EXTERNAL functionSignature SCO
    |    EXTERNAL getterSignature SCO
    |    EXTERNAL setterSignature SCO
    |    EXTERNAL finalVarOrType identifierList SCO
    |    getterSignature functionBody
    |    setterSignature functionBody
    |    functionSignature functionBody
    |    (FINAL | CONST) type? staticFinalDeclarationList SCO
    |    LATE FINAL type? initializedIdentifierList SCO
    |    LATE? varOrType identifier (E expression)?
         (COM initializedIdentifier)* SCO
    ;

declaredIdentifier
    :    COVARIANT? finalConstVarOrType identifier
    ;

finalConstVarOrType
    :    LATE? FINAL type?
    |    CONST type?
    |    LATE? varOrType
    ;

finalVarOrType
    :    FINAL type?
    |    varOrType
    ;

varOrType
    :    VAR
    |    type
    ;

initializedIdentifier
    :    identifier (E expression)?
    ;

initializedIdentifierList
    :    initializedIdentifier (COM initializedIdentifier)*
    ;

functionSignature
    :    type? identifierNotFUNCTION formalParameterPart
    ;

functionBodyPrefix
    :    ASYNC? EGT
    |    (ASYNC | ASYNC MUL | SYNC MUL)? LBRACE
    ;

functionBody
    :    EGT { $parser.startNonAsyncFunction(); } expression { $parser.endFunction(); } SCO
    |    { $parser.startNonAsyncFunction(); } block { $parser.endFunction(); }
    |    ASYNC EGT
         { $parser.startAsyncFunction(); } expression { $parser.endFunction(); } SCO
    |    (ASYNC | ASYNC MUL | SYNC MUL)
         { $parser.startAsyncFunction(); } block { $parser.endFunction(); }
    ;

block
    :    LBRACE statements RBRACE
    ;

formalParameterPart
    :    typeParameters? formalParameterList
    ;

formalParameterList
    :    OP CP
    |    OP normalFormalParameters COM? CP
    |    OP normalFormalParameters COM optionalOrNamedFormalParameters CP
    |    OP optionalOrNamedFormalParameters CP
    ;

normalFormalParameters
    :    normalFormalParameter (COM normalFormalParameter)*
    ;

optionalOrNamedFormalParameters
    :    optionalPositionalFormalParameters
    |    namedFormalParameters
    ;

optionalPositionalFormalParameters
    :    OB defaultFormalParameter (COM defaultFormalParameter)* COM? CB
    ;

namedFormalParameters
    :    LBRACE defaultNamedParameter (COM defaultNamedParameter)* COM? RBRACE
    ;

normalFormalParameter
    :    metadata normalFormalParameterNoMetadata
    ;

normalFormalParameterNoMetadata
    :    functionFormalParameter
    |    fieldFormalParameter
    |    simpleFormalParameter
    ;

// NB: It is an anomaly that a functionFormalParameter cannot be FINAL.
functionFormalParameter
    :    COVARIANT? type? identifierNotFUNCTION formalParameterPart Q?
    ;

simpleFormalParameter
    :    declaredIdentifier
    |    COVARIANT? identifier
    ;

// NB: It is an anomaly that VAR can be a return type (`var this.x()`).
fieldFormalParameter
    :    finalConstVarOrType? THIS DOT identifier (formalParameterPart Q?)?
    ;

defaultFormalParameter
    :    normalFormalParameter (E expression)?
    ;

defaultNamedParameter
    :    REQUIRED? normalFormalParameter ((CO | E) expression)?
    ;

typeWithParameters
    :    typeIdentifier typeParameters?
    ;

classDeclaration
    :    ABSTRACT? CLASS typeWithParameters superclass? mixins? interfaces?
         LBRACE (metadata classMemberDefinition)* RBRACE
    |    ABSTRACT? CLASS mixinApplicationClass
    ;

superclass
    :    EXTENDS typeNotVoidNotFunction
    ;

mixins
    :    WITH typeNotVoidNotFunctionList
    ;

interfaces
    :    IMPLEMENTS typeNotVoidNotFunctionList
    ;

classMemberDefinition
    :    methodSignature functionBody
    |    declaration SCO
    ;

mixinApplicationClass
    :    typeWithParameters E mixinApplication SCO
    ;

mixinDeclaration
    :    MIXIN typeIdentifier typeParameters?
         (ON typeNotVoidNotFunctionList)? interfaces?
         LBRACE (metadata mixinMemberDefinition)* RBRACE
    ;

// TODO: We will probably want to make this more strict.
mixinMemberDefinition
    :    classMemberDefinition
    ;

extensionDeclaration
    :    EXTENSION identifier? typeParameters? ON type
         LBRACE (metadata extensionMemberDefinition)* RBRACE
    ;

// TODO: We might want to make this more strict.
extensionMemberDefinition
    :    classMemberDefinition
    ;

methodSignature
    :    constructorSignature initializers
    |    factoryConstructorSignature
    |    STATIC? functionSignature
    |    STATIC? getterSignature
    |    STATIC? setterSignature
    |    operatorSignature
    |    constructorSignature
    ;

declaration
    :    EXTERNAL factoryConstructorSignature
    |    EXTERNAL constantConstructorSignature
    |    EXTERNAL constructorSignature
    |    (EXTERNAL STATIC?)? getterSignature
    |    (EXTERNAL STATIC?)? setterSignature
    |    (EXTERNAL STATIC?)? functionSignature
    |    EXTERNAL (STATIC? finalVarOrType | COVARIANT varOrType) identifierList
    |    ABSTRACT (finalVarOrType | COVARIANT varOrType) identifierList
    |    EXTERNAL? operatorSignature
    |    STATIC (FINAL | CONST) type? staticFinalDeclarationList
    |    STATIC LATE FINAL type? initializedIdentifierList
    |    STATIC LATE? varOrType initializedIdentifierList
    |    COVARIANT LATE FINAL type? identifierList
    |    COVARIANT LATE? varOrType initializedIdentifierList
    |    LATE? (FINAL type? | varOrType) initializedIdentifierList
    |    redirectingFactoryConstructorSignature
    |    constantConstructorSignature (redirection | initializers)?
    |    constructorSignature (redirection | initializers)?
    ;

staticFinalDeclarationList
    :    staticFinalDeclaration (COM staticFinalDeclaration)*
    ;

staticFinalDeclaration
    :    identifier E expression
    ;

operatorSignature
    :    type? OPERATOR operator formalParameterList
    ;

operator
    :    SQ
    |    binaryOperator
    |    OB CB
    |    OB CB E
    ;

binaryOperator
    :    multiplicativeOperator
    |    additiveOperator
    |    shiftOperator
    |    relationalOperator
    |    EE
    |    bitwiseOperator
    ;

getterSignature
    :    type? GET identifier
    ;

setterSignature
    :    type? SET identifier formalParameterList
    ;

constructorSignature
    :    constructorName formalParameterList
    ;

constructorName
    :    typeIdentifier (DOT (identifier | NEW))?
    ;

redirection
    :    CO THIS (DOT (identifier | NEW))? arguments
    ;

initializers
    :    CO initializerListEntry (COM initializerListEntry)*
    ;

initializerListEntry
    :    SUPER arguments
    |    SUPER DOT (identifier | NEW) arguments
    |    fieldInitializer
    |    assertion
    ;

fieldInitializer
    :    (THIS DOT)? identifier E initializerExpression
    ;

initializerExpression
    :    conditionalExpression
    |    cascade
    ;

factoryConstructorSignature
    :    CONST? FACTORY constructorName formalParameterList
    ;

redirectingFactoryConstructorSignature
    :    CONST? FACTORY constructorName formalParameterList E
         constructorDesignation
    ;

constantConstructorSignature
    :    CONST constructorName formalParameterList
    ;

mixinApplication
    :    typeNotVoidNotFunction mixins interfaces?
    ;

enumType
    :    ENUM typeIdentifier typeParameters? mixins? interfaces? LBRACE
         enumEntry (COM enumEntry)* (COM)?
         (SCO (metadata classMemberDefinition)*)?
         RBRACE
    ;

enumEntry
    :    metadata identifier argumentPart?
    |    metadata identifier typeArguments? DOT identifier arguments
    ;

typeParameter
    :    metadata typeIdentifier (EXTENDS typeNotVoid)?
    ;

typeParameters
    :    LT typeParameter (COM typeParameter)* GT
    ;

metadata
    :    (AT metadatum)*
    ;

metadatum
    :    constructorDesignation arguments
    |    identifier
    |    qualifiedName
    ;

expression
    :    functionExpression
    |    throwExpression
    |    assignableExpression assignmentOperator expression
    |    conditionalExpression
    |    cascade
    ;

expressionWithoutCascade
    :    functionExpressionWithoutCascade
    |    throwExpressionWithoutCascade
    |    assignableExpression assignmentOperator expressionWithoutCascade
    |    conditionalExpression
    ;

expressionList
    :    expression (COM expression)*
    ;

primary
    :    thisExpression
    |    SUPER unconditionalAssignableSelector
    |    constObjectExpression
    |    newExpression
    |    constructorInvocation
    |    functionPrimary
    |    OP expression CP
    |    literal
    |    identifier
    |    constructorTearoff
    ;

constructorInvocation
    :    typeName typeArguments DOT NEW arguments
    |    typeName DOT NEW arguments
    ;

literal
    :    nullLiteral
    |    booleanLiteral
    |    numericLiteral
    |    stringLiteral
    |    symbolLiteral
    |    setOrMapLiteral
    |    listLiteral
    ;

nullLiteral
    :    NULL
    ;

numericLiteral
    :    NUMBER
    |    HEX_NUMBER
    ;

booleanLiteral
    :    TRUE
    |    FALSE
    ;

stringLiteral
    :    (multiLineString | singleLineString)+
    ;

// Not used in the specification (needed here for <uri>).
stringLiteralWithoutInterpolation
    :    singleStringWithoutInterpolation+
    ;

setOrMapLiteral
    : CONST? typeArguments? LBRACE elements? RBRACE
    ;

listLiteral
    : CONST? typeArguments? OB elements? CB
    ;

elements
    : element (COM element)* COM?
    ;

element
    : expressionElement
    | mapElement
    | spreadElement
    | ifElement
    | forElement
    ;

expressionElement
    : expression
    ;

mapElement
    : expression CO expression
    ;

spreadElement
    : (DDD | DDDQ) expression
    ;

ifElement
    : IF OP expression CP element (ELSE element)?
    ;

forElement
    : AWAIT? FOR OP forLoopParts CP element
    ;

constructorTearoff
    :    typeName typeArguments? DOT NEW
    ;

throwExpression
    :    THROW expression
    ;

throwExpressionWithoutCascade
    :    THROW expressionWithoutCascade
    ;

functionExpression
    :    formalParameterPart functionExpressionBody
    ;

functionExpressionBody
    :    EGT { $parser.startNonAsyncFunction(); } expression { $parser.endFunction(); }
    |    ASYNC EGT { $parser.startAsyncFunction(); } expression { $parser.endFunction(); }
    ;

functionExpressionBodyPrefix
    :    ASYNC? EGT
    ;

functionExpressionWithoutCascade
    :    formalParameterPart functionExpressionWithoutCascadeBody
    ;

functionExpressionWithoutCascadeBody
    :    EGT { $parser.startNonAsyncFunction(); }
         expressionWithoutCascade { $parser.endFunction(); }
    |    ASYNC EGT { $parser.startAsyncFunction(); }
         expressionWithoutCascade { $parser.endFunction(); }
    ;

functionPrimary
    :    formalParameterPart functionPrimaryBody
    ;

functionPrimaryBody
    :    { $parser.startNonAsyncFunction(); } block { $parser.endFunction(); }
    |    (ASYNC | ASYNC MUL | SYNC MUL)
         { $parser.startAsyncFunction(); } block { $parser.endFunction(); }
    ;

functionPrimaryBodyPrefix
    : (ASYNC | ASYNC MUL | SYNC MUL)? LBRACE
    ;

thisExpression
    :    THIS
    ;

newExpression
    :    NEW constructorDesignation arguments
    ;

constObjectExpression
    :    CONST constructorDesignation arguments
    ;

arguments
    :    OP (argumentList COM?)? CP
    ;

argumentList
    :    namedArgument (COM namedArgument)*
    |    expressionList (COM namedArgument)*
    ;

namedArgument
    :    label expression
    ;

cascade
    :     cascade DD cascadeSection
    |     conditionalExpression (QDD | DD) cascadeSection
    ;

cascadeSection
    :    cascadeSelector cascadeSectionTail
    ;

cascadeSelector
    :    OB expression CB
    |    identifier
    ;

cascadeSectionTail
    :    cascadeAssignment
    |    selector* (assignableSelector cascadeAssignment)?
    ;

cascadeAssignment
    :    assignmentOperator expressionWithoutCascade
    ;

assignmentOperator
    :    E
    |    compoundAssignmentOperator
    ;

compoundAssignmentOperator
    :    MULE
    |    SLE
    |    SQSE
    |    MODE
    |    PE
    |    ME
    |    LLE
    |    GT GT GT E
    |    GT GT E
    |    ANDE
    |    TE
    |    BE
    |    QQE
    ;

conditionalExpression
    :    ifNullExpression
         (Q expressionWithoutCascade CO expressionWithoutCascade)?
    ;

ifNullExpression
    :    logicalOrExpression (QQ logicalOrExpression)*
    ;

logicalOrExpression
    :    logicalAndExpression (BB logicalAndExpression)*
    ;

logicalAndExpression
    :    equalityExpression (AA equalityExpression)*
    ;

equalityExpression
    :    relationalExpression (equalityOperator relationalExpression)?
    |    SUPER equalityOperator relationalExpression
    ;

equalityOperator
    :    EE
    |    NE
    ;

relationalExpression
    :    bitwiseOrExpression
         (typeTest | typeCast | relationalOperator bitwiseOrExpression)?
    |    SUPER relationalOperator bitwiseOrExpression
    ;

relationalOperator
    :    GT E
    |    GT
    |    LE
    |    LT
    ;

bitwiseOrExpression
    :    bitwiseXorExpression (B bitwiseXorExpression)*
    |    SUPER (B bitwiseXorExpression)+
    ;

bitwiseXorExpression
    :    bitwiseAndExpression (TIL bitwiseAndExpression)*
    |    SUPER (TIL bitwiseAndExpression)+
    ;

bitwiseAndExpression
    :    shiftExpression (AND shiftExpression)*
    |    SUPER (AND shiftExpression)+
    ;

bitwiseOperator
    :    AND
    |    TIL
    |    B
    ;

shiftExpression
    :    additiveExpression (shiftOperator additiveExpression)*
    |    SUPER (shiftOperator additiveExpression)+
    ;

shiftOperator
    :    LL
    |    GT GT GT
    |    GT GT
    ;

additiveExpression
    :    multiplicativeExpression (additiveOperator multiplicativeExpression)*
    |    SUPER (additiveOperator multiplicativeExpression)+
    ;

additiveOperator
    :    P
    |    MIN
    ;

multiplicativeExpression
    :    unaryExpression (multiplicativeOperator unaryExpression)*
    |    SUPER (multiplicativeOperator unaryExpression)+
    ;

multiplicativeOperator
    :    MUL
    |    SL
    |    MOD
    |    SQE
    ;

unaryExpression
    :    prefixOperator unaryExpression
    |    awaitExpression
    |    postfixExpression
    |    (minusOperator | tildeOperator) SUPER
    |    incrementOperator assignableExpression
    ;

prefixOperator
    :    minusOperator
    |    negationOperator
    |    tildeOperator
    ;

minusOperator
    :    MIN
    ;

negationOperator
    :    BANG
    ;

tildeOperator
    :    SQ
    ;

awaitExpression
    :    AWAIT unaryExpression
    ;

postfixExpression
    :    assignableExpression postfixOperator
    |    primary selector*
    ;

postfixOperator
    :    incrementOperator
    ;

selector
    :    BANG
    |    assignableSelector
    |    argumentPart
    |    typeArguments
    ;

argumentPart
    :    typeArguments? arguments
    ;

incrementOperator
    :    PP
    |    MINMIN
    ;

assignableExpression
    :    SUPER unconditionalAssignableSelector
    |    primary assignableSelectorPart
    |    identifier
    ;

assignableSelectorPart
    :    selector* assignableSelector
    ;

unconditionalAssignableSelector
    :    OB expression CB
    |    DOT identifier
    ;

assignableSelector
    :    unconditionalAssignableSelector
    |    QD identifier
    |    Q OB expression CB
    ;

identifierNotFUNCTION
    :    IDENTIFIER
    |    builtInIdentifier
    |    ASYNC // Not a built-in identifier.
    |    HIDE // Not a built-in identifier.
    |    OF // Not a built-in identifier.
    |    ON // Not a built-in identifier.
    |    SHOW // Not a built-in identifier.
    |    SYNC // Not a built-in identifier.
    |    { $parser.pred1() }? (AWAIT|YIELD)
    ;

identifier
    :    identifierNotFUNCTION
    |    FUNCTION // Built-in identifier that can be used as a type.
    ;

qualifiedName
    :    typeIdentifier DOT (identifier | NEW)
    |    typeIdentifier DOT typeIdentifier DOT (identifier | NEW)
    ;

typeIdentifier
    :    IDENTIFIER
    |    DYNAMIC // Built-in identifier that can be used as a type.
    |    ASYNC // Not a built-in identifier.
    |    HIDE // Not a built-in identifier.
    |    OF // Not a built-in identifier.
    |    ON // Not a built-in identifier.
    |    SHOW // Not a built-in identifier.
    |    SYNC // Not a built-in identifier.
    |    { $parser.pred1() }? (AWAIT|YIELD)
    ;

typeTest
    :    isOperator typeNotVoid
    ;

isOperator
    :    IS BANG?
    ;

typeCast
    :    asOperator typeNotVoid
    ;

asOperator
    :    AS
    ;

statements
    :    statement*
    ;

statement
    :    label* nonLabelledStatement
    ;

// Exception in the language specification: An expressionStatement cannot
// start with LBRACE. We force anything that starts with LBRACE to be a block,
// which will prevent an expressionStatement from starting with LBRACE, and
// which will not interfere with the recognition of any other case. If we
// add another statement which can start with LBRACE we must adjust this
// check.
nonLabelledStatement
    :    block
    |    localVariableDeclaration
    |    forStatement
    |    whileStatement
    |    doStatement
    |    switchStatement
    |    ifStatement
    |    rethrowStatement
    |    tryStatement
    |    breakStatement
    |    continueStatement
    |    returnStatement
    |    localFunctionDeclaration
    |    assertStatement
    |    yieldStatement
    |    yieldEachStatement
    |    expressionStatement
    ;

expressionStatement
    :    expression? SCO
    ;

localVariableDeclaration
    :    metadata initializedVariableDeclaration SCO
    ;

initializedVariableDeclaration
    :    declaredIdentifier (E expression)? (COM initializedIdentifier)*
    ;

localFunctionDeclaration
    :    metadata functionSignature functionBody
    ;

ifStatement
    :    IF OP expression CP statement (ELSE statement)?
    ;

forStatement
    :    AWAIT? FOR OP forLoopParts CP statement
    ;

forLoopParts
    :    metadata declaredIdentifier IN expression
    |    metadata identifier IN expression
    |    forInitializerStatement expression? SCO expressionList?
    ;

// The localVariableDeclaration cannot be CONST, but that can
// be enforced in a later phase, and the grammar allows it.
forInitializerStatement
    :    localVariableDeclaration
    |    expression? SCO
    ;

whileStatement
    :    WHILE OP expression CP statement
    ;

doStatement
    :    DO statement WHILE OP expression CP SCO
    ;

switchStatement
    :    SWITCH OP expression CP LBRACE switchCase* defaultCase? RBRACE
    ;

switchCase
    :    label* CASE expression CO statements
    ;

defaultCase
    :    label* DEFAULT CO statements
    ;

rethrowStatement
    :    RETHROW SCO
    ;

tryStatement
    :    TRY block (onParts finallyPart? | finallyPart)
    ;

onPart
    :    catchPart block
    |    ON typeNotVoid catchPart? block
    ;

onParts
    :    onPart onParts
    |    onPart
    ;

catchPart
    :    CATCH OP identifier (COM identifier)? CP
    ;

finallyPart
    :    FINALLY block
    ;

returnStatement
    :    RETURN expression? SCO
    ;

label
    :    identifier CO
    ;

breakStatement
    :    BREAK identifier? SCO
    ;

continueStatement
    :    CONTINUE identifier? SCO
    ;

yieldStatement
    :    YIELD expression SCO
    ;

yieldEachStatement
    :    YIELD MUL expression SCO
    ;

assertStatement
    :    assertion SCO
    ;

assertion
    :    ASSERT OP expression (COM expression)? COM? CP
    ;

libraryName
    :    metadata LIBRARY dottedIdentifierList SCO
    ;

dottedIdentifierList
    :    identifier (DOT identifier)*
    ;

importOrExport
    :    libraryImport
    |    libraryExport
    ;

libraryImport
    :    metadata importSpecification
    ;

importSpecification
    :    IMPORT configurableUri (DEFERRED? AS identifier)? combinator* SCO
    ;

combinator
    :    SHOW identifierList
    |    HIDE identifierList
    ;

identifierList
    :    identifier (COM identifier)*
    ;

libraryExport
    :    metadata EXPORT uri combinator* SCO
    ;

partDirective
    :    metadata PART uri SCO
    ;

partHeader
    :    metadata PART OF (dottedIdentifierList | uri)SCO
    ;

partDeclaration
    :    partHeader topLevelDefinition* EOF
    ;

// In the specification a plain <stringLiteral> is used.
// TODO(eernst): Check whether it creates ambiguities to do that.
uri
    :    stringLiteralWithoutInterpolation
    ;

configurableUri
    :    uri configurationUri*
    ;

configurationUri
    :    IF OP uriTest CP uri
    ;

uriTest
    :    dottedIdentifierList (EE stringLiteral)?
    ;

type
    :    functionType Q?
    |    typeNotFunction
    ;

typeNotVoid
    :    functionType Q?
    |    typeNotVoidNotFunction
    ;

typeNotFunction
    :    typeNotVoidNotFunction
    |    VOID
    ;

typeNotVoidNotFunction
    :    typeName typeArguments? Q?
    |    FUNCTION Q?
    ;

typeName
    :    typeIdentifier (DOT typeIdentifier)?
    ;

typeArguments
    :    LT typeList GT
    ;

typeList
    :    type (COM type)*
    ;

typeNotVoidNotFunctionList
    :    typeNotVoidNotFunction (COM typeNotVoidNotFunction)*
    ;

typeAlias
    :    TYPEDEF typeIdentifier typeParameters? E type SCO
    |    TYPEDEF functionTypeAlias
    ;

functionTypeAlias
    :    functionPrefix formalParameterPart SCO
    ;

functionPrefix
    :    type identifier
    |    identifier
    ;

functionTypeTail
    :    FUNCTION typeParameters? parameterTypeList
    ;

functionTypeTails
    :    functionTypeTail Q? functionTypeTails
    |    functionTypeTail
    ;

functionType
    :    functionTypeTails
    |    typeNotFunction functionTypeTails
    ;

parameterTypeList
    :    OP CP
    |    OP normalParameterTypes COM optionalParameterTypes CP
    |    OP normalParameterTypes COM? CP
    |    OP optionalParameterTypes CP
    ;

normalParameterTypes
    :    normalParameterType (COM normalParameterType)*
    ;

normalParameterType
    :    metadata typedIdentifier
    |    metadata type
    ;

optionalParameterTypes
    :    optionalPositionalParameterTypes
    |    namedParameterTypes
    ;

optionalPositionalParameterTypes
    :    OB normalParameterTypes COM? CB
    ;

namedParameterTypes
    :    LBRACE namedParameterType (COM namedParameterType)* COM? RBRACE
    ;

namedParameterType
    :    metadata REQUIRED? typedIdentifier
    ;

typedIdentifier
    :    type identifier
    ;

constructorDesignation
    :    typeIdentifier
    |    qualifiedName
    |    typeName typeArguments (DOT (identifier | NEW))?
    ;

symbolLiteral
    :    POUND (operator | (identifier (DOT identifier)*) | VOID)
    ;

// Not used in the specification (needed here for <uri>).
singleStringWithoutInterpolation
    :    RAW_SINGLE_LINE_STRING
    |    RAW_MULTI_LINE_STRING
    |    SINGLE_LINE_STRING_DQ_BEGIN_END
    |    SINGLE_LINE_STRING_SQ_BEGIN_END
    |    MULTI_LINE_STRING_DQ_BEGIN_END
    |    MULTI_LINE_STRING_SQ_BEGIN_END
    ;

singleLineString
    :    RAW_SINGLE_LINE_STRING
    |    SINGLE_LINE_STRING_SQ_BEGIN_END
    |    SINGLE_LINE_STRING_SQ_BEGIN_MID expression
         (SINGLE_LINE_STRING_SQ_MID_MID expression)*
         SINGLE_LINE_STRING_SQ_MID_END
    |    SINGLE_LINE_STRING_DQ_BEGIN_END
    |    SINGLE_LINE_STRING_DQ_BEGIN_MID expression
         (SINGLE_LINE_STRING_DQ_MID_MID expression)*
         SINGLE_LINE_STRING_DQ_MID_END
    ;

multiLineString
    :    RAW_MULTI_LINE_STRING
    |    MULTI_LINE_STRING_SQ_BEGIN_END
    |    MULTI_LINE_STRING_SQ_BEGIN_MID expression
         (MULTI_LINE_STRING_SQ_MID_MID expression)*
         MULTI_LINE_STRING_SQ_MID_END
    |    MULTI_LINE_STRING_DQ_BEGIN_END
    |    MULTI_LINE_STRING_DQ_BEGIN_MID expression
         (MULTI_LINE_STRING_DQ_MID_MID expression)*
         MULTI_LINE_STRING_DQ_MID_END
    ;

reservedWord
    :    ASSERT
    |    BREAK
    |    CASE
    |    CATCH
    |    CLASS
    |    CONST
    |    CONTINUE
    |    DEFAULT
    |    DO
    |    ELSE
    |    ENUM
    |    EXTENDS
    |    FALSE
    |    FINAL
    |    FINALLY
    |    FOR
    |    IF
    |    IN
    |    IS
    |    NEW
    |    NULL
    |    RETHROW
    |    RETURN
    |    SUPER
    |    SWITCH
    |    THIS
    |    THROW
    |    TRUE
    |    TRY
    |    VAR
    |    VOID
    |    WHILE
    |    WITH
    ;

builtInIdentifier
    :    ABSTRACT
    |    AS
    |    COVARIANT
    |    DEFERRED
    |    DYNAMIC
    |    EXPORT
    |    EXTENSION
    |    EXTERNAL
    |    FACTORY
    |    FUNCTION
    |    GET
    |    IMPLEMENTS
    |    IMPORT
    |    INTERFACE
    |    LATE
    |    LIBRARY
    |    OPERATOR
    |    MIXIN
    |    PART
    |    REQUIRED
    |    SET
    |    STATIC
    |    TYPEDEF
    ;
