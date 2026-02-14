/*
 * SysML v2 ANTLR4 Grammar
 * Derived from the OMG SysML v2 specification (KEBNF format).
 * Source: https://github.com/Systems-Modeling/SysML-v2-Release
 * Generator: https://github.com/daltskin/sysml-v2-grammar
 * License: MIT
 */

parser grammar SysMLv2Parser;

options {
    tokenVocab = SysMLv2Lexer;
}

// $antlr-format alignTrailingComments true, columnLimit 150, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true
// $antlr-format alignSemicolons hanging, alignColons hanging
// $antlr-format minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false

// ===== Expression rules (precedence-climbing) =====

ownedExpression
    : IF ownedExpression QUESTION ownedExpression ELSE ownedExpression
    | ownedExpression QUESTION_QUESTION ownedExpression
    | ownedExpression IMPLIES ownedExpression
    | ownedExpression OR ownedExpression
    | ownedExpression AND ownedExpression
    | ownedExpression XOR ownedExpression
    | ownedExpression PIPE ownedExpression
    | ownedExpression AMP ownedExpression
    | ownedExpression ( EQ_EQ | BANG_EQ | EQ_EQ_EQ | BANG_EQ_EQ) ownedExpression
    | ownedExpression ( LT | GT | LE | GE) ownedExpression
    | ownedExpression DOT_DOT ownedExpression
    | ownedExpression ( PLUS | MINUS) ownedExpression
    | ownedExpression ( STAR | SLASH | PERCENT) ownedExpression
    | <assoc = right> ownedExpression ( STAR_STAR | CARET) ownedExpression
    | ( PLUS | MINUS | TILDE | NOT) ownedExpression
    | ( AT_SIGN | AT_AT) typeReference
    | ownedExpression ( ISTYPE | HASTYPE | AT_SIGN) typeReference
    | ownedExpression AS typeReference
    | ownedExpression AT_AT typeReference
    | ownedExpression META typeReference
    | ownedExpression LBRACK sequenceExpressionList? RBRACK
    | ownedExpression HASH LPAREN sequenceExpressionList? RPAREN
    | ownedExpression argumentList
    | ownedExpression DOT qualifiedName
    | ownedExpression DOT_QUESTION bodyExpression
    | ownedExpression ARROW qualifiedName ( bodyExpression | argumentList)
    | ALL typeReference
    | baseExpression
    ;

typeReference
    : qualifiedName
    ;

sequenceExpressionList
    : ownedExpression (COMMA ownedExpression)*
    ;

baseExpression
    : nullExpression
    | literalExpression
    | qualifiedName (argumentList | DOT METADATA)? // merged featureRef/metadataAccess/invocation
    | constructorExpression
    | bodyExpression
    | LPAREN sequenceExpressionList? RPAREN
    ;

nullExpression
    : NULL
    | LPAREN RPAREN
    ;

featureReferenceExpression
    : qualifiedName
    ;

metadataAccessExpression
    : qualifiedName DOT METADATA
    ;

invocationExpression
    : qualifiedName argumentList
    ;

constructorExpression
    : NEW qualifiedName argumentList
    ;

bodyExpression
    : LBRACE functionBodyPart RBRACE
    ;

argumentList
    : LPAREN (positionalArgumentList | namedArgumentList)? RPAREN
    ;

positionalArgumentList
    : ownedExpression (COMMA ownedExpression)*
    ;

namedArgumentList
    : namedArgument (COMMA namedArgument)*
    ;

namedArgument
    : qualifiedName EQ ownedExpression
    ;

literalExpression
    : literalBoolean
    | literalString
    | literalInteger
    | literalReal
    | literalInfinity
    ;

literalBoolean
    : TRUE
    | FALSE
    ;

literalString
    : DOUBLE_STRING
    ;

literalInteger
    : INTEGER
    ;

literalReal
    : REAL
    ;

literalInfinity
    : STAR
    ;

argumentMember
    : ownedExpression
    ;

argumentExpressionMember
    : ownedExpression
    ;

// ===== Name rule (Identifier or UnrestrictedName) =====

name
    : IDENTIFIER
    | STRING
    ;

// ===== Parser rules =====

identification
    : LT name GT name
    | LT name GT
    | name
    ;

relationshipBody
    : SEMI
    | LBRACE relationshipOwnedElement* RBRACE
    | LBRACE ( ownedAnnotation)* RBRACE
    ;

relationshipOwnedElement
    : ownedRelatedElement
    | ownedAnnotation
    ;

ownedRelatedElement
    : nonFeatureElement
    | featureElement
    ;

dependency
    : (prefixMetadataAnnotation)* DEPENDENCY (identification? FROM)? qualifiedName (
        COMMA qualifiedName
    )* TO qualifiedName (COMMA qualifiedName)* relationshipBody
    | (prefixMetadataAnnotation)* DEPENDENCY dependencyDeclaration relationshipBody
    ;

annotation
    : qualifiedName
    ;

ownedAnnotation
    : annotatingElement
    ;

annotatingElement
    : comment
    | documentation
    | textualRepresentation
    | metadataFeature
    ;

comment
    : (COMMENT identification? ( ABOUT annotation ( COMMA annotation)*)?)? (LOCALE DOUBLE_STRING)? REGULAR_COMMENT
    ;

documentation
    : DOC identification? (LOCALE DOUBLE_STRING)? REGULAR_COMMENT
    ;

textualRepresentation
    : (REP identification?)? LANGUAGE DOUBLE_STRING REGULAR_COMMENT
    ;

rootNamespace
    : packageBodyElement* EOF
    ;

namespace
    : (prefixMetadataMember)* namespaceDeclaration namespaceBody
    ;

namespaceDeclaration
    : NAMESPACE identification?
    ;

namespaceBody
    : SEMI
    | LBRACE namespaceBodyElement* RBRACE
    ;

namespaceBodyElement
    : namespaceMember
    | aliasMember
    | importRule
    ;

memberPrefix
    : (visibilityIndicator)?
    ;

visibilityIndicator
    : PUBLIC
    | PRIVATE
    | PROTECTED
    ;

namespaceMember
    : nonFeatureMember
    | namespaceFeatureMember
    ;

nonFeatureMember
    : memberPrefix memberElement
    ;

namespaceFeatureMember
    : memberPrefix featureElement
    ;

aliasMember
    : memberPrefix ALIAS (LT name GT)? (name)? FOR qualifiedName relationshipBody
    ;

qualifiedName
    : (DOLLAR COLON_COLON)? (name COLON_COLON)* name
    ;

importRule
    : (visibilityIndicator)? IMPORT (ALL)? importDeclaration relationshipBody
    ;

importDeclaration
    : membershipImport
    | namespaceImport
    ;

membershipImport
    : qualifiedName (COLON_COLON STAR_STAR)?
    ;

namespaceImport
    : qualifiedName COLON_COLON STAR (COLON_COLON STAR_STAR)?
    | filterPackage
    ;

filterPackage
    : filterPackageImportDeclaration (filterPackageMember)+
    | filterPackageImport ( filterPackageMember)+
    ;

filterPackageMember
    : LBRACK ownedExpression RBRACK
    ;

memberElement
    : annotatingElement
    | nonFeatureElement
    ;

nonFeatureElement
    : dependency
    | namespace
    | type
    | classifier
    | dataType
    | class
    | structure
    | metaclass
    | association
    | associationStructure
    | interaction
    | behavior
    | function
    | predicate
    | multiplicity
    | package
    | libraryPackage
    | specialization
    | conjugation
    | subclassification
    | disjoining
    | featureInverting
    | featureTyping
    | subsetting
    | redefinition
    | typeFeaturing
    ;

featureElement
    : feature
    | step
    | expression
    | booleanExpression
    | invariant
    | connector
    | bindingConnector
    | succession
    | flow
    | successionFlow
    ;

type
    : typePrefix TYPE typeDeclaration typeBody
    ;

typePrefix
    : (ABSTRACT)? (prefixMetadataMember)*
    ;

typeDeclaration
    : (ALL)? identification? (ownedMultiplicity)? (specializationPart | conjugationPart)+ typeRelationshipPart*
    ;

specializationPart
    : (COLON_GT | SPECIALIZES) ownedSpecialization (COMMA ownedSpecialization)*
    ;

conjugationPart
    : (TILDE | CONJUGATES) ownedConjugation
    ;

typeRelationshipPart
    : disjoiningPart
    | unioningPart
    | intersectingPart
    | differencingPart
    ;

disjoiningPart
    : DISJOINT FROM ownedDisjoining (COMMA ownedDisjoining)*
    ;

unioningPart
    : UNIONS unioning (COMMA unioning)*
    ;

intersectingPart
    : INTERSECTS intersecting (COMMA intersecting)*
    ;

differencingPart
    : DIFFERENCES differencing (COMMA differencing)*
    ;

typeBody
    : SEMI
    | LBRACE typeBodyElement* RBRACE
    ;

typeBodyElement
    : nonFeatureMember
    | featureMember
    | aliasMember
    | importRule
    ;

specialization
    : (SPECIALIZATION identification?)? SUBTYPE specificType (COLON_GT | SPECIALIZES) generalType relationshipBody
    ;

ownedSpecialization
    : generalType
    ;

specificType
    : qualifiedName (DOT qualifiedName)*
    ;

generalType
    : qualifiedName (DOT qualifiedName)*
    ;

conjugation
    : (CONJUGATION identification?)? CONJUGATE qualifiedName (DOT qualifiedName)* (
        TILDE
        | CONJUGATES
    ) qualifiedName (DOT qualifiedName)* relationshipBody
    ;

ownedConjugation
    : qualifiedName (DOT qualifiedName)*
    ;

disjoining
    : (DISJOINING identification?)? DISJOINT qualifiedName (DOT qualifiedName)* FROM qualifiedName (
        DOT qualifiedName
    )* relationshipBody
    ;

ownedDisjoining
    : qualifiedName (DOT qualifiedName)*
    ;

unioning
    : qualifiedName (DOT qualifiedName)*
    ;

intersecting
    : qualifiedName (DOT qualifiedName)*
    ;

differencing
    : qualifiedName (DOT qualifiedName)*
    ;

featureMember
    : typeFeatureMember
    | ownedFeatureMember
    ;

typeFeatureMember
    : memberPrefix MEMBER featureElement
    ;

ownedFeatureMember
    : memberPrefix featureElement
    ;

classifier
    : typePrefix CLASSIFIER classifierDeclaration typeBody
    ;

classifierDeclaration
    : (ALL)? identification? (ownedMultiplicity)? (superclassingPart | conjugationPart)? typeRelationshipPart*
    ;

superclassingPart
    : (COLON_GT | SPECIALIZES) ownedSubclassification (COMMA ownedSubclassification)*
    ;

subclassification
    : (SPECIALIZATION identification?)? SUBCLASSIFIER qualifiedName (COLON_GT | SPECIALIZES) qualifiedName relationshipBody
    ;

ownedSubclassification
    : qualifiedName
    ;

feature
    : (
        featurePrefix ( FEATURE | prefixMetadataMember) featureDeclaration?
        | ( endFeaturePrefix | basicFeaturePrefix) featureDeclaration
    ) valuePart? typeBody
    ;

endFeaturePrefix
    : (CONST)? END
    ;

basicFeaturePrefix
    : (featureDirection)? (DERIVED)? (ABSTRACT)? (COMPOSITE | PORTION)? (VAR | CONST)?
    ;

featurePrefix
    : (endFeaturePrefix ownedCrossFeatureMember | basicFeaturePrefix) (prefixMetadataMember)*
    ;

ownedCrossFeatureMember
    : ownedCrossFeature
    ;

ownedCrossFeature
    : basicFeaturePrefix featureDeclaration
    | basicUsagePrefix usageDeclaration?
    ;

featureDirection
    : IN
    | OUT
    | INOUT
    ;

featureDeclaration
    : (ALL)? (
        featureIdentification ( featureSpecializationPart | conjugationPart)?
        | featureSpecializationPart
        | conjugationPart
    ) featureRelationshipPart*
    ;

featureIdentification
    : LT name GT (name)?
    | name
    ;

featureRelationshipPart
    : typeRelationshipPart
    | chainingPart
    | invertingPart
    | typeFeaturingPart
    ;

chainingPart
    : CHAINS qualifiedName (DOT qualifiedName)*
    ;

invertingPart
    : INVERSE OF ownedFeatureInverting
    ;

typeFeaturingPart
    : FEATURED BY ownedTypeFeaturing (COMMA ownedTypeFeaturing)*
    ;

featureSpecializationPart
    : featureSpecialization+ multiplicityPart? featureSpecialization*
    | multiplicityPart featureSpecialization*
    ;

multiplicityPart
    : ownedMultiplicity (ORDERED ( NONUNIQUE)? | NONUNIQUE ( ORDERED)?)?
    | ( ORDERED ( NONUNIQUE)? | NONUNIQUE ( ORDERED)?)
    ;

featureSpecialization
    : typings
    | subsettings
    | references
    | crosses
    | redefinitions
    ;

typings
    : typedBy (COMMA featureTyping)*
    ;

typedBy
    : (COLON | TYPED BY | DEFINED BY) featureTyping
    ;

subsettings
    : subsets (COMMA ownedSubsetting)*
    ;

subsets
    : (COLON_GT | SUBSETS) ownedSubsetting
    ;

references
    : (COLON_COLON_GT | REFERENCES) ownedReferenceSubsetting
    ;

crosses
    : (FAT_ARROW | CROSSES) ownedCrossSubsetting
    ;

redefinitions
    : redefines (COMMA ownedRedefinition)*
    ;

redefines
    : (COLON_GT_GT | REDEFINES) ownedRedefinition
    ;

featureTyping
    : (SPECIALIZATION identification?)? TYPING qualifiedName (COLON | TYPED BY) generalType relationshipBody
    | ownedFeatureTyping
    | conjugatedPortTyping
    ;

ownedFeatureTyping
    : qualifiedName (DOT qualifiedName)*
    ;

subsetting
    : (SPECIALIZATION identification?)? SUBSET specificType (COLON_GT | SUBSETS) generalType relationshipBody
    ;

ownedSubsetting
    : qualifiedName (DOT qualifiedName)*
    ;

ownedReferenceSubsetting
    : qualifiedName (DOT qualifiedName)*
    ;

ownedCrossSubsetting
    : qualifiedName (DOT qualifiedName)*
    ;

redefinition
    : (SPECIALIZATION identification?)? REDEFINITION specificType (COLON_GT_GT | REDEFINES) generalType relationshipBody
    ;

ownedRedefinition
    : qualifiedName (DOT qualifiedName)*
    ;

ownedFeatureChain
    : featureChain
    | ownedFeatureChaining ( DOT ownedFeatureChaining)+
    ;

featureChain
    : ownedFeatureChaining (DOT ownedFeatureChaining)+
    ;

ownedFeatureChaining
    : qualifiedName
    ;

featureInverting
    : (INVERTING identification?)? INVERSE qualifiedName (DOT qualifiedName)* OF qualifiedName (
        DOT qualifiedName
    )* relationshipBody
    ;

ownedFeatureInverting
    : qualifiedName (DOT qualifiedName)*
    ;

typeFeaturing
    : FEATURING (identification? OF)? qualifiedName BY qualifiedName relationshipBody
    ;

ownedTypeFeaturing
    : qualifiedName
    ;

dataType
    : typePrefix DATATYPE classifierDeclaration typeBody
    ;

class
    : typePrefix CLASS classifierDeclaration typeBody
    ;

structure
    : typePrefix STRUCT classifierDeclaration typeBody
    ;

association
    : typePrefix ASSOC classifierDeclaration typeBody
    ;

associationStructure
    : typePrefix ASSOC STRUCT classifierDeclaration typeBody
    ;

connector
    : featurePrefix CONNECTOR (featureDeclaration? valuePart? | connectorDeclaration) typeBody
    ;

connectorDeclaration
    : binaryConnectorDeclaration
    | naryConnectorDeclaration
    ;

binaryConnectorDeclaration
    : (featureDeclaration? FROM | ALL FROM?)? connectorEndMember TO connectorEndMember
    ;

naryConnectorDeclaration
    : featureDeclaration? LPAREN connectorEndMember COMMA connectorEndMember (
        COMMA connectorEndMember
    )* RPAREN
    ;

connectorEndMember
    : connectorEnd
    ;

connectorEnd
    : (ownedCrossMultiplicityMember)? (name ( COLON_COLON_GT | REFERENCES))? ownedReferenceSubsetting
    ;

ownedCrossMultiplicityMember
    : ownedCrossMultiplicity
    ;

ownedCrossMultiplicity
    : ownedMultiplicity
    ;

bindingConnector
    : featurePrefix BINDING bindingConnectorDeclaration typeBody
    ;

bindingConnectorDeclaration
    : featureDeclaration (OF connectorEndMember EQ connectorEndMember)?
    | ( ALL)? ( OF? connectorEndMember EQ connectorEndMember)?
    ;

succession
    : featurePrefix SUCCESSION successionDeclaration typeBody
    ;

successionDeclaration
    : featureDeclaration (FIRST connectorEndMember THEN connectorEndMember)?
    | ( ALL)? ( FIRST? connectorEndMember THEN connectorEndMember)?
    ;

behavior
    : typePrefix BEHAVIOR classifierDeclaration typeBody
    ;

step
    : featurePrefix STEP featureDeclaration valuePart? typeBody
    ;

function
    : typePrefix FUNCTION classifierDeclaration functionBody
    ;

functionBody
    : SEMI
    | LBRACE functionBodyPart RBRACE
    ;

functionBodyPart
    : (typeBodyElement | returnFeatureMember)* (resultExpressionMember)?
    ;

returnFeatureMember
    : memberPrefix RETURN featureElement
    ;

resultExpressionMember
    : memberPrefix ownedExpression
    ;

expression
    : featurePrefix EXPR featureDeclaration valuePart? functionBody
    ;

predicate
    : typePrefix PREDICATE classifierDeclaration functionBody
    ;

booleanExpression
    : featurePrefix BOOL featureDeclaration valuePart? functionBody
    ;

invariant
    : featurePrefix INV (TRUE | FALSE)? featureDeclaration valuePart? functionBody
    ;

ownedExpressionMember
    : ownedExpression
    ;

metadataReference
    : elementReferenceMember
    ;

typeReferenceMember
    : typeReference
    ;

typeResultMember
    : typeReference
    ;

referenceTyping
    : qualifiedName
    ;

emptyResultMember
    : emptyFeature_
    ;

sequenceOperatorExpression
    : ownedExpressionMember COMMA sequenceExpressionListMember
    ;

sequenceExpressionListMember
    : sequenceExpressionList
    ;

bodyArgumentMember
    : bodyArgument
    ;

bodyArgument
    : bodyArgumentValue
    ;

bodyArgumentValue
    : bodyExpression
    ;

functionReferenceArgumentMember
    : functionReferenceArgument
    ;

functionReferenceArgument
    : functionReferenceArgumentValue
    ;

functionReferenceArgumentValue
    : functionReferenceExpression
    ;

functionReferenceExpression
    : functionReferenceMember
    ;

functionReferenceMember
    : functionReference
    ;

functionReference
    : referenceTyping
    ;

featureChainMember
    : qualifiedName (DOT qualifiedName)*
    ;

ownedFeatureChainMember
    : featureChain
    | ownedFeatureChain
    ;

featureReferenceMember
    : featureReference
    ;

featureReference
    : qualifiedName
    ;

elementReferenceMember
    : qualifiedName
    ;

constructorResultMember
    : constructorResult
    ;

constructorResult
    : argumentList
    ;

instantiatedTypeMember
    : qualifiedName (DOT qualifiedName)*
    ;

instantiatedTypeReference
    : qualifiedName
    ;

namedArgumentMember
    : namedArgument
    ;

parameterRedefinition
    : qualifiedName
    ;

expressionBodyMember
    : expressionBody
    ;

expressionBody
    : LBRACE functionBodyPart RBRACE
    ;

booleanValue
    : TRUE
    | FALSE
    ;

realValue
    : INTEGER? DOT (INTEGER | REAL)
    | REAL
    ;

interaction
    : typePrefix INTERACTION classifierDeclaration typeBody
    ;

flow
    : featurePrefix FLOW flowDeclaration typeBody
    ;

successionFlow
    : featurePrefix SUCCESSION FLOW flowDeclaration typeBody
    ;

flowDeclaration
    : featureDeclaration valuePart? (OF payloadFeatureMember)? (
        FROM flowEndMember TO flowEndMember
    )?
    | ( ALL)? flowEndMember TO flowEndMember
    | usageDeclaration? valuePart? (OF flowPayloadFeatureMember)? (
        FROM flowEndMember TO flowEndMember
    )?
    ;

payloadFeatureMember
    : payloadFeature
    ;

payloadFeature
    : identification? valuePart
    | identification? payloadFeatureSpecializationPart valuePart?
    | ownedFeatureTyping ( ownedMultiplicity)?
    | ownedMultiplicity ( ownedFeatureTyping)?
    ;

payloadFeatureSpecializationPart
    : featureSpecialization+ multiplicityPart? featureSpecialization*
    | multiplicityPart featureSpecialization+
    ;

flowEndMember
    : flowEnd
    ;

flowEnd
    : qualifiedName (DOT qualifiedName)*
    ;

flowFeatureMember
    : flowFeature
    ;

flowFeature
    : flowFeatureRedefinition
    ;

flowFeatureRedefinition
    : qualifiedName
    ;

valuePart
    : featureValue
    ;

featureValue
    : (EQ | COLON_EQ | DEFAULT ( EQ | COLON_EQ)?) ownedExpression
    ;

multiplicity
    : multiplicitySubset
    | multiplicityRange
    ;

multiplicitySubset
    : MULTIPLICITY identification? subsets typeBody
    ;

multiplicityRange
    : MULTIPLICITY identification? multiplicityBounds typeBody
    ;

ownedMultiplicity
    : ownedMultiplicityRange
    ;

ownedMultiplicityRange
    : multiplicityBounds
    ;

multiplicityBounds
    : LBRACK (multiplicityExpressionMember DOT_DOT)? multiplicityExpressionMember RBRACK
    ;

multiplicityExpressionMember
    : (literalExpression | featureReferenceExpression)
    ;

metaclass
    : typePrefix METACLASS classifierDeclaration typeBody
    ;

prefixMetadataAnnotation
    : HASH prefixMetadataFeature
    | HASH prefixMetadataUsage
    ;

prefixMetadataMember
    : HASH prefixMetadataFeature
    | HASH prefixMetadataUsage
    ;

prefixMetadataFeature
    : ownedFeatureTyping
    ;

metadataFeature
    : (prefixMetadataMember)* (AT_SIGN | METADATA) metadataFeatureDeclaration (
        ABOUT annotation ( COMMA annotation)*
    )? metadataBody
    ;

metadataFeatureDeclaration
    : (identification? ( COLON | TYPED BY))? ownedFeatureTyping
    ;

metadataBody
    : SEMI
    | LBRACE ( metadataBodyElement)* RBRACE
    | LBRACE (definitionMember | metadataBodyUsageMember | aliasMember | importRule)* RBRACE
    ;

metadataBodyElement
    : nonFeatureMember
    | metadataBodyFeatureMember
    | aliasMember
    | importRule
    ;

metadataBodyFeatureMember
    : metadataBodyFeature
    ;

metadataBodyFeature
    : FEATURE? (COLON_GT_GT | REDEFINES)? ownedRedefinition featureSpecializationPart? valuePart? metadataBody
    ;

package
    : (prefixMetadataMember)* packageDeclaration packageBody
    ;

libraryPackage
    : (STANDARD)? LIBRARY (prefixMetadataMember)* packageDeclaration packageBody
    ;

packageDeclaration
    : PACKAGE identification?
    ;

packageBody
    : SEMI
    | LBRACE packageBodyElement* RBRACE
    ;

elementFilterMember
    : memberPrefix FILTER ownedExpression SEMI
    ;

dependencyDeclaration
    : (identification? FROM)? qualifiedName (COMMA qualifiedName)* TO qualifiedName (
        COMMA qualifiedName
    )*
    ;

annotatingMember
    : annotatingElement
    ;

packageBodyElement
    : packageMember
    | elementFilterMember
    | aliasMember
    | importRule
    ;

packageMember
    : memberPrefix (definitionElement | usageElement)
    ;

definitionElement
    : package
    | libraryPackage
    | annotatingElement
    | dependency
    | attributeDefinition
    | enumerationDefinition
    | occurrenceDefinition
    | individualDefinition
    | itemDefinition
    | partDefinition
    | connectionDefinition
    | flowDefinition
    | interfaceDefinition
    | portDefinition
    | actionDefinition
    | calculationDefinition
    | stateDefinition
    | constraintDefinition
    | requirementDefinition
    | concernDefinition
    | caseDefinition
    | analysisCaseDefinition
    | verificationCaseDefinition
    | useCaseDefinition
    | viewDefinition
    | viewpointDefinition
    | renderingDefinition
    | metadataDefinition
    | allocationDefinition
    | extendedDefinition
    ;

usageElement
    : nonOccurrenceUsageElement
    | occurrenceUsageElement
    ;

basicDefinitionPrefix
    : ABSTRACT
    | VARIATION
    ;

definitionExtensionKeyword
    : prefixMetadataMember
    ;

definitionPrefix
    : basicDefinitionPrefix? definitionExtensionKeyword*
    ;

definition
    : definitionDeclaration definitionBody
    ;

definitionDeclaration
    : identification? subclassificationPart?
    ;

definitionBody
    : SEMI
    | LBRACE definitionBodyItem* RBRACE
    ;

definitionBodyItem
    : importRule
    | memberPrefix definitionBodyItemContent
    | ( sourceSuccessionMember)? memberPrefix occurrenceUsageElement
    ;

// Factored dispatch: after memberPrefix is consumed, the next token
// (ALIAS, VARIANT, keyword, or identifier) unambiguously selects the branch.
// This reduces the SLL prediction DFA from 6 nullable-prefix alternatives to 3+4.
definitionBodyItemContent
    : ALIAS (LT name GT)? (name)? FOR qualifiedName relationshipBody
    | VARIANT variantUsageElement
    | definitionElement
    | nonOccurrenceUsageElement
    ;

definitionMember
    : memberPrefix definitionElement
    ;

variantUsageMember
    : memberPrefix VARIANT variantUsageElement
    ;

nonOccurrenceUsageMember
    : memberPrefix nonOccurrenceUsageElement
    ;

occurrenceUsageMember
    : memberPrefix occurrenceUsageElement
    ;

structureUsageMember
    : memberPrefix structureUsageElement
    ;

behaviorUsageMember
    : memberPrefix behaviorUsageElement
    ;

refPrefix
    : (featureDirection)? (DERIVED)? (ABSTRACT | VARIATION)? (CONSTANT)?
    ;

basicUsagePrefix
    : refPrefix (REF)?
    ;

endUsagePrefix
    : END ownedCrossFeatureMember
    ;

usageExtensionKeyword
    : prefixMetadataMember
    ;

unextendedUsagePrefix
    : endUsagePrefix
    | basicUsagePrefix
    ;

usagePrefix
    : unextendedUsagePrefix usageExtensionKeyword*
    ;

usage
    : usageDeclaration? usageCompletion
    ;

usageDeclaration
    : identification featureSpecializationPart?
    | featureSpecializationPart
    ;

usageCompletion
    : valuePart? usageBody
    ;

usageBody
    : definitionBody
    ;

defaultReferenceUsage
    : refPrefix usage
    ;

referenceUsage
    : (endUsagePrefix | refPrefix) REF usage
    ;

// Unnamed end feature with specialization (e.g., end :>> QualifiedName;)
// Handles end features in connection/flow/interface definition bodies
// where no name is given, only a redefines/subsets/typing.
endFeatureUsage
    : endUsagePrefix featureDeclaration usageCompletion
    ;

variantReference
    : ownedReferenceSubsetting featureSpecialization* usageBody
    ;

nonOccurrenceUsageElement
    : referenceUsage
    | endFeatureUsage
    | attributeUsage
    | enumerationUsage
    | bindingConnectorAsUsage
    | successionAsUsage
    | extendedUsage
    | defaultReferenceUsage
    ;

occurrenceUsageElement
    : structureUsageElement
    | behaviorUsageElement
    ;

structureUsageElement
    : occurrenceUsage
    | individualUsage
    | portionUsage
    | eventOccurrenceUsage
    | itemUsage
    | partUsage
    | viewUsage
    | renderingUsage
    | portUsage
    | connectionUsage
    | interfaceUsage
    | allocationUsage
    | message
    | flowUsage
    | successionFlowUsage
    ;

behaviorUsageElement
    : actionUsage
    | calculationUsage
    | stateUsage
    | constraintUsage
    | requirementUsage
    | concernUsage
    | caseUsage
    | analysisCaseUsage
    | verificationCaseUsage
    | useCaseUsage
    | viewpointUsage
    | performActionUsage
    | exhibitStateUsage
    | includeUseCaseUsage
    | assertConstraintUsage
    | satisfyRequirementUsage
    ;

variantUsageElement
    : variantReference
    | referenceUsage
    | attributeUsage
    | bindingConnectorAsUsage
    | successionAsUsage
    | occurrenceUsage
    | individualUsage
    | portionUsage
    | eventOccurrenceUsage
    | itemUsage
    | partUsage
    | viewUsage
    | renderingUsage
    | portUsage
    | connectionUsage
    | interfaceUsage
    | allocationUsage
    | message
    | flowUsage
    | successionFlowUsage
    | behaviorUsageElement
    ;

subclassificationPart
    : (COLON_GT | SPECIALIZES) ownedSubclassification (COMMA ownedSubclassification)*
    ;

attributeDefinition
    : definitionPrefix ATTRIBUTE DEF definition
    ;

attributeUsage
    : usagePrefix ATTRIBUTE usage
    ;

enumerationDefinition
    : definitionExtensionKeyword* ENUM DEF definitionDeclaration enumerationBody
    ;

enumerationBody
    : SEMI
    | LBRACE ( annotatingMember | enumerationUsageMember)* RBRACE
    ;

enumerationUsageMember
    : memberPrefix enumeratedValue
    ;

enumeratedValue
    : ENUM? usage
    ;

enumerationUsage
    : usagePrefix ENUM usage
    ;

occurrenceDefinitionPrefix
    : basicDefinitionPrefix? (INDIVIDUAL emptyMultiplicityMember)? definitionExtensionKeyword*
    ;

occurrenceDefinition
    : occurrenceDefinitionPrefix OCCURRENCE DEF definition
    ;

individualDefinition
    : basicDefinitionPrefix? INDIVIDUAL definitionExtensionKeyword* DEF definition emptyMultiplicityMember
    ;

emptyMultiplicityMember
    : emptyMultiplicity_
    ;

occurrenceUsagePrefix
    : basicUsagePrefix (INDIVIDUAL)? (portionKind)? usageExtensionKeyword*
    ;

occurrenceUsage
    : occurrenceUsagePrefix OCCURRENCE usage
    ;

individualUsage
    : basicUsagePrefix INDIVIDUAL usageExtensionKeyword* usage
    ;

portionUsage
    : basicUsagePrefix (INDIVIDUAL)? portionKind usageExtensionKeyword* usage
    ;

portionKind
    : SNAPSHOT
    | TIMESLICE
    ;

eventOccurrenceUsage
    : occurrenceUsagePrefix EVENT (
        ownedReferenceSubsetting featureSpecializationPart?
        | OCCURRENCE usageDeclaration?
    ) usageCompletion
    ;

sourceSuccessionMember
    : THEN sourceSuccession
    ;

sourceSuccession
    : sourceEndMember
    ;

sourceEndMember
    : sourceEnd
    ;

sourceEnd
    : (ownedMultiplicity)?
    ;

itemDefinition
    : occurrenceDefinitionPrefix ITEM DEF definition
    ;

itemUsage
    : occurrenceUsagePrefix ITEM usage
    ;

partDefinition
    : occurrenceDefinitionPrefix PART DEF definition
    ;

partUsage
    : occurrenceUsagePrefix PART usage
    ;

portDefinition
    : definitionPrefix PORT DEF definition conjugatedPortDefinitionMember
    ;

conjugatedPortDefinitionMember
    : conjugatedPortDefinition
    ;

conjugatedPortDefinition
    : portConjugation
    ;

portUsage
    : occurrenceUsagePrefix PORT usage
    ;

conjugatedPortTyping
    : TILDE qualifiedName
    ;

connectionDefinition
    : occurrenceDefinitionPrefix CONNECTION DEF definition
    ;

connectionUsage
    : occurrenceUsagePrefix (
        CONNECTION usageDeclaration? valuePart? ( CONNECT connectorPart)?
        | CONNECT connectorPart
    ) usageBody
    ;

connectorPart
    : binaryConnectorPart
    | naryConnectorPart
    ;

binaryConnectorPart
    : connectorEndMember TO connectorEndMember
    ;

naryConnectorPart
    : LPAREN connectorEndMember COMMA connectorEndMember (COMMA connectorEndMember)* RPAREN
    ;

bindingConnectorAsUsage
    : usagePrefix (BINDING usageDeclaration?)? BIND connectorEndMember EQ connectorEndMember usageBody
    ;

successionAsUsage
    : usagePrefix (SUCCESSION usageDeclaration?)? FIRST connectorEndMember THEN connectorEndMember usageBody
    ;

interfaceDefinition
    : occurrenceDefinitionPrefix INTERFACE DEF definitionDeclaration interfaceBody
    ;

interfaceBody
    : SEMI
    | LBRACE interfaceBodyItem* RBRACE
    ;

interfaceBodyItem
    : definitionMember
    | variantUsageMember
    | interfaceNonOccurrenceUsageMember
    | ( sourceSuccessionMember)? interfaceOccurrenceUsageMember
    | aliasMember
    | importRule
    ;

interfaceNonOccurrenceUsageMember
    : memberPrefix interfaceNonOccurrenceUsageElement
    ;

interfaceNonOccurrenceUsageElement
    : referenceUsage
    | attributeUsage
    | enumerationUsage
    | bindingConnectorAsUsage
    | successionAsUsage
    ;

interfaceOccurrenceUsageMember
    : memberPrefix interfaceOccurrenceUsageElement
    ;

interfaceOccurrenceUsageElement
    : defaultInterfaceEnd
    | structureUsageElement
    | behaviorUsageElement
    ;

defaultInterfaceEnd
    : END usage
    ;

interfaceUsage
    : occurrenceUsagePrefix INTERFACE interfaceUsageDeclaration interfaceBody
    ;

interfaceUsageDeclaration
    : usageDeclaration? valuePart? (CONNECT interfacePart)?
    | interfacePart
    ;

interfacePart
    : binaryInterfacePart
    | naryInterfacePart
    ;

binaryInterfacePart
    : interfaceEndMember TO interfaceEndMember
    ;

naryInterfacePart
    : LPAREN interfaceEndMember COMMA interfaceEndMember (COMMA interfaceEndMember)* RPAREN
    ;

interfaceEndMember
    : interfaceEnd
    ;

interfaceEnd
    : (ownedCrossMultiplicityMember)? (name ( COLON_COLON_GT | REFERENCES))? ownedReferenceSubsetting
    ;

allocationDefinition
    : occurrenceDefinitionPrefix ALLOCATION DEF definition
    ;

allocationUsage
    : occurrenceUsagePrefix allocationUsageDeclaration usageBody
    ;

allocationUsageDeclaration
    : ALLOCATION usageDeclaration? (ALLOCATE connectorPart)?
    | ALLOCATE connectorPart
    ;

flowDefinition
    : occurrenceDefinitionPrefix FLOW DEF definition
    ;

message
    : occurrenceUsagePrefix MESSAGE messageDeclaration definitionBody
    ;

messageDeclaration
    : usageDeclaration? valuePart? (OF flowPayloadFeatureMember)? (
        FROM messageEventMember TO messageEventMember
    )?
    | messageEventMember TO messageEventMember
    ;

messageEventMember
    : messageEvent
    ;

messageEvent
    : ownedReferenceSubsetting
    ;

flowUsage
    : occurrenceUsagePrefix FLOW flowDeclaration definitionBody
    ;

successionFlowUsage
    : occurrenceUsagePrefix SUCCESSION FLOW flowDeclaration definitionBody
    ;

flowPayloadFeatureMember
    : flowPayloadFeature
    ;

flowPayloadFeature
    : payloadFeature
    ;

flowEndSubsetting
    : qualifiedName
    | featureChainPrefix
    ;

featureChainPrefix
    : (ownedFeatureChaining DOT)+ ownedFeatureChaining DOT
    ;

actionDefinition
    : occurrenceDefinitionPrefix ACTION DEF definitionDeclaration actionBody
    ;

actionBody
    : SEMI
    | LBRACE actionBodyItem* RBRACE
    ;

actionBodyItem
    : nonBehaviorBodyItem
    | initialNodeMember ( actionTargetSuccessionMember)*
    | (sourceSuccessionMember)? actionBehaviorMember (actionTargetSuccessionMember)*
    | guardedSuccessionMember
    ;

nonBehaviorBodyItem
    : importRule
    | aliasMember
    | definitionMember
    | variantUsageMember
    | nonOccurrenceUsageMember
    | ( sourceSuccessionMember)? structureUsageMember
    ;

actionBehaviorMember
    : behaviorUsageMember
    | actionNodeMember
    ;

initialNodeMember
    : memberPrefix FIRST qualifiedName relationshipBody
    ;

actionNodeMember
    : memberPrefix actionNode
    ;

actionTargetSuccessionMember
    : memberPrefix actionTargetSuccession
    ;

guardedSuccessionMember
    : memberPrefix guardedSuccession
    ;

actionUsage
    : occurrenceUsagePrefix ACTION actionUsageDeclaration actionBody
    ;

actionUsageDeclaration
    : usageDeclaration? valuePart?
    ;

performActionUsage
    : occurrenceUsagePrefix PERFORM performActionUsageDeclaration actionBody
    ;

performActionUsageDeclaration
    : (ownedReferenceSubsetting featureSpecializationPart? | ACTION usageDeclaration?) valuePart?
    ;

actionNode
    : controlNode
    | sendNode
    | acceptNode
    | assignmentNode
    | terminateNode
    | ifNode
    | whileLoopNode
    | forLoopNode
    ;

actionNodeUsageDeclaration
    : ACTION usageDeclaration?
    ;

actionNodePrefix
    : occurrenceUsagePrefix actionNodeUsageDeclaration?
    ;

controlNode
    : mergeNode
    | decisionNode
    | joinNode
    | forkNode
    ;

controlNodePrefix
    : refPrefix (INDIVIDUAL)? (portionKind)? usageExtensionKeyword*
    ;

mergeNode
    : controlNodePrefix MERGE usageDeclaration? actionBody
    ;

decisionNode
    : controlNodePrefix DECIDE usageDeclaration? actionBody
    ;

joinNode
    : controlNodePrefix JOIN usageDeclaration? actionBody
    ;

forkNode
    : controlNodePrefix FORK usageDeclaration? actionBody
    ;

acceptNode
    : occurrenceUsagePrefix acceptNodeDeclaration actionBody
    ;

acceptNodeDeclaration
    : actionNodeUsageDeclaration? ACCEPT acceptParameterPart
    ;

acceptParameterPart
    : payloadParameterMember (VIA nodeParameterMember)?
    ;

payloadParameterMember
    : payloadParameter
    ;

payloadParameter
    : payloadFeature
    | identification? payloadFeatureSpecializationPart? triggerValuePart
    ;

triggerValuePart
    : triggerFeatureValue
    ;

triggerFeatureValue
    : triggerExpression
    ;

triggerExpression
    : (AT | AFTER) argumentMember
    | WHEN argumentExpressionMember
    ;

sendNode
    : occurrenceUsagePrefix (actionNodeUsageDeclaration | actionUsageDeclaration) SEND (
        nodeParameterMember senderReceiverPart?
        | emptyParameterMember senderReceiverPart
    ) actionBody
    ;

sendNodeDeclaration
    : actionNodeUsageDeclaration? SEND nodeParameterMember senderReceiverPart?
    ;

senderReceiverPart
    : VIA nodeParameterMember (TO nodeParameterMember)?
    | emptyParameterMember TO nodeParameterMember
    ;

nodeParameterMember
    : nodeParameter
    ;

nodeParameter
    : featureBinding
    ;

featureBinding
    : ownedExpression
    ;

emptyParameterMember
    : emptyUsage_
    ;

assignmentNode
    : occurrenceUsagePrefix assignmentNodeDeclaration actionBody
    ;

assignmentNodeDeclaration
    : (actionNodeUsageDeclaration)? ASSIGN assignmentTargetMember featureChainMember COLON_EQ nodeParameterMember
    ;

assignmentTargetMember
    : assignmentTargetParameter
    ;

assignmentTargetParameter
    : (assignmentTargetBinding DOT)?
    ;

assignmentTargetBinding
    : nonFeatureChainPrimaryExpression
    ;

terminateNode
    : occurrenceUsagePrefix actionNodeUsageDeclaration? TERMINATE (nodeParameterMember)? actionBody
    ;

ifNode
    : actionNodePrefix IF expressionParameterMember actionBodyParameterMember (
        ELSE ( actionBodyParameterMember | ifNodeParameterMember)
    )?
    ;

expressionParameterMember
    : ownedExpression
    ;

actionBodyParameterMember
    : actionBodyParameter
    ;

actionBodyParameter
    : (ACTION usageDeclaration?)? LBRACE actionBodyItem* RBRACE
    ;

ifNodeParameterMember
    : ifNode
    ;

whileLoopNode
    : actionNodePrefix (WHILE expressionParameterMember | LOOP emptyParameterMember) actionBodyParameterMember (
        UNTIL expressionParameterMember SEMI
    )?
    ;

forLoopNode
    : actionNodePrefix FOR forVariableDeclarationMember IN nodeParameterMember actionBodyParameterMember
    ;

forVariableDeclarationMember
    : usageDeclaration?
    ;

forVariableDeclaration
    : usageDeclaration?
    ;

actionTargetSuccession
    : (targetSuccession | guardedTargetSuccession | defaultTargetSuccession) usageBody
    ;

targetSuccession
    : sourceEndMember THEN connectorEndMember
    ;

guardedTargetSuccession
    : guardExpressionMember THEN transitionSuccessionMember
    ;

defaultTargetSuccession
    : ELSE transitionSuccessionMember
    ;

guardedSuccession
    : (SUCCESSION usageDeclaration?)? FIRST featureChainMember guardExpressionMember THEN transitionSuccessionMember usageBody
    ;

stateDefinition
    : occurrenceDefinitionPrefix STATE DEF definitionDeclaration stateDefBody
    ;

stateDefBody
    : SEMI
    | ( PARALLEL)? LBRACE stateBodyItem* RBRACE
    ;

stateBodyItem
    : nonBehaviorBodyItem
    | (sourceSuccessionMember)? behaviorUsageMember (targetTransitionUsageMember)*
    | transitionUsageMember
    | entryActionMember ( entryTransitionMember)*
    | doActionMember
    | exitActionMember
    ;

entryActionMember
    : memberPrefix ENTRY stateActionUsage
    ;

doActionMember
    : memberPrefix DO stateActionUsage
    ;

exitActionMember
    : memberPrefix EXIT stateActionUsage
    ;

entryTransitionMember
    : memberPrefix (guardedTargetSuccession | THEN transitionSuccessionMember) SEMI
    ;

stateActionUsage
    : emptyActionUsage_ SEMI
    | statePerformActionUsage
    | stateAcceptActionUsage
    | stateSendActionUsage
    | stateAssignmentActionUsage
    ;

statePerformActionUsage
    : performActionUsageDeclaration actionBody
    ;

stateAcceptActionUsage
    : acceptNodeDeclaration actionBody
    ;

stateSendActionUsage
    : sendNodeDeclaration actionBody
    ;

stateAssignmentActionUsage
    : assignmentNodeDeclaration actionBody
    ;

transitionUsageMember
    : memberPrefix transitionUsage
    ;

targetTransitionUsageMember
    : memberPrefix targetTransitionUsage
    ;

stateUsage
    : occurrenceUsagePrefix STATE actionUsageDeclaration stateUsageBody
    ;

stateUsageBody
    : SEMI
    | ( PARALLEL)? LBRACE stateBodyItem* RBRACE
    ;

exhibitStateUsage
    : occurrenceUsagePrefix EXHIBIT (
        ownedReferenceSubsetting featureSpecializationPart?
        | STATE usageDeclaration?
    ) valuePart? stateUsageBody
    ;

transitionUsage
    : TRANSITION (usageDeclaration? FIRST)? featureChainMember emptyParameterMember (
        emptyParameterMember triggerActionMember
    )? (guardExpressionMember)? (effectBehaviorMember)? THEN transitionSuccessionMember actionBody
    ;

targetTransitionUsage
    : emptyParameterMember (
        TRANSITION (emptyParameterMember triggerActionMember)? (guardExpressionMember)? (
            effectBehaviorMember
        )?
        | emptyParameterMember triggerActionMember (guardExpressionMember)? (effectBehaviorMember)?
        | guardExpressionMember ( effectBehaviorMember)?
    )? THEN transitionSuccessionMember actionBody
    ;

triggerActionMember
    : ACCEPT triggerAction
    ;

triggerAction
    : acceptParameterPart
    ;

guardExpressionMember
    : IF ownedExpression
    ;

effectBehaviorMember
    : DO effectBehaviorUsage
    ;

effectBehaviorUsage
    : emptyActionUsage_
    | transitionPerformActionUsage
    | transitionAcceptActionUsage
    | transitionSendActionUsage
    | transitionAssignmentActionUsage
    ;

transitionPerformActionUsage
    : performActionUsageDeclaration (LBRACE actionBodyItem* RBRACE)?
    ;

transitionAcceptActionUsage
    : acceptNodeDeclaration (LBRACE actionBodyItem* RBRACE)?
    ;

transitionSendActionUsage
    : sendNodeDeclaration (LBRACE actionBodyItem* RBRACE)?
    ;

transitionAssignmentActionUsage
    : assignmentNodeDeclaration (LBRACE actionBodyItem* RBRACE)?
    ;

transitionSuccessionMember
    : transitionSuccession
    ;

transitionSuccession
    : emptyEndMember connectorEndMember
    ;

emptyEndMember
    : emptyFeature_
    ;

calculationDefinition
    : occurrenceDefinitionPrefix CALC DEF definitionDeclaration calculationBody
    ;

calculationUsage
    : occurrenceUsagePrefix CALC actionUsageDeclaration calculationBody
    ;

calculationBody
    : SEMI
    | LBRACE calculationBodyPart RBRACE
    ;

calculationBodyPart
    : calculationBodyItem* (resultExpressionMember)?
    ;

calculationBodyItem
    : actionBodyItem
    | returnParameterMember
    ;

returnParameterMember
    : memberPrefix RETURN usageElement
    ;

constraintDefinition
    : occurrenceDefinitionPrefix CONSTRAINT DEF definitionDeclaration calculationBody
    ;

constraintUsage
    : occurrenceUsagePrefix CONSTRAINT constraintUsageDeclaration calculationBody
    ;

assertConstraintUsage
    : occurrenceUsagePrefix ASSERT (NOT)? (
        ownedReferenceSubsetting featureSpecializationPart?
        | CONSTRAINT constraintUsageDeclaration
    ) calculationBody
    ;

constraintUsageDeclaration
    : usageDeclaration? valuePart?
    ;

requirementDefinition
    : occurrenceDefinitionPrefix REQUIREMENT DEF definitionDeclaration requirementBody
    ;

requirementBody
    : SEMI
    | LBRACE requirementBodyItem* RBRACE
    ;

requirementBodyItem
    : definitionBodyItem
    | subjectMember
    | requirementConstraintMember
    | framedConcernMember
    | requirementVerificationMember
    | actorMember
    | stakeholderMember
    ;

subjectMember
    : memberPrefix subjectUsage
    ;

subjectUsage
    : SUBJECT usageExtensionKeyword* usage
    ;

requirementConstraintMember
    : memberPrefix requirementKind requirementConstraintUsage
    ;

requirementKind
    : ASSUME
    | REQUIRE
    ;

requirementConstraintUsage
    : ownedReferenceSubsetting featureSpecializationPart? requirementBody
    | (usageExtensionKeyword* CONSTRAINT | usageExtensionKeyword+) constraintUsageDeclaration calculationBody
    ;

framedConcernMember
    : memberPrefix FRAME framedConcernUsage
    ;

framedConcernUsage
    : ownedReferenceSubsetting featureSpecializationPart? calculationBody
    | (usageExtensionKeyword* CONCERN | usageExtensionKeyword+) calculationUsageDeclaration calculationBody
    ;

actorMember
    : memberPrefix actorUsage
    ;

actorUsage
    : ACTOR usageExtensionKeyword* usage
    ;

stakeholderMember
    : memberPrefix stakeholderUsage
    ;

stakeholderUsage
    : STAKEHOLDER usageExtensionKeyword* usage
    ;

requirementUsage
    : occurrenceUsagePrefix REQUIREMENT constraintUsageDeclaration requirementBody
    ;

satisfyRequirementUsage
    : occurrenceUsagePrefix (ASSERT ( NOT)?)? SATISFY (
        ownedReferenceSubsetting featureSpecializationPart?
        | REQUIREMENT usageDeclaration?
    ) valuePart? (BY satisfactionSubjectMember)? requirementBody
    ;

satisfactionSubjectMember
    : satisfactionParameter
    ;

satisfactionParameter
    : satisfactionFeatureValue
    ;

satisfactionFeatureValue
    : satisfactionReferenceExpression
    ;

satisfactionReferenceExpression
    : featureChainMember
    ;

concernDefinition
    : occurrenceDefinitionPrefix CONCERN DEF definitionDeclaration requirementBody
    ;

concernUsage
    : occurrenceUsagePrefix CONCERN constraintUsageDeclaration requirementBody
    ;

caseDefinition
    : occurrenceDefinitionPrefix CASE DEF definitionDeclaration caseBody
    ;

caseUsage
    : occurrenceUsagePrefix CASE constraintUsageDeclaration caseBody
    ;

caseBody
    : SEMI
    | LBRACE caseBodyItem* ( resultExpressionMember)? RBRACE
    ;

caseBodyItem
    : actionBodyItem
    | returnParameterMember
    | subjectMember
    | actorMember
    | objectiveMember
    ;

objectiveMember
    : memberPrefix OBJECTIVE objectiveRequirementUsage
    ;

objectiveRequirementUsage
    : usageExtensionKeyword* constraintUsageDeclaration requirementBody
    ;

analysisCaseDefinition
    : occurrenceDefinitionPrefix ANALYSIS DEF definitionDeclaration caseBody
    ;

analysisCaseUsage
    : occurrenceUsagePrefix ANALYSIS constraintUsageDeclaration caseBody
    ;

verificationCaseDefinition
    : occurrenceDefinitionPrefix VERIFICATION DEF definitionDeclaration caseBody
    ;

verificationCaseUsage
    : occurrenceUsagePrefix VERIFICATION constraintUsageDeclaration caseBody
    ;

requirementVerificationMember
    : memberPrefix VERIFY requirementVerificationUsage
    ;

requirementVerificationUsage
    : ownedReferenceSubsetting featureSpecialization* requirementBody
    | (usageExtensionKeyword* REQUIREMENT | usageExtensionKeyword+) constraintUsageDeclaration requirementBody
    ;

useCaseDefinition
    : occurrenceDefinitionPrefix USE CASE DEF definitionDeclaration caseBody
    ;

useCaseUsage
    : occurrenceUsagePrefix USE CASE constraintUsageDeclaration caseBody
    ;

includeUseCaseUsage
    : occurrenceUsagePrefix INCLUDE (
        ownedReferenceSubsetting featureSpecializationPart?
        | USE CASE usageDeclaration?
    ) valuePart? caseBody
    ;

viewDefinition
    : occurrenceDefinitionPrefix VIEW DEF definitionDeclaration viewDefinitionBody
    ;

viewDefinitionBody
    : SEMI
    | LBRACE viewDefinitionBodyItem* RBRACE
    ;

viewDefinitionBodyItem
    : definitionBodyItem
    | elementFilterMember
    | viewRenderingMember
    ;

viewRenderingMember
    : memberPrefix RENDER viewRenderingUsage
    ;

viewRenderingUsage
    : ownedReferenceSubsetting featureSpecializationPart? usageBody
    | ( usageExtensionKeyword* RENDERING | usageExtensionKeyword+) usage
    ;

viewUsage
    : occurrenceUsagePrefix VIEW usageDeclaration? valuePart? viewBody
    ;

viewBody
    : SEMI
    | LBRACE viewBodyItem* RBRACE
    ;

viewBodyItem
    : definitionBodyItem
    | elementFilterMember
    | viewRenderingMember
    | expose
    ;

expose
    : EXPOSE (membershipExpose | namespaceExpose) relationshipBody
    ;

membershipExpose
    : membershipImport
    ;

namespaceExpose
    : namespaceImport
    ;

viewpointDefinition
    : occurrenceDefinitionPrefix VIEWPOINT DEF definitionDeclaration requirementBody
    ;

viewpointUsage
    : occurrenceUsagePrefix VIEWPOINT constraintUsageDeclaration requirementBody
    ;

renderingDefinition
    : occurrenceDefinitionPrefix RENDERING DEF definition
    ;

renderingUsage
    : occurrenceUsagePrefix RENDERING usage
    ;

metadataDefinition
    : (ABSTRACT)? definitionExtensionKeyword* METADATA DEF definition
    ;

prefixMetadataUsage
    : ownedFeatureTyping
    ;

metadataUsage
    : usageExtensionKeyword* (AT_SIGN | METADATA) metadataUsageDeclaration (
        ABOUT annotation ( COMMA annotation)*
    )? metadataBody
    ;

metadataUsageDeclaration
    : (identification? ( COLON | TYPED BY))? ownedFeatureTyping
    ;

metadataBodyUsageMember
    : metadataBodyUsage
    ;

metadataBodyUsage
    : REF? (COLON_GT_GT | REDEFINES)? ownedRedefinition featureSpecializationPart? valuePart? metadataBody
    ;

extendedDefinition
    : basicDefinitionPrefix? definitionExtensionKeyword+ DEF definition
    ;

extendedUsage
    : unextendedUsagePrefix usageExtensionKeyword+ usage
    ;

filterPackageImportDeclaration
    : membershipImport
    | namespaceImportDirect
    ;

namespaceImportDirect
    : qualifiedName COLON_COLON STAR (COLON_COLON STAR_STAR)?
    ;

// ===== Stub rules for undefined references =====
// These rules are referenced in the spec but not fully defined.
// They need manual review and completion.

calculationUsageDeclaration
    : usageDeclaration? valuePart?
    ;

emptyActionUsage_
    : /* epsilon */
    ;

emptyFeature_
    : /* epsilon */
    ;

emptyMultiplicity_
    : /* epsilon */
    ;

emptyUsage_
    : /* epsilon */
    ;

filterPackageImport
    : IDENTIFIER /* TODO: stub for filterPackageImport */
    ;

nonFeatureChainPrimaryExpression
    : IDENTIFIER /* TODO: stub for nonFeatureChainPrimaryExpression */
    ;

portConjugation
    : /* epsilon */
    ;