/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved;

Redistribution and  in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1; Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer;
2; Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution;
3; Neither the name of Tom Everett nor the names of its contributors
   may be d to endorse or promote products derived from this software
   without specific prior written permission;

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED; IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE;
*/

// Transcribed from "Grammar of Mizar", version 7.4.01, February 10, 2005,
// by Grzegorz Bancerek: https://mizar.uwb.edu.pl/language/syntax.html
grammar mizar;

article
   : environmentDeclaration textProper '.'
   ;

environmentDeclaration
   : 'environ' directive*
   ;

directive
   : vocabularyDirective
   | libraryDirective
   | requirementDirective
   ;

vocabularyDirective
   : 'vocabularies' vocabularyName (',' vocabularyName)* ';'
   ;

vocabularyName
   : fileName
   ;

libraryDirective
   : ('notations' | 'constructors' | 'registrations' | 'definitions' | 'theorems' | 'schemes') articleName (',' articleName)* ';'
   ;

articleName
   : fileName
   ;

requirementDirective
   : 'requirements' requirement (',' requirement)* ';'
   ;

requirement
   : fileName
   ;

textProper
   : section (section)*
   ;

section
   : 'begin' (textItem)*
   ;

textItem
   : reservation
   | definitionalItem
   | registrationItem
   | notationItem
   | theorem
   | schemeItem
   | auxiliaryItem
   | canceledTheorem
   ;

reservation
   : 'reserve' reservationSegment (',' reservationSegment)* ';'
   ;

reservationSegment
   : reservedIdentifiers 'for' typeExpression
   ;

reservedIdentifiers
   : identifier (',' identifier)*
   ;

definitionalItem
   : definitionalBlock ';'
   ;

registrationItem
   : registrationBlock ';'
   ;

notationItem
   : notationBlock ';'
   ;

definitionalBlock
   : 'definition' ((definitionItem | definition))* (redefinitionBlock)? 'end'
   ;

redefinitionBlock
   : 'redefine' ((definitionItem | definition))*
   ;

registrationBlock
   : 'registration' ((lociDeclaration | clusterRegistration))* 'end'
   ;

notationBlock
   : 'notation' ((lociDeclaration | notationDeclaration))* 'end'
   ;

definitionItem
   : lociDeclaration
   | permissiveAssumption
   | auxiliaryItem
   ;

notationDeclaration
   : attributeSynonym
   | attributeAntonym
   | functorSynonym
   | modeSynonym
   | predicateSynonym
   | predicateAntonym
   ;

lociDeclaration
   : 'let' qualifiedVariables ('such' conditions)? ';'
   ;

permissiveAssumption
   : assumption
   ;

definition
   : structureDefinition
   | modeDefinition
   | functorDefinition
   | predicateDefinition
   | attributeDefinition
   | canceledDefinition
   ;

structureDefinition
   : 'struct' ('(' ancestors ')')? structureSymbol ('over' loci)? '(#' fields '#)' ';'
   ;

ancestors
   : structureTypeExpression (',' structureTypeExpression)*
   ;

structureSymbol
   : symbol
   ;

loci
   : locus (',' locus)*
   ;

fields
   : fieldSegment (',' fieldSegment)*
   ;

locus
   : variableIdentifier
   ;

variableIdentifier
   : identifier
   ;

fieldSegment
   : selectorSymbol (',' selectorSymbol)* specification
   ;

selectorSymbol
   : symbol
   ;

specification
   : '->' typeExpression
   ;

modeDefinition
   : 'mode' modePattern ((specification)? ('means' definiens)? ';' correctnessConditions | 'is' typeExpression ';')
   ;

modePattern
   : modeSymbol ('of' loci)?
   ;

modeSymbol
   : symbol
   ;

modeSynonym
   : 'synonym' modePattern 'for' modePattern ';'
   ;

definiens
   : simpleDefiniens
   | conditionalDefiniens
   ;

simpleDefiniens
   : (':' labelIdentifier ':')? (sentence | termExpression)
   ;

labelIdentifier
   : identifier
   ;

conditionalDefiniens
   : (':' labelIdentifier ':')? partialDefiniensList ('otherwise' (sentence | termExpression))?
   ;

partialDefiniensList
   : partialDefiniens (',' partialDefiniens)*
   ;

partialDefiniens
   : (sentence | termExpression) 'if' sentence
   ;

functorDefinition
   : 'func' functorPattern (specification)? (('means' | 'equals') definiens)? ';' correctnessConditions (functorProperty)*
   ;

functorPattern
   : (functorLoci)? functorSymbol (functorLoci)?
   | leftFunctorBracket loci rightFunctorBracket
   ;

functorProperty
   : ('commutativity' | 'idempotence' | 'involutiveness' | 'projectivity') justification ';'
   ;

functorSynonym
   : 'synonym' functorPattern 'for' functorPattern ';'
   ;

functorLoci
   : locus
   | '(' loci ')'
   ;

functorSymbol
   : symbol
   ;

leftFunctorBracket
   : symbol
   ;

rightFunctorBracket
   : symbol
   ;

predicateDefinition
   : 'pred' predicatePattern ('means' definiens)? ';' ('correctness' justification ';')? (predicateProperty)*
   ;

predicatePattern
   : (loci)? predicateSymbol (loci)?
   ;

predicateProperty
   : ('symmetry' | 'asymmetry' | 'connectedness' | 'reflexivity' | 'irreflexivity') justification ';'
   ;

predicateSynonym
   : 'synonym' predicatePattern 'for' predicatePattern ';'
   ;

predicateAntonym
   : 'antonym' predicatePattern 'for' predicatePattern ';'
   ;

predicateSymbol
   : symbol
   ;

attributeDefinition
   : 'attr' attributePattern 'means' definiens ';' ('correctness' justification ';')?
   ;

attributePattern
   : locus 'is' attributeSymbol
   ;

attributeSynonym
   : 'synonym' (attributePattern | predicatePattern) 'for' attributePattern ';'
   ;

attributeAntonym
   : 'antonym' (attributePattern | predicatePattern) 'for' attributePattern ';'
   ;

attributeSymbol
   : symbol
   ;

canceledDefinition
   : 'canceled' (numeral)? ';'
   ;

clusterRegistration
   : existentialRegistration
   | conditionalRegistration
   | functorialRegistration
   ;

existentialRegistration
   : 'cluster' adjectiveCluster typeExpression ';' correctnessConditions
   ;

adjectiveCluster
   : (adjective)*
   ;

adjective
   : ('non')? attributeSymbol
   ;

conditionalRegistration
   : 'cluster' adjectiveCluster '->' adjectiveCluster typeExpression ';' correctnessConditions
   ;

functorialRegistration
   : 'cluster' termExpression '->' adjectiveCluster ';' correctnessConditions
   ;

correctnessConditions
   : (correctnessCondition)* ('correctness' justification ';')?
   ;

correctnessCondition
   : ('existence' | 'uniqueness' | 'coherence' | 'compatibility' | 'consistency') justification ';'
   ;

theorem
   : 'theorem' compactStatement
   ;

schemeItem
   : schemeBlock ';'
   ;

schemeBlock
   : 'scheme' schemeIdentifier '{' schemeParameters '}' ':' schemeConclusion ('provided' schemePremise ('and' schemePremise)*)? 'proof' (reasoningItem)* 'end'
   ;

schemeIdentifier
   : identifier
   ;

schemeParameters
   : schemeSegment (',' schemeSegment)*
   ;

schemeConclusion
   : sentence
   ;

schemePremise
   : proposition
   ;

schemeSegment
   : predicateSegment
   | functorSegment
   ;

predicateSegment
   : predicateIdentifier (',' predicateIdentifier)* '[' (typeExpressionList)? ']'
   ;

predicateIdentifier
   : identifier
   ;

functorSegment
   : functorIdentifier (',' functorIdentifier)* '(' (typeExpressionList)? ')' specification
   ;

functorIdentifier
   : identifier
   ;

auxiliaryItem
   : statement
   | privateDefinition
   ;

canceledTheorem
   : 'canceled' (numeral)? ';'
   ;

privateDefinition
   : constantDefinition
   | privateFunctorDefinition
   | privatePredicateDefinition
   ;

constantDefinition
   : 'set' equatingList ';'
   ;

equatingList
   : equating (',' equating)*
   ;

equating
   : variableIdentifier '=' termExpression
   ;

privateFunctorDefinition
   : 'deffunc' privateFunctorPattern '=' termExpression
   ;

privatePredicateDefinition
   : 'defpred' privatePredicatePattern 'means' sentence
   ;

privateFunctorPattern
   : functorIdentifier '(' (typeExpressionList)? ')'
   ;

privatePredicatePattern
   : predicateIdentifier '[' (typeExpressionList)? ']'
   ;

reasoning
   : (reasoningItem)* ('per' 'cases' simpleJustification ';' (caseList | supposeList))?
   ;

caseList
   : case (case)*
   ;

case
   : 'case' (proposition | conditions) ';' (reasoningItem)* 'end' ';'
   ;

supposeList
   : suppose (suppose)*
   ;

suppose
   : 'suppose' (proposition | conditions) ';' (reasoningItem)* 'end' ';'
   ;

reasoningItem
   : auxiliaryItem
   | skeletonItem
   ;

skeletonItem
   : generalization
   | assumption
   | conclusion
   | exemplification
   ;

generalization
   : 'let' qualifiedVariables ('such' conditions)? ';'
   ;

assumption
   : singleAssumption
   | collectiveAssumption
   | existentialAssumption
   ;

singleAssumption
   : 'assume' proposition ';'
   ;

collectiveAssumption
   : 'assume' conditions ';'
   ;

existentialAssumption
   : 'given' qualifiedVariables ('such' conditions)? ';'
   ;

conclusion
   : ('thus' | 'hence') compactStatement
   | diffuseConclusion
   ;

diffuseConclusion
   : 'thus' diffuseStatement
   | 'hereby' reasoning 'end' ';'
   ;

exemplification
   : 'take' example (',' example)* ';'
   ;

example
   : termExpression
   | variableIdentifier '=' termExpression
   ;

statement
   : ('then')? linkableStatement
   | diffuseStatement
   ;

linkableStatement
   : compactStatement
   | choiceStatement
   | typeChangingStatement
   | iterativeEquality
   ;

compactStatement
   : proposition justification ';'
   ;

choiceStatement
   : 'consider' qualifiedVariables ('such' conditions)? simpleJustification ';'
   ;

typeChangingStatement
   : 'reconsider' typeChangeList 'as' typeExpression simpleJustification ';'
   ;

typeChangeList
   : (equating | variableIdentifier) (',' (equating | variableIdentifier))*
   ;

iterativeEquality
   : termExpression '=' termExpression simpleJustification ('.=' termExpression simpleJustification)+ ';'
   ;

diffuseStatement
   : (labelIdentifier ':')? 'now' reasoning 'end' ';'
   ;

justification
   : simpleJustification
   | proof
   ;

simpleJustification
   : straightforwardJustification
   | schemeJustification
   ;

proof
   : ('proof' | '@proof') reasoning 'end'
   ;

straightforwardJustification
   : ('by' references)?
   ;

schemeJustification
   : 'from' schemeReference ('(' references ')')?
   ;

references
   : reference (',' reference)*
   ;

reference
   : localReference
   | libraryReference
   ;

schemeReference
   : localSchemeReference
   | librarySchemeReference
   ;

localReference
   : labelIdentifier
   ;

localSchemeReference
   : schemeIdentifier
   ;

libraryReference
   : articleName ':' (theoremNumber | 'def' definitionNumber) (',' (theoremNumber | 'def' definitionNumber))*
   ;

librarySchemeReference
   : articleName ':' 'sch' schemeNumber
   ;

theoremNumber
   : numeral
   ;

definitionNumber
   : numeral
   ;

schemeNumber
   : numeral
   ;

conditions
   : 'that' proposition ('and' proposition)*
   ;

proposition
   : (labelIdentifier ':')? sentence
   ;

sentence
   : formulaExpression
   ;

formulaExpression
   : '(' formulaExpression ')'
   | atomicFormulaExpression
   | quantifiedFormulaExpression
   | formulaExpression '&' formulaExpression
   | formulaExpression 'or' formulaExpression
   | formulaExpression 'implies' formulaExpression
   | formulaExpression 'iff' formulaExpression
   | 'not' formulaExpression
   | 'contradiction'
   | 'thesis'
   ;

atomicFormulaExpression
   : (termExpressionList)? predicateSymbol (termExpressionList)?
   | predicateIdentifier '[' (termExpressionList)? ']'
   | termExpression 'is' adjective (adjective)*
   | termExpression 'is' typeExpression
   ;

quantifiedFormulaExpression
   : 'for' qualifiedVariables ('st' formulaExpression)? ('holds' formulaExpression | quantifiedFormulaExpression)
   | 'ex' qualifiedVariables 'st' formulaExpression
   ;

qualifiedVariables
   : implicitlyQualifiedVariables
   | explicitlyQualifiedVariables
   | explicitlyQualifiedVariables ',' implicitlyQualifiedVariables
   ;

implicitlyQualifiedVariables
   : variables
   ;

explicitlyQualifiedVariables
   : qualifiedSegment (',' qualifiedSegment)*
   ;

qualifiedSegment
   : variables qualification
   ;

variables
   : variableIdentifier (',' variableIdentifier)*
   ;

qualification
   : ('being' | 'be') typeExpression
   ;

// The reference's "Adjective-Cluster Type-Expression" alternative is nullable on
// the left (Adjective-Cluster = {Adjective}), which would make this indirectly
// left-recursive through zero consumed tokens; ANTLR4 can't rewrite that away
// automatically (only direct self-recursion), so it's tightened to "one or more"
// adjectives here, which denotes the same language (the zero-adjective case is
// already covered by falling through to radixType).
typeExpression
   : '(' typeExpression ')'
   | (adjective)+ typeExpression
   | radixType
   ;

structureTypeExpression
   : '(' structureTypeExpression ')'
   | adjectiveCluster structureSymbol ('over' termExpressionList)?
   ;

radixType
   : modeSymbol ('of' termExpressionList)?
   | structureSymbol ('over' termExpressionList)?
   ;

typeExpressionList
   : typeExpression (',' typeExpression)*
   ;

// The reference's "[Arguments] Functor-Symbol [Arguments]" alternative is
// indirectly left-recursive (Arguments = Term-Expression | '(' ... ')'), which
// ANTLR4 rejects as mutual left recursion across two rules (it only auto-rewrites
// *direct* self-recursion within one rule). The leading-argument case is split
// out here into its two shapes (bare term-expression, now directly
// left-recursive and so handled automatically; parenthesized list, which starts
// with a literal token and so isn't left-recursive) so it compiles; the trailing
// optional argument keeps using the unchanged `arguments` rule, since recursion
// there isn't at the start of the alternative.
termExpression
   : '(' termExpression ')'
   | termExpression functorSymbol (arguments)?
   | '(' termExpressionList ')' functorSymbol (arguments)?
   | functorSymbol (arguments)?
   | leftFunctorBracket termExpressionList rightFunctorBracket
   | functorIdentifier '(' (termExpressionList)? ')'
   | structureSymbol '(#' termExpressionList '#)'
   | variableIdentifier
   | '{' termExpression (postqualification)? ':' sentence '}'
   | numeral
   | termExpression 'qua' typeExpression
   | 'the' selectorSymbol 'of' termExpression
   | 'the' selectorSymbol
   | privateDefinitionParameter
   | 'it'
   ;

arguments
   : termExpression
   | '(' termExpressionList ')'
   ;

termExpressionList
   : termExpression (',' termExpression)*
   ;

postqualification
   : 'where' postqualifyingSegment (',' postqualifyingSegment)*
   ;

postqualifyingSegment
   : postqualifiedVariable (',' postqualifiedVariable)* ('is' | 'are') typeExpression
   ;

postqualifiedVariable
   : identifier
   ;

identifier
   : IDENTIFIER
   ;

fileName
   : IDENTIFIER
   ;

numeral
   : NUMERAL
   ;

symbol
   : SYMBOL
   ;

// Symbol, like Identifier, Numeral and File-Name, is left undefined by the
// reference as a "system term" (its lexical form is determined by the
// vocabulary files a real article imports, which is outside the scope of a
// context-free grammar). To keep it lexically distinguishable from IDENTIFIER
// without ambiguity, it's defined here as a run of special characters that
// doesn't overlap with any multi-character literal used elsewhere in the
// grammar ('->', '(#', '#)').
SYMBOL
   : [+*/<=~^&|@%!?]+
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

NUMERAL
   : [0-9]+
   ;

privateDefinitionParameter
   : '$1'
   | '$2'
   | '$3'
   | '$4'
   | '$5'
   | '$6'
   | '$7'
   | '$8'
   | '$9'
   | '$10'
   ;

WS
   : [ \r\n\t]+ -> skip
   ;
