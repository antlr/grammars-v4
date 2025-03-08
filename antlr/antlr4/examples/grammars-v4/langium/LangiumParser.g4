// ******************************************************************************
// Copyright 2021 TypeFox GmbH
// This program and the accompanying materials are made available under the
// terms of the MIT License, which is available in the project root.
// *****************************************************************************


// $antlr-format alignColons hanging, alignSemicolons hanging, alignTrailingComments true, allowShortBlocksOnASingleLine true
// $antlr-format allowShortRulesOnASingleLine false, columnLimit 150, maxEmptyLinesToKeep 1, minEmptyLines 1, reflowComments false, useTab false

parser grammar LangiumParser;

options {
    tokenVocab = LangiumLexer;
}

file_
    : start_ EOF
    ;

start_
    : ('grammar' ID ('with' ID (',' ID)*)? ('hidden' '(' (ID (',' ID)*)? ')')?)? grammarImport* (
        abstractRule
        | interface
        | type
    )+
    ;

interface
    : 'interface' ID ('extends' ID (',' ID)*)? '{' typeAttribute* '}' ';'?
    ;

typeAttribute
    : featureName '?'? ':' typeDefinition ('=' valueLiteral)? ';'?
    ;

valueLiteral
    : stringLiteral
    | numberLiteral
    | booleanLiteral
    | arrayLiteral
    ;

stringLiteral
    : STRING
    ;

numberLiteral
    : NUMBER
    ;

booleanLiteral
    : 'true'
    | 'false'
    ;

arrayLiteral
    : '[' (valueLiteral (',' valueLiteral)*)? ']'
    ;

typeDefinition
    : unionType
    ;

unionType
    : arrayType (('|' arrayType)+)?
    ;

arrayType
    : referenceType ('[' ']')?
    ;

referenceType
    : simpleType
    | '@' simpleType
    ;

simpleType
    : '(' typeDefinition ')'
    | (ID | primitiveType | STRING)
    ;

primitiveType
    : 'string'
    | 'number'
    | 'boolean'
    | 'Date'
    | 'bigint'
    ;

type
    : 'type' ID '=' typeDefinition ';'?
    ;

abstractRule
    : parserRule_
    | terminalRule
    ;

grammarImport
    : 'import' STRING ';'?
    ;

parserRule_
    : ('entry' | 'fragment')? ruleNameAndParams (
        '*'
        | ('returns' (ID | primitiveType))
        | inferredType
    )? ('hidden' '(' (ID (',' ID)*)? ')')? ':' alternatives ';'
    ;

inferredType
    : ('infer' | 'infers') ID
    ;

ruleNameAndParams
    : ID ('<' (parameter (',' parameter)*)? '>')?
    ;

parameter
    : ID
    ;

alternatives
    : conditionalBranch (('|' conditionalBranch)+)?
    ;

conditionalBranch
    : unorderedGroup
    | '<' disjunction '>' (abstractToken)+
    ;

unorderedGroup
    : group (('&' group)+)?
    ;

group
    : abstractToken (abstractToken+)?
    ;

abstractToken
    : abstractTokenWithCardinality
    | action
    ;

abstractTokenWithCardinality
    : (assignment | abstractTerminal) ('?' | '*' | '+')?
    ;

action
    : '{' (ID | inferredType <true>) ('.' featureName ('=' | '+=') 'current')? '}'
    ;

abstractTerminal
    : keyword
    | ruleCall
    | parenthesizedElement
    | predicatedKeyword
    | predicatedRuleCall
    | predicatedGroup
    ;

keyword
    : STRING
    ;

ruleCall
    : ID ('<' namedArgument (',' namedArgument)* '>')?
    ;

namedArgument
    : (ID '=')? (disjunction)
    ;

disjunction
    : conjunction ('|' conjunction)*
    ;

conjunction
    : negation ('&' negation)*
    ;

negation
    : atom
    | '!' negation
    ;

atom
    : parameterReference
    | parenthesizedCondition
    | booleanLiteral
    ;

parenthesizedCondition
    : '(' disjunction ')'
    ;

parameterReference
    : ID
    ;

predicatedKeyword
    : ('=>' | '->') STRING
    ;

predicatedRuleCall
    : ('=>' | '->') ID ('<' namedArgument (',' namedArgument)* '>')?
    ;

assignment
    : ('=>' | '->')? featureName ('+=' | '=' | '?=') assignableTerminal
    ;

assignableTerminal
    : keyword
    | ruleCall
    | parenthesizedAssignableElement
    | crossReference
    ;

parenthesizedAssignableElement
    : '(' assignableAlternatives ')'
    ;

assignableAlternatives
    : assignableTerminal (('|' assignableTerminal)+)?
    ;

crossReference
    : '[' crossReferenceableTerminal (('|' | ':') crossReferenceableTerminal)? ']'
    ;

crossReferenceableTerminal
    : keyword
    | ruleCall
    ;

parenthesizedElement
    : '(' alternatives ')'
    ;

predicatedGroup
    : ('=>' | '->') '(' alternatives ')'
    ;

returnType
    : (primitiveType | ID)
    ;

terminalRule
    : 'hidden'? 'terminal' ('fragment' ID | ID ('returns' returnType)?) ':' terminalAlternatives ';'
    ;

terminalAlternatives
    : terminalGroup ('|' terminalGroup)*
    ;

terminalGroup
    : terminalToken (terminalToken+)?
    ;

terminalToken
    : terminalTokenElement ('?' | '*' | '+')?
    ;

terminalTokenElement
    : characterRange
    | terminalRuleCall
    | parenthesizedTerminalElement
    | negatedToken
    | untilToken
    | regexToken
    | wildcard
    ;

parenthesizedTerminalElement
    : '(' (('?=' | '?!' | '?<=' | '?<!'))? terminalAlternatives ')'
    ;

terminalRuleCall
    : ID
    ;

negatedToken
    : '!' terminalTokenElement
    ;

untilToken
    : '->' terminalTokenElement
    ;

regexToken
    : RegexLiteral
    ;

wildcard
    : '.'
    ;

characterRange
    : keyword ('..' keyword)?
    ;

featureName
    : 'current'
    | 'entry'
    | 'extends'
    | 'false'
    | 'fragment'
    | 'grammar'
    | 'hidden'
    | 'import'
    | 'interface'
    | 'returns'
    | 'terminal'
    | 'true'
    | 'type'
    | 'infer'
    | 'infers'
    | 'with'
    | primitiveType
    | ID
    ;