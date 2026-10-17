/******************************
* Parser for OCL 2.4 with extensions for textual notations
* for UML classes and usecases. 
*
* Arrow operators ->op are used consistently for any OCL 
* operator, not just collection operators. 
* 
* Copyright (c) 2003--2023 Kevin Lano
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar OCL;

multipleContextSpecifications
    : (singleInvariant | singleDerivedAttribute)+ EOF
    ;

contextSpecification
    : (singleInvariant | singleDerivedAttribute) EOF
    ;

singleInvariant
    : 'context' ID 'inv' ID? ':' expression
    ;

singleDerivedAttribute
    : 'context' qualified_name ':' type ('init:' expression)? 'derive:' expression
    ;

type
    : 'Sequence' '(' type ')'
    | 'Set' '(' type ')'
    | 'Bag' '(' type ')'
    | 'OrderedSet' '(' type ')'
    | 'Ref' '(' type ')'  
    | 'Map' '(' type ',' type ')'
    | 'Function' '(' type ',' type ')'
    | identifier
    ;

expressionList
    : (expression ',')* expression
    ;

expression
    : logicalExpression
    | conditionalExpression
    | letExpression
    ;

conditionalExpression
    : 'if' expression 'then' expression 'else' expression 'endif'
    ;

letExpression
    : 'let' letBinding (',' letBinding)* 'in' expression
    ;

letBinding
    : ID (':' type)? '=' expression
    ;


basicExpression
    : NULL_LITERAL
    | BOOLEAN_LITERAL
    | basicExpression '.' ID
    | basicExpression '(' expressionList? ')'
    | basicExpression '[' expression ']'
    | INT
    | FLOAT_LITERAL
    | STRING1_LITERAL
    | STRING2_LITERAL
    | ENUMERATION_LITERAL
    | ID
    | '(' expression ')'
    ;

///////////////////////////////////////////////////////////////////////////////
// Infix precedence:
// Expression precedence levels (lowest precedence first)
///////////////////////////////////////////////////////////////////////////////

logicalExpression
    : equalityExpression (('and' | '&' | 'or' | 'xor' | '=>' | 'implies') equalityExpression)* ;

equalityExpression
    : additiveExpression (('=' | '<' | '>' | '>=' | '<=' | '/=' | '<>' | ':' | '/:' | '<:') additiveExpression)* ;
    
additiveExpression
    : multiplicativeExpression (('+' | '-' | '..' | '|->') multiplicativeExpression)* ;

multiplicativeExpression
    : unaryExpression (('*' | '/' | 'mod' | 'div') unaryExpression)* ;

unaryExpression
    : ('not' | '-' | '+' | '?' | '!') unaryExpression
    | navigationExpression
    ;

//////////////////////////////////////////////////////////////////////////////
// Postfix chaning expression handling.
// 'navigationExpression' is a postfix expression that 
// is direct left recursive: it can appear on LHS of ->
// ->subrange is used for ->substring and ->subSequence
//////////////////////////////////////////////////////////////////////////////

navigationExpression
    : primaryFactor (postfixSuffix)* ;

primaryFactor
    : setExpression
    | basicExpression
    ;

postfixSuffix
    : '.' 'allInstances' '(' ')'
    | '.' 'oclType' '(' ')'
    | '.' 'oclIsUndefined' '(' ')'
    | '.' 'oclIsInvalid' '(' ')'
    | '.' 'oclIsNew' '(' ')'
    | '.' 'oclAsSet' '(' ')'
    | '.' 'oclIsTypeOf' '(' expression ')'
    | '.' 'oclIsKindOf' '(' expression ')'
    | '.' 'oclAsType' '(' expression ')' ('.' ID)?
    | '.' 'size' '(' ')'
    | '.' 'max' '(' ')'
    | '.' 'min' '(' ')'
    | '.' 'indexOf' '(' expression ')'
    | '.' 'at' '(' expression ')' ('.' ID)?
    | '.' ID '(' (expression (',' expression)*)? ')' ('.' ID)?  // Generic dot operation with optional args and chaining
    | '.' ID
    | '->' 'size' '(' ')'
    | '->' 'isEmpty' '(' ')'
    | '->' 'notEmpty' '(' ')'
    | '->' 'asSet' '(' ')'
    | '->' 'asBag' '(' ')'
    | '->' 'asOrderedSet' '(' ')'
    | '->' 'asSequence' '(' ')'
    | '->' 'any' '(' ')' ('.' ID)?
    | '->' 'first' '(' ')' ('.' ID)?
    | '->' 'last' '(' ')' ('.' ID)?
    | '->' 'reverse' '(' ')'
    | '->' 'floor' '(' ')'
    | '->' 'round' '(' ')'
    | '->' 'abs' '(' ')'
    | '->' 'oclType' '(' ')'
    | '->' 'oclIsUndefined' '(' ')'
    | '->' 'oclIsInvalid' '(' ')'
    | '->' 'oclIsNew' '(' ')'
    | '->' 'sum' '(' ')'
    | '->' 'max' '(' ')'
    | '->' 'min' '(' ')'
    | '->' 'characters' '(' ')'
    | '->' 'toInteger' '(' ')'
    | '->' 'toReal' '(' ')'
    | '->' 'toBoolean' '(' ')'
    | '->' 'toUpperCase' '(' ')'
    | '->' 'toLowerCase' '(' ')'
    | '->' (
        'union'
        | 'intersection'
        | 'includes'
        | 'excludes'
        | 'including'
        | 'excluding'
        | 'includesAll'
        | 'symmetricDifference'
        | 'excludesAll'
        | 'prepend'
        | 'append'
        | 'count'
        | 'indexOf'
        | 'count'
      ) '(' expression ')'
    | '->' 'equalsIgnoreCase' '(' expression ')'
    | '->' ('oclAsType' | 'at') '(' expression ')' ('.' ID)?
    | '->' ('oclIsTypeOf' | 'oclIsKindOf' | 'oclAsSet') '(' expression ')'
    | '->' 'collect' '(' (identOptType '|')? expression ')'
    | '->' 'select' '(' (identOptType '|')? expression ')'
    | '->' 'reject' '(' (identOptType '|')? expression ')'
    | '->' 'forAll' '(' (identOptTypeList '|')? expression ')'
    | '->' 'exists' '(' (identOptTypeList '|')? expression ')'
    | '->' 'one' '(' (identOptType '|')? expression ')'
    | '->' 'any' '(' (identOptType '|')? expression ')' ('.' ID)?
    | '->' 'closure' '(' (identOptType '|')? expression ')'
    | '->' 'sortedBy' '(' (identOptType '|')? expression ')'
    | '->' 'isUnique' '(' (identOptType '|')? expression ')'
    | '->' 'insertAt' '(' expression ',' expression ')'
    | '->' 'iterate' '(' identifier ';' identOptType '=' expression '|' expression ')'
    | '->' ID '(' (expression (',' expression)*)? ')' ('.' ID)?  // Generic arrow operation with optional args and chaining
    ;

///////////////////////////////////////////////////////////////////////////////


identOptType
    : ID (':' type)?
    ;

identOptTypeList
    : identOptType (',' identOptType)*
    ;

setExpression
    : 'OrderedSet{' expressionList? '}'
    | 'Bag{' expressionList? '}'
    | 'Set{' expressionList? '}'
    | 'Sequence{' expressionList? '}'
    | 'Map{' expressionList? '}'
    ;

identifier
    : ID
    ;

qualified_name
    : ENUMERATION_LITERAL
    ;


BOOLEAN_LITERAL
    : ('true' | 'false')
    ;

FLOAT_LITERAL
    : Digits '.' Digits
    ;

STRING1_LITERAL
    : '"' (~["\\\r\n] | EscapeSequence)* '"'
    ;

STRING2_LITERAL
    : '\'' (~['\\\r\n] | EscapeSequence)* '\'';

ENUMERATION_LITERAL
    : ID '::' ID
    ;

NULL_LITERAL
    : 'null'
    ;

MULTILINE_COMMENT
    : '/*' .*? '*/' -> channel(HIDDEN)
    ;

// Skip comments starting with '--'
LINE_COMMENT: '--' ~[\r\n]* -> skip;

fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;

fragment Digits
    : [0-9]+
    ;

NEWLINE
    : [\r\n]+ -> skip
    ;

INT
    : [0-9]+
    ;

ID
    : [a-zA-Z_$]+ [a-zA-Z0-9_$@]*
    ; // match identifiers

WS
    : [ \t\n\r]+ -> skip
    ;
