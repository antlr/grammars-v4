/*
 [The "BSD licence"]
 Copyright (c) 2012 George S. Cowan
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


/** Java 7 grammar for Antlr 4
    Based on Terence Parr's Java 6 grammar at
      https://github.com/antlr/grammars-v4/tree/master/java
    with changes from the Java 7 Language Specification
      http://docs.oracle.com/javase/specs/jls/se7/jls7.pdf
    but with my own refactorings.
  */
grammar Java7;

/* the keywords enum and assert have compiler options to allow their use as
   identifiers instead. This presents a problem best solved with
   implementation dependent code. I have commented out a Java solution.
 */
// @lexer::members {
//   protected boolean enumIsKeyword = true;  // A subclass of Java7Parser.java can
//   protected boolean assertIsKeyword = true;// accept parameters and override these
// }

/*******************************************************************************
        Parser section -
*******************************************************************************/

compilationUnit
  : packageDeclaration? importDeclaration* typeDeclaration* EOF
  ;

packageDeclaration
  : annotation* 'package' qualifiedIdentifier ';'
  ;

importDeclaration
  : 'import' 'static'? qualifiedIdentifier ('.' '*')? ';'
  ;

typeDeclaration
  : classOrInterfaceDeclaration
  | ';'
  ;

classOrInterfaceDeclaration
  : modifier* (classDeclaration | interfaceDeclaration)
  ;


classDeclaration
  : normalClassDeclaration
  | enumDeclaration
  ;

normalClassDeclaration
  : 'class' Identifier typeParameters?
    ('extends' typeRef)?
    ('implements' typeList)?
    classBody
  ;

typeParameters
  : '<' typeParameter (',' typeParameter)* '>'
  ;

typeParameter
  : Identifier ('extends' bound)?
  ;

bound
  : classOrInterfaceType ('&' classOrInterfaceType)*
  ;

enumDeclaration
  : ENUM Identifier ('implements' typeList)?
    '{' enumConstants?
        ( ';' classBodyDeclaration* )?
    '}'
  ;

enumConstants
  // The comma after the last enumConstant is optional.
  // Oddly, there may also be a comma without any enumConstants
  : enumConstant (',' enumConstant)* ','?
  | ','
  ;

interfaceDeclaration
  : normalInterfaceDeclaration
  | annotationTypeDeclaration
  ;
normalInterfaceDeclaration
  : 'interface' Identifier typeParameters? ('extends' typeList)? interfaceBody
  ;

classBody
  :   '{' classBodyDeclaration* '}'
  ;

interfaceBody
  : '{'
       ( modifier* interfaceMemberDecl | ';' )*
    '}'
  ;

classBodyDeclaration
  : ';'
  | modifier* memberDecl
  | 'static'? block
  ;

memberDecl
  : methodDeclaration // generic and void methods are here, too
  | fieldDeclaration
  | constructorDeclaration
  | interfaceDeclaration
  | classDeclaration
  ;

fieldDeclaration
  : typeRef variableDeclarators ';'
  ;

block
  : '{' blockStatement* '}'
  ;

blockStatement
  : localVariableDeclaration ';'
  | classOrInterfaceDeclaration
  | statement
  ;

enumConstant
  : annotation* Identifier arguments? classBody?
  ;

typeList
  : classOrInterfaceType (',' classOrInterfaceType)*
  ;

typeArguments
  : '<' typeArgument (',' typeArgument)* '>'
  ;

typeArgument
  : classOrArrayType
  | '?' ( ('extends' | 'super') classOrArrayType )?
  ;

classOrArrayType
// Includes primitive arrays, which are really a kind of class.
// Just like rule typeRef, except that a simple primative is forbidden.
  : primitiveType '[' ']' ('[' ']')*
  | classOrInterfaceType  ('[' ']')*
  ;

interfaceMemberDecl
  : interfaceMethodOrFieldDecl
  | interfaceGenericMethodDecl
  | 'void' Identifier voidInterfaceMethodDeclaratorRest
  | interfaceDeclaration
  | classDeclaration
  ;


/** Allows brackets after parameters for backwards compatibility only; do not use. */
methodDeclaration
  : typeParameters? (typeRef | 'void')
    Identifier formalParameters ('[' ']')*   ('throws' qualifiedIdentifierList)?
    ( methodBody
    | ';'
    )
  ;

constructorDeclaration
  : typeParameters? Identifier formalParameters ('throws' qualifiedIdentifierList)?
    '{'
        explicitConstructorInvocation?
        blockStatement*
    '}'
  ;

variableModifier
  : 'final'
  | annotation
  ;

interfaceMethodOrFieldDecl
  : typeRef Identifier interfaceMethodOrFieldRest
  ;

interfaceMethodOrFieldRest
  : constantDeclaratorsRest ';'
  | interfaceMethodDeclaratorRest
  ;

interfaceMethodDeclaratorRest
  : formalParameters ('[' ']')* ('throws' qualifiedIdentifierList)? ';'
  ;

interfaceGenericMethodDecl
  : typeParameters (typeRef | 'void') Identifier
    interfaceMethodDeclaratorRest
  ;

voidInterfaceMethodDeclaratorRest
  : formalParameters ('throws' qualifiedIdentifierList)? ';'
  ;

constantDeclarator
  : Identifier constantDeclaratorRest
  ;

variableDeclarators
  : variableDeclarator (',' variableDeclarator)*
  ;

variableDeclarator
  : variableDeclaratorId ('=' variableInitializer)?
  ;

constantDeclaratorsRest
  : constantDeclaratorRest (',' constantDeclarator)*
  ;

constantDeclaratorRest
  : ('[' ']')* '=' variableInitializer
  ;

variableDeclaratorId
  : Identifier ('[' ']')*
  ;

variableInitializer
  : arrayInitializer
  | expression
  ;

arrayInitializer
  : '{' ( variableInitializer (',' variableInitializer)* (',')?
        )?
    '}'
  ;

modifier
  : annotation
  | 'public'
  | 'protected'
  | 'private'
  | 'static'
  | 'abstract'
  | 'final'       // not for interface or annotation declarations
  | 'native'
  | 'synchronized'
  | 'transient'
  | 'volatile'
  | 'strictfp'
  ;

packageOrTypeName
  : qualifiedIdentifier
  ;

enumConstantName
  : Identifier
  ;

typeName
  : qualifiedIdentifier
  ;

typeRef
  : classOrInterfaceType ('[' ']')*
  | primitiveType ('[' ']')*
  ;

/** classOrInterfaceType is not used in the Java 7 Language Specification. We use
    it as a substitute name for the ReferenceType that is defined in the Syntax
    section (Chapter 18, p. 593). The reason that we do not just use the rule name
    referenceType instead of classOrInterfaceType is that elsewhere in the
    Java 7 Reference (topic 4.3, p.52), ReferenceType is defined differently.
    We are doing our best here to avoid confusion.
  */
classOrInterfaceType
  : Identifier typeArguments? ( '.' Identifier typeArguments? )*
  ;

primitiveType
  : 'boolean'
  | 'char'
  | 'byte'
  | 'short'
  | 'int'
  | 'long'
  | 'float'
  | 'double'
  ;

qualifiedIdentifierList
  : qualifiedIdentifier (',' qualifiedIdentifier)*
  ;

formalParameters
  : '(' formalParameterDeclarations? ')'
  ;

formalParameterDeclarations
// if there is a variable arity parameter, it is the last parameter
  :  variableModifier* typeRef
       ('...' variableDeclaratorId
       | variableDeclaratorId (',' formalParameterDeclarations)?
       )
  ;

methodBody
  : block
  ;

explicitConstructorInvocation
  : nonWildcardTypeArguments? ('this' | 'super') arguments ';'
  | primary '.' nonWildcardTypeArguments? 'super' arguments ';'
  ;

qualifiedIdentifier
  : Identifier ('.' Identifier)*
  ;
literal
  : integerLiteral
  | FloatingPointLiteral
  | CharacterLiteral
  | StringLiteral
  | booleanLiteral
  | 'null'
  ;

integerLiteral
  : HexLiteral
  | OctalLiteral
  | DecimalLiteral
  | BinaryLiteral
  ;

booleanLiteral
  : 'true'
  | 'false'
  ;

// ANNOTATIONS

annotation
  : '@' annotationName ( '(' ( elementValuePairs | elementValue )? ')' )?
  ;

annotationName
  : Identifier ('.' Identifier)*
  ;

elementValuePairs
  : elementValuePair (',' elementValuePair)*
  ;

elementValuePair
  : Identifier '=' elementValue
  ;

elementValue
  : expression
  | annotation
  | elementValueArrayInitializer
  ;

elementValueArrayInitializer
  : '{' (elementValue (',' elementValue)*)? (',')? '}'
  ;

annotationTypeDeclaration
  : '@' 'interface' Identifier
    '{'
       (modifier* annotationTypeElement)*
    '}'
  ;

annotationTypeElement
  : typeRef (annotationMethod | variableDeclarators) ';'
  | classDeclaration ';'?
  | normalInterfaceDeclaration ';'?
  | enumDeclaration ';'?
  | annotationTypeDeclaration ';'?
  ;

annotationMethod
  : Identifier '(' ')' defaultValue?
  ;

defaultValue
  : 'default' elementValue
  ;

// STATEMENTS / BLOCKS

localVariableDeclaration
  : variableModifier* typeRef variableDeclarators
  ;

statement
  : block
  | ASSERT expression (':' expression)? ';'
  | 'if' parExpression statement ('else' statement)?
  | 'for' '(' forControl ')' statement
  | 'while' parExpression statement
  | 'do' statement 'while' parExpression ';'
  | tryStatement
  | 'switch' parExpression switchBlock
  | 'synchronized' parExpression block
  | 'return' expression? ';'
  | 'throw' expression ';'
  | 'break' Identifier? ';'
  | 'continue' Identifier? ';'
  | ';'
  | statementExpression ';'
  | Identifier ':' statement
  ;

tryStatement
  // must contain at least one resource, catch, or finally
  : 'try' '(' resources ')' block   catchClause* ('finally' block)?
  | 'try'                   block ( catchClause+ ('finally' block)?
                                  |               'finally' block
                                  )
  ;

catchClause
  : 'catch' '(' variableModifier* typeName ('|' typeName)* Identifier ')' block
  ;

resources
  // Semicolon may be ommited for last resource
  : resource (';' resource)* ';'?
  ;

resource
  : variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
  ;

switchBlock
  : '{' switchBlockStatementGroup* switchLabel* '}'
  ;

switchBlockStatementGroup
  : switchLabel+ blockStatement*
  ;

switchLabel
  : 'case' constantExpression ':'
  | 'case' enumConstantName ':'
  | 'default' ':'
  ;

forControl
  : enhancedForControl
  | forInit? ';' expression? ';' forUpdate?
  ;

forInit
  : localVariableDeclaration
  | expressionList
  ;

enhancedForControl
  : variableModifier* typeRef Identifier ':' expression
  ;

forUpdate
  : expressionList
  ;

// EXPRESSIONS

parExpression
  : '(' expression ')'
  ;

expressionList
  : expression (',' expression)*
  ;

statementExpression
  : expression
  ;

constantExpression
  : expression
  ;

expression
  : primary
  | expression '.' Identifier
  | expression '.' 'this'
  | expression '.' 'super' '(' expressionList? ')'
  | expression '.' 'new' Identifier '(' expressionList? ')'
  | expression '.' 'super' '.' Identifier arguments?
  | expression '.' explicitGenericInvocation
  | expression '[' expression ']'
  | expression '(' expressionList? ')'
  | expression ('++' | '--')
  | ('+'|'-'|'++'|'--') expression
  | ('~'|'!') expression
  | '(' typeRef ')' expression
  | 'new' creator
  | expression ('*'|'/'|'%') expression
  | expression ('+'|'-') expression
  | expression ('<' '<' | '>' '>' '>' | '>' '>') expression
  | expression ('<' '=' | '>' '=' | '>' | '<') expression
  | expression 'instanceof' typeRef
  | expression ('==' | '!=') expression
  | expression '&' expression
  | expression '^' expression
  | expression '|' expression
  | expression '&&' expression
  | expression '||' expression
  | expression '?' expression ':' expression
  | expression
    ( '^='            <assoc=right>
    | '+='            <assoc=right>
    | '-='            <assoc=right>
    | '*='            <assoc=right>
    | '/='            <assoc=right>
    | '&='            <assoc=right>
    | '|='            <assoc=right>
    | '='             <assoc=right>
    | '>' '>' '='     <assoc=right>
    | '>' '>' '>' '=' <assoc=right>
    | '<' '<' '='     <assoc=right>
    | '%='            <assoc=right>
    )
    expression
  ;

primary
  : '(' expression ')'
  | 'this'
  | 'super'
  | literal
  | Identifier
  | typeRef '.' 'class'
  | 'void' '.' 'class'
  ;

creator
  : nonWildcardTypeArguments createdName classCreatorRest
  | createdName (arrayCreatorRest | classCreatorRest)
  ;

createdName
  : primitiveType
  | // classOrInterfaceType but with possible <>
    Identifier (typeArguments | '<' '>')? ('.' Identifier (typeArguments | '<' '>')? )*
  ;

innerCreator
  : (nonWildcardTypeArguments | '<' '>')? Identifier classCreatorRest
  ;

explicitGenericInvocation
  : nonWildcardTypeArguments Identifier arguments
  ;

arrayCreatorRest
  : '[' ']' ('[' ']')* arrayInitializer
  | '[' expression ']' ('[' expression ']')* ('[' ']')*
  ;

classCreatorRest
  : arguments classBody?
  ;

nonWildcardTypeArguments
  : '<' typeList '>'
  ;

arguments
  : '(' expressionList? ')'
  ;


// LEXER =====================================================

HexLiteral
  // underscores may be freely inserted after first hex digit and before last
  : '0' ('x'|'X')
    HexDigits
    IntegerTypeSuffix?
  ;

DecimalLiteral
  // Only a single zero digit may begin with a zero
  // Underscores may be freely inserted after first digit and before last
  : ( '0' | '1'..'9' ('_'* Digit)* ) IntegerTypeSuffix?
  ;

OctalLiteral
  // Underscores may be freely inserted before the last digit.
  // Don't know why underscores here are different from others -
  // Maybe the leading 0 is considered a digit as well as a marker
  // indicating that the following is a base 8 number
  : '0' ('_'* '0'..'7')+ IntegerTypeSuffix?
  ;

BinaryLiteral
  // underscores may be freely inserted after first digit and before last
  : '0' ('b'|'B')
    BinaryDigit ('_'* BinaryDigit)*
    IntegerTypeSuffix?
  ;

fragment
BinaryDigit : ('0'|'1') ;

fragment
HexDigits : HexDigit ('_'* HexDigit)* ;

fragment
HexDigit : (Digit|'a'..'f'|'A'..'F') ;

fragment
Digits : Digit ('_'* Digit)* ;

fragment
Digit : '0'..'9' ;

fragment
IntegerTypeSuffix : ('l'|'L') ;

FloatingPointLiteral
  : Digits '.' Digits? Exponent? FloatTypeSuffix?
  | '.' Digits Exponent? FloatTypeSuffix?
  | Digits Exponent FloatTypeSuffix?
  | Digits FloatTypeSuffix

    // Hex float literal
  | ('0x' | '0X')
    HexDigits? ('.' HexDigits?)?
    ( 'p' | 'P' ) ( '+' | '-' )? Digits // note decimal exponent
    FloatTypeSuffix?
  ;

fragment
Exponent : ('e'|'E') ('+'|'-')? Digits ;

fragment
FloatTypeSuffix : ('f'|'F'|'d'|'D') ;

CharacterLiteral
  : '\'' ( EscapeSequence | ~('\''|'\\') ) '\''
  ;

StringLiteral
  :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
  ;

fragment
EscapeSequence
  : '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
  | UnicodeEscape
  | OctalEscape
  ;

fragment
OctalEscape
  : '\\' ('0'..'3') ('0'..'7') ('0'..'7')
  | '\\' ('0'..'7') ('0'..'7')
  | '\\' ('0'..'7')
  ;

fragment
UnicodeEscape
  : '\\' 'u' HexDigit HexDigit HexDigit HexDigit
  ;

ENUM
  : 'enum' // {if (!enumIsKeyword) setType(Identifier);}
  ;

ASSERT
  : 'assert' // {if (!assertIsKeyword) setType(Identifier);}
  ;

Identifier
  : Letter (Letter|JavaIDDigit)*
  ;

/**I found this char range in JavaCC's grammar, but Letter and Digit overlap.
   Still works, but...
 */
fragment
Letter
  : '\u0024'                 // $
  | '\u0041'..'\u005a'       // A-Z
  | '\u005f'                 // _
  | '\u0061'..'\u007a'       // a-z
  | '\u00c0'..'\u00d6'       // Latin Capital Letter A with grave - Latin Capital letter O with diaeresis
  | '\u00d8'..'\u00f6'       // Latin Capital letter O with stroke - Latin Small Letter O with diaeresis
  | '\u00f8'..'\u00ff'       // Latin Small Letter O with stroke - Latin Small Letter Y with diaeresis
  | '\u0100'..'\u1fff'       // Latin Capital Letter A with macron - Latin Small Letter O with stroke and acute
  | '\u3040'..'\u318f'       // Hiragana
  | '\u3300'..'\u337f'       // CJK compatibility
  | '\u3400'..'\u3d2d'       // CJK compatibility
  | '\u4e00'..'\u9fff'       // CJK compatibility
  | '\uf900'..'\ufaff'       // CJK compatibility
  ;

fragment
JavaIDDigit
  : '\u0030'..'\u0039'       // 0-9
  | '\u0660'..'\u0669'       // Arabic-Indic Digit 0-9
  | '\u06f0'..'\u06f9'       // Extended Arabic-Indic Digit 0-9
  | '\u0966'..'\u096f'       // Devanagari 0-9
  | '\u09e6'..'\u09ef'       // Bengali 0-9
  | '\u0a66'..'\u0a6f'       // Gurmukhi 0-9
  | '\u0ae6'..'\u0aef'       // Gujarati 0-9
  | '\u0b66'..'\u0b6f'       // Oriya 0-9
  | '\u0be7'..'\u0bef'       // Tami 0-9
  | '\u0c66'..'\u0c6f'       // Telugu 0-9
  | '\u0ce6'..'\u0cef'       // Kannada 0-9
  | '\u0d66'..'\u0d6f'       // Malayala 0-9
  | '\u0e50'..'\u0e59'       // Thai 0-9
  | '\u0ed0'..'\u0ed9'       // Lao 0-9
  | '\u1040'..'\u1049'       // Myanmar 0-9?
  ;

WS
  : [ \r\t\u000C\n]+ -> channel(HIDDEN)
  ;

COMMENT
  : '/*' .*? '*/'    -> channel(HIDDEN)
  ;

LINE_COMMENT
  : '//' ~[\r\n]* -> channel(HIDDEN)
  ;

