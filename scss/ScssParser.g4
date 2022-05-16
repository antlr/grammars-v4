/*
 [The "BSD licence"]
 Copyright (c) 2014 Vlad Shlosberg
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

parser grammar ScssParser;

options { tokenVocab=ScssLexer; }

stylesheet
  : statement*
  ;

statement
  : importDeclaration
  | mediaDeclaration
  | ruleset
  | mixinDeclaration
  | contentDeclaration
  | functionDeclaration
  | variableDeclaration
  | includeDeclaration
  | ifDeclaration
  | forDeclaration
  | whileDeclaration
  | eachDeclaration
  | fontFaceDeclaration
  | keyframesDeclaration
  ;


// Params declared by rules such as @mixin and @function.
declaredParams
  : declaredParam (COMMA declaredParam)* Ellipsis?
  ;

declaredParam
  : variableName paramOptionalValue?
  ;

variableName
  : namespace? (DOLLAR | MINUS_DOLLAR | PLUS_DOLLAR) Identifier
  ;

paramOptionalValue
  : COLON expression+
  ;

// Params passed to rules such as @include and @content.
passedParams
  : passedParam (COMMA passedParam)* (COMMA|Ellipsis)?
  ;

passedParam
  : (variableName COLON)? (commandStatement | listSpaceSeparated | listBracketed | map_)
  ;

// MIXINS and related rules
mixinDeclaration
  : MIXIN (FunctionIdentifier declaredParams? RPAREN |
           Identifier (LPAREN declaredParams? RPAREN)?) block
  ;

contentDeclaration
  : CONTENT (LPAREN passedParams? RPAREN)? SEMI
  ;

includeDeclaration
  : INCLUDE (Identifier | functionCall)
    (SEMI | (USING LPAREN declaredParams RPAREN)? block)?
  ;


// FUNCTIONS
functionDeclaration
  : FUNCTION (FunctionIdentifier | Identifier LPAREN) declaredParams? RPAREN
    BlockStart functionBody? BlockEnd
  ;

functionBody
  : functionStatement* functionReturn
  ;

functionReturn
  : RETURN commandStatement SEMI
  ;

functionStatement
  : commandStatement SEMI | statement
  ;

fontFaceDeclaration
 : FONT_FACE block
 ;

keyframesDeclaration
  : KEYFRAMES identifier? block
  ;

commandStatement
  : (expression
      | (LPAREN | MINUS_LPAREN | PLUS_LPAREN) commandStatement RPAREN
    ) mathStatement?
  ;

mathCharacter
  : TIMES | PLUS | DIV | MINUS | PERC
  ;

mathStatement
  : mathCharacter commandStatement
  ;


expression
  : measurement
  | identifier
  | Color
  | StringLiteral
  | NULL_
  | Var
  | url
  | variableName
  | functionCall
  ;


//If statement
ifDeclaration
  : AT_IF conditions block elseIfStatement* elseStatement?
  ;

elseIfStatement
  : AT_ELSE IF conditions block
  ;

elseStatement
  : AT_ELSE block
  ;

conditions
  : condition (COMBINE_COMPARE conditions)?
  | NULL_
  ;

condition
  : commandStatement (( EQEQ | LT | GT | NOTEQ) conditions)?
  | LPAREN conditions RPAREN
  ;


variableDeclaration
  : variableName COLON (propertyValue | listBracketed | map_) POUND_DEFAULT? SEMI
  ;


//for
forDeclaration
  : AT_FOR variableName FROM fromNumber (TO|THROUGH) throughNumber block
  ;

fromNumber
  : Number
  ;

throughNumber
  : Number
  | functionCall
  ;

//while
whileDeclaration
  : AT_WHILE conditions block
  ;

//EACH
eachDeclaration
  : AT_EACH variableName (COMMA variableName)* IN eachValueList block
  ;

eachValueList
  : commandStatement
  | list_
  | map_
  ;

//Imports
importDeclaration
  : IMPORT referenceUrl SEMI
  | REQUIRE  referenceUrl SEMI?
  | USE referenceUrl asClause? withClause? SEMI?
  | FORWARD referenceUrl asClause? (showClause | hideClause)?
  ;

referenceUrl
    : StringLiteral
    | UrlStart Url UrlEnd
    ;

asClause
  : AS (TIMES | identifier)
  ;

withClause
  : WITH LPAREN keywordArgument (COMMA keywordArgument)* COMMA? RPAREN
  ;

keywordArgument
  : identifierVariableName COLON expression
  ;

hideClause
  : HIDE memberName (COMMA memberName)*
  ;

showClause
  : SHOW memberName (COMMA memberName)
  ;

memberName
  : DOLLAR? identifier
  ;

// MEDIA
mediaDeclaration
  : MEDIA mediaQueryList block
  ;

mediaQueryList
  : (mediaQuery (COMMA mediaQuery)* )?
  ;

mediaQuery
  : (ONLY | NOT)? mediaType (AND_WORD mediaExpression)*
  | mediaExpression (AND_WORD mediaExpression)*
  ;

// Typically only 'all', 'print', 'screen', and 'speech', but there are some
// deprecated values too.
mediaType
  : Identifier
  ;

mediaExpression
  : LPAREN mediaFeature (COLON commandStatement)? RPAREN
  ;

// Typically 'max-width', 'hover', 'orientation', etc. Many possible values.
mediaFeature
  : Identifier
  ;


//Rules
ruleset
  : selectors block
  ;

block
  : BlockStart (property_ | statement)* lastProperty? BlockEnd
  ;

selectors
  : selector (COMMA selector)*
  ;

selector
  : element+
  ;

element
  : identifier
  | Color identifier
  | HASH identifier
  | DOT identifier
  | AND
  | TIMES
  | combinator
  | attrib
  | pseudo
  ;

combinator
  : (GT | PLUS | TIL)
  ;

pseudo
  : pseudoIdentifier
  | pseudoIdentifier LPAREN (selector | commandStatement) RPAREN
  ;

attrib
  : LBRACK Identifier (attribRelate (StringLiteral | Identifier))? RBRACK
  ;

attribRelate
  : EQ
  | PIPE_EQ
  | TILD_EQ
  ;

identifier
  : Identifier identifierPart*
  | InterpolationStart identifierVariableName BlockEnd identifierPart*
  // These are keywords in some contexts, but can be used as identifiers too.
  | AND_WORD
  | FROM
  | NOT
  | ONLY
  | THROUGH
  | TO
  | USING
  ;

pseudoIdentifier
  : PseudoIdentifier identifierPart*
  ;

identifierPart
  : InterpolationStartAfter identifierVariableName BlockEnd
  | IdentifierAfter
  ;
identifierVariableName
  : DOLLAR (Identifier | IdentifierAfter)
  ;

property_
  : identifier COLON propertyValue IMPORTANT? SEMI
  | identifier COLON block
  | identifier COLON propertyValue IMPORTANT? block
  ;

lastProperty
  : identifier COLON propertyValue IMPORTANT?
  ;

propertyValue
  : commandStatement (COMMA? commandStatement)*
  ;

url
  : UrlStart Url UrlEnd
  ;

measurement
  : Number Unit?
  ;


functionCall
  : namespace? FunctionIdentifier passedParams? RPAREN
  ;

namespace
  : (Identifier DOT)+
  ;


list_
  : listCommaSeparated
  | listSpaceSeparated
  | listBracketed
  ;

listCommaSeparated
  : listElement (COMMA listElement)+ COMMA?
  ;

listSpaceSeparated
  : listElement listElement+
  ;

listBracketed
  : LBRACK (listCommaSeparated | listSpaceSeparated) RBRACK
  ;

listElement
  : commandStatement
  | LPAREN list_ RPAREN
  ;


map_
  : LPAREN mapEntry (COMMA mapEntry)* COMMA? RPAREN
  ;

mapEntry
  : mapKey COLON mapValue;

mapKey
  : commandStatement
  | list_
  | map_
  ;

mapValue
  : commandStatement
  | list_
  | map_
  ;
