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
  | nested
  | ruleset
  | mixinDeclaration
  | functionDeclaration
  | variableDeclaration
  | includeDeclaration
  | ifDeclaration
  | forDeclaration
  | whileDeclaration
  | eachDeclaration
  ;



//Params to mixins, includes, etc
params
  : param (COMMA param)* Ellipsis?
  ;

param
  : variableName paramOptionalValue?
  ;

variableName
  : DOLLAR Identifier
  ;

paramOptionalValue
  : COLON expression+
  ;


//MIXINS
mixinDeclaration
  : '@mixin' Identifier (LPAREN params? RPAREN)? block
  ;

//Includes
includeDeclaration
  : INCLUDE Identifier (';' | (LPAREN values? RPAREN ';'?)? block?)
  ;

//FUNCTIONS
functionDeclaration
  : '@function' Identifier LPAREN params? RPAREN BlockStart functionBody? BlockEnd
  ;

functionBody
  : functionStatement* functionReturn
  ;

functionReturn
  : '@return' commandStatement ';'
  ;

functionStatement
  : commandStatement ';' | statement
  ;


commandStatement
  : (expression+ | '(' commandStatement ')') mathStatement?
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
  | NULL
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
  | NULL
  ;

condition
  : commandStatement (( '==' | LT | GT | '!=') conditions)?
  | LPAREN conditions ')'
  ;

variableDeclaration
  : variableName COLON values '!default'? ';'
  ;


//for
forDeclaration
  : AT_FOR variableName 'from' fromNumber 'through' throughNumber block
  ;

fromNumber
  : Number
  ;
throughNumber
  : Number
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
  :  Identifier (COMMA Identifier)*
  |  identifierListOrMap (COMMA identifierListOrMap)*
  ;

identifierListOrMap
  : LPAREN identifierValue (COMMA identifierValue)* RPAREN
  ;

identifierValue
  : identifier (COLON values)?
  ;


//Imports
importDeclaration
	: '@import' referenceUrl mediaTypes? ';'
	;

referenceUrl
    : StringLiteral
    | UrlStart Url UrlEnd
    ;


mediaTypes
  : (Identifier (COMMA Identifier)*)
  ;




//Nested (stylesheets, etc)
nested
 	: '@' nest selectors BlockStart stylesheet BlockEnd
	;

nest
	: (Identifier | '&') Identifier* pseudo*
	;





//Rules
ruleset
 	: selectors block
	;

block
  : BlockStart (property ';' | statement)* property? BlockEnd
  ;

selectors
	: selector (COMMA selector)*
	;

selector
	: element+ (selectorPrefix element)* attrib* pseudo?
	;

selectorPrefix
  : (GT | PLUS | TIL)
  ;

element
	: identifier
  | '#' identifier
  | '.' identifier
  | '&'
  | '*'
	;

pseudo
	: (COLON|COLONCOLON) Identifier
	| (COLON|COLONCOLON) functionCall
	;

attrib
	: '[' Identifier (attribRelate (StringLiteral | Identifier))? ']'
	;

attribRelate
	: '='
	| '~='
	| '|='
	;

identifier
  : Identifier identifierPart*
  | InterpolationStart identifierVariableName BlockEnd identifierPart*
  ;

identifierPart
  : InterpolationStartAfter identifierVariableName BlockEnd
  | IdentifierAfter
  ;
identifierVariableName
  : DOLLAR (Identifier | IdentifierAfter)
  ;

property
	: identifier COLON values
	;

values
	: commandStatement (COMMA commandStatement)*
	;

url
  : UrlStart Url UrlEnd
  ;

measurement
  : Number Unit?
  ;


functionCall
	: Identifier LPAREN values? RPAREN
	;
