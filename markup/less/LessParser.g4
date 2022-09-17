/*
 [The "MIT licence"]
 Copyright (c) 2014 Kyle Lee
 All rights reserved.
*/

parser grammar LessParser;

options
   { tokenVocab = LessLexer; }

stylesheet
   : statement* EOF
   ;

statement
   : importDeclaration
   | ruleset
   | variableDeclaration ';'
   | mixinDefinition
   ;

variableName
   : AT variableName
   | AT Identifier
   ;

commandStatement
   : (expression +) mathStatement?
   ;

mathCharacter
   : TIMES
   | PLUS
   | DIV
   | MINUS
   | PERC
   ;

mathStatement
   : mathCharacter commandStatement
   ;

expression
   : measurement
   | identifier IMPORTANT
   | identifier
   | identifier LPAREN values? RPAREN
   | Color
   | StringLiteral
   | url
   | variableName IMPORTANT
   | variableName
   ;

function_
   : FUNCTION_NAME LPAREN values? RPAREN
   ;

conditions
   : condition ((AND | COMMA) condition)*
   ;

condition
   : LPAREN conditionStatement RPAREN
   | NOT LPAREN conditionStatement RPAREN
   ;

conditionStatement
   : commandStatement (EQ | LT | GT | GTEQ | LTEQ) commandStatement
   | commandStatement
   ;

variableDeclaration
   : variableName COLON values
   ;

//Imports
importDeclaration
   : '@import' (LPAREN (importOption (COMMA importOption)*) RPAREN)? referenceUrl mediaTypes? ';'
   ;

importOption
   : REFERENCE
   | INLINE
   | LESS
   | CSS
   | ONCE
   | MULTIPLE
   ;

referenceUrl
   : StringLiteral
   | UrlStart Url UrlEnd
   ;

mediaTypes
   : (Identifier (COMMA Identifier)*)
   ;

//Rules
ruleset
   : selectors block
   ;

block
   : BlockStart (property_ ';' | statement | mixinReference)* property_? BlockEnd
   ;

mixinDefinition
   : selectors LPAREN (mixinDefinitionParam (';' mixinDefinitionParam)*)? Ellipsis? RPAREN mixinGuard? block
   ;

mixinGuard
   : WHEN conditions
   ;

mixinDefinitionParam
   : variableName
   | variableDeclaration
   ;

mixinReference
   : selector LPAREN values? RPAREN IMPORTANT? ';'
   ;

selectors
   : selector (COMMA selector)*
   ;

selector
   : element + attrib* pseudo?
   ;

attrib
   : '[' Identifier (attribRelate (StringLiteral | Identifier))? ']'
   ;

negation
   : COLON NOT LPAREN LBRACK? selectors RBRACK? RPAREN
   ;

pseudo
   : (COLON | COLONCOLON) Identifier
   ;

element
   : selectorPrefix identifier
   | identifier
   | '#' identifier
   | pseudo
   | negation
   | PARENTREF
   | '*'
   ;

selectorPrefix
   : (GT | PLUS | TIL)
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
   : (Identifier | IdentifierAfter)
   ;

property_
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
