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
    : ws
    ( charset ( Comment | Space | Cdo | Cdc )* )*
    ( import_ ( Comment | Space | Cdo | Cdc )* )*
    ( namespace_ ( Comment | Space | Cdo | Cdc )* )*
    ( nestedStatement ( Comment | Space | Cdo | Cdc )* )*
    EOF
    ;

charset
    : Charset ws String_ ws Semi ws
    | Charset ws String_ ws
    ;

import_
    : (Import | Use | Require) ws ( String_ | Uri ) ws mediaQueryList Semi ws
    | (Import | Use | Require) ws ( String_ | Uri ) ws Semi ws
    | (Import | Use | Require) ws ( String_ | Uri ) ws mediaQueryList
    | (Import | Use | Require) ws ( String_ | Uri ) ws
    ;

// Namespaces
// https://www.w3.org/TR/css-namespaces-3/
namespace_
    : Namespace ws (namespacePrefix ws)? ( String_ | Uri ) ws Semi ws
    | Namespace ws (namespacePrefix ws)? ( String_ | Uri ) ws
    ;

namespacePrefix
    : ident
    ;

// Media queries
// https://www.w3.org/TR/css3-mediaqueries/
media
    : Media ws mediaQueryList groupRuleBody ws
    ;

mediaQueryList
    : ( mediaQuery ( Comma ws mediaQuery )* )? ws
    ;

mediaQuery
    : ( MediaOnly | Not )? ws mediaType ws ( And ws mediaExpression )*
    | mediaExpression ( And ws mediaExpression )*
    ;

mediaType
    : ident
    ;

// Grammar allows for 'and(', which gets tokenized as Function. In practice, people always insert space before '(' to
// have it work on Chrome.
mediaExpression
    : Lparen ws mediaFeature ( Colon ws expr )? Rparen ws
    ;

mediaFeature
    : ident ws
    ;

// Page
page
    : Page ws pseudoPage? BlockStart ws declaration? ( Semi ws declaration? )* BlockEnd ws
    ;

pseudoPage
    : Colon ident ws
    ;

// Selectors
// https://www.w3.org/TR/css3-selectors/
selectorGroup
    : selector ( Comma ws selector )*
    ;

selector
    : simpleSelectorSequence ws ( combinator simpleSelectorSequence ws )*
    ;

combinator
    : Plus ws
    | Greater ws
    | Tilde ws
    | Space ws
    ;

simpleSelectorSequence
    : ( typeSelector | universal ) ( Hash | className | attrib | pseudo | negation )*
    | ( Hash | className | attrib | pseudo | negation )+
    ;

typeSelector
    : typeNamespacePrefix? elementName
    ;

typeNamespacePrefix
    : ( ident | Times )? Pipe
    ;

elementName
    : ident
    ;

universal
    : typeNamespacePrefix? Times
    ;

className
    : Dot ident
    ;

attrib
    : Lbrack ws typeNamespacePrefix? ident ws
    ( ( PrefixMatch | SuffixMatch | SubstringMatch | Eq | Includes | DashMatch ) ws
    ( ident | String_ ) ws )? Rbrack
    ;

pseudo
    /* '::' starts a pseudo-element, ':' a pseudo-class */
    /* Exceptions: :first-line, :first-letter, :before And :after. */
    /* Note that pseudo-elements are restricted to one per selector And */
    /* occur MediaOnly in the last simple_selector_sequence. */
    : Colon Colon? ( ident | functionalPseudo )
    ;

functionalPseudo
    : Function_ ws expression Rparen
    ;

expression
    /* In CSS3, the expressions are identifiers, strings, */
    /* or of the form "an+b" */
    : ( ( Plus | Minus | Dimension | UnknownDimension | Number | String_ | ident ) ws )+
    ;

negation
    : PseudoNot ws negationArg ws Rparen
    ;

negationArg
    : typeSelector
    | universal
    | Hash
    | className
    | attrib
    | pseudo
    ;

// Rules
operator_
    : Div ws
    | Comma ws
    | Space ws
    | Eq ws  // IE filter and DXImageTransform function
    ;

property_
    : ident ws
    | Variable ws
    | Times ident  // IE hacks
    | Under ident  // IE hacks
    ;

ruleset
    : selectorGroup BlockStart ws declarationList? BlockEnd ws
    | any_* BlockStart ws declarationList? BlockEnd ws
    ;

declarationList
    : ( Semi ws )* declaration ws ( Semi ws declaration? )*
    ;

declaration
    : property_ Colon ws expr prio?
    | property_ Colon ws value
    ;

prio
    : Important ws
    ;

value
    : ( any_ | block | atKeyword ws )+
    ;

expr
    : term ( operator_? term )*
    ;

term
    : number ws
    | percentage ws
    | dimension ws
    | String_ ws
    | UnicodeRange ws
    | ident ws
    | var_
    | Uri ws
    | hexcolor
    | calc
    | function_
    | unknownDimension ws
    | dxImageTransform
    ;

function_
    : Function_ ws expr Rparen ws
    ;

dxImageTransform
    : DxImageTransform ws expr Rparen ws    // IE DXImageTransform function
    ;

hexcolor
    : Hash ws
    ;

number
    : ( Plus | Minus )? Number
    ;

percentage
    : ( Plus | Minus )? Percentage
    ;

dimension
    : ( Plus | Minus )? Dimension
    ;

unknownDimension
    : ( Plus | Minus )? UnknownDimension
    ;

// Error handling
any_
    : ident ws
    | number ws
    | percentage ws
    | dimension ws
    | unknownDimension ws
    | String_ ws
    | Delim ws
    | Uri ws
    | Hash ws
    | UnicodeRange ws
    | Includes ws
    | DashMatch ws
    | Colon ws
    | Function_ ws ( any_ | unused )* Rparen ws
    | Lparen ws ( any_ | unused )* Rparen ws
    | Lbrack ws ( any_ | unused )* Rbrack ws
    ;

atRule
    : atKeyword ws any_* ( block | Semi ws )
    ;

atKeyword
    : At ident
    ;

unused
    : block
    | atKeyword ws
    | Semi ws
    | Cdo ws
    | Cdc ws
    ;

block
    : BlockStart ws (  declarationList | nestedStatement | any_ | block | atKeyword ws | Semi ws )* BlockEnd ws
    ;

// Conditional
// https://www.w3.org/TR/css3-conditional/
nestedStatement
    : ruleset
    | media
    | page
    | fontFaceRule
    | keyframesRule
    | supportsRule
    | viewport
    | counterStyle
    | fontFeatureValuesRule
    | atRule
    ;

groupRuleBody
    : BlockStart ws nestedStatement* BlockEnd ws
    ;

supportsRule
    : Supports ws supportsCondition ws groupRuleBody
    ;

supportsCondition
    : supportsNegation
    | supportsConjunction
    | supportsDisjunction
    | supportsConditionInParens
    ;

supportsConditionInParens
    : Lparen ws supportsCondition ws Rparen
    | supportsDeclarationCondition
    | generalEnclosed
    ;

supportsNegation
    : Not ws Space ws supportsConditionInParens
    ;

supportsConjunction
    : supportsConditionInParens ( ws Space ws And ws Space ws supportsConditionInParens )+
    ;

supportsDisjunction
    : supportsConditionInParens ( ws Space ws Or ws Space ws supportsConditionInParens )+
    ;

supportsDeclarationCondition
    : Lparen ws declaration Rparen
    ;

generalEnclosed
    : ( Function_ | Lparen ) ( any_ | unused )* Rparen
    ;

// Variable
// https://www.w3.org/TR/css-variables-1
var_
    : Var ws Variable ws Rparen ws
    ;

// Calc
// https://www.w3.org/TR/css3-values/#calc-syntax
calc
    : Calc ws calcSum Rparen ws
    ;

calcSum
    : calcProduct ( Space ws ( Plus | Minus ) ws Space ws calcProduct )*
    ;

calcProduct
    : calcValue ( Times ws calcValue | Div ws number ws )*
    ;

calcValue
    : number ws
    | dimension ws
    | unknownDimension ws
    | percentage ws
    | Lparen ws calcSum Rparen ws
    ;

// Font face
// https://www.w3.org/TR/2013/CR-css-fonts-3-20131003/#font-face-rule
fontFaceRule
    : FontFace ws BlockStart ws fontFaceDeclaration? ( Semi ws fontFaceDeclaration? )* BlockEnd ws
    ;

fontFaceDeclaration
    : property_ Colon ws expr     # knownFontFaceDeclaration
    | property_ Colon ws value    # unknownFontFaceDeclaration
    ;

// Animations
// https://www.w3.org/TR/css3-animations/
keyframesRule
    : Keyframes ws Space ws ident ws BlockStart ws keyframesBlocks BlockEnd ws
    ;

keyframesBlocks
    : ( keyframeSelector BlockStart ws declarationList? BlockEnd ws )*
    ;

keyframeSelector
    : ( From | To | Percentage ) ws ( Comma ws ( From | To | Percentage ) ws )*
    ;

// Viewport
// https://www.w3.org/TR/css-device-adapt-1/
viewport
    : Viewport ws BlockStart ws declarationList? BlockEnd ws
    ;

// Counter style
// https://www.w3.org/TR/css-counter-styles-3/
counterStyle
    : CounterStyle ws ident ws BlockStart ws declarationList? BlockEnd ws
    ;

// Font feature values
// https://www.w3.org/TR/css-fonts-3/
fontFeatureValuesRule
    : FontFeatureValues ws fontFamilyNameList ws BlockStart ws featureValueBlock* BlockEnd ws
    ;

fontFamilyNameList
    : fontFamilyName ( ws Comma ws fontFamilyName )*
    ;

fontFamilyName
    : String_
    | ident ( ws ident )*
    ;

featureValueBlock
    : featureType ws BlockStart ws featureValueDefinition? ( ws Semi ws featureValueDefinition? )* BlockEnd ws
    ;

featureType
    : atKeyword
    ;

featureValueDefinition
    : ident ws Colon ws number ( ws number )*
    ;

// The specific words can be identifiers too
ident
    : Ident
    | MediaOnly
    | Not
    | And
    | Or
    | From
    | To
    ;

// Comments might be part of CSS hacks, thus pass them to visitor to decide whether to skip
// Spaces are significant around '+' '-' '(', thus they should not be skipped
ws
    : ( Comment | Space )*
    ;

/*
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
*/