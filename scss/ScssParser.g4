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
    : ( statement ( Comment | Space | Cdo | Cdc )* )* EOF
    ;


statement
    : charset
    | nestedStatement
    | import_
    | cssNamespace_
    | variableDeclaration
    | comment
    ;

comment
    : Comment Space? (Comment Space?)*
    ;

charset
    : Charset ws String_ ws Semi ws
    | Charset ws String_ ws
    ;

import_
    : importDerective ws importPath ws mediaQueryList Semi
    | importDerective ws importPath ws Semi
    | importDerective ws function_ ws Semi
    | importDerective ws importPath
    ;

importDerective
    : Import
    | Use
    | Require
    | Forward
    | Include
    ;

importPath
    : String_
    | Uri
    ;

// Namespaces
// https://www.w3.org/TR/css-namespaces-3/
cssNamespace_
    : Namespace ws (cssNamespacePrefix ws)? ( String_ | Uri ) ws Semi ws
    | Namespace ws (cssNamespacePrefix ws)? ( String_ | Uri ) ws
    ;

cssNamespacePrefix
    : ident
    ;

namespace_
    : (ident Dot)+
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
    : combinator? simpleSelectorSequence ws ( combinator simpleSelectorSequence ws )*
    ;

combinator
    : Plus ws
    | Greater ws
    | Tilde ws
    | Space ws
    ;

simpleSelectorSequence
    : ( typeSelector | universal ) ( Hash | className | attrib | pseudo | negation | interpolation | parent )*
    | ( Hash | className | attrib | pseudo | negation | interpolation | parent )+
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

interpolation
    : (elementName Dot)? Hash BlockStart namespace_? varialbeName BlockEnd
    ;

parent
    : Amp
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
    : Ident Lparen ws pseudoParameter+ Rparen
    ;

pseudoParameter
    : ( ( expr | className | interpolation ) ws Comma? ws)
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
    : Div
    | Comma
    | Space
    | Eq  // IE filter and DXImageTransform function
    ;

variableDeclaration
    : varialbeName ws Colon ws varialbeValue ws prio? Semi
    ;

varialbeName
    : namespace_ Dollar ident
    | ((Minus Minus) | Dollar | Plus Dollar | Minus Dollar) ident
    ;

varialbeValue
    : value
    | expr
    | function_
    | map_
    | list_
    | varialbeName
    ;

ruleset
    : selectorGroup block
    | any_* block
    ;

declarationList
    : ( Semi ws )* declaration ws ( Semi ws declaration? )*
    ;

declaration
    : variableDeclaration
    | propertyDeclaration
    | selectorGroup
    | include
    ;

propertyDeclaration
    : propertyName Colon ws expr ws prio?
    | propertyName Colon ws value
    | propertyName Colon ws varialbeName
    ;

propertyName
    : ident
    ;

prio
    : Important
    | Default
    ;

value
    : ( term | block | atKeyword ws )+
    ;

expr
    : ( term | function_ ) Comma? ws ( ( operator_? term | function_ ) Comma? ws )*
    | (Plus | Minus)? Lparen ws calcExpression Rparen ws expr*
    ;

term
    : number
    | percentage
    | dimension
    | boolean
    | String_
    | UnicodeRange
    | varialbeName
    | ident
    | var_
    | Uri
    | Format
    | hexcolor
    | calc
    | rotate
    | unknownDimension
    | dxImageTransform
    ;

function_
    : namespace_? ident? ws Lparen ws functionParameters ws Rparen
    ;

functionParameters
    : ( list_ | map_ )?
    ;

dxImageTransform
    : DxImageTransform ws expr Rparen    // IE DXImageTransform function
    ;

hexcolor
    : Hash
    ;

number
    : ( Plus | Minus )? Number
    ;

percentage
    : ( Plus | Minus )? Percentage
    ;

degree
    : ( Plus | Minus )? Degree
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
    | boolean ws
    | percentage ws
    | dimension ws
    | unknownDimension ws
    | String_ ws
    | Name ws
    | Delim ws
    | Uri ws
    | Hash ws
    | UnicodeRange ws
    | Includes ws
    | DashMatch ws
    | Colon ws
    | Ident Lparen ws ( any_ | unused )* Rparen ws
    | Lparen ws ( any_ | unused )* Rparen ws
    | Lbrack ws ( any_ | unused )* Rbrack ws
    ;

list_
  : listCommaSeparated
  | listSpaceSeparated
  | listBracketed
  ;

listCommaSeparated
  : listElement ws (Comma ws listElement)+ ws Comma?
  ;

listSpaceSeparated
  : listElement ws listElement+
  ;

listBracketed
  : Lbrack ws (listCommaSeparated | listSpaceSeparated) ws Rbrack
  ;

listElement
  : expr
  | Lparen ws list_ ws Rparen
  ;

map_
  : Lparen ws mapEntry ws (Comma ws mapEntry)* ws Comma? ws Rparen
  ;

mapEntry
  : mapKey ws Colon ws mapValue;

mapKey
  : list_
  | map_
  | expr
  ;

mapValue
  : list_
  | map_
  | varialbeName
  | expr
  ;

boolean
    : True
    | False
    ;

atRule
    : atKeyword ws any_* ( block | Semi ws )
    ;

atKeyword
    : include
    | At ident
    ;

include
    : Include ws
    ;

unused
    : block
    | atKeyword ws
    | Semi ws
    | Cdo ws
    | Cdc ws
    ;

block
    : BlockStart ws (  declarationList | nestedStatement | atKeyword )* BlockEnd ws
    ;

// Conditional
// https://www.w3.org/TR/css3-conditional/
nestedStatement
    : ruleset
    | media
    | page
    | fontFaceRule
    | keyframesDeclaration
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
    | function_
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

// Variable
// https://www.w3.org/TR/css-variables-1
var_
    : Var ws Variable ws Rparen ws
    ;

// Calc
// https://www.w3.org/TR/css3-values/#calc-syntax
calc
    : Calc ws calcExpression Rparen ws
    ;

calcExpression
    : calcProduct ( Space ws ( Plus | Minus ) ws Space ws calcProduct )*
    ;

calcProduct
    : calcValue ( Times ws calcValue | Div ws number ws )*
    ;

calcValue
    : number ws
    | dimension ws
    | varialbeName
    | unknownDimension ws
    | percentage ws
    | Lparen ws calcExpression Rparen ws
    ;

rotate
    : Rotate ws degree Rparen ws
    ;

// Font face
// https://www.w3.org/TR/2013/CR-css-fonts-3-20131003/#font-face-rule
fontFaceRule
    : FontFace ws BlockStart ws declarationList BlockEnd ws
    ;

// Animations
// https://www.w3.org/TR/css3-animations/
keyframesDeclaration
    : Keyframes ws Space ws ident? ws block
    ;

/*keyframesBlocks
    : ( keyframeSelector BlockStart ws declarationList? BlockEnd ws )*
    ;

keyframeSelector
    : ( From | To | Percentage ) ws ( Comma ws ( From | To | Percentage ) ws )*
    ;
*/

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