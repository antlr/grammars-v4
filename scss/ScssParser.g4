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
stylesheet : statement* EOF;
statement: importDeclaration| variableDeclaration| propertyDeclaration|interpolationDeclaration| mediaDeclaration| mixinDeclaration| contentDeclaration | functionDeclaration| ifDeclaration| forDeclaration| whileDeclaration| eachDeclaration| fontFaceDeclaration| keyframesDeclaration|includeDeclaration| ruleset;

// Import
importDeclaration
    : Import ws importPath ws Semi? ws
    | Require ws importPath ws Semi? ws
    | Use ws importPath ws asClause? ws withClause? ws Semi? ws
    | Forward ws importPath ws
    ;
importPath : uri | String_;
asClause   : As ws (Times | identifier);
withClause : With ws Lparen ws parameters ws Rparen;

// Declarations
variableDeclaration : variableName ws Colon ws variableValue ws prio? ws Semi? ws;
variableValue : value | functionDeclaration | functionCall | mapDeclaration | listDeclaration | variableName;
variableName
    : ((Minus Minus) Dollar | plusMinus Dollar | Dollar) identifier
    | namespace_? Dollar identifier
    ;
namespace_: (identifier Dot)+;

propertyDeclaration
    :identifier Colon ws propertyValue ws Semi? ws;
prio : Important | Default ;
propertyValue: value ws prio?| value? ws prio? ws block|variableName|valueList|expression|functionCall;
valueList: (value ws Comma? ws)+ ws;

mediaDeclaration: Media ws mediaQueryList ws block ws;
mediaQueryList: (mediaQuery ws (Comma ws mediaQuery ws)* )? ws;
mediaQuery: (Only | Not)? ws (identifier|value) ws (And ws mediaExpression)*| ws mediaExpression ws(And ws mediaExpression)* ws;
mediaExpression: Lparen ws identifier ws (Colon ws value)? ws Rparen ws;

mixinDeclaration: Mixin ws (identifier| identifier Lparen ws parameters ws Rparen) ws block;
contentDeclaration: Content ws (Lparen ws parameters ws Rparen)? ws Semi? ws;

fontFaceDeclaration: FontFace ws BlockStart ws statement* BlockEnd ws;
keyframesDeclaration: Keyframes ws Space ws identifier? ws block;
includeDeclaration: Include ws (identifier | functionCall) ws (Semi | Using ws Lparen ws parameters ws Rparen ws)? block? ws;

interpolationDeclaration: interpolation Colon ws propertyValue ws Semi? ws;

// Structure
ruleset: selectorGroup block;
block: BlockStart ws statement* BlockEnd ws;

// Selectors
selectorGroup: selector ( Comma ws selector )*;
selector: combinator? ws selectorSequence ws ( combinator ws selectorSequence ws )*;
combinator: Plus| Greater| Tilde| Space;

selectorSequence
    : ( typeSelector | universal ) ( id | className | attrib | pseudo | negation | interpolation | parent )*
    | ( id | className | attrib | pseudo | negation | interpolation | parent )+
    ;

id: Hash identifier;
typeSelector: typeNamespacePrefix? identifier;
typeNamespacePrefix: ( identifier | Times )? Pipe;
universal: typeNamespacePrefix? Times;
className: Dot identifier;
interpolation: namespace_? Hash BlockStart namespace_? value BlockEnd;
parent: Amp;
attrib
    : Lbrack ws typeNamespacePrefix? identifier ws
    ( ( PrefixMatch | SuffixMatch | SubstringMatch | Eq | Includes | DashMatch ) ws
    ( identifier | String_ ) ws )? Rbrack
    ;
pseudo: Colon Colon? ( identifier | functionalPseudo );
functionalPseudo: Ident Lparen ws pseudoParameter+ Rparen;
pseudoParameter: ( ( value | className | interpolation ) ws Comma? ws);
negation: PseudoNot ws negationArg ws Rparen;
negationArg: typeSelector| universal| Hash| className| attrib | pseudo;

// Operators
operator_: Div|Times|Minus|Plus|Greater|Less|Greater Eq|Less Eq |Eq Eq?|NotEq;
value: (unit|number|boolean|calc|rotate|rgba|var_|uri|Format|functionCall|variableName|interpolation|expression|hexcolor|identifier|String_ | block)+ ws;

// Function
functionDeclaration: Function ws (namespace_? identifier)? ws Lparen ws parameters ws Rparen ws BlockStart ws functionBody? ws BlockEnd ws;
parameters: parameter? (ws Comma ws parameter)* ws;
parameter: (value|expression|variableDeclaration|listSpaceSeparated) arglist? ws;
functionBody: functionStatement* ws functionReturn? ws;
functionReturn: Return ws expression ws Semi ws;
functionStatement: expression Semi | statement;

functionCall: namespace_? identifier ws Lparen ws parameters ws Rparen ws;

expression: expressionPart ws ( operator_ ws expressionPart )* ws;
expressionPart: unit ws| variableName ws|number ws| plusMinus? Lparen ws expression Rparen ws;


// List & Map
listDeclaration: listCommaSeparated| listSpaceSeparated| listBracketed;
listCommaSeparated: listElement ws (Comma ws listElement)+ ws Comma? ws;
listSpaceSeparated: listElement ws listElement* ws Comma? ws;
listBracketed: Lbrack ws (listCommaSeparated | listSpaceSeparated) ws Rbrack ws;
listElement: Lparen? ws value ws Rparen? ws;

mapDeclaration: Lparen ws mapEntry ws (Comma ws mapEntry)* ws Comma? ws Rparen;
mapEntry: mapKey ws Colon ws mapValue;
mapKey: listDeclaration| mapDeclaration | identifier;
mapValue: listDeclaration | mapDeclaration | value;

// Flow control
ifDeclaration : AtIf ws expression ws block ws elseIfStatement* ws elseStatement? ws;
elseIfStatement: AtElse ws If ws expression ws block ws;
elseStatement: AtElse ws block ws;
forDeclaration: AtFor ws variableName ws From ws Number ws (To|Through) ws through ws block ws;
through: Number ws| functionCall;
whileDeclaration: AtWhile ws expression block;
eachDeclaration: AtEach ws variableName ws (Comma ws variableName ws)* In ws eachValueList ws block;
eachValueList: listDeclaration| mapDeclaration;

// Embeded functions
var_: Var ws Variable ws Rparen ws;
calc: Calc ws expression Rparen ws;
rotate: Rotate ws degree Rparen ws;
rgba  : Rgba ws (number Comma? ws)* Rparen ws;

// Primitives
unit: plusMinus? (length|dimension|percentage|degree) ws;
length: Number (AbsLength|FontRelative|ViewportRelative);
dimension: Number (Time| Freq| Resolution| Angle);
percentage: Number Percentage;
degree: Number Angle;

uri : Uri ws (String_ ws | Ident ws)+ Rparen ws;
arglist: Dot Dot Dot;
plusMinus: Plus | Minus;
hexcolor: Hash color;
color: (Number | Ident)+;
boolean : True| False ;
number: plusMinus? Number;
identifier : Ident| Not| And| Or| From| To;
ws: ( Comment | Space )*;