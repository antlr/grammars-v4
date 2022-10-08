/*
 [The "BSD licence"]
 Copyright (c) 2014 Vlad Shlosberg
 Copyright (c) 2022 Sergei Russkikh
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
    : statement* EOF;

statement
    : importDeclaration
    | variableDeclaration
    | propertyDeclaration
    | interpolationDeclaration
    | mediaDeclaration
    | mixinDeclaration
    | contentDeclaration
    | functionDeclaration
    | ifDeclaration
    | forDeclaration
    | whileDeclaration
    | eachDeclaration
    | fontFaceDeclaration
    | keyframesDeclaration
    | includeDeclaration
    | extendDeclaration
    | warndingDeclaration
    | errorDeclaration
    | ruleset
    | atStatementDeclaration
    | percentageStatement
    ;

// Import
importDeclaration
    : Import importPath Semi?
    | Require importPath Semi?
    | Use importPath asClause? withClause? Semi?
    | Forward importPath
    ;

importPath
    : uri
    | String_
    ;

asClause
    : As (Times | identifier)
    ;

withClause
    : With Lparen parameters Rparen;

// Declarations
variableDeclaration
    : variableName Colon variableValue prio? Semi?
    ;

variableValue
    : value
    | functionDeclaration
    | functionCall
    | mapDeclaration+
    | listDeclaration+
    | variableName
    | identifier
    ;

variableName
    : ( ( Minus Minus ) Dollar | plusMinus Dollar | Dollar) identifier
    | plusMinus? namespace_? Dollar ( identifier | measurment )
    | Variable
    ;

namespace_
    : (identifier Dot)+
    ;

propertyDeclaration
    : identifier Colon propertyValue Semi?
    ;

prio
    : Important | Default
    ;

propertyValue
    : ( value
        | value? prio? block
        | variableName
        | listSpaceSeparated
        | listCommaSeparated
        | expression
        | functionCall
      ) prio?;

mediaDeclaration
    : Media mediaQueryList block
    ;

mediaQueryList
    : ( mediaQuery ( Comma mediaQuery )* )?
    ;

mediaQuery
    : ( Only | Not )? ( identifier | value ) ( And mediaExpression )*
    | mediaExpression ( And mediaExpression )*
    ;

mediaExpression
    : Lparen identifier ( Colon value )? Rparen
    ;

mixinDeclaration
    : Mixin ( identifier| identifier Lparen parameters Rparen ) block
    ;

contentDeclaration
    : Content (Lparen parameters Rparen)? Semi?
    ;

fontFaceDeclaration
    : FontFace BlockStart statement* BlockEnd
    ;

keyframesDeclaration
    : Keyframes identifier? keyframesBlock
    ;

keyframesBlock
    : BlockStart percentageStatement* BlockEnd
    | block
    ;

percentageStatement
    : percentage block
    ;

includeDeclaration
    : Include namespace_? (identifier | functionCall)
    ( Semi | Using Lparen parameters Rparen )? block?
    ;

interpolationDeclaration
    : interpolation Colon propertyValue Semi?
    ;

extendDeclaration
    : Extend ( Percentage | parentRef )?
    ( id | typeSelector | universal | className | attrib | pseudo | interpolation | parentRef )+ Semi?
    ;

warndingDeclaration
    : Warn String_ Semi
    ;

errorDeclaration
    : Error String_ Semi
    ;

atStatementDeclaration
    : At ( identifier Lparen parameters Rparen | identifier ) block
    ;

// Structure
ruleset
    : selectorGroup block
    ;

block
    : BlockStart statement* functionReturn? BlockEnd
    ;

// Selectors
selectorGroup
    : selector ( Comma selector )*
    ;

selector
    : combinator? selectorSequence ( combinator selectorSequence )*
    ;

combinator
    : Plus
    | Greater
    | Tilde
    | Space
    ;

selectorSequence
    : ( typeSelector | universal ) ( id | className | attrib | pseudo | negation | interpolation ( variableName | Percentage )? | parentRef )*
    | ( typeSelector| id | className | attrib | pseudo | negation | interpolation ( variableName | Percentage )? | parentRef )+
    ;

id
    : Hash identifier
    ;

typeSelector
    : typeNamespacePrefix? ( Percentage | parentRef )? ( identifier | variableName )
    ;

typeNamespacePrefix
    : ( identifier | Times )? Pipe
    ;

universal
    : typeNamespacePrefix? Times
    ;

className
    : Dot ( Minus | identifier | interpolation )+
    ;

interpolation
    : namespace_? Hash BlockStart namespace_? ( ifExpression | value | parentRef ) BlockEnd measurment?
    ;

parentRef
    : Amp
    ;

attrib
    : Lbrack typeNamespacePrefix? identifier
    ( ( PrefixMatch | SuffixMatch | SubstringMatch | Eq | Includes | DashMatch )
    ( identifier | String_ ) )? Rbrack
    ;

pseudo
    : Colon Colon? ( interpolation | identifier | functionalPseudo )
    ;

functionalPseudo
    : Ident Lparen pseudoParameter+ Rparen
    ;

pseudoParameter
    : ( ( value | className | interpolation ) Comma?)
    ;

negation
    : PseudoNot negationArg Rparen
    ;

negationArg
    : typeSelector
    | universal
    | Hash
    | className
    | attrib
    | pseudo
    ;

// Operators
operator_
    : Div
    | Times
    | Minus
    | Plus
    | Greater
    | Less
    | Greater Eq
    | Less Eq
    | Eq Eq?
    | NotEq
    | And
    | Or
    | Not
    ;

value
    : unit
    | number
    | boolean
    | calc
    | rotate
    | rgba
    | var_
    | uri
    | repeat
    | Format
    | String_
    | functionCall
    | variableName
    | interpolation
    | hexcolor
    | identifier
    | expression
    | block
    | Lparen Rparen
    | measurment
    ;

// Function
functionDeclaration
    : Function ( namespace_? identifier )?
    Lparen parameters Rparen BlockStart functionBody? BlockEnd
    ;

parameters
    : parameter? (Comma parameter)*
    ;

parameter
    : ( value | variableDeclaration | listSpaceSeparated | mapDeclaration ) arglist? prio?
    ;

functionBody
    : functionStatement* functionReturn?
    ;

functionReturn
    : Return expression (Comma expression)* Semi?
    ;

functionStatement
    : expression Semi
    | statement
    ;

functionCall
    : namespace_? identifier Lparen parameters Rparen
    ;

expression
    : Not? expressionPart (operator_ Not? expressionPart )*
    ;

expressionPart
    : unit
    | identifier
    | variableName
    | var_
    | boolean
    | calc
    | rotate
    | rgba
    | number
    | uri
    | Format
    | String_
    | interpolation
    | hexcolor
    | ifExpression
    | functionCall
    | plusMinus? Lparen expression? Rparen
    | prio
    | measurment
    ;

ifExpression
    : If Lparen ( expression | parentRef ) Comma value Comma value Rparen measurment? prio?
    ;


// List & Map
listDeclaration
    : ( listBracketed
        | listCommaSeparated
        | listSpaceSeparated
      )
    | Lparen listDeclaration Rparen;

listCommaSeparated
    : listElement (Comma listElement)* Comma?
    ;

listSpaceSeparated
    : listElement+
    ;

listBracketed
    : Lbrack ( listSpaceSeparated | listCommaSeparated ) Rbrack
    ;

listElement
    : Lparen? (value Comma?)+ Rparen? Comma?
    ;

mapDeclaration
    : Lparen mapEntries Rparen
    ;

mapEntries
    : mapEntry (Comma mapEntry)* Comma?
    ;

mapEntry
    : mapKey Colon mapValue
    ;

mapKey
    : value
    | listDeclaration
    | mapDeclaration
    ;

mapValue
    : value
    | listDeclaration
    | mapDeclaration
    ;

// Flow control
ifDeclaration
    : AtIf expression block elseIfStatement* elseStatement?
    ;

elseIfStatement
    : AtElse If expression block
    ;

elseStatement
    : AtElse block
    ;

forDeclaration
    : AtFor variableName From Number ( To | Through ) through block
    ;

through
    : Number
    | functionCall
    | expression
    ;

whileDeclaration
    : AtWhile expression block
    ;

eachDeclaration
    : AtEach variableName (Comma variableName)* In eachValueList block
    ;

eachValueList
    : listDeclaration
    | mapDeclaration
    ;

// Embeded functions
var_
    : Var Variable (Comma value)? Rparen
    ;

calc
    : Calc expression Rparen
    ;

rotate
    : Rotate degree Rparen
    ;

rgba
    : Rgba value (Comma? value)* Rparen
    ;

repeat
    : Repeat value Comma number Freq Rparen
    ;

// Primitives
unit
    : ( length | dimension | percentage | degree )
    ;

length
    : plusMinus? Number ( AbsLength | FontRelative | ViewportRelative )
    ;

dimension
    : plusMinus? Number ( Time | Freq | Resolution | Angle)
    ;

percentage
    : plusMinus? Number Percentage
    ;

degree
    : plusMinus? Number Angle
    ;

measurment
    : AbsLength
    | FontRelative
    | ViewportRelative
    | Time
    | Freq
    | Resolution
    | Angle
    | Percentage
    ;

uri
    : Uri
    ;

arglist
    : Dot Dot Dot
    ;

plusMinus
    : Plus
    | Minus
    ;

hexcolor
    : Hash color
    ;

color
    : ( Number | Ident )+
    ;

boolean
    : True
    | False
    ;

number
    : plusMinus? Number
    ;

identifier
    : ( VendorPrefix  | Minus )? Ident
    | From
    | To
    ;