/*
 [The "BSD licence"]
 Copyright (c) 2018 Mario Schroeder
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

/*
 A grammar for Hyperledger Composer Modeling Language
 https://hyperledger.github.io/composer/v0.19/reference/cto_language.html
 */

parser grammar CtoParser;

options { tokenVocab=CtoLexer; }

modelUnit
    : namespaceDeclaration importDeclaration* typeDeclaration* EOF
    ;

namespaceDeclaration
    : NAMESPACE qualifiedName
    ;

importDeclaration
    : IMPORT qualifiedName ('.' '*')?
    ;

typeDeclaration
    : assetDeclaration
    | conceptDeclaration  
    | enumDeclaration
    | participantDeclaration
    | transactionDeclaration
    | eventDeclaration
    ;

classModifier
    : decorator
    | ABSTRACT
    ;

assetDeclaration
    : classModifier*
      ASSET IDENTIFIER
      extendsOrIdentified
      classBody
    ;

conceptDeclaration
    : classModifier*
      CONCEPT IDENTIFIER
      (EXTENDS IDENTIFIER)?
      classBody
    ;

enumDeclaration
    : ENUM IDENTIFIER '{' enumConstant* '}';

enumConstant
    : VAR IDENTIFIER;

eventDeclaration
    : EVENT IDENTIFIER
      classBody
    ;

participantDeclaration
    : classModifier*
      PARTICIPANT IDENTIFIER
      extendsOrIdentified
      classBody
    ;

transactionDeclaration
    : classModifier*
      TRANSACTION IDENTIFIER
      classBody
    ;

extendsOrIdentified: EXTENDS IDENTIFIER | identified;

identified: IDENTIFIED IDENTIFIER;

classBody
    : '{' classBodyDeclaration* '}';

classBodyDeclaration
    : ';'
    | fieldDeclaration
    ;

fieldDeclaration
    : stringField identifier defaultString? regexDeclaration? OPTIONAL?
    | booleanField identifier defaultBoolean? OPTIONAL?
    | numericField identifier defaultNumber? rangeValidation? OPTIONAL?
    | dateField identifier defaultDate? OPTIONAL?
    | identifierField identifier
    | reference identifier;

identifierField
    : VAR IDENTIFIER square*;

numericField
    : VAR numericPrimitive square*;

numericPrimitive
    : DOUBLE
    | INTEGER
    | LONG
    ;

booleanField
    : VAR BOOLEAN square*;

dateField
    : VAR DATE_TIME square*;

defaultDate
    : DEFAULT ASSIGN DATE_TIME_LITERAL;

regexDeclaration
    : REGEX ASSIGN REGEX_EXPR;

stringField
    : VAR STRING square*;

reference
    : REF IDENTIFIER square*;

qualifiedName
    : IDENTIFIER ('.' IDENTIFIER)*;

rangeValidation
    : RANGE ASSIGN rangeDeclaration;

rangeDeclaration
    : '[' numberLiteral ',' ']'
    | '[' ',' numberLiteral ']'
    | '[' numberLiteral ',' numberLiteral ']';

defaultBoolean
    : DEFAULT ASSIGN BOOL_LITERAL;

defaultString
    : DEFAULT ASSIGN stringLiteral;

defaultNumber
    : DEFAULT ASSIGN numberLiteral;

identifier: IDENTIFIER | ASSET | PARTICIPANT;

literal
    : numberLiteral
    | stringLiteral
    | BOOL_LITERAL
    ;

numberLiteral
    : integerLiteral
    | floatLiteral;

stringLiteral
    : CHAR_LITERAL
    | STRING_LITERAL
    ;

integerLiteral
    : DECIMAL_LITERAL
    | OCT_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL;

decorator
    : AT qualifiedName ('(' elementValuePair ')')?;

elementValuePair
    : literal (',' literal)*;

square: '[' ']';