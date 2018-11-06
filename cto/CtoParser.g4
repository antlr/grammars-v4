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

parser grammar CtoParser;

options { tokenVocab=CtoLexer; }

modelUnit
    : namespaceDeclaration? importDeclaration* typeDeclaration* EOF
    ;

namespaceDeclaration
    : NAMESPACE qualifiedName
    ;

importDeclaration
    : IMPORT qualifiedName (DOT MUL)?
    ;

typeDeclaration
    : (assetDeclaration
    | conceptDeclaration  
    | enumDeclaration
    | participantDeclaration
    | transactionDeclaration
    | eventDeclaration)
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
    : ENUM IDENTIFIER LBRACE enumConstant* RBRACE;

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

extendsOrIdentified: ((EXTENDS IDENTIFIER) | identified);

identified: (IDENTIFIED IDENTIFIER);

classBody
    : LBRACE classBodyDeclaration* RBRACE;

classBodyDeclaration
    : ';'
    | fieldDeclaration
    ;

fieldDeclaration
    : stringFieldType identifier defaultLiteral? regexDeclaration? OPTIONAL?
    | numericFieldType identifier defaultLiteral? rangeValidation? OPTIONAL?
    | otherFieldType identifier defaultOptional
    | identifierFieldType identifier
    | refType identifier;

identifierFieldType
    : VAR IDENTIFIER ('[' ']')*;

numericFieldType
    : VAR numericPrimitive ('[' ']')*;

numericPrimitive
    : DOUBLE
    | INTEGER
    | LONG
    ;

otherFieldType
    : VAR otherPrimitive ('[' ']')*;

otherPrimitive
    : BOOLEAN
    | DATE_TIME
    ;

regexDeclaration
    : REGEX ASSIGN REGEX_EXPR;

stringFieldType
    : VAR STRING ('[' ']')*;

refType
    : REF IDENTIFIER ('[' ']')*;

qualifiedName
    : IDENTIFIER ('.' IDENTIFIER)*;

rangeValidation
    : RANGE ASSIGN rangeDeclaration;

rangeDeclaration
    : ('[' numberLiteral ',' ']')
    | ('[' ',' numberLiteral ']')
    | ('[' numberLiteral ',' numberLiteral ']');

defaultOptional
    : defaultLiteral? OPTIONAL?;

defaultLiteral
    : (DEFAULT ASSIGN literal);

identifier: IDENTIFIER | ASSET | PARTICIPANT;

literal
    : numberLiteral
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOL_LITERAL
    ;

numberLiteral
    : integerLiteral
    | floatLiteral;

integerLiteral
    : DECIMAL_LITERAL
    | OCT_LITERAL
    ;

floatLiteral: FLOAT_LITERAL;

decorator
    : AT qualifiedName ('(' elementValuePair ')')?;

elementValuePair
    : literal (',' literal)*;

