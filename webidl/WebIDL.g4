/*
BSD License

Copyright (c) 2013, Rainer Schuster
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Rainer Schuster nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

Web IDL grammar derived from:

    http://dev.w3.org/2006/webapi/WebIDL/

    Web IDL (Second Edition)
    W3C Editor's Draft 26 March 2013
 */
grammar WebIDL;

// Note: Replaced keywords: const, default, enum, interface, null.
// Note: Added "wrapper" rule webIDL with EOF token.

webIDL
 : definitions EOF
;


definitions
 : extendedAttributeList definition definitions 
 | /* empty */
;

definition
 : callbackOrInterface 
 | partial 
 | dictionary 
 | exception 
 | enum_ 
 | typedef 
 | implementsStatement
;

callbackOrInterface
 : 'callback' callbackRestOrInterface 
 | interface_
;

callbackRestOrInterface
 : callbackRest 
 | interface_
;

interface_
 : 'interface' IDENTIFIER_WEBIDL inheritance '{' interfaceMembers '}' ';'
;

partial
 : 'partial' partialDefinition
;

partialDefinition
 : partialInterface 
 | partialDictionary
;

partialInterface
 : 'interface' IDENTIFIER_WEBIDL '{' interfaceMembers '}' ';'
;

interfaceMembers
 : extendedAttributeList interfaceMember interfaceMembers 
 | /* empty */
;

interfaceMember
 : const_ 
 | attributeOrOperationOrIterator
;

dictionary
 : 'dictionary' IDENTIFIER_WEBIDL inheritance '{' dictionaryMembers '}' ';'
;

dictionaryMembers
 : extendedAttributeList dictionaryMember dictionaryMembers 
 | /* empty */
;

dictionaryMember
 : type IDENTIFIER_WEBIDL default_ ';'
;

partialDictionary
 : 'dictionary' IDENTIFIER_WEBIDL '{' dictionaryMembers '}' ';'
;

default_
 : '=' defaultValue 
 | /* empty */
;

defaultValue
 : constValue 
 | STRING_WEBIDL
;

exception
 : 'exception' IDENTIFIER_WEBIDL inheritance '{' exceptionMembers '}' ';'
;

exceptionMembers
 : extendedAttributeList exceptionMember exceptionMembers 
 | /* empty */
;

inheritance
 : ':' IDENTIFIER_WEBIDL 
 | /* empty */
;

enum_
 : 'enum' IDENTIFIER_WEBIDL '{' enumValueList '}' ';'
;

enumValueList
 : STRING_WEBIDL enumValues
;

enumValues
 : ',' STRING_WEBIDL enumValues 
 | /* empty */
;

callbackRest
 : IDENTIFIER_WEBIDL '=' returnType '(' argumentList ')' ';'
;

typedef
 : 'typedef' extendedAttributeList type IDENTIFIER_WEBIDL ';'
;

implementsStatement
 : IDENTIFIER_WEBIDL 'implements' IDENTIFIER_WEBIDL ';'
;

const_
 : 'const' constType IDENTIFIER_WEBIDL '=' constValue ';'
;

constValue
 : booleanLiteral 
 | floatLiteral 
 | INTEGER_WEBIDL 
 | 'null'
;

booleanLiteral
 : 'true' 
 | 'false'
;

floatLiteral
 : FLOAT_WEBIDL 
 | '-' 'Infinity' 
 | 'Infinity' 
 | 'NaN'
;

attributeOrOperationOrIterator
 : serializer 
 | qualifier attributeOrOperationRest 
 | attribute 
 | operationOrIterator
;

serializer
 : 'serializer' serializerRest
;

serializerRest
 : operationRest 
 | '=' serializationPattern 
 | /* empty */
;

serializationPattern
 : '{' serializationPatternMap '}' 
 | '[' serializationPatternList ']' 
 | IDENTIFIER_WEBIDL
;

serializationPatternMap
 : 'getter' 
 | 'inherit' identifiers 
 | IDENTIFIER_WEBIDL identifiers 
 | /* empty */
;

serializationPatternList
 : 'getter' 
 | IDENTIFIER_WEBIDL identifiers 
 | /* empty */
;

identifiers
 : ',' IDENTIFIER_WEBIDL identifiers 
 | /* empty */
;

qualifier
 : 'static' 
 | 'stringifier'
;

attributeOrOperationRest
 : attributeRest 
 | returnType operationRest 
 | ';'
;

attribute
 : inherit attributeRest
;

attributeRest
 : readOnly 'attribute' type IDENTIFIER_WEBIDL ';'
;

inherit
 : 'inherit' 
 | /* empty */
;

readOnly
 : 'readonly' 
 | /* empty */
;

operationOrIterator
 : returnType operationOrIteratorRest 
 | specialOperation
;

specialOperation
 : special specials returnType operationRest
;

specials
 : special specials 
 | /* empty */
;

special
 : 'getter' 
 | 'setter' 
 | 'creator' 
 | 'deleter' 
 | 'legacycaller'
;

operationOrIteratorRest
 : iteratorRest 
 | operationRest
;

iteratorRest
 : 'iterator' optionalIteratorInterfaceOrObject ';'
;

optionalIteratorInterfaceOrObject
 : optionalIteratorInterface 
 | 'object'
;

optionalIteratorInterface
 : '=' IDENTIFIER_WEBIDL 
 | /* empty */
;

operationRest
 : optionalIdentifier '(' argumentList ')' ';'
;

optionalIdentifier
 : IDENTIFIER_WEBIDL 
 | /* empty */
;

argumentList
 : argument arguments 
 | /* empty */
;

arguments
 : ',' argument arguments 
 | /* empty */
;

argument
 : extendedAttributeList optionalOrRequiredArgument
;

optionalOrRequiredArgument
 : 'optional' type argumentName default_ 
 | type ellipsis argumentName
;

argumentName
 : argumentNameKeyword 
 | IDENTIFIER_WEBIDL
;

ellipsis
 : '...' 
 | /* empty */
;

exceptionMember
 : const_ 
 | exceptionField
;

exceptionField
 : type IDENTIFIER_WEBIDL ';'
;

extendedAttributeList
 : '[' extendedAttribute extendedAttributes ']' 
 | /* empty */
;

extendedAttributes
 : ',' extendedAttribute extendedAttributes 
 | /* empty */
;

extendedAttribute
 :  '(' extendedAttributeInner ')' extendedAttributeRest 
 | '[' extendedAttributeInner ']' extendedAttributeRest 
 | '{' extendedAttributeInner '}' extendedAttributeRest 
 | other extendedAttributeRest
;

extendedAttributeRest
 : extendedAttribute 
 | /* empty */
;

extendedAttributeInner
 :  '(' extendedAttributeInner ')' extendedAttributeInner 
 | '[' extendedAttributeInner ']' extendedAttributeInner 
 | '{' extendedAttributeInner '}' extendedAttributeInner 
 | otherOrComma extendedAttributeInner 
 | /* empty */
;

other
 :  INTEGER_WEBIDL 
 | FLOAT_WEBIDL 
 | IDENTIFIER_WEBIDL 
 | STRING_WEBIDL 
 | OTHER_WEBIDL 
 | '-' 
 | '.' 
 | '...' 
 | ':' 
 | ';' 
 | '<' 
 | '=' 
 | '>' 
 | '?' 
 | 'ByteString' 
 | 'Date' 
 | 'DOMString' 
 | 'Infinity' 
 | 'NaN' 
 | 'RegExp' 
 | 'any' 
 | 'boolean' 
 | 'byte' 
 | 'double' 
 | 'false' 
 | 'float' 
 | 'long' 
 | 'null' 
 | 'object' 
 | 'octet' 
 | 'or' 
 | 'optional' 
 | 'sequence' 
 | 'short' 
 | 'true' 
 | 'unsigned' 
 | 'void' 
 | argumentNameKeyword
;

argumentNameKeyword
 :  'attribute' 
 | 'callback' 
 | 'const' 
 | 'creator' 
 | 'deleter' 
 | 'dictionary' 
 | 'enum' 
 | 'exception' 
 | 'getter' 
 | 'implements' 
 | 'inherit' 
 | 'interface' 
 | 'legacycaller' 
 | 'partial' 
 | 'serializer' 
 | 'setter' 
 | 'static' 
 | 'stringifier' 
 | 'typedef' 
 | 'unrestricted'
;

otherOrComma
 : other
 | ','
;

type
 : singleType 
 | unionType typeSuffix
;

singleType
 : nonAnyType 
 | 'any' typeSuffixStartingWithArray
;

unionType
 : '(' unionMemberType 'or' unionMemberType unionMemberTypes ')'
;

unionMemberType
 : nonAnyType 
 | unionType typeSuffix 
 | 'any' '[' ']' typeSuffix
;

unionMemberTypes
 : 'or' unionMemberType unionMemberTypes 
 | /* empty */
;

nonAnyType
 : primitiveType typeSuffix 
 | 'ByteString' typeSuffix 
 | 'DOMString' typeSuffix 
 | IDENTIFIER_WEBIDL typeSuffix 
 | 'sequence' '<' type '>' null_ 
 | 'object' typeSuffix 
 | 'Date' typeSuffix 
 | 'RegExp' typeSuffix
;

constType
 : primitiveType null_ 
 | IDENTIFIER_WEBIDL null_
;

primitiveType
 : unsignedIntegerType 
 | unrestrictedFloatType 
 | 'boolean' 
 | 'byte' 
 | 'octet'
;

unrestrictedFloatType
 : 'unrestricted' floatType 
 | floatType
;

floatType
 : 'FLOAT_WEBIDL' 
 | 'double'
;

unsignedIntegerType
 : 'unsigned' integerType 
 | integerType
;

integerType
 : 'short' 
 | 'long' optionalLong
;

optionalLong
 : 'long' 
 | /* empty */
;

typeSuffix
 : '[' ']' typeSuffix 
 | '?' typeSuffixStartingWithArray 
 | /* empty */
;

typeSuffixStartingWithArray
 : '[' ']' typeSuffix 
 | /* empty */
;

null_
 : '?' 
 | /* empty */
;

returnType
 : type 
 | 'void'
;

extendedAttributeNoArgs
 : IDENTIFIER_WEBIDL
;

extendedAttributeArgList
 : IDENTIFIER_WEBIDL '(' argumentList ')'
;

extendedAttributeIdent
 : IDENTIFIER_WEBIDL '=' IDENTIFIER_WEBIDL
;

extendedAttributeNamedArgList
 : IDENTIFIER_WEBIDL '=' IDENTIFIER_WEBIDL '(' argumentList ')'
;


INTEGER_WEBIDL
 : '-'?('0'([Xx][0-9A-Fa-f]+|[0-7]*)|[1-9][0-9]*)
;

FLOAT_WEBIDL
 : '-'?(([0-9]+'.'[0-9]*|[0-9]*'.'[0-9]+)([Ee][\+\-]?[0-9]+)?|[0-9]+[Ee][\+\-]?[0-9]+)
;

IDENTIFIER_WEBIDL
 : [A-Z_a-z][0-9A-Z_a-z]*
;

STRING_WEBIDL
 : '"'~['"']*'"'
;

WHITESPACE_WEBIDL
 : [\t\n\r ]+ -> skip
;

COMMENT_WEBIDL
 : ('//'~[\n\r]*|'/*'(.|'\n')*?'*/')+ -> skip
; // Note: '/''/'~[\n\r]* instead of '/''/'.* (non-greedy because of wildcard).

OTHER_WEBIDL
 : ~[\t\n\r 0-9A-Z_a-z]
;
