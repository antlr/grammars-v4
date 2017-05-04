/*
BSD License

Copyright (c) 2013,2015 Rainer Schuster
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

    http://heycam.github.io/webidl/

    Web IDL (Second Edition)
    W3C Editor's Draft 17 November 2015
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
	| enum_
	| typedef
	| implementsStatement
;

callbackOrInterface
	: 'callback' callbackRestOrInterface
	| interface_
	| class_
;

callbackRestOrInterface
	: callbackRest
	| interface_
;

interface_
	: 'interface' IDENTIFIER_WEBIDL inheritance '{' interfaceMembers '}' ';'
;

class_
    : 'class' IDENTIFIER_WEBIDL extension '{' interfaceMembers '}' ';'
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
	| operation
	| serializer
	| stringifier
	| staticMember
	| iterable
	| readonlyMember
	| readWriteAttribute
	| readWriteMaplike
	| readWriteSetlike
;

dictionary
	: 'dictionary' IDENTIFIER_WEBIDL inheritance '{' dictionaryMembers '}' ';'
;

dictionaryMembers
	: extendedAttributeList dictionaryMember dictionaryMembers
	| /* empty */
;

dictionaryMember
	: required type IDENTIFIER_WEBIDL default_ ';'
;

required
	: 'required'
	| /* empty */
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
	| '[' ']'
;

inheritance
	: ':' IDENTIFIER_WEBIDL
	| /* empty */
;

extension
    : 'extends' IDENTIFIER_WEBIDL
    | /* empty */
;

enum_
	: 'enum' IDENTIFIER_WEBIDL '{' enumValueList '}' ';'
;

enumValueList
	: STRING_WEBIDL enumValueListComma
;

enumValueListComma
	: ',' enumValueListString
	| /* empty */
;

enumValueListString
	: STRING_WEBIDL enumValueListComma
	| /* empty */
;

callbackRest
	: IDENTIFIER_WEBIDL '=' returnType '(' argumentList ')' ';'
;

typedef
	: 'typedef' type IDENTIFIER_WEBIDL ';'
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
	| '-Infinity'
	| 'Infinity'
	| 'NaN'
;

serializer
	: 'serializer' serializerRest
;

serializerRest
	: operationRest
	| '=' serializationPattern ';'
	| ';'
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

stringifier
	: 'stringifier' stringifierRest
;

stringifierRest
	: readOnly attributeRest
	| returnType operationRest
	| ';'
;

staticMember
	: 'static' staticMemberRest
;

staticMemberRest
	: readOnly attributeRest
	| returnType operationRest
;

readonlyMember
	: 'readonly' readonlyMemberRest
;

readonlyMemberRest
	: attributeRest
	| readWriteMaplike
	| readWriteSetlike
;

readWriteAttribute
	: 'inherit' readOnly attributeRest
	| attributeRest
;

attributeRest
	: 'attribute' type attributeName ';'
;

attributeName
	: attributeNameKeyword
	| IDENTIFIER_WEBIDL
;

attributeNameKeyword
	: 'required'
;

inherit
	: 'inherit'
	| /* empty */
;

readOnly
	: 'readonly'
	| /* empty */
;

operation
	: returnType operationRest
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
	| 'deleter'
	| 'legacycaller'
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

iterable
	: 'iterable' '<' type optionalType '>' ';'
;

optionalType
	: ',' type
	| /* empty */
;

readWriteMaplike
	: maplikeRest
;

readWriteSetlike
	: setlikeRest
;

maplikeRest
	: 'maplike' '<' type ',' type '>' ';'
;

setlikeRest
	: 'setlike' '<' type '>' ';'
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
	: '(' extendedAttributeInner ')' extendedAttributeRest
	| '[' extendedAttributeInner ']' extendedAttributeRest
	| '{' extendedAttributeInner '}' extendedAttributeRest
	| other extendedAttributeRest
;

extendedAttributeRest
	: extendedAttribute
	| /* empty */
;

extendedAttributeInner
	: '(' extendedAttributeInner ')' extendedAttributeInner
	| '[' extendedAttributeInner ']' extendedAttributeInner
	| '{' extendedAttributeInner '}' extendedAttributeInner
	| otherOrComma extendedAttributeInner
	| /* empty */
;

other
	: INTEGER_WEBIDL
	| FLOAT_WEBIDL
	| IDENTIFIER_WEBIDL
	| STRING_WEBIDL
	| OTHER_WEBIDL
	| '-'
	| '-Infinity'
	| '.'
	| '...'
	| ':'
	| ';'
	| '<'
	| '='
	| '>'
	| '?'
	| 'ByteString'
	| 'DOMString'
	| 'FrozenArray'
	| 'Infinity'
	| 'NaN'
	| 'RegExp'
	| 'USVString'
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
	| bufferRelatedType
;

argumentNameKeyword
	: 'attribute'
	| 'callback'
	| 'const'
	| 'deleter'
	| 'dictionary'
	| 'enum'
	| 'getter'
	| 'implements'
	| 'inherit'
	| 'interface'
	| 'iterable'
	| 'legacycaller'
	| 'maplike'
	| 'partial'
	| 'required'
	| 'serializer'
	| 'setlike'
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
	| unionType null_
;

singleType
	: nonAnyType
	| 'any'
;

unionType
	: '(' unionMemberType 'or' unionMemberType unionMemberTypes ')'
;

unionMemberType
	: nonAnyType
	| unionType null_
;

unionMemberTypes
	: 'or' unionMemberType unionMemberTypes
	| /* empty */
;

nonAnyType
	: primitiveType  null_
	| promiseType null_
	| 'ByteString'  null_
	| 'DOMString'  null_
	| 'USVString'  null_
	| IDENTIFIER_WEBIDL  null_
	| 'sequence' '<' type '>' null_
	| 'object'  null_
	| 'RegExp'  null_
	| 'DOMException'  null_
	| bufferRelatedType  null_
	| 'FrozenArray' '<' type '>' null_
;

bufferRelatedType
	: 'ArrayBuffer'
	| 'DataView'
	| 'Int8Array'
	| 'Int16Array'
	| 'Int32Array'
	| 'Uint8Array'
	| 'Uint16Array'
	| 'Uint32Array'
	| 'Uint8ClampedArray'
	| 'Float32Array'
	| 'Float64Array'
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
	: 'float'
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

promiseType
	: 'Promise' '<' returnType '>'
;

null_
	: '?'
	| /* empty */
;

returnType
	: type
	| 'void'
;

identifierList
	: IDENTIFIER_WEBIDL identifiers
;

identifiers
	: ',' IDENTIFIER_WEBIDL identifiers
	| /* empty */
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

extendedAttributeIdentList
	: IDENTIFIER_WEBIDL '=' '(' identifierList ')'
;

extendedAttributeNamedArgList
	: IDENTIFIER_WEBIDL '=' IDENTIFIER_WEBIDL '(' argumentList ')'
;


INTEGER_WEBIDL
	: '-'?('0'([Xx][0-9A-Fa-f]+|[0-7]*)|[1-9][0-9]*)
;

FLOAT_WEBIDL
	: '-'?(([0-9]+'.'[0-9]*|[0-9]*'.'[0-9]+)([Ee][+\-]?[0-9]+)?|[0-9]+[Ee][+\-]?[0-9]+)
;

IDENTIFIER_WEBIDL
	: [A-Z_a-z][0-9A-Z_a-z]*
;

STRING_WEBIDL
	: '"' ~["]* '"'
;

WHITESPACE_WEBIDL
	: [\t\n\r ]+ -> channel(HIDDEN)
;

COMMENT_WEBIDL
	: ('//'~[\n\r]*|'/*'(.|'\n')*?'*/')+ -> channel(HIDDEN)
; // Note: '/''/'~[\n\r]* instead of '/''/'.* (non-greedy because of wildcard).

OTHER_WEBIDL
	: ~[\t\n\r 0-9A-Z_a-z]
;
