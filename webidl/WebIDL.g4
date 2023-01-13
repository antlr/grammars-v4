/*
BSD License

Copyright (c) 2013,2015 Rainer Schuster
Copyright (c) 2021 ethanmdavidson
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
    Editor’s Draft, 3 May 2021
 */
grammar WebIDL;

/* Note: appended underscore to keywords for CPP compat:
    const, default, enum, namespace, null
    see github.com/antlr/grammars-v4/issues/2099 for more info
*/
// Note: Added "wrapper" rule webIDL with EOF token.

webIDL
	: definitions EOF
;


definitions
	: extendedAttributeList definition definitions
	| /* empty */
;

definition
	: callbackOrInterfaceOrMixin
	| namespace_
	| partial
	| dictionary
	| enum_
	| typedef_
	| includesStatement
;

argumentNameKeyword
	: 'async'
	| 'attribute'
	| 'callback'
	| 'const'
	| 'constructor'
	| 'deleter'
	| 'dictionary'
	| 'enum'
	| 'getter'
	| 'includes'
	| 'inherit'
	| 'interface'
	| 'iterable'
	| 'maplike'
	| 'mixin'
	| 'namespace'
	| 'partial'
	| 'readonly'
	| 'required'
	| 'setlike'
	| 'setter'
	| 'static'
	| 'stringifier'
	| 'typedef'
	| 'unrestricted'
;

callbackOrInterfaceOrMixin
	: 'callback' callbackRestOrInterface
	| 'interface' interfaceOrMixin
;

interfaceOrMixin
    : interfaceRest
    | mixinRest
;

interfaceRest
	: IDENTIFIER_WEBIDL inheritance '{' interfaceMembers '}' ';'
;

partial
	: 'partial' partialDefinition
;

partialDefinition
	: 'interface' partialInterfaceOrPartialMixin
	| partialDictionary
	| namespace_
;

partialInterfaceOrPartialMixin
    : partialInterfaceRest
    | mixinRest
;

partialInterfaceRest
	: IDENTIFIER_WEBIDL '{' partialInterfaceMembers '}' ';'
;

interfaceMembers
	: extendedAttributeList interfaceMember interfaceMembers
	| /* empty */
;

interfaceMember
    : partialInterfaceMember
    | constructor
;

partialInterfaceMembers
    : extendedAttributeList partialInterfaceMember partialInterfaceMembers
    | /* empty */
;

partialInterfaceMember
	: const_
	| operation
	| stringifier
	| staticMember
	| iterable
	| asyncIterable
	| readonlyMember
	| readWriteAttribute
	| readWriteMaplike
	| readWriteSetlike
	| inheritAttribute
;

inheritance
	: ':' IDENTIFIER_WEBIDL
	| /* empty */
;

mixinRest
    : 'mixin' IDENTIFIER_WEBIDL '{' mixinMembers '}' ';'
;

mixinMembers
    : extendedAttributeList mixinMember mixinMembers
    | /* empty */
;

mixinMember
    : const_
    | regularOperation
    | stringifier
    | optionalReadOnly attributeRest
;

includesStatement
	: IDENTIFIER_WEBIDL 'includes' IDENTIFIER_WEBIDL ';'
;

callbackRestOrInterface
	: callbackRest
	| 'interface' IDENTIFIER_WEBIDL '{' callbackInterfaceMembers '}' ';'
;

callbackInterfaceMembers
    : extendedAttributeList callbackInterfaceMember callbackInterfaceMembers
    | /* empty */
;

callbackInterfaceMember
    : const_
    | regularOperation
;

const_
	: 'const' constType IDENTIFIER_WEBIDL '=' constValue ';'
;

constValue
	: booleanLiteral
	| floatLiteral
	| INTEGER_WEBIDL
;

booleanLiteral
	: 'true'
	| 'false'
;

floatLiteral
	: DECIMAL_WEBIDL
	| '-Infinity'
	| 'Infinity'
	| 'NaN'
;

constType
	: primitiveType
	| IDENTIFIER_WEBIDL
;

readonlyMember
	: 'readonly' readonlyMemberRest
;

readonlyMemberRest
	: attributeRest
	| maplikeRest
	| setlikeRest
;

readWriteAttribute
	: attributeRest
;

inheritAttribute
    : 'inherit' attributeRest
;

attributeRest
	: 'attribute' typeWithExtendedAttributes attributeName ';'
;

attributeName
	: attributeNameKeyword
	| IDENTIFIER_WEBIDL
;

attributeNameKeyword
	: 'async'
	| 'required'
;

optionalReadOnly
    : 'readonly'
    | /* empty */
;

defaultValue
	: constValue
	| STRING_WEBIDL
	| '[' ']'
	| '{' '}'
	| 'null'
;

operation
	: regularOperation
	| specialOperation
;

regularOperation
    : type_ operationRest
;

specialOperation
	: special regularOperation
;

special
	: 'getter'
	| 'setter'
	| 'deleter'
;

operationRest
	: optionalOperationName '(' argumentList ')' ';'
;

optionalOperationName
	: operationName
	| /* empty */
;

operationName
    : operationNameKeyword
    | IDENTIFIER_WEBIDL
;

operationNameKeyword
    : 'includes'
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
	: extendedAttributeList argumentRest
;

argumentRest
    : 'optional' typeWithExtendedAttributes argumentName default_
    | type_ ellipsis argumentName
;

argumentName
	: argumentNameKeyword
	| IDENTIFIER_WEBIDL
;

ellipsis
	: '...'
	| /* empty */
;

constructor
    : 'constructor' '(' argumentList ')' ';'
;

stringifier
	: 'stringifier' stringifierRest
;

stringifierRest
	: optionalReadOnly attributeRest
	| regularOperation
	| ';'
;

staticMember
	: 'static' staticMemberRest
;

staticMemberRest
	: optionalReadOnly attributeRest
	| regularOperation
;

iterable
	: 'iterable' '<' typeWithExtendedAttributes optionalType '>' ';'
;

optionalType
	: ',' typeWithExtendedAttributes
	| /* empty */
;

asyncIterable
    : 'async' 'iterable' '<' typeWithExtendedAttributes optionalType '>' optionalArgumentList ';'
;

optionalArgumentList
    : '(' argumentList ')'
    | /* empty */
;

readWriteMaplike
	: maplikeRest
;

maplikeRest
	: 'maplike' '<' typeWithExtendedAttributes ',' typeWithExtendedAttributes '>' ';'
;

readWriteSetlike
	: setlikeRest
;

setlikeRest
	: 'setlike' '<' typeWithExtendedAttributes '>' ';'
;

namespace_
    : 'namespace' IDENTIFIER_WEBIDL '{' namespaceMembers '}' ';'
;

namespaceMembers
    : extendedAttributeList namespaceMember namespaceMembers
    | /* empty */
;

namespaceMember
    : regularOperation
    | 'readonly' attributeRest
    | const_
;

dictionary
	: 'dictionary' IDENTIFIER_WEBIDL inheritance '{' dictionaryMembers '}' ';'
;

dictionaryMembers
	: dictionaryMember dictionaryMembers
	| /* empty */
;

dictionaryMember
	: extendedAttributeList dictionaryMemberRest
;

dictionaryMemberRest
    : 'required' typeWithExtendedAttributes IDENTIFIER_WEBIDL ';'
    | type_ IDENTIFIER_WEBIDL default_ ';'
;

partialDictionary
	: 'dictionary' IDENTIFIER_WEBIDL '{' dictionaryMembers '}' ';'
;

default_
	: '=' defaultValue
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
	: IDENTIFIER_WEBIDL '=' type_ '(' argumentList ')' ';'
;

typedef_
	: 'typedef' typeWithExtendedAttributes IDENTIFIER_WEBIDL ';'
;

type_
	: singleType
	| unionType null_
;

typeWithExtendedAttributes
    : extendedAttributeList type_
;

singleType
	: distinguishableType
	| 'any'
	| promiseType
;

unionType
	: '(' unionMemberType 'or' unionMemberType unionMemberTypes ')'
;

unionMemberType
	: extendedAttributeList distinguishableType
	| unionType null_
;

unionMemberTypes
	: 'or' unionMemberType unionMemberTypes
	| /* empty */
;

distinguishableType
    : primitiveType null_
    | stringType null_
    | IDENTIFIER_WEBIDL null_
    | 'sequence' '<' typeWithExtendedAttributes '>' null_
    | 'object' null_
    | 'symbol' null_
    | bufferRelatedType null_
    | 'FrozenArray' '<' typeWithExtendedAttributes '>' null_
    | 'ObservableArray' '<' typeWithExtendedAttributes '>' null_
    | recordType null_
;

primitiveType
	: unsignedIntegerType
	| unrestrictedFloatType
	| 'undefined'
	| 'boolean'
	| 'byte'
	| 'octet'
	| 'bigint'
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

stringType
    : 'ByteString'
    | 'DOMString'
    | 'USVString'
;

promiseType
	: 'Promise' '<' type_ '>'
;

recordType
    : 'record' '<' stringType ',' typeWithExtendedAttributes '>'
;

null_
	: '?'
	| /* empty */
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

extendedAttributeList
	: '[' extendedAttribute extendedAttributes ']'
	| /* empty */
;

extendedAttributes
	: ',' extendedAttribute extendedAttributes
	| /* empty */
;

/* https://heycam.github.io/webidl/#idl-extended-attributes
   "The ExtendedAttribute grammar symbol matches nearly any sequence of tokens,
   however the extended attributes defined in this document only accept a more
   restricted syntax."
   I use the more restrictive syntax here because it will be more useful for
   parsing things in the real world.
*/
extendedAttribute
    : extendedAttributeNoArgs
    | extendedAttributeArgList
    | extendedAttributeNamedArgList
    | extendedAttributeIdent
    | extendedAttributeIdentList
    | extendedAttributeString
    | extendedAttributeStringList
;

/*
Here is the extendedAttribute grammar as defined in the spec
(https://heycam.github.io/webidl/#prod-ExtendedAttribute):

extendedAttribute
	: '(' extendedAttributeInner ')' extendedAttributeRest
	| '[' extendedAttributeInner ']' extendedAttributeRest
	| '{' extendedAttributeInner '}' extendedAttributeRest
	| other extendedAttributeRest
;

extendedAttributeRest
	: extendedAttribute
	| // empty
;

extendedAttributeInner
	: '(' extendedAttributeInner ')' extendedAttributeInner
	| '[' extendedAttributeInner ']' extendedAttributeInner
	| '{' extendedAttributeInner '}' extendedAttributeInner
	| otherOrComma extendedAttributeInner
	| // empty
*/

other
	: INTEGER_WEBIDL
	| DECIMAL_WEBIDL
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
	| 'ObservableArray'
	| 'Promise'
	| 'USVString'
	| 'any'
	| 'bigint'
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
	| 'record'
	| 'sequence'
	| 'short'
	| 'symbol'
	| 'true'
	| 'unsigned'
	| 'undefined'
	| argumentNameKeyword
	| bufferRelatedType
;

otherOrComma
	: other
	| ','
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

/* Chromium IDL also allows string literals in extendedAttributes
https://chromium.googlesource.com/chromium/src/+/refs/heads/main/third_party/blink/renderer/bindings/IDLExtendedAttributes.md
*/
extendedAttributeString
    : IDENTIFIER_WEBIDL '=' STRING_WEBIDL
;

extendedAttributeStringList
    : IDENTIFIER_WEBIDL '=' '(' stringList ')'
;

stringList
    : STRING_WEBIDL strings
;

strings
    : ',' STRING_WEBIDL strings
    | /* empty */
;

INTEGER_WEBIDL
	: '-'?('0'([Xx][0-9A-Fa-f]+|[0-7]*)|[1-9][0-9]*)
;

DECIMAL_WEBIDL
	: '-'?(([0-9]+'.'[0-9]*|[0-9]*'.'[0-9]+)([Ee][+\-]?[0-9]+)?|[0-9]+[Ee][+\-]?[0-9]+)
;

IDENTIFIER_WEBIDL
	: [_-]?[A-Z_a-z][0-9A-Z_a-z]*
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
