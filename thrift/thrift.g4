/*
 *  Copyright 2008 Martin Traverso
 *  Copyright 2012 Facebook, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

grammar thrift;

options {
    output = AST;
}

tokens {
    DOCUMENT;
    INCLUDE;
    TYPEDEF;
    NAMESPACE;
    DEFAULT_NAMESPACE;
    CPP_INCLUDE;
    ENUM;
    SENUM;
    STRUCT;
    UNION;
    EXCEPTION;
    FIELD;
    MAP;
    ENTRY;
    SET;
    LIST;
    REQUIREDNESS;
    REQUIRED;
    OPTIONAL;
    SERVICE;
    EXTENDS;
    METHOD;
    ONEWAY;
    THROWS;
    ARGS;
    CONST;
    TYPES;
    TYPE;
    CPP_TYPE;
    VOID;
}

@header {
    package com.facebook.swift.parser.antlr;
}

@lexer::header {
    package com.facebook.swift.parser.antlr;
}


document
    : header* definition* EOF -> ^(DOCUMENT header* definition*)
    ;


header
    : include | namespace | cpp_include
    ;

include
    : 'include' LITERAL -> ^(INCLUDE LITERAL)
    ;

namespace
    : 'namespace' '*' (v=IDENTIFIER | v=LITERAL) -> ^(DEFAULT_NAMESPACE $v)
    | 'namespace' k=IDENTIFIER (v=IDENTIFIER | v=LITERAL) -> ^(NAMESPACE $k $v)
    | 'cpp_namespace' IDENTIFIER -> ^(NAMESPACE IDENTIFIER["cpp"] IDENTIFIER)
    | 'php_namespace' IDENTIFIER -> ^(NAMESPACE IDENTIFIER["php"] IDENTIFIER)
    ;

cpp_include
    : 'cpp_include' LITERAL -> ^(CPP_INCLUDE LITERAL)
    ;


definition
    : const_rule | typedef | enum_rule | senum | struct | union | exception | service
    ;

const_rule
    : 'const' field_type IDENTIFIER '=' const_value list_separator?
        -> ^(CONST IDENTIFIER field_type const_value)
    ;

typedef
    : 'typedef' field_type IDENTIFIER type_annotations? -> ^(TYPEDEF IDENTIFIER field_type)
    ;

enum_rule
    : 'enum' IDENTIFIER '{' enum_field* '}' type_annotations? -> ^(ENUM IDENTIFIER enum_field*)
    ;

enum_field
    : IDENTIFIER ('=' integer)? type_annotations? list_separator? -> ^(IDENTIFIER integer?)
    ;

senum
    : 'senum' IDENTIFIER '{' (LITERAL list_separator?)* '}' type_annotations? -> ^(SENUM IDENTIFIER LITERAL*)
    ;

struct
    : 'struct' IDENTIFIER '{' field* '}' type_annotations? -> ^(STRUCT IDENTIFIER field* type_annotations?)
    ;

union
    : 'union' IDENTIFIER '{' field* '}' type_annotations? -> ^(UNION IDENTIFIER field* type_annotations?)
    ;

exception
    : 'exception' IDENTIFIER '{' field* '}' type_annotations? -> ^(EXCEPTION IDENTIFIER field* type_annotations?)
    ;

service
    : 'service' s=IDENTIFIER ('extends' e=IDENTIFIER)? '{' f=function* '}' type_annotations? -> ^(SERVICE $s ^(EXTENDS $e?) function* type_annotations?)
    ;


field
    : field_id? field_req? field_type IDENTIFIER ('=' const_value)? type_annotations? list_separator?
        -> ^(FIELD IDENTIFIER field_type field_id? ^(REQUIREDNESS field_req?) const_value? type_annotations?)
    ;

field_id
    : integer ':' -> integer
    ;

field_req
    : 'required' -> REQUIRED
    | 'optional' -> OPTIONAL
    ;


function
    : oneway? function_type IDENTIFIER '(' field* ')' throws_list? type_annotations? list_separator?
        -> ^(METHOD IDENTIFIER function_type ^(ARGS field*) oneway? throws_list? type_annotations?)
    ;

oneway
    : ('oneway' | 'async') -> ONEWAY
    ;

function_type
    : field_type
    | 'void' -> VOID
    ;

throws_list
    : 'throws' '(' field* ')' -> ^(THROWS field*)
    ;


type_annotations
    : '(' type_annotation* ')' -> ^(TYPES type_annotation*)
    ;

type_annotation
    : IDENTIFIER ('=' annotation_value)? list_separator? -> ^(TYPE IDENTIFIER annotation_value?)
    ;

annotation_value
    : integer | LITERAL
    ;


field_type
    : base_type | IDENTIFIER | container_type
    ;

base_type
    : real_base_type type_annotations?
    ;

container_type
    : (map_type | set_type | list_type) type_annotations?
    ;

map_type
    : 'map' cpp_type? '<' field_type COMMA field_type '>' -> ^(MAP field_type field_type cpp_type?)
    ;

set_type
    : 'set' cpp_type? '<' field_type '>' -> ^(SET field_type cpp_type?)
    ;

list_type
    : 'list' '<' field_type '>' cpp_type? -> ^(LIST field_type cpp_type?)
    ;

cpp_type
    : 'cpp_type' LITERAL -> ^(CPP_TYPE LITERAL)
    ;


const_value
    : integer | DOUBLE | LITERAL | IDENTIFIER | const_list | const_map
    ;

integer
    : INTEGER | HEX_INTEGER
    ;

INTEGER
    : ('+' | '-')? DIGIT+
    ;

HEX_INTEGER
    : '0x' HEX_DIGIT+
    ;

DOUBLE
    : ('+' | '-')? DIGIT* ('.' DIGIT+)? (('E' | 'e') INTEGER)?
    ;

const_list
    : '[' (const_value list_separator?)* ']' -> ^(LIST const_value*)
    ;

const_map_entry
    : k=const_value ':' v=const_value list_separator? -> ^(ENTRY $k $v)
    ;

const_map
    : '{' const_map_entry* '}' -> ^(MAP const_map_entry*)
    ;

list_separator
    : COMMA | ';'
    ;

real_base_type
    :  TYPE_BOOL | TYPE_BYTE | TYPE_I16 | TYPE_I32 | TYPE_I64 | TYPE_DOUBLE | TYPE_STRING | TYPE_BINARY
    ;

TYPE_BOOL: 'bool';
TYPE_BYTE: 'byte';
TYPE_I16: 'i16';
TYPE_I32: 'i32';
TYPE_I64: 'i64';
TYPE_DOUBLE: 'double';
TYPE_STRING: 'string';
TYPE_BINARY: 'binary';

LITERAL
    : (('"' ~'"'* '"') | ('\'' ~'\''* '\''))
        { setText(getText().substring(1, getText().length() - 1)); }
    ;

IDENTIFIER
    : (LETTER | '_') (LETTER | DIGIT | '.' | '_')*
    ;

COMMA
    : ','
    ;

fragment LETTER
    : 'A'..'Z' | 'a'..'z'
    ;

fragment DIGIT
    : '0'..'9'
    ;

fragment HEX_DIGIT
    : DIGIT | 'A'..'F' | 'a'..'f'
    ;

WS
    : (' ' | '\t' | '\r' '\n' | '\n')+ { $channel = HIDDEN; }
    ;

COMMENT
    : '/*' (options {greedy=false;} : .)* '*/' { $channel = HIDDEN; }
    | ('//' | '#') (~'\n')* { $channel = HIDDEN; }
    ;
