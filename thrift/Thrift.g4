grammar Thrift;

document
    : header* definition* EOF
    ;

header
    : include | namespace | cpp_include
    ;

include
    : 'include' LITERAL
    ;

namespace
    : 'namespace' '*' (IDENTIFIER | LITERAL)
    | 'namespace' IDENTIFIER (IDENTIFIER | LITERAL)
    | 'cpp_namespace' IDENTIFIER
    | 'php_namespace' IDENTIFIER
    ;

cpp_include
    : 'cpp_include' LITERAL
    ;


definition
    : const_rule | typedef | enum_rule | senum | struct | union | exception | service
    ;

const_rule
    : 'const' field_type IDENTIFIER ( '=' const_value )? list_separator?
    ;

typedef
    : 'typedef' field_type IDENTIFIER type_annotations?
    ;

enum_rule
    : 'enum' IDENTIFIER '{' enum_field* '}' type_annotations?
    ;

enum_field
    : IDENTIFIER ('=' integer)? type_annotations? list_separator?
    ;

senum
    : 'senum' IDENTIFIER '{' (LITERAL list_separator?)* '}' type_annotations?
    ;

struct
    : 'struct' IDENTIFIER '{' field* '}' type_annotations?
    ;

union
    : 'union' IDENTIFIER '{' field* '}' type_annotations?
    ;

exception
    : 'exception' IDENTIFIER '{' field* '}' type_annotations?
    ;

service
    : 'service' IDENTIFIER ('extends' IDENTIFIER)? '{' function* '}' type_annotations?
    ;

field
    : field_id? field_req? field_type IDENTIFIER ('=' const_value)? type_annotations? list_separator?
    ;

field_id
    : integer ':'
    ;

field_req
    : 'required'
    | 'optional'
    ;

function
    : oneway? function_type IDENTIFIER '(' field* ')' throws_list? type_annotations? list_separator?
    ;

oneway
    : ('oneway' | 'async')
    ;

function_type
    : field_type
    | 'void'
    ;

throws_list
    : 'throws' '(' field* ')'
    ;


type_annotations
    : '(' type_annotation* ')'
    ;

type_annotation
    : IDENTIFIER ('=' annotation_value)? list_separator?
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
    : 'map' cpp_type? '<' field_type COMMA field_type '>'
    ;

set_type
    : 'set' cpp_type? '<' field_type '>'
    ;

list_type
    : 'list' '<' field_type '>' cpp_type?
    ;

cpp_type
    : 'cpp_type' LITERAL
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
    : '-'? '0x' HEX_DIGIT+
    ;

DOUBLE
    : ('+' | '-')? ( DIGIT+ ('.' DIGIT+)? | '.' DIGIT+ ) (('E' | 'e') INTEGER)?
    ;

const_list
    : '[' (const_value list_separator?)* ']'
    ;

const_map_entry
    : const_value ':' const_value list_separator?
    ;

const_map
    : '{' const_map_entry* '}'
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
    : (' ' | '\t' | '\r' '\n' | '\n')+ -> channel(HIDDEN)
    ;

SL_COMMENT
    : ('//' | '#') (~'\n')* ('\r')? '\n' -> channel(HIDDEN)
    ;

ML_COMMENT
    : '/*' .*? '*/' -> channel(HIDDEN)
    ;
