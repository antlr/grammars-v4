
grammar FlatBuffers ;

// Parser rules

schema : include* ( namespace_decl | type_decl | enum_decl | root_decl | file_extension_decl | file_identifier_decl | attribute_decl | rpc_decl | object )* ;

include : 'include' STRING_CONSTANT ';' ;

namespace_decl : 'namespace' IDENT ( '.' IDENT )* ';' ;

attribute_decl : 'attribute' STRING_CONSTANT ';' ;

type_decl : ( 'table' | 'struct' ) IDENT metadata '{' ( field_decl )* '}' ;

enum_decl : ( 'enum' IDENT ( ':' type )? | 'union' IDENT ) metadata '{' commasep_enumval_decl '}' ;

root_decl : 'root_type' IDENT ';' ;

field_decl : IDENT ':' type ( '=' scalar )? metadata ';' ;

rpc_decl : 'rpc_service' IDENT '{' rpc_method+ '}' ;

rpc_method : IDENT '(' IDENT ')' ':' IDENT metadata ';' ;

// fixed original grammar: allow namespaces for IDENTs
type : '[' type ']' | BASE_TYPE_NAME | ns_ident ;

enumval_decl : ns_ident ( '=' INTEGER_CONSTANT )? ;

commasep_enumval_decl : enumval_decl ( ',' enumval_decl )* ','? ;

ident_with_opt_single_value : IDENT ( ':' single_value )? ;

commasep_ident_with_opt_single_value : ident_with_opt_single_value ( ',' ident_with_opt_single_value )* ;

metadata : ( '(' commasep_ident_with_opt_single_value ')' )? ;

// fix original grammar: enum values (IDENT) are allowed as well
scalar : INTEGER_CONSTANT | FLOAT_CONSTANT | IDENT ;

object : '{' commasep_ident_with_value '}' ;

ident_with_value : IDENT ':' value ;

commasep_ident_with_value : ident_with_value ( ',' ident_with_value )* ','? ;

single_value : scalar | STRING_CONSTANT ;

value : single_value | object | '[' commasep_value ']' ;

commasep_value : value( ',' value )* ','? ;

file_extension_decl : 'file_extension' STRING_CONSTANT ;

file_identifier_decl : 'file_identifier' STRING_CONSTANT ;

ns_ident : IDENT ( '.' IDENT )* ;

// Lexer rules

STRING_CONSTANT : '"' ~["\r\n]* '"' ;

BASE_TYPE_NAME : 'bool' | 'byte' | 'ubyte' | 'short' | 'ushort' | 'int' | 'uint' | 'float' | 'long' | 'ulong' | 'double' | 'int8' | 'uint8' | 'int16' | 'uint16' | 'int32' | 'uint32' | 'int64' | 'uint64' | 'float32' | 'float64' | 'string' ;

IDENT : [a-zA-Z_] [a-zA-Z0-9_]* ;

INTEGER_CONSTANT : '-'? [0-9]+ | 'true' | 'false' ;

FLOAT_CONSTANT : '-'? [0-9]+ '.' [0-9]+ (('e'|'E') ('+'|'-')? [0-9]+ )? ;

// fixed original grammar: allow line comments
COMMENT : '//' ~[\r\n]* -> channel(HIDDEN);

WHITESPACE : [ \t\r\n] -> skip ;