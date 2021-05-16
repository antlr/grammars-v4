
grammar FlatBuffers ;

// Parser rules

schema : include_* ( namespace_decl | type_decl | enum_decl | root_decl | file_extension_decl | file_identifier_decl | attribute_decl | rpc_decl | object_ )* ;

include_ : 'include' STRING_CONSTANT ';' ;

namespace_decl : 'namespace' IDENT ( '.' IDENT )* ';' ;

attribute_decl : 'attribute' STRING_CONSTANT ';' ;

type_decl : ( 'table' | 'struct' ) IDENT metadata '{' ( field_decl )* '}' ;

enum_decl : ( 'enum' IDENT ( ':' type_ )? | 'union' IDENT ) metadata '{' commasep_enumval_decl '}' ;

root_decl : 'root_type' IDENT ';' ;

field_decl : IDENT ':' type_ ( '=' scalar )? metadata ';' ;

rpc_decl : 'rpc_service' IDENT '{' rpc_method+ '}' ;

rpc_method : IDENT '(' IDENT ')' ':' IDENT metadata ';' ;

// fixed original grammar: allow namespaces for IDENTs
type_ : '[' type_ ']' | BASE_TYPE_NAME | ns_ident ;

enumval_decl : ns_ident ( '=' integer_const )? ;

commasep_enumval_decl : enumval_decl ( ',' enumval_decl )* ','? ;

ident_with_opt_single_value : IDENT ( ':' single_value )? ;

commasep_ident_with_opt_single_value : ident_with_opt_single_value ( ',' ident_with_opt_single_value )* ;

metadata : ( '(' commasep_ident_with_opt_single_value ')' )? ;

// fix original grammar: enum values (IDENT) are allowed as well
scalar : INTEGER_CONSTANT | HEX_INTEGER_CONSTANT | FLOAT_CONSTANT | IDENT ;

object_ : '{' commasep_ident_with_value '}' ;

ident_with_value : IDENT ':' value ;

commasep_ident_with_value : ident_with_value ( ',' ident_with_value )* ','? ;

single_value : scalar | STRING_CONSTANT ;

value : single_value | object_ | '[' commasep_value ']' ;

commasep_value : value( ',' value )* ','? ;

file_extension_decl : 'file_extension' STRING_CONSTANT ;

file_identifier_decl : 'file_identifier' STRING_CONSTANT ;

ns_ident : IDENT ( '.' IDENT )* ;

integer_const :  INTEGER_CONSTANT | HEX_INTEGER_CONSTANT ;

// Lexer rules

STRING_CONSTANT : '"' ~["\r\n]* '"' ;

BASE_TYPE_NAME : 'bool' | 'byte' | 'ubyte' | 'short' | 'ushort' | 'int' | 'uint' | 'float' | 'long' | 'ulong' | 'double' | 'int8' | 'uint8' | 'int16' | 'uint16' | 'int32' | 'uint32' | 'int64' | 'uint64' | 'float32' | 'float64' | 'string' ;

IDENT : [a-zA-Z_] [a-zA-Z0-9_]* ;

HEX_INTEGER_CONSTANT : [-+]? '0' [xX][0-9a-fA-F]+ ;

INTEGER_CONSTANT : [-+]? [0-9]+ | 'true' | 'false' ;

FLOAT_CONSTANT : '-'? [0-9]+ '.' [0-9]+ (('e'|'E') ('+'|'-')? [0-9]+ )? ;

BLOCK_COMMENT:	'/*' .*? '*/' -> channel(HIDDEN);

// fixed original grammar: allow line comments
COMMENT : '//' ~[\r\n]* -> channel(HIDDEN);

WHITESPACE : [ \t\r\n] -> skip ;
