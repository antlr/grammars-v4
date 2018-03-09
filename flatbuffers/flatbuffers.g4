
grammar Flatbuffers ;

schema : include* ( namespace_decl | type_decl | enum_decl | root_decl | file_extension_decl | file_identifier_decl | attribute_decl | rpc_decl | object )* ;

include : 'include' string_constant ';' ;

namespace_decl : 'namespace' IDENT ( '.' IDENT )* ';' ;

attribute_decl : 'attribute' string_constant ';' ;

type_decl : ( 'table' | 'struct' ) IDENT metadata { field_decl+ } ;

enum_decl : ( 'enum' IDENT ( ':' type )? | 'union' IDENT ) metadata '{' commasep_enumval_decl '}' ;

root_decl : 'root_type' IDENT ';' ;

field_decl : IDENT ':' type ( '=' scalar )? metadata ';' ;

rpc_decl : 'rpc_service' IDENT '{' rpc_method+ '}' ;

rpc_method : IDENT '(' IDENT ')' ':' IDENT metadata ';' ;

type : '[' type ']' | BASE_TYPE_NAME | IDENT ;

BASE_TYPE_NAME : 'bool' | 'byte' | 'ubyte' | 'short' | 'ushort' | 'int' | 'uint' | 'float' | 'long' | 'ulong' | 'double' | 'int8' | 'uint8' | 'int16' | 'uint16' | 'int32' | 'uint32' | 'int64' | 'uint64' | 'float32' | 'float64' | 'string' ;

enumval_decl : IDENT ( '=' integer_constant )? ;

commasep_enumval_decl : enumval_decl ( ',' enumval_decl )* ;

ident_with_opt_single_value : IDENT ( ':' single_value )? ;

commasep_ident_with_opt_single_value : ident_with_opt_single_value ( ',' ident_with_opt_single_value )* ;

metadata : ( '(' commasep_ident_with_opt_single_value ')' )? ;

scalar : integer_constant | float_constant ;

object : '{' commasep_ident_with_value '}' ;

ident_with_value : IDENT ':' value ;

commasep_ident_with_value : ident_with_value ( ',' ident_with_value )* ;

single_value : scalar | string_constant ;

value : single_value | object | '[' commasep_value ']' ;

commasep_value : value( ',' value )* ;

file_extension_decl : 'file_extension' string_constant ;

file_identifier_decl : 'file_identifier' string_constant ;

fragment DIGIT : [0-9] ;

fragment LETTER_INCL_DIGITS : [a-zA-Z0-9_] ;

fragment LETTER_EXCL_DIGITS : [a-zA-Z_] ;

integer_constant : ( '-' )? (DIGIT)+ | 'true' | 'false' ;

float_constant : ('-')? (DIGIT)+ '.' (DIGIT)+ (('e'|'E') ('+'|'-')? (DIGIT)+)? ;

string_constant : '"' .*? '"' ;

IDENT : LETTER_EXCL_DIGITS ( LETTER_INCL_DIGITS )* ;
