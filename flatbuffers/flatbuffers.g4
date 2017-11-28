grammar flatbuffers;

schema : include* ( namespace_decl | type_decl | enum_decl | root_decl | file_extension_decl | file_identifier_decl | attribute_decl | rpc_decl | object )* ;

include : include string_constant ;

namespace_decl : namespace ident ( . ident )* ;

attribute_decl : attribute string_constant ;

type_decl : ( table | struct ) ident metadata { field_decl+ }

enum_decl : ( enum ident [ : type ] | union ident ) metadata { commasep( enumval_decl ) }

root_decl : root_type ident ;

field_decl : ident ":" type [ "=" scalar ] metadata ;

rpc_decl : rpc_service ident { rpc_method+ } ;

rpc_method : ident ( ident ) ":" ident metadata ;

type : bool | byte | ubyte | short | ushort | int | uint | float | long | ulong | double | int8 | uint8 | int16 | uint16 | int32 | uint32| int64 | uint64 | float32 | float64 | string | [ type ] | ident ;

enumval_decl : ident [ = integer_constant ] ;

metadata : [ ( commasep( ident [ : single_value ] ) ) ] ;

scalar : integer_constant | float_constant ;

object : { commasep( ident ":" value ) } ;

single_value : scalar | string_constant ;

value : single_value | object | [ commasep( value ) ] ;

commasep(x) : [ x ( , x )* ] ;

file_extension_decl : file_extension string_constant ;

file_identifier_decl : file_identifier string_constant ;

integer_constant : -?[0-9]+ | true | false ;

float_constant : -?[0-9]+.[0-9]+((e|E)(+|-)?[0-9]+)? ;

string_constant : \".*?\\" ;

ident : [a-zA-Z_][a-zA-Z0-9_]* ;
