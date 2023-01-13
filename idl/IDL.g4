/*
[The "BSD licence"]
Copyright (c) 2014 AutoTest Technologies, LLC
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

/** CORBA IDL v3.5 grammar built from the OMG IDL language spec 'ptc-13-02-02'
    http://www.omg.org/spec/IDL35/Beta1/PDF/

    Initial IDL spec implementation in ANTLR v3 by Dong Nguyen.
    Migrated to ANTLR v4 by Steve Osselton.
    Current revision prepared by Nikita Visnevski.
    Renaming of COMA to COMMA and addition of OCTAL_LITERAL in `literal`
    by Oliver Kellogg.
    Support for IDL4 annotation applications, integration of eProsima IDL4
    updates by Oliver Kellogg.
*/

grammar IDL;

specification
   : import_decl* definition + EOF
   ;

definition
   : annapps
     ( type_decl SEMICOLON
     | const_decl SEMICOLON
     | except_decl SEMICOLON
     | interface_or_forward_decl SEMICOLON
     | module SEMICOLON
     | value SEMICOLON
     | type_id_decl SEMICOLON
     | type_prefix_decl SEMICOLON
     | event SEMICOLON
     | component SEMICOLON
     | home_decl SEMICOLON
     | annotation_decl SEMICOLON
     )
   ;

module
   : KW_MODULE identifier LEFT_BRACE definition + RIGHT_BRACE
   ;

interface_or_forward_decl
   : annapps
     ( interface_decl
     | forward_decl
     )
   ;

interface_decl
   : interface_header LEFT_BRACE interface_body RIGHT_BRACE
   ;

forward_decl
   : (KW_ABSTRACT | KW_LOCAL)? KW_INTERFACE identifier
   ;

interface_header
   : (KW_ABSTRACT | KW_LOCAL)? KW_INTERFACE identifier (interface_inheritance_spec)?
   ;

interface_body
   : export_*
   ;

export_
   : annapps
     ( type_decl SEMICOLON
     | const_decl SEMICOLON
     | except_decl SEMICOLON
     | attr_decl SEMICOLON
     | op_decl SEMICOLON
     | type_id_decl SEMICOLON
     | type_prefix_decl SEMICOLON
     )
   ;

interface_inheritance_spec
   : COLON interface_name (COMMA interface_name)*
   ;

interface_name
   : a_scoped_name
   ;

// scoped_name with optional prefixed annotations
a_scoped_name
   : annapps scoped_name
   ;

scoped_name
   : (DOUBLE_COLON)? ID (DOUBLE_COLON ID)*
   ;

value
   : annapps
     (value_decl | value_abs_decl | value_box_decl | value_forward_decl)
   ;
 
value_forward_decl
   : (KW_ABSTRACT)? KW_VALUETYPE identifier
   ;

value_box_decl
   : KW_VALUETYPE identifier type_spec
   ;

value_abs_decl
   : KW_ABSTRACT KW_VALUETYPE identifier value_inheritance_spec LEFT_BRACE export_* RIGHT_BRACE
   ;

value_decl
   : value_header LEFT_BRACE value_element* RIGHT_BRACE
   ;

value_header
   : (KW_CUSTOM)? KW_VALUETYPE identifier value_inheritance_spec
   ;

value_inheritance_spec
   : (COLON (KW_TRUNCATABLE)? value_name (COMMA value_name)*)? (KW_SUPPORTS interface_name (COMMA interface_name)*)?
   ;

value_name
   : a_scoped_name
   ;

value_element
   : (export_ | state_member | init_decl)
   ;

state_member
   : annapps ( KW_PUBLIC annapps | KW_PRIVATE annapps ) type_spec declarators SEMICOLON
   ;

init_decl
   : annapps KW_FACTORY identifier LEFT_BRACKET (init_param_decls)? RIGHT_BRACKET (raises_expr)? SEMICOLON
   ;

init_param_decls
   : init_param_decl (COMMA init_param_decl)*
   ;

init_param_decl
   : annapps init_param_attribute annapps param_type_spec annapps simple_declarator
   ;

init_param_attribute
   : KW_IN
   ;

const_decl
   : KW_CONST const_type identifier EQUAL const_exp
   ;

const_type
   : annapps
     ( integer_type
     | char_type
     | wide_char_type
     | boolean_type
     | floating_pt_type
     | string_type
     | wide_string_type
     | fixed_pt_const_type
     | scoped_name
     | octet_type
     )
   ;

const_exp
   : or_expr
   ;

or_expr
   : xor_expr (PIPE xor_expr)*
   ;

xor_expr
   : and_expr (CARET and_expr)*
   ;

and_expr
   : shift_expr (AMPERSAND shift_expr)*
   ;

shift_expr
   : add_expr ((RIGHT_SHIFT | LEFT_SHIFT) add_expr)*
   ;

add_expr
   : mult_expr ((PLUS | MINUS) mult_expr)*
   ;

mult_expr
   : unary_expr ((STAR | SLASH | PERCENT) unary_expr)*
   ;

unary_expr
   : unary_operator primary_expr
   | primary_expr
   ;

unary_operator
   : (MINUS | PLUS | TILDE)
   ;

primary_expr
   : scoped_name
   | literal
   | LEFT_BRACKET const_exp RIGHT_BRACKET
   ;

literal
   : (HEX_LITERAL | INTEGER_LITERAL | OCTAL_LITERAL | STRING_LITERAL | WIDE_STRING_LITERAL | CHARACTER_LITERAL | WIDE_CHARACTER_LITERAL | FIXED_PT_LITERAL | FLOATING_PT_LITERAL | BOOLEAN_LITERAL)
   ;

positive_int_const
   : const_exp
   ;

type_decl
   : KW_TYPEDEF annapps type_declarator
   | struct_type
   | union_type
   | enum_type
   | bitset_type
   | bitmask_type
   | KW_NATIVE annapps simple_declarator
   | constr_forward_decl
   ;

type_declarator
   : type_spec declarators
   ;

type_spec
   : simple_type_spec
   | constr_type_spec
   ;

simple_type_spec
   : base_type_spec
   | template_type_spec
   | scoped_name
   ;

bitfield_type_spec
   : integer_type
   | boolean_type
   | octet_type
   ;

base_type_spec
   : floating_pt_type
   | integer_type
   | char_type
   | wide_char_type
   | boolean_type
   | octet_type
   | any_type
   | object_type
   | value_base_type
   ;

template_type_spec
   : sequence_type
   | set_type
   | map_type
   | string_type
   | wide_string_type
   | fixed_pt_type
   ;

constr_type_spec
   : struct_type
   | union_type
   | enum_type
   | bitset_type
   | bitmask_type
   ;

simple_declarators
   : identifier (COMMA identifier)*
   ;

declarators
   : declarator (COMMA declarator)*
   ;

declarator
   : annapps
     ( simple_declarator
     | complex_declarator
     )
   ;

simple_declarator
   : ID
   ;

complex_declarator
   : array_declarator
   ;

floating_pt_type
   : (KW_FLOAT | KW_DOUBLE | KW_LONG KW_DOUBLE)
   ;

integer_type
   : signed_int
   | unsigned_int
   ;

signed_int
   : signed_short_int
   | signed_long_int
   | signed_longlong_int
   | signed_tiny_int
   ;

signed_tiny_int
   : KW_INT8
   ;

signed_short_int
   : KW_SHORT
   | KW_INT16
   ;

signed_long_int
   : KW_LONG
   | KW_INT32
   ;

signed_longlong_int
   : KW_LONG KW_LONG
   | KW_INT64
   ;

unsigned_int
   : unsigned_short_int
   | unsigned_long_int
   | unsigned_longlong_int
   | unsigned_tiny_int
   ;

unsigned_tiny_int
   : KW_UINT8
   ;

unsigned_short_int
   : KW_UNSIGNED KW_SHORT
   | KW_UINT16
   ;

unsigned_long_int
   : KW_UNSIGNED KW_LONG
   | KW_UINT32
   ;

unsigned_longlong_int
   : KW_UNSIGNED KW_LONG KW_LONG
   | KW_UINT64
   ;

char_type
   : KW_CHAR
   ;

wide_char_type
   : KW_WCHAR
   ;

boolean_type
   : KW_BOOLEAN
   ;

octet_type
   : KW_OCTET
   ;

any_type
   : KW_ANY
   ;

object_type
   : KW_OBJECT
   ;

annotation_decl
   : annotation_def
   | annotation_forward_dcl
   ;

annotation_def
   : annotation_header LEFT_BRACE annotation_body RIGHT_BRACE
   ;

annotation_header
   : KW_AT_ANNOTATION identifier (annotation_inheritance_spec)?
   ;

annotation_inheritance_spec
   : COLON scoped_name
   ;

annotation_body
   : ( annotation_member
     | enum_type SEMICOLON
     | const_decl SEMICOLON
     | KW_TYPEDEF type_declarator SEMICOLON
     )*
   ;

annotation_member
   : const_type simple_declarator (KW_DEFAULT const_exp)? SEMICOLON
   ;

annotation_forward_dcl
   : KW_AT_ANNOTATION scoped_name
   ;

bitset_type
   : KW_BITSET identifier (COLON scoped_name)? LEFT_BRACE bitfield RIGHT_BRACE
   ;

bitfield
   : ( bitfield_spec (simple_declarators)? SEMICOLON )+
   ;

bitfield_spec
   : annapps KW_BITFIELD LEFT_ANG_BRACKET positive_int_const (COMMA bitfield_type_spec)? RIGHT_ANG_BRACKET
   ;

bitmask_type
   : KW_BITMASK identifier LEFT_BRACE bit_values RIGHT_BRACE
   ;

bit_values
   : identifier (COMMA identifier)*
   ;

struct_type
   : KW_STRUCT identifier (COLON scoped_name)? LEFT_BRACE member_list RIGHT_BRACE
   ;

member_list
   : member *
   ;

member
   : annapps type_spec declarators SEMICOLON
   ;

union_type
   : KW_UNION identifier KW_SWITCH LEFT_BRACKET annapps switch_type_spec RIGHT_BRACKET LEFT_BRACE switch_body RIGHT_BRACE
   ;

switch_type_spec
   : integer_type
   | char_type
   | wide_char_type
   | octet_type
   | boolean_type
   | enum_type
   | scoped_name
   ;

switch_body
   : case_stmt +
   ;

case_stmt
   : case_label + element_spec SEMICOLON
   ;

case_label
   : annapps
     ( KW_CASE const_exp COLON
     | KW_DEFAULT COLON
     )
   ;

element_spec
   : annapps type_spec declarator
   ;

enum_type
   : KW_ENUM identifier LEFT_BRACE enumerator (COMMA enumerator)* RIGHT_BRACE
   ;

enumerator
   : identifier
   ;

sequence_type
   : KW_SEQUENCE LEFT_ANG_BRACKET annapps simple_type_spec (COMMA positive_int_const)? RIGHT_ANG_BRACKET
   ;

set_type
   : KW_SET LEFT_ANG_BRACKET simple_type_spec (COMMA positive_int_const)? RIGHT_ANG_BRACKET
   ;

map_type
   : KW_MAP LEFT_ANG_BRACKET simple_type_spec COMMA simple_type_spec (COMMA positive_int_const)?  RIGHT_ANG_BRACKET
   ;

string_type
   : KW_STRING (LEFT_ANG_BRACKET positive_int_const RIGHT_ANG_BRACKET)?
   ;

wide_string_type
   : KW_WSTRING (LEFT_ANG_BRACKET positive_int_const RIGHT_ANG_BRACKET)?
   ;

array_declarator
   : ID fixed_array_size +
   ;

fixed_array_size
   : LEFT_SQUARE_BRACKET positive_int_const RIGHT_SQUARE_BRACKET
   ;

attr_decl
   : readonly_attr_spec
   | attr_spec
   ;

except_decl
   : KW_EXCEPTION identifier LEFT_BRACE member* RIGHT_BRACE
   ;

op_decl
   : (op_attribute)? op_type_spec identifier parameter_decls (raises_expr)? (context_expr)?
   ;

op_attribute
   : KW_ONEWAY
   ;

op_type_spec
   : annapps
     ( param_type_spec
     | KW_VOID
     )
   ;

parameter_decls
   : LEFT_BRACKET ( param_decl (COMMA param_decl)* )? RIGHT_BRACKET
   ;

param_decl
   : annapps param_attribute annapps param_type_spec annapps simple_declarator
   ;

param_attribute
   : KW_IN
   | KW_OUT
   | KW_INOUT
   ;

raises_expr
   : KW_RAISES LEFT_BRACKET a_scoped_name (COMMA a_scoped_name)* RIGHT_BRACKET
   ;

context_expr
   : KW_CONTEXT LEFT_BRACKET STRING_LITERAL (COMMA STRING_LITERAL)* RIGHT_BRACKET
   ;

param_type_spec
   : base_type_spec
   | string_type
   | wide_string_type
   | scoped_name
   ;

fixed_pt_type
   : KW_FIXED LEFT_ANG_BRACKET positive_int_const COMMA positive_int_const RIGHT_ANG_BRACKET
   ;

fixed_pt_const_type
   : KW_FIXED
   ;

value_base_type
   : KW_VALUEBASE
   ;

constr_forward_decl
   : KW_STRUCT ID
   | KW_UNION ID
   ;

import_decl
   : annapps KW_IMPORT annapps imported_scope SEMICOLON
   ;

imported_scope
   : scoped_name
   | STRING_LITERAL
   ;

type_id_decl
   : KW_TYPEID a_scoped_name STRING_LITERAL
   ;

type_prefix_decl
   : KW_TYPEPREFIX a_scoped_name STRING_LITERAL
   ;

readonly_attr_spec
   : KW_READONLY KW_ATTRIBUTE annapps param_type_spec readonly_attr_declarator
   ;

readonly_attr_declarator
   : annapps simple_declarator
     ( raises_expr
     | (COMMA annapps simple_declarator)*
     )
   ;

attr_spec
   : KW_ATTRIBUTE annapps param_type_spec attr_declarator
   ;

attr_declarator
   : annapps simple_declarator
     ( attr_raises_expr
     | (COMMA annapps simple_declarator)*
     )
   ;

attr_raises_expr
   : get_excep_expr (set_excep_expr)?
   | set_excep_expr
   ;

get_excep_expr
   : KW_GETRAISES exception_list
   ;

set_excep_expr
   : KW_SETRAISES exception_list
   ;

exception_list
   : LEFT_BRACKET a_scoped_name (COMMA a_scoped_name)* RIGHT_BRACKET
   ;

component
   : component_decl
   | component_forward_decl
   ;

component_forward_decl
   : KW_COMPONENT ID
   ;

component_decl
   : component_header LEFT_BRACE component_body RIGHT_BRACE
   ;

component_header
   : KW_COMPONENT identifier (component_inheritance_spec)? (supported_interface_spec)?
   ;

supported_interface_spec
   : KW_SUPPORTS a_scoped_name (COMMA a_scoped_name)*
   ;

component_inheritance_spec
   : COLON a_scoped_name
   ;

component_body
   : component_export*
   ;

component_export
   : annapps
     ( provides_decl SEMICOLON
     | uses_decl SEMICOLON
     | emits_decl SEMICOLON
     | publishes_decl SEMICOLON
     | consumes_decl SEMICOLON
     | attr_decl SEMICOLON
     )
   ;

provides_decl
   : KW_PROVIDES interface_type ID
   ;

interface_type
   : annapps
     ( scoped_name
     | KW_OBJECT
     )
   ;

uses_decl
   : KW_USES (KW_MULTIPLE)? interface_type ID
   ;

emits_decl
   : KW_EMITS a_scoped_name ID
   ;

publishes_decl
   : KW_PUBLISHES a_scoped_name ID
   ;

consumes_decl
   : KW_CONSUMES a_scoped_name ID
   ;

home_decl
   : home_header home_body
   ;

home_header
   : KW_HOME identifier (home_inheritance_spec)? (supported_interface_spec)? KW_MANAGES a_scoped_name (primary_key_spec)?
   ;

home_inheritance_spec
   : COLON a_scoped_name
   ;

primary_key_spec
   : KW_PRIMARYKEY a_scoped_name
   ;

home_body
   : LEFT_BRACE home_export* RIGHT_BRACE
   ;

home_export
   : export_
   | annapps
     ( factory_decl
     | finder_decl
     )
     SEMICOLON
   ;

factory_decl
   : KW_FACTORY identifier LEFT_BRACKET (init_param_decls)? RIGHT_BRACKET (raises_expr)?
   ;

finder_decl
   : KW_FINDER identifier LEFT_BRACKET (init_param_decls)? RIGHT_BRACKET (raises_expr)?
   ;

event
   : (event_decl | event_abs_decl | event_forward_decl)
   ;

event_forward_decl
   : (KW_ABSTRACT)? KW_EVENTTYPE ID
   ;

event_abs_decl
   : KW_ABSTRACT KW_EVENTTYPE identifier value_inheritance_spec LEFT_BRACE export_* RIGHT_BRACE
   ;

event_decl
   : event_header LEFT_BRACE value_element* RIGHT_BRACE
   ;

event_header
   : (KW_CUSTOM)? KW_EVENTTYPE identifier value_inheritance_spec
   ;

annapps
   : ( annotation_appl )*
   ;

annotation_appl
   : AT scoped_name ( LEFT_BRACKET annotation_appl_params RIGHT_BRACKET )?
   ;

annotation_appl_params
   : const_exp
   | annotation_appl_param ( COMMA annotation_appl_param )*
   ;

annotation_appl_param
   : ID EQUAL const_exp
   ;

identifier
   : annapps ID
   ;


INTEGER_LITERAL
   : ('0' | '1' .. '9' '0' .. '9'*) INTEGER_TYPE_SUFFIX?
   ;


OCTAL_LITERAL
   : '0' ('0' .. '7') + INTEGER_TYPE_SUFFIX?
   ;


HEX_LITERAL
   : '0' ('x' | 'X') HEX_DIGIT + INTEGER_TYPE_SUFFIX?
   ;


fragment HEX_DIGIT
   : ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
   ;


fragment INTEGER_TYPE_SUFFIX
   : ('l' | 'L')
   ;


FLOATING_PT_LITERAL
   : ('0' .. '9') + '.' ('0' .. '9')* EXPONENT? FLOAT_TYPE_SUFFIX?
   | '.' ('0' .. '9') + EXPONENT? FLOAT_TYPE_SUFFIX?
   | ('0' .. '9') + EXPONENT FLOAT_TYPE_SUFFIX?
   | ('0' .. '9') + EXPONENT? FLOAT_TYPE_SUFFIX
   ;


FIXED_PT_LITERAL
   : FLOATING_PT_LITERAL
   ;


fragment EXPONENT
   : ('e' | 'E') (PLUS | MINUS)? ('0' .. '9') +
   ;


fragment FLOAT_TYPE_SUFFIX
   : ('f' | 'F' | 'd' | 'D')
   ;


WIDE_CHARACTER_LITERAL
   : 'L' CHARACTER_LITERAL
   ;


CHARACTER_LITERAL
   : '\'' (ESCAPE_SEQUENCE | ~ ('\'' | '\\')) '\''
   ;


WIDE_STRING_LITERAL
   : 'L' STRING_LITERAL
   ;


STRING_LITERAL
   : '"' (ESCAPE_SEQUENCE | ~ ('\\' | '"'))* '"'
   ;


BOOLEAN_LITERAL
   : 'TRUE'
   | 'FALSE'
   ;


fragment ESCAPE_SEQUENCE
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
   | UNICODE_ESCAPE
   | OCTAL_ESCAPE
   ;


fragment OCTAL_ESCAPE
   : '\\' ('0' .. '3') ('0' .. '7') ('0' .. '7')
   | '\\' ('0' .. '7') ('0' .. '7')
   | '\\' ('0' .. '7')
   ;


fragment UNICODE_ESCAPE
   : '\\' 'u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
   ;


fragment LETTER
   : '\u0024'
   | '\u0041' .. '\u005a'
   | '\u005f'
   | '\u0061' .. '\u007a'
   | '\u00c0' .. '\u00d6'
   | '\u00d8' .. '\u00f6'
   | '\u00f8' .. '\u00ff'
   | '\u0100' .. '\u1fff'
   | '\u3040' .. '\u318f'
   | '\u3300' .. '\u337f'
   | '\u3400' .. '\u3d2d'
   | '\u4e00' .. '\u9fff'
   | '\uf900' .. '\ufaff'
   ;


fragment ID_DIGIT
   : '\u0030' .. '\u0039'
   | '\u0660' .. '\u0669'
   | '\u06f0' .. '\u06f9'
   | '\u0966' .. '\u096f'
   | '\u09e6' .. '\u09ef'
   | '\u0a66' .. '\u0a6f'
   | '\u0ae6' .. '\u0aef'
   | '\u0b66' .. '\u0b6f'
   | '\u0be7' .. '\u0bef'
   | '\u0c66' .. '\u0c6f'
   | '\u0ce6' .. '\u0cef'
   | '\u0d66' .. '\u0d6f'
   | '\u0e50' .. '\u0e59'
   | '\u0ed0' .. '\u0ed9'
   | '\u1040' .. '\u1049'
   ;


SEMICOLON
   : ';'
   ;


COLON
   : ':'
   ;


COMMA
   : ','
   ;


LEFT_BRACE
   : '{'
   ;


RIGHT_BRACE
   : '}'
   ;


LEFT_BRACKET
   : '('
   ;


RIGHT_BRACKET
   : ')'
   ;


LEFT_SQUARE_BRACKET
   : '['
   ;


RIGHT_SQUARE_BRACKET
   : ']'
   ;


TILDE
   : '~'
   ;


SLASH
   : '/'
   ;


LEFT_ANG_BRACKET
   : '<'
   ;


RIGHT_ANG_BRACKET
   : '>'
   ;


STAR
   : '*'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


CARET
   : '^'
   ;


AMPERSAND
   : '&'
   ;


PIPE
   : '|'
   ;


EQUAL
   : '='
   ;


PERCENT
   : '%'
   ;


DOUBLE_COLON
   : '::'
   ;


RIGHT_SHIFT
   : '>>'
   ;


LEFT_SHIFT
   : '<<'
   ;


AT : '@'
   ;


KW_SETRAISES
   : 'setraises'
   ;


KW_OUT
   : 'out'
   ;


KW_EMITS
   : 'emits'
   ;


KW_STRING
   : 'string'
   ;


KW_SWITCH
   : 'switch'
   ;


KW_PUBLISHES
   : 'publishes'
   ;


KW_TYPEDEF
   : 'typedef'
   ;


KW_USES
   : 'uses'
   ;


KW_PRIMARYKEY
   : 'primarykey'
   ;


KW_CUSTOM
   : 'custom'
   ;


KW_OCTET
   : 'octet'
   ;


KW_SEQUENCE
   : 'sequence'
   ;


KW_IMPORT
   : 'import'
   ;


KW_STRUCT
   : 'struct'
   ;


KW_NATIVE
   : 'native'
   ;


KW_READONLY
   : 'readonly'
   ;


KW_FINDER
   : 'finder'
   ;


KW_RAISES
   : 'raises'
   ;


KW_VOID
   : 'void'
   ;


KW_PRIVATE
   : 'private'
   ;


KW_EVENTTYPE
   : 'eventtype'
   ;


KW_WCHAR
   : 'wchar'
   ;


KW_IN
   : 'in'
   ;


KW_DEFAULT
   : 'default'
   ;


KW_PUBLIC
   : 'public'
   ;


KW_SHORT
   : 'short'
   ;


KW_LONG
   : 'long'
   ;


KW_ENUM
   : 'enum'
   ;


KW_WSTRING
   : 'wstring'
   ;


KW_CONTEXT
   : 'context'
   ;


KW_HOME
   : 'home'
   ;


KW_FACTORY
   : 'factory'
   ;


KW_EXCEPTION
   : 'exception'
   ;


KW_GETRAISES
   : 'getraises'
   ;


KW_CONST
   : 'const'
   ;


KW_VALUEBASE
   : 'ValueBase'
   ;


KW_VALUETYPE
   : 'valuetype'
   ;


KW_SUPPORTS
   : 'supports'
   ;


KW_MODULE
   : 'module'
   ;


KW_OBJECT
   : 'Object'
   ;


KW_TRUNCATABLE
   : 'truncatable'
   ;


KW_UNSIGNED
   : 'unsigned'
   ;


KW_FIXED
   : 'fixed'
   ;


KW_UNION
   : 'union'
   ;


KW_ONEWAY
   : 'oneway'
   ;


KW_ANY
   : 'any'
   ;


KW_CHAR
   : 'char'
   ;


KW_CASE
   : 'case'
   ;


KW_FLOAT
   : 'float'
   ;


KW_BOOLEAN
   : 'boolean'
   ;


KW_MULTIPLE
   : 'multiple'
   ;


KW_ABSTRACT
   : 'abstract'
   ;


KW_INOUT
   : 'inout'
   ;


KW_PROVIDES
   : 'provides'
   ;


KW_CONSUMES
   : 'consumes'
   ;


KW_DOUBLE
   : 'double'
   ;


KW_TYPEPREFIX
   : 'typeprefix'
   ;


KW_TYPEID
   : 'typeid'
   ;


KW_ATTRIBUTE
   : 'attribute'
   ;


KW_LOCAL
   : 'local'
   ;


KW_MANAGES
   : 'manages'
   ;


KW_INTERFACE
   : 'interface'
   ;


KW_COMPONENT
   : 'component'
   ;

KW_SET
   : 'set'
   ;

KW_MAP
   : 'map'
   ;

KW_BITFIELD
   : 'bitfield'
   ;

KW_BITSET
   : 'bitset'
   ;

KW_BITMASK
   : 'bitmask'
   ;

KW_INT8
   : 'int8'
   ;

KW_UINT8
   : 'uint8'
   ;

KW_INT16
   : 'int16'
   ;

KW_UINT16
   : 'uint16'
   ;

KW_INT32
   : 'int32'
   ;

KW_UINT32
   : 'uint32'
   ;

KW_INT64
   : 'int64'
   ;

KW_UINT64
   : 'uint64'
   ;

KW_AT_ANNOTATION
   : '@annotation'
   ;

ID
   : LETTER (LETTER | ID_DIGIT)*
   ;


WS
   : (' ' | '\r' | '\t' | '\u000C' | '\n') -> channel (HIDDEN)
   ;


COMMENT
   : '/*' .*? '*/' -> channel (HIDDEN)
   ;


LINE_COMMENT
   : '//' ~ ('\n' | '\r')* '\r'? '\n' -> channel (HIDDEN)
   ;

