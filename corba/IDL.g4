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

CORBA IDL grammar derived from:

    www.omg.org/spec/CORBA/3.3/

    Common Object Request Broker Architecture (CORBA) Specification, Version 3.3
 */
grammar IDL;

// Note: Replaced keywords: case, import, interface.

specification
	: import_* definition+ EOF
;
definition
	: type_dcl ';'
	| const_dcl ';'
	| except_dcl ';'
	| interface_ ';'
	| module ';'
	| value ';'
	| type_id_dcl ';'
	| type_prefix_dcl ';'
	| event ';'
	| component ';'
	| home_dcl ';'
;
module
	: 'module' identifier '{' definition+ '}'
;
interface_
	: interface_dcl
	| forward_dcl
;
interface_dcl
	: interface_header '{' interface_body '}'
;
forward_dcl
	: ('abstract' | 'local')? 'interface' identifier
;
interface_header
	: ('abstract' | 'local')? 'interface' identifier interface_inheritance_spec?
;
interface_body
	: export*
;
export
	: type_dcl ';'
	| const_dcl ';'
	| except_dcl ';'
	| attr_dcl ';'
	| op_dcl ';'
	| type_id_dcl ';'
	| type_prefix_dcl ';'
;
interface_inheritance_spec
	: ':' interface_name (',' interface_name)*
;
interface_name
	: scoped_name
;
scoped_name
	: identifier
	| '::' identifier
	| scoped_name '::' identifier
;
value
	: ( value_dcl | value_abs_dcl | value_box_dcl | value_forward_dcl)
;
value_forward_dcl
	: 'abstract'? 'valuetype' identifier
;
value_box_dcl
	: 'valuetype' identifier type_spec
;
value_abs_dcl
	: 'abstract' 'valuetype' identifier value_inheritance_spec? '{' export* '}'
;
value_dcl
	: value_header '{'  value_element* '}'
;
value_header
	: 'custom'? 'valuetype' identifier value_inheritance_spec?
;
value_inheritance_spec
	: (':' 'truncatable'? value_name (',' value_name)*)? ('supports' interface_name (',' interface_name)*)?
;
value_name
	: scoped_name
;
value_element
	: export |  state_member | init_dcl
;
state_member
	: ( 'public' | 'private' ) type_spec declarators ';'
;
init_dcl
	: 'factory' identifier '(' init_param_decls? ')' raises_expr? ';'
;
init_param_decls
	: init_param_decl (',' init_param_decl)*
;
init_param_decl
	: init_param_attribute param_type_spec simple_declarator
;
init_param_attribute
	: 'in'
;
const_dcl
	: 'const' const_type identifier '=' const_exp
;
const_type
	: integer_type
	| char_type
	| wide_char_type
	| boolean_type
	| floating_pt_type
	| string_type
	| wide_string_type
	| fixed_pt_const_type
	| scoped_name
	| octet_type
;
const_exp
	: or_expr
;
or_expr
	: xor_expr
	| or_expr '|' xor_expr
;
xor_expr
	: and_expr
	| xor_expr '^' and_expr
;
and_expr
	: shift_expr
	| and_expr '&' shift_expr
;
shift_expr
	: add_expr
	| shift_expr '>>' add_expr
	| shift_expr '<<' add_expr
;
add_expr
	: mult_expr
	| add_expr '+' mult_expr
	| add_expr '-' mult_expr
;
mult_expr
	: unary_expr
	| mult_expr '*' unary_expr
	| mult_expr '/' unary_expr
	| mult_expr '%' unary_expr
;
unary_expr
	: unary_operator primary_expr
	| primary_expr
;
unary_operator
	: '-'
	| '+'
	| '~'
;
primary_expr
	: scoped_name
	| literal
	| '(' const_exp ')'
;
literal
	: integer_literal
	| string_literal
	| wide_string_literal
	| character_literal
	| wide_character_literal
	| fixed_pt_literal
	| floating_pt_literal
	| boolean_literal
;
boolean_literal
	: 'TRUE'
	| 'FALSE'
;
positive_int_const
	: const_exp
;
type_dcl
	: 'typedef' type_declarator
	| struct_type
	| union_type
	| enum_type
	| 'native' simple_declarator
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
	| string_type
	| wide_string_type
	| fixed_pt_type
;
constr_type_spec
	: struct_type
	| union_type
	| enum_type
;
declarators
	: declarator (',' declarator)*
;
declarator
	: simple_declarator
	| complex_declarator
;
simple_declarator
	: identifier
;
complex_declarator
	: array_declarator
;
floating_pt_type
	: 'float'
	| 'double'
	| 'long' 'double'
;
integer_type
	: signed_int
	| unsigned_int
;
signed_int
	: signed_short_int
	| signed_long_int
	| signed_longlong_int
;
signed_short_int
	: 'short'
;
signed_long_int
	: 'long'
;
signed_longlong_int
	: 'long' 'long'
;
unsigned_int
	: unsigned_short_int
	| unsigned_long_int
	| unsigned_longlong_int
;
unsigned_short_int
	: 'unsigned' 'short'
;
unsigned_long_int
	: 'unsigned' 'long'
;
unsigned_longlong_int
	: 'unsigned' 'long' 'long'
;
char_type
	: 'char'
;
wide_char_type
	: 'wchar'
;
boolean_type
	: 'boolean'
;
octet_type
	: 'octet'
;
any_type
	: 'any'
;
object_type
	: 'Object'
;
struct_type
	: 'struct' identifier '{' member_list '}'
;
member_list
	: member+
;
member
	: type_spec declarators ';'
;
union_type
	: 'union' identifier 'switch' '(' switch_type_spec ')' '{' switch_body '}'
;
switch_type_spec
	: integer_type
	| char_type
	| boolean_type
	| enum_type
	| scoped_name
;
switch_body
	: case_+
;
case_
	: case_label+ element_spec ';'
;
case_label
	: 'case' const_exp ':'
	| 'default' ':'
;
element_spec
	: type_spec declarator
;
enum_type
	: 'enum' identifier '{' enumerator ( ',' enumerator)* '}'
;
enumerator
	: identifier
;
sequence_type
	: 'sequence' '<' simple_type_spec ',' positive_int_const '>'
	| 'sequence' '<' simple_type_spec '>'
;
string_type
	: 'string' '<' positive_int_const '>'
	| 'string'
;
wide_string_type
	: 'wstring' '<' positive_int_const '>'
	| 'wstring'
;
array_declarator
	: identifier fixed_array_size+
;
fixed_array_size
	: '[' positive_int_const ']'
;
attr_dcl
	: readonly_attr_spec
	| attr_spec
;
except_dcl
	: 'exception' identifier '{' member* '}'
;
op_dcl
	: op_attribute? op_type_spec identifier parameter_dcls raises_expr? context_expr?
;
op_attribute
	: 'oneway'
;
op_type_spec
	: param_type_spec
	| 'void'
;
parameter_dcls
	: '(' param_dcl (',' param_dcl)* ')'
	| '(' ')'
;
param_dcl
	: param_attribute param_type_spec simple_declarator
;
param_attribute
	: 'in'
	| 'out'
	| 'inout'
;
raises_expr
	: 'raises' '(' scoped_name (',' scoped_name)* ')'
;
context_expr
	: 'context' '(' string_literal (',' string_literal)* ')'
;
param_type_spec
	: base_type_spec
	| string_type
	| wide_string_type
	| scoped_name
;
fixed_pt_type
	: 'fixed' '<' positive_int_const ',' positive_int_const '>'
;
fixed_pt_const_type
	: 'fixed'
;
value_base_type
	: 'ValueBase'
;
constr_forward_decl
	: 'struct' identifier
	| 'union' identifier
;
import_
	: 'import' imported_scope ';'
;
imported_scope
	: scoped_name | string_literal
;
type_id_dcl
	: 'typeid' scoped_name string_literal
;
type_prefix_dcl
	: 'typeprefix' scoped_name string_literal
;
readonly_attr_spec
	: 'readonly' 'attribute' param_type_spec readonly_attr_declarator
;
readonly_attr_declarator
	: simple_declarator raises_expr
	| simple_declarator (',' simple_declarator)*
;
attr_spec
	: 'attribute' param_type_spec attr_declarator
;
attr_declarator
	: simple_declarator attr_raises_expr
	| simple_declarator (',' simple_declarator)*
;
attr_raises_expr
	: get_excep_expr set_excep_expr?
	| set_excep_expr
;
get_excep_expr
	: 'getraises' exception_list
;
set_excep_expr
	: 'setraises' exception_list
;
exception_list
	: '(' scoped_name (',' scoped_name) * ')'
;

/*NOTE: Grammar rules 1 through 111 with the exception of the last three lines of rule 2 constitutes the portion of IDL that
is not related to components.*/

component
	: component_dcl
	| component_forward_dcl
;
component_forward_dcl
	: 'component' identifier
;
component_dcl
	: component_header '{' component_body '}'
;
component_header
	: 'component' identifier component_inheritance_spec? supported_interface_spec?
;
supported_interface_spec
	: 'supports' scoped_name (',' scoped_name)*
;
component_inheritance_spec
	: ':' scoped_name
;
component_body
	: component_export*
;
component_export
	: provides_dcl ';'
	| uses_dcl ';'
	| emits_dcl ';'
	| publishes_dcl ';'
	| consumes_dcl ';'
	| attr_dcl ';'
;
provides_dcl
	: 'provides' interface_type identifier
;
interface_type
	: scoped_name
	| 'Object'
;
uses_dcl
	: 'uses' 'multiple'? interface_type identifier
;
emits_dcl
	: 'emits' scoped_name identifier
;
publishes_dcl
	: 'publishes' scoped_name identifier
;
consumes_dcl
	: 'consumes' scoped_name identifier
;
home_dcl
	: home_header home_body
;
home_header
	: 'home' identifier home_inheritance_spec? supported_interface_spec? 'manages' scoped_name primary_key_spec?
;
home_inheritance_spec
	: ':' scoped_name
;
primary_key_spec
	: 'primarykey' scoped_name
;
home_body
	: '{' home_export* '}'
;
home_export // TODO: home_export was trunkated!
	: export
	| factory_dcl ';'
	| finder_dcl ';'
;
factory_dcl
	: 'factory' identifier '(' init_param_decls? ')' raises_expr?
;
finder_dcl
	: 'finder' identifier '(' init_param_decls? ')' raises_expr?
;
event
	: ( event_dcl | event_abs_dcl | event_forward_dcl)
;
event_forward_dcl
	: 'abstract'? 'eventtype' identifier
;
event_abs_dcl // TODO identifier was trunkated
	: 'abstract' 'eventtype' identifier value_inheritance_spec? '{' export* '}'
;
event_dcl
	: event_header '{' value_element * '}'
;
event_header
	: 'custom'? 'eventtype' identifier value_inheritance_spec?
;


identifier: Identifier;

integer_literal: HexLiteral | DecimalLiteral | OctalLiteral;
string_literal: StringLiteral;
wide_string_literal: WideStringLiteral;
character_literal: CharacterLiteral;
wide_character_literal: WideCharacterLiteral;
fixed_pt_literal: FixedPointLiteral;
floating_pt_literal: FloatingPointLiteral;


// LEXER

HexLiteral : '0' ('x'|'X') HexDigit+;

DecimalLiteral : ('0' | '1'..'9' '0'..'9'*);

OctalLiteral : '0' ('0'..'7')+;

fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;

FloatingPointLiteral
    :   ('0'..'9')+ '.' ('0'..'9')* Exponent?
    |   '.' ('0'..'9')+ Exponent?
    |   ('0'..'9')+ Exponent
    |   ('0'..'9')+
;

FixedPointLiteral
    :   ('0'..'9')+ '.' ('0'..'9')* FixedPointSuffix
    |   '.' ('0'..'9')+ FixedPointSuffix
    |   ('0'..'9')+ FixedPointSuffix
;

fragment
Exponent : ('e'|'E') ('+'|'-')? ('0'..'9')+ ;

fragment
FixedPointSuffix : ('d'|'D') ;

CharacterLiteral
    :   '\'' ( EscapeSequence | ~('\''|'\\') ) '\''
;

WideCharacterLiteral
    :   'L' CharacterLiteral
;

StringLiteral
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"'
;

WideStringLiteral
    :  'L' StringLiteral
;

fragment
EscapeSequence
    :   '\\' ('n'|'t'|'v'|'b'|'r'|'f'|'a'|'\\'|'?'|'\''|'\"')
    |   OctalEscape
    |   HexEscape
    |   UnicodeEscape
;

fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
;

fragment
HexEscape
    :   '\\' 'x' HexDigit HexDigit
;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
;

Identifier 
    :   '_'* Letter (Letter|Digit|'_')*
;

fragment
Letter
    :  '\u0041'..'\u005a' |     // A-Z
       '\u0061'..'\u007a'       // a-z
;

fragment
Digit
    :  '\u0030'..'\u0039'       // 0-9
;

WS
    : [ \r\t\u000C\n]+ -> skip
;

COMMENT
    : '/*' .*? '*/' -> skip
;

LINE_COMMENT
    : '//' ~[\r\n]* ('\r'? '\n' | EOF) -> skip
;

// TODO: Do not ignore preprocessing instructions!
PREPROCESSING
    : '#' ~[\r\n]* ('\r'? '\n' | EOF) -> skip
;
