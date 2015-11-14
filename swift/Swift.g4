/*
 * [The "BSD license"]
 *  Copyright (c) 2014 Terence Parr
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE
 *
 * Converted from Apple's doc, http://tinyurl.com/n8rkoue, to ANTLR's
 * meta-language.
 */
grammar Swift; // 2.1

top_level : (statement | expression)* EOF ;

// Statements

// GRAMMAR OF A STATEMENT

statement
 : expression ';'?
 | declaration ';'?
 | loop_statement ';'?
 | branch_statement ';'?
 | labeled_statement ';'?
 | control_transfer_statement ';'?
 | defer_statement ';'?
 | do_statement ';'?
 | compiler_control_statement
 ;

statements : statement+ ;

// GRAMMAR OF A LOOP STATEMENT

loop_statement : for_statement
 | for_in_statement
 | while_statement
 | repeat_while_statement
 ;

// GRAMMAR OF A FOR STATEMENT

for_statement
 : 'for' for_init? ';' expression? ';' expression? code_block
 | 'for' '(' for_init?';' expression? ';' expression? ')' code_block
 ;

for_init : variable_declaration | expression_list  ;

// GRAMMAR OF A FOR_IN STATEMENT

for_in_statement : 'for' 'case'? pattern 'in' expression where_clause? code_block  ;

// GRAMMAR OF A WHILE STATEMENT

while_statement : 'while' condition_clause code_block  ;

condition_clause : expression
 | expression ',' condition_list
 | condition_list
 | availability_condition ',' expression
 ;

condition_list : condition | condition ',' condition_list ;
condition : availability_condition | case_condition | optional_binding_condition ;
case_condition : 'case' pattern initializer where_clause? ;
optional_binding_condition : optional_binding_head optional_binding_continuation_list? where_clause? ;
optional_binding_head : 'let' pattern initializer | 'var' pattern initializer ;
optional_binding_continuation_list : optional_binding_continuation | optional_binding_continuation ',' optional_binding_continuation_list ;
optional_binding_continuation : pattern initializer | optional_binding_head ;


// GRAMMAR OF A REPEAT-WHILE STATEMENT

repeat_while_statement : 'repeat' code_block 'while' expression ;

// GRAMMAR OF A BRANCH STATEMENT

branch_statement : if_statement
 | guard_statement
 | switch_statement
 ;

// GRAMMAR OF AN IF STATEMENT

if_statement : 'if' condition_clause code_block else_clause? ;
else_clause : 'else' code_block | 'else' if_statement  ;

// GRAMMAR OF A GUARD STATEMENT

guard_statement : 'guard' condition_clause 'else' code_block ;

// GRAMMAR OF A SWITCH STATEMENT

switch_statement : 'switch' expression '{' switch_cases? '}'  ;
switch_cases : switch_case switch_cases? ;
switch_case : case_label statements | default_label statements  ;
case_label : 'case' case_item_list ':' ;
case_item_list : pattern where_clause? | pattern where_clause? ',' case_item_list  ;
default_label : 'default' ':' ;
where_clause : 'where' where_expression ;
where_expression : expression ;

// GRAMMAR OF A LABELED STATEMENT

labeled_statement : statement_label loop_statement | statement_label if_statement | statement_label switch_statement ;
statement_label : label_name ':' ;
label_name : identifier  ;

// GRAMMAR OF A CONTROL TRANSFER STATEMENT

control_transfer_statement : break_statement
 | continue_statement
 | fallthrough_statement
 | return_statement
 | throw_statement
 ;

// GRAMMAR OF A BREAK STATEMENT

break_statement : 'break' label_name? ;

// GRAMMAR OF A CONTINUE STATEMENT

continue_statement : 'continue' label_name? ;

// GRAMMAR OF A FALLTHROUGH STATEMENT

fallthrough_statement : 'fallthrough'  ;

// GRAMMAR OF A RETURN STATEMENT

return_statement : 'return' expression? ;


// GRAMMAR OF AN AVAILABILITY CONDITION

availability_condition : '#available' '(' availability_arguments ')' ;
availability_arguments : availability_argument | availability_argument ',' availability_arguments ;
availability_argument : platform_name platform_version
 | '*'
 ;

platform_name : 'iOS' | 'iOSApplicationExtension'
 | 'OSX' | 'OSXApplicationExtension'
 | 'watchOS'
 ;

platform_version : Decimal_digits // TODO not sure what to do about this error "Implicit definitionsof token Decimal_digits in parser"
 | Decimal_digits '.' Decimal_digits
 | Decimal_digits '.' Decimal_digits '.' Decimal_digits
 ;

// GRAMMAR OF A THROW STATEMENT
throw_statement : 'throw' expression ;

// GRAMMAR OF A DEFER STATEMENT

defer_statement : 'defer' code_block ;

// GRAMMAR OF A DO STATEMENT

do_statement : 'do' code_block catch_clauses? ;
catch_clauses : catch_clause catch_clauses? ;
catch_clause : 'catch' pattern? where_clause? code_block ;

// GRAMMAR OF A COMPILER CONTROL STATEMENT

compiler_control_statement : build_configuration_statement
 | line_control_statement
 ;

// GRAMMAR OF A BUILD CONFIGURATION STATEMENT

build_configuration_statement: '#if' build_configuration statements? build_configuration_elseif_clauses? build_configuration_else_clause? '#endif' ;
build_configuration_elseif_clauses : build_configuration_elseif_clause build_configuration_elseif_clauses? ;
build_configuration_elseif_clause : '#elseif' build_configuration statements? ;
build_configuration_else_clause : '#else' statements? ;

build_configuration : platform_testing_function
 | identifier
 | boolean_literal
 | '(' build_configuration ')'
 | '!' build_configuration
 | build_configuration '&&' build_configuration
 | build_configuration '||' build_configuration
 ;

platform_testing_function : 'os' '(' operating_system ')'
 | 'arch' '(' architecture ')'
 ;

operating_system : 'OSX' | 'iOS' | 'watchOS' | 'tvOS' ;
architecture : 'i386' | 'x86_64' | 'arm' | 'arm64' ;

// GRAMMAR OF A LINE CONTROL STATEMENT

line_control_statement : '#line'
 | '#line' line_number file_name
 ;

line_number : integer_literal ;
file_name : Static_string_literal ;

// Generic Parameters and Arguments

// GRAMMAR OF A GENERIC PARAMETER CLAUSE

generic_parameter_clause : '<' generic_parameter_list requirement_clause? '>'  ;
generic_parameter_list : generic_parameter | generic_parameter ',' generic_parameter_list  ;
generic_parameter : type_name | type_name ':' type_identifier | type_name ':' protocol_composition_type  ;

requirement_clause : 'where' requirement_list  ;
requirement_list : requirement | requirement ',' requirement_list  ;
requirement : conformance_requirement | same_type_requirement  ;

conformance_requirement : type_identifier ':' type_identifier | type_identifier ':' protocol_composition_type  ;
same_type_requirement : type_identifier '==' type  ;

// GRAMMAR OF A GENERIC ARGUMENT CLAUSE

generic_argument_clause : '<' generic_argument_list '>'  ;
generic_argument_list : generic_argument | generic_argument ',' generic_argument_list ;
generic_argument : type  ;

// Declarations

// GRAMMAR OF A DECLARATION

declaration
 : import_declaration
 | constant_declaration
 | variable_declaration
 | typealias_declaration
 | function_declaration
 | enum_declaration
 | struct_declaration
 | class_declaration
 | protocol_declaration
 | initializer_declaration
 | deinitializer_declaration
 | extension_declaration
 | subscript_declaration
 | operator_declaration
 ;

declarations : declaration declarations? ;


// GRAMMAR OF A TOP-LEVEL DECLARATION

//top_level_declaration : statements? ;

// GRAMMAR OF A CODE BLOCK

code_block : '{' statements? '}' ;

// GRAMMAR OF AN IMPORT DECLARATION

import_declaration : attributes? 'import' import_kind? import_path  ;
import_kind : 'typealias' | 'struct' | 'class' | 'enum' | 'protocol' | 'var' | 'func'  ;
import_path : import_path_identifier | import_path_identifier '.' import_path  ;
import_path_identifier : identifier | operator  ;

// GRAMMAR OF A CONSTANT DECLARATION

constant_declaration : attributes? declaration_modifiers? 'let' pattern_initializer_list  ;
pattern_initializer_list : pattern_initializer | pattern_initializer ',' pattern_initializer_list ;
pattern_initializer : pattern initializer? ;
initializer : '=' expression  ;

// GRAMMAR OF A VARIABLE DECLARATION

variable_declaration
 : variable_declaration_head pattern_initializer_list
 | variable_declaration_head variable_name type_annotation code_block
 | variable_declaration_head variable_name type_annotation getter_setter_block
 | variable_declaration_head variable_name type_annotation getter_setter_keyword_block
 | variable_declaration_head variable_name type_annotation initializer? willSet_didSet_block
 | variable_declaration_head variable_name type_annotation type_annotation initializer? willSet_didSet_block
 ;

variable_declaration_head : attributes? declaration_modifiers? 'var'  ;
variable_name : identifier  ;

getter_setter_block : '{' getter_clause setter_clause?'}'  | '{' setter_clause getter_clause '}'  ;
getter_clause : attributes? 'get' code_block  ;
setter_clause : attributes? 'set' setter_name? code_block  ;
setter_name : '(' identifier ')'  ;

getter_setter_keyword_block : '{' getter_keyword_clause setter_keyword_clause?'}' | '{' setter_keyword_clause getter_keyword_clause '}'  ;
getter_keyword_clause : attributes? 'get'  ;
setter_keyword_clause : attributes? 'set'  ;

willSet_didSet_block : '{' willSet_clause didSet_clause?'}' | '{' didSet_clause willSet_clause '}'  ;
willSet_clause : attributes? 'willSet' setter_name? code_block  ;
didSet_clause : attributes? 'didSet' setter_name? code_block  ;

// GRAMMAR OF A TYPE ALIAS DECLARATION

typealias_declaration : typealias_head typealias_assignment  ;
typealias_head : attributes? access_level_modifier? 'typealias' typealias_name  ;
typealias_name : identifier  ;
typealias_assignment : '=' type  ;

// GRAMMAR OF A FUNCTION DECLARATION
// NOTE: Swift Grammar Spec indicates that a function_body is optional
function_declaration : function_head function_name generic_parameter_clause? function_signature function_body?  ;
function_head : attributes? declaration_modifiers? 'func'  ;
function_name : identifier |  operator  ;
function_signature : parameter_clauses 'throws'? function_result?
 | parameter_clauses 'rethrows' function_result?
 ;
function_result : '->' attributes? type  ;
function_body : code_block  ;
parameter_clauses : parameter_clause parameter_clauses? ;
parameter_clause : '(' ')' |  '(' parameter_list ')'  ;
parameter_list : parameter | parameter ',' parameter_list  ;
parameter : 'let'? external_parameter_name? local_parameter_name type_annotation default_argument_clause?
 | 'var' external_parameter_name? local_parameter_name type_annotation default_argument_clause?
 | 'inout' external_parameter_name? local_parameter_name type_annotation
 | external_parameter_name? local_parameter_name type_annotation '...'
 ;
external_parameter_name : identifier | '_'  ;
local_parameter_name : identifier | '_'  ;
default_argument_clause : '=' expression  ;


// GRAMMAR OF AN ENUMERATION DECLARATION

enum_declaration : attributes? access_level_modifier? union_style_enum | attributes? access_level_modifier? raw_value_style_enum  ;
union_style_enum : 'indirect'? 'enum' enum_name generic_parameter_clause? type_inheritance_clause? '{' union_style_enum_members?'}' ;
union_style_enum_members : union_style_enum_member union_style_enum_members? ;
union_style_enum_member : declaration | union_style_enum_case_clause  ;
union_style_enum_case_clause : attributes? 'indirect'? 'case' union_style_enum_case_list  ;
union_style_enum_case_list : union_style_enum_case | union_style_enum_case ',' union_style_enum_case_list  ;
union_style_enum_case : enum_case_name tuple_type? ;
enum_name : identifier  ;
enum_case_name : identifier  ;
raw_value_style_enum : 'enum' enum_name generic_parameter_clause? type_inheritance_clause '{' raw_value_style_enum_members '}' ;
raw_value_style_enum_members : raw_value_style_enum_member raw_value_style_enum_members? ;
raw_value_style_enum_member : declaration | raw_value_style_enum_case_clause  ;
raw_value_style_enum_case_clause : attributes? 'case' raw_value_style_enum_case_list  ;
raw_value_style_enum_case_list : raw_value_style_enum_case | raw_value_style_enum_case ',' raw_value_style_enum_case_list  ;
raw_value_style_enum_case : enum_case_name raw_value_assignment? ;
raw_value_assignment : '=' raw_value_literal  ;
raw_value_literal : numeric_literal | Static_string_literal | boolean_literal ;

// GRAMMAR OF A STRUCTURE DECLARATION TODO did not update

struct_declaration : attributes? access_level_modifier? 'struct' struct_name generic_parameter_clause? type_inheritance_clause? struct_body  ;
struct_name : identifier  ;
struct_body : '{' declarations?'}'  ;

// GRAMMAR OF A CLASS DECLARATION

class_declaration : attributes? access_level_modifier? 'class' class_name generic_parameter_clause? type_inheritance_clause? class_body  ;
class_name : identifier ;
class_body : '{' declarations? '}'  ;

// GRAMMAR OF A PROTOCOL DECLARATION

protocol_declaration : attributes? access_level_modifier? 'protocol' protocol_name type_inheritance_clause? protocol_body  ;
protocol_name : identifier  ;
protocol_body : '{' protocol_member_declarations?'}'  ;
protocol_member_declaration : protocol_property_declaration
 | protocol_method_declaration
 | protocol_initializer_declaration
 | protocol_subscript_declaration
 | protocol_associated_type_declaration
 ;
protocol_member_declarations : protocol_member_declaration protocol_member_declarations? ;

// GRAMMAR OF A PROTOCOL PROPERTY DECLARATION

protocol_property_declaration : variable_declaration_head variable_name type_annotation getter_setter_keyword_block  ;

// GRAMMAR OF A PROTOCOL METHOD DECLARATION

protocol_method_declaration : function_head function_name generic_parameter_clause? function_signature  ;

// GRAMMAR OF A PROTOCOL INITIALIZER DECLARATION

protocol_initializer_declaration
 : initializer_head generic_parameter_clause? parameter_clause 'throws'?
 | initializer_head generic_parameter_clause? parameter_clause 'rethrows'
 ;

// GRAMMAR OF A PROTOCOL SUBSCRIPT DECLARATION

protocol_subscript_declaration : subscript_head subscript_result getter_setter_keyword_block  ;

// GRAMMAR OF A PROTOCOL ASSOCIATED TYPE DECLARATION

protocol_associated_type_declaration : typealias_head type_inheritance_clause? typealias_assignment? ;

// GRAMMAR OF AN INITIALIZER DECLARATION

initializer_declaration
 : initializer_head generic_parameter_clause? parameter_clause 'throws'? initializer_body
 | initializer_head generic_parameter_clause? parameter_clause 'rethrows' initializer_body
 ;

initializer_head
 : attributes? declaration_modifiers? 'init'
 | attributes? declaration_modifiers? 'init' '?'
 | attributes? declaration_modifiers? 'init' '!'
 ;

initializer_body : code_block  ;

// GRAMMAR OF A DEINITIALIZER DECLARATION

deinitializer_declaration : attributes? 'deinit' code_block  ;

// GRAMMAR OF AN EXTENSION DECLARATION

extension_declaration : access_level_modifier? 'extension' type_identifier type_inheritance_clause? extension_body  ;
extension_body : '{' declarations?'}'  ;

// GRAMMAR OF A SUBSCRIPT DECLARATION

subscript_declaration
 : subscript_head subscript_result code_block
 | subscript_head subscript_result getter_setter_block
 | subscript_head subscript_result getter_setter_keyword_block
 ;

subscript_head : attributes? declaration_modifiers? 'subscript' parameter_clause  ;
subscript_result : '->' attributes? type  ;

// GRAMMAR OF AN OPERATOR DECLARATION

operator_declaration : prefix_operator_declaration | postfix_operator_declaration | infix_operator_declaration  ;
prefix_operator_declaration : 'prefix' 'operator' operator '{' '}'  ;
postfix_operator_declaration : 'postfix' 'operator' operator '{' '}'  ;
infix_operator_declaration : 'infix' 'operator' operator '{' infix_operator_attributes '}'  ; // NOTE used to be infix_operator_attributes?
infix_operator_attributes : precedence_clause? associativity_clause? ;
precedence_clause : 'precedence' precedence_level ;
precedence_level : integer_literal ;
associativity_clause : 'associativity' associativity ;
associativity : 'left' | 'right' | 'none' ;

// GRAMMAR OF A DECLARATION MODIFIER
declaration_modifier
 : 'class' | 'convenience' | 'dynamic' | 'final' | 'infix' | 'lazy' | 'mutating' | 'nonmutating' | 'optional' | 'override' | 'postfix' | 'prefix' | 'required' | 'static' | 'unowned' | 'unowned' '(' 'safe' ')' | 'unowned' '(' 'unsafe' ')' | 'weak'
 | access_level_modifier
 ;
declaration_modifiers : declaration_modifier declaration_modifiers? ;

access_level_modifier
 : 'internal' | 'internal' '(' 'set' ')'
 | 'private' | 'private' '(' 'set' ')'
 | 'public' | 'public' '(' 'set' ')'
 ;

// Patterns

// GRAMMAR OF A PATTERN

pattern
 : wildcard_pattern type_annotation?
 | identifier_pattern type_annotation?
 | value_binding_pattern
 | tuple_pattern type_annotation?
 | enum_case_pattern
 | optional_pattern
 | 'is' type
 | pattern 'as' type
 | expression_pattern
 ;

// GRAMMAR OF A WILDCARD PATTERN

wildcard_pattern : '_'  ;

// GRAMMAR OF AN IDENTIFIER PATTERN

identifier_pattern : identifier  ;

// GRAMMAR OF A VALUE_BINDING PATTERN

value_binding_pattern : 'var' pattern | 'let' pattern  ;

// GRAMMAR OF A TUPLE PATTERN

tuple_pattern : '(' tuple_pattern_element_list? ')'  ;
tuple_pattern_element_list : tuple_pattern_element | tuple_pattern_element ',' tuple_pattern_element_list ;
tuple_pattern_element : pattern  ;

// GRAMMAR OF AN ENUMERATION CASE PATTERN

enum_case_pattern : type_identifier? '.' enum_case_name tuple_pattern? ;

// GRAMMAR OF AN OPTIONAL PATTERN
optional_pattern : identifier_pattern '?' ;

// GRAMMAR OF A TYPE CASTING PATTERN
// ** NOTE ** I tried including these into the above pattern but was getting recursive errors

//type_casting_pattern : 'is' type | pattern 'as' type  ;
//is_pattern : 'is' type  ;
//as_pattern : pattern 'as' type  ;

// GRAMMAR OF AN EXPRESSION PATTERN

expression_pattern : expression  ;

// Attributes

// GRAMMAR OF AN ATTRIBUTE

attribute : '@' attribute_name attribute_argument_clause? ;
attribute_name : identifier  ;
attribute_argument_clause : '('  balanced_tokens?  ')'  ;
attributes : attribute attributes? ;
balanced_tokens : balanced_token balanced_tokens? ;
balanced_token
 : '('  balanced_tokens? ')'
 | '[' balanced_tokens? ']'
 | '{' balanced_tokens? '}'
 | identifier | expression | context_sensitive_keyword | literal | operator
// | Any punctuation except ( ,  ')' , '[' , ']' , { , or } TODO add?
 ;

// Expressions

// GRAMMAR OF AN EXPRESSION
expression : try_operator? prefix_expression binary_expressions? ;

//expression
//: try_operator? prefix_expression
//| try_operator? postfix_expression
//| try_operator? binary_expression
//;

expression_list : expression | expression ',' expression_list ;

// GRAMMAR OF A PREFIX EXPRESSION

prefix_expression
  : prefix_operator? postfix_expression
  | in_out_expression
  ;

in_out_expression : '&' identifier ;

// GRAMMAR OF A TRY EXPRESSION

try_operator : 'try' | 'try' '?' | 'try' '!' ;

// GRAMMAR OF A BINARY EXPRESSION


binary_expression
  : binary_operator prefix_expression
  | assignment_operator try_operator? prefix_expression
  | conditional_operator try_operator? prefix_expression
  | type_casting_operator
  ;

binary_expressions : binary_expression binary_expressions? ;

// GRAMMAR OF AN ASSIGNMENT OPERATOR

assignment_operator : '='  ;

// GRAMMAR OF A CONDITIONAL OPERATOR

conditional_operator : '?' try_operator? expression ':' ;

// GRAMMAR OF A TYPE_CASTING OPERATOR

type_casting_operator
  : 'is' type
  | 'as' type
  | 'as' '?' type
  | 'as' '!' type
  ;

// GRAMMAR OF A PRIMARY EXPRESSION

primary_expression
 : identifier generic_argument_clause?
 | literal_expression
 | self_expression
 | superclass_expression
 | closure_expression
 | parenthesized_expression
// | implicit_member_expression disallow as ambig with explicit member expr in postfix_expression
 | wildcard_expression
 ;

// GRAMMAR OF A LITERAL EXPRESSION

literal_expression
 : literal
 | array_literal | dictionary_literal
 | '__FILE__' | '__LINE__' | '__COLUMN__' | '__FUNCTION__'
 ;

array_literal : '[' array_literal_items? ']'  ;
array_literal_items : array_literal_item ','? | array_literal_item ',' array_literal_items  ;
array_literal_item : expression ;
dictionary_literal : '[' dictionary_literal_items ']' | '[' ':' ']'  ;
dictionary_literal_items : dictionary_literal_item ','? | dictionary_literal_item ',' dictionary_literal_items ;
dictionary_literal_item : expression ':' expression  ;

// GRAMMAR OF A SELF EXPRESSION

self_expression
 : 'self'
 | 'self' '.' identifier
 | 'self' '[' expression_list ']'
 | 'self' '.' 'init'
 ;

// GRAMMAR OF A SUPERCLASS EXPRESSION

superclass_expression
  : superclass_method_expression
  | superclass_subscript_expression
  | superclass_initializer_expression
  ;

superclass_method_expression : 'super' '.' identifier  ;
superclass_subscript_expression : 'super' '[' expression ']'  ;
superclass_initializer_expression : 'super' '.' 'init'  ;

// GRAMMAR OF A CLOSURE EXPRESSION

closure_expression : '{' closure_signature? statements '}'  ;
closure_signature
 : parameter_clause function_result? 'in'
 | identifier_list function_result? 'in'
 | capture_list parameter_clause function_result? 'in'
 | capture_list identifier_list function_result? 'in'
 | capture_list 'in'
 ;

capture_list : '[' capture_list_items ']'  ;
capture_list_items : capture_list_item | capture_list_item ',' capture_list_items ;
capture_list_item : capture_specifier? expression ;

capture_specifier : 'weak' | 'unowned' | 'unowned(safe)' | 'unowned(unsafe)'  ;

// GRAMMAR OF A IMPLICIT MEMBER EXPRESSION

//implicit_member_expression : '.' identifier  ; NOTE this was causing recursion issues when included in primary_expression

// GRAMMAR OF A PARENTHESIZED EXPRESSION

parenthesized_expression : '(' expression_element_list? ')'  ;
expression_element_list : expression_element | expression_element ',' expression_element_list  ;
expression_element : expression | identifier ':' expression  ;

// GRAMMAR OF A WILDCARD EXPRESSION

wildcard_expression : '_'  ;

// GRAMMAR OF A POSTFIX EXPRESSION

postfix_expression
 : primary_expression
 | postfix_expression postfix_operator
 | postfix_expression parenthesized_expression
 | postfix_expression parenthesized_expression? trailing_closure
 | postfix_expression '.' 'init'
 | postfix_expression '.' Decimal_digits // TODO: don't allow '_' here in Decimal_literal
 | postfix_expression '.' identifier generic_argument_clause?
 | postfix_expression '.' 'self'
 | postfix_expression '.' 'dynamicType'
 | postfix_expression '[' expression_list ']'
 | postfix_expression '!'
 | postfix_expression '?'
 ;

// GRAMMAR OF A FUNCTION CALL EXPRESSION ** Note - commented out expressions caused recursive issues in postfix_expression

//function_call_expression
//  : postfix_expression parenthesized_expression
//  | postfix_expression parenthesized_expression? trailing_closure
//  ;

trailing_closure : closure_expression ;

//initializer_expression : postfix_expression '.' 'init' ;

//explicit_member_expression
//  : postfix_expression '.' Decimal_literal // TODO: don't allow '_' here in Decimal_literal
//  | postfix_expression '.' identifier generic_argument_clause?
//  ;

//postfix_self_expression : postfix_expression '.' 'self' ;

// GRAMMAR OF A DYNAMIC TYPE EXPRESSION

//dynamic_type_expression : postfix_expression '.' 'dynamicType' ;

// GRAMMAR OF A SUBSCRIPT EXPRESSION

//subscript_expression : postfix_expression '[' expression_list ']' ;

// GRAMMAR OF A FORCED_VALUE EXPRESSION

//forced_value_expression : postfix_expression '!' ;

// GRAMMAR OF AN OPTIONAL_CHAINING EXPRESSION

//optional_chaining_expression : postfix_expression '?' ;

// Types

// GRAMMAR OF A TYPE

type
 : '[' type ']'
 | '[' type ':' type ']'
 | type 'throws'? '->' type
 | type 'rethrows' '->' type
 | type_identifier
 | tuple_type
 | type '?'
 | type '!'
 | protocol_composition_type
 | type '.' 'Type'
 | type '.' 'Protocol'
 ;

// GRAMMAR OF A TYPE ANNOTATION

type_annotation : ':' attributes? type  ;

// GRAMMAR OF A TYPE IDENTIFIER

type_identifier
 : type_name generic_argument_clause?
 | type_name generic_argument_clause? '.' type_identifier
 ;

type_name : identifier ;

// GRAMMAR OF A TUPLE TYPE

tuple_type : '(' tuple_type_body? ')'  ;
tuple_type_body : tuple_type_element_list '...'? ;
tuple_type_element_list : tuple_type_element | tuple_type_element ',' tuple_type_element_list  ;
tuple_type_element : attributes? 'inout'? type | 'inout'? element_name type_annotation ;
element_name : identifier  ;

// GRAMMAR OF A FUNCTION TYPE

//function_type
// : type 'throws'? '->' type
// | type 'rethrows' '->' type
// ;

// GRAMMAR OF AN ARRAY TYPE
//array_type : '[' type ']' ;

// GRAMMAR OF A DICTIONARY TYPE
//dictionary_type : '[' type ':' type ']' ;

// GRAMMAR OF AN OPTIONAL TYPE
//optional_type : type '?' ;

// GRAMMAR OF AN IMPLICITLY UNWRAPPED OPTIONAL TYPE
//implicitly_unwrapped_optional_type : type '!' ;

// GRAMMAR OF A PROTOCOL COMPOSITION TYPE

protocol_composition_type : 'protocol' '<' protocol_identifier_list?'>'  ;
protocol_identifier_list : protocol_identifier | protocol_identifier ',' protocol_identifier_list  ;
protocol_identifier : type_identifier  ;

// GRAMMAR OF A METATYPE TYPE

//metatype_type : type '.' 'Type' | type '.' 'Protocol';

// GRAMMAR OF A TYPE INHERITANCE CLAUSE

type_inheritance_clause
 : ':' class_requirement ',' type_inheritance_list
 | ':' class_requirement
 | ':' type_inheritance_list
 ;

type_inheritance_list
: type_identifier
| type_identifier ',' type_inheritance_list
;

class_requirement : 'class' ;

// ---------- Lexical Structure -----------

// GRAMMAR OF AN IDENTIFIER

identifier : Identifier | context_sensitive_keyword ;


Identifier : Identifier_head Identifier_characters?
 | '`' Identifier_head Identifier_characters? '`'
 | Implicit_parameter_name
 ;

identifier_list : identifier | identifier ',' identifier_list  ;

fragment Identifier_head : [a-zA-Z_]
 | '\u00A8' | '\u00AA' | '\u00AD' | '\u00AF' | [\u00B2-\u00B5] | [\u00B7-\u00BA]
 | [\u00BC-\u00BE] | [\u00C0-\u00D6] | [\u00D8-\u00F6] | [\u00F8-\u00FF]
 | [\u0100-\u02FF] | [\u0370-\u167F] | [\u1681-\u180D] | [\u180F-\u1DBF]
 | [\u1E00-\u1FFF]
 | [\u200B-\u200D] | [\u202A-\u202E] | [\u203F-\u2040] | '\u2054' | [\u2060-\u206F]
 | [\u2070-\u20CF] | [\u2100-\u218F] | [\u2460-\u24FF] | [\u2776-\u2793]
 | [\u2C00-\u2DFF] | [\u2E80-\u2FFF]
 | [\u3004-\u3007] | [\u3021-\u302F] | [\u3031-\u303F] | [\u3040-\uD7FF]
 | [\uF900-\uFD3D] | [\uFD40-\uFDCF] | [\uFDF0-\uFE1F] | [\uFE30-\uFE44]
 | [\uFE47-\uFFFD]
/*
 | U+10000–U+1FFFD | U+20000–U+2FFFD | U+30000–U+3FFFD | U+40000–U+4FFFD
 | U+50000–U+5FFFD | U+60000–U+6FFFD | U+70000–U+7FFFD | U+80000–U+8FFFD
 | U+90000–U+9FFFD | U+A0000–U+AFFFD | U+B0000–U+BFFFD | U+C0000–U+CFFFD
 | U+D0000–U+DFFFD or U+E0000–U+EFFFD
*/
 ;

fragment Identifier_character : [0-9]
 | [\u0300-\u036F] | [\u1DC0-\u1DFF] | [\u20D0-\u20FF] | [\uFE20-\uFE2F]
 | Identifier_head
 ;


context_sensitive_keyword :
 'associativity' | 'didSet' | 'get' | 'infix' | 'inout' | 'left' | 'mutating' | 'none' |
 'nonmutating' | 'operator' | 'override' | 'postfix' | 'precedence' | 'prefix' | 'right' |
 'set' | 'unowned' | 'unowned(safe)' | 'unowned(unsafe)' | 'weak' | 'willSet'
 ;

// GRAMMAR OF OPERATORS

// split the operators out into the individual tokens as some of those tokens
// are also referenced individually. For example, type signatures use
// <...>.
//operator
//  : Operator_head Operator_character*
//  | '..' ('.'|Operator_character)*
//  ;

operator : Operator ;

Operator
  : Operator_head Operator_character*
  | '..' ('.'|Operator_character)*
  ;

Operator_head
  : '/' | '=' | '\\' | '-' | '+' | '!' | '*' | '%' | '<' | '>' | '&' | '|' | '^' | '!' | '.'
  | [\u00A1-\u00A7]
  | [\u00A9\u00AB\u00AC\u00AE]
  | [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
  | [\u2016-\u2017\u2020-\u2027]
  | [\u2030-\u203E]
  | [\u2041-\u2053]
  | [\u2055-\u205E]
  | [\u2190-\u23FF]
  | [\u2500-\u2775]
  | [\u2794-\u2BFF]
  | [\u2E00-\u2E7F]
  | [\u3001-\u3003]
  | [\u3008-\u3030]
  ;

Operator_character
  : Operator_head
  | [\u0300–\u036F]
  | [\u1DC0–\u1DFF]
  | [\u20D0–\u20FF]
  | [\uFE00–\uFE0F]
  | [\uFE20–\uFE2F]
  //| [\uE0100–\uE01EF]  ANTLR can't do >16bit char
  ;

Dot_operator_head : '..' ;

Operator_characters : Operator_character Operator_characters? ;

Dot_operator_character : '.' | Operator_character ;
Dot_operator_characters : Dot_operator_character Dot_operator_characters? ;


// WHITESPACE scariness:

/* http://tinyurl.com/oalzfus
"If an operator has no whitespace on the left but is followed
immediately by a dot (.), it is treated as a postfix unary
operator. As an example, the ++ operator in a++.b is treated as a
postfix unary operator (a++ . b rather than a ++ .b).  For the
purposes of these rules, the characters (, [, and { before an
operator, the characters ), ], and } after an operator, and the
characters ,, ;, and : are also considered whitespace.

There is one caveat to the rules above. If the ! or ? operator has no
whitespace on the left, it is treated as a postfix operator,
regardless of whether it has whitespace on the right. To use the ?
operator as syntactic sugar for the Optional type, it must not have
whitespace on the left. To use it in the conditional (? :) operator,
it must have whitespace around both sides."
 */

/**
 "If an operator has whitespace around both sides or around neither side,
 it is treated as a binary operator. As an example, the + operator in a+b
  and a + b is treated as a binary operator."
*/
binary_operator : operator ;

/**
 "If an operator has whitespace on the left side only, it is treated as a
 prefix unary operator. As an example, the ++ operator in a ++b is treated
 as a prefix unary operator."
*/
prefix_operator : operator ; // only if space on left but not right

/**
 "If an operator has whitespace on the right side only, it is treated as a
 postfix unary operator. As an example, the ++ operator in a++ b is treated
 as a postfix unary operator."
 */
postfix_operator : operator ;


fragment Identifier_characters : Identifier_character+ ;

Implicit_parameter_name : '$' Decimal_literal ; // TODO: don't allow '_' here

// GRAMMAR OF A LITERAL

literal : numeric_literal | String_literal | boolean_literal | nil_literal  ;

numeric_literal : '-'? integer_literal | '-'? Floating_point_literal ;
boolean_literal : 'true' | 'false' ;
nil_literal : 'nil' ;

// GRAMMAR OF AN INTEGER LITERAL

integer_literal
 : Binary_literal
 | Octal_literal
 | Decimal_literal
 | Hexadecimal_literal
 ;

Binary_literal : '0b' Binary_digit Binary_literal_characters? ;
fragment Binary_digit : [01] ;
fragment Binary_literal_character : Binary_digit | '_'  ;
fragment Binary_literal_characters : Binary_literal_character+ ;

Octal_literal : '0o' Octal_digit Octal_literal_characters? ;
fragment Octal_digit : [0-7] ;
fragment Octal_literal_character : Octal_digit | '_'  ;
fragment Octal_literal_characters : Octal_literal_character+ ;

Decimal_literal : Decimal_digit Decimal_literal_characters? ;
fragment Decimal_digit : [0-9] ;
fragment Decimal_digits : Decimal_digit+ ;
fragment Decimal_literal_character : Decimal_digit | '_'  ;
fragment Decimal_literal_characters : Decimal_literal_character+ ;

Hexadecimal_literal : '0x' Hexadecimal_digit Hexadecimal_literal_characters? ;
fragment Hexadecimal_digit : [0-9a-fA-F] ;
fragment Hexadecimal_literal_character : Hexadecimal_digit | '_'  ;
fragment Hexadecimal_literal_characters : Hexadecimal_literal_character+ ;

// GRAMMAR OF A FLOATING_POINT LITERAL

Floating_point_literal
 : Decimal_literal Decimal_fraction? Decimal_exponent?
 | Hexadecimal_literal Hexadecimal_fraction? Hexadecimal_exponent
 ;
fragment Decimal_fraction : '.' Decimal_literal ;
fragment Decimal_exponent : Floating_point_e Sign? Decimal_literal ;
fragment Hexadecimal_fraction : '.' Hexadecimal_digit Hexadecimal_literal_characters? ;
fragment Hexadecimal_exponent : Floating_point_p Sign? Decimal_literal ;
fragment Floating_point_e : 'e' | 'E' ;
fragment Floating_point_p : 'p' | 'P' ;
fragment Sign : '+' | '-' ;

// GRAMMAR OF A STRING LITERAL

String_literal : Static_string_literal /*| Interpolated_string_literal*/ ; // TODO add this in once interpolated strings work
Static_string_literal : '"' Quoted_text? '"' ;
fragment Quoted_text : Quoted_text_item Quoted_text? ;
fragment Quoted_text_item : Escaped_character
// | '\\(' expression ')'
 | ~["\\\u000A\u000D]
 ;

Escaped_character : '\\' [0\\tnr"']
 | '\\x' Hexadecimal_digit Hexadecimal_digit
 | '\\u' Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit
 | '\\U' Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit Hexadecimal_digit
;

//Interpolated_string_literal : '"' Interpolated_text? '"' ;
//fragment Interpolated_text : Interpolated_text_item Interpolated_text? ;
//fragment Interpolated_text_item : '\\(' expression ')' | Quoted_text_item ;
//fragment Interpolated_text_item : interpolated_expression | Quoted_text_item ;
//fragment interpolated_expression : '\\(' expression ')' ;

fragment SPACE : [ \n\r\t\u000B\u000C\u0000];

WS : SPACE+ -> channel(HIDDEN) ;

Block_comment : '/*' (Block_comment|.)*? '*/' -> channel(HIDDEN) ; // nesting allow

Line_comment : '//' .*? '\n' -> channel(HIDDEN) ;