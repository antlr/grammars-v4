/*
Odin Language grammar.
The MIT License (MIT).

Copyright (c) 2026, Michał Lorek.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

grammar odin;

// ─── Parser Rules ─────────────────────────────────────────────────────────────

unit
    : package_decl? import_list? top_decl* EOF
    ;

foreign_import_decl
    : FOREIGN IMPORT id? (STRING | RAW_STRING)
    ;

foreign_block
    : attribute_list? FOREIGN id '{' foreign_entry* '}'
    ;

foreign_entry
    : attribute_list? id '::' PROC calling_convention? '(' param_list? ')' ('->' return_type)? UNINIT ';'?
    ;

package_decl
    : PACKAGE id
    ;

import_list
    : import_item+
    ;

import_item
    : IMPORT id? STRING
    ;

// ─── Top-level declarations ───────────────────────────────────────────────────

top_decl
    : attribute_list? named_decl
    | attribute_list? var_decl
    | foreign_import_decl
    | attribute_list? foreign_block
    | when_stmt                    // top-level when
    ;

attribute_list
    : attribute+
    ;

attribute
    : '@' '(' attribute_elem (',' attribute_elem)* ')'
    ;

attribute_elem
    : id ('=' expr)?
    ;

named_decl
    : id '::' rhs
    | id ':' type ':' expr
    | id ':' type ':' proc_expr    // constant proc with explicit type
    ;

// Right-hand side of :: declarations (type aliases, consts, procs, composites)
rhs
    : proc_expr
    | proc_overload
    | DISTINCT type
    | struct_type
    | enum_type
    | union_type
    | bit_set_type
    | bit_field_type
    | matrix_type
    | expr
    ;

proc_overload
    : PROC '{' id (',' id)* '}'
    ;

var_decl
    : ids_list ':' type ('=' expr_list)?
    | ids_list ':=' expr_list
    ;

// ─── Types ────────────────────────────────────────────────────────────────────

type
    : basic_type
    | pointer_type
    | multi_pointer_type
    | array_type
    | slice_type
    | dynamic_array_type
    | map_type
    | proc_type
    | struct_type
    | enum_type
    | union_type
    | bit_set_type
    | bit_field_type
    | matrix_type
    | soa_array_type
    | sparse_array_type
    | DISTINCT type
    | '(' type ')'
    | poly_param ('/' type)?                 // $T or $T/Specialization used as a type
    | qualified_id ('(' type_arg_list ')')?  // named type, optionally parametric
    ;

// Parametric type argument list: Table(string, int)
type_arg_list
    : type_arg (',' type_arg)*
    ;

type_arg
    : type
    | expr
    ;

basic_type
    : BOOL | B8 | B16 | B32 | B64
    | INT | I8 | I16 | I32 | I64 | I128
    | UINT | U8 | U16 | U32 | U64 | U128 | UINTPTR
    | F16 | F32 | F64
    | COMPLEX32 | COMPLEX64 | COMPLEX128
    | QUATERNION64 | QUATERNION128 | QUATERNION256
    | RUNE | STRING_KW | CSTRING | RAWPTR | TYPEID | ANY
    ;

pointer_type
    : '^' type
    ;

multi_pointer_type
    : '[' '^' ']' type
    ;

array_type
    : '[' expr ']' type
    | '[' poly_param ']' type    // polymorphic dimension e.g. [$N]T
    | '[' '?' ']' type           // compiler-inferred length
    ;

slice_type
    : '[' ']' type
    ;

dynamic_array_type
    : '[' DYNAMIC ']' type
    | '[' DYNAMIC ';' expr ']' type    // with fixed backing capacity
    ;

map_type
    : MAP '[' type ']' type
    ;

proc_type
    : PROC calling_convention? '(' param_list? ')' ('->' return_type)?
    ;

calling_convention
    : STRING    // "c" | "contextless" | "stdcall" | "fastcall" | "none" | ...
    ;

param_list
    : param (',' param)*
    ;

param
    : ids_list ':' '..' type               // variadic
    | USING ids_list ':' type              // `using` parameter modifier
    | ids_list ':' type ('=' expr)?        // named, optional default
    | ids_list ':=' expr                   // named, type-inferred default
    | poly_param_decl                     // polymorphic $T: typeid  or  $N: int
    | type                                // unnamed
    ;

// A polymorphic parameter name prefixed with $
poly_param
    : DOLLAR id
    ;

return_type
    : type
    | '(' return_param (',' return_param)* ')'
    ;

return_param
    : ids_list ':' type    // named return
    | type
    ;

struct_type
    : STRUCT poly_param_list? struct_tag* '{' struct_field_list? '}'
    ;

// Optional polymorphic parameter list on struct/union: struct($Key, $Value: typeid)
poly_param_list
    : '(' poly_param_group (',' poly_param_group)* ')'
    ;

// A group shares one type: $Key, $Value: typeid
poly_param_group
    : poly_param (',' poly_param)* ':' type ('/' type)?
    ;

// Single poly param declaration (used in proc params)
poly_param_decl
    : poly_param ':' type ('/' type)?    // $T: typeid, $N: int, or $T: typeid/[]$E
    ;

struct_tag
    : HASH_ALIGN '(' expr ')'
    | HASH_MIN_FIELD_ALIGN '(' expr ')'
    | HASH_PACKED
    | HASH_RAW_UNION
    | HASH_SIMPLE
    | HASH_ALL_OR_NONE
    | WHERE where_condition_list
    ;

struct_field_list
    : struct_field (',' struct_field)* ','?
    ;

struct_field
    : USING? ids_list ':' type field_tag?
    ;

field_tag
    : STRING
    | RAW_STRING
    ;

enum_type
    : ENUM basic_type? '{' enum_body? '}'
    ;

enum_body
    : enum_field (',' enum_field)* ','?
    ;

enum_field
    : id ('=' expr)?
    ;

union_type
    : UNION poly_param_list? union_tag* '{' union_body? '}'
    ;

union_tag
    : HASH_NO_NIL
    | HASH_SHARED_NIL
    | HASH_MAYBE
    ;

union_body
    : type (',' type)* ','?
    ;

bit_set_type
    : BIT_SET '[' bit_set_elem (';' type)? ']'
    ;

bit_set_elem
    : expr '..' '=' expr    // inclusive range
    | expr '..' '<' expr    // half-open range
    | type                  // enum type
    ;

bit_field_type
    : BIT_FIELD type '{' bit_field_body? '}'
    ;

bit_field_body
    : bit_field_entry (',' bit_field_entry)* ','?
    ;

bit_field_entry
    : id ':' type '|' expr (WHEN expr ELSE expr)?
    ;

matrix_type
    : HASH_ROW_MAJOR? MATRIX '[' expr ',' expr ']' type
    ;

soa_array_type
    : HASH_SOA '[' expr ']' type
    | HASH_SOA '[' '?' ']' type
    | HASH_SOA '[' ']' type
    | HASH_SOA '[' DYNAMIC ']' type
    ;

sparse_array_type
    : HASH_SPARSE '[' type ']' type
    ;

// ─── Procedures ───────────────────────────────────────────────────────────────

proc_expr
    : PROC calling_convention? '(' param_list? ')' ('->' return_type)? where_clause? proc_body
    ;

where_clause
    : WHERE where_condition_list
    ;

where_condition_list
    : where_condition (',' where_condition)*
    ;

// A where condition is an expression, but allows `expr == type` and `expr != type`
// to support: `where type_of(x) == [2]int`
where_condition
    : expr ('==' | '!=') type
    | expr
    ;

proc_body
    : '{' stmt* '}'
    ;

// ─── Statements ───────────────────────────────────────────────────────────────

stmt
    : labeled_stmt
    | attribute_list? named_decl ';'?
    | attribute_list? var_decl ';'?
    | assign_stmt ';'?
    | if_stmt
    | for_stmt
    | switch_stmt
    | when_stmt
    | return_stmt ';'?
    | defer_stmt ';'?
    | using_stmt ';'?
    | break_stmt ';'?
    | continue_stmt ';'?
    | fallthrough_stmt ';'?
    | hash_assert_stmt ';'?
    | foreign_block
    | block
    | expr ';'?
    ;

hash_assert_stmt
    : HASH_ASSERT '(' where_condition ')'
    ;

labeled_stmt
    : id ':' (for_stmt | switch_stmt | block)
    ;

assign_stmt
    : expr_list assign_op expr_list
    ;

assign_op
    : '=' | '+=' | '-=' | '*=' | '/=' | '%=' | '%%='
    | '|=' | '~=' | '&=' | '&~=' | '<<=' | '>>='
    | '&&=' | '||='
    ;

if_stmt
    : IF (simple_stmt ';')? expr block (ELSE (if_stmt | block))?
    ;

simple_stmt
    : var_decl
    | assign_stmt
    | expr
    ;

for_stmt
    : for_directive? FOR block                                          // infinite
    | for_directive? FOR simple_stmt? ';' expr? ';' simple_stmt? block // C-style (init optional)
    | for_directive? FOR '&'? id (',' id)? IN expr block               // range with value[,index]
    | for_directive? FOR ids_list IN range_expr block                    // range into id_list
    | for_directive? FOR expr block                                     // while-condition
    ;

for_directive
    : HASH_REVERSE
    | HASH_UNROLL
    ;

range_expr
    : expr '..' '=' expr
    | expr '..' '<' expr
    | expr
    ;

switch_stmt
    : HASH_PARTIAL? SWITCH (simple_stmt ';')? expr? '{' switch_case* '}'          // normal switch
    | HASH_PARTIAL? SWITCH (simple_stmt ';')? id IN expr '{' switch_case* '}'     // type switch: `switch v in val`
    ;

switch_case
    : CASE switch_case_expr_list ':' stmt*
    | CASE ':'                   stmt*
    ;

// Switch case expressions may be types (for union/any switches), ranges, or regular exprs
switch_case_expr_list
    : switch_case_expr (',' switch_case_expr)*
    ;

switch_case_expr
    : expr '..' '=' expr    // inclusive range: 'A'..='Z', 0..=9
    | expr '..' '<' expr    // half-open range: 0..<10
    | type                  // union type-switch case: `case int:`, `case bool:`
    | expr
    ;

when_stmt
    : WHEN expr when_body (ELSE (when_stmt | when_body))?
    ;

// A when body may contain top-level declarations (e.g. foreign import) or regular statements
when_body
    : '{' (top_decl | stmt)* '}'
    ;

return_stmt
    : RETURN expr_list?
    ;

defer_stmt
    : DEFER stmt
    ;

using_stmt
    : USING expr    // `using entity`, `using entity.position`, etc.
    ;

break_stmt
    : BREAK id?
    ;

continue_stmt
    : CONTINUE id?
    ;

fallthrough_stmt
    : FALLTHROUGH
    ;

block
    : '{' stmt* '}'
    ;

// ─── Expressions ──────────────────────────────────────────────────────────────

expr
    : primary_expr
    | unary_op expr
    | CAST '(' type ')' expr
    | TRANSMUTE '(' type ')' expr
    | AUTO_CAST expr
    | expr binary_op expr
    | expr IF expr ELSE expr        // x if cond else y
    | expr WHEN expr ELSE expr      // compile-time conditional value
    | expr '?' expr ':' expr        // C-style ternary
    | expr OR_ELSE expr
    | expr OR_RETURN
    | expr OR_CONTINUE id?
    | expr OR_BREAK id?
    ;

unary_op
    : '+' | '-' | '~' | '!' | '^' | '&'
    ;

binary_op
    : '+' | '-' | '*' | '/' | '%' | '%%'
    | '&' | '|' | '~' | '&~' | '<<' | '>>'
    | '&&' | '||'
    | '==' | '!=' | '<' | '<=' | '>' | '>='
    | IN | NOT_IN
    ;

primary_expr
    : operand
    | primary_expr '.' id                            // field / package access
    | primary_expr '.' '(' type ')'                 // union type assertion
    | primary_expr '.' '?'                           // maybe/optional unwrap (.?)
    | primary_expr '[' expr ']'                     // index
    | primary_expr '[' expr ',' expr ']'            // matrix two-index: m[1, 2]
    | primary_expr '[' expr? ':' expr? ']'          // slice
    | primary_expr '[' expr? ':' expr? ':' expr ']' // slice with explicit cap
    | primary_expr '(' call_arg_list? ')'           // procedure call
    | primary_expr '^'                              // pointer dereference
    | primary_expr '{' literal_elem_list? '}'       // composite literal
    | HASH_ASSERT '(' where_condition ')'           // #assert(cond) or #assert(type_of(x) == T)
    | HASH_DEFINED '(' id ')'                       // #defined(sym)
    | HASH_PROCEDURE                                // #procedure built-in string
    | HASH_LOAD '(' expr ')'                        // #load("file")
    ;

operand
    : literal
    | qualified_id ('(' type_arg_list ')')?         // plain or parametric identifier
    | basic_type                                     // keyword-type used as value (int(...), u32(...))
    | '.' id                                        // implicit enum/selector expr
    | '(' expr ')'
    | '{' literal_elem_list? '}'                    // bare composite literal: {1, 2} or {x=1}
    | proc_expr
    | array_type         '{' literal_elem_list? '}' // [N]T{...} or [?]T{...}
    | slice_type         '{' literal_elem_list? '}' // []T{...}
    | dynamic_array_type '{' literal_elem_list? '}' // [dynamic]T{...}
    | map_type           '{' literal_elem_list? '}' // map[K]V{...}
    | struct_type        '{' literal_elem_list? '}' // struct{...}{...}
    | enum_type          '{' literal_elem_list? '}' // enum{...}{...} for enumerated arrays
    | soa_array_type     '{' literal_elem_list? '}' // #soa[N]T{...}
    | sparse_array_type  '{' literal_elem_list? '}' // #sparse[Foo]int{...}
    | matrix_type        '{' literal_elem_list? '}' // matrix[R,C]T{...}
    | bit_set_type       '{' literal_elem_list? '}' // bit_set[Foo]{...}
    ;

qualified_id
    : id ('.' id)?
    ;

call_arg_list
    : call_arg (',' call_arg)* ','?
    ;

call_arg
    : (id '=')? expr    // positional or named argument
    | '..' expr         // spread / expand
    ;

literal_elem_list
    : literal_elem (',' literal_elem)* ','?
    ;

literal_elem
    : literal_key '=' expr
    | expr
    | '{' literal_elem_list? '}'    // anonymous composite literal: {0, 3, 4}
    ;

literal_key
    : expr '..' '=' expr    // range key:  3..=5 = "Frog"
    | expr '..' '<' expr    // range key:  6..<8 = "Cat"
    | expr                  // id, string, integer, enum selector, etc.
    ;

literal
    : INTEGER
    | FLOAT
    | IMAG
    | STRING
    | RAW_STRING
    | CHAR
    | NIL
    | TRUE
    | FALSE
    | UNINIT
    ;

ids_list
    : id (',' id)*
    ;

expr_list
    : expr (',' expr)*
    ;

id
    : ID
    ;

// ─── Lexer Rules ──────────────────────────────────────────────────────────────

// Control-flow keywords
IF          : 'if';
ELSE        : 'else';
FOR         : 'for';
IN          : 'in';
NOT_IN      : 'not_in';
SWITCH      : 'switch';
CASE        : 'case';
WHEN        : 'when';
WHERE       : 'where';
RETURN      : 'return';
DEFER       : 'defer';
BREAK       : 'break';
CONTINUE    : 'continue';
FALLTHROUGH : 'fallthrough';

// Declaration keywords
PACKAGE     : 'package';
IMPORT      : 'import';
PROC        : 'proc';
STRUCT      : 'struct';
ENUM        : 'enum';
UNION       : 'union';
BIT_SET     : 'bit_set';
BIT_FIELD   : 'bit_field';
MATRIX      : 'matrix';
MAP         : 'map';
DYNAMIC     : 'dynamic';
DISTINCT    : 'distinct';
USING       : 'using';

// Cast / conversion keywords
CAST        : 'cast';
TRANSMUTE   : 'transmute';
AUTO_CAST   : 'auto_cast';

// Error-handling operators
OR_ELSE     : 'or_else';
OR_RETURN   : 'or_return';
OR_CONTINUE : 'or_continue';
OR_BREAK    : 'or_break';

// Boolean / nil literals
NIL         : 'nil';
TRUE        : 'true';
FALSE       : 'false';

// Built-in types (must appear before ID rule)
BOOL        : 'bool';
B8          : 'b8';
B16         : 'b16';
B32         : 'b32';
B64         : 'b64';
INT         : 'int';
I8          : 'i8';
I16         : 'i16';
I32         : 'i32';
I64         : 'i64';
I128        : 'i128';
UINT        : 'uint';
U8          : 'u8';
U16         : 'u16';
U32         : 'u32';
U64         : 'u64';
U128        : 'u128';
UINTPTR     : 'uintptr';
F16         : 'f16';
F32         : 'f32';
F64         : 'f64';
COMPLEX32   : 'complex32';
COMPLEX64   : 'complex64';
COMPLEX128  : 'complex128';
QUATERNION64  : 'quaternion64';
QUATERNION128 : 'quaternion128';
QUATERNION256 : 'quaternion256';
RUNE        : 'rune';
STRING_KW   : 'string';
CSTRING     : 'cstring';
RAWPTR      : 'rawptr';
TYPEID      : 'typeid';
ANY         : 'any';

// Hash-prefixed directives (longest match wins over bare HASH)
HASH_PARTIAL         : '#partial';
HASH_REVERSE         : '#reverse';
HASH_UNROLL          : '#unroll';
HASH_SOA             : '#soa';
HASH_ROW_MAJOR       : '#row_major';
HASH_ALIGN           : '#align';
HASH_PACKED          : '#packed';
HASH_RAW_UNION       : '#raw_union';
HASH_MIN_FIELD_ALIGN : '#min_field_align';
HASH_SIMPLE          : '#simple';
HASH_ALL_OR_NONE     : '#all_or_none';
HASH_NO_NIL          : '#no_nil';
HASH_SHARED_NIL      : '#shared_nil';
HASH_MAYBE           : '#maybe';
HASH_NO_BOUNDS_CHECK : '#no_bounds_check';
HASH_BOUNDS_CHECK    : '#bounds_check';
HASH_SPARSE          : '#sparse';
HASH_LOAD            : '#load';
HASH_ASSERT          : '#assert';
HASH_DEFINED         : '#defined';
HASH_PROCEDURE       : '#procedure';    // built-in that yields current procedure name
HASH                 : '#';

// Foreign system keyword
FOREIGN : 'foreign';

// Uninitialized marker
UNINIT : '---';

// Numeric literals
fragment DEC_DIGITS : [0-9] [0-9_]*;
fragment HEX_DIGITS : [0-9a-fA-F] [0-9a-fA-F_]*;
fragment OCT_DIGITS : [0-7] [0-7_]*;
fragment BIN_DIGITS : [01] [01_]*;
fragment DEC_EXP    : [eE] [+\-]? [0-9]+;
fragment HEX_EXP    : [pP] [+\-]? [0-9]+;

INTEGER
    : '0b' BIN_DIGITS
    | '0o' OCT_DIGITS
    | '0x' HEX_DIGITS
    | DEC_DIGITS
    ;

FLOAT
    : DEC_DIGITS '.' DEC_DIGITS DEC_EXP?
    | DEC_DIGITS DEC_EXP
    | '0x' HEX_DIGITS '.' HEX_DIGITS? HEX_EXP?
    ;

IMAG
    : DEC_DIGITS [ijk]
    | DEC_DIGITS '.' DEC_DIGITS DEC_EXP? [ijk]
    | DEC_DIGITS DEC_EXP [ijk]
    ;

// String / character literals
fragment ESCAPE_SEQ
    : '\\' [abefnrtv\\'"]
    | '\\' [0-7] [0-7]? [0-7]?
    | '\\x' HEX_DIGIT HEX_DIGIT
    | '\\u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    | '\\U' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    ;

fragment HEX_DIGIT : [0-9a-fA-F];

STRING
    : '"' (~["\\\r\n] | ESCAPE_SEQ)* '"'
    ;

RAW_STRING
    : '`' ~[`]* '`'
    ;

CHAR
    : '\'' (~['\\\r\n] | ESCAPE_SEQ) '\''
    ;

// Identifier (after all keyword rules so keywords take precedence)
ID : [a-zA-Z_] [a-zA-Z0-9_]*;

// Polymorphic parameter sigil
DOLLAR : '$';

// Whitespace and comments
SPACE         : [ \t\r\n]+                    -> channel(HIDDEN);
LINE_COMMENT  : '//' ~[\r\n]*                 -> skip;
BUILD_TAG     : '#+' ~[\r\n]*                 -> skip;
BLOCK_COMMENT : '/*' (BLOCK_COMMENT | .)*? '*/' -> skip;  // supports nesting
