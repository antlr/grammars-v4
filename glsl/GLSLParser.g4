/*
MIT License

Copyright (c) 2022 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

parser grammar GLSLParser;
options { tokenVocab = GLSLLexer; }

translation_unit
	: external_declaration* EOF
	;
variable_identifier
	: IDENTIFIER
	;
primary_expression
	: variable_identifier
	| TRUE
	| FALSE
	| INTCONSTANT
	| UINTCONSTANT
	| FLOATCONSTANT
	| DOUBLECONSTANT
	| LEFT_PAREN expression RIGHT_PAREN
	;
postfix_expression
	: primary_expression
	| postfix_expression LEFT_BRACKET integer_expression RIGHT_BRACKET
	| postfix_expression LEFT_PAREN function_call_parameters? RIGHT_PAREN
	| type_specifier LEFT_PAREN function_call_parameters? RIGHT_PAREN
	| postfix_expression DOT field_selection
	| postfix_expression INC_OP
	| postfix_expression DEC_OP
	;
field_selection
	: variable_identifier
	| function_call
	;
integer_expression
	: expression
	;
function_call
	: function_identifier LEFT_PAREN function_call_parameters? RIGHT_PAREN
	;
function_identifier
	: type_specifier
	| postfix_expression
	;
function_call_parameters
	: assignment_expression (COMMA assignment_expression)*
	| VOID
	;
unary_expression
	: postfix_expression
	| INC_OP unary_expression
	| DEC_OP unary_expression
	| unary_operator unary_expression
	;
unary_operator
	: PLUS
	| DASH
	| BANG
	| TILDE
	;
assignment_expression
	: constant_expression
	| unary_expression assignment_operator assignment_expression
	;
assignment_operator
	: EQUAL
	| MUL_ASSIGN
	| DIV_ASSIGN
	| MOD_ASSIGN
	| ADD_ASSIGN
	| SUB_ASSIGN
	| LEFT_ASSIGN
	| RIGHT_ASSIGN
	| AND_ASSIGN
	| XOR_ASSIGN
	| OR_ASSIGN
	;
binary_expression
	: unary_expression
	| binary_expression (STAR | SLASH | PERCENT) binary_expression
	| binary_expression (PLUS | DASH) binary_expression
	| binary_expression (LEFT_OP | RIGHT_OP) binary_expression
	| binary_expression (LEFT_ANGLE | RIGHT_ANGLE | LE_OP | GE_OP) binary_expression
	| binary_expression (EQ_OP | NE_OP) binary_expression
	| binary_expression AMPERSAND binary_expression
	| binary_expression CARET binary_expression
	| binary_expression VERTICAL_BAR binary_expression
	| binary_expression AND_OP binary_expression
	| binary_expression XOR_OP binary_expression
	| binary_expression OR_OP binary_expression
	;
expression
	: assignment_expression
	| expression COMMA assignment_expression
	;
constant_expression
	: binary_expression
	| binary_expression QUESTION expression COLON assignment_expression
	;
declaration
	: function_prototype SEMICOLON
	| init_declarator_list SEMICOLON
	| PRECISION precision_qualifier type_specifier SEMICOLON
	| type_qualifier IDENTIFIER LEFT_BRACE struct_declaration_list RIGHT_BRACE (IDENTIFIER array_specifier?)? SEMICOLON
	| type_qualifier identifier_list? SEMICOLON
	;
identifier_list
	: IDENTIFIER (COMMA IDENTIFIER)*
	;
function_prototype
	: fully_specified_type IDENTIFIER LEFT_PAREN function_parameters? RIGHT_PAREN
	;
function_parameters
	: parameter_declaration (COMMA parameter_declaration)*
	;
parameter_declarator
	: type_specifier IDENTIFIER array_specifier?
	;
parameter_declaration
	: type_qualifier (parameter_declarator | parameter_type_specifier)
	| parameter_declarator
	| parameter_type_specifier
	;
parameter_type_specifier
	: type_specifier
	;
init_declarator_list
	: single_declaration (COMMA typeless_declaration)*
	;
single_declaration
	: fully_specified_type typeless_declaration?
	;
typeless_declaration
	: IDENTIFIER array_specifier? (EQUAL initializer)?
	;
fully_specified_type
	: type_specifier
	| type_qualifier type_specifier
	;
invariant_qualifier
	: INVARIANT
	;
interpolation_qualifier
	: SMOOTH
	| FLAT
	| NOPERSPECTIVE
	;
layout_qualifier
	: LAYOUT LEFT_PAREN layout_qualifier_id_list RIGHT_PAREN
	;
layout_qualifier_id_list
	: layout_qualifier_id (COMMA layout_qualifier_id)*
	;
layout_qualifier_id
	: IDENTIFIER (EQUAL constant_expression)?
	| SHARED
	;
precise_qualifier
	: PRECISE
	;
type_qualifier
	: single_type_qualifier+
	;
single_type_qualifier
	: storage_qualifier
	| layout_qualifier
	| precision_qualifier
	| interpolation_qualifier
	| invariant_qualifier
	| precise_qualifier
	;
storage_qualifier
	: CONST
	| IN
	| OUT
	| INOUT
	| CENTROID
	| PATCH
	| SAMPLE
	| UNIFORM
	| BUFFER
	| SHARED
	| COHERENT
	| VOLATILE
	| RESTRICT
	| READONLY
	| WRITEONLY
	| SUBROUTINE (LEFT_PAREN type_name_list RIGHT_PAREN)?
	| ATTRIBUTE
	| VARYING
	;
type_name_list
	: type_name (COMMA type_name)*
	;
type_name
	: IDENTIFIER
	;
type_specifier
	: type_specifier_nonarray array_specifier?
	;
array_specifier
	: dimension+
	;
dimension
	: LEFT_BRACKET constant_expression? RIGHT_BRACKET
	;
type_specifier_nonarray
	: VOID
	| FLOAT
	| DOUBLE
	| INT
	| UINT
	| BOOL
	| VEC2
	| VEC3
	| VEC4
	| DVEC2
	| DVEC3
	| DVEC4
	| BVEC2
	| BVEC3
	| BVEC4
	| IVEC2
	| IVEC3
	| IVEC4
	| UVEC2
	| UVEC3
	| UVEC4
	| MAT2
	| MAT3
	| MAT4
	| MAT2X2
	| MAT2X3
	| MAT2X4
	| MAT3X2
	| MAT3X3
	| MAT3X4
	| MAT4X2
	| MAT4X3
	| MAT4X4
	| DMAT2
	| DMAT3
	| DMAT4
	| DMAT2X2
	| DMAT2X3
	| DMAT2X4
	| DMAT3X2
	| DMAT3X3
	| DMAT3X4
	| DMAT4X2
	| DMAT4X3
	| DMAT4X4
	| ATOMIC_UINT
	| SAMPLER2D
	| SAMPLER3D
	| SAMPLERCUBE
	| SAMPLER2DSHADOW
	| SAMPLERCUBESHADOW
	| SAMPLER2DARRAY
	| SAMPLER2DARRAYSHADOW
	| SAMPLERCUBEARRAY
	| SAMPLERCUBEARRAYSHADOW
	| ISAMPLER2D
	| ISAMPLER3D
	| ISAMPLERCUBE
	| ISAMPLER2DARRAY
	| ISAMPLERCUBEARRAY
	| USAMPLER2D
	| USAMPLER3D
	| USAMPLERCUBE
	| USAMPLER2DARRAY
	| USAMPLERCUBEARRAY
	| SAMPLER1D
	| SAMPLER1DSHADOW
	| SAMPLER1DARRAY
	| SAMPLER1DARRAYSHADOW
	| ISAMPLER1D
	| ISAMPLER1DARRAY
	| USAMPLER1D
	| USAMPLER1DARRAY
	| SAMPLER2DRECT
	| SAMPLER2DRECTSHADOW
	| ISAMPLER2DRECT
	| USAMPLER2DRECT
	| SAMPLERBUFFER
	| ISAMPLERBUFFER
	| USAMPLERBUFFER
	| SAMPLER2DMS
	| ISAMPLER2DMS
	| USAMPLER2DMS
	| SAMPLER2DMSARRAY
	| ISAMPLER2DMSARRAY
	| USAMPLER2DMSARRAY
	| IMAGE2D
	| IIMAGE2D
	| UIMAGE2D
	| IMAGE3D
	| IIMAGE3D
	| UIMAGE3D
	| IMAGECUBE
	| IIMAGECUBE
	| UIMAGECUBE
	| IMAGEBUFFER
	| IIMAGEBUFFER
	| UIMAGEBUFFER
	| IMAGE1D
	| IIMAGE1D
	| UIMAGE1D
	| IMAGE1DARRAY
	| IIMAGE1DARRAY
	| UIMAGE1DARRAY
	| IMAGE2DRECT
	| IIMAGE2DRECT
	| UIMAGE2DRECT
	| IMAGE2DARRAY
	| IIMAGE2DARRAY
	| UIMAGE2DARRAY
	| IMAGECUBEARRAY
	| IIMAGECUBEARRAY
	| UIMAGECUBEARRAY
	| IMAGE2DMS
	| IIMAGE2DMS
	| UIMAGE2DMS
	| IMAGE2DMSARRAY
	| IIMAGE2DMSARRAY
	| UIMAGE2DMSARRAY
	| struct_specifier
	| type_name
	;
precision_qualifier
	: HIGHP
	| MEDIUMP
	| LOWP
	;
struct_specifier
	: STRUCT IDENTIFIER? LEFT_BRACE struct_declaration_list RIGHT_BRACE
	;
struct_declaration_list
	: struct_declaration+
	;
struct_declaration
	: type_specifier struct_declarator_list SEMICOLON
	| type_qualifier type_specifier struct_declarator_list SEMICOLON
	;
struct_declarator_list
	: struct_declarator (COMMA struct_declarator)*
	;
struct_declarator
	: IDENTIFIER array_specifier?
	;
initializer
	: assignment_expression
	| LEFT_BRACE initializer_list COMMA? RIGHT_BRACE
	;
initializer_list
	: initializer (COMMA initializer)*
	;
declaration_statement
	: declaration
	;
statement
	: compound_statement
	| simple_statement
	;
simple_statement
	: declaration_statement
	| expression_statement
	| selection_statement
	| switch_statement
	| case_label
	| iteration_statement
	| jump_statement
	;
compound_statement
	: LEFT_BRACE statement_list? RIGHT_BRACE
	;
statement_no_new_scope
	: compound_statement_no_new_scope
	| simple_statement
	;
compound_statement_no_new_scope
	: LEFT_BRACE statement_list? RIGHT_BRACE
	;
statement_list
	: statement+
	;
expression_statement
	: SEMICOLON
	| expression SEMICOLON
	;
selection_statement
	: IF LEFT_PAREN expression RIGHT_PAREN selection_rest_statement
	;
selection_rest_statement
	: statement (ELSE statement)?
	;
condition
	: expression
	| fully_specified_type IDENTIFIER EQUAL initializer
	;
switch_statement
	: SWITCH LEFT_PAREN expression RIGHT_PAREN LEFT_BRACE statement_list? RIGHT_BRACE
	;
case_label
	: CASE expression COLON
	| DEFAULT COLON
	;
iteration_statement
	: WHILE LEFT_PAREN condition RIGHT_PAREN statement_no_new_scope
	| DO statement WHILE LEFT_PAREN expression RIGHT_PAREN SEMICOLON
	| FOR LEFT_PAREN for_init_statement for_rest_statement RIGHT_PAREN statement_no_new_scope
	;
for_init_statement
	: expression_statement
	| declaration_statement
	;
for_rest_statement
	: condition? SEMICOLON expression?
	;
jump_statement
	: CONTINUE SEMICOLON
	| BREAK SEMICOLON
	| RETURN expression? SEMICOLON
	| DISCARD SEMICOLON
	;
external_declaration
	: function_definition
	| declaration
	| SEMICOLON
	;
function_definition
	: function_prototype compound_statement_no_new_scope
	;
