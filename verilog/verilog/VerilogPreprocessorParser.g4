// Author: Mustafa Said Ağca
// License: MIT

parser grammar VerilogPreprocessorParser;

options {
	tokenVocab = VerilogLexer;
}

// 19. Compiler directives
// START SYMBOL
compiler_directive
	: celldefine_compiler_directive
	| endcelldefine_compiler_directive
	| default_nettype_compiler_directive
	| text_macro_definition
	| text_macro_usage
	| undefine_compiler_directive
	| ifdef_directive
	| ifndef_directive
	| include_compiler_directive
	| resetall_compiler_directive
	| line_compiler_directive
	| timescale_compiler_directive
	| unconnected_drive_compiler_directive
	| nounconnected_drive_compiler_directive
	| pragma
	| keywords_directive
	| endkeywords_directive
	;

// 19.1 `celldefine and `endcelldefine

celldefine_compiler_directive
	: GRAVE_CELLDEFINE
	;

endcelldefine_compiler_directive
	: GRAVE_ENDCELLDEFINE
	;

// 19.2 `default_nettype

default_nettype_compiler_directive
	: GRAVE_DEFAULT_NETTYPE default_nettype_value
	;

default_nettype_value
	: WIRE
	| TRI
	| TRI0
	| TRI1
	| WAND
	| TRIAND
	| WOR
	| TRIOR
	| TRIREG
	| UWIRE
	| NONE
	;

// 19.3 `define and `undef
// 19.3.1 `define

text_macro_definition
	: GRAVE_DEFINE text_macro_name MACRO_TEXT
	;

text_macro_name
	: text_macro_identifier (LEFT_PARENTHESIS list_of_formal_arguments RIGHT_PARENTHESIS)?
	;

list_of_formal_arguments
	: formal_argument_identifier (COMMA formal_argument_identifier)*
	;

formal_argument_identifier
	: SIMPLE_IDENTIFIER
	;

text_macro_usage
	: GRAVE_ACCENT text_macro_identifier (LEFT_PARENTHESIS LIST_OF_ACTUAL_ARGUMENTS RIGHT_PARENTHESIS)?
	;

// 19.3.2 `undef

undefine_compiler_directive
	: GRAVE_UNDEF text_macro_identifier
	;

// 19.4 `ifdef, `else, `elsif, `endif , `ifndef

ifdef_directive
	: GRAVE_IFDEF text_macro_identifier ifdef_group_of_lines (GRAVE_ELSIF text_macro_identifier elsif_group_of_lines)* (GRAVE_ELSE else_group_of_lines)? GRAVE_ENDIF
	;

ifndef_directive
	: GRAVE_IFNDEF text_macro_identifier ifndef_group_of_lines (GRAVE_ELSIF text_macro_identifier elsif_group_of_lines)* (GRAVE_ELSE else_group_of_lines)? GRAVE_ENDIF
	;

ifdef_group_of_lines
	: GROUP_OF_LINES
	;

ifndef_group_of_lines
	: GROUP_OF_LINES
	;

elsif_group_of_lines
	: GROUP_OF_LINES
	;

else_group_of_lines
	: GROUP_OF_LINES
	;

// 19.5 `include

include_compiler_directive
	: GRAVE_INCLUDE DOUBLE_QUOTE filename DOUBLE_QUOTE
	;

filename
	: STRING
	;

// 19.6 `resetall

resetall_compiler_directive
	: GRAVE_RESETALL
	;

// 19.7 `line

line_compiler_directive
	: GRAVE_LINE number STRING LEVEL
	;

// 19.8 `timescale

timescale_compiler_directive
	: GRAVE_TIMESCALE TIME_LITERAL SLASH TIME_LITERAL
	;

// 19.9 `unconnected_drive and `nounconnected_drive

unconnected_drive_compiler_directive
	: GRAVE_UNCONNECTED_DRIVE (PULL0 | PULL1)
	;

nounconnected_drive_compiler_directive
	: GRAVE_NOUNCONNECTED_DRIVE
	;

// 19.10 `pragma

pragma
	: GRAVE_PRAGMA pragma_name (pragma_expression (COMMA pragma_expression )*)?
	;

pragma_name
	: SIMPLE_IDENTIFIER
	;

pragma_expression
	: pragma_keyword
	| pragma_keyword EQUAL pragma_value
	| pragma_value
	;

pragma_value
	: LEFT_PARENTHESIS pragma_expression (COMMA pragma_expression)* RIGHT_PARENTHESIS
	| number
	| STRING
	| identifier
	;

pragma_keyword
	: SIMPLE_IDENTIFIER
	;

// 19.11 `begin_keywords, `end_keywords

keywords_directive
	: GRAVE_BEGIN_KEYWORDS DOUBLE_QUOTE VERSION_SPECIFIER DOUBLE_QUOTE
	;

endkeywords_directive
	: GRAVE_END_KEYWORDS
	;

// A.8.7 Numbers

number
	: DECIMAL_NUMBER
	| OCTAL_NUMBER
	| BINARY_NUMBER
	| HEX_NUMBER
	| REAL_NUMBER
	;

// A.9.3 Identifiers

identifier
	: SIMPLE_IDENTIFIER
	| ESCAPED_IDENTIFIER
	;

text_macro_identifier
	: identifier
	;
