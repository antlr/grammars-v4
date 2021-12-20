// Author: Mustafa Said Ağca
// License: MIT

parser grammar VerilogPreprocessorParser;

options { tokenVocab=VerilogLexer; }

// START SYMBOL
source_text
	: compiler_directive*
	;

// 19. Compiler directives

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
	//| pragma
	| keywords_directive
	| endkeywords_directive
	;

// 19.1 `celldefine and `endcelldefine

celldefine_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_CELLDEFINE
	;

endcelldefine_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_ENDCELLDEFINE
	;

// 19.2 `default_nettype

default_nettype_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_DEFAULT_NETTYPE default_nettype_value
	;

default_nettype_value
	: DEFAULT_NETTYPE_VALUE
	;

// 19.3 `define and `undef
// 19.3.1 `define

text_macro_definition
	: GRAVE_ACCENT DIRECTIVE_DEFINE text_macro_identifier (MACRO_TEXT | MACRO_TEXT_BACKSLASH_NEWLINE)*
	;

text_macro_usage
	: GRAVE_ACCENT text_macro_identifier macro_list_of_actual_arguments?
	;

macro_list_of_actual_arguments
	: DIRECTIVE_LIST_OF_ARGUMENTS
	;

text_macro_identifier
	: DIRECTIVE_IDENTIFIER
	;

// 19.3.2 `undef

undefine_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_UNDEF text_macro_identifier
	;

// 19.4 `ifdef, `else, `elsif, `endif , `ifndef

ifdef_directive
	: GRAVE_ACCENT DIRECTIVE_IFDEF text_macro_identifier (SOURCE_TEXT | compiler_directive)* elsif_directive* else_directive? endif_directive
	;

ifndef_directive
	: GRAVE_ACCENT DIRECTIVE_IFNDEF text_macro_identifier (SOURCE_TEXT | compiler_directive)* elsif_directive* else_directive? endif_directive
	;

elsif_directive
	: GRAVE_ACCENT DIRECTIVE_ELSIF text_macro_identifier (SOURCE_TEXT | compiler_directive)*
	;

else_directive
	: GRAVE_ACCENT DIRECTIVE_ELSE (SOURCE_TEXT | compiler_directive)*
	;

endif_directive
	: GRAVE_ACCENT DIRECTIVE_ENDIF
	;

// 19.5 `include

include_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_INCLUDE filename
	;

filename
	: DIRECTIVE_STRING
	;

// 19.6 `resetall

resetall_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_RESETALL
	;

// 19.7 `line

line_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_LINE line_number filename line_level
	;

line_number
	: DIRECTIVE_NUMBER
	;

line_level
	: DIRECTIVE_NUMBER
	;

// 19.8 `timescale

timescale_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_TIMESCALE time_literal DIRECTIVE_SLASH time_literal
	;

time_literal
	: time_number time_unit
	;

time_number
	: DIRECTIVE_NUMBER
	;

time_unit
	: TIME_UNIT
	;

// 19.9 `unconnected_drive and `nounconnected_drive

unconnected_drive_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_UNCONNECTED_DRIVE unconnected_drive_value
	;

unconnected_drive_value
	: UNCONNECTED_DRIVE_VALUE
	;

nounconnected_drive_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_NOUNCONNECTED_DRIVE
	;

// 19.10 `pragma
/*
pragma
	: GRAVE_ACCENT DIRECTIVE_PRAGMA pragma_name (pragma_expression (DIRECTIVE_COMMA pragma_expression)*)?
	;

pragma_name
	: DIRECTIVE_IDENTIFIER
	;

pragma_expression
	: pragma_keyword (DIRECTIVE_EQUAL pragma_value)?
	| pragma_value
	;

pragma_value
	: DIRECTIVE_IDENTIFIER
	| DIRECTIVE_NUMBER
	| DIRECTIVE_STRING
	;

pragma_keyword
	: DIRECTIVE_IDENTIFIER
	;
*/
// 19.11 `begin_keywords, `end_keywords

keywords_directive
	: GRAVE_ACCENT DIRECTIVE_BEGIN_KEYWORDS version_specifier
	;

version_specifier
	: DIRECTIVE_STRING
	;

endkeywords_directive
	: GRAVE_ACCENT DIRECTIVE_END_KEYWORDS
	;
