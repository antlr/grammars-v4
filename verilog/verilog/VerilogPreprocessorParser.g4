// Author: Mustafa Said Ağca
// License: MIT

parser grammar VerilogPreprocessorParser;

options { tokenVocab = VerilogLexer; }

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
	| pragma
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
	: GRAVE_ACCENT DIRECTIVE_DEFAULT_NETTYPE DEFAULT_NETTYPE_VALUE
	;

// 19.3 `define and `undef
// 19.3.1 `define

text_macro_definition
	: GRAVE_ACCENT DIRECTIVE_DEFINE TEXT_MACRO_NAME (MACRO_TEXT | MACRO_TEXT_BACKSLASH_NEWLINE)+
	;

text_macro_usage
	: GRAVE_ACCENT DIRECTIVE_IDENTIFIER TEXT_MACRO_USAGE?
	;

// 19.3.2 `undef

undefine_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_UNDEF UNDEF_DIRECTIVE_IDENTIFIER
	;

// 19.4 `ifdef, `else, `elsif, `endif , `ifndef

ifdef_directive
	: GRAVE_ACCENT DIRECTIVE_IFDEF CONDITIONAL_DIRECTIVE_IDENTIFIER VERILOG_TEXT (GRAVE_ACCENT DIRECTIVE_ELSIF CONDITIONAL_DIRECTIVE_IDENTIFIER VERILOG_TEXT)* (GRAVE_ACCENT DIRECTIVE_ELSE VERILOG_TEXT)? GRAVE_ACCENT DIRECTIVE_ENDIF
	;

ifndef_directive
	: GRAVE_ACCENT DIRECTIVE_IFNDEF CONDITIONAL_DIRECTIVE_IDENTIFIER VERILOG_TEXT (GRAVE_ACCENT DIRECTIVE_ELSIF CONDITIONAL_DIRECTIVE_IDENTIFIER VERILOG_TEXT)* (GRAVE_ACCENT DIRECTIVE_ELSE VERILOG_TEXT)? GRAVE_ACCENT DIRECTIVE_ENDIF
	;

// 19.5 `include

include_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_INCLUDE FILENAME_STRING
	;

// 19.6 `resetall

resetall_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_RESETALL
	;

// 19.7 `line

line_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_LINE LINE_ARGUMENTS
	;

// 19.8 `timescale

timescale_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_TIMESCALE TIMESCALE_ARGUMENTS
	;

// 19.9 `unconnected_drive and `nounconnected_drive

unconnected_drive_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_UNCONNECTED_DRIVE UNCONNECTED_DRIVE_VALUE
	;

nounconnected_drive_compiler_directive
	: GRAVE_ACCENT DIRECTIVE_NOUNCONNECTED_DRIVE
	;

// 19.10 `pragma

pragma
	: GRAVE_ACCENT DIRECTIVE_PRAGMA PRAGMA_ARGUMENTS
	;

// 19.11 `begin_keywords, `end_keywords

keywords_directive
	: GRAVE_ACCENT DIRECTIVE_BEGIN_KEYWORDS VERSION_SPECIFIER
	;

endkeywords_directive
	: GRAVE_ACCENT DIRECTIVE_END_KEYWORDS
	;
