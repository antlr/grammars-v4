// Author: Mustafa Said Ağca
// License: MIT

parser grammar VerilogPreprocessorParser;

options {
	tokenVocab = VerilogLexer;
}

// 19. Compiler directives
/*
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
*/