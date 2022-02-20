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

parser grammar SystemVerilogPreParser;

options { tokenVocab=SystemVerilogLexer; }

source_text
	: compiler_directive*
	;

compiler_directive
	: file_directive
	| line_directive_
	| begin_keywords_directive
	| celldefine_directive
	| default_nettype_directive
	| endcelldefine_directive
	| end_keywords_directive
	| ifdef_directive
	| ifndef_directive
	| include_directive
	| line_directive
	| nounconnected_drive_directive
	| pragma_directive
	| resetall_directive
	| text_macro_definition
	| text_macro_usage
	| timescale_directive
	| unconnected_drive_directive
	| undef_directive
	| undefineall_directive
	;

file_directive : GA FILE_DIRECTIVE ;
line_directive_ : GA LINE_DIRECTIVE_ ;
begin_keywords_directive : GA BEGIN_KEYWORDS_DIRECTIVE DIRECTIVE_TEXT ;
celldefine_directive : GA CELLDEFINE_DIRECTIVE ;
default_nettype_directive : GA DEFAULT_NETTYPE_DIRECTIVE DIRECTIVE_TEXT ;
endcelldefine_directive : GA ENDCELLDEFINE_DIRECTIVE ;
end_keywords_directive : GA END_KEYWORDS_DIRECTIVE ;
ifdef_directive : GA IFDEF_DIRECTIVE text_macro_identifier ifdef_group_of_lines elsif_directive* else_directive? endif_directive ;
ifndef_directive : GA IFNDEF_DIRECTIVE text_macro_identifier ifndef_group_of_lines elsif_directive* else_directive? endif_directive ;
include_directive : GA INCLUDE_DIRECTIVE DIRECTIVE_TEXT ;
line_directive : GA LINE_DIRECTIVE DIRECTIVE_TEXT ;
nounconnected_drive_directive : GA NOUNCONNECTED_DRIVE_DIRECTIVE ;
pragma_directive : GA PRAGMA_DIRECTIVE DIRECTIVE_TEXT ;
resetall_directive : GA RESETALL_DIRECTIVE ;
text_macro_definition : GA DEFINE_DIRECTIVE text_macro_identifier macro_text ;
text_macro_usage : GA MACRO_USAGE ;
timescale_directive : GA TIMESCALE_DIRECTIVE  DIRECTIVE_TEXT ;
unconnected_drive_directive : GA UNCONNECTED_DRIVE_DIRECTIVE DIRECTIVE_TEXT ;
undef_directive : GA UNDEF_DIRECTIVE text_macro_identifier ;
undefineall_directive : GA UNDEFINEALL_DIRECTIVE ;
elsif_directive : GA ELSIF_DIRECTIVE text_macro_identifier elsif_group_of_lines ;
else_directive : GA ELSE_DIRECTIVE else_group_of_lines ;
endif_directive : GA ENDIF_DIRECTIVE ;
text_macro_identifier : DIRECTIVE_IDENTIFIER ;
ifdef_group_of_lines : ( SOURCE_TEXT | compiler_directive )* ;
ifndef_group_of_lines : ( SOURCE_TEXT | compiler_directive )* ;
elsif_group_of_lines : ( SOURCE_TEXT | compiler_directive )* ;
else_group_of_lines : ( SOURCE_TEXT | compiler_directive )* ;
macro_text : MACRO_TEXT* ;
