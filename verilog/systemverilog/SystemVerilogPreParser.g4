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
options { tokenVocab = SystemVerilogLexer; }

source_text : compiler_directive* ;
compiler_directive
	: begin_keywords_directive
	| celldefine_directive
	| default_nettype_directive
	| end_keywords_directive
	| endcelldefine_directive
	| file_directive
	| ifdef_directive
	| ifndef_directive
	| include_directive
	| line_directive
	| line_directive_
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
begin_keywords_directive : GA BEGIN_KEYWORDS_DIRECTIVE DQ version_specifier DQ ;
celldefine_directive : GA CELLDEFINE_DIRECTIVE ;
default_nettype_directive : GA DEFAULT_NETTYPE_DIRECTIVE default_nettype_value ;
default_nettype_value : DEFAULT_NETTYPE_VALUE ;
else_directive : GA ELSE_DIRECTIVE group_of_lines ;
elsif_directive : GA ELSIF_DIRECTIVE macro_identifier group_of_lines ;
end_keywords_directive : GA END_KEYWORDS_DIRECTIVE ;
endcelldefine_directive : GA ENDCELLDEFINE_DIRECTIVE ;
endif_directive : GA ENDIF_DIRECTIVE ;
file_directive : GA FILE_DIRECTIVE ;
filename : FILENAME ;
group_of_lines : ( source_text_ | compiler_directive )* ;
identifier : SIMPLE_IDENTIFIER ;
ifdef_directive : GA IFDEF_DIRECTIVE macro_identifier group_of_lines elsif_directive* else_directive? endif_directive ;
ifndef_directive : GA IFNDEF_DIRECTIVE macro_identifier group_of_lines elsif_directive* else_directive? endif_directive ;
include_directive : GA INCLUDE_DIRECTIVE ( DQ filename DQ | LT filename GT | text_macro_usage ) ;
level : UNSIGNED_NUMBER ;
line_directive : GA LINE_DIRECTIVE number DQ filename DQ level ;
line_directive_ : GA LINE_DIRECTIVE_ ;
macro_delimiter : MACRO_DELIMITER ;
macro_esc_newline : MACRO_ESC_NEWLINE ;
macro_esc_quote : MACRO_ESC_QUOTE ;
macro_identifier : MACRO_IDENTIFIER ;
macro_name : MACRO_NAME ;
macro_quote : MACRO_QUOTE ;
macro_text : ( macro_text_ | macro_delimiter | macro_esc_newline | macro_esc_quote | macro_quote | string_literal )* ;
macro_text_ : MACRO_TEXT ;
macro_usage : MACRO_USAGE ;
nounconnected_drive_directive : GA NOUNCONNECTED_DRIVE_DIRECTIVE ;
number : UNSIGNED_NUMBER ;
pragma_directive : GA PRAGMA_DIRECTIVE pragma_name ( pragma_expression ( CO pragma_expression )* )? ;
pragma_expression : ( pragma_keyword EQ )? pragma_value ;
pragma_keyword : SIMPLE_IDENTIFIER ;
pragma_name : SIMPLE_IDENTIFIER ;
pragma_value : LP pragma_expression ( CO pragma_expression )* RP | number | string_literal | identifier ;
resetall_directive : GA RESETALL_DIRECTIVE ;
source_text_ : SOURCE_TEXT ;
string_literal : STRING_LITERAL ;
text_macro_definition : GA DEFINE_DIRECTIVE macro_name macro_text ;
text_macro_usage : GA macro_usage ;
time_precision : TIME_VALUE TIME_UNIT ;
time_unit : TIME_VALUE TIME_UNIT ;
timescale_directive : GA TIMESCALE_DIRECTIVE time_unit SL time_precision ;
unconnected_drive_directive : GA UNCONNECTED_DRIVE_DIRECTIVE unconnected_drive_value ;
unconnected_drive_value : UNCONNECTED_DRIVE_VALUE ;
undef_directive : GA UNDEF_DIRECTIVE macro_identifier ;
undefineall_directive : GA UNDEFINEALL_DIRECTIVE ;
version_specifier : VERSION_SPECIFIER ;
