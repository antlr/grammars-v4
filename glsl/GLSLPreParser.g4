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

parser grammar GLSLPreParser;
options { tokenVocab = GLSLLexer; }

translation_unit : compiler_directive* ;
compiler_directive
	: define_directive
	| elif_directive
	| else_directive
	| endif_directive
	| error_directive
	| extension_directive
	| if_directive
	| ifdef_directive
	| ifndef_directive
	| line_directive
	| pragma_directive
	| undef_directive
	| version_directive
	;
behavior : BEHAVIOR ;
constant_expression : CONSTANT_EXPRESSION ;
define_directive : NUMBER_SIGN DEFINE_DIRECTIVE macro_name macro_text ;
elif_directive : NUMBER_SIGN ELIF_DIRECTIVE constant_expression group_of_lines ;
else_directive : NUMBER_SIGN ELSE_DIRECTIVE group_of_lines ;
endif_directive : NUMBER_SIGN ENDIF_DIRECTIVE ;
error_directive : NUMBER_SIGN ERROR_DIRECTIVE error_message ;
error_message : ERROR_MESSAGE ;
extension_directive : NUMBER_SIGN EXTENSION_DIRECTIVE extension_name COLON behavior ;
extension_name : EXTENSION_NAME ;
group_of_lines : (program_text | compiler_directive)* ;
if_directive : NUMBER_SIGN IF_DIRECTIVE constant_expression group_of_lines elif_directive* else_directive? endif_directive ;
ifdef_directive : NUMBER_SIGN IFDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive ;
ifndef_directive : NUMBER_SIGN IFNDEF_DIRECTIVE macro_identifier group_of_lines elif_directive* else_directive? endif_directive ;
line_directive : NUMBER_SIGN LINE_DIRECTIVE line_expression ;
line_expression : LINE_EXPRESSION ;
macro_esc_newline : MACRO_ESC_NEWLINE ;
macro_identifier : MACRO_IDENTIFIER ;
macro_name : MACRO_NAME ;
macro_text : (macro_text_ | macro_esc_newline)* ;
macro_text_ : MACRO_TEXT ;
number : NUMBER ;
off : OFF ;
on : ON ;
pragma_debug : DEBUG LEFT_PAREN (on | off) RIGHT_PAREN ;
pragma_directive : NUMBER_SIGN PRAGMA_DIRECTIVE (stdgl | pragma_debug | pragma_optimize) ;
pragma_optimize : OPTIMIZE LEFT_PAREN (on | off) RIGHT_PAREN ;
profile : PROFILE ;
program_text : PROGRAM_TEXT ;
stdgl : STDGL ;
undef_directive : NUMBER_SIGN UNDEF_DIRECTIVE macro_identifier ;
version_directive : NUMBER_SIGN VERSION_DIRECTIVE number profile? ;
