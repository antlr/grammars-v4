// Eclipse Public License - v 1.0, http://www.eclipse.org/legal/epl-v10.html
// Copyright (c) 2013, Christian Wulf (chwchw@gmx.de)
// Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

parser grammar CSharpPreprocessorParser;

options { tokenVocab=CSharpLexer; }

@parser::header { using System.Linq; }

@parser::members
{Stack<bool> conditions = new Stack<bool>(new bool[] { true });
public HashSet<string> ConditionalSymbols = new HashSet<string>() { "DEBUG" };}

preprocessor_directive returns [bool value]
	: DEFINE CONDITIONAL_SYMBOL directive_new_line_or_sharp{ ConditionalSymbols.Add($CONDITIONAL_SYMBOL.text);
	   $value = conditions.All(c => c); } #preprocessorDeclaration

	| UNDEF CONDITIONAL_SYMBOL directive_new_line_or_sharp{ ConditionalSymbols.Remove($CONDITIONAL_SYMBOL.text);
	   $value = conditions.All(c => c); } #preprocessorDeclaration

	| IF expr=preprocessor_expression directive_new_line_or_sharp
	  { $value = $expr.value == "true" && conditions.All(c => c); conditions.Push($expr.value == "true"); }
	  #preprocessorConditional

	| ELIF expr=preprocessor_expression directive_new_line_or_sharp
	  { if (!conditions.Peek()) { conditions.Pop(); $value = $expr.value == "true" && conditions.All(c => c);
	     conditions.Push($expr.value == "true"); } else $value = false; }
	     #preprocessorConditional

	| ELSE directive_new_line_or_sharp
	  { if (!conditions.Peek()) { conditions.Pop(); $value = true && conditions.All(c => c); conditions.Push(true); }
	    else $value = false; }    #preprocessorConditional

	| ENDIF directive_new_line_or_sharp             { conditions.Pop(); $value = conditions.Peek(); }
	   #preprocessorConditional
	| LINE (DIGITS STRING? | DEFAULT | DIRECTIVE_HIDDEN) directive_new_line_or_sharp { $value = conditions.All(c => c); }
	   #preprocessorLine

	| ERROR TEXT directive_new_line_or_sharp       { $value = conditions.All(c => c); }   #preprocessorDiagnostic

	| WARNING TEXT directive_new_line_or_sharp     { $value = conditions.All(c => c); }   #preprocessorDiagnostic

	| REGION TEXT? directive_new_line_or_sharp      { $value = conditions.All(c => c); }   #preprocessorRegion

	| ENDREGION TEXT? directive_new_line_or_sharp  { $value = conditions.All(c => c); }   #preprocessorRegion

	| PRAGMA TEXT directive_new_line_or_sharp      { $value = conditions.All(c => c); }   #preprocessorPragma

	| NULLABLE TEXT directive_new_line_or_sharp      { $value = conditions.All(c => c); }   #preprocessorNullable
	;

directive_new_line_or_sharp
    : DIRECTIVE_NEW_LINE
    | EOF
    ;

preprocessor_expression returns [string value]
	: TRUE                                 { $value = "true"; }
	| FALSE                                { $value = "false"; }
	| CONDITIONAL_SYMBOL                   { $value = ConditionalSymbols.Contains($CONDITIONAL_SYMBOL.text) ? "true" : "false"; }
	| OPEN_PARENS expr=preprocessor_expression CLOSE_PARENS { $value = $expr.value; }
	| BANG expr=preprocessor_expression     { $value = $expr.value == "true" ? "false" : "true"; }
	| expr1=preprocessor_expression OP_EQ expr2=preprocessor_expression
	  { $value = ($expr1.value == $expr2.value ? "true" : "false"); }
	| expr1=preprocessor_expression OP_NE expr2=preprocessor_expression
	  { $value = ($expr1.value != $expr2.value ? "true" : "false"); }
	| expr1=preprocessor_expression OP_AND expr2=preprocessor_expression
	  { $value = ($expr1.value == "true" && $expr2.value == "true" ? "true" : "false"); }
	| expr1=preprocessor_expression OP_OR expr2=preprocessor_expression
	  { $value = ($expr1.value == "true" || $expr2.value == "true" ? "true" : "false"); }
	;