// Eclipse Public License - v 1.0, http://www.eclipse.org/legal/epl-v10.html
// Copyright (c) 2013, Christian Wulf (chwchw@gmx.de)
// Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

parser grammar CSharpPreprocessorParser;

options { tokenVocab=CSharpLexer; }

@parser::header 
{import java.util.Stack;
import java.util.HashSet;}

@parser::members
{Stack<Boolean> conditions = new Stack<Boolean>() {{ conditions.push(true); }};
public HashSet<String> ConditionalSymbols = new HashSet<String>() {{ ConditionalSymbols.add("DEBUG"); }};

private boolean allConditions() {
	for(boolean condition: conditions) {
		if (!condition)
			return false;
	}
	return true;
}
}

preprocessor_directive returns [boolean value]
	: DEFINE CONDITIONAL_SYMBOL directive_new_line_or_sharp{ ConditionalSymbols.add($CONDITIONAL_SYMBOL.text);
	   $value = allConditions(); } #preprocessorDeclaration

	| UNDEF CONDITIONAL_SYMBOL directive_new_line_or_sharp{ ConditionalSymbols.remove($CONDITIONAL_SYMBOL.text);
	   $value = allConditions(); } #preprocessorDeclaration

	| IF expr=preprocessor_expression directive_new_line_or_sharp
	  { $value = $expr.value.equals("true") && allConditions(); conditions.push($expr.value.equals("true")); }
	  #preprocessorConditional

	| ELIF expr=preprocessor_expression directive_new_line_or_sharp
	  { if (!conditions.peek()) { conditions.pop(); $value = $expr.value.equals("true") && allConditions();
	     conditions.push($expr.value.equals("true")); } else $value = false; }
	     #preprocessorConditional

	| ELSE directive_new_line_or_sharp
	  { if (!conditions.peek()) { conditions.pop(); $value = true && allConditions(); conditions.push(true); }
	    else $value = false; }    #preprocessorConditional

	| ENDIF directive_new_line_or_sharp             { conditions.pop(); $value = conditions.peek(); }
	   #preprocessorConditional
	| LINE (DIGITS STRING? | DEFAULT | DIRECTIVE_HIDDEN) directive_new_line_or_sharp { $value = allConditions(); }
	   #preprocessorLine

	| ERROR TEXT directive_new_line_or_sharp       { $value = allConditions(); }   #preprocessorDiagnostic

	| WARNING TEXT directive_new_line_or_sharp     { $value = allConditions(); }   #preprocessorDiagnostic

	| REGION TEXT? directive_new_line_or_sharp      { $value = allConditions(); }   #preprocessorRegion

	| ENDREGION TEXT? directive_new_line_or_sharp  { $value = allConditions(); }   #preprocessorRegion

	| PRAGMA TEXT directive_new_line_or_sharp      { $value = allConditions(); }   #preprocessorPragma

	| NULLABLE TEXT directive_new_line_or_sharp      { $value = allConditions(); }   #preprocessorNullable
	;

directive_new_line_or_sharp
    : DIRECTIVE_NEW_LINE
    | EOF
    ;

preprocessor_expression returns [String value]
	: TRUE                                 { $value = "true"; }
	| FALSE                                { $value = "false"; }
	| CONDITIONAL_SYMBOL                   { $value = ConditionalSymbols.contains($CONDITIONAL_SYMBOL.text) ? "true" : "false"; }
	| OPEN_PARENS expr=preprocessor_expression CLOSE_PARENS { $value = $expr.value; }
	| BANG expr=preprocessor_expression     { $value = $expr.value.equals("true") ? "false" : "true"; }
	| expr1=preprocessor_expression OP_EQ expr2=preprocessor_expression
	  { $value = ($expr1.value == $expr2.value ? "true" : "false"); }
	| expr1=preprocessor_expression OP_NE expr2=preprocessor_expression
	  { $value = ($expr1.value != $expr2.value ? "true" : "false"); }
	| expr1=preprocessor_expression OP_AND expr2=preprocessor_expression
	  { $value = ($expr1.value.equals("true") && $expr2.value.equals("true") ? "true" : "false"); }
	| expr1=preprocessor_expression OP_OR expr2=preprocessor_expression
	  { $value = ($expr1.value.equals("true") || $expr2.value.equals("true") ? "true" : "false"); }
	;