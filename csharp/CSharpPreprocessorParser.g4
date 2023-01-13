// Eclipse Public License - v 1.0, http://www.eclipse.org/legal/epl-v10.html
// Copyright (c) 2013, Christian Wulf (chwchw@gmx.de)
// Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.

parser grammar CSharpPreprocessorParser;

options { tokenVocab=CSharpLexer; superClass=CSharpPreprocessorParserBase; }

preprocessor_directive returns [Boolean value]
	: DEFINE CONDITIONAL_SYMBOL directive_new_line_or_sharp { this.OnPreprocessorDirectiveDefine(); }  #preprocessorDeclaration
	| UNDEF CONDITIONAL_SYMBOL directive_new_line_or_sharp { this.OnPreprocessorDirectiveUndef(); } #preprocessorDeclaration
	| IF expr=preprocessor_expression directive_new_line_or_sharp { this.OnPreprocessorDirectiveIf(); }	  #preprocessorConditional
	| ELIF expr=preprocessor_expression directive_new_line_or_sharp { this.OnPreprocessorDirectiveElif(); } #preprocessorConditional
	| ELSE directive_new_line_or_sharp { this.OnPreprocessorDirectiveElse(); }    #preprocessorConditional
	| ENDIF directive_new_line_or_sharp { this.OnPreprocessorDirectiveEndif(); } #preprocessorConditional
	| LINE (DIGITS STRING? | DEFAULT | DIRECTIVE_HIDDEN) directive_new_line_or_sharp { this.OnPreprocessorDirectiveLine(); } #preprocessorLine
	| ERROR TEXT directive_new_line_or_sharp { this.OnPreprocessorDirectiveError(); }   #preprocessorDiagnostic
	| WARNING TEXT directive_new_line_or_sharp { this.OnPreprocessorDirectiveWarning(); }   #preprocessorDiagnostic
	| REGION TEXT? directive_new_line_or_sharp { this.OnPreprocessorDirectiveRegion(); }   #preprocessorRegion
	| ENDREGION TEXT? directive_new_line_or_sharp { this.OnPreprocessorDirectiveEndregion(); }   #preprocessorRegion
	| PRAGMA TEXT directive_new_line_or_sharp { this.OnPreprocessorDirectivePragma(); }   #preprocessorPragma
	| NULLABLE TEXT directive_new_line_or_sharp { this.OnPreprocessorDirectiveNullable(); }   #preprocessorNullable
	;

directive_new_line_or_sharp
    : DIRECTIVE_NEW_LINE
    | EOF
    ;

preprocessor_expression returns [String value]
	: TRUE { this.OnPreprocessorExpressionTrue(); }
	| FALSE { this.OnPreprocessorExpressionFalse(); }
	| CONDITIONAL_SYMBOL { this.OnPreprocessorExpressionConditionalSymbol(); }
	| OPEN_PARENS expr=preprocessor_expression CLOSE_PARENS { this.OnPreprocessorExpressionConditionalOpenParens(); }
	| BANG expr=preprocessor_expression { this.OnPreprocessorExpressionConditionalBang(); }
	| expr1=preprocessor_expression OP_EQ expr2=preprocessor_expression { this.OnPreprocessorExpressionConditionalEq(); }
	| expr1=preprocessor_expression OP_NE expr2=preprocessor_expression { this.OnPreprocessorExpressionConditionalNe(); }
	| expr1=preprocessor_expression OP_AND expr2=preprocessor_expression { this.OnPreprocessorExpressionConditionalAnd(); }
	| expr1=preprocessor_expression OP_OR expr2=preprocessor_expression { this.OnPreprocessorExpressionConditionalOr(); }
	;
