using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;
using Test;

public abstract class CSharpPreprocessorParserBase : Parser
{
    protected CSharpPreprocessorParserBase(ITokenStream input)
        : base(input)
    {
        conditions.Push(true);
        ConditionalSymbols.Add("DEBUG");
    }

    Stack<bool> conditions = new Stack<bool>();
    public HashSet<string> ConditionalSymbols = new HashSet<string>();

    protected bool AllConditions()
    {
	    foreach (bool condition in conditions)
	    {
		    if (!condition)
			    return false;
	    }
	    return true;
    }
    
    protected void OnPreprocessorDirectiveDefine()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorDeclarationContext;
        ConditionalSymbols.Add(d.CONDITIONAL_SYMBOL().GetText());
	    d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveUndef()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorDeclarationContext;
        ConditionalSymbols.Remove(d.CONDITIONAL_SYMBOL().GetText());
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveIf()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorConditionalContext;
        d.value = d.expr.value.Equals("true") && AllConditions();
	    conditions.Push(d.expr.value.Equals("true"));
    }

    protected void OnPreprocessorDirectiveElif()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorConditionalContext;
        if (!conditions.Peek())
        {
            conditions.Pop();
            d.value = d.expr.value.Equals("true") && AllConditions();
            conditions.Push(d.expr.value.Equals("true"));
        }
        else
        {
            d.value = false;
        }
    }

    protected void OnPreprocessorDirectiveElse()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorConditionalContext;
        if (!conditions.Peek())
        {
            conditions.Pop();
            d.value = true && AllConditions();
            conditions.Push(true);
        }
        else
        {
            d.value = false;
        }
    }

    protected void OnPreprocessorDirectiveEndif()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorConditionalContext;
        conditions.Pop();
        d.value = conditions.Peek();
    }

    protected void OnPreprocessorDirectiveLine()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorLineContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveError()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorDiagnosticContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveWarning()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorDiagnosticContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveRegion()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorRegionContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveEndregion()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorRegionContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectivePragma()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorPragmaContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorDirectiveNullable()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.PreprocessorNullableContext;
        d.value = AllConditions();
    }

    protected void OnPreprocessorExpressionTrue()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
        d.value = "true";
    }

    protected void OnPreprocessorExpressionFalse()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
        d.value = "false";
    }

    protected void OnPreprocessorExpressionConditionalSymbol()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
        d.value = ConditionalSymbols.Contains(d.CONDITIONAL_SYMBOL().GetText()) ? "true" : "false";
    }

    protected void OnPreprocessorExpressionConditionalOpenParens()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
        d.value = d.expr.value;
    }

    protected void OnPreprocessorExpressionConditionalBang()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
	    d.value = d.expr.value.Equals("true") ? "false" : "true";
    }

    protected void OnPreprocessorExpressionConditionalEq()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
	    d.value = (d.expr1.value == d.expr2.value ? "true" : "false");
    }

    protected void OnPreprocessorExpressionConditionalNe()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
	    d.value = (d.expr1.value != d.expr2.value ? "true" : "false");
    }

    protected void OnPreprocessorExpressionConditionalAnd()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
	    d.value = (d.expr1.value.Equals("true") && d.expr2.value.Equals("true") ? "true" : "false");
    }

    protected void OnPreprocessorExpressionConditionalOr()
    {
        ParserRuleContext c = this.Context;
        var d = c as CSharpPreprocessorParser.Preprocessor_expressionContext;
	    d.value = (d.expr1.value.Equals("true") || d.expr2.value.Equals("true") ? "true" : "false");
    }
}
