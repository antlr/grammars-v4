using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class CSharpParserBase : Parser
{
    private static readonly string[] ALL_SEMANTIC_FUNCTIONS = {
        "IsLocalVariableDeclaration"
    };

    private readonly HashSet<string> _noSemantics;

    protected CSharpParserBase(ITokenStream input)
        : base(input)
    {
        _noSemantics = ParseNoSemantics(Environment.GetCommandLineArgs());
    }

    protected CSharpParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _noSemantics = ParseNoSemantics(Environment.GetCommandLineArgs());
    }

    private static HashSet<string> ParseNoSemantics(string[] args)
    {
        var result = new HashSet<string>();
        foreach (var a in args)
        {
            if (a.StartsWith("--no-semantics", StringComparison.OrdinalIgnoreCase))
            {
                int eq = a.IndexOf('=');
                if (eq == -1)
                {
                    foreach (var f in ALL_SEMANTIC_FUNCTIONS) result.Add(f);
                }
                else
                {
                    foreach (var f in a.Substring(eq + 1).Split(','))
                        result.Add(f.Trim());
                }
            }
        }
        return result;
    }

    protected bool IsRightArrow() => AreAdjacent();
    protected bool IsRightShift() => AreAdjacent();
    protected bool IsRightShiftAssignment() => AreAdjacent();

    private bool AreAdjacent()
    {
        var first  = this.TokenStream.LT(-2);
        var second = this.TokenStream.LT(-1);
        return first != null && second != null &&
               first.TokenIndex + 1 == second.TokenIndex;
    }

    protected bool IsLocalVariableDeclaration()
    {
        if (_noSemantics.Contains("IsLocalVariableDeclaration")) return true;
        var local_var_decl = this.Context as CSharpParser.Local_variable_declarationContext;
        if (local_var_decl == null) return true;
        var local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.GetText() == "var") return false;
        return true;
    }
}
