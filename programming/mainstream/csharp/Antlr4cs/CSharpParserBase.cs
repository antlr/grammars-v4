using Antlr4.Runtime;
using System.Collections.Generic;
using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

public abstract class CSharpParserBase : Parser
{
    protected CSharpParserBase(ITokenStream input)
            : base(input)
    {
    }

    protected bool IsLocalVariableDeclaration()
    {
        var local_var_decl = this.Context as Test.CSharpParser.Local_variable_declarationContext;
        if (local_var_decl == null) return true;
        var local_variable_type = local_var_decl.local_variable_type();
        if (local_variable_type == null) return true;
        if (local_variable_type.GetText() == "var") return false;
        return true;
    }
}
