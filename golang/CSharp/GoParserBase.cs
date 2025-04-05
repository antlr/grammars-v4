using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class GoParserBase : Parser
{
    const bool debug = false;
    HashSet<string> table = new HashSet<string>();

    protected GoParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected GoParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }

    protected void myreset()
    {
        table = new HashSet<string>();
    }

    protected bool closingBracket()
    {
        var la = tokenStream.LT(1);
        return la.Type == GoParser.R_PAREN || la.Type == GoParser.R_CURLY || la.Type == Eof;
    }

    public bool isNotReceive()
    {
        var la = tokenStream.LT(2);
        return la.Type != GoParser.RECEIVE;
    }

    public void addImportSpec()
    {
        var ctx = this.Context;
        var count = ctx.ChildCount;
        var importSpec = ctx as GoParser.ImportSpecContext;
        if (importSpec == null) return;
        var packageName = importSpec.packageName();
        if (packageName != null)
        {
            var name = packageName.GetText();
            if (debug) System.Console.WriteLine("Entering " + name);
            table.Add(name);
        }
        else
        {
            var name = importSpec.importPath().GetText();
            name = name.Replace("\"", "");
            name = name.Replace("\\", "/");
            string[] pathArr = name.Split('/');
            string[] fileArr = pathArr.Last().Split('.');
            string fileName = fileArr.Last().ToString();
            if (debug) System.Console.WriteLine("Entering " + fileName);
            table.Add(fileName);
        }
    }

    public bool isOperand()
    {
        var la = tokenStream.LT(1);
        if (la.Text == "err") return true;
        bool result = true;
        if (la.Type != GoParser.IDENTIFIER) {
            if (debug) System.Console.WriteLine("isOperand Returning " + result + " for " + la);
            return result;
        }
        result = table.Contains(la.Text);
        var la2 = tokenStream.LT(2);
        // If it's not followed by a '.', then it really should be
        // considered as operand.
        if (la2.Type != GoParser.DOT) {
            result = true;
            if (debug) System.Console.WriteLine("isOperand Returning " + result + " for " + la);
            return result;
        }
        // If it's followed by '.', and then followed by '(', then
        // it is a typeAssertion, and so la must be an operand.
        var la3 = tokenStream.LT(3);
        if (la3.Type == GoParser.L_PAREN)
        {
            result = true;
            if (debug) System.Console.WriteLine("isOperand Returning " + result + " for " + la);
            return result;
        }
        if (debug) System.Console.WriteLine("isOperand Returning " + result + " for " + la);
        return result;
    }

    public bool isConversion()
    {
        var la = tokenStream.LT(1);
        var result = la.Type != GoParser.IDENTIFIER;
        if (debug) System.Console.WriteLine("isConversion Returning " + result + " for " + la);
        return result;
    }

    public bool isMethodExpr()
    {
        var la = tokenStream.LT(1);
        bool result = true;
        // See if it looks like a method expr.
        if (la.Type == GoParser.STAR) {
            if (debug) System.Console.WriteLine("isMethodExpr Returning " + result + " for " + la);
            return result;
        }
        if (la.Type != GoParser.IDENTIFIER) {
            result = false;
            if (debug) System.Console.WriteLine("isMethodExpr Returning " + result + " for " + la);
            return result;
        }
        result = ! table.Contains(la.Text);
        if (debug) System.Console.WriteLine("isMethodExpr Returning " + result + " for " + la);
        return result;
    }
}
