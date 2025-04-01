using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class GoParserBase : Parser
{
	const bool debug = false;
	
    protected GoParserBase(ITokenStream input)
        : base(input)
    {
    }

    protected GoParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }


    protected bool closingBracket()
    {
        int la = tokenStream.LA(1);
        return la == GoLexer.R_PAREN || la == GoLexer.R_CURLY || la == Eof;
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }

    public bool isNotReceive()
    {
        int la = tokenStream.LA(2);
        return la != GoLexer.RECEIVE;
    }

    List<string> table = new List<string>();

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
        if (la.Text == "hMd5")
        { }
        var result = la.Type != GoLexer.IDENTIFIER;
        if (debug) System.Console.WriteLine("isConversion Returning " + result + " for " + la);
        return result;
    }

    public bool isMethodExpr()
    {
        var la = tokenStream.LT(1);
        if (la.Text == "hMd5")
        { }
        bool result = true;
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
