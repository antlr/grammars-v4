using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class GoParserBase : Parser
{
    private static bool debug = false;
    HashSet<string> table = new HashSet<string>();
    private HashSet<string> no_semantics = new HashSet<string>();
    private static readonly string[] ALL_SEMANTIC_FUNCTIONS = {
        "isNotReceive", "isOperand", "isConversion",
        "isMethodExpr", "isTypeArgument", "isExpressionArgument"
    };

    static GoParserBase()
    {
        string[] args = Environment.GetCommandLineArgs();
        debug = HasArg(args, "--debug");
        if (debug)
        {
            Console.WriteLine("debug = " + debug);
        }
    }

    private static bool HasArg(string[] args, string arg)
    {
        foreach (string a in args)
        {
            if (a.ToLower().Contains(arg.ToLower()))
            {
                return true;
            }
        }
        return false;
    }

    protected GoParserBase(ITokenStream input)
        : base(input)
    {
        var args = Environment.GetCommandLineArgs().ToList();
        no_semantics = ParseNoSemantics(args);
    }

    protected GoParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        var args = Environment.GetCommandLineArgs().ToList();
        no_semantics = ParseNoSemantics(args);
    }

    private static HashSet<string> ParseNoSemantics(List<string> args)
    {
        var result = new HashSet<string>();
        if (args == null) return result;
        foreach (var a in args)
        {
            if (a.StartsWith("--no-semantics", StringComparison.OrdinalIgnoreCase))
            {
                int eqIndex = a.IndexOf('=');
                if (eqIndex == -1)
                {
                    // --no-semantics without value: disable all semantic functions
                    foreach (var func in ALL_SEMANTIC_FUNCTIONS)
                    {
                        result.Add(func);
                    }
                }
                else
                {
                    // --no-semantics=Func1,Func2,...
                    var value = a.Substring(eqIndex + 1);
                    var funcs = value.Split(',');
                    foreach (var func in funcs)
                    {
                        result.Add(func.Trim());
                    }
                }
            }
        }
        return result;
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
        if (no_semantics.Contains("isNotReceive")) return true;
        var la = tokenStream.LT(2);
        return la.Type != GoParser.RECEIVE;
    }

    public void addImportSpec()
    {
        var ctx = this.Context;
        var importSpec = ctx as GoParser.ImportSpecContext;
        if (importSpec == null) return;
        var packageName = importSpec.packageName();
        if (packageName != null)
        {
            var name = packageName.GetText();
            if (debug) System.Console.WriteLine("Entering " + name);
            table.Add(name);
            return;
        }
        var importPath = importSpec.importPath();
        if (importPath == null) return;
        var name2 = importPath.GetText();
        if (debug) System.Console.WriteLine("import path " + name2);
        name2 = name2.Replace("\"", "");
        if (string.IsNullOrEmpty(name2)) return;
        name2 = name2.Replace("\\", "/");
        string[] pathArr = name2.Split('/');
        if (pathArr.Length == 0) return;
        string lastComponent = pathArr.Last();
        if (string.IsNullOrEmpty(lastComponent)) return;
        // Handle special cases like "." and ".."
        if (lastComponent == "." || lastComponent == "..") return;
        string[] fileArr = lastComponent.Split('.');
        // Guard against empty array (can happen if lastComponent is all dots)
        if (fileArr.Length == 0)
        {
            table.Add(lastComponent);
            if (debug) System.Console.WriteLine("Entering " + lastComponent);
            return;
        }
        string fileName = fileArr.Last();
        if (string.IsNullOrEmpty(fileName))
        {
            // Fall back to lastComponent if split resulted in empty string
            fileName = lastComponent;
        }
        if (debug) System.Console.WriteLine("Entering " + fileName);
        table.Add(fileName);
    }

    public bool isOperand()
    {
        if (no_semantics.Contains("isOperand")) return true;
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
        if (no_semantics.Contains("isConversion")) return true;
        var la = tokenStream.LT(1);
        var result = la.Type != GoParser.IDENTIFIER;
        if (debug) System.Console.WriteLine("isConversion Returning " + result + " for " + la);
        return result;
    }

    public bool isMethodExpr()
    {
        if (no_semantics.Contains("isMethodExpr")) return true;
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

    // Built-in functions that take a type as first argument
    private static readonly HashSet<string> BUILTIN_TYPE_FUNCTIONS = new HashSet<string> {
        "make", "new"
    };

    // Check if we're in a call to a built-in function that takes a type as first argument.
    // Called after L_PAREN has been matched in the arguments rule.
    public bool isTypeArgument()
    {
        if (no_semantics.Contains("isTypeArgument")) return true;
        // After matching L_PAREN, LT(-1) is '(' and LT(-2) is the token before it
        var funcToken = tokenStream.LT(-2);
        if (funcToken == null || funcToken.Type != GoParser.IDENTIFIER)
        {
            if (debug) System.Console.WriteLine("isTypeArgument Returning false - no identifier before (");
            return false;
        }
        var result = BUILTIN_TYPE_FUNCTIONS.Contains(funcToken.Text);
        if (debug) System.Console.WriteLine("isTypeArgument Returning " + result + " for " + funcToken.Text);
        return result;
    }

    // Check if we're NOT in a call to a built-in function that takes a type.
    // This is the inverse of isTypeArgument for the expressionList alternative.
    public bool isExpressionArgument()
    {
        if (no_semantics.Contains("isExpressionArgument")) return true;
        var result = !isTypeArgument() || no_semantics.Contains("isTypeArgument");
        if (debug) System.Console.WriteLine("isExpressionArgument Returning " + result);
        return result;
    }
}
