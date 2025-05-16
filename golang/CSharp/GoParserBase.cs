using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

public abstract class GoParserBase : Parser
{
    bool debug = false;
    SymbolTable symbolTable = new SymbolTable();
    Dictionary<IParseTree, Symbol> ContextToSymbolMap = new Dictionary<IParseTree, Symbol>();

    /*

    // A Sym represents a single symbol table entry.
    class Sym {
        uint64 Value;
        byte Type;
        string Name;
        uint64 GoType;
        // If this symbol is a function symbol, the corresponding Func
        Func Func;
        string goVersion;
    };

    // IsStatic reports whether this symbol is static (not visible outside its file).
    bool IsStatic(Sym s) { return s.Type >= 'a' }

    // nameWithoutInst returns s.Name if s.Name has no brackets (does not reference an
    // instantiated type, function, or method). If s.Name contains brackets, then it
    // returns s.Name with all the contents between (and including) the outermost left
    // and right bracket removed. This is useful to ignore any extra slashes or dots
    // inside the brackets from the string searches below, where needed.
    string nameWithoutInst(Sym s)
    {
        var start = strings.Index(s.Name, "[");
        if (start < 0) {
            return s.Name;
        }
        var end = strings.LastIndex(s.Name, "]");
        if (end < 0) {
            // Malformed name, should contain closing bracket too.
            return s.Name;
        }
        return s.Name[0:start] + s.Name[end+1:];
    }

    
    enum GoClassification {
        GoVariable,
        GoField,
        GoParameterType,
        GoArrayType,
        GoStructType,
        GoPointerType,
        GoFunctionType,
        GoInterfaceType,
        GoSliceType,
        GoMapType,
        GoChannelType,
    };

    class Frame {
        Dictionary<string, GoClassification> table = new Dictionary<string, GoClassification>();
        Dictionary<string, string> type = new Dictionary<string, string>();
    };
    
    Stack<Frame> table = new HashSet<Frame>();
*/
    
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

    protected void DefinePackageClause()
    {
        // Grab package name, and start a scope.
        var ctx = this.Context;
        var tctx = (GoParser.PackageClauseContext) ctx;
        var identifier = tctx.packageName().identifier();
        var name = identifier.IDENTIFIER().GetText();
        var newScope = new Symbol() { Name = "<package>", Classification = GoClassification.GoPackage };
        var sym = new Symbol() { Name = name, Type = newScope, Classification = GoClassification.GoPackage };
        symbolTable.Define(sym);
        symbolTable.EnterScope(newScope);
        if (debug) System.Console.WriteLine("defined " + sym);
    }

    protected bool closingBracket()
    {
        var la = tokenStream.LT(1);
        return la.Type == GoParser.R_PAREN || la.Type == GoParser.R_CURLY || la.Type == Eof;
    }

    public bool isNotReceive()
    {
        var la = tokenStream.LT(2);
        var result = la.Type != GoParser.RECEIVE;
        if (debug) System.Console.WriteLine("isNotReceive returning " + result);
        return result;
    }

    public void DefineImportSpec()
    {
        var ctx = this.Context;
        var count = ctx.ChildCount;
        var importSpec = ctx as GoParser.ImportSpecContext;
        if (importSpec == null) return;
        var packageName = importSpec.packageName();
        var parent_scope = symbolTable.CurrentScope().Parent;
        if (packageName != null)
        {
            var name = packageName.GetText();
            // Import declarations occur after the packageClause.
            // So, add the package to the *parent* of the
            // packageClause, i.e., the global scope.
            var newScope = new Symbol() { Name = "<package>", Classification = GoClassification.GoPackage };
            var symbol = new Symbol() { Name = name, Type = newScope, Classification = GoClassification.GoPackage};
            symbolTable.DefineInScope(parent_scope, symbol);
            if (debug) System.Console.WriteLine("defined " + symbol);
        }
        else
        {
            var fname = importSpec.importPath().GetText();
            fname = fname.Replace("\"", "");
            fname = fname.Replace("\\", "/");
            string[] pathArr = fname.Split('/');
            string[] fileArr = pathArr.Last().Split('.');
            string name = fileArr.Last().ToString();
            var newScope = new Symbol() { Name = "<package>", Classification = GoClassification.GoPackage };
            var symbol = new Symbol() { Name = name, Type = newScope, Classification = GoClassification.GoPackage};
            symbolTable.DefineInScope(parent_scope, symbol);
            if (debug) System.Console.WriteLine("defined " + symbol);
        }
    }

    public bool isOperand()
    {
        var la = tokenStream.LT(1);
        if (debug) System.Console.WriteLine("testing isOperand for " + la.Text);
        if (la.Text == "err") return true;
        if (la.Type != GoParser.IDENTIFIER) {
            if (debug) System.Console.WriteLine("isOperand Returning " + true + " for " + la);
            return true;
        }
        var symbol = symbolTable.Resolve(la.Text);
        var result = symbol != null;
        var la2 = tokenStream.LT(2);
        // If it's not followed by a '.', then it really should be
        // considered as operand.
        if (la2.Type != GoParser.DOT) {
            if (debug) System.Console.WriteLine("isOperand Returning " + true + " for " + la);
            return true;
        }
        // If it's followed by '.', and then followed by '(', then
        // it is a typeAssertion, and so la must be an operand.
        var la3 = tokenStream.LT(3);
        if (la3.Type == GoParser.L_PAREN)
        {
            if (debug) System.Console.WriteLine("isOperand Returning " + true + " for " + la);
            return true;
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
        result = symbolTable.Resolve(la.Text) == null;
        if (debug) System.Console.WriteLine("isMethodExpr Returning " + result + " for " + la);
        return result;
    }

    public void EnterFunctionDecl()
    {
        var ctx = this.Context.Parent;
        var function_decl = (GoParser.FunctionDeclContext) ctx;
        var id = function_decl.IDENTIFIER();
        var id_name = id.GetText();
        var sym = new Symbol() { Name = id_name, Classification = GoClassification.GoFunctionType };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine("defined " + sym);
        var newScope = new Symbol() { Name = "<function>", Classification = GoClassification.GoFunctionType };
        symbolTable.EnterScope(newScope);
    }

    public void ExitStruct()
    {
        if (debug) System.Console.WriteLine("ExitStruct");
        symbolTable.ExitScope();
    }
            
    public void ExitFunctionDecl()
    {
        if (debug) System.Console.WriteLine("ExitFunctionDecl");
        symbolTable.ExitScope();
    }

    public void EnterInterface()
    {
        var newScope = new Symbol() { Name = "<interface>", Classification = GoClassification.GoInterfaceType };
        if (debug) System.Console.WriteLine("EnterInterface " + newScope);
        symbolTable.EnterScope(newScope);
    }

    public void EnterStruct()
    {
        var newScope = new Symbol() { Name = "<struct>", Classification = GoClassification.GoStructType };
        if (debug) System.Console.WriteLine("EnterStruct " + newScope);
        symbolTable.EnterScope(newScope);
    }

    public void EnterBlock()
    {
        var newScope = new Symbol() { Name = "<block>", Classification = GoClassification.GoBlock };
        if (debug) System.Console.WriteLine("EnterBlock " + newScope);
        symbolTable.EnterScope(newScope);
    }

    public void ExitScope()
    {
        if (debug) System.Console.WriteLine("ExitScope " + symbolTable.CurrentScope());
        symbolTable.ExitScope();
    }

    public void ExitInterface()
    {
        if (debug) System.Console.WriteLine("ExitInterface " + symbolTable.CurrentScope());
        symbolTable.ExitScope();
    }

    public void ShortVarDecl()
    {
        var ctx = this.Context;
        var tctx = (GoParser.ShortVarDeclContext) ctx;
        var id_list = tctx.identifierList();
        var ids = id_list.IDENTIFIER();
        foreach (var id in ids)
        {
            var id_name = id.GetText();
            var sym = new Symbol() { Name = id_name, Classification = GoClassification.GoVariable };
            symbolTable.Define(sym);
            if (debug) System.Console.WriteLine("defined " + sym);
        }
    }

    public void DefineInterfaceType()
    {
        var ctx = this.Context;
        var tctx = (GoParser.InterfaceTypeContext) ctx;
        var method_specs = tctx.methodSpec();
        foreach (var method_spec in method_specs)
        {
            var name = method_spec.IDENTIFIER().GetText();
            var method = new Symbol() { Name = name, Classification = GoClassification.GoMethod };
            symbolTable.Define(method);
            if (debug) System.Console.WriteLine("defined " + method);
            // Assert parent of method is in interface scope.
            if (method.Parent.Classification != GoClassification.GoInterfaceType)
                throw new Exception("interface type definition error.");
        }
        // Bind the interface type to parent.
        var symbol = symbolTable.CurrentScope();
        this.ContextToSymbolMap[ctx] = symbol;
    }

    public void DefineStructType()
    {
        var ctx = this.Context;
        var tctx = (GoParser.StructTypeContext) ctx;
        // Bind the interface type to parent.
        var symbol = symbolTable.CurrentScope();
        this.ContextToSymbolMap[ctx] = symbol;
    }

    public void AssignChildToParent(int c)
    {
        var ctx = this.Context;
        var child = ctx.GetChild(c);
        if (debug) System.Console.WriteLine("assigning to " + ctx + " from " + child);
        var found = this.ContextToSymbolMap.TryGetValue(child, out Symbol symbol);
        if (symbol != null)
            this.ContextToSymbolMap[ctx] = symbol;
    }

    public void BindTypeToName()
    {
        var ctx = this.Context;
        var type_def = (GoParser.TypeDefContext) ctx;
        var type = this.ContextToSymbolMap.TryGetValue(type_def.type_(),
            out Symbol symbol);
        var name = type_def.IDENTIFIER().GetText();
        if (symbol != null)
        {
            // BIND name to TYPE.
            var classification = symbol.Classification;
            var new_symbol = new Symbol() { Name = name, Type = symbol, Classification = classification };
            symbolTable.Define(new_symbol);
            if (debug) System.Console.WriteLine("defined " + new_symbol);
        }
    }

    public void AddParameterDecl()
    {
        var ctx = this.Context;
        var tctx = (GoParser.ParameterDeclContext) ctx;
        var identifier_list = tctx.identifierList();
        if (identifier_list == null) return;
        var identifiers = identifier_list.IDENTIFIER();
        foreach (var identifier in identifiers)
        {
            var name = identifier.GetText();
            var variable = new Symbol() { Name = name, Classification = GoClassification.GoVariable };
            symbolTable.Define(variable);
            if (debug) System.Console.WriteLine("defined " + variable);
        }
    }

    public bool IsType()
    {
        var la = tokenStream.LT(1);
        /* Defer to parser for certain types. */
        if (la.Type == GoParser.CHAN)
        {
            if (debug) System.Console.WriteLine("IsType testing " + la.Text + " return " + true);
            return true;
        }
        if (la.Type == GoParser.L_BRACKET)
        {
            if (debug) System.Console.WriteLine("IsType testing " + la.Text + " return " + true);
            return true;
        }
        if (la.Type == GoParser.MAP)
        {
            if (debug) System.Console.WriteLine("IsType testing " + la.Text + " return " + true);
            return true;
        }
        var id = la.Text;
        var sym = symbolTable.Resolve(id);
        if (sym == null)
        {
            if (debug) System.Console.WriteLine("IsType testing " + la.Text + " return " + false);
            return false;
        }
        bool is_type = false;
        switch (sym.Classification)
        {
            case GoClassification.GoParameterType:
            case GoClassification.GoArrayType:
            case GoClassification.GoStructType:
            case GoClassification.GoPointerType:
            case GoClassification.GoFunctionType:
            case GoClassification.GoInterfaceType:
            case GoClassification.GoSliceType:
            case GoClassification.GoMapType:
            case GoClassification.GoChannelType:
                is_type = true;
                break;
        }
        if (debug) System.Console.WriteLine("testing " + sym + " return " + is_type);
        return is_type;
    }
}
