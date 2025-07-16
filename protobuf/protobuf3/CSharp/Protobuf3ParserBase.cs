using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime.Tree.Pattern;

public abstract class Protobuf3ParserBase : Parser
{
    private readonly ITokenStream _input;
    private bool debug = false;
    private const string prefix = "   ";
    private SymbolTable symbolTable = new SymbolTable();
    private TypeClassification default_type = TypeClassification.Message_;
    private static List<string> imported_files = new List<string>();
    
    public Protobuf3ParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        _input = input;
    }

    public void DoMessageNameDef_()
    {
        var ctx = this.Context;
        var tctx = (Protobuf3Parser.DoMessageNameDefContext)ctx;
        var identifier = ((Protobuf3Parser.MessageDefContext)(tctx.Parent)).messageName().ident();
        var name = identifier.GetText();
        var current = symbolTable.CurrentScope();
        var sym = new Symbol() { Name = name, Classification = TypeClassification.Message_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Message " + sym);
    }

    public void DoEnumNameDef_()
    {
        var ctx = this.Context;
        var tctx = (Protobuf3Parser.DoEnumNameDefContext)ctx;
        var identifier = ((Protobuf3Parser.EnumDefContext)(tctx.Parent)).enumName().ident();
        var name = identifier.GetText();
        var sym = new Symbol() { Name = name, Classification = TypeClassification.Enum_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Enum " + sym);
    }

    public void DoServiceNameDef_()
    {
        var ctx = this.Context;
        var tctx = (Protobuf3Parser.DoServiceNameDefContext)ctx;
        var identifier = ((Protobuf3Parser.ServiceDefContext)(tctx.Parent)).serviceName().ident();
        var name = identifier.GetText();
        var sym = new Symbol() { Name = name, Classification = TypeClassification.Service_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Service " + sym);
    }

    public void DoEnterBlock_()
    {
        var current_scope = symbolTable.CurrentScope();
        // Go up parse tree.
        var ctx1 = (this.Context?.Parent?.Parent as Protobuf3Parser.MessageDefContext)?.messageName()?.GetText();
        var ctx2 = (this.Context?.Parent?.Parent as Protobuf3Parser.EnumDefContext)?.enumName()?.GetText();
        var ctx3 = (this.Context?.Parent as Protobuf3Parser.ServiceDefContext)?.serviceName()?.GetText();
        var ctx = ctx1 != null ? ctx1 : (ctx2 != null ? ctx2 : ctx3);
        if (ctx == null) throw new Exception();
        var newScope = symbolTable.Resolve(ctx);
        if (debug) System.Console.WriteLine(prefix + "EnterBlock " + newScope);
        symbolTable.EnterScope(newScope);
    }

    public void DoExitBlock_()
    {
        if (debug) System.Console.WriteLine(prefix + "ExitBlock " + symbolTable.CurrentScope());
        var current = symbolTable.CurrentScope();
        var parent = current.Parent;
        symbolTable.ExitScope();
    }

    public bool IsMessageType_()
    {
        int i = 1;
        Symbol scope = null;
        Symbol symbol = null;
        bool first = true;
        bool global = false;
        for (; ; ++i, first = false)
        {
            var la = this.TokenStream.LT(i);
            var id = la.Text;
            if (debug) System.Console.Error.Write(id);
            if (la.Type == Protobuf3Parser.DOT)
            {
                if (first) global = true;
                if (this.TokenStream.LT(i + 1).Type != Protobuf3Parser.IDENTIFIER) break;
            }
            else if (la.Type == Protobuf3Parser.IDENTIFIER)
            {
                symbol = symbolTable.Resolve(id, scope);
                if (symbol != null)
                {
                    scope = symbol;
                }
                else break;
                if (this.TokenStream.LT(i + 1).Type != Protobuf3Parser.DOT) break;
            }
            else break;
        }
        if (symbol != null && symbol.Classification == TypeClassification.Message_)
        {
            if (debug) System.Console.WriteLine("IsMessageType_ found " + true);
            return true;
        }
        else if (symbol != null && symbol.Classification != TypeClassification.Message_)
        {
            if (debug) System.Console.WriteLine("IsMessageType_ found " + false);
            return false;
        }
        if (debug) System.Console.WriteLine("IsMessageType_ not found " + (this.default_type == TypeClassification.Message_));
        return this.default_type == TypeClassification.Message_;
    }

    public bool IsEnumType_()
    {
        int i = 1;
        Symbol scope = null;
        Symbol symbol = null;
        bool first = true;
        bool global = false;
        for (; ; ++i, first = false)
        {
            var la = this.TokenStream.LT(i);
            var id = la.Text;
            if (debug) System.Console.Error.Write(id);
            if (la.Type == Protobuf3Parser.DOT)
            {
                if (first) global = true;
                if (this.TokenStream.LT(i + 1).Type != Protobuf3Parser.IDENTIFIER) break;
            }
            else if (la.Type == Protobuf3Parser.IDENTIFIER)
            {
                symbol = symbolTable.Resolve(id, scope);
                if (symbol != null)
                {
                    scope = symbol;
                }
                else break;
                if (this.TokenStream.LT(i + 1).Type != Protobuf3Parser.DOT) break;
            }
            else break;
        }
        if (symbol != null && symbol.Classification == TypeClassification.Enum_)
        {
            if (debug) System.Console.WriteLine("IsEnumType found " + true);
            return true;
        }
        else if (symbol != null && symbol.Classification != TypeClassification.Enum_)
        {
            if (debug) System.Console.WriteLine("IsEnumType found " + false);
            return false;
        }
        if (debug) System.Console.WriteLine("IsEnumType not found " + (this.default_type == TypeClassification.Message_));
        return this.default_type == TypeClassification.Enum_;
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }

    public void DoRewind()
    {
        // Parse proto as a dry run to construct symbol table,
        // the rewind to do follow up parse in grammar.
        var parser = this as Protobuf3Parser;
        var _ctx = parser.Context;
        parser.Reset();
        parser.proto();
        parser.Reset();
        _ctx.RemoveLastChild();
        parser.Context = _ctx;
    }

    public void DoImportStatement_()
    {
        // Open input file and parse. Note, parse tree
        // will not be inserted here, but the symbol table
        // is reused.
        try {
            var ctx = this.Context;
            System.Diagnostics.Debug.Assert(ctx is Protobuf3Parser.ImportStatementContext);
            var tctx = ctx as Protobuf3Parser.ImportStatementContext;
            var import_file_name = TrimQuotes(tctx.strLit().GetText());
            var current_file = this.tokenStream.TokenSource.SourceName;
            if (debug) System.Console.Error.WriteLine("current file = " + current_file);
            if (debug) System.Console.Error.WriteLine("imported file = " + import_file_name);
            var save = Environment.CurrentDirectory.Replace("\\", "/");
            var current = Path.GetDirectoryName(current_file);
            var fp_dir = Path.GetFullPath(current);
            //Environment.CurrentDirectory = fp_dir;
            // Make sure we haven't done this before.
            var fp = Path.GetFullPath(import_file_name);
            if (imported_files.Contains(fp)) return;
            imported_files.Add(fp);
            ICharStream str = CharStreams.fromPath(fp);
            var lexer = new Protobuf3Lexer(str);
            CommonTokenStream tokens = new CommonTokenStream(lexer);
            var parser = new Protobuf3Parser(tokens);
            parser.twoPassParse();
            Environment.CurrentDirectory = save;
        }
        catch
        {
        }
    }

    private string TrimQuotes(string s)
    {
        return string.IsNullOrEmpty(s) ? s : s.Substring(1, s.Length - 2);
    }

    public bool IsNotKeyword()
    {
        // Make sure next word is not a keyword.
        var la = tokenStream.LT(1);
        switch (la.Type) {
            case Protobuf3Parser.DOUBLE:
            case Protobuf3Parser.FLOAT:
            case Protobuf3Parser.INT32:
            case Protobuf3Parser.INT64:
            case Protobuf3Parser.UINT32:
            case Protobuf3Parser.UINT64:
            case Protobuf3Parser.SINT32:
            case Protobuf3Parser.SINT64:
            case Protobuf3Parser.FIXED32:
            case Protobuf3Parser.FIXED64:
            case Protobuf3Parser.SFIXED32:
            case Protobuf3Parser.SFIXED64:
            case Protobuf3Parser.BOOL:
            case Protobuf3Parser.STRING:
            case Protobuf3Parser.BYTES:
                return false;
        }
        return true;
    }
}
