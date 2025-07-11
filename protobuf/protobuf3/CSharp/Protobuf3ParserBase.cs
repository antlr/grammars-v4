using Antlr4.Runtime;
using System.Diagnostics;
using System.IO;

public abstract class Protobuf3ParserBase : Parser
{
    private readonly ITokenStream _input;
    private bool debug = false;
    private const string prefix = "   ";
    private SymbolTable symbolTable = new SymbolTable();
    private TypeClassification default_type = TypeClassification.Message_;

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
        var sym = new Symbol() { Name = name, Type = current, Classification = TypeClassification.Message_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Message " + sym);
    }

    public void DoEnumNameDef_()
    {
        var ctx = this.Context;
        var tctx = (Protobuf3Parser.DoEnumNameDefContext)ctx;
        var identifier = ((Protobuf3Parser.EnumDefContext)(tctx.Parent)).enumName().ident();
        var name = identifier.GetText();
        var current = symbolTable.CurrentScope();
        var sym = new Symbol() { Name = name, Type = current, Classification = TypeClassification.Enum_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Enum " + sym);
    }

    public void DoServiceNameDef_()
    {
        var ctx = this.Context;
        var tctx = (Protobuf3Parser.DoServiceNameDefContext)ctx;
        var identifier = ((Protobuf3Parser.ServiceDefContext)(tctx.Parent)).serviceName().ident();
        var name = identifier.GetText();
        var current = symbolTable.CurrentScope();
        var sym = new Symbol() { Name = name, Type = current, Classification = TypeClassification.Service_ };
        symbolTable.Define(sym);
        if (debug) System.Console.WriteLine(prefix + "defined Service " + sym);
    }

    public void DoEnterBlock_()
    {
        var newScope = new Symbol() { Name = "<block>", Classification = TypeClassification.Block_ };
        if (debug) System.Console.WriteLine(prefix + "EnterBlock " + newScope);
        symbolTable.EnterScope(newScope);
    }

    public void DoExitBlock_()
    {
        if (debug) System.Console.WriteLine(prefix + "ExitBlock " + symbolTable.CurrentScope());
        symbolTable.ExitScope();
    }

    public bool IsMessageType_()
    {
//        var ctx = this.Context;
//        System.Diagnostics.Debug.Assert(ctx is Protobuf3Parser.MessageTypeContext);
//        var tctx = ctx as Protobuf3Parser.MessageTypeContext;

        var la = tokenStream.LT(1);
        var id = la.Text;

//        var mn = tctx.messageName();
//        var id = mn.GetText();
//      System.Console.WriteLine("IsMessageType " + id);
        var symbol = symbolTable.Resolve(id);
        if (symbol != null)
        {
            if (symbol.Classification == TypeClassification.Message_)
            {
                if (debug) System.Console.WriteLine("IsMessageType_ found " + id + " " + true);
                return true;
            }
            else
            {
                if (debug) System.Console.WriteLine("IsMessageType_ found " + id + " " + false);
                return false;
            }
        }
        if (debug) System.Console.WriteLine("IsMessageType_ not found " + id + " " + (this.default_type == TypeClassification.Message_));
            return this.default_type == TypeClassification.Message_;
    }

    public bool IsEnumType_()
    {
//        var ctx = this.Context;
//        System.Diagnostics.Debug.Assert(ctx is Protobuf3Parser.EnumTypeContext);
//        var tctx = ctx as Protobuf3Parser.EnumTypeContext;

        var la = tokenStream.LT(1);
        var id = la.Text;

//        var mn = tctx.enumName();
//        var id = mn.GetText();
//      System.Console.WriteLine("IsEnumType " + id);
        var symbol = symbolTable.Resolve(id);
        if (symbol != null)
        {
            if (symbol.Classification == TypeClassification.Enum_)
            {
                if (debug) System.Console.WriteLine("IsEnumType found " + id + " " + true);
                return true;
            }
            else
            {
                if (debug) System.Console.WriteLine("IsEnumType found " + id + " " + false);
                return false;
            }
        }
        if (debug) System.Console.WriteLine("IsEnumType not found " + id + " " + (this.default_type == TypeClassification.Enum_));
            return this.default_type == TypeClassification.Enum_;
    }

    private ITokenStream tokenStream
    {
        get
        {
            return TokenStream;
        }
    }
}
