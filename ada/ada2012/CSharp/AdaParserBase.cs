using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Antlr4.Runtime;

public abstract class AdaParserBase : Parser
{
    private SymbolTable _st;
    private Stack<Symbol> _expectedTypeStack = new Stack<Symbol>();
    private bool debug = false;
    private bool outputSymbolTable = false;
    private bool outputAppliedOccurrences = false;
    private HashSet<string> noSemantics = new HashSet<string>();

    private static readonly string[] ALL_SEMANTIC_FUNCTIONS = {
        "IsAggregate", "IsTypeName"
    };

    protected AdaParserBase(ITokenStream input) : base(input)
    {
        InitOptions();
    }

    protected AdaParserBase(ITokenStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
        InitOptions();
    }

    private void InitOptions()
    {
        var args = Environment.GetCommandLineArgs().ToList();
        noSemantics = ParseNoSemantics(args);
        debug = args?.Where(a => a.IndexOf("--debug", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        outputSymbolTable = args?.Where(a => a.IndexOf("--output-symbol-table", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        outputAppliedOccurrences = args?.Where(a => a.IndexOf("--output-applied-occurrences", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        _st = new SymbolTable();
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
                    foreach (var func in ALL_SEMANTIC_FUNCTIONS)
                    {
                        result.Add(func);
                    }
                }
                else
                {
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

    public bool IsAggregate()
    {
        if (noSemantics.Contains("IsAggregate")) return true;
        var stream = this.InputStream as CommonTokenStream;
        var lt1 = stream.LT(1);
        if (lt1.Type != AdaLexer.LP)
        {
            if (debug) System.Console.Error.WriteLine("IsAggregate: LT(1) is not LP, returning false");
            return false;
        }
        // Structural scan from LT(2), tracking paren depth
        int depth = 0;
        for (int i = 2; ; i++)
        {
            var t = stream.LT(i);
            if (t == null || t.Type == TokenConstants.EOF) break;
            if (t.Type == AdaLexer.LP)
            {
                depth++;
            }
            else if (t.Type == AdaLexer.RP)
            {
                if (depth == 0)
                {
                    // Reached closing paren without indicators — fall through to type context
                    break;
                }
                depth--;
            }
            else if (depth == 0)
            {
                if (t.Type == AdaLexer.COMMA)
                {
                    if (debug) System.Console.Error.WriteLine("IsAggregate: found COMMA at depth 0, returning true");
                    return true;
                }
                if (t.Type == AdaLexer.ARROW)
                {
                    if (debug) System.Console.Error.WriteLine("IsAggregate: found ARROW at depth 0, returning true");
                    return true;
                }
                if (t.Type == AdaLexer.WITH)
                {
                    if (debug) System.Console.Error.WriteLine("IsAggregate: found WITH at depth 0, returning true");
                    return true;
                }
                if (t.Type == AdaLexer.NULL_)
                {
                    var next = stream.LT(i + 1);
                    if (next != null && next.Type == AdaLexer.RECORD)
                    {
                        if (debug) System.Console.Error.WriteLine("IsAggregate: found NULL RECORD at depth 0, returning true");
                        return true;
                    }
                }
            }
        }
        // Type-context check
        if (_expectedTypeStack.Count > 0)
        {
            var expected = _expectedTypeStack.Peek();
            if (expected != null && expected.IsComposite)
            {
                if (debug) System.Console.Error.WriteLine("IsAggregate: expected type is composite, returning true");
                return true;
            }
        }
        if (debug) System.Console.Error.WriteLine("IsAggregate: no aggregate indicators, returning false");
        return false;
    }

    public bool IsTypeName()
    {
        if (noSemantics.Contains("IsTypeName")) return true;
        var stream = this.InputStream as CommonTokenStream;
        var lt1 = stream.LT(1);
        if (lt1.Type != AdaLexer.IDENTIFIER_)
        {
            if (debug) System.Console.Error.WriteLine("IsTypeName: LT(1) is not IDENTIFIER_, returning false");
            return false;
        }
        // Collect dotted name
        string name = lt1.Text;
        int i = 2;
        while (true)
        {
            var dot = stream.LT(i);
            if (dot == null || dot.Type != AdaLexer.DOT) break;
            var next = stream.LT(i + 1);
            if (next == null || next.Type != AdaLexer.IDENTIFIER_) break;
            name += "." + next.Text;
            i += 2;
        }
        // For dotted names, just use the first component for lookup
        var firstName = lt1.Text;
        var resolved = _st.Resolve(firstName);
        if (resolved != null && resolved.Classification.Contains(TypeClassification.TypeName_))
        {
            if (debug) System.Console.Error.WriteLine("IsTypeName: " + firstName + " resolved as TypeName_, returning true");
            return true;
        }
        if (debug) System.Console.Error.WriteLine("IsTypeName: " + firstName + " not a type name, returning false");
        return false;
    }

    public void EnterDeclaration()
    {
        if (debug) System.Console.Error.WriteLine("EnterDeclaration");
        ParserRuleContext context = this.Context;
        for (; context != null; context = (ParserRuleContext)context.Parent)
        {
            if (context is AdaParser.Full_type_declarationContext ftd)
            {
                var defId = ftd.defining_identifier();
                if (defId != null)
                {
                    var name = defId.GetText();
                    bool isComposite = false;
                    var typeDef = ftd.type_definition();
                    if (typeDef != null)
                    {
                        isComposite = typeDef.record_type_definition() != null
                                   || typeDef.array_type_definition() != null;
                    }
                    DefineSymbol(name, TypeClassification.TypeName_, defId.Start, isComposite);
                }
                return;
            }
            if (context is AdaParser.Subtype_declarationContext std)
            {
                var defId = std.defining_identifier();
                if (defId != null)
                {
                    var name = defId.GetText();
                    bool isComposite = false;
                    var si = std.subtype_indication();
                    if (si != null)
                    {
                        var sm = si.subtype_mark();
                        if (sm != null)
                        {
                            var baseSym = _st.Resolve(sm.GetText());
                            if (baseSym != null) isComposite = baseSym.IsComposite;
                        }
                    }
                    DefineSymbol(name, TypeClassification.TypeName_, defId.Start, isComposite);
                }
                return;
            }
            if (context is AdaParser.Object_declarationContext od)
            {
                var defIdList = od.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Number_declarationContext nd)
            {
                var defIdList = nd.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Subprogram_declarationContext spd)
            {
                DefineSubprogramFromSpec(spd.subprogram_specification());
                return;
            }
            if (context is AdaParser.Subprogram_bodyContext spb)
            {
                DefineSubprogramFromSpec(spb.subprogram_specification());
                return;
            }
            if (context is AdaParser.Package_declarationContext pkd)
            {
                var pkgSpec = pkd.package_specification();
                if (pkgSpec != null)
                {
                    var dpun = pkgSpec.defining_program_unit_name();
                    if (dpun != null)
                    {
                        var defId = dpun.defining_identifier();
                        if (defId != null)
                            DefineSymbol(defId.GetText(), TypeClassification.PackageName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Package_bodyContext pkb)
            {
                var dpun = pkb.defining_program_unit_name();
                if (dpun != null)
                {
                    var defId = dpun.defining_identifier();
                    if (defId != null)
                        DefineSymbol(defId.GetText(), TypeClassification.PackageName_, defId.Start);
                }
                return;
            }
            if (context is AdaParser.Exception_declarationContext exd)
            {
                var defIdList = exd.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ExceptionName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Task_type_declarationContext ttd)
            {
                var defId = ttd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Single_task_declarationContext staskd)
            {
                var defId = staskd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Protected_type_declarationContext ptd)
            {
                var defId = ptd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Single_protected_declarationContext sprd)
            {
                var defId = sprd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Entry_declarationContext ed)
            {
                var defId = ed.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.SubprogramName_, defId.Start);
                return;
            }
            if (context is AdaParser.Component_declarationContext cd)
            {
                var defIdList = cd.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ComponentName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Incomplete_type_declarationContext itd)
            {
                var defId = itd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Private_type_declarationContext pvtd)
            {
                var defId = pvtd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Private_extension_declarationContext ped)
            {
                var defId = ped.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start, true);
                return;
            }
            if (context is AdaParser.Generic_instantiationContext gi)
            {
                var dpun = gi.defining_program_unit_name();
                if (dpun != null)
                {
                    var defId = dpun.defining_identifier();
                    if (defId != null)
                    {
                        // Determine classification from the keyword
                        var tc = TypeClassification.PackageName_;
                        if (gi.PROCEDURE() != null) tc = TypeClassification.SubprogramName_;
                        else if (gi.FUNCTION() != null) tc = TypeClassification.SubprogramName_;
                        DefineSymbol(defId.GetText(), tc, defId.Start);
                    }
                }
                var dd = gi.defining_designator();
                if (dd != null)
                {
                    var ddDpun = dd.defining_program_unit_name();
                    if (ddDpun != null)
                    {
                        var defId = ddDpun.defining_identifier();
                        if (defId != null)
                            DefineSymbol(defId.GetText(), TypeClassification.SubprogramName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Object_renaming_declarationContext ord)
            {
                var defId = ord.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Exception_renaming_declarationContext erd)
            {
                var defId = erd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ExceptionName_, defId.Start);
                return;
            }
            if (context is AdaParser.Package_renaming_declarationContext prd)
            {
                var dpun = prd.defining_program_unit_name();
                if (dpun != null)
                {
                    var defId = dpun.defining_identifier();
                    if (defId != null)
                        DefineSymbol(defId.GetText(), TypeClassification.PackageName_, defId.Start);
                }
                return;
            }
            if (context is AdaParser.Formal_complete_type_declarationContext fctd)
            {
                var defId = fctd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Formal_incomplete_type_declarationContext fitd)
            {
                var defId = fitd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.TypeName_, defId.Start);
                return;
            }
            if (context is AdaParser.Formal_object_declarationContext fod)
            {
                var defIdList = fod.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Formal_package_declarationContext fpd)
            {
                var defId = fpd.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.PackageName_, defId.Start);
                return;
            }
            if (context is AdaParser.Parameter_specificationContext ps)
            {
                var defIdList = ps.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                    }
                }
                return;
            }
            if (context is AdaParser.Loop_parameter_specificationContext lps)
            {
                var defId = lps.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Iterator_specificationContext its)
            {
                var defId = its.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Enumeration_literal_specificationContext els)
            {
                var defId = els.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.EnumerationLiteral_, defId.Start);
                return;
            }
            if (context is AdaParser.Choice_parameter_specificationContext cps)
            {
                var defId = cps.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Entry_index_specificationContext eis)
            {
                var defId = eis.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Extended_return_object_declarationContext erod)
            {
                var defId = erod.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Entry_bodyContext eb)
            {
                var defId = eb.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.SubprogramName_, defId.Start);
                return;
            }
            if (context is AdaParser.Task_bodyContext tb)
            {
                var defId = tb.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Protected_bodyContext pb)
            {
                var defId = pb.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                return;
            }
            if (context is AdaParser.Discriminant_specificationContext ds)
            {
                var defIdList = ds.defining_identifier_list();
                if (defIdList != null)
                {
                    foreach (var defId in defIdList.defining_identifier())
                    {
                        DefineSymbol(defId.GetText(), TypeClassification.ObjectName_, defId.Start);
                    }
                }
                return;
            }
        }
    }

    private void DefineSubprogramFromSpec(AdaParser.Subprogram_specificationContext spec)
    {
        if (spec == null) return;
        var procSpec = spec.procedure_specification();
        if (procSpec != null)
        {
            var dpun = procSpec.defining_program_unit_name();
            if (dpun != null)
            {
                var defId = dpun.defining_identifier();
                if (defId != null)
                    DefineSymbol(defId.GetText(), TypeClassification.SubprogramName_, defId.Start);
            }
            return;
        }
        var funcSpec = spec.function_specification();
        if (funcSpec != null)
        {
            var dd = funcSpec.defining_designator();
            if (dd != null)
            {
                var dpun = dd.defining_program_unit_name();
                if (dpun != null)
                {
                    var defId = dpun.defining_identifier();
                    if (defId != null)
                        DefineSymbol(defId.GetText(), TypeClassification.SubprogramName_, defId.Start);
                }
            }
        }
    }

    private void DefineSymbol(string name, TypeClassification classification, IToken token, bool isComposite = false)
    {
        var symbol = new Symbol()
        {
            Name = name,
            Classification = new HashSet<TypeClassification>() { classification },
            IsComposite = isComposite,
            DefinedFile = token?.TokenSource?.SourceName ?? "",
            DefinedLine = token?.Line ?? 0,
            DefinedColumn = token?.Column ?? 0
        };
        _st.Define(symbol);
        if (debug) System.Console.Error.WriteLine("Defined symbol: " + symbol);
    }

    public void EnterScope()
    {
        if (debug) System.Console.Error.WriteLine("EnterScope");
        _st.PushBlockScope();
    }

    public void ExitScope()
    {
        if (debug) System.Console.Error.WriteLine("ExitScope");
        _st.PopBlockScope();
    }

    public void PushExpectedType()
    {
        ParserRuleContext context = this.Context;
        for (; context != null; context = (ParserRuleContext)context.Parent)
        {
            if (context is AdaParser.Object_declarationContext od)
            {
                var si = od.subtype_indication();
                if (si != null)
                {
                    var sm = si.subtype_mark();
                    if (sm != null)
                    {
                        var resolved = _st.Resolve(sm.GetText());
                        _expectedTypeStack.Push(resolved);
                        if (debug) System.Console.Error.WriteLine("PushExpectedType: " + (resolved?.Name ?? "null") + " from object_declaration");
                        return;
                    }
                }
                // access_definition or array_type_definition — no composite context
                _expectedTypeStack.Push(null);
                return;
            }
            if (context is AdaParser.Assignment_statementContext ast)
            {
                var n = ast.name();
                if (n != null)
                {
                    // Try to resolve the LHS name
                    var nameText = n.GetText();
                    // Simple case: direct name
                    var resolved = _st.Resolve(nameText);
                    _expectedTypeStack.Push(resolved);
                    if (debug) System.Console.Error.WriteLine("PushExpectedType: " + (resolved?.Name ?? "null") + " from assignment_statement");
                    return;
                }
                _expectedTypeStack.Push(null);
                return;
            }
            if (context is AdaParser.Component_declarationContext cd)
            {
                var compDef = cd.component_definition();
                if (compDef != null)
                {
                    var si = compDef.subtype_indication();
                    if (si != null)
                    {
                        var sm = si.subtype_mark();
                        if (sm != null)
                        {
                            var resolved = _st.Resolve(sm.GetText());
                            _expectedTypeStack.Push(resolved);
                            if (debug) System.Console.Error.WriteLine("PushExpectedType: " + (resolved?.Name ?? "null") + " from component_declaration");
                            return;
                        }
                    }
                }
                _expectedTypeStack.Push(null);
                return;
            }
            if (context is AdaParser.Number_declarationContext)
            {
                // Number declarations are always scalar
                _expectedTypeStack.Push(null);
                if (debug) System.Console.Error.WriteLine("PushExpectedType: null (scalar) from number_declaration");
                return;
            }
            if (context is AdaParser.Parameter_specificationContext ps)
            {
                var sm = ps.subtype_mark();
                if (sm != null)
                {
                    var resolved = _st.Resolve(sm.GetText());
                    _expectedTypeStack.Push(resolved);
                    if (debug) System.Console.Error.WriteLine("PushExpectedType: " + (resolved?.Name ?? "null") + " from parameter_specification");
                    return;
                }
                _expectedTypeStack.Push(null);
                return;
            }
            if (context is AdaParser.Simple_return_statementContext)
            {
                // Would need to find enclosing function's return type — complex lookup
                _expectedTypeStack.Push(null);
                if (debug) System.Console.Error.WriteLine("PushExpectedType: null from simple_return_statement");
                return;
            }
        }
        _expectedTypeStack.Push(null);
    }

    public void PopExpectedType()
    {
        if (_expectedTypeStack.Count > 0)
        {
            _expectedTypeStack.Pop();
            if (debug) System.Console.Error.WriteLine("PopExpectedType");
        }
    }

    public void OutputSymbolTable()
    {
        if (outputSymbolTable)
        {
            System.Console.Error.WriteLine(_st.ToString());
        }
    }

    protected void ParsePragmas()
    {
        var stream = (BufferedTokenStream)TokenStream;
        stream.Fill();
        var allTokens = stream.GetTokens();
        const int PRAGMA_CHANNEL = 2;
        List<IToken> currentPragma = null;
        var pragmas = new List<List<IToken>>();
        foreach (var token in allTokens)
        {
            if (token.Channel != PRAGMA_CHANNEL)
                continue;
            if (token.Type == AdaLexer.PRAGMA)
            {
                currentPragma = new List<IToken>();
                currentPragma.Add(token);
            }
            else if (currentPragma != null)
            {
                currentPragma.Add(token);
                if (token.Type == AdaLexer.SEMI)
                {
                    pragmas.Add(currentPragma);
                    currentPragma = null;
                }
            }
        }
        foreach (var pragmaTokens in pragmas)
        {
            var defaultChannelTokens = new List<IToken>();
            foreach (var t in pragmaTokens)
            {
                var ct = new CommonToken(t);
                ct.Channel = Lexer.DefaultTokenChannel;
                defaultChannelTokens.Add(ct);
            }
            var eof = new CommonToken(TokenConstants.EOF);
            eof.Channel = Lexer.DefaultTokenChannel;
            defaultChannelTokens.Add(eof);
            var tokenSource = new ListTokenSource(defaultChannelTokens);
            var tokenStream = new CommonTokenStream(tokenSource);
            var parser = new AdaParser(tokenStream);
            parser.RemoveErrorListeners();
            foreach (var listener in this.ErrorListeners)
            {
                parser.AddErrorListener(listener);
            }
            parser.pragmaRule();
        }
    }
}
