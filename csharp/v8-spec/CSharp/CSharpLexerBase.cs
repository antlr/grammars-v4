using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Antlr4.Runtime;

public abstract class CSharpLexerBase : Lexer
{
    protected CSharpLexerBase(ICharStream input)
        : base(input) { InitPreprocessor(); }

    protected CSharpLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { InitPreprocessor(); }

    // ANTLR provides PushMode/PopMode/CurrentMode but no PeekMode.
    // Peek at the top of the mode stack without popping it.
    public int PeekMode()
    {
        return ModeStack.Count == 0 ? DEFAULT_MODE : ModeStack.Peek();
    }

    // Override PopMode to make the stack bottomless: mismatched brackets
    // produce a parser-level error rather than a stack underflow exception.
    public override int PopMode()
    {
        if (ModeStack.Count == 0)
        {
            Console.Error.WriteLine("unbalanced ()/{}/[]");
            return DEFAULT_MODE;
        }
        return base.PopMode();
    }

    // Convenience predicates — names must match Macros.json expansions.
    public bool PeekModeIs(int mode) => PeekMode() == mode;

    public bool LookAheadIs(int pos, int value)    => InputStream.LA(pos) == value;
    public bool LookAheadIsNot(int pos, int value) => InputStream.LA(pos) != value;

    public bool LookAheadIsRBrace1()    => InputStream.LA(1) == '}';
    public bool LookAheadIsNotLBrace2() => InputStream.LA(2) != '{';
    public bool PeekModeIsIrsCont()     => PeekModeIs(CSharpLexer.IRS_CONT);
    public bool PeekModeIsIvsCont()     => PeekModeIs(CSharpLexer.IVS_CONT);

    // Wrap an interpolated-string fragment token in Unicode tortoise brackets
    // U+3014 〔 and U+3015 〕 so that grun tree output remains parseable.
    public void WrapToken()
    {
        Text = "\u3014" + Text.Replace("\u3015", "\u3015\u3015") + "\u3015";
    }

    // -------------------------------------------------------------------------
    // Preprocessor state
    // -------------------------------------------------------------------------
    private readonly Queue<IToken>   _pending   = new Queue<IToken>();
    private readonly HashSet<string> _symbols   = new HashSet<string>(StringComparer.Ordinal);
    private readonly Stack<bool>     _condition = new Stack<bool>(); // is current section active?
    private readonly Stack<bool>     _taken     = new Stack<bool>(); // was any branch taken at this level?

    // Expression evaluator cursor (reused per Evaluate call)
    private IList<IToken> _expr = Array.Empty<IToken>();
    private int           _epos;

    private void InitPreprocessor()
    {
        // Pre-populate from --DSYM or --DSYM;SYM2 command-line arguments (mirrors csc /define:)
        foreach (var arg in Environment.GetCommandLineArgs())
        {
            if (arg.StartsWith("--D", StringComparison.Ordinal))
            {
                foreach (var sym in arg.Substring(3).Split(';'))
                    if (sym.Length > 0) _symbols.Add(sym);
            }
        }
    }

    private bool IsActive() => _condition.Count == 0 || _condition.Peek();

    // -------------------------------------------------------------------------
    // NextToken override — intercepts DIRECTIVE-channel tokens
    // -------------------------------------------------------------------------
    public override IToken NextToken()
    {
        if (_pending.Count > 0) return _pending.Dequeue();

        IToken tok = base.NextToken();

        if (tok.Channel == CSharpLexer.DIRECTIVE)
        {
            IToken skipped = null;
            switch (tok.Type)
            {
                case CSharpLexer.DEFINE:   HandleDefine(); break;
                case CSharpLexer.UNDEF:    HandleUndef();  break;
                case CSharpLexer.KW_IF:    skipped = HandleIf();   break;
                case CSharpLexer.ELIF:     skipped = HandleElif(); break;
                case CSharpLexer.KW_ELSE:  skipped = HandleElse(); break;
                case CSharpLexer.ENDIF:    HandleEndif();  break;
            }
            if (skipped != null) _pending.Enqueue(skipped);
        }

        return tok;
    }

    // -------------------------------------------------------------------------
    // Directive handlers
    // -------------------------------------------------------------------------
    private void HandleDefine()
    {
        var line = CollectLine();
        string sym = SymbolFromLine(line);
        if (IsActive() && sym != null) _symbols.Add(sym);
    }

    private void HandleUndef()
    {
        var line = CollectLine();
        string sym = SymbolFromLine(line);
        if (IsActive() && sym != null) _symbols.Remove(sym);
    }

    private IToken HandleIf()
    {
        var line = CollectLine();
        bool outer = IsActive();
        bool result = outer && Evaluate(line);
        _condition.Push(result);
        _taken.Push(result);
        return result ? null : SkipFalseBlock();
    }

    private IToken HandleElif()
    {
        var line = CollectLine();
        bool alreadyTaken = _taken.Count > 0 ? _taken.Pop() : false;
        if (_condition.Count > 0) _condition.Pop();
        bool outer = IsActive();
        bool result = !alreadyTaken && outer && Evaluate(line);
        _condition.Push(result);
        _taken.Push(alreadyTaken || result);
        return result ? null : SkipFalseBlock();
    }

    private IToken HandleElse()
    {
        CollectLine(); // consume trailing whitespace / newline; no expression
        bool alreadyTaken = _taken.Count > 0 ? _taken.Pop() : false;
        if (_condition.Count > 0) _condition.Pop();
        bool outer = IsActive();
        bool result = !alreadyTaken && outer;
        _condition.Push(result);
        _taken.Push(true); // #else is always the final branch at this level
        return result ? null : SkipFalseBlock();
    }

    private void HandleEndif()
    {
        CollectLine(); // consume trailing whitespace / newline
        if (_condition.Count > 0) _condition.Pop();
        if (_taken.Count > 0)    _taken.Pop();
    }

    // -------------------------------------------------------------------------
    // CollectLine — drain DIRECTIVE_MODE tokens up to and including DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    private IList<IToken> CollectLine()
    {
        var tokens = new List<IToken>();
        IToken t;
        do
        {
            t = base.NextToken();
            // Drop hidden-channel tokens (DIRECTIVE_WHITESPACES, DIRECTIVE_SINGLE_LINE_COMMENT)
            if (t.Channel != Lexer.Hidden)
                tokens.Add(t);
        }
        while (t.Type != CSharpLexer.DIRECTIVE_NEW_LINE && t.Type != TokenConstants.EOF);
        return tokens;
    }

    private static string SymbolFromLine(IList<IToken> line)
    {
        foreach (var t in line)
            if (t.Type == CSharpLexer.CONDITIONAL_SYMBOL) return t.Text;
        return null;
    }

    // -------------------------------------------------------------------------
    // SkipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    // -------------------------------------------------------------------------
    private IToken SkipFalseBlock()
    {
        var sb           = new StringBuilder();
        var stream       = (ICharStream)InputStream;
        int depth        = 1;   // nesting depth; we start inside the false #if (depth 1)
        bool atLineStart = true;
        int startLine    = Line;

        while (true)
        {
            int c = stream.LA(1);
            if (c == IntStreamConstants.EOF) break;

            // Newline — reset line-start flag
            if (c == '\r' || c == '\n' || c == 0x85 || c == 0x2028 || c == 0x2029)
            {
                stream.Consume();
                sb.Append((char)c);
                if (c == '\r' && stream.LA(1) == '\n') // \r\n — consume as one newline
                {
                    stream.Consume();
                    sb.Append('\n');
                }
                atLineStart = true;
                continue;
            }

            // Leading whitespace on a fresh line — keep atLineStart true
            if (atLineStart && (c == ' ' || c == '\t'))
            {
                stream.Consume();
                sb.Append((char)c);
                continue;
            }

            // Potential preprocessor directive
            if (atLineStart && c == '#')
            {
                string kw = PeekKeyword(stream);
                if (kw == "if")
                {
                    depth++;
                }
                else if (kw == "endif")
                {
                    if (--depth == 0)
                        break; // leave '#' in stream; HandleEndif pops the stacks
                }
                else if ((kw == "else" || kw == "elif") && depth == 1)
                {
                    break; // leave '#' in stream; HandleElse/HandleElif will handle
                }
                // else: nested #else/#elif inside a deeper #if — consume normally
            }

            atLineStart = false;
            stream.Consume();
            sb.Append((char)c);
        }

        var tok = new CommonToken(CSharpLexer.SKIPPED_SECTION, sb.ToString());
        tok.Channel = Lexer.Hidden;
        tok.Line    = startLine;
        return tok;
    }

    // Peek at the directive keyword after '#' at LA(1) without consuming.
    private static string PeekKeyword(ICharStream stream)
    {
        int i = 2; // LA(1) is '#'
        while (stream.LA(i) == ' ' || stream.LA(i) == '\t') i++;
        var sb = new StringBuilder();
        int c;
        while ((c = stream.LA(i)) != -1 && char.IsLetter((char)c))
        {
            sb.Append((char)c);
            i++;
        }
        return sb.ToString();
    }

    // -------------------------------------------------------------------------
    // Recursive-descent expression evaluator over DIRECTIVE_MODE token list
    // -------------------------------------------------------------------------
    private bool Evaluate(IList<IToken> tokens)
    {
        _expr = tokens;
        _epos = 0;
        return ParseOr();
    }

    private int PeekType()
    {
        if (_epos < _expr.Count)
        {
            int t = _expr[_epos].Type;
            if (t != CSharpLexer.DIRECTIVE_NEW_LINE && t != TokenConstants.EOF) return t;
        }
        return -1;
    }

    private IToken EConsume() => _expr[_epos++];

    private bool ParseOr()
    {
        bool v = ParseAnd();
        while (PeekType() == CSharpLexer.TK_OR_OR)  { EConsume(); v = ParseAnd() || v; }
        return v;
    }

    private bool ParseAnd()
    {
        bool v = ParseEq();
        while (PeekType() == CSharpLexer.TK_AND_AND) { EConsume(); v = ParseEq() && v; }
        return v;
    }

    private bool ParseEq()
    {
        bool v = ParseUnary();
        if      (PeekType() == CSharpLexer.TK_EQ_EQ) { EConsume(); return v == ParseUnary(); }
        else if (PeekType() == CSharpLexer.TK_NOT_EQ) { EConsume(); return v != ParseUnary(); }
        return v;
    }

    private bool ParseUnary()
    {
        if (PeekType() == CSharpLexer.TK_NOT) { EConsume(); return !ParseUnary(); }
        return ParsePrimary();
    }

    private bool ParsePrimary()
    {
        int t = PeekType();
        if (t == CSharpLexer.TRUE)               { EConsume(); return true; }
        if (t == CSharpLexer.FALSE)              { EConsume(); return false; }
        if (t == CSharpLexer.CONDITIONAL_SYMBOL) { return _symbols.Contains(EConsume().Text); }
        if (t == CSharpLexer.TK_LPAREN)
        {
            EConsume();
            bool v = ParseOr();
            if (PeekType() == CSharpLexer.TK_RPAREN) EConsume();
            return v;
        }
        return false; // malformed expression
    }
}

