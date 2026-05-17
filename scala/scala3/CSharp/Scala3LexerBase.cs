#nullable enable
using System;
using System.Linq;
// Scala 3 INDENT/DEDENT injection for ANTLR4
// Modelled on Dotty (Scala 3 compiler) Scanners.scala handleNewLine logic.
//
// Region model
// ------------
// The injector tracks a stack of "regions":
//   TopLevel  – outermost scope
//   Indented  – inside an indentation block (INDENT … DEDENT)
//   InBraces  – inside {…}  — NEWLINE surfaced as separator; no INDENT/DEDENT
//   InParens  – inside (…) or […] — newlines suppressed entirely
//
// On each NEWLINE:
//   InParens              → suppress (implicit line joining)
//   InBraces              → keep NEWLINE on default channel if canEndStat(prev)
//                                  && canStartStat(next) && !isStatCtd(next)
//   Indented / TopLevel:
//     indent increases    → suppress NEWLINE, then INDENT if canStartIndent(prev)
//     indent unchanged    → keep NEWLINE on default channel if conditions met
//     indent decreases    → suppress NEWLINE, DEDENT(s), then re-emit NEWLINE
//                           on default channel if outer context needs it
//
// The NEWLINE token itself is used as the statement separator (mirroring Dotty's
// virtual NEWLINE/NEWLINES tokens).  No synthetic token is injected; the raw
// NEWLINE is simply kept on the default channel or suppressed to the hidden
// channel.  Grammar rule  end_of_stat : NEWLINE+ | SEMI  handles both.
//
// Dotty token-set predicates (canEndStatTokens, canStartStatTokens,
// isStatCtdTokens, canStartIndentTokens) are implemented as switch expressions.

using Antlr4.Runtime;
using System.Collections.Generic;
using System.IO;

public abstract class Scala3LexerBase : Lexer
{
    // Set to true when --3.0-migration is passed on the command line.
    // Enables Scala 2-compatible syntax (._  wildcard imports, [_] type wildcards).
    public static bool Migration30 { get; } =
        Environment.GetCommandLineArgs()
            .Any(a => a.Equals("--3.0-migration", StringComparison.OrdinalIgnoreCase));

    private enum Region { TopLevel, Indented, InBraces, InParens }

    // Region stack: bottom = TopLevel, top = innermost.
    private Stack<Region> _regionStack = new();
    // Indentation lengths for Indented regions only (parallel to regionStack Indented entries).
    private Stack<int> _indentLengthStack = new();
    // Queue of tokens waiting to be returned by NextToken().
    private LinkedList<IToken> _pendingTokens = new();

    private int _previousPendingTokenType = 0;
    // Last token type sent on the default channel — used for canEndStat checks.
    private int _lastNonHiddenType = 0;
    // Two-token look-ahead.
    private IToken? _curToken = null;
    private IToken? _ffgToken = null;

    protected Scala3LexerBase(ICharStream input) : base(input) { }
    protected Scala3LexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) { }

    public override IToken NextToken()
    {
        CheckNextToken();
        IToken token = _pendingTokens.First!.Value;
        _pendingTokens.RemoveFirst();
        return token;
    }

    public override void Reset()
    {
        Init();
        base.Reset();
    }

    // -----------------------------------------------------------------------
    // Initialisation
    // -----------------------------------------------------------------------

    private void Init()
    {
        _regionStack         = new();
        _indentLengthStack   = new();
        _pendingTokens       = new();
        _previousPendingTokenType = 0;
        _lastNonHiddenType   = 0;
        _curToken            = null;
        _ffgToken            = null;
    }

    // -----------------------------------------------------------------------
    // Core dispatch loop
    // -----------------------------------------------------------------------

    private void CheckNextToken()
    {
        if (_previousPendingTokenType == TokenConstants.EOF)
            return;

        if (_regionStack.Count == 0) // very first call
        {
            _regionStack.Push(Region.TopLevel);
            _indentLengthStack.Push(0); // base indentation for TopLevel
            SetCurrentAndFollowingTokens();
            HandleStartOfInput();
        }
        else
        {
            SetCurrentAndFollowingTokens();
        }

        switch (_curToken!.Type)
        {
            case Scala3Lexer.NEWLINE:
                HandleNewlineToken();
                break;

            case Scala3Lexer.LPAREN:
            case Scala3Lexer.LBRACKET:
                _regionStack.Push(Region.InParens);
                AddPendingToken(_curToken);
                break;

            case Scala3Lexer.COMMA:
                // Drain any Indented regions opened inside InParens (e.g. a same-line
                // colonArgument body: `f: u => expr,` — the INDENT was emitted but no
                // NEWLINE-triggered DEDENT ran before the comma).
                while (_regionStack.Count > 0 && _regionStack.Peek() == Region.Indented)
                {
                    _indentLengthStack.Pop();
                    _regionStack.Pop();
                    CreateAndAddPendingToken(Scala3Lexer.DEDENT, TokenConstants.DefaultChannel, "<DEDENT>", _curToken);
                }
                AddPendingToken(_curToken);
                break;

            case Scala3Lexer.RPAREN:
            case Scala3Lexer.RBRACKET:
                // Drain any Indented regions opened inside InParens (e.g. a
                // colonArgument body whose closing ')' is on the same source
                // line as the body text, so no NEWLINE-triggered DEDENT ran).
                while (_regionStack.Count > 0 && _regionStack.Peek() == Region.Indented)
                {
                    _indentLengthStack.Pop();
                    _regionStack.Pop();
                    CreateAndAddPendingToken(Scala3Lexer.DEDENT, TokenConstants.DefaultChannel, "<DEDENT>", _curToken);
                }
                if (_regionStack.Count > 0 && _regionStack.Peek() == Region.InParens)
                    _regionStack.Pop();
                AddPendingToken(_curToken);
                break;

            case Scala3Lexer.LBRACE:
                _regionStack.Push(Region.InBraces);
                AddPendingToken(_curToken);
                break;

            case Scala3Lexer.RBRACE:
                if (_regionStack.Count > 0 && _regionStack.Peek() == Region.InBraces)
                    _regionStack.Pop();
                AddPendingToken(_curToken);
                break;

            case TokenConstants.EOF:
                HandleEofToken();
                break;

            default:
                AddPendingToken(_curToken);
                break;
        }
    }

    private void SetCurrentAndFollowingTokens()
    {
        _curToken = _ffgToken ?? base.NextToken();
        _ffgToken = _curToken.Type == TokenConstants.EOF
                  ? _curToken
                  : base.NextToken();
    }

    // -----------------------------------------------------------------------
    // Start-of-input: skip leading blank lines
    // -----------------------------------------------------------------------

    private void HandleStartOfInput()
    {
        while (_curToken!.Type == Scala3Lexer.NEWLINE)
        {
            HideAndAddPendingToken(_curToken);
            SetCurrentAndFollowingTokens();
        }
    }

    // -----------------------------------------------------------------------
    // NEWLINE handling
    // -----------------------------------------------------------------------

    private void HandleNewlineToken()
    {
        // nlToken: default-channel copy of the raw NEWLINE token.
        // We will either add it to the queue on the default channel (making it
        // visible to the parser as a statement separator) or suppress it to the
        // hidden channel.  No separate synthetic token is injected.
        IToken nlToken = new CommonToken(_curToken!);

        bool hasLeadingWS = _ffgToken!.Type == Scala3Lexer.WS;
        if (hasLeadingWS)
            SetCurrentAndFollowingTokens(); // _curToken=WS, _ffgToken=first of next line

        // Blank / comment-only line: suppress everything and bail.
        if (_ffgToken!.Type == Scala3Lexer.NEWLINE)
        {
            HideAndAddPendingToken(nlToken);
            if (hasLeadingWS) HideAndAddPendingToken(_curToken!);
            return;
        }

        Region top = _regionStack.Peek();

        // Compute next-line indentation from the WS text before enqueuing anything,
        // so we can decide NEWLINE visibility first and maintain source token order.
        int newIndent;
        if (_ffgToken.Type == TokenConstants.EOF)
            newIndent = 0;
        else if (hasLeadingWS)
            newIndent = GetIndentLength(_curToken!.Text);
        else
            newIndent = 0;

        // Lines starting with '.' are method-chain continuations — suppress
        // NEWLINE as separator and INDENT, but still emit DEDENT(s).
        bool isDot = _ffgToken.Type == Scala3Lexer.DOT;
        int curIndent = _indentLengthStack.Peek();

        // Whether we will emit an INDENT for this transition.
        // When true, NEWLINE is always suppressed (even in InBraces) because
        // emitting NEWLINE before INDENT would produce an unexpected separator.
        //
        // ARROW (=>) and CTXARROW (?=>) inside InParens are NOT excluded:
        // multi-statement lambda bodies (e.g. `loginData =>\n  val x = …\n  x`)
        // require INDENT/DEDENT even inside (…).  Trailing-comma cases such as
        // `err => body,` are handled by the COMMA drain in CheckNextToken, which
        // emits DEDENT before the comma so the grammar sees the separator correctly.
        // `extension (params)` ends with RPAREN but has an indented method body.
        // Allow RPAREN to trigger INDENT only when the outer context is Indented
        // or TopLevel — not inside a function-call argument list (InParens) or a
        // brace block (InBraces), and not when the following line starts with a
        // class-template continuation keyword (extends / with / derives), which
        // is a header-continuation line, not a new indented block.
        bool rparenOpensIndent = _lastNonHiddenType == Scala3Lexer.RPAREN
                              && _regionStack.Peek() != Region.InParens
                              && _regionStack.Peek() != Region.InBraces
                              && _ffgToken!.Type != Scala3Lexer.EXTENDS
                              && _ffgToken!.Type != Scala3Lexer.WITH;
        bool willIndent = newIndent > curIndent
                       && !isDot
                       && (CanStartIndent(_lastNonHiddenType) || rparenOpensIndent);

        // ---- Decide whether to surface the NEWLINE as a statement separator ----
        //
        // Rules (in priority order):
        //  1. Indent changes (DEDENT or INDENT upcoming) → always suppress.
        //  2. InParens → always suppress (implicit line joining).
        //  3. InBraces → surface at statement boundary (no extra guards needed
        //     because RBRACE/EOF are not in CanStartStat).
        //  4. Indented/TopLevel, same level → surface at statement boundary
        //     with extra EOF/RBRACE guards.
        //  5. Indented/TopLevel, increasing indent, no INDENT → suppress
        //     (continuation line at unexpected depth).
        bool surfaceNewline;
        if (newIndent < curIndent || willIndent)
            surfaceNewline = false;
        else if (top == Region.InParens)
            surfaceNewline = false;
        else if (top == Region.InBraces)
            surfaceNewline = !isDot
                && CanEndStat(_lastNonHiddenType)
                && CanStartStat(_ffgToken.Type)
                && !IsStatContinuation(_ffgToken.Type);
        else if (newIndent > curIndent) // Indented/TopLevel continuation line
            surfaceNewline = false;
        else // Indented or TopLevel, same level
            surfaceNewline = !isDot
                && _ffgToken.Type != TokenConstants.EOF
                && _ffgToken.Type != Scala3Lexer.RBRACE
                && CanEndStat(_lastNonHiddenType)
                && CanStartStat(_ffgToken.Type)
                && !IsStatContinuation(_ffgToken.Type);

        // ---- Emit NEWLINE then WS in source order ----
        if (surfaceNewline) AddPendingToken(nlToken);
        else HideAndAddPendingToken(nlToken);
        if (hasLeadingWS) HideAndAddPendingToken(_curToken!);

        // ---- Handle INDENT / DEDENT ----
        //
        // InBraces and InParens participate fully: for-comprehensions, match
        // expressions, and other indented constructs inside { } and ( ) need
        // real INDENT/DEDENT tokens.  InsertDedentTokens stops at InBraces/
        // InParens boundaries (it only pops Indented regions), so the brace/
        // paren region is never accidentally removed.
        if (newIndent < curIndent)
        {
            // Decreasing indentation: emit DEDENT(s), then re-surface a NEWLINE
            // copy on the default channel if the outer context needs a separator.
            InsertDedentTokens(newIndent, _ffgToken);
            Region newTop = _regionStack.Peek();
            if (newTop != Region.InParens && !isDot
                && _ffgToken.Type != TokenConstants.EOF
                && _ffgToken.Type != Scala3Lexer.RBRACE
                && CanEndStat(_lastNonHiddenType)
                && CanStartStat(_ffgToken.Type)
                && !IsStatContinuation(_ffgToken.Type))
            {
                // Re-emit a default-channel copy of the NEWLINE after the DEDENTs.
                AddPendingToken(new CommonToken(nlToken));
            }
        }
        else if (willIndent)
        {
            // Increasing indentation: open a new indented block.
            _indentLengthStack.Push(newIndent);
            _regionStack.Push(Region.Indented);
            CreateAndAddPendingToken(Scala3Lexer.INDENT, TokenConstants.DefaultChannel, "<INDENT>", _ffgToken);
        }
        // else: same level (or increase without CanStartIndent) — no structural tokens.
    }

    // Emit DEDENT tokens for all Indented regions deeper than newIndent.
    private void InsertDedentTokens(int newIndent, IToken anchor)
    {
        while (_regionStack.Count > 0 && _regionStack.Peek() == Region.Indented)
        {
            if (_indentLengthStack.Peek() <= newIndent)
                break;
            _indentLengthStack.Pop();
            _regionStack.Pop();
            CreateAndAddPendingToken(Scala3Lexer.DEDENT, TokenConstants.DefaultChannel, "<DEDENT>", anchor);
        }
    }

    // -----------------------------------------------------------------------
    // EOF handling
    // -----------------------------------------------------------------------

    private void HandleEofToken()
    {
        // Close every open indented block.
        while (_regionStack.Count > 0 && _regionStack.Peek() == Region.Indented)
        {
            _indentLengthStack.Pop();
            _regionStack.Pop();
            CreateAndAddPendingToken(Scala3Lexer.DEDENT, TokenConstants.DefaultChannel, "<DEDENT>", _curToken!);
        }
        AddPendingToken(_curToken!);
    }

    // -----------------------------------------------------------------------
    // Token-set predicates (mirroring Dotty Tokens.scala)
    // -----------------------------------------------------------------------

    // canEndStatTokens = atomicExprTokens ∪ {TYPE, GIVEN, RPAREN, RBRACE, RBRACKET, DEDENT}
    // atomicExprTokens = literals ∪ identifiers ∪ {USCORE, THIS, SUPER, RETURN, QUOTEID}
    // endMarkerTag keywords (IF, WHILE, FOR, MATCH, TRY, VAL, NEW, EXTENSION) are also
    // included because 'end X' statements end with those keyword tokens.
    private static bool CanEndStat(int t) => t switch
    {
        Scala3Lexer.Id                        => true,
        Scala3Lexer.Varid                     => true,
        Scala3Lexer.BacktickId                => true,
        Scala3Lexer.Op                        => true,
        Scala3Lexer.IntegerLiteral            => true,
        Scala3Lexer.FloatingPointLiteral      => true,
        Scala3Lexer.BooleanLiteral            => true,
        Scala3Lexer.CharacterLiteral          => true,
        Scala3Lexer.StringLiteral             => true,
        Scala3Lexer.InterpolatedStringLiteral => true,
        Scala3Lexer.SymbolLiteral             => true,
        Scala3Lexer.NullLiteral               => true,
        Scala3Lexer.QuoteId                   => true,
        Scala3Lexer.USCORE                    => true,
        Scala3Lexer.THIS                      => true,
        Scala3Lexer.SUPER                     => true,
        Scala3Lexer.RETURN                    => true,
        Scala3Lexer.TYPE                      => true,
        Scala3Lexer.GIVEN                     => true,
        Scala3Lexer.RPAREN                    => true,
        Scala3Lexer.RBRACE                    => true,
        Scala3Lexer.RBRACKET                  => true,
        Scala3Lexer.DEDENT                    => true,
        Scala3Lexer.NEWLINE                   => true,
        // endMarkerTag keywords: 'end if', 'end while', etc.
        Scala3Lexer.IF                        => true,
        Scala3Lexer.WHILE                     => true,
        Scala3Lexer.FOR                       => true,
        Scala3Lexer.MATCH                     => true,
        Scala3Lexer.TRY                       => true,
        Scala3Lexer.VAL                       => true,
        Scala3Lexer.NEW                       => true,
        Scala3Lexer.EXTENSION                 => true,
        _                                     => false
    };

    // isStatCtdTokens: next-token set that suppresses NL even when canEndStat is true.
    private static bool IsStatContinuation(int t) => t switch
    {
        Scala3Lexer.THEN    => true,
        Scala3Lexer.ELSE    => true,
        Scala3Lexer.DO      => true,
        Scala3Lexer.CATCH   => true,
        Scala3Lexer.FINALLY => true,
        Scala3Lexer.YIELD   => true,
        Scala3Lexer.MATCH   => true,
        _                   => false
    };

    // canStartStatTokens: next-token set that can open a new statement.
    private static bool CanStartStat(int t) => t switch
    {
        Scala3Lexer.Id                        => true,
        Scala3Lexer.Varid                     => true,
        Scala3Lexer.BacktickId                => true,
        Scala3Lexer.Op                        => true,
        Scala3Lexer.IntegerLiteral            => true,
        Scala3Lexer.FloatingPointLiteral      => true,
        Scala3Lexer.BooleanLiteral            => true,
        Scala3Lexer.CharacterLiteral          => true,
        Scala3Lexer.StringLiteral             => true,
        Scala3Lexer.InterpolatedStringLiteral => true,
        Scala3Lexer.SymbolLiteral             => true,
        Scala3Lexer.NullLiteral               => true,
        Scala3Lexer.QuoteId                   => true,
        Scala3Lexer.USCORE                    => true,
        Scala3Lexer.THIS                      => true,
        Scala3Lexer.SUPER                     => true,
        Scala3Lexer.NEW                       => true,
        Scala3Lexer.RETURN                    => true,
        Scala3Lexer.THROW                     => true,
        Scala3Lexer.IF                        => true,
        Scala3Lexer.WHILE                     => true,
        Scala3Lexer.FOR                       => true,
        Scala3Lexer.TRY                       => true,
        Scala3Lexer.LBRACE                    => true,
        Scala3Lexer.LPAREN                    => true,
        Scala3Lexer.LBRACKET                  => true,
        Scala3Lexer.QUOTE                     => true,
        Scala3Lexer.INDENT                    => true,
        Scala3Lexer.AT                        => true,
        Scala3Lexer.CASE                      => true,
        Scala3Lexer.END                       => true,
        Scala3Lexer.DEF                       => true,
        Scala3Lexer.VAL                       => true,
        Scala3Lexer.VAR                       => true,
        Scala3Lexer.TYPE                      => true,
        Scala3Lexer.GIVEN                     => true,
        Scala3Lexer.ABSTRACT                  => true,
        Scala3Lexer.FINAL                     => true,
        Scala3Lexer.PRIVATE                   => true,
        Scala3Lexer.PROTECTED                 => true,
        Scala3Lexer.OVERRIDE                  => true,
        Scala3Lexer.SEALED                    => true,
        Scala3Lexer.CLASS                     => true,
        Scala3Lexer.TRAIT                     => true,
        Scala3Lexer.OBJECT                    => true,
        Scala3Lexer.ENUM                      => true,
        Scala3Lexer.IMPORT                    => true,
        Scala3Lexer.EXPORT                    => true,
        Scala3Lexer.PACKAGE                   => true,
        Scala3Lexer.INLINE                    => true,
        Scala3Lexer.LAZY                      => true,
        Scala3Lexer.IMPLICIT                  => true,
        Scala3Lexer.EXTENSION                 => true,
        // contextual modifier keywords
        Scala3Lexer.OPEN                      => true,
        Scala3Lexer.INFIX                     => true,
        Scala3Lexer.TRANSPARENT               => true,
        Scala3Lexer.OPAQUE                    => true,
        // contextual keywords usable as plain identifiers
        Scala3Lexer.AS                        => true,
        Scala3Lexer.DERIVES                   => true,
        Scala3Lexer.USING                     => true,
        _                                     => false
    };

    // canStartIndentTokens: previous tokens that allow INDENT emission.
    private static bool CanStartIndent(int t) => t switch
    {
        Scala3Lexer.THEN     => true,
        Scala3Lexer.ELSE     => true,
        Scala3Lexer.DO       => true,
        Scala3Lexer.CATCH    => true,
        Scala3Lexer.FINALLY  => true,
        Scala3Lexer.YIELD    => true,
        Scala3Lexer.MATCH    => true,
        Scala3Lexer.COLON    => true,
        Scala3Lexer.WITH     => true,
        Scala3Lexer.ASSIGN   => true,
        Scala3Lexer.ARROW    => true,
        Scala3Lexer.CTXARROW => true,
        Scala3Lexer.LARROW   => true,
        Scala3Lexer.WHILE    => true,
        Scala3Lexer.TRY      => true,
        Scala3Lexer.FOR      => true,
        Scala3Lexer.IF       => true,
        Scala3Lexer.THROW    => true,
        Scala3Lexer.RETURN   => true,
        _                    => false
    };

    // -----------------------------------------------------------------------
    // Indentation length
    // -----------------------------------------------------------------------

    private static int GetIndentLength(string text)
    {
        int length = 0;
        foreach (char c in text)
        {
            switch (c)
            {
                case ' ':  length++;                        break;
                case '\t': length = (length / 8 + 1) * 8; break;
                case '\f': length = 0;                     break;
            }
        }
        return length;
    }

    // -----------------------------------------------------------------------
    // Token queue helpers
    // -----------------------------------------------------------------------

    private void AddPendingToken(IToken token)
    {
        _previousPendingTokenType = token.Type;
        if (token.Channel == TokenConstants.DefaultChannel)
            _lastNonHiddenType = token.Type;
        _pendingTokens.AddLast(token);
    }

    private void HideAndAddPendingToken(IToken token)
    {
        var hidden = new CommonToken(token) { Channel = TokenConstants.HiddenChannel };
        AddPendingToken(hidden);
    }

    private void CreateAndAddPendingToken(int type, int channel, string text, IToken anchor)
    {
        var t = new CommonToken(anchor)
        {
            Type       = type,
            Channel    = channel,
            Text       = text,
            StartIndex = anchor.StartIndex,
            StopIndex  = anchor.StartIndex - 1  // zero-length synthetic token
        };
        AddPendingToken(t);
    }
}
