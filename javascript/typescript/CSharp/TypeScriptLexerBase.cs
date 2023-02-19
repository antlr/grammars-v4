using System;
using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;
using static TypeScriptParser;

/// <summary>
/// All lexer methods that used in grammar (IsStrictMode)
/// should start with Upper Case Char similar to Lexer rules.
/// </summary>
public abstract class TypeScriptLexerBase : Lexer
{
    /// <summary>
    /// Stores values of nested modes. By default mode is strict or
    /// defined externally(useStrictDefault)
    /// </summary>
    private readonly Stack<bool> _scopeStrictModes = new Stack<bool>();

    private IToken _lastToken;

    /// <summary>
    /// Default value of strict mode
    /// Can be defined externally by changing UseStrictDefault
    /// </summary>
    private bool _useStrictDefault;

    /// <summary>
    /// Current value of strict mode
    /// Can be defined during parsing, see StringFunctions.js and StringGlobal.js samples
    /// </summary>
    private bool _useStrictCurrent;

    /// <summary>
    /// Keeps track of the current depth of nested template string backticks.
    /// E.g. after the X in:
    ///
    /// `${a ? `${X
    ///
    /// templateDepth will be 2. This variable is needed to determine if a `}` is a
    /// plain CloseBrace, or one that closes an expression inside a template string.
    /// </summary>
    private int _templateDepth = 0;

    /// <summary>
    /// Keeps track of the depth of open- and close-braces. Used for expressions like:
    ///
    /// `${[1, 2, 3].map(x => { return x * 2;}).join("")}`
    ///
    /// where the '}' from `return x * 2;}` should not become a `TemplateCloseBrace`
    /// token but rather a `CloseBrace` token.
    /// </summary>
    private int _bracesDepth = 0;

    public TypeScriptLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public bool UseStrictDefault
    {
        get => _useStrictDefault;

        set
        {
            _useStrictDefault = value;
            _useStrictCurrent = value;
        }
    }

    public bool IsStartOfFile()
    {
        return _lastToken == null;
    }

    public bool IsStrictMode()
    {
        return _useStrictCurrent;
    }

    public void StartTemplateString()
    {
      _bracesDepth = 0;
    }

    public bool IsInTemplateString()
    {
        return _templateDepth > 0 && _bracesDepth == 0;
    }

    /// <summary>
    /// Return the next token from the character stream and records this last
    /// token in case it resides on the default channel. This recorded token
    /// is used to determine when the lexer could possibly match a regex
    /// literal.
    /// 
    /// </summary>
    /// <returns>
    /// The next token from the character stream.
    /// </returns>
    public override IToken NextToken()
    {
        // Get the next token.
        IToken next = base.NextToken();

        if (next.Channel == DefaultTokenChannel)
        {
            // Keep track of the last token on the default channel.
            _lastToken = next;
        }

        return next;
    }

    protected void ProcessOpenBrace()
    {
        _bracesDepth++;
        _useStrictCurrent = (_scopeStrictModes.Count > 0 && _scopeStrictModes.Peek()) || UseStrictDefault;
        _scopeStrictModes.Push(_useStrictCurrent);
    }

    protected void ProcessCloseBrace()
    {
        _bracesDepth--;
        _useStrictCurrent = _scopeStrictModes.Count > 0 ? _scopeStrictModes.Pop() : UseStrictDefault;
    }

    protected void ProcessStringLiteral()
    {
        if (_lastToken == null || _lastToken.Type == OpenBrace)
        {
            if (Text.Equals("\"use strict\"", StringComparison.InvariantCulture) ||
                Text.Equals("'use strict'", StringComparison.InvariantCulture))
            {
                if (_scopeStrictModes.Count > 0)
                {
                    _scopeStrictModes.Pop();
                }

                _useStrictCurrent = true;
                _scopeStrictModes.Push(_useStrictCurrent);
            }
        }
    }

    public void IncreaseTemplateDepth()
    {
        _templateDepth++;
    }

    public void DecreaseTemplateDepth()
    {
        _templateDepth--;
    }

    /// <summary>
    /// Returns true if the lexer can match a regex literal.
    /// </summary>
    /// <returns>bool</returns>
    protected bool IsRegexPossible()
    {
        if (_lastToken == null)
        {
            // No token has been produced yet: at the start of the input,
            // no division is possible, so a regex literal _is_ possible.
            return true;
        }

        switch (_lastToken.Type)
        {
            case Identifier:
            case NullLiteral:
            case BooleanLiteral:
            case This:
            case CloseBracket:
            case CloseParen:
            case OctalIntegerLiteral:
            case DecimalLiteral:
            case HexIntegerLiteral:
            case StringLiteral:
            case PlusPlus:
            case MinusMinus:
                // After any of the tokens above, no regex literal can follow.
                return false;
            default:
                // In all other cases, a regex literal _is_ possible.
                return true;
        }
    }
}
