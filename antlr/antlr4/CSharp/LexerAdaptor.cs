/*
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

using System;
using System.IO;
using System.Reflection;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;

public abstract class LexerAdaptor : Lexer
{
    private static readonly int PREQUEL_CONSTRUCT = -10;
    private static readonly int OPTIONS_CONSTRUCT = -11;

    // I copy a reference to the stream, so It can be used as a Char Stream, not as a IISStream
    readonly ICharStream stream;

    // Tokens are read only so I hack my way
    readonly FieldInfo tokenInput = typeof(CommonToken).GetField("_type", BindingFlags.NonPublic | BindingFlags.Instance);

    protected LexerAdaptor(ICharStream input)
         : base(input, Console.Out, Console.Error)
    {
        CurrentRuleType = TokenConstants.InvalidType;
        _insideOptionsBlock = false;
        stream = input;
    }

    protected LexerAdaptor(ICharStream input, TextWriter output, TextWriter errorOutput)
         : base(input, output, errorOutput)
    {
        CurrentRuleType = TokenConstants.InvalidType;
        _insideOptionsBlock = false;
        stream = input;
    }

    /**
     * Track whether we are inside of a rule and whether it is lexical parser. _currentRuleType==TokenConstants.InvalidType
     * means that we are outside of a rule. At the first sign of a rule name reference and _currentRuleType==invalid, we
     * can assume that we are starting a parser rule. Similarly, seeing a token reference when not already in rule means
     * starting a token rule. The terminating ';' of a rule, flips this back to invalid type.
     *
     * This is not perfect logic but works. For example, "grammar T;" means that we start and stop a lexical rule for
     * the "T;". Dangerous but works.
     *
     * The whole point of this state information is to distinguish between [..arg actions..] and [charsets]. Char sets
     * can only occur in lexical rules and arg actions cannot occur.
     */
    private int CurrentRuleType { get; set; } = TokenConstants.InvalidType;

    private bool _insideOptionsBlock;

    protected void handleBeginArgument()
    {
        if (InLexerRule)
        {
            PushMode(ANTLRv4Lexer.LexerCharSet);
            More();
        }
        else
        {
            PushMode(ANTLRv4Lexer.Argument);
        }
    }

    protected void handleEndArgument()
    {
        PopMode();
        if (ModeStack.Count > 0)
        {
            Type = ANTLRv4Lexer.ARGUMENT_CONTENT;
        }
    }

    protected void handleEndAction()
    {
        var oldMode = CurrentMode;
        var newMode = PopMode();

        if (ModeStack.Count > 0 && newMode == ANTLRv4Lexer.TargetLanguageAction && oldMode == newMode)
        {
            Type = ANTLRv4Lexer.ACTION_CONTENT;
        }
    }

    private bool InLexerRule
    {
        get { return CurrentRuleType == ANTLRv4Lexer.TOKEN_REF; }
    }

    public override IToken Emit()
    {
        if ((Type == ANTLRv4Lexer.OPTIONS || Type == ANTLRv4Lexer.TOKENS || Type == ANTLRv4Lexer.CHANNELS) && CurrentRuleType == TokenConstants.InvalidType)
        {
            // enter prequel construct ending with an RBRACE
            CurrentRuleType = PREQUEL_CONSTRUCT;
        }
        else if (Type == ANTLRv4Lexer.OPTIONS && CurrentRuleType == ANTLRv4Lexer.TOKEN_REF)
        {
            CurrentRuleType = OPTIONS_CONSTRUCT;
        }
        else if (Type == ANTLRv4Lexer.RBRACE && CurrentRuleType == PREQUEL_CONSTRUCT)
        {
            // exit prequel construct
            CurrentRuleType = TokenConstants.InvalidType;
        }
        else if (Type == ANTLRv4Lexer.RBRACE && CurrentRuleType == OPTIONS_CONSTRUCT)
        { // exit options
            CurrentRuleType = ANTLRv4Lexer.TOKEN_REF;
        }
        else if (Type == ANTLRv4Lexer.AT && CurrentRuleType == TokenConstants.InvalidType)
        {
            // enter action
            CurrentRuleType = ANTLRv4Lexer.AT;
        }
        else if (Type == ANTLRv4Lexer.SEMI && CurrentRuleType == OPTIONS_CONSTRUCT)
        { // ';' in options { .... }. Don't change anything.
        }
        else if (Type == ANTLRv4Lexer.END_ACTION && CurrentRuleType == ANTLRv4Lexer.AT)
        {
            // exit action
            CurrentRuleType = TokenConstants.InvalidType;
        }
        else if (Type == ANTLRv4Lexer.ID)
        {
            var firstChar = stream.GetText(Interval.Of(TokenStartCharIndex, TokenStartCharIndex))[0];
            if (char.IsUpper(firstChar))
            {
                Type = ANTLRv4Lexer.TOKEN_REF;
            }
            if (char.IsLower(firstChar))
            {
                Type = ANTLRv4Lexer.RULE_REF;
            }

            if (CurrentRuleType == TokenConstants.InvalidType)
            {
                // if outside of rule def
                CurrentRuleType = Type; // set to inside lexer or parser rule
            }
        }
        else if (Type == ANTLRv4Lexer.SEMI)
        {
            CurrentRuleType = TokenConstants.InvalidType;
        }

        return base.Emit();
    }

    public override void Reset()
    {
        CurrentRuleType = TokenConstants.InvalidType;
        _insideOptionsBlock = false;
        base.Reset();
    }
}
