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

namespace LexerAdaptor
{
    using System;
    using System.IO;
    using System.Reflection;
    using Antlr4.Runtime;
    using Antlr4.Runtime.Misc;

    public abstract class LexerAdaptor : Lexer
    {
        // I copy a reference to the stream, so It can be used as a Char Stream, not as a IISStream
        readonly ICharStream stream;
        // Tokens are read only so I hack my way
        readonly FieldInfo tokenInput = typeof(CommonToken).GetField("_type", BindingFlags.NonPublic | BindingFlags.Instance);
        protected LexerAdaptor(ICharStream input)
             : base(input, Console.Out, Console.Error)
        {
            stream = input;
        }

        protected LexerAdaptor(ICharStream input, TextWriter output, TextWriter errorOutput)
             : base(input, output, errorOutput)
        {
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

        private bool InsideOptionsBlock { get; set; } = false;

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
                CurrentRuleType = (ANTLRv4Lexer.ARGUMENT_CONTENT);
            }
        }

        protected void handleEndAction()
        {
            PopMode();
            if (ModeStack.Count > 0)
            {
                CurrentRuleType = (ANTLRv4Lexer.ACTION_CONTENT);
            }
        }

        protected void handleOptionsLBrace()
        {
            if (InsideOptionsBlock)
            {
                CurrentRuleType = ANTLRv4Lexer.BEGIN_ACTION;
                PushMode(ANTLRv4Lexer.TargetLanguageAction);
            }
            else
            {
                CurrentRuleType = ANTLRv4Lexer.LBRACE;
                InsideOptionsBlock = true;
            }
        }


        public override IToken NextToken()
        {
            var token = base.NextToken();
            if (Type == ANTLRv4Lexer.ID)
            {
                char firstChar = stream.GetText(Interval.Of(TokenStartCharIndex, TokenStartCharIndex))[0];
                if (char.IsUpper(firstChar))
                {
                    Type = ANTLRv4Lexer.TOKEN_REF;
                    tokenInput.SetValue(token, ANTLRv4Lexer.TOKEN_REF);
                }
                if (char.IsLower(firstChar))
                {

                    Type = ANTLRv4Lexer.RULE_REF;
                    tokenInput.SetValue(token, ANTLRv4Lexer.RULE_REF);
                }

                if (CurrentRuleType == TokenConstants.InvalidType)
                { // if outside of rule def
                    CurrentRuleType = Type; // set to inside lexer or parser rule
                }
            }
            else if (Type == ANTLRv4Lexer.SEMI)
            { // exit rule def
                CurrentRuleType = TokenConstants.InvalidType;
            }

            return token;
        }

        private bool InLexerRule => CurrentRuleType == ANTLRv4Lexer.TOKEN_REF;


        private bool InParserRule => CurrentRuleType == ANTLRv4Lexer.RULE_REF;
        
    }
}
