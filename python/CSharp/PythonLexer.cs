using System.Collections.Generic;
using Antlr4.Runtime;

namespace PythonParseTree
{
    public abstract class PythonBaseLexer : Lexer
    {
        public static int TabSize { get; set; } = 8;

        // The amount of opened braces, brackets and parenthesis.
        private int _opened;

        // The stack that keeps track of the indentation level.
        private readonly Stack<int> _indents = new Stack<int>();

        // A list where extra tokens are pushed on (see the NEWLINE lexer rule).
        private int _tokensInd;
        private readonly List<IToken> _tokens = new List<IToken>();
        private IToken _lastToken;

        protected PythonBaseLexer(ICharStream charStream)
            : base(charStream)
        {
        }

        public override void Emit(IToken token)
        {
            base.Emit(token);
            _tokens.Add(token);
            _lastToken = token;
        }

        public override IToken NextToken()
        {
            // Check if the end-of-file is ahead and there are still some DEDENTS expected.
            if (_input.La(1) == Eof && _indents.Count > 0)
            {
                // Remove any trailing EOF tokens from our buffer.
                while (_tokensInd < _tokens.Count && _tokens[_tokensInd].Type == Eof)
                {
                    _tokens[_tokensInd] = null;
                    _tokensInd++;
                }

                if (_tokensInd < _tokens.Count && _tokens[_tokensInd].Type != PythonLexer.LINE_BREAK)
                {
                    // First emit an extra line break that serves as the end of the statement.
                    Emit(PythonLexer.LINE_BREAK);
                }

                // Now emit as much DEDENT tokens as needed.
                while (_indents.Count != 0)
                {
                    Emit(PythonLexer.DEDENT);
                    _indents.Pop();
                }

                // Put the EOF back on the token stream.
                Emit(Eof);
            }

            IToken next = base.NextToken();

            if (_tokens.Count == _tokensInd)
            {
                return next;
            }

            var result = _tokens[_tokensInd];
            _tokens[_tokensInd++] = null;
            return result;
        }

        protected void HandleNewLine()
        {
            Emit(PythonLexer.NEWLINE, Hidden, Text);

            char next = (char) _input.La(1);

            // Process whitespaces in HandleSpaces
            if (next != ' ' && next != '\t' && IsNotNewLineOrComment(next))
            {
                ProcessNewLine(0);
            }
        }

        protected void HandleSpaces()
        {
            char next = (char) _input.La(1);

            if ((_lastToken == null || _lastToken.Type == PythonLexer.NEWLINE) && IsNotNewLineOrComment(next))
            {
                // Calculates the indentation of the provided spaces, taking the
                // following rules into account:
                //
                // "Tabs are replaced (from left to right) by one to eight spaces
                //  such that the total number of characters up to and including
                //  the replacement is a multiple of eight [...]"
                //
                //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation

                int indent = 0;

                foreach (char c in Text)
                    indent += c == '\t' ? TabSize - indent % TabSize : 1;

                ProcessNewLine(indent);
            }

            Emit(PythonLexer.WS, Hidden, Text);
        }

        protected void IncIndentLevel() => _opened++;

        protected void DecIndentLevel()
        {
            if (_opened > 0)
            {
                --_opened;
            }
        }

        private bool IsNotNewLineOrComment(char next) =>
            _opened == 0 && next != '\r' && next != '\n' && next != '\f' && next != '#';

        private void ProcessNewLine(int indent)
        {
            Emit(PythonLexer.LINE_BREAK);

            int previous = _indents.Count == 0 ? 0 : _indents.Peek();

            if (indent > previous)
            {
                _indents.Push(indent);
                Emit(PythonLexer.INDENT);
            }
            else
            {
                // Possibly emit more than 1 DEDENT token.
                while (_indents.Count != 0 && _indents.Peek() > indent)
                {
                    Emit(PythonLexer.DEDENT);
                    _indents.Pop();
                }
            }
        }

        private void Emit(int tokenType, int channel = DefaultTokenChannel, string text = "")
        {
            IToken token =
#if LIGHT_TOKEN
                new PT.PM.AntlrUtils.LightToken((PT.PM.AntlrUtils.LightInputStream) _tokenFactorySourcePair.Item2,
                    tokenType,
                    channel, -1, CharIndex - text.Length, CharIndex);
#else
            new CommonToken(_tokenFactorySourcePair, tokenType, channel, CharIndex - text.Length, CharIndex)
            {
                Line = Line,
                Column = Column,
                Text = text
            };
#endif
            Emit(token);
        }
    }
}