using Antlr4.Runtime;

    public abstract class PlSqlLexerBase : Lexer
    {
        public PlSqlLexerBase self;
        
        public PlSqlLexerBase(ICharStream input)
            : base(input)
        {
            self = this;
        }

        public bool IsNewlineAtPos(int pos)
        {
            int la = _input.La(pos);
            return la == -1 || la == '\n';
        }
    }
