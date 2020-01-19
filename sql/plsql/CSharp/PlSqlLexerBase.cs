using Antlr4.Runtime;

namespace PlSqlParseTree
{
    public abstract class PlSqlLexerBase : Lexer
    {
        public PlSqlLexerBase(ICharStream input)
            : base(input)
        {
        }

        protected bool IsNewlineAtPos(int pos)
        {
            int la = _input.La(pos);
            return la == -1 || la == '\n';
        }
    }
}