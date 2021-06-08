using Antlr4.Runtime;

namespace PlSqlParseTree
{
    public abstract class PlSqlParserBase : Parser
    {
        private bool _isVersion10 = false;
        private bool _isVersion12 = true;

        protected PlSqlParserBase(ITokenStream input)
            : base(input)
        {
        }

        public bool isVersion10() => _isVersion10;

        public bool isVersion12() => _isVersion12;

        public bool setVersion10(bool value) => _isVersion10 = value;

        public bool setVersion12(bool value) => _isVersion12 = value;
    }
}