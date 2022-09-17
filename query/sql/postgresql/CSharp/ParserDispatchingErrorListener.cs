using Antlr4.Runtime;
using System.IO;

class ParserDispatchingErrorListener : IAntlrErrorListener<IToken>
{
    Parser _parent;

    public ParserDispatchingErrorListener(Parser parent)
    {
        _parent = parent;
    }
    public void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
    {
        var foo = new ProxyErrorListener<IToken>(_parent.ErrorListeners);
        foo.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e);
    }
}
