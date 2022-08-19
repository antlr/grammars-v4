using Antlr4.Runtime;
using System.IO;

class LexerDispatchingErrorListener : IAntlrErrorListener<int>
{
    Lexer _parent;

    public LexerDispatchingErrorListener(Lexer parent)
    {
        _parent = parent;
    }

    public void SyntaxError(TextWriter output, IRecognizer recognizer, int offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
    {
        var foo = new ProxyErrorListener<int>(_parent.ErrorListeners);
        foo.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e);
    }
}
