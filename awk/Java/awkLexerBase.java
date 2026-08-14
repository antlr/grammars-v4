import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

public abstract class awkLexerBase extends Lexer
{
    private boolean _afterExpr = false;

    public awkLexerBase(CharStream input)
    {
        super(input);
    }

    @Override
    public Token nextToken()
    {
        Token token = super.nextToken();
        if (token.getChannel() == Token.DEFAULT_CHANNEL)
        {
            this._afterExpr = token.getType() == awkLexer.WORD
                     || token.getType() == awkLexer.NUMBER
                     || token.getType() == awkLexer.STRING
                     || token.getType() == awkLexer.BUILTIN_FUNC_NAME
                     || token.getType() == awkLexer.INCR
                     || token.getType() == awkLexer.DECR
                     || token.getType() == awkLexer.Rp
                     || token.getType() == awkLexer.Rb;
        }
        return token;
    }

    public boolean IsNotAfterExpr()
    {
        return !this._afterExpr;
    }
}
