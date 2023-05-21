import org.antlr.v4.runtime.*;

public class ChannelCommonTokenStream extends CommonTokenStream
{
    public ChannelCommonTokenStream(TokenSource tokenSource)
    {
        super(tokenSource);
    }


    public ChannelCommonTokenStream(TokenStream input)
    {
        this(((CommonTokenStream)input).getTokenSource());
    }

    protected int myPreviousTokenOnChannel(int i, int channel) {
        sync(i);
        if (i >= size()) {
                        // the EOF token is on every channel
            return size() - 1;
        }

        while (i >= 0) {
            Token token = tokens.get(i);
            if (token.getType() == Token.EOF || token.getChannel() == channel || token.getChannel() == Lexer.DEFAULT_TOKEN_CHANNEL) {
                return i;
            }

            i--;
        }

        return i;
    }

    protected int myNextTokenOnChannel(int i, int channel) {
        sync(i);
        if (i >= size()) {
            return size() - 1;
        }

        Token token = tokens.get(i);
        while ( token.getChannel() != channel && token.getChannel() != Lexer.DEFAULT_TOKEN_CHANNEL ) {
            if ( token.getType()==Token.EOF ) {
                return i;
            }

            i++;
            sync(i);
            token = tokens.get(i);
        }

        return i;
    }

    protected Token LB(int k, int ch)
    {
        if (k == 0 || (p - k) < 0)
        {
            return null;
        }
        int i = p;
        int n = 1;
        // find k good tokens looking backwards
        while (n <= k)
        {
            // skip off-channel tokens
            i = myPreviousTokenOnChannel(i - 1, ch);
            n++;
        }
        if (i < 0)
        {
            return null;
        }
        return tokens.get(i);
    }

    public Token LT(int k, int ch)
    {
        //System.out.println("enter LT("+k+")");
        lazyInit();
        if (k == 0)
        {
            return null;
        }
        if (k < 0)
        {
            return LB(-k, ch);
        }
        int i = p;
        int n = 1;
        // we know tokens[p] is a good one
        // find k good tokens
        while (n < k)
        {
            // skip off-channel tokens, but make sure to not look past EOF
            if (sync(i + 1))
            {
                i = myNextTokenOnChannel(i + 1, ch);
            }
            n++;
        }
        //              if ( i>range ) range = i;
        return tokens.get(i);
    }
}
