using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

internal class ChannelCommonTokenStream : CommonTokenStream
{
    ITokenSource original = null;

    public ChannelCommonTokenStream(ITokenSource tokenSource)
        : base(tokenSource)
    {
    }

    public ChannelCommonTokenStream(ITokenStream input)
        : this(PrepareBaseParameters(input))
    {
        CommonTokenStream i = input as CommonTokenStream;
        original = i.TokenSource;
    }

    private ChannelCommonTokenStream(FatData fd)
        : base(fd.input.TokenSource)
    {
    }

    private static FatData PrepareBaseParameters(ITokenStream input)
    {
        var fd = new FatData(input);
        return fd;
    }

    private readonly record struct FatData(ITokenStream input);

    protected int MyPreviousTokenOnChannel(int i, int channel)
    {
        Sync(i);
        if (i >= Size)
        {
            // the EOF token is on every channel
            return Size - 1;
        }
        while (i >= 0)
        {
            IToken token = tokens[i];
            if (token.Type == TokenConstants.EOF || token.Channel == channel || token.Channel == Lexer.DefaultTokenChannel)
            {
                return i;
            }
            i--;
        }
        return i;
    }

    protected int MyNextTokenOnChannel(int i, int channel)
    {
        Sync(i);
        if (i >= Size)
        {
            return Size - 1;
        }
        IToken token = tokens[i];
        while (token.Channel != channel && token.Channel != Lexer.DefaultTokenChannel)
        {
            if (token.Type == TokenConstants.EOF)
            {
                return i;
            }
            i++;
            Sync(i);
            token = tokens[i];
        }
        return i;
    }
    
    protected internal IToken Lb(int k, int ch)
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
            i = MyPreviousTokenOnChannel(i - 1, ch);
            n++;
        }
        if (i < 0)
        {
            return null;
        }
        return tokens[i];
    }

    public IToken LT(int k, int ch)
    {
        //System.out.println("enter LT("+k+")");
        LazyInit();
        if (k == 0)
        {
            return null;
        }
        if (k < 0)
        {
            return Lb(-k, ch);
        }
        int i = p;
        int n = 1;
        // we know tokens[p] is a good one
        // find k good tokens
        while (n < k)
        {
            // skip off-channel tokens, but make sure to not look past EOF
            if (Sync(i + 1))
            {
                i = MyNextTokenOnChannel(i + 1, ch);
            }
            n++;
        }
        //      if ( i>range ) range = i;
        return tokens[i];
    }
}
