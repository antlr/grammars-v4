import org.antlr.v4.runtime.*;

public abstract class bcplParserBase extends Parser {
    public bcplParserBase(TokenStream input){
        super(new ChannelCommonTokenStream(input));
    }

    public boolean IsNl()
    {
        Token c = ((ChannelCommonTokenStream)this.getInputStream()).LT(-1, 2);
        Token d = ((ChannelCommonTokenStream)this.getInputStream()).LT(1, 2);
        return c.getType() == bcplParser.NL;
    }

    public boolean IsNotNl()
    {
        Token c = ((ChannelCommonTokenStream)this.getInputStream()).LT(-1, 2);
        Token d = ((ChannelCommonTokenStream)this.getInputStream()).LT(1, 2);
        return d.getType() != bcplParser.NL;
    }
}
