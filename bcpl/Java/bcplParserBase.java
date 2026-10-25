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

    // Guards for the "(semi X)*" loops so that a trailing blank line (which
    // satisfies the zero-width `nl` alternative of `semi`) does not cause the
    // loop to commit to another iteration when there is no further X to parse.
    public boolean IsDeclarationStart()
    {
        int la = this.getInputStream().LA(1);
        return la == bcplParser.KLET || la == bcplParser.KMANIFEST || la == bcplParser.KGLOBAL || la == bcplParser.KSTATIC;
    }

    public boolean IsNotCB()
    {
        int la = this.getInputStream().LA(1);
        return la != bcplParser.CB;
    }
}
