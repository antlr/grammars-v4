import org.antlr.v4.runtime.*;

public abstract class PostgreSQLParserBase extends Parser
{
    public PostgreSQLParserBase self;

    public PostgreSQLParserBase(TokenStream input) {
        super(input);
        self = this;
    }

}
