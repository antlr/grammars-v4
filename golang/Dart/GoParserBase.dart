import 'package:antlr4/antlr4.dart';
import 'GoLexer.dart';

abstract class GoParserBase extends Parser
{
    GoParserBase(TokenStream input)
        : super(input)
    {
    }

    bool closingBracket()
    {
        var la = this.tokenStream.LA(1);
        return la == GoLexer.TOKEN_R_PAREN || la == GoLexer.TOKEN_R_CURLY || la == IntStream.EOF;
    }

    bool isType()
    {
        var la = tokenStream.LA(1);
        return la != GoLexer.TOKEN_IDENTIFIER;
    }

    bool isNotReceive()
    {
        var la = tokenStream.LA(1);
        return la != GoLexer.TOKEN_RECEIVE;
    }
}
