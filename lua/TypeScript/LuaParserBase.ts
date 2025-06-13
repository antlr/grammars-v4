import {Parser, Lexer, Token, TokenStream, BufferedTokenStream, ParserRuleContext} from "antlr4";
import LuaLexer from './LuaLexer';
import LuaParser from './LuaParser';

export default abstract class LuaParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    IsFunctionCall(): boolean
    {
        const stream = this._input as BufferedTokenStream;
        var la = stream.LT(1);
        if (la.type !== LuaParser.NAME) return false;
	la = stream.LT(2);
        if (la.type === LuaParser.OP) return false;
        return true;
    }
}

