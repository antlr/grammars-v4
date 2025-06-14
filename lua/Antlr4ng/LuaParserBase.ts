import {Parser, Lexer, Token, TokenStream, ParserRuleContext} from "antlr4ng";
import { LuaLexer } from './LuaLexer.js';
import { LuaParser } from './LuaParser.js';

export abstract class LuaParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    IsFunctionCall(): boolean
    {
        var la = this.inputStream.LT(1);
        if (la.type !== LuaParser.NAME) return false;
	la = this.inputStream.LT(2);
        if (la.type === LuaParser.OP) return false;
        return true;
    }
}

