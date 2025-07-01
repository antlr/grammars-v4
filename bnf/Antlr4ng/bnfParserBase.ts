import {Parser, Lexer, Token, TokenStream, ParserRuleContext} from "antlr4ng";
import { bnfParser } from './bnfParser.js';
import { bnfLexer } from './bnfLexer.js';

export default abstract class bnfParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    NotNL(): boolean
    {
        var i = 1;
        var c = this.inputStream.LT(i);
        var v = c.type != bnfParser.NL;
        return v;
    }
}
