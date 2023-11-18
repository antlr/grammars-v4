import {Parser, Lexer, Token, TokenStream, ParserRuleContext} from "antlr4";
import gvprParser from './gvprParser';
//import ParametersAndQualifiersContext from './CPP14Parser';

export default abstract class GvprParserBase extends Parser {

    constructor(input: TokenStream) {
        super(input);
    }

    IsSemiRequired() : boolean {
        var c = this._input.LT(-1);
        var d = this._input.LT(1);
        return c.type !== gvprParser.CCBC;
    }

    IsSemiNotRequired() : boolean {
        var c = this._input.LT(-1);
        var d = this._input.LT(1);
        return c.type === gvprParser.CCBC;
    }
}
