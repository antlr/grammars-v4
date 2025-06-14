import { Parser } from "antlr4";
import LuaParser from './LuaParser.js';
export default class LuaParserBase extends Parser {
    constructor(input) {
        super(input);
    }
    IsFunctionCall() {
        const stream = this._input;
        var la = stream.LT(1);
        if (la.type !== LuaParser.NAME)
            return false;
        la = stream.LT(2);
        if (la.type === LuaParser.OP)
            return false;
        return true;
    }
}
