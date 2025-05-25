import antlr4 from 'antlr4';
import gvprParser from './gvprParser.js';

export default class GvprParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
    }

    IsSemiRequired() {
		var c = this._input.LT(-1);
		var d = this._input.LT(1);
		return c.type !== gvprParser.CCBC;
    }

	IsSemiNotRequired() {
		var c = this._input.LT(-1);
		var d = this._input.LT(1);
		return c.type === gvprParser.CCBC;
	}
}
