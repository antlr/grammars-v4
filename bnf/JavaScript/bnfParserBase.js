import antlr4 from 'antlr4';
import bnfParser from './bnfParser.js';
import bnfLexer from './bnfLexer.js';

export default class bnfParserBase extends antlr4.Parser {

	constructor(input) {
		super(input);
	}

	NotNL()
	{
		var i = 1;
		var c = this._input.LT(i);
		var v = c.Type != bnfParser.NL;
		return v;
	}
}
