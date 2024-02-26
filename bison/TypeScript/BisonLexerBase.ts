import { CommonToken, Lexer, CharStream, Token } from "antlr4";
import BisonLexer from './BisonLexer';

export default abstract class BisonLexerBase extends Lexer {
    percent_percent_count: number;

    constructor(input: CharStream) {
        super(input);
        this.percent_percent_count = 0;
    }

    reset() {
        this.percent_percent_count = 0;
        super.reset();
    }

    NextMode()
    {
	    ++this.percent_percent_count;
	    if (this.percent_percent_count == 1)
	    {
		    return;
	    } else if (this.percent_percent_count == 2)
	    {
		    this.pushMode(BisonLexer.EpilogueMode);
		    return;
	    } else
	    {
		    this._type = BisonLexer.PercentPercent;
		    return;
	    }
    }
}

