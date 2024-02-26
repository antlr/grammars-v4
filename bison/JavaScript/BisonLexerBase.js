import antlr4 from 'antlr4';
import BisonLexer from './BisonLexer.js';

export default class BisonLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);
        this.percent_percent_count = 0;
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
		    this.type = BisonLexer.PercentPercent;
		    return;
	    }
    }

    reset() {
        this.percent_percent_count = 0;
        super.reset();
    }
}

