import org.antlr.v4.runtime.*;

import java.io.Writer;

public abstract class bnfParserBase extends Parser {
    private final TokenStream input;

    protected bnfParserBase(TokenStream input) {
	super(input);
	this.input = input;
    }

    public boolean NotNL() {
	int i = 1;
	Token c = ((CommonTokenStream) this.getInputStream()).LT(i);
	boolean v = c.getType() != bnfParser.NL;
	return v;
    }

    public boolean notAssign() {
	int i = 1;
	Token c = ((CommonTokenStream) this.getInputStream()).LT(i);
	while (c != null && c.getType() == bnfLexer.WS)
	    c = ((CommonTokenStream) this.getInputStream()).LT(++i);
	boolean v = !(c.getType() == bnfParser.ASSIGN1
		      || c.getType() == bnfParser.ASSIGN2
		      || c.getType() == bnfParser.ASSIGN3
		      || c.getType() == bnfParser.ASSIGN4);
	return v;
    }

    public boolean notLhs() {
	int i = 1;
	Token c = ((CommonTokenStream) this.getInputStream()).LT(i);
	while (c != null && c.getType() == bnfLexer.WS)
	    c = ((CommonTokenStream) this.getInputStream()).LT(++i);
	if (c != null && c.getType() != bnfLexer.X1)
	    return true;
	// '<'
	for (;;) {
	    while (c != null && c.getType() == bnfLexer.WS)
		c = ((CommonTokenStream) this.getInputStream()).LT(++i);
	    if (c != null && c.getType() != bnfLexer.ID && c.getType() != bnfLexer.X2)
		return true;
	    // ID
	    if (c == null) return true;
	    if (c.getType() == bnfLexer.X2)
		break;
	}
	// '>'
	while (c != null && c.getType() == bnfLexer.WS)
	    c = ((CommonTokenStream) this.getInputStream()).LT(++i);
	if (c.getType() == bnfLexer.ASSIGN1) return false;
	if (c.getType() == bnfLexer.ASSIGN2) return false;
	if (c.getType() == bnfLexer.ASSIGN3) return false;
	if (c.getType() == bnfLexer.ASSIGN4) return false;
	return true;
    }

}
