/*
 * Project      : a helper class to implement specific PEG grammar expressions in an ANTLR4 grammar
 *
 * Developed by : Robert Einhorn
 */

// Related PEG grammar expressions:
// &e
// https://peps.python.org/pep-0617/#e-3
//
// !e
// https://peps.python.org/pep-0617/#e-4

import org.antlr.v4.runtime.*;

public abstract class PythonParserBase extends Parser {
    protected PythonParserBase(TokenStream input) {
        super(input);
    }

    public PythonParserBase self = this; // for compatibility with PythonParserBase.py

    // https://docs.python.org/3/reference/lexical_analysis.html#soft-keywords
    public boolean isEqualCurrentTokenText(String tokenText) {
        return getCurrentToken().getText().equals(tokenText);
    }

    public boolean isnotEqualCurrentTokenText(String tokenText) {
        return !isEqualCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
