///{packageLine}

import org.antlr.v4.runtime.*;

public abstract class PlSqlLexerBase extends Lexer
{
    /** Line of the last emitted token (any channel), or -1 if none yet. */
    private int _lastTokenLine = -1;

    public PlSqlLexerBase(CharStream input)
    {
        super(input);
    }

    @Override
    public Token nextToken()
    {
        Token token = super.nextToken();
        if (token != null)
        {
            _lastTokenLine = token.getLine();
        }
        return token;
    }

    @Override
    public void reset()
    {
        _lastTokenLine = -1;
        super.reset();
    }

    protected boolean IsNewlineAtPos(int pos)
    {
        int la = _input.LA(pos);
        return la == -1 || la == '\n';
    }

    /**
     * Semantic predicate for {@code SQLPLUS_EXECUTE}: true when {@code /} is the
     * only non-whitespace character on its line (SQL*Plus separator), false
     * otherwise (division operator, matched by {@code SOLIDUS}).
     */
    protected boolean isOnlySymbolOnLine()
    {
        // Look-behind: previous token (any channel) must be on a different line.
        if (_lastTokenLine != -1 && _lastTokenLine == _tokenStartLine) {
            return false;
        }

        // Look-ahead: after '/', only whitespace until EOL/EOF.
        // Predicate runs before '/' is consumed, so LA(1) is '/' — scan from LA(2).
        int i = 2;
        while (true)
        {
            int c = _input.LA(i);
            if (c == -1 || c == '\n')
                break;
            if (!isWhitespace(c))
                return false;
            i++;
        }

        return true;
    }

    private static boolean isWhitespace(int c)
    {
        return c == ' ' || c == '\t' || c == '\r';
    }
}
