import org.antlr.v4.runtime.*;
import java.util.ArrayDeque;
import java.util.Deque;

abstract class CSharpLexerBase extends Lexer
{
    protected CSharpLexerBase(CharStream input)
    {
        super(input);
    }

    protected int interpolatedStringLevel;
    protected final Deque<Boolean> interpolatedVerbatiums = new ArrayDeque<>();
    protected final Deque<Integer> curlyLevels = new ArrayDeque<>();
    protected boolean verbatium;

    protected void OnInterpolatedRegularStringStart()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.push(false);
        verbatium = false;
    }

    protected void OnInterpolatedVerbatiumStringStart()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.push(true);
        verbatium = true;
    }

    protected void OnOpenBrace()
    {
        if (interpolatedStringLevel > 0)
        {
            curlyLevels.push(curlyLevels.pop() + 1);
        }
    }

    protected void OnCloseBrace()
    {

        if (interpolatedStringLevel > 0)
        {
            curlyLevels.push(curlyLevels.pop() - 1);
            if (curlyLevels.peek() == 0)
            {
                curlyLevels.pop();
                skip();
                popMode();
            }
        }
    }

    protected void OnColon()
    {

        if (interpolatedStringLevel > 0)
        {
            int ind = 1;
            boolean switchToFormatString = true;
            while ((char)_input.LA(ind) != '}')
            {
                if (_input.LA(ind) == ':' || _input.LA(ind) == ')')
                {
                    switchToFormatString = false;
                    break;
                }
                ind++;
            }
            if (switchToFormatString)
            {
                mode(CSharpLexer.INTERPOLATION_FORMAT);
            }
        }
    }

    protected void OpenBraceInside()
    {
        curlyLevels.push(1);
    }

    protected void OnDoubleQuoteInside()
    {
        interpolatedStringLevel--;
        interpolatedVerbatiums.pop();
        verbatium = (interpolatedVerbatiums.size() > 0 ? interpolatedVerbatiums.peek() : false);
    }

    protected void OnCloseBraceInside()
    {
        curlyLevels.pop();
    }

    protected boolean IsRegularCharInside()
    {
        return !verbatium;
    }

    protected boolean IsVerbatiumDoubleQuoteInside()
    {
        return verbatium;
    }
}
