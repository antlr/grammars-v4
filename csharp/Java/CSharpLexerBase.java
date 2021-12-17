import org.antlr.v4.runtime.*;
import java.util.Stack;

abstract class CSharpLexerBase extends Lexer
{
    protected CSharpLexerBase(CharStream input)
    {
        super(input);
    }

    protected int interpolatedStringLevel;
    protected Stack<Boolean> interpolatedVerbatiums = new Stack<Boolean>();
    protected Stack<Integer> curlyLevels = new Stack<Integer>();
    protected boolean verbatium;

    protected void Action1()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.push(false);
        verbatium = false;
    }

    protected void Action2()
    {
        interpolatedStringLevel++;
        interpolatedVerbatiums.push(true);
        verbatium = true;
    }

    protected void Action3()
    {
        if (interpolatedStringLevel > 0)
        {
            curlyLevels.push(curlyLevels.pop() + 1);
        }
    }

    protected void Action4()
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

    protected void Action5()
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

    protected void Action6()
    {
        curlyLevels.push(1);
    }

    protected void Action7()
    {
        interpolatedStringLevel--;
        interpolatedVerbatiums.pop();
        verbatium = (interpolatedVerbatiums.size() > 0 ? interpolatedVerbatiums.peek() : false);
    }

    protected void Action8()
    {
        curlyLevels.pop();
    }

    protected boolean Pred1()
    {
        return !verbatium;
    }

    protected boolean Pred2()
    {
        return verbatium;
    }
}
