import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.Pair;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Queue;

abstract class CSharpLexerBase extends Lexer
{
    protected CSharpLexerBase(CharStream input)
    {
        super(input);
        initPreprocessor();
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
            while ((char)getInputStream().LA(ind) != '}')
            {
                if (getInputStream().LA(ind) == ':' || getInputStream().LA(ind) == ')')
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

    // -------------------------------------------------------------------------
    // Preprocessor state
    // -------------------------------------------------------------------------
    private final Queue<Token>    _pending   = new ArrayDeque<>();
    private final HashSet<String> _symbols   = new HashSet<>();
    private final Deque<Boolean>  _condition = new ArrayDeque<>(); // is current section active?
    private final Deque<Boolean>  _taken     = new ArrayDeque<>(); // was any branch taken at this level?

    // Expression evaluator cursor (reused per Evaluate call)
    private List<Token> _expr = new ArrayList<>();
    private int         _epos;

    private void initPreprocessor()
    {
        // Pre-populate from --DSYM or --DSYM;SYM2 command-line arguments (mirrors javac -D / csc /define:)
        for (String arg : System.getProperty("sun.java.command", "").split(" "))
        {
            if (arg.startsWith("--D"))
            {
                for (String sym : arg.substring(3).split(";"))
                    if (!sym.isEmpty()) _symbols.add(sym);
            }
        }
    }

    private boolean isActive()
    {
        return _condition.isEmpty() || _condition.peek();
    }

    // -------------------------------------------------------------------------
    // nextToken override — intercepts DIRECTIVE-channel tokens
    // -------------------------------------------------------------------------
    @Override
    public Token nextToken()
    {
        if (!_pending.isEmpty()) return _pending.poll();

        Token tok = super.nextToken();

        if (tok.getChannel() == CSharpLexer.DIRECTIVE)
        {
            Token skipped = null;
            int type = tok.getType();
            if      (type == CSharpLexer.DEFINE) handleDefine();
            else if (type == CSharpLexer.UNDEF)  handleUndef();
            else if (type == CSharpLexer.IF)     skipped = handleIf();
            else if (type == CSharpLexer.ELIF)   skipped = handleElif();
            else if (type == CSharpLexer.ELSE)   skipped = handleElse();
            else if (type == CSharpLexer.ENDIF)  handleEndif();
            if (skipped != null) _pending.add(skipped);
        }

        return tok;
    }

    // -------------------------------------------------------------------------
    // Directive handlers
    // -------------------------------------------------------------------------
    private void handleDefine()
    {
        List<Token> line = collectLine();
        String sym = symbolFromLine(line);
        if (isActive() && sym != null) _symbols.add(sym);
    }

    private void handleUndef()
    {
        List<Token> line = collectLine();
        String sym = symbolFromLine(line);
        if (isActive() && sym != null) _symbols.remove(sym);
    }

    private Token handleIf()
    {
        List<Token> line = collectLine();
        boolean outer  = isActive();
        boolean result = outer && evaluate(line);
        _condition.push(result);
        _taken.push(result);
        return result ? null : skipFalseBlock();
    }

    private Token handleElif()
    {
        List<Token> line = collectLine();
        boolean alreadyTaken = !_taken.isEmpty() ? _taken.pop() : false;
        if (!_condition.isEmpty()) _condition.pop();
        boolean outer  = isActive();
        boolean result = !alreadyTaken && outer && evaluate(line);
        _condition.push(result);
        _taken.push(alreadyTaken || result);
        return result ? null : skipFalseBlock();
    }

    private Token handleElse()
    {
        collectLine(); // consume trailing whitespace / newline; no expression
        boolean alreadyTaken = !_taken.isEmpty() ? _taken.pop() : false;
        if (!_condition.isEmpty()) _condition.pop();
        boolean outer  = isActive();
        boolean result = !alreadyTaken && outer;
        _condition.push(result);
        _taken.push(true); // #else is always the final branch at this level
        return result ? null : skipFalseBlock();
    }

    private void handleEndif()
    {
        collectLine(); // consume trailing whitespace / newline
        if (!_condition.isEmpty()) _condition.pop();
        if (!_taken.isEmpty())    _taken.pop();
    }

    // -------------------------------------------------------------------------
    // collectLine — drain DIRECTIVE_MODE tokens up to and including DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    private List<Token> collectLine()
    {
        List<Token> tokens = new ArrayList<>();
        Token t;
        do
        {
            t = super.nextToken();
            // Drop hidden-channel whitespace (DIRECTIVE_WHITESPACES) and comments
            if (t.getChannel() != Lexer.HIDDEN && t.getChannel() != CSharpLexer.COMMENTS_CHANNEL)
                tokens.add(t);
        }
        while (t.getType() != CSharpLexer.DIRECTIVE_NEW_LINE && t.getType() != Token.EOF);
        return tokens;
    }

    private static String symbolFromLine(List<Token> line)
    {
        for (Token t : line)
            if (t.getType() == CSharpLexer.CONDITIONAL_SYMBOL) return t.getText();
        return null;
    }

    // -------------------------------------------------------------------------
    // skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    // -------------------------------------------------------------------------
    private Token skipFalseBlock()
    {
        StringBuilder sb   = new StringBuilder();
        CharStream stream   = getInputStream();
        int depth          = 1;   // nesting depth; we start inside the false #if (depth 1)
        boolean atLineStart = true;
        int startLine      = getLine();

        while (true)
        {
            int c = stream.LA(1);
            if (c == IntStream.EOF) break;

            // Newline — reset line-start flag
            if (c == '\r' || c == '\n' || c == 0x85 || c == 0x2028 || c == 0x2029)
            {
                stream.consume();
                sb.append((char)c);
                if (c == '\r' && stream.LA(1) == '\n') // \r\n — consume as one newline
                {
                    stream.consume();
                    sb.append('\n');
                }
                atLineStart = true;
                continue;
            }

            // Leading whitespace on a fresh line — keep atLineStart true
            if (atLineStart && (c == ' ' || c == '\t'))
            {
                stream.consume();
                sb.append((char)c);
                continue;
            }

            // Potential preprocessor directive
            if (atLineStart && c == '#')
            {
                String kw = peekKeyword(stream);
                if (kw.equals("if"))
                {
                    depth++;
                }
                else if (kw.equals("endif"))
                {
                    if (--depth == 0)
                        break; // leave '#' in stream; handleEndif pops the stacks
                }
                else if ((kw.equals("else") || kw.equals("elif")) && depth == 1)
                {
                    break; // leave '#' in stream; handleElse/handleElif will handle
                }
                // else: nested #else/#elif inside a deeper #if — consume normally
            }

            atLineStart = false;
            stream.consume();
            sb.append((char)c);
        }

        CommonToken tok = new CommonToken(CSharpLexer.SKIPPED_SECTION, sb.toString());
        tok.setChannel(Lexer.HIDDEN);
        tok.setLine(startLine);
        return tok;
    }

    // Peek at the directive keyword after '#' at LA(1) without consuming.
    private static String peekKeyword(CharStream stream)
    {
        int i = 2; // LA(1) is '#'
        while (stream.LA(i) == ' ' || stream.LA(i) == '\t') i++;
        StringBuilder sb = new StringBuilder();
        int c;
        while ((c = stream.LA(i)) != -1 && Character.isLetter(c))
        {
            sb.append((char)c);
            i++;
        }
        return sb.toString();
    }

    // -------------------------------------------------------------------------
    // Recursive-descent expression evaluator over DIRECTIVE_MODE token list
    // -------------------------------------------------------------------------
    private boolean evaluate(List<Token> tokens)
    {
        _expr = tokens;
        _epos = 0;
        return parseOr();
    }

    private int peekType()
    {
        if (_epos < _expr.size())
        {
            int t = _expr.get(_epos).getType();
            if (t != CSharpLexer.DIRECTIVE_NEW_LINE && t != Token.EOF) return t;
        }
        return -1;
    }

    private Token eConsume() { return _expr.get(_epos++); }

    private boolean parseOr()
    {
        boolean v = parseAnd();
        while (peekType() == CSharpLexer.OP_OR)  { eConsume(); v = parseAnd() || v; }
        return v;
    }

    private boolean parseAnd()
    {
        boolean v = parseEq();
        while (peekType() == CSharpLexer.OP_AND) { eConsume(); v = parseEq() && v; }
        return v;
    }

    private boolean parseEq()
    {
        boolean v = parseUnary();
        if      (peekType() == CSharpLexer.OP_EQ) { eConsume(); return v == parseUnary(); }
        else if (peekType() == CSharpLexer.OP_NE) { eConsume(); return v != parseUnary(); }
        return v;
    }

    private boolean parseUnary()
    {
        if (peekType() == CSharpLexer.BANG) { eConsume(); return !parseUnary(); }
        return parsePrimary();
    }

    private boolean parsePrimary()
    {
        int t = peekType();
        if (t == CSharpLexer.TRUE)               { eConsume(); return true; }
        if (t == CSharpLexer.FALSE)              { eConsume(); return false; }
        if (t == CSharpLexer.CONDITIONAL_SYMBOL) { return _symbols.contains(eConsume().getText()); }
        if (t == CSharpLexer.OPEN_PARENS)
        {
            eConsume();
            boolean v = parseOr();
            if (peekType() == CSharpLexer.CLOSE_PARENS) eConsume();
            return v;
        }
        return false; // malformed expression
    }
}
