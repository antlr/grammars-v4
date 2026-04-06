import org.antlr.v4.runtime.*;
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

    // -------------------------------------------------------------------------
    // Mode-stack helpers
    // -------------------------------------------------------------------------

    public int PeekMode()
    {
        return _modeStack.isEmpty() ? DEFAULT_MODE : _modeStack.peek();
    }

    @Override
    public int popMode()
    {
        if (_modeStack.isEmpty())
        {
            System.err.println("unbalanced ()/{}/[]");
            return DEFAULT_MODE;
        }
        return super.popMode();
    }

    public boolean PeekModeIs(int mode)
    {
        return PeekMode() == mode;
    }

    public boolean LookAheadIs(int pos, int value)
    {
        return getInputStream().LA(pos) == value;
    }

    public boolean LookAheadIsNot(int pos, int value)
    {
        return getInputStream().LA(pos) != value;
    }

    public boolean LookAheadIsRBrace1()    { return getInputStream().LA(1) == '}'; }
    public boolean LookAheadIsNotLBrace2() { return getInputStream().LA(2) != '{'; }
    public boolean PeekModeIsIrsCont()     { return PeekModeIs(CSharpLexer.IRS_CONT); }
    public boolean PeekModeIsIvsCont()     { return PeekModeIs(CSharpLexer.IVS_CONT); }

    public void WrapToken()
    {
        String text = getText();
        setText("\u3014" + text.replace("\u3015", "\u3015\u3015") + "\u3015");
    }

    // -------------------------------------------------------------------------
    // Preprocessor state
    // -------------------------------------------------------------------------
    private final Queue<Token>    _pending   = new ArrayDeque<>();
    private final HashSet<String> _symbols   = new HashSet<>();
    private final Deque<Boolean>  _condition = new ArrayDeque<>();
    private final Deque<Boolean>  _taken     = new ArrayDeque<>();

    // Expression evaluator cursor
    private List<Token> _expr = new ArrayList<>();
    private int         _epos;

    private void initPreprocessor()
    {
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
            if      (type == CSharpLexer.DEFINE)   handleDefine();
            else if (type == CSharpLexer.UNDEF)    handleUndef();
            else if (type == CSharpLexer.KW_IF)    skipped = handleIf();
            else if (type == CSharpLexer.ELIF)     skipped = handleElif();
            else if (type == CSharpLexer.KW_ELSE)  skipped = handleElse();
            else if (type == CSharpLexer.ENDIF)    handleEndif();
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
        collectLine();
        boolean alreadyTaken = !_taken.isEmpty() ? _taken.pop() : false;
        if (!_condition.isEmpty()) _condition.pop();
        boolean outer  = isActive();
        boolean result = !alreadyTaken && outer;
        _condition.push(result);
        _taken.push(true);
        return result ? null : skipFalseBlock();
    }

    private void handleEndif()
    {
        collectLine();
        if (!_condition.isEmpty()) _condition.pop();
        if (!_taken.isEmpty())    _taken.pop();
    }

    // -------------------------------------------------------------------------
    // collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    private List<Token> collectLine()
    {
        List<Token> tokens = new ArrayList<>();
        Token t;
        do
        {
            t = super.nextToken();
            if (t.getChannel() != Lexer.HIDDEN)
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
        int depth          = 1;
        boolean atLineStart = true;
        int startLine      = getLine();

        while (true)
        {
            int c = stream.LA(1);
            if (c == IntStream.EOF) break;

            if (c == '\r' || c == '\n' || c == 0x85 || c == 0x2028 || c == 0x2029)
            {
                stream.consume();
                sb.append((char)c);
                if (c == '\r' && stream.LA(1) == '\n')
                {
                    stream.consume();
                    sb.append('\n');
                }
                atLineStart = true;
                continue;
            }

            if (atLineStart && (c == ' ' || c == '\t'))
            {
                stream.consume();
                sb.append((char)c);
                continue;
            }

            if (atLineStart && c == '#')
            {
                String kw = peekKeyword(stream);
                if (kw.equals("if"))
                    depth++;
                else if (kw.equals("endif"))
                {
                    if (--depth == 0) break;
                }
                else if ((kw.equals("else") || kw.equals("elif")) && depth == 1)
                    break;
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
    // Recursive-descent expression evaluator
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
        while (peekType() == CSharpLexer.TK_OR_OR)  { eConsume(); v = parseAnd() || v; }
        return v;
    }

    private boolean parseAnd()
    {
        boolean v = parseEq();
        while (peekType() == CSharpLexer.TK_AND_AND) { eConsume(); v = parseEq() && v; }
        return v;
    }

    private boolean parseEq()
    {
        boolean v = parseUnary();
        if      (peekType() == CSharpLexer.TK_EQ_EQ) { eConsume(); return v == parseUnary(); }
        else if (peekType() == CSharpLexer.TK_NOT_EQ) { eConsume(); return v != parseUnary(); }
        return v;
    }

    private boolean parseUnary()
    {
        if (peekType() == CSharpLexer.TK_NOT) { eConsume(); return !parseUnary(); }
        return parsePrimary();
    }

    private boolean parsePrimary()
    {
        int t = peekType();
        if (t == CSharpLexer.TRUE)               { eConsume(); return true; }
        if (t == CSharpLexer.FALSE)              { eConsume(); return false; }
        if (t == CSharpLexer.CONDITIONAL_SYMBOL) { return _symbols.contains(eConsume().getText()); }
        if (t == CSharpLexer.TK_LPAREN)
        {
            eConsume();
            boolean v = parseOr();
            if (peekType() == CSharpLexer.TK_RPAREN) eConsume();
            return v;
        }
        return false;
    }
}
