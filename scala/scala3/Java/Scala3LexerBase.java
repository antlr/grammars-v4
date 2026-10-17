// Scala 3 INDENT/DEDENT injection for ANTLR4
// Modelled on Dotty (Scala 3 compiler) Scanners.scala handleNewLine logic.
//
// Region model
// ------------
// The injector tracks a stack of "regions":
//   TopLevel  – outermost scope
//   Indented  – inside an indentation block (INDENT … DEDENT)
//   InBraces  – inside {…}  — NEWLINE surfaced as separator; no INDENT/DEDENT
//   InParens  – inside (…) or […] — newlines suppressed entirely
//
// On each NEWLINE:
//   InParens              → suppress (implicit line joining)
//   InBraces              → keep NEWLINE on default channel if canEndStat(prev)
//                                  && canStartStat(next) && !isStatCtd(next)
//   Indented / TopLevel:
//     indent increases    → suppress NEWLINE, then INDENT if canStartIndent(prev)
//     indent unchanged    → keep NEWLINE on default channel if conditions met
//     indent decreases    → suppress NEWLINE, DEDENT(s), then re-emit NEWLINE
//                           on default channel if outer context needs it
//
// The NEWLINE token itself is used as the statement separator (mirroring Dotty's
// virtual NEWLINE/NEWLINES tokens).  No synthetic token is injected; the raw
// NEWLINE is simply kept on the default channel or suppressed to the hidden
// channel.  Grammar rule  end_of_stat : NEWLINE+ | SEMI  handles both.
//
// Dotty token-set predicates (canEndStatTokens, canStartStatTokens,
// isStatCtdTokens, canStartIndentTokens) are implemented as if-else chains.

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedList;

public abstract class Scala3LexerBase extends Lexer {

    // Set to true when --3.0-migration is passed on the command line.
    // Enables Scala 2-compatible syntax (._  wildcard imports, [_] type wildcards).
    public static final boolean migration30 = checkMigration30();

    private static boolean checkMigration30() {
        String cmd = System.getProperty("sun.java.command", "");
        for (String arg : cmd.split("\\s+")) {
            if (arg.equalsIgnoreCase("--3.0-migration")) return true;
        }
        return false;
    }

    private enum Region { TopLevel, Indented, InBraces, InParens }

    // Region stack: bottom = TopLevel, top = innermost.
    private Deque<Region> regionStack = new ArrayDeque<>();
    // Indentation lengths for Indented regions only (parallel to regionStack Indented entries).
    private Deque<Integer> indentLengthStack = new ArrayDeque<>();
    // Queue of tokens waiting to be returned by nextToken().
    private LinkedList<Token> pendingTokens = new LinkedList<>();

    private int previousPendingTokenType = 0;
    // Last token type sent on the default channel — used for canEndStat checks.
    private int lastNonHiddenType = 0;
    // Two-token look-ahead.
    private Token curToken = null;
    private Token ffgToken = null;

    protected Scala3LexerBase(CharStream input) {
        super(input);
    }

    @Override
    public Token nextToken() {
        checkNextToken();
        Token token = pendingTokens.getFirst();
        pendingTokens.removeFirst();
        return token;
    }

    @Override
    public void reset() {
        init();
        super.reset();
    }

    // -----------------------------------------------------------------------
    // Initialisation
    // -----------------------------------------------------------------------

    private void init() {
        regionStack        = new ArrayDeque<>();
        indentLengthStack  = new ArrayDeque<>();
        pendingTokens      = new LinkedList<>();
        previousPendingTokenType = 0;
        lastNonHiddenType  = 0;
        curToken           = null;
        ffgToken           = null;
    }

    // -----------------------------------------------------------------------
    // Core dispatch loop
    // -----------------------------------------------------------------------

    private void checkNextToken() {
        if (previousPendingTokenType == Token.EOF)
            return;

        if (regionStack.isEmpty()) { // very first call
            regionStack.push(Region.TopLevel);
            indentLengthStack.push(0); // base indentation for TopLevel
            setCurrentAndFollowingTokens();
            handleStartOfInput();
        } else {
            setCurrentAndFollowingTokens();
        }

        int type = curToken.getType();
        if (type == Scala3Lexer.NEWLINE) {
            handleNewlineToken();
        } else if (type == Scala3Lexer.LPAREN || type == Scala3Lexer.LBRACKET) {
            regionStack.push(Region.InParens);
            addPendingToken(curToken);
        } else if (type == Scala3Lexer.COMMA) {
            // Drain any Indented regions opened inside InParens (e.g. a same-line
            // colonArgument body: `f: u => expr,` — the INDENT was emitted but no
            // NEWLINE-triggered DEDENT ran before the comma).
            while (!regionStack.isEmpty() && regionStack.peek() == Region.Indented) {
                indentLengthStack.pop();
                regionStack.pop();
                createAndAddPendingToken(Scala3Lexer.DEDENT, Token.DEFAULT_CHANNEL, "<DEDENT>", curToken);
            }
            addPendingToken(curToken);
        } else if (type == Scala3Lexer.RPAREN || type == Scala3Lexer.RBRACKET) {
            // Drain any Indented regions opened inside InParens (e.g. a
            // colonArgument body whose closing ')' is on the same source
            // line as the body text, so no NEWLINE-triggered DEDENT ran).
            while (!regionStack.isEmpty() && regionStack.peek() == Region.Indented) {
                indentLengthStack.pop();
                regionStack.pop();
                createAndAddPendingToken(Scala3Lexer.DEDENT, Token.DEFAULT_CHANNEL, "<DEDENT>", curToken);
            }
            if (!regionStack.isEmpty() && regionStack.peek() == Region.InParens)
                regionStack.pop();
            addPendingToken(curToken);
        } else if (type == Scala3Lexer.LBRACE) {
            regionStack.push(Region.InBraces);
            addPendingToken(curToken);
        } else if (type == Scala3Lexer.RBRACE) {
            if (!regionStack.isEmpty() && regionStack.peek() == Region.InBraces)
                regionStack.pop();
            addPendingToken(curToken);
        } else if (type == Token.EOF) {
            handleEofToken();
        } else {
            addPendingToken(curToken);
        }
    }

    private void setCurrentAndFollowingTokens() {
        curToken = (ffgToken != null) ? ffgToken : super.nextToken();
        ffgToken = (curToken.getType() == Token.EOF) ? curToken : super.nextToken();
    }

    // -----------------------------------------------------------------------
    // Start-of-input: skip leading blank lines
    // -----------------------------------------------------------------------

    private void handleStartOfInput() {
        while (curToken.getType() == Scala3Lexer.NEWLINE) {
            hideAndAddPendingToken(curToken);
            setCurrentAndFollowingTokens();
        }
    }

    // -----------------------------------------------------------------------
    // NEWLINE handling
    // -----------------------------------------------------------------------

    private void handleNewlineToken() {
        // nlToken: default-channel copy of the raw NEWLINE token.
        CommonToken nlToken = new CommonToken(curToken);

        boolean hasLeadingWS = ffgToken.getType() == Scala3Lexer.WS;
        if (hasLeadingWS)
            setCurrentAndFollowingTokens(); // curToken=WS, ffgToken=first of next line

        // Blank / comment-only line: suppress everything and bail.
        if (ffgToken.getType() == Scala3Lexer.NEWLINE) {
            hideAndAddPendingToken(nlToken);
            if (hasLeadingWS) hideAndAddPendingToken(curToken);
            return;
        }

        Region top = regionStack.peek();

        // Compute next-line indentation from the WS text before enqueuing anything,
        // so we can decide NEWLINE visibility first and maintain source token order.
        int newIndent;
        if (ffgToken.getType() == Token.EOF)
            newIndent = 0;
        else if (hasLeadingWS)
            newIndent = getIndentLength(curToken.getText());
        else
            newIndent = 0;

        // Lines starting with '.' are method-chain continuations — suppress
        // NEWLINE as separator and INDENT, but still emit DEDENT(s).
        boolean isDot = ffgToken.getType() == Scala3Lexer.DOT;
        int curIndent = indentLengthStack.peek();

        // Whether we will emit an INDENT for this transition.
        // When true, NEWLINE is always suppressed (even in InBraces) because
        // emitting NEWLINE before INDENT would produce an unexpected separator.
        //
        // ARROW (=>) and CTXARROW (?=>) inside InParens are NOT excluded:
        // multi-statement lambda bodies (e.g. `loginData =>\n  val x = …\n  x`)
        // require INDENT/DEDENT even inside (…).  Trailing-comma cases such as
        // `err => body,` are handled by the COMMA drain in checkNextToken, which
        // emits DEDENT before the comma so the grammar sees the separator correctly.
        // `extension (params)` ends with RPAREN but has an indented method body.
        // Allow RPAREN to trigger INDENT only when the outer context is Indented
        // or TopLevel — not inside a function-call argument list (InParens) or a
        // brace block (InBraces), and not when the following line starts with a
        // class-template continuation keyword (extends / with / derives), which
        // is a header-continuation line, not a new indented block.
        boolean rparenOpensIndent = lastNonHiddenType == Scala3Lexer.RPAREN
                                 && regionStack.peek() != Region.InParens
                                 && regionStack.peek() != Region.InBraces
                                 && ffgToken.getType() != Scala3Lexer.EXTENDS
                                 && ffgToken.getType() != Scala3Lexer.WITH;
        boolean willIndent = newIndent > curIndent
            && !isDot
            && (canStartIndent(lastNonHiddenType) || rparenOpensIndent);

        // ---- Decide whether to surface the NEWLINE as a statement separator ----
        //
        // Rules (in priority order):
        //  1. Indent changes (DEDENT or INDENT upcoming) → always suppress.
        //  2. InParens → always suppress (implicit line joining).
        //  3. InBraces → surface at statement boundary (no extra guards needed
        //     because RBRACE/EOF are not in canStartStat).
        //  4. Indented/TopLevel, same level → surface at statement boundary
        //     with extra EOF/RBRACE guards.
        //  5. Indented/TopLevel, increasing indent, no INDENT → suppress
        //     (continuation line at unexpected depth).
        boolean surfaceNewline;
        if (newIndent < curIndent || willIndent)
            surfaceNewline = false;
        else if (top == Region.InParens)
            surfaceNewline = false;
        else if (top == Region.InBraces)
            surfaceNewline = !isDot
                && canEndStat(lastNonHiddenType)
                && canStartStat(ffgToken.getType())
                && !isStatContinuation(ffgToken.getType());
        else if (newIndent > curIndent) // Indented/TopLevel continuation line
            surfaceNewline = false;
        else // Indented or TopLevel, same level
            surfaceNewline = !isDot
                && ffgToken.getType() != Token.EOF
                && ffgToken.getType() != Scala3Lexer.RBRACE
                && canEndStat(lastNonHiddenType)
                && canStartStat(ffgToken.getType())
                && !isStatContinuation(ffgToken.getType());

        // ---- Emit NEWLINE then WS in source order ----
        if (surfaceNewline) addPendingToken(nlToken);
        else hideAndAddPendingToken(nlToken);
        if (hasLeadingWS) hideAndAddPendingToken(curToken);

        // ---- Handle INDENT / DEDENT ----
        //
        // InBraces and InParens participate fully: for-comprehensions, match
        // expressions, and other indented constructs inside { } and ( ) need
        // real INDENT/DEDENT tokens.  insertDedentTokens stops at InBraces/
        // InParens boundaries (it only pops Indented regions), so the brace/
        // paren region is never accidentally removed.
        if (newIndent < curIndent) {
            // Decreasing indentation: emit DEDENT(s), then re-surface a NEWLINE
            // copy on the default channel if the outer context needs a separator.
            insertDedentTokens(newIndent, ffgToken);
            Region newTop = regionStack.peek();
            if (newTop != Region.InParens && !isDot
                    && ffgToken.getType() != Token.EOF
                    && ffgToken.getType() != Scala3Lexer.RBRACE
                    && canEndStat(lastNonHiddenType)
                    && canStartStat(ffgToken.getType())
                    && !isStatContinuation(ffgToken.getType())) {
                // Re-emit a default-channel copy of the NEWLINE after the DEDENTs.
                addPendingToken(new CommonToken(nlToken));
            }
        } else if (willIndent) {
            // Increasing indentation: open a new indented block.
            indentLengthStack.push(newIndent);
            regionStack.push(Region.Indented);
            createAndAddPendingToken(Scala3Lexer.INDENT, Token.DEFAULT_CHANNEL, "<INDENT>", ffgToken);
        }
        // else: same level (or increase without canStartIndent) — no structural tokens.
    }

    // Emit DEDENT tokens for all Indented regions deeper than newIndent.
    private void insertDedentTokens(int newIndent, Token anchor) {
        while (!regionStack.isEmpty() && regionStack.peek() == Region.Indented) {
            if (indentLengthStack.peek() <= newIndent)
                break;
            indentLengthStack.pop();
            regionStack.pop();
            createAndAddPendingToken(Scala3Lexer.DEDENT, Token.DEFAULT_CHANNEL, "<DEDENT>", anchor);
        }
    }

    // -----------------------------------------------------------------------
    // EOF handling
    // -----------------------------------------------------------------------

    private void handleEofToken() {
        // Close every open indented block.
        while (!regionStack.isEmpty() && regionStack.peek() == Region.Indented) {
            indentLengthStack.pop();
            regionStack.pop();
            createAndAddPendingToken(Scala3Lexer.DEDENT, Token.DEFAULT_CHANNEL, "<DEDENT>", curToken);
        }
        addPendingToken(curToken);
    }

    // -----------------------------------------------------------------------
    // Token-set predicates (mirroring Dotty Tokens.scala)
    // -----------------------------------------------------------------------

    // canEndStatTokens = atomicExprTokens ∪ {TYPE, GIVEN, RPAREN, RBRACE, RBRACKET, DEDENT}
    // atomicExprTokens = literals ∪ identifiers ∪ {USCORE, THIS, SUPER, RETURN, QUOTEID}
    // endMarkerTag keywords (IF, WHILE, FOR, MATCH, TRY, VAL, NEW, EXTENSION) are also
    // included because 'end X' statements end with those keyword tokens.
    private static boolean canEndStat(int t) {
        switch (t) {
            case Scala3Lexer.Id:
            case Scala3Lexer.Varid:
            case Scala3Lexer.BacktickId:
            case Scala3Lexer.Op:
            case Scala3Lexer.IntegerLiteral:
            case Scala3Lexer.FloatingPointLiteral:
            case Scala3Lexer.BooleanLiteral:
            case Scala3Lexer.CharacterLiteral:
            case Scala3Lexer.StringLiteral:
            case Scala3Lexer.InterpolatedStringLiteral:
            case Scala3Lexer.SymbolLiteral:
            case Scala3Lexer.NullLiteral:
            case Scala3Lexer.QuoteId:
            case Scala3Lexer.USCORE:
            case Scala3Lexer.THIS:
            case Scala3Lexer.SUPER:
            case Scala3Lexer.RETURN:
            case Scala3Lexer.TYPE:
            case Scala3Lexer.GIVEN:
            case Scala3Lexer.RPAREN:
            case Scala3Lexer.RBRACE:
            case Scala3Lexer.RBRACKET:
            case Scala3Lexer.DEDENT:
            case Scala3Lexer.NEWLINE:
            // endMarkerTag keywords: 'end if', 'end while', etc.
            case Scala3Lexer.IF:
            case Scala3Lexer.WHILE:
            case Scala3Lexer.FOR:
            case Scala3Lexer.MATCH:
            case Scala3Lexer.TRY:
            case Scala3Lexer.VAL:
            case Scala3Lexer.NEW:
            case Scala3Lexer.EXTENSION:
                return true;
            default:
                return false;
        }
    }

    // isStatCtdTokens: next-token set that suppresses NL even when canEndStat is true.
    private static boolean isStatContinuation(int t) {
        switch (t) {
            case Scala3Lexer.THEN:
            case Scala3Lexer.ELSE:
            case Scala3Lexer.DO:
            case Scala3Lexer.CATCH:
            case Scala3Lexer.FINALLY:
            case Scala3Lexer.YIELD:
            case Scala3Lexer.MATCH:
                return true;
            default:
                return false;
        }
    }

    // canStartStatTokens: next-token set that can open a new statement.
    private static boolean canStartStat(int t) {
        switch (t) {
            case Scala3Lexer.Id:
            case Scala3Lexer.Varid:
            case Scala3Lexer.BacktickId:
            case Scala3Lexer.Op:
            case Scala3Lexer.IntegerLiteral:
            case Scala3Lexer.FloatingPointLiteral:
            case Scala3Lexer.BooleanLiteral:
            case Scala3Lexer.CharacterLiteral:
            case Scala3Lexer.StringLiteral:
            case Scala3Lexer.InterpolatedStringLiteral:
            case Scala3Lexer.SymbolLiteral:
            case Scala3Lexer.NullLiteral:
            case Scala3Lexer.QuoteId:
            case Scala3Lexer.USCORE:
            case Scala3Lexer.THIS:
            case Scala3Lexer.SUPER:
            case Scala3Lexer.NEW:
            case Scala3Lexer.RETURN:
            case Scala3Lexer.THROW:
            case Scala3Lexer.IF:
            case Scala3Lexer.WHILE:
            case Scala3Lexer.FOR:
            case Scala3Lexer.TRY:
            case Scala3Lexer.LBRACE:
            case Scala3Lexer.LPAREN:
            case Scala3Lexer.LBRACKET:
            case Scala3Lexer.QUOTE:
            case Scala3Lexer.INDENT:
            case Scala3Lexer.AT:
            case Scala3Lexer.CASE:
            case Scala3Lexer.END:
            case Scala3Lexer.DEF:
            case Scala3Lexer.VAL:
            case Scala3Lexer.VAR:
            case Scala3Lexer.TYPE:
            case Scala3Lexer.GIVEN:
            case Scala3Lexer.ABSTRACT:
            case Scala3Lexer.FINAL:
            case Scala3Lexer.PRIVATE:
            case Scala3Lexer.PROTECTED:
            case Scala3Lexer.OVERRIDE:
            case Scala3Lexer.SEALED:
            case Scala3Lexer.CLASS:
            case Scala3Lexer.TRAIT:
            case Scala3Lexer.OBJECT:
            case Scala3Lexer.ENUM:
            case Scala3Lexer.IMPORT:
            case Scala3Lexer.EXPORT:
            case Scala3Lexer.PACKAGE:
            case Scala3Lexer.INLINE:
            case Scala3Lexer.LAZY:
            case Scala3Lexer.IMPLICIT:
            case Scala3Lexer.EXTENSION:
            // contextual modifier keywords
            case Scala3Lexer.OPEN:
            case Scala3Lexer.INFIX:
            case Scala3Lexer.TRANSPARENT:
            case Scala3Lexer.OPAQUE:
            // contextual keywords usable as plain identifiers
            case Scala3Lexer.AS:
            case Scala3Lexer.DERIVES:
            case Scala3Lexer.USING:
                return true;
            default:
                return false;
        }
    }

    // canStartIndentTokens: previous tokens that allow INDENT emission.
    private static boolean canStartIndent(int t) {
        switch (t) {
            case Scala3Lexer.THEN:
            case Scala3Lexer.ELSE:
            case Scala3Lexer.DO:
            case Scala3Lexer.CATCH:
            case Scala3Lexer.FINALLY:
            case Scala3Lexer.YIELD:
            case Scala3Lexer.MATCH:
            case Scala3Lexer.COLON:
            case Scala3Lexer.WITH:
            case Scala3Lexer.ASSIGN:
            case Scala3Lexer.ARROW:
            case Scala3Lexer.CTXARROW:
            case Scala3Lexer.LARROW:
            case Scala3Lexer.WHILE:
            case Scala3Lexer.TRY:
            case Scala3Lexer.FOR:
            case Scala3Lexer.IF:
            case Scala3Lexer.THROW:
            case Scala3Lexer.RETURN:
                return true;
            default:
                return false;
        }
    }

    // -----------------------------------------------------------------------
    // Indentation length
    // -----------------------------------------------------------------------

    private static int getIndentLength(String text) {
        int length = 0;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (c == ' ')       length++;
            else if (c == '\t') length = (length / 8 + 1) * 8;
            else if (c == '\f') length = 0;
        }
        return length;
    }

    // -----------------------------------------------------------------------
    // Token queue helpers
    // -----------------------------------------------------------------------

    private void addPendingToken(Token token) {
        previousPendingTokenType = token.getType();
        if (token.getChannel() == Token.DEFAULT_CHANNEL)
            lastNonHiddenType = token.getType();
        pendingTokens.addLast(token);
    }

    private void hideAndAddPendingToken(Token token) {
        CommonToken hidden = new CommonToken(token);
        hidden.setChannel(Token.HIDDEN_CHANNEL);
        addPendingToken(hidden);
    }

    private void createAndAddPendingToken(int type, int channel, String text, Token anchor) {
        CommonToken t = new CommonToken(anchor);
        t.setType(type);
        t.setChannel(channel);
        t.setText(text);
        t.setStartIndex(anchor.getStartIndex());
        t.setStopIndex(anchor.getStartIndex() - 1); // zero-length synthetic token
        addPendingToken(t);
    }
}
