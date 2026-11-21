import org.antlr.v4.runtime.*;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedList;

// based on Python3LexerBase.java / GDScriptLexerBase.java with modification
abstract class ReaperLexerBase extends Lexer {
    // A queue where extra tokens are pushed on (see the NEWLINE lexer rule).
    private LinkedList<Token> pendingTokens = new LinkedList<>();
    // The stack that keeps track of the indentation level.
    private Deque<Integer> indents = new ArrayDeque<>();

    protected ReaperLexerBase(CharStream input) {
        super(input);
    }

    @Override
    public void emit(Token t) {
        super.setToken(t);
        pendingTokens.offer(t);
    }

    @Override
    public Token nextToken() {
        // Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if (_input.LA(1) == EOF && !indents.isEmpty()) {
            // Remove any trailing EOF tokens from our buffer.
            for (int i = pendingTokens.size() - 1; i >= 0; i--) {
                if (pendingTokens.get(i).getType() == Token.EOF) {
                    pendingTokens.remove(i);
                }
            }
            // Now emit as many DEDENT tokens as needed.
            while (!indents.isEmpty()) {
                emit(createIndentToken(reaperLexer.DEDENT, ""));
                indents.pop();
            }
            // Put the EOF back on the token stream.
            emit(createIndentToken(Token.EOF, "<EOF>"));
        }

        Token next = super.nextToken();

        return pendingTokens.isEmpty() ? next : pendingTokens.poll();
    }

    private CommonToken createIndentToken(int type, String text) {
        int stop = getCharIndex() - 1;
        int start = text.isEmpty() ? stop : stop - text.length() + 1;
        return new CommonToken(_tokenFactorySourcePair, type, DEFAULT_TOKEN_CHANNEL, start, stop);
    }

    // Calculates the indentation of the provided spaces, taking the
    // following rule into account:
    //
    // "Tabs are replaced (from left to right) by one to eight spaces
    //  such that the total number of characters up to and including
    //  the replacement is a multiple of eight [...]"
    //
    //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
    private static int indentWidth(String spaces) {
        int count = 0;
        for (int i = 0; i < spaces.length(); i++) {
            char ch = spaces.charAt(i);
            if (ch == '\t') {
                count += 8 - (count % 8);
            } else {
                count++;
            }
        }
        return count;
    }

    // Called from the NEWLINE lexer rule's action. Reaper's grammar has no
    // use for the newline character itself as a parser token (statements
    // are juxtaposed, not newline-terminated); only the resulting
    // INDENT/DEDENT/SEP tokens matter.
    //
    // A bodyless class reference (the esolang wiki's "class-prototype",
    // e.g. the final bare "Cat" in the wiki's example program) is simply a
    // class_declaration with no following INDENT, which already
    // unambiguously distinguishes it from class_definition (which requires
    // INDENT destructor DEDENT) without needing a separate REDENT token.
    void onNewline() {
        String spaces = getText().replaceAll("[\r\n]+", "");
        int next = _input.LA(1);
        if (next == '\r' || next == '\n') {
            // A blank line: ignore it entirely.
            skip();
            return;
        }
        if (next == EOF) {
            // Trailing newline right before end of file: flush any
            // outstanding DEDENTs so every open block is closed.
            while (!indents.isEmpty()) {
                emit(createIndentToken(reaperLexer.DEDENT, ""));
                indents.pop();
            }
            skip();
            return;
        }
        int indent = indentWidth(spaces);
        int previous = indents.isEmpty() ? 0 : indents.peek();
        if (indent == previous) {
            // Same indentation level: a plain statement separator.
            emit(createIndentToken(reaperLexer.SEP, ""));
        } else if (indent > previous) {
            indents.push(indent);
            emit(createIndentToken(reaperLexer.INDENT, spaces));
        } else {
            // Possibly emit more than one DEDENT token.
            while (!indents.isEmpty() && indents.peek() > indent) {
                emit(createIndentToken(reaperLexer.DEDENT, ""));
                indents.pop();
            }
            skip();
        }
    }

    @Override
    public void reset() {
        pendingTokens = new LinkedList<>();
        indents = new ArrayDeque<>();
        super.reset();
    }
}
