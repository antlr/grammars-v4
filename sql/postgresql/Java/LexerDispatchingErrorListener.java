/*
PostgreSQL grammar.
The MIT License (MIT).
Copyright (c) 2021-2023, Oleksii Kovalov (Oleksii.Kovalov@outlook.com).
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

import java.util.BitSet;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.*;
import org.antlr.v4.runtime.misc.*;

public class LexerDispatchingErrorListener implements ANTLRErrorListener
{
    Lexer _parent;

    public LexerDispatchingErrorListener(Lexer parent)
    {
        _parent = parent;
    }

    public void syntaxError(Recognizer<?, ?> recognizer, Object offendingSymbol, int line, int charPositionInLine, String msg, RecognitionException e)
    {
        ProxyErrorListener foo = new ProxyErrorListener(_parent.getErrorListeners());
        foo.syntaxError(recognizer, offendingSymbol, line, charPositionInLine, msg, e);
    }

    public void reportAmbiguity(Parser recognizer,
                                DFA dfa,
                                int startIndex,
                                int stopIndex,
                                boolean exact,
                                BitSet ambigAlts,
                                ATNConfigSet configs)
    {
        ProxyErrorListener foo = new ProxyErrorListener(_parent.getErrorListeners());
        foo.reportAmbiguity(recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs);
    }

    public void reportAttemptingFullContext(Parser recognizer,
                                            DFA dfa,
                                            int startIndex,
                                            int stopIndex,
                                            BitSet conflictingAlts,
                                            ATNConfigSet configs)
    {
        ProxyErrorListener foo = new ProxyErrorListener(_parent.getErrorListeners());
        foo.reportAttemptingFullContext(recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs);
    }

    public void reportContextSensitivity(Parser recognizer,
                                         DFA dfa,
                                         int startIndex,
                                         int stopIndex,
                                         int prediction,
                                         ATNConfigSet configs)
    {
        ProxyErrorListener foo = new ProxyErrorListener(_parent.getErrorListeners());
        foo.reportContextSensitivity(recognizer, dfa, startIndex, stopIndex, prediction, configs);
    }
}
