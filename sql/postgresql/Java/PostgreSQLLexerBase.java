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

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Lexer;

import java.util.Stack;

public abstract class PostgreSQLLexerBase extends Lexer {
    protected final Stack<String> tags = new Stack<>();

    protected PostgreSQLLexerBase(CharStream input) {
        super(input);

    }

    public void PushTag() {
        tags.push(getText());
    }

    public boolean IsTag() {
        return getText().equals(tags.peek());
    }

    public void PopTag() {
        tags.pop();
    }

    public void UnterminatedBlockCommentDebugAssert() {
        //Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
    }

    public boolean CheckLaMinus() {
        return getInputStream().LA(1) != '-';
    }

    public boolean CheckLaStar() {
        return getInputStream().LA(1) != '*';
    }

    public boolean CharIsLetter() {
        return Character.isLetter(getInputStream().LA(-1));
    }

    public void HandleNumericFail() {
        getInputStream().seek(getInputStream().index() - 2);
        setType(PostgreSQLLexer.Integral);
    }

    public void HandleLessLessGreaterGreater() {
        if (getText() == "<<") setType(PostgreSQLLexer.LESS_LESS);
        if (getText() == ">>") setType(PostgreSQLLexer.GREATER_GREATER);
    }

    public boolean CheckIfUtf32Letter() {
        int codePoint = getInputStream().LA(-2) << 8 + getInputStream().LA(-1);
        char[] c;
        if (codePoint < 0x10000) {
            c = new char[]{(char) codePoint};
        } else {
            codePoint -= 0x10000;
            c = new char[]{(char) (codePoint / 0x400 + 0xd800), (char) (codePoint % 0x400 + 0xdc00)};
        }
        return Character.isLetter(c[0]);
    }

    public boolean IsSemiColon()
    {
        return  ';' == (char)getInputStream().LA(1);
    }
}