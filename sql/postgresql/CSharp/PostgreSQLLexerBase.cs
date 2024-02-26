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

using System;
using Antlr4.Runtime;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;

public class PostgreSQLLexerBase : Lexer
{
    protected static Queue<string> tags = new Queue<string>();

    public PostgreSQLLexerBase(ICharStream input) : base(input)
    {
    }
    public PostgreSQLLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
    : base(input, output, errorOutput)
    {
    }

    private IIntStream getInputStream() { return InputStream; }
    public override string[] RuleNames => throw new NotImplementedException();

    public override IVocabulary Vocabulary => throw new NotImplementedException();

    public override string GrammarFileName => throw new NotImplementedException();

    public void pushTag() { tags.Enqueue(this.Text); }

    public bool isTag() { return this.Text.Equals(tags.Peek()); }

    public void popTag()
    {
        tags.Dequeue();
    }

    public void UnterminatedBlockCommentDebugAssert()
    {
        Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
    }

    public bool checkLA(int c)
    {
        return getInputStream().LA(1) != c;
    }

    public bool charIsLetter()
    {
        return Char.IsLetter((char)InputStream.LA(-1));
    }

    public void HandleNumericFail()
    {
        InputStream.Seek(getInputStream().Index - 2);
        Type = PostgreSQLLexer.Integral;
    }

    public void HandleLessLessGreaterGreater()
    {
        if (Text == "<<") Type = PostgreSQLLexer.LESS_LESS;
        if (Text == ">>") Type = PostgreSQLLexer.GREATER_GREATER;
    }

    public bool CheckIfUtf32Letter()
    {
        return Char.IsLetter(Char.ConvertFromUtf32(Char.ConvertToUtf32((char)InputStream.LA(-2), (char)InputStream.LA(-1))).Substring(0)[0]);
    }

}
