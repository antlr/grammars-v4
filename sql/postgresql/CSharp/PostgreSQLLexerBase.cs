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

public abstract class PostgreSQLLexerBase : Lexer
{
    protected Stack<string> tags = new Stack<string>();

    public PostgreSQLLexerBase(ICharStream input)
        : base(input)
    {
    }

    public PostgreSQLLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput)
    {
    }

    public void PushTag()
    {
        tags.Push(this.Text);
    }

    public bool IsTag()
    {
        return this.Text.Equals(tags.Peek());
    }

    public void PopTag()
    {
        tags.Pop();
    }

    public void UnterminatedBlockCommentDebugAssert()
    {
        Debug.Assert(InputStream.LA(1) == -1 /*EOF*/);
    }

    public bool CheckLaMinus()
    {
        return this.InputStream.LA(1) != '-';
    }

    public bool CheckLaStar()
    {
        return this.InputStream.LA(1) != '*';
    }

    public bool CharIsLetter()
    {
        return Char.IsLetter((char)InputStream.LA(-1));
    }

    public void HandleNumericFail()
    {
        InputStream.Seek(this.InputStream.Index - 2);
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

    public bool IsSemiColon()
    {
        return  ';' == (char)InputStream.LA(1);
    }
}
