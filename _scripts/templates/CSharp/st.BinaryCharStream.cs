// [The "BSD license"]
// Copyright (c) 2020 Tom Everett
// All rights reserved.

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 3. The name of the author may not be used to endorse or promote products
//    derived from this software without specific prior written permission.

// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System.Text;

public class BinaryCharStream : ICharStream
{
    private readonly ICharStream stream;

    public BinaryCharStream(ICharStream stream)
    {
        this.stream = stream;
    }

    public void Consume()
    {
        stream.Consume();
    }

    public int LA(int i)
    {
        return stream.LA(i);
    }

    public int Mark()
    {
        return stream.Mark();
    }

    public void Release(int marker)
    {
        stream.Release(marker);
    }

    public void Seek(int index)
    {
        stream.Seek(index);
    }

    public int Index => stream.Index;

    public int Size => stream.Size;

    public string SourceName => stream.SourceName;

    public string GetText(Interval interval)
    {
        var buf = new StringBuilder();
        int start = interval.a;
        int stop = interval.b;
        int index = stream.Index;
        stream.Seek(0);
        bool more = false;
        for (int i = start; i \<= stop; i++)
        {
            int t = stream.LA(i + 1);
            if (more) buf.Append(" ");
            more = true;
            buf.Append((char)t);
        }
        stream.Seek(index);
        return buf.ToString();
    }
}
