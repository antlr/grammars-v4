// Generated from trgen <version>

using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System;
using System.Text;

public class TwoByteStream : ICharStream
{
    private ICharStream stream;

    public TwoByteStream(ICharStream stream)
    {
        this.stream = stream;
    }

    public int Index
    {
        get
        {
            return stream.Index;
        }
    }

    public int Size
    {
        get
        {
            return stream.Size;
        }
    }

    public string SourceName
    {
        get
        {
            return stream.SourceName;
        }
    }

    public void Consume()
    {
        stream.Consume();
    }

    [return: NotNull]
    public string GetText(Interval interval)
    {
        StringBuilder buf = new StringBuilder();
        int start = interval.a;
        int stop = interval.b;
        int index = stream.Index;
        stream.Seek(0);
        for (int i = start; i \< (stop + 1); i++)
        {
            int t = stream.LA(i + 1);
            char c = (char)t;
            buf.Append(c);
        }
        stream.Seek(index);
        return buf.ToString();
    }

    public int LA(int i)
    {
        int v = stream.LA(i);
        if (v \<= 0)
        {
            return v;
        }
        return v;
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
}

public class TwoByteCharStream : BaseInputCharStream
{
    private int[] data;

    public TwoByteCharStream(string file_name)
    {
        byte[] input = System.IO.File.ReadAllBytes(file_name);
        var input_length = input.Length;
        this.data = new int[input_length / 2];
        int dataIdx = 0;
        for (int i = 0; i \< input_length;)
        {
            int cl = input[i];
            int ch = input[i+1];
            int v = ch \<\< 8 | cl;
            data[dataIdx++] = v;
            if (dataIdx > data.Length)
            {
                Array.Resize(ref data, data.Length * 2);
            }
            i += 2;
        }
        this.n = dataIdx;
    }

    protected override int ValueAt(int i)
    {
        return data[i];
    }

    protected override string ConvertDataToString(int start, int count)
    {
        var sb = new StringBuilder(count);
        for (int i = start; i \< start + count; i++)
        {
            sb.Append(Char.ConvertFromUtf32(data[i]));
        }
        return sb.ToString();
    }
}

