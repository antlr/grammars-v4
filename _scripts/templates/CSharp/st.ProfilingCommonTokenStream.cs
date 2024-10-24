// Generated from trgen <version>

using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

public class ProfilingCommonTokenStream : CommonTokenStream
{
    public Dictionary\<int, int> HitCount = new Dictionary\<int, int>();
    public Dictionary\<int, Dictionary\<string,int>> CallStacks = new Dictionary\<int, Dictionary\<string, int>>();

    public ProfilingCommonTokenStream(ITokenSource tokenSource)
        : base(tokenSource)
    {
    }

    public override IToken Get(int i)
    {
        IToken token = base.Get(i);
        if (token == null)
        {
            return null;
        }

        return token;
    }

    public override int LA(int i)
    {
        var token = LT(i);
        if (token == null)
        {
            return -1;
        }

        return token.Type;
    }

    public override IToken LT(int k)
    {
        IToken token = base.LT(k);
        if (token == null)
        {
            return null;
        }

        HitCount.TryGetValue(token.TokenIndex, out int before);
        var after = before + 1;
        HitCount[token.TokenIndex] = after;

        // Get call stack and note parser rule context.
        System.Diagnostics.StackTrace t = new System.Diagnostics.StackTrace();
        // Note all different types of call stack traces.
        StringBuilder tb = new StringBuilder();
        bool record = false;
        bool breakon = false;
        foreach (var frame in t.GetFrames().Reverse())
        {
            string method = frame.GetMethod().Name;
            string line_number = frame.GetNativeOffset().ToString();
            if (method == "Parse2") record = true;
            if (method == "LT") breakon = true;
            if (record)
            {
                tb.Append(method + ":" + line_number + ";");
            }
            if (breakon) break;
        }
        var call_stack = tb.ToString();
        if (!CallStacks.ContainsKey(token.TokenIndex))
        {
            CallStacks[token.TokenIndex] = new Dictionary\<string, int>();
        }

        CallStacks[token.TokenIndex].TryGetValue(call_stack, out int b);
        CallStacks[token.TokenIndex][call_stack] = 1 + b;
        return token;
    }
}