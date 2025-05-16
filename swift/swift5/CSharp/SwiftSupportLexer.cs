using System.Collections.Generic;
using System.IO;
using Antlr4.Runtime;

public class SwiftLexerStack : Stack<int>
{
    public void push(int v) => Push(v);
    public int pop() => Pop();
    public bool isEmpty() => Count == 0;
    public int peek() => Peek();
    public void clear() => Clear();
}
public abstract class SwiftSupportLexer : Lexer
{
    protected SwiftLexerStack parenthesis = new();
    
    protected SwiftSupportLexer(ICharStream input, TextWriter output, TextWriter errorOutput) 
        : base(input, output, errorOutput)
    {
    }

    protected void popMode() => PopMode();

    public override void Reset()
    {
        base.Reset();
        parenthesis.clear();
    }
    
}