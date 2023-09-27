using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using System;
using System.IO;
using System.Reflection;

public abstract class Adaptor : Lexer
{
    readonly ICharStream stream;
    readonly FieldInfo tokenInput = typeof(CommonToken).GetField("_type", BindingFlags.NonPublic | BindingFlags.Instance);
    protected Adaptor(ICharStream input)
            : base(input, Console.Out, Console.Error)
    {
        stream = input;
    }

    protected Adaptor(ICharStream input, TextWriter output, TextWriter errorOutput)
            : base(input, output, errorOutput)
    {
        stream = input;
    }

    private int CurrentRuleType { get; set; } = TokenConstants.InvalidType;

    public bool AtEnd()
    {
        return CurrentMode == pegen_v3_10Lexer.ACTION_MODE;
    }

    protected void handleEndAction()
    {
        int oldMode = CurrentMode;
        int newMode = PopMode();
        bool isActionWithinAction = ModeStack.Count > 0
            && newMode == pegen_v3_10Lexer.ACTION_MODE
            && oldMode == newMode;

        if (isActionWithinAction)
        {
            CurrentRuleType = (pegen_v3_10Lexer.ACTION);
        }
    }
}
