using System;
using System.IO;
using Antlr4.Runtime;
using Antlr4.Runtime.Atn;
using Antlr4.Runtime.Dfa;

foreach (bool explicitWriters in new[] { false, true })
{
    var input = new AntlrInputStream(":");
    var lexer = explicitWriters
        ? new Probe(input, TextWriter.Null, TextWriter.Null)
        : new Probe(input);
    if (lexer.Check1()) throw new Exception("Colon must fail Check1.");
    lexer.SetInputStream(new AntlrInputStream("a"));
    if (!lexer.Check1()) throw new Exception("Check1 used the old input stream.");
    lexer.SetInputStream(new AntlrInputStream(":"));
    if (lexer.Check1()) throw new Exception("Replacement colon must fail Check1.");
}
Console.WriteLine("Both constructors use the current lexer input stream.");

sealed class Probe : LexerBase
{
    public Probe(ICharStream input) : base(input) { Initialize(); }
    public Probe(ICharStream input, TextWriter output, TextWriter error)
        : base(input, output, error) { Initialize(); }
    private void Initialize()
    {
        Interpreter = new LexerATNSimulator(this, new ATN(ATNType.Lexer, 0),
            Array.Empty<DFA>(), new PredictionContextCache());
    }
    public override string[] RuleNames => Array.Empty<string>();
    public override string GrammarFileName => "Probe";
    public override IVocabulary Vocabulary => new Vocabulary(Array.Empty<string>(), Array.Empty<string>());
}
