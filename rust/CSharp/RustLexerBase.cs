using Antlr4.Runtime;
using System.IO;

public abstract class RustLexerBase : Lexer {
    private readonly ICharStream _input;

    protected RustLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) {
            _input = input;
    }

    public bool SOF(){
        return _input.LA(-1) <=0;
    }

    public bool next(char expect){
        return _input.LA(1) == expect;
    }

    public bool floatDotPossible(){
        int next = _input.LA(1);
        // only block . _ identifier after float
        if(next == '.' || next =='_') return false;
        if(next == 'f') {
            // 1.f32
            if (_input.LA(2)=='3'&&_input.LA(3)=='2')return true;
            //1.f64
            if (_input.LA(2)=='6'&&_input.LA(3)=='4')return true;
            return false;
        }
        if(next>='a'&&next<='z') return false;
        if(next>='A'&&next<='Z') return false;
        return true;
    }
}
