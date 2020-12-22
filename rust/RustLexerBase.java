import org.antlr.v4.runtime.*;

public abstract class RustLexerBase extends Lexer{
    public RustLexerBase(CharStream input){
        super(input);
    }

    public boolean SOF(){
        return _input.LA(-1) <=0;
    }
    
    public boolean next(char expect){
        return _input.LA(1) == expect;
    }
}