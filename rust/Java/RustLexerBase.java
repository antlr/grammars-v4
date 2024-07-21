import org.antlr.v4.runtime.*;

public abstract class RustLexerBase extends Lexer{
    public RustLexerBase(CharStream input){
        super(input);
    }

    Token lt1;
    Token lt2;

    @Override
    public Token nextToken() {
        Token next = super.nextToken();

        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            // Keep track of the last token on the default channel.
            this.lt2 = this.lt1;
            this.lt1 = next;
        }

        return next;
    }

    public boolean SOF(){
        return _input.LA(-1) <=0;
    }
    
    public boolean next(char expect){
        return _input.LA(1) == expect;
    }

    public boolean floatDotPossible(){
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

    public boolean floatLiteralPossible(){
        if(this.lt1 == null || this.lt2 == null) return true;
        if(this.lt1.getType() != RustLexer.DOT) return true;
        switch (this.lt2.getType()){
            case RustLexer.CHAR_LITERAL:
            case RustLexer.STRING_LITERAL:
            case RustLexer.RAW_STRING_LITERAL:
            case RustLexer.BYTE_LITERAL:
            case RustLexer.BYTE_STRING_LITERAL:
            case RustLexer.RAW_BYTE_STRING_LITERAL:
            case RustLexer.INTEGER_LITERAL:
            case RustLexer.DEC_LITERAL:
            case RustLexer.HEX_LITERAL:
            case RustLexer.OCT_LITERAL:
            case RustLexer.BIN_LITERAL:

            case RustLexer.KW_SUPER:
            case RustLexer.KW_SELFVALUE:
            case RustLexer.KW_SELFTYPE:
            case RustLexer.KW_CRATE:
            case RustLexer.KW_DOLLARCRATE:

            case RustLexer.GT:
            case RustLexer.RCURLYBRACE:
            case RustLexer.RSQUAREBRACKET:
            case RustLexer.RPAREN:

            case RustLexer.KW_AWAIT:

            case RustLexer.NON_KEYWORD_IDENTIFIER:
            case RustLexer.RAW_IDENTIFIER:
            case RustLexer.KW_MACRORULES:
                return false;
            default:
                return true;
        }
    }
}