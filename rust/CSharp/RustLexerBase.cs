using Antlr4.Runtime;
using System.IO;

public abstract class RustLexerBase : Lexer {
    private readonly ICharStream _input;

    protected RustLexerBase(ICharStream input, TextWriter output, TextWriter errorOutput)
        : base(input, output, errorOutput) {
            _input = input;
    }

    IToken lt1;
    IToken lt2;

    public override IToken NextToken()
    {
        // Get the next token.
        IToken next = base.NextToken();

        if (next.Channel == DefaultTokenChannel)
        {
            // Keep track of the last token on the default channel.
            lt2 = lt1;
            lt1 = next;
        }

        return next;
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

    public bool floatLiteralPossible(){
        if(lt1 == null || lt2 == null) return true;
        if(lt1.Type != DOT) return true;
        switch (lt2.Type){
            case CHAR_LITERAL:
            case STRING_LITERAL:
            case RAW_STRING_LITERAL:
            case BYTE_LITERAL:
            case BYTE_STRING_LITERAL:
            case RAW_BYTE_STRING_LITERAL:
            case INTEGER_LITERAL:
            case DEC_LITERAL:
            case HEX_LITERAL:
            case OCT_LITERAL:
            case BIN_LITERAL:

            case KW_SUPER:
            case KW_SELFVALUE:
            case KW_SELFTYPE:
            case KW_CRATE:
            case KW_DOLLARCRATE:

            case GT:
            case RCURLYBRACE:
            case RSQUAREBRACKET:
            case RPAREN:

            case KW_AWAIT:

            case NON_KEYWORD_IDENTIFIER:
            case RAW_IDENTIFIER:
            case KW_MACRORULES:
                return false;
            default:
                return true;
        }
    }
}
