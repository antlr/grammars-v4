import 'package:antlr4/antlr4.dart';
import 'dart:io';
import 'RustLexer.dart';

abstract class RustLexerBase extends Lexer {

    RustLexerBase(CharStream input)
        : super(input) {
    }

    Token? lt1;
    Token? lt2;

    @override Token nextToken()
    {
        // Get the next token.
        var next = super.nextToken();

        if (next.channel == Token.DEFAULT_CHANNEL)
        {
            // Keep track of the last token on the default channel.
            lt2 = lt1;
            lt1 = next;
        }

        return next;
    }

    bool SOF(){
	var input = this.inputStream.LA(-1) ?? 0;
        return input <= 0;
    }

    bool FloatDotPossible(){
        int next = inputStream.LA(1) ?? 0;
	// only block . _ identifier after float
	if (next == '.'.codeUnitAt(0) || next == '_'.codeUnitAt(0)) {
		return false;
	}
        if (next == 'f'.codeUnitAt(0)) {
            // 1.f32
	    var v2 = inputStream.LA(2) ?? 0;
	    var v3 = inputStream.LA(3) ?? 0;
	    if (v2 == '3'.codeUnitAt(0) && v3 == '2'.codeUnitAt(0))
	    {
		return true;
	    }
            //1.f64
	    if (v2 == '6'.codeUnitAt(0) && v3 == '4'.codeUnitAt(0))
	    {
		return true;
	    }
            return false;
        }
	if (next >= 'a'.codeUnitAt(0) && next <= 'z'.codeUnitAt(0)) {
	    return false;
	}
	if (next >= 'A'.codeUnitAt(0) && next <= 'Z'.codeUnitAt(0))
	{
	    return false;
	}
        return true;
    }

    bool FloatLiteralPossible()
    {
	if (lt1 == null || lt2 == null)
	{
	    return true;
	}
	if (lt1?.type != RustLexer.TOKEN_DOT) {
	    return true;
	}
        switch (lt2?.type) {
            case RustLexer.TOKEN_CHAR_LITERAL:
            case RustLexer.TOKEN_STRING_LITERAL:
            case RustLexer.TOKEN_RAW_STRING_LITERAL:
            case RustLexer.TOKEN_BYTE_LITERAL:
            case RustLexer.TOKEN_BYTE_STRING_LITERAL:
            case RustLexer.TOKEN_RAW_BYTE_STRING_LITERAL:
            case RustLexer.TOKEN_INTEGER_LITERAL:
            case RustLexer.TOKEN_DEC_LITERAL:
            case RustLexer.TOKEN_HEX_LITERAL:
            case RustLexer.TOKEN_OCT_LITERAL:
            case RustLexer.TOKEN_BIN_LITERAL:
            case RustLexer.TOKEN_KW_SUPER:
            case RustLexer.TOKEN_KW_SELFVALUE:
            case RustLexer.TOKEN_KW_SELFTYPE:
            case RustLexer.TOKEN_KW_CRATE:
            case RustLexer.TOKEN_KW_DOLLARCRATE:
            case RustLexer.TOKEN_GT:
            case RustLexer.TOKEN_RCURLYBRACE:
            case RustLexer.TOKEN_RSQUAREBRACKET:
            case RustLexer.TOKEN_RPAREN:
            case RustLexer.TOKEN_KW_AWAIT:
            case RustLexer.TOKEN_NON_KEYWORD_IDENTIFIER:
            case RustLexer.TOKEN_RAW_IDENTIFIER:
            case RustLexer.TOKEN_KW_MACRORULES:
                return false;
            default:
                return true;
        }
    }
}
