// Generated from com/khubla/ebnf/ebnf.g4 by ANTLR 4.2.2
package com.khubla.ebnf;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ebnfLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ID=1, ASSIGN=2, LPAREN=3, RPAREN=4, LBRACE=5, RBRACE=6, LEND=7, REND=8, 
		BAR=9, DOT=10, STRINGLITERAL=11, COMMENT=12, WS=13;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"ID", "ASSIGN", "')'", "'('", "'}'", "'{'", "']'", "'['", "'|'", "'.'", 
		"STRINGLITERAL", "COMMENT", "WS"
	};
	public static final String[] ruleNames = {
		"ID", "ASSIGN", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "LEND", "REND", 
		"BAR", "DOT", "STRINGLITERAL", "LETTER", "DIGIT", "SYMBOL", "COMMENT", 
		"WS"
	};


	public ebnfLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "ebnf.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\17h\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\3\2\3\2\3"+
		"\2\3\2\7\2(\n\2\f\2\16\2+\13\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7"+
		"\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\7\fA\n\f\f\f\16\fD\13\f\3\f"+
		"\3\f\3\f\7\fI\n\f\f\f\16\fL\13\f\3\f\5\fO\n\f\3\r\3\r\3\16\3\16\3\17\3"+
		"\17\3\20\3\20\3\20\3\20\7\20[\n\20\f\20\16\20^\13\20\3\20\3\20\3\20\3"+
		"\20\3\20\3\21\3\21\3\21\3\21\5BJ\\\2\22\3\3\5\4\7\5\t\6\13\7\r\b\17\t"+
		"\21\n\23\13\25\f\27\r\31\2\33\2\35\2\37\16!\17\3\2\6\4\2<<??\4\2C\\c|"+
		"\4\2//aa\5\2\13\f\17\17\"\"k\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3"+
		"\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2"+
		"\2\25\3\2\2\2\2\27\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\3#\3\2\2\2\5,\3\2\2"+
		"\2\7.\3\2\2\2\t\60\3\2\2\2\13\62\3\2\2\2\r\64\3\2\2\2\17\66\3\2\2\2\21"+
		"8\3\2\2\2\23:\3\2\2\2\25<\3\2\2\2\27N\3\2\2\2\31P\3\2\2\2\33R\3\2\2\2"+
		"\35T\3\2\2\2\37V\3\2\2\2!d\3\2\2\2#)\5\31\r\2$(\5\31\r\2%(\5\33\16\2&"+
		"(\5\35\17\2\'$\3\2\2\2\'%\3\2\2\2\'&\3\2\2\2(+\3\2\2\2)\'\3\2\2\2)*\3"+
		"\2\2\2*\4\3\2\2\2+)\3\2\2\2,-\t\2\2\2-\6\3\2\2\2./\7+\2\2/\b\3\2\2\2\60"+
		"\61\7*\2\2\61\n\3\2\2\2\62\63\7\177\2\2\63\f\3\2\2\2\64\65\7}\2\2\65\16"+
		"\3\2\2\2\66\67\7_\2\2\67\20\3\2\2\289\7]\2\29\22\3\2\2\2:;\7~\2\2;\24"+
		"\3\2\2\2<=\7\60\2\2=\26\3\2\2\2>B\7$\2\2?A\13\2\2\2@?\3\2\2\2AD\3\2\2"+
		"\2BC\3\2\2\2B@\3\2\2\2CE\3\2\2\2DB\3\2\2\2EO\7$\2\2FJ\7)\2\2GI\13\2\2"+
		"\2HG\3\2\2\2IL\3\2\2\2JK\3\2\2\2JH\3\2\2\2KM\3\2\2\2LJ\3\2\2\2MO\7)\2"+
		"\2N>\3\2\2\2NF\3\2\2\2O\30\3\2\2\2PQ\t\3\2\2Q\32\3\2\2\2RS\4\62;\2S\34"+
		"\3\2\2\2TU\t\4\2\2U\36\3\2\2\2VW\7*\2\2WX\7,\2\2X\\\3\2\2\2Y[\13\2\2\2"+
		"ZY\3\2\2\2[^\3\2\2\2\\]\3\2\2\2\\Z\3\2\2\2]_\3\2\2\2^\\\3\2\2\2_`\7,\2"+
		"\2`a\7+\2\2ab\3\2\2\2bc\b\20\2\2c \3\2\2\2de\t\5\2\2ef\3\2\2\2fg\b\21"+
		"\3\2g\"\3\2\2\2\t\2\')BJN\\\4\2\3\2\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}