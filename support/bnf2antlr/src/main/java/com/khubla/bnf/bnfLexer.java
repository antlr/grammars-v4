// Generated from com/khubla/bnf/bnf.g4 by ANTLR 4.2.2
package com.khubla.bnf;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class bnfLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ID=1, ASSIGN=2, LPAREN=3, RPAREN=4, LBRACE=5, RBRACE=6, LEND=7, REND=8, 
		BAR=9, TEXT=10, STRINGLITERAL=11, WS=12;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"ID", "'::='", "')'", "'('", "'}'", "'{'", "']'", "'['", "'|'", "TEXT", 
		"STRINGLITERAL", "WS"
	};
	public static final String[] ruleNames = {
		"ID", "ASSIGN", "LPAREN", "RPAREN", "LBRACE", "RBRACE", "LEND", "REND", 
		"BAR", "TEXT", "STRINGLITERAL", "LETTER", "DIGIT", "SYMBOL", "WS"
	};


	public bnfLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "bnf.g4"; }

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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\16V\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\3\2\3\2\7\2$\n\2\f\2"+
		"\16\2\'\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3"+
		"\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3\13\6\13@\n\13\r\13\16\13A\3\f\3\f\7"+
		"\fF\n\f\f\f\16\fI\13\f\3\f\3\f\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3"+
		"\20\3\20\4%G\2\21\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31"+
		"\2\33\2\35\2\37\16\3\2\5\4\2C\\c|\n\2#),\61<B`b\u00a3\u0101\u0154\u0194"+
		"\u2015\u2124\u2192\u2301\5\2\13\f\17\17\"\"W\2\3\3\2\2\2\2\5\3\2\2\2\2"+
		"\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2"+
		"\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\37\3\2\2\2\3!\3\2\2\2\5"+
		"*\3\2\2\2\7.\3\2\2\2\t\60\3\2\2\2\13\62\3\2\2\2\r\64\3\2\2\2\17\66\3\2"+
		"\2\2\218\3\2\2\2\23:\3\2\2\2\25?\3\2\2\2\27C\3\2\2\2\31L\3\2\2\2\33N\3"+
		"\2\2\2\35P\3\2\2\2\37R\3\2\2\2!%\7>\2\2\"$\13\2\2\2#\"\3\2\2\2$\'\3\2"+
		"\2\2%&\3\2\2\2%#\3\2\2\2&(\3\2\2\2\'%\3\2\2\2()\7@\2\2)\4\3\2\2\2*+\7"+
		"<\2\2+,\7<\2\2,-\7?\2\2-\6\3\2\2\2./\7+\2\2/\b\3\2\2\2\60\61\7*\2\2\61"+
		"\n\3\2\2\2\62\63\7\177\2\2\63\f\3\2\2\2\64\65\7}\2\2\65\16\3\2\2\2\66"+
		"\67\7_\2\2\67\20\3\2\2\289\7]\2\29\22\3\2\2\2:;\7~\2\2;\24\3\2\2\2<@\5"+
		"\31\r\2=@\5\33\16\2>@\5\35\17\2?<\3\2\2\2?=\3\2\2\2?>\3\2\2\2@A\3\2\2"+
		"\2A?\3\2\2\2AB\3\2\2\2B\26\3\2\2\2CG\7$\2\2DF\13\2\2\2ED\3\2\2\2FI\3\2"+
		"\2\2GH\3\2\2\2GE\3\2\2\2HJ\3\2\2\2IG\3\2\2\2JK\7$\2\2K\30\3\2\2\2LM\t"+
		"\2\2\2M\32\3\2\2\2NO\4\62;\2O\34\3\2\2\2PQ\t\3\2\2Q\36\3\2\2\2RS\t\4\2"+
		"\2ST\3\2\2\2TU\b\20\2\2U \3\2\2\2\7\2%?AG\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}