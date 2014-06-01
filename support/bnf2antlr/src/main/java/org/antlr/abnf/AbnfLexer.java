// Generated from org/antlr/abnf/Abnf.g4 by ANTLR 4.2.2
package org.antlr.abnf;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AbnfLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__8=1, T__7=2, T__6=3, T__5=4, T__4=5, T__3=6, T__2=7, T__1=8, T__0=9, 
		NumberValue=10, ProseValue=11, ID=12, INT=13, COMMENT=14, WS=15, STRING=16;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"']'", "'{'", "')'", "'*'", "'['", "':'", "'('", "'}'", "'|'", "NumberValue", 
		"ProseValue", "ID", "INT", "COMMENT", "WS", "STRING"
	};
	public static final String[] ruleNames = {
		"T__8", "T__7", "T__6", "T__5", "T__4", "T__3", "T__2", "T__1", "T__0", 
		"NumberValue", "BinaryValue", "DecimalValue", "HexValue", "ProseValue", 
		"ID", "INT", "COMMENT", "WS", "STRING", "BIT", "DIGIT", "HEX_DIGIT"
	};


	public AbnfLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Abnf.g4"; }

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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\22\u00c5\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\3\2\3\2\3\3\3"+
		"\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\3"+
		"\13\3\13\5\13F\n\13\3\f\3\f\6\fJ\n\f\r\f\16\fK\3\f\3\f\6\fP\n\f\r\f\16"+
		"\fQ\6\fT\n\f\r\f\16\fU\3\f\3\f\6\fZ\n\f\r\f\16\f[\5\f^\n\f\3\r\3\r\6\r"+
		"b\n\r\r\r\16\rc\3\r\3\r\6\rh\n\r\r\r\16\ri\6\rl\n\r\r\r\16\rm\3\r\3\r"+
		"\6\rr\n\r\r\r\16\rs\5\rv\n\r\3\16\3\16\6\16z\n\16\r\16\16\16{\3\16\3\16"+
		"\6\16\u0080\n\16\r\16\16\16\u0081\6\16\u0084\n\16\r\16\16\16\u0085\3\16"+
		"\3\16\6\16\u008a\n\16\r\16\16\16\u008b\5\16\u008e\n\16\3\17\3\17\7\17"+
		"\u0092\n\17\f\17\16\17\u0095\13\17\3\17\3\17\3\20\3\20\7\20\u009b\n\20"+
		"\f\20\16\20\u009e\13\20\3\21\6\21\u00a1\n\21\r\21\16\21\u00a2\3\22\3\22"+
		"\7\22\u00a7\n\22\f\22\16\22\u00aa\13\22\3\22\5\22\u00ad\n\22\3\22\3\22"+
		"\3\22\3\22\3\23\3\23\3\23\3\23\3\24\3\24\7\24\u00b9\n\24\f\24\16\24\u00bc"+
		"\13\24\3\24\3\24\3\25\3\25\3\26\3\26\3\27\3\27\2\2\30\3\3\5\4\7\5\t\6"+
		"\13\7\r\b\17\t\21\n\23\13\25\f\27\2\31\2\33\2\35\r\37\16!\17#\20%\21\'"+
		"\22)\2+\2-\2\3\2\t\3\2@@\4\2C\\c|\7\2//\62;C\\aac|\4\2\f\f\17\17\5\2\13"+
		"\f\17\17\"\"\3\2$$\5\2\62;CHch\u00d8\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2"+
		"\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23"+
		"\3\2\2\2\2\25\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2"+
		"\2%\3\2\2\2\2\'\3\2\2\2\3/\3\2\2\2\5\61\3\2\2\2\7\63\3\2\2\2\t\65\3\2"+
		"\2\2\13\67\3\2\2\2\r9\3\2\2\2\17;\3\2\2\2\21=\3\2\2\2\23?\3\2\2\2\25A"+
		"\3\2\2\2\27G\3\2\2\2\31_\3\2\2\2\33w\3\2\2\2\35\u008f\3\2\2\2\37\u0098"+
		"\3\2\2\2!\u00a0\3\2\2\2#\u00a4\3\2\2\2%\u00b2\3\2\2\2\'\u00b6\3\2\2\2"+
		")\u00bf\3\2\2\2+\u00c1\3\2\2\2-\u00c3\3\2\2\2/\60\7_\2\2\60\4\3\2\2\2"+
		"\61\62\7}\2\2\62\6\3\2\2\2\63\64\7+\2\2\64\b\3\2\2\2\65\66\7,\2\2\66\n"+
		"\3\2\2\2\678\7]\2\28\f\3\2\2\29:\7<\2\2:\16\3\2\2\2;<\7*\2\2<\20\3\2\2"+
		"\2=>\7\177\2\2>\22\3\2\2\2?@\7~\2\2@\24\3\2\2\2AE\7\'\2\2BF\5\27\f\2C"+
		"F\5\31\r\2DF\5\33\16\2EB\3\2\2\2EC\3\2\2\2ED\3\2\2\2F\26\3\2\2\2GI\7d"+
		"\2\2HJ\5)\25\2IH\3\2\2\2JK\3\2\2\2KI\3\2\2\2KL\3\2\2\2L]\3\2\2\2MO\7\60"+
		"\2\2NP\5)\25\2ON\3\2\2\2PQ\3\2\2\2QO\3\2\2\2QR\3\2\2\2RT\3\2\2\2SM\3\2"+
		"\2\2TU\3\2\2\2US\3\2\2\2UV\3\2\2\2V^\3\2\2\2WY\7/\2\2XZ\5)\25\2YX\3\2"+
		"\2\2Z[\3\2\2\2[Y\3\2\2\2[\\\3\2\2\2\\^\3\2\2\2]S\3\2\2\2]W\3\2\2\2]^\3"+
		"\2\2\2^\30\3\2\2\2_a\7f\2\2`b\5+\26\2a`\3\2\2\2bc\3\2\2\2ca\3\2\2\2cd"+
		"\3\2\2\2du\3\2\2\2eg\7\60\2\2fh\5+\26\2gf\3\2\2\2hi\3\2\2\2ig\3\2\2\2"+
		"ij\3\2\2\2jl\3\2\2\2ke\3\2\2\2lm\3\2\2\2mk\3\2\2\2mn\3\2\2\2nv\3\2\2\2"+
		"oq\7/\2\2pr\5+\26\2qp\3\2\2\2rs\3\2\2\2sq\3\2\2\2st\3\2\2\2tv\3\2\2\2"+
		"uk\3\2\2\2uo\3\2\2\2uv\3\2\2\2v\32\3\2\2\2wy\7z\2\2xz\5-\27\2yx\3\2\2"+
		"\2z{\3\2\2\2{y\3\2\2\2{|\3\2\2\2|\u008d\3\2\2\2}\177\7\60\2\2~\u0080\5"+
		"-\27\2\177~\3\2\2\2\u0080\u0081\3\2\2\2\u0081\177\3\2\2\2\u0081\u0082"+
		"\3\2\2\2\u0082\u0084\3\2\2\2\u0083}\3\2\2\2\u0084\u0085\3\2\2\2\u0085"+
		"\u0083\3\2\2\2\u0085\u0086\3\2\2\2\u0086\u008e\3\2\2\2\u0087\u0089\7/"+
		"\2\2\u0088\u008a\5-\27\2\u0089\u0088\3\2\2\2\u008a\u008b\3\2\2\2\u008b"+
		"\u0089\3\2\2\2\u008b\u008c\3\2\2\2\u008c\u008e\3\2\2\2\u008d\u0083\3\2"+
		"\2\2\u008d\u0087\3\2\2\2\u008d\u008e\3\2\2\2\u008e\34\3\2\2\2\u008f\u0093"+
		"\7>\2\2\u0090\u0092\n\2\2\2\u0091\u0090\3\2\2\2\u0092\u0095\3\2\2\2\u0093"+
		"\u0091\3\2\2\2\u0093\u0094\3\2\2\2\u0094\u0096\3\2\2\2\u0095\u0093\3\2"+
		"\2\2\u0096\u0097\7@\2\2\u0097\36\3\2\2\2\u0098\u009c\t\3\2\2\u0099\u009b"+
		"\t\4\2\2\u009a\u0099\3\2\2\2\u009b\u009e\3\2\2\2\u009c\u009a\3\2\2\2\u009c"+
		"\u009d\3\2\2\2\u009d \3\2\2\2\u009e\u009c\3\2\2\2\u009f\u00a1\4\62;\2"+
		"\u00a0\u009f\3\2\2\2\u00a1\u00a2\3\2\2\2\u00a2\u00a0\3\2\2\2\u00a2\u00a3"+
		"\3\2\2\2\u00a3\"\3\2\2\2\u00a4\u00a8\7=\2\2\u00a5\u00a7\n\5\2\2\u00a6"+
		"\u00a5\3\2\2\2\u00a7\u00aa\3\2\2\2\u00a8\u00a6\3\2\2\2\u00a8\u00a9\3\2"+
		"\2\2\u00a9\u00ac\3\2\2\2\u00aa\u00a8\3\2\2\2\u00ab\u00ad\7\17\2\2\u00ac"+
		"\u00ab\3\2\2\2\u00ac\u00ad\3\2\2\2\u00ad\u00ae\3\2\2\2\u00ae\u00af\7\f"+
		"\2\2\u00af\u00b0\3\2\2\2\u00b0\u00b1\b\22\2\2\u00b1$\3\2\2\2\u00b2\u00b3"+
		"\t\6\2\2\u00b3\u00b4\3\2\2\2\u00b4\u00b5\b\23\2\2\u00b5&\3\2\2\2\u00b6"+
		"\u00ba\7$\2\2\u00b7\u00b9\n\7\2\2\u00b8\u00b7\3\2\2\2\u00b9\u00bc\3\2"+
		"\2\2\u00ba\u00b8\3\2\2\2\u00ba\u00bb\3\2\2\2\u00bb\u00bd\3\2\2\2\u00bc"+
		"\u00ba\3\2\2\2\u00bd\u00be\7$\2\2\u00be(\3\2\2\2\u00bf\u00c0\4\62\63\2"+
		"\u00c0*\3\2\2\2\u00c1\u00c2\4\62;\2\u00c2,\3\2\2\2\u00c3\u00c4\t\b\2\2"+
		"\u00c4.\3\2\2\2\31\2EKQU[]cimsu{\u0081\u0085\u008b\u008d\u0093\u009c\u00a2"+
		"\u00a8\u00ac\u00ba\3\2\3\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}