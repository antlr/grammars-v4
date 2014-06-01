// Generated from org/antlr/abnf/Abnf.g4 by ANTLR 4.2.2
package org.antlr.abnf;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class AbnfParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__7=1, T__6=2, T__5=3, T__4=4, T__3=5, T__2=6, T__1=7, T__0=8, NumberValue=9, 
		ProseValue=10, ID=11, INT=12, COMMENT=13, WS=14, STRING=15;
	public static final String[] tokenNames = {
		"<INVALID>", "']'", "'=/'", "')'", "'*'", "'['", "'('", "'/'", "'='", 
		"NumberValue", "ProseValue", "ID", "INT", "COMMENT", "WS", "STRING"
	};
	public static final int
		RULE_rulelist = 0, RULE_rule_ = 1, RULE_elements = 2, RULE_alternation = 3, 
		RULE_concatenation = 4, RULE_repetition = 5, RULE_repeat = 6, RULE_element = 7, 
		RULE_group = 8, RULE_option = 9;
	public static final String[] ruleNames = {
		"rulelist", "rule_", "elements", "alternation", "concatenation", "repetition", 
		"repeat", "element", "group", "option"
	};

	@Override
	public String getGrammarFileName() { return "Abnf.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public AbnfParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class RulelistContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(AbnfParser.EOF, 0); }
		public List<Rule_Context> rule_() {
			return getRuleContexts(Rule_Context.class);
		}
		public Rule_Context rule_(int i) {
			return getRuleContext(Rule_Context.class,i);
		}
		public RulelistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rulelist; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterRulelist(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitRulelist(this);
		}
	}

	public final RulelistContext rulelist() throws RecognitionException {
		RulelistContext _localctx = new RulelistContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_rulelist);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(23);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ID) {
				{
				{
				setState(20); rule_();
				}
				}
				setState(25);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(26); match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class Rule_Context extends ParserRuleContext {
		public ElementsContext elements() {
			return getRuleContext(ElementsContext.class,0);
		}
		public TerminalNode ID() { return getToken(AbnfParser.ID, 0); }
		public Rule_Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rule_; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterRule_(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitRule_(this);
		}
	}

	public final Rule_Context rule_() throws RecognitionException {
		Rule_Context _localctx = new Rule_Context(_ctx, getState());
		enterRule(_localctx, 2, RULE_rule_);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(28); match(ID);
			setState(29);
			_la = _input.LA(1);
			if ( !(_la==2 || _la==8) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(30); elements();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElementsContext extends ParserRuleContext {
		public AlternationContext alternation() {
			return getRuleContext(AlternationContext.class,0);
		}
		public ElementsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elements; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterElements(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitElements(this);
		}
	}

	public final ElementsContext elements() throws RecognitionException {
		ElementsContext _localctx = new ElementsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_elements);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(32); alternation();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AlternationContext extends ParserRuleContext {
		public ConcatenationContext concatenation(int i) {
			return getRuleContext(ConcatenationContext.class,i);
		}
		public List<ConcatenationContext> concatenation() {
			return getRuleContexts(ConcatenationContext.class);
		}
		public AlternationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alternation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterAlternation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitAlternation(this);
		}
	}

	public final AlternationContext alternation() throws RecognitionException {
		AlternationContext _localctx = new AlternationContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_alternation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(34); concatenation();
			setState(39);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==7) {
				{
				{
				setState(35); match(7);
				setState(36); concatenation();
				}
				}
				setState(41);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConcatenationContext extends ParserRuleContext {
		public List<RepetitionContext> repetition() {
			return getRuleContexts(RepetitionContext.class);
		}
		public RepetitionContext repetition(int i) {
			return getRuleContext(RepetitionContext.class,i);
		}
		public ConcatenationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_concatenation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterConcatenation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitConcatenation(this);
		}
	}

	public final ConcatenationContext concatenation() throws RecognitionException {
		ConcatenationContext _localctx = new ConcatenationContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_concatenation);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(42); repetition();
			setState(46);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(43); repetition();
					}
					} 
				}
				setState(48);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RepetitionContext extends ParserRuleContext {
		public ElementContext element() {
			return getRuleContext(ElementContext.class,0);
		}
		public RepeatContext repeat() {
			return getRuleContext(RepeatContext.class,0);
		}
		public RepetitionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_repetition; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterRepetition(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitRepetition(this);
		}
	}

	public final RepetitionContext repetition() throws RecognitionException {
		RepetitionContext _localctx = new RepetitionContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_repetition);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(50);
			_la = _input.LA(1);
			if (_la==4 || _la==INT) {
				{
				setState(49); repeat();
				}
			}

			setState(52); element();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class RepeatContext extends ParserRuleContext {
		public List<TerminalNode> INT() { return getTokens(AbnfParser.INT); }
		public TerminalNode INT(int i) {
			return getToken(AbnfParser.INT, i);
		}
		public RepeatContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_repeat; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterRepeat(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitRepeat(this);
		}
	}

	public final RepeatContext repeat() throws RecognitionException {
		RepeatContext _localctx = new RepeatContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_repeat);
		int _la;
		try {
			setState(62);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(54); match(INT);
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(56);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(55); match(INT);
					}
				}

				setState(58); match(4);
				setState(60);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(59); match(INT);
					}
				}

				}
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElementContext extends ParserRuleContext {
		public OptionContext option() {
			return getRuleContext(OptionContext.class,0);
		}
		public TerminalNode ID() { return getToken(AbnfParser.ID, 0); }
		public TerminalNode ProseValue() { return getToken(AbnfParser.ProseValue, 0); }
		public TerminalNode STRING() { return getToken(AbnfParser.STRING, 0); }
		public GroupContext group() {
			return getRuleContext(GroupContext.class,0);
		}
		public TerminalNode NumberValue() { return getToken(AbnfParser.NumberValue, 0); }
		public ElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_element; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitElement(this);
		}
	}

	public final ElementContext element() throws RecognitionException {
		ElementContext _localctx = new ElementContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_element);
		try {
			setState(70);
			switch (_input.LA(1)) {
			case ID:
				enterOuterAlt(_localctx, 1);
				{
				setState(64); match(ID);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 2);
				{
				setState(65); group();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 3);
				{
				setState(66); option();
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 4);
				{
				setState(67); match(STRING);
				}
				break;
			case NumberValue:
				enterOuterAlt(_localctx, 5);
				{
				setState(68); match(NumberValue);
				}
				break;
			case ProseValue:
				enterOuterAlt(_localctx, 6);
				{
				setState(69); match(ProseValue);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class GroupContext extends ParserRuleContext {
		public AlternationContext alternation() {
			return getRuleContext(AlternationContext.class,0);
		}
		public GroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_group; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitGroup(this);
		}
	}

	public final GroupContext group() throws RecognitionException {
		GroupContext _localctx = new GroupContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_group);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(72); match(6);
			setState(73); alternation();
			setState(74); match(3);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class OptionContext extends ParserRuleContext {
		public AlternationContext alternation() {
			return getRuleContext(AlternationContext.class,0);
		}
		public OptionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_option; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterOption(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitOption(this);
		}
	}

	public final OptionContext option() throws RecognitionException {
		OptionContext _localctx = new OptionContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_option);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(76); match(5);
			setState(77); alternation();
			setState(78); match(1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\21S\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\3"+
		"\2\7\2\30\n\2\f\2\16\2\33\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\4\3\4\3\5\3\5"+
		"\3\5\7\5(\n\5\f\5\16\5+\13\5\3\6\3\6\7\6/\n\6\f\6\16\6\62\13\6\3\7\5\7"+
		"\65\n\7\3\7\3\7\3\b\3\b\5\b;\n\b\3\b\3\b\5\b?\n\b\5\bA\n\b\3\t\3\t\3\t"+
		"\3\t\3\t\3\t\5\tI\n\t\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\2\2\f\2"+
		"\4\6\b\n\f\16\20\22\24\2\3\4\2\4\4\n\nT\2\31\3\2\2\2\4\36\3\2\2\2\6\""+
		"\3\2\2\2\b$\3\2\2\2\n,\3\2\2\2\f\64\3\2\2\2\16@\3\2\2\2\20H\3\2\2\2\22"+
		"J\3\2\2\2\24N\3\2\2\2\26\30\5\4\3\2\27\26\3\2\2\2\30\33\3\2\2\2\31\27"+
		"\3\2\2\2\31\32\3\2\2\2\32\34\3\2\2\2\33\31\3\2\2\2\34\35\7\2\2\3\35\3"+
		"\3\2\2\2\36\37\7\r\2\2\37 \t\2\2\2 !\5\6\4\2!\5\3\2\2\2\"#\5\b\5\2#\7"+
		"\3\2\2\2$)\5\n\6\2%&\7\t\2\2&(\5\n\6\2\'%\3\2\2\2(+\3\2\2\2)\'\3\2\2\2"+
		")*\3\2\2\2*\t\3\2\2\2+)\3\2\2\2,\60\5\f\7\2-/\5\f\7\2.-\3\2\2\2/\62\3"+
		"\2\2\2\60.\3\2\2\2\60\61\3\2\2\2\61\13\3\2\2\2\62\60\3\2\2\2\63\65\5\16"+
		"\b\2\64\63\3\2\2\2\64\65\3\2\2\2\65\66\3\2\2\2\66\67\5\20\t\2\67\r\3\2"+
		"\2\28A\7\16\2\29;\7\16\2\2:9\3\2\2\2:;\3\2\2\2;<\3\2\2\2<>\7\6\2\2=?\7"+
		"\16\2\2>=\3\2\2\2>?\3\2\2\2?A\3\2\2\2@8\3\2\2\2@:\3\2\2\2A\17\3\2\2\2"+
		"BI\7\r\2\2CI\5\22\n\2DI\5\24\13\2EI\7\21\2\2FI\7\13\2\2GI\7\f\2\2HB\3"+
		"\2\2\2HC\3\2\2\2HD\3\2\2\2HE\3\2\2\2HF\3\2\2\2HG\3\2\2\2I\21\3\2\2\2J"+
		"K\7\b\2\2KL\5\b\5\2LM\7\5\2\2M\23\3\2\2\2NO\7\7\2\2OP\5\b\5\2PQ\7\3\2"+
		"\2Q\25\3\2\2\2\n\31)\60\64:>@H";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}