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
		T__8=1, T__7=2, T__6=3, T__5=4, T__4=5, T__3=6, T__2=7, T__1=8, T__0=9, 
		NumberValue=10, ProseValue=11, ID=12, INT=13, COMMENT=14, WS=15, STRING=16;
	public static final String[] tokenNames = {
		"<INVALID>", "']'", "'{'", "')'", "'*'", "'['", "':'", "'('", "'}'", "'|'", 
		"NumberValue", "ProseValue", "ID", "INT", "COMMENT", "WS", "STRING"
	};
	public static final int
		RULE_rulelist = 0, RULE_rule_ = 1, RULE_elements = 2, RULE_alternation = 3, 
		RULE_concatenation = 4, RULE_repetition = 5, RULE_repeat = 6, RULE_element = 7, 
		RULE_group = 8, RULE_oneOrMore = 9, RULE_option = 10;
	public static final String[] ruleNames = {
		"rulelist", "rule_", "elements", "alternation", "concatenation", "repetition", 
		"repeat", "element", "group", "oneOrMore", "option"
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
			setState(25);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==ID) {
				{
				{
				setState(22); rule_();
				}
				}
				setState(27);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(28); match(EOF);
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
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(30); match(ID);
			setState(31); match(6);
			setState(32); elements();
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
			setState(34); alternation();
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
			setState(36); concatenation();
			setState(41);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==9) {
				{
				{
				setState(37); match(9);
				setState(38); concatenation();
				}
				}
				setState(43);
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
			setState(44); repetition();
			setState(48);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(45); repetition();
					}
					} 
				}
				setState(50);
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
			setState(52);
			_la = _input.LA(1);
			if (_la==4 || _la==INT) {
				{
				setState(51); repeat();
				}
			}

			setState(54); element();
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
			setState(64);
			switch ( getInterpreter().adaptivePredict(_input,6,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(56); match(INT);
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(58);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(57); match(INT);
					}
				}

				setState(60); match(4);
				setState(62);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(61); match(INT);
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
		public OneOrMoreContext oneOrMore() {
			return getRuleContext(OneOrMoreContext.class,0);
		}
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
			setState(73);
			switch (_input.LA(1)) {
			case ID:
				enterOuterAlt(_localctx, 1);
				{
				setState(66); match(ID);
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 2);
				{
				setState(67); group();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 3);
				{
				setState(68); option();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 4);
				{
				setState(69); oneOrMore();
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 5);
				{
				setState(70); match(STRING);
				}
				break;
			case NumberValue:
				enterOuterAlt(_localctx, 6);
				{
				setState(71); match(NumberValue);
				}
				break;
			case ProseValue:
				enterOuterAlt(_localctx, 7);
				{
				setState(72); match(ProseValue);
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
			setState(75); match(7);
			setState(76); alternation();
			setState(77); match(3);
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

	public static class OneOrMoreContext extends ParserRuleContext {
		public AlternationContext alternation() {
			return getRuleContext(AlternationContext.class,0);
		}
		public OneOrMoreContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_oneOrMore; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).enterOneOrMore(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof AbnfListener ) ((AbnfListener)listener).exitOneOrMore(this);
		}
	}

	public final OneOrMoreContext oneOrMore() throws RecognitionException {
		OneOrMoreContext _localctx = new OneOrMoreContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_oneOrMore);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(79); match(2);
			setState(80); alternation();
			setState(81); match(8);
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
		enterRule(_localctx, 20, RULE_option);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(83); match(5);
			setState(84); alternation();
			setState(85); match(1);
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\22Z\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4"+
		"\f\t\f\3\2\7\2\32\n\2\f\2\16\2\35\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\4\3\4"+
		"\3\5\3\5\3\5\7\5*\n\5\f\5\16\5-\13\5\3\6\3\6\7\6\61\n\6\f\6\16\6\64\13"+
		"\6\3\7\5\7\67\n\7\3\7\3\7\3\b\3\b\5\b=\n\b\3\b\3\b\5\bA\n\b\5\bC\n\b\3"+
		"\t\3\t\3\t\3\t\3\t\3\t\3\t\5\tL\n\t\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13"+
		"\3\f\3\f\3\f\3\f\3\f\2\2\r\2\4\6\b\n\f\16\20\22\24\26\2\2[\2\33\3\2\2"+
		"\2\4 \3\2\2\2\6$\3\2\2\2\b&\3\2\2\2\n.\3\2\2\2\f\66\3\2\2\2\16B\3\2\2"+
		"\2\20K\3\2\2\2\22M\3\2\2\2\24Q\3\2\2\2\26U\3\2\2\2\30\32\5\4\3\2\31\30"+
		"\3\2\2\2\32\35\3\2\2\2\33\31\3\2\2\2\33\34\3\2\2\2\34\36\3\2\2\2\35\33"+
		"\3\2\2\2\36\37\7\2\2\3\37\3\3\2\2\2 !\7\16\2\2!\"\7\b\2\2\"#\5\6\4\2#"+
		"\5\3\2\2\2$%\5\b\5\2%\7\3\2\2\2&+\5\n\6\2\'(\7\13\2\2(*\5\n\6\2)\'\3\2"+
		"\2\2*-\3\2\2\2+)\3\2\2\2+,\3\2\2\2,\t\3\2\2\2-+\3\2\2\2.\62\5\f\7\2/\61"+
		"\5\f\7\2\60/\3\2\2\2\61\64\3\2\2\2\62\60\3\2\2\2\62\63\3\2\2\2\63\13\3"+
		"\2\2\2\64\62\3\2\2\2\65\67\5\16\b\2\66\65\3\2\2\2\66\67\3\2\2\2\678\3"+
		"\2\2\289\5\20\t\29\r\3\2\2\2:C\7\17\2\2;=\7\17\2\2<;\3\2\2\2<=\3\2\2\2"+
		"=>\3\2\2\2>@\7\6\2\2?A\7\17\2\2@?\3\2\2\2@A\3\2\2\2AC\3\2\2\2B:\3\2\2"+
		"\2B<\3\2\2\2C\17\3\2\2\2DL\7\16\2\2EL\5\22\n\2FL\5\26\f\2GL\5\24\13\2"+
		"HL\7\22\2\2IL\7\f\2\2JL\7\r\2\2KD\3\2\2\2KE\3\2\2\2KF\3\2\2\2KG\3\2\2"+
		"\2KH\3\2\2\2KI\3\2\2\2KJ\3\2\2\2L\21\3\2\2\2MN\7\t\2\2NO\5\b\5\2OP\7\5"+
		"\2\2P\23\3\2\2\2QR\7\4\2\2RS\5\b\5\2ST\7\n\2\2T\25\3\2\2\2UV\7\7\2\2V"+
		"W\5\b\5\2WX\7\3\2\2X\27\3\2\2\2\n\33+\62\66<@BK";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}