// Generated from com/khubla/bnf/bnf.g4 by ANTLR 4.2.2
package com.khubla.bnf;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class bnfParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		ASSIGN=1, LPAREN=2, RPAREN=3, LBRACE=4, RBRACE=5, LEND=6, REND=7, BAR=8, 
		GT=9, LT=10, ID=11, WS=12;
	public static final String[] tokenNames = {
		"<INVALID>", "'::='", "')'", "'('", "'}'", "'{'", "']'", "'['", "'|'", 
		"'>'", "'<'", "ID", "WS"
	};
	public static final int
		RULE_rulelist = 0, RULE_rule_ = 1, RULE_lhs = 2, RULE_rhs = 3, RULE_alternatives = 4, 
		RULE_alternative = 5, RULE_element = 6, RULE_optional = 7, RULE_zeroormore = 8, 
		RULE_oneormore = 9, RULE_text = 10, RULE_id = 11, RULE_ruleid = 12;
	public static final String[] ruleNames = {
		"rulelist", "rule_", "lhs", "rhs", "alternatives", "alternative", "element", 
		"optional", "zeroormore", "oneormore", "text", "id", "ruleid"
	};

	@Override
	public String getGrammarFileName() { return "bnf.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }

	public bnfParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class RulelistContext extends ParserRuleContext {
		public Rule_Context rule_(int i) {
			return getRuleContext(Rule_Context.class,i);
		}
		public List<Rule_Context> rule_() {
			return getRuleContexts(Rule_Context.class);
		}
		public TerminalNode EOF() { return getToken(bnfParser.EOF, 0); }
		public RulelistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rulelist; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterRulelist(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitRulelist(this);
		}
	}

	public final RulelistContext rulelist() throws RecognitionException {
		RulelistContext _localctx = new RulelistContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_rulelist);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(29);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LT) {
				{
				{
				setState(26); rule_();
				}
				}
				setState(31);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(32); match(EOF);
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
		public TerminalNode ASSIGN() { return getToken(bnfParser.ASSIGN, 0); }
		public RhsContext rhs() {
			return getRuleContext(RhsContext.class,0);
		}
		public LhsContext lhs() {
			return getRuleContext(LhsContext.class,0);
		}
		public Rule_Context(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rule_; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterRule_(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitRule_(this);
		}
	}

	public final Rule_Context rule_() throws RecognitionException {
		Rule_Context _localctx = new Rule_Context(_ctx, getState());
		enterRule(_localctx, 2, RULE_rule_);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(34); lhs();
			setState(35); match(ASSIGN);
			setState(36); rhs();
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

	public static class LhsContext extends ParserRuleContext {
		public IdContext id() {
			return getRuleContext(IdContext.class,0);
		}
		public LhsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lhs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterLhs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitLhs(this);
		}
	}

	public final LhsContext lhs() throws RecognitionException {
		LhsContext _localctx = new LhsContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_lhs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(38); id();
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

	public static class RhsContext extends ParserRuleContext {
		public AlternativesContext alternatives() {
			return getRuleContext(AlternativesContext.class,0);
		}
		public RhsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_rhs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterRhs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitRhs(this);
		}
	}

	public final RhsContext rhs() throws RecognitionException {
		RhsContext _localctx = new RhsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_rhs);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(40); alternatives();
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

	public static class AlternativesContext extends ParserRuleContext {
		public AlternativeContext alternative(int i) {
			return getRuleContext(AlternativeContext.class,i);
		}
		public List<TerminalNode> BAR() { return getTokens(bnfParser.BAR); }
		public List<AlternativeContext> alternative() {
			return getRuleContexts(AlternativeContext.class);
		}
		public TerminalNode BAR(int i) {
			return getToken(bnfParser.BAR, i);
		}
		public AlternativesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alternatives; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterAlternatives(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitAlternatives(this);
		}
	}

	public final AlternativesContext alternatives() throws RecognitionException {
		AlternativesContext _localctx = new AlternativesContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_alternatives);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(42); alternative();
			setState(47);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==BAR) {
				{
				{
				setState(43); match(BAR);
				setState(44); alternative();
				}
				}
				setState(49);
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

	public static class AlternativeContext extends ParserRuleContext {
		public List<ElementContext> element() {
			return getRuleContexts(ElementContext.class);
		}
		public ElementContext element(int i) {
			return getRuleContext(ElementContext.class,i);
		}
		public AlternativeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_alternative; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterAlternative(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitAlternative(this);
		}
	}

	public final AlternativeContext alternative() throws RecognitionException {
		AlternativeContext _localctx = new AlternativeContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_alternative);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(53);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,2,_ctx);
			while ( _alt!=2 && _alt!=ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(50); element();
					}
					} 
				}
				setState(55);
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

	public static class ElementContext extends ParserRuleContext {
		public ZeroormoreContext zeroormore() {
			return getRuleContext(ZeroormoreContext.class,0);
		}
		public TextContext text() {
			return getRuleContext(TextContext.class,0);
		}
		public IdContext id() {
			return getRuleContext(IdContext.class,0);
		}
		public OneormoreContext oneormore() {
			return getRuleContext(OneormoreContext.class,0);
		}
		public OptionalContext optional() {
			return getRuleContext(OptionalContext.class,0);
		}
		public ElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_element; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitElement(this);
		}
	}

	public final ElementContext element() throws RecognitionException {
		ElementContext _localctx = new ElementContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_element);
		try {
			setState(61);
			switch (_input.LA(1)) {
			case REND:
				enterOuterAlt(_localctx, 1);
				{
				setState(56); optional();
				}
				break;
			case RBRACE:
				enterOuterAlt(_localctx, 2);
				{
				setState(57); zeroormore();
				}
				break;
			case RPAREN:
				enterOuterAlt(_localctx, 3);
				{
				setState(58); oneormore();
				}
				break;
			case ID:
				enterOuterAlt(_localctx, 4);
				{
				setState(59); text();
				}
				break;
			case LT:
				enterOuterAlt(_localctx, 5);
				{
				setState(60); id();
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

	public static class OptionalContext extends ParserRuleContext {
		public TerminalNode REND() { return getToken(bnfParser.REND, 0); }
		public TerminalNode LEND() { return getToken(bnfParser.LEND, 0); }
		public AlternativesContext alternatives() {
			return getRuleContext(AlternativesContext.class,0);
		}
		public OptionalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_optional; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterOptional(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitOptional(this);
		}
	}

	public final OptionalContext optional() throws RecognitionException {
		OptionalContext _localctx = new OptionalContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_optional);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(63); match(REND);
			setState(64); alternatives();
			setState(65); match(LEND);
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

	public static class ZeroormoreContext extends ParserRuleContext {
		public TerminalNode RBRACE() { return getToken(bnfParser.RBRACE, 0); }
		public TerminalNode LBRACE() { return getToken(bnfParser.LBRACE, 0); }
		public AlternativesContext alternatives() {
			return getRuleContext(AlternativesContext.class,0);
		}
		public ZeroormoreContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_zeroormore; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterZeroormore(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitZeroormore(this);
		}
	}

	public final ZeroormoreContext zeroormore() throws RecognitionException {
		ZeroormoreContext _localctx = new ZeroormoreContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_zeroormore);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(67); match(RBRACE);
			setState(68); alternatives();
			setState(69); match(LBRACE);
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

	public static class OneormoreContext extends ParserRuleContext {
		public TerminalNode LPAREN() { return getToken(bnfParser.LPAREN, 0); }
		public TerminalNode RPAREN() { return getToken(bnfParser.RPAREN, 0); }
		public AlternativesContext alternatives() {
			return getRuleContext(AlternativesContext.class,0);
		}
		public OneormoreContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_oneormore; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterOneormore(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitOneormore(this);
		}
	}

	public final OneormoreContext oneormore() throws RecognitionException {
		OneormoreContext _localctx = new OneormoreContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_oneormore);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(71); match(RPAREN);
			setState(72); alternatives();
			setState(73); match(LPAREN);
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

	public static class TextContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(bnfParser.ID, 0); }
		public TextContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_text; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterText(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitText(this);
		}
	}

	public final TextContext text() throws RecognitionException {
		TextContext _localctx = new TextContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_text);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(75); match(ID);
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

	public static class IdContext extends ParserRuleContext {
		public TerminalNode LT() { return getToken(bnfParser.LT, 0); }
		public TerminalNode GT() { return getToken(bnfParser.GT, 0); }
		public RuleidContext ruleid() {
			return getRuleContext(RuleidContext.class,0);
		}
		public IdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_id; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitId(this);
		}
	}

	public final IdContext id() throws RecognitionException {
		IdContext _localctx = new IdContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_id);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(77); match(LT);
			setState(78); ruleid();
			setState(79); match(GT);
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

	public static class RuleidContext extends ParserRuleContext {
		public TerminalNode ID() { return getToken(bnfParser.ID, 0); }
		public RuleidContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_ruleid; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).enterRuleid(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof bnfListener ) ((bnfListener)listener).exitRuleid(this);
		}
	}

	public final RuleidContext ruleid() throws RecognitionException {
		RuleidContext _localctx = new RuleidContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_ruleid);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(81); match(ID);
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
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\16V\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4"+
		"\f\t\f\4\r\t\r\4\16\t\16\3\2\7\2\36\n\2\f\2\16\2!\13\2\3\2\3\2\3\3\3\3"+
		"\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\6\7\6\60\n\6\f\6\16\6\63\13\6\3\7\7"+
		"\7\66\n\7\f\7\16\79\13\7\3\b\3\b\3\b\3\b\3\b\5\b@\n\b\3\t\3\t\3\t\3\t"+
		"\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\f\3\f\3\r\3\r\3\r\3\r\3\16\3\16"+
		"\3\16\2\2\17\2\4\6\b\n\f\16\20\22\24\26\30\32\2\2O\2\37\3\2\2\2\4$\3\2"+
		"\2\2\6(\3\2\2\2\b*\3\2\2\2\n,\3\2\2\2\f\67\3\2\2\2\16?\3\2\2\2\20A\3\2"+
		"\2\2\22E\3\2\2\2\24I\3\2\2\2\26M\3\2\2\2\30O\3\2\2\2\32S\3\2\2\2\34\36"+
		"\5\4\3\2\35\34\3\2\2\2\36!\3\2\2\2\37\35\3\2\2\2\37 \3\2\2\2 \"\3\2\2"+
		"\2!\37\3\2\2\2\"#\7\2\2\3#\3\3\2\2\2$%\5\6\4\2%&\7\3\2\2&\'\5\b\5\2\'"+
		"\5\3\2\2\2()\5\30\r\2)\7\3\2\2\2*+\5\n\6\2+\t\3\2\2\2,\61\5\f\7\2-.\7"+
		"\n\2\2.\60\5\f\7\2/-\3\2\2\2\60\63\3\2\2\2\61/\3\2\2\2\61\62\3\2\2\2\62"+
		"\13\3\2\2\2\63\61\3\2\2\2\64\66\5\16\b\2\65\64\3\2\2\2\669\3\2\2\2\67"+
		"\65\3\2\2\2\678\3\2\2\28\r\3\2\2\29\67\3\2\2\2:@\5\20\t\2;@\5\22\n\2<"+
		"@\5\24\13\2=@\5\26\f\2>@\5\30\r\2?:\3\2\2\2?;\3\2\2\2?<\3\2\2\2?=\3\2"+
		"\2\2?>\3\2\2\2@\17\3\2\2\2AB\7\t\2\2BC\5\n\6\2CD\7\b\2\2D\21\3\2\2\2E"+
		"F\7\7\2\2FG\5\n\6\2GH\7\6\2\2H\23\3\2\2\2IJ\7\5\2\2JK\5\n\6\2KL\7\4\2"+
		"\2L\25\3\2\2\2MN\7\r\2\2N\27\3\2\2\2OP\7\f\2\2PQ\5\32\16\2QR\7\13\2\2"+
		"R\31\3\2\2\2ST\7\r\2\2T\33\3\2\2\2\6\37\61\67?";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}