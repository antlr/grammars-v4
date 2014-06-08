// Generated from com/khubla/ebnf/ebnf.g4 by ANTLR 4.2.2
package com.khubla.ebnf;

import java.util.List;

import org.antlr.v4.runtime.NoViableAltException;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;

@SuppressWarnings({ "all", "warnings", "unchecked", "unused", "cast" })
public class ebnfParser extends Parser {
   public static class AlternationContext extends ParserRuleContext {
      public AlternationContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public List<TerminalNode> BAR() {
         return getTokens(ebnfParser.BAR);
      }

      public TerminalNode BAR(int i) {
         return getToken(ebnfParser.BAR, i);
      }

      public List<ElementContext> element() {
         return getRuleContexts(ElementContext.class);
      }

      public ElementContext element(int i) {
         return getRuleContext(ElementContext.class, i);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterAlternation(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitAlternation(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_alternation;
      }
   }

   public static class ElementContext extends ParserRuleContext {
      public ElementContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterElement(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitElement(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_element;
      }

      public IdContext id() {
         return getRuleContext(IdContext.class, 0);
      }

      public OneormoreContext oneormore() {
         return getRuleContext(OneormoreContext.class, 0);
      }

      public OptionalContext optional() {
         return getRuleContext(OptionalContext.class, 0);
      }

      public StringliteralContext stringliteral() {
         return getRuleContext(StringliteralContext.class, 0);
      }

      public ZeroormoreContext zeroormore() {
         return getRuleContext(ZeroormoreContext.class, 0);
      }
   }

   public static class IdContext extends ParserRuleContext {
      public IdContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterId(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitId(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_id;
      }

      public TerminalNode ID() {
         return getToken(ebnfParser.ID, 0);
      }
   }

   public static class OneormoreContext extends ParserRuleContext {
      public OneormoreContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public List<AlternationContext> alternation() {
         return getRuleContexts(AlternationContext.class);
      }

      public AlternationContext alternation(int i) {
         return getRuleContext(AlternationContext.class, i);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterOneormore(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitOneormore(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_oneormore;
      }

      public TerminalNode LPAREN() {
         return getToken(ebnfParser.LPAREN, 0);
      }

      public TerminalNode RPAREN() {
         return getToken(ebnfParser.RPAREN, 0);
      }
   }

   public static class OptionalContext extends ParserRuleContext {
      public OptionalContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public List<AlternationContext> alternation() {
         return getRuleContexts(AlternationContext.class);
      }

      public AlternationContext alternation(int i) {
         return getRuleContext(AlternationContext.class, i);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterOptional(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitOptional(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_optional;
      }

      public TerminalNode LEND() {
         return getToken(ebnfParser.LEND, 0);
      }

      public TerminalNode REND() {
         return getToken(ebnfParser.REND, 0);
      }
   }

   public static class RhsContext extends ParserRuleContext {
      public RhsContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public List<AlternationContext> alternation() {
         return getRuleContexts(AlternationContext.class);
      }

      public AlternationContext alternation(int i) {
         return getRuleContext(AlternationContext.class, i);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterRhs(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitRhs(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_rhs;
      }
   }

   public static class Rule_Context extends ParserRuleContext {
      public Rule_Context(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public TerminalNode ASSIGN() {
         return getToken(ebnfParser.ASSIGN, 0);
      }

      public TerminalNode DOT() {
         return getToken(ebnfParser.DOT, 0);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterRule_(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitRule_(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_rule_;
      }

      public RhsContext rhs() {
         return getRuleContext(RhsContext.class, 0);
      }

      public RulenameContext rulename() {
         return getRuleContext(RulenameContext.class, 0);
      }
   }

   public static class RulelistContext extends ParserRuleContext {
      public RulelistContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterRulelist(this);
         }
      }

      public TerminalNode EOF() {
         return getToken(Recognizer.EOF, 0);
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitRulelist(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_rulelist;
      }

      public List<Rule_Context> rule_() {
         return getRuleContexts(Rule_Context.class);
      }

      public Rule_Context rule_(int i) {
         return getRuleContext(Rule_Context.class, i);
      }
   }

   public static class RulenameContext extends ParserRuleContext {
      public RulenameContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterRulename(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitRulename(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_rulename;
      }

      public IdContext id() {
         return getRuleContext(IdContext.class, 0);
      }
   }

   public static class StringliteralContext extends ParserRuleContext {
      public StringliteralContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterStringliteral(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitStringliteral(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_stringliteral;
      }

      public TerminalNode STRINGLITERAL() {
         return getToken(ebnfParser.STRINGLITERAL, 0);
      }
   }

   public static class ZeroormoreContext extends ParserRuleContext {
      public ZeroormoreContext(ParserRuleContext parent, int invokingState) {
         super(parent, invokingState);
      }

      public List<AlternationContext> alternation() {
         return getRuleContexts(AlternationContext.class);
      }

      public AlternationContext alternation(int i) {
         return getRuleContext(AlternationContext.class, i);
      }

      @Override
      public void enterRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).enterZeroormore(this);
         }
      }

      @Override
      public void exitRule(ParseTreeListener listener) {
         if (listener instanceof ebnfListener) {
            ((ebnfListener) listener).exitZeroormore(this);
         }
      }

      @Override
      public int getRuleIndex() {
         return RULE_zeroormore;
      }

      public TerminalNode LBRACE() {
         return getToken(ebnfParser.LBRACE, 0);
      }

      public TerminalNode RBRACE() {
         return getToken(ebnfParser.RBRACE, 0);
      }
   }

   protected static final DFA[] _decisionToDFA;
   protected static final PredictionContextCache _sharedContextCache = new PredictionContextCache();
   public static final int ID = 1, ASSIGN = 2, LPAREN = 3, RPAREN = 4, LBRACE = 5, RBRACE = 6, LEND = 7, REND = 8, BAR = 9, DOT = 10, STRINGLITERAL = 11, COMMENT = 12, WS = 13;
   public static final String[] tokenNames = { "<INVALID>", "ID", "ASSIGN", "')'", "'('", "'}'", "'{'", "']'", "'['", "'|'", "'.'", "STRINGLITERAL", "COMMENT", "WS" };
   public static final int RULE_rulelist = 0, RULE_rule_ = 1, RULE_rulename = 2, RULE_rhs = 3, RULE_alternation = 4, RULE_element = 5, RULE_optional = 6, RULE_zeroormore = 7, RULE_oneormore = 8,
         RULE_stringliteral = 9, RULE_id = 10;
   public static final String[] ruleNames = { "rulelist", "rule_", "rulename", "rhs", "alternation", "element", "optional", "zeroormore", "oneormore", "stringliteral", "id" };
   public static final String _serializedATN = "\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\17[\4\2\t\2\4\3\t" + "\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t\13\4"
         + "\f\t\f\3\2\7\2\32\n\2\f\2\16\2\35\13\2\3\2\3\2\3\3\3\3\3\3\3\3\5\3%\n" + "\3\3\4\3\4\3\5\6\5*\n\5\r\5\16\5+\3\6\3\6\3\6\5\6\61\n\6\7\6\63\n\6\f"
         + "\6\16\6\66\13\6\3\7\3\7\3\7\3\7\3\7\5\7=\n\7\3\b\3\b\6\bA\n\b\r\b\16\b" + "B\3\b\3\b\3\t\3\t\6\tI\n\t\r\t\16\tJ\3\t\3\t\3\n\3\n\6\nQ\n\n\r\n\16\n"
         + "R\3\n\3\n\3\13\3\13\3\f\3\f\3\f\2\2\r\2\4\6\b\n\f\16\20\22\24\26\2\2[" + "\2\33\3\2\2\2\4 \3\2\2\2\6&\3\2\2\2\b)\3\2\2\2\n-\3\2\2\2\f<\3\2\2\2\16"
         + ">\3\2\2\2\20F\3\2\2\2\22N\3\2\2\2\24V\3\2\2\2\26X\3\2\2\2\30\32\5\4\3" + "\2\31\30\3\2\2\2\32\35\3\2\2\2\33\31\3\2\2\2\33\34\3\2\2\2\34\36\3\2\2"
         + "\2\35\33\3\2\2\2\36\37\7\2\2\3\37\3\3\2\2\2 !\5\6\4\2!\"\7\4\2\2\"$\5" + "\b\5\2#%\7\f\2\2$#\3\2\2\2$%\3\2\2\2%\5\3\2\2\2&\'\5\26\f\2\'\7\3\2\2"
         + "\2(*\5\n\6\2)(\3\2\2\2*+\3\2\2\2+)\3\2\2\2+,\3\2\2\2,\t\3\2\2\2-\64\5" + "\f\7\2.\60\7\13\2\2/\61\5\f\7\2\60/\3\2\2\2\60\61\3\2\2\2\61\63\3\2\2"
         + "\2\62.\3\2\2\2\63\66\3\2\2\2\64\62\3\2\2\2\64\65\3\2\2\2\65\13\3\2\2\2" + "\66\64\3\2\2\2\67=\5\16\b\28=\5\20\t\29=\5\22\n\2:=\5\24\13\2;=\5\26\f"
         + "\2<\67\3\2\2\2<8\3\2\2\2<9\3\2\2\2<:\3\2\2\2<;\3\2\2\2=\r\3\2\2\2>@\7" + "\n\2\2?A\5\n\6\2@?\3\2\2\2AB\3\2\2\2B@\3\2\2\2BC\3\2\2\2CD\3\2\2\2DE\7"
         + "\t\2\2E\17\3\2\2\2FH\7\b\2\2GI\5\n\6\2HG\3\2\2\2IJ\3\2\2\2JH\3\2\2\2J" + "K\3\2\2\2KL\3\2\2\2LM\7\7\2\2M\21\3\2\2\2NP\7\6\2\2OQ\5\n\6\2PO\3\2\2"
         + "\2QR\3\2\2\2RP\3\2\2\2RS\3\2\2\2ST\3\2\2\2TU\7\5\2\2U\23\3\2\2\2VW\7\r" + "\2\2W\25\3\2\2\2XY\7\3\2\2Y\27\3\2\2\2\13\33$+\60\64<BJR";
   public static final ATN _ATN = new ATNDeserializer().deserialize(_serializedATN.toCharArray());
   static {
      _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
      for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
         _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
      }
   }

   public ebnfParser(TokenStream input) {
      super(input);
      _interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
   }

   public final AlternationContext alternation() throws RecognitionException {
      final AlternationContext _localctx = new AlternationContext(_ctx, getState());
      enterRule(_localctx, 8, RULE_alternation);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(43);
            element();
            setState(50);
            _errHandler.sync(this);
            _la = _input.LA(1);
            while (_la == BAR) {
               {
                  {
                     setState(44);
                     match(BAR);
                     setState(46);
                     switch (getInterpreter().adaptivePredict(_input, 3, _ctx)) {
                        case 1: {
                           setState(45);
                           element();
                        }
                           break;
                     }
                  }
               }
               setState(52);
               _errHandler.sync(this);
               _la = _input.LA(1);
            }
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final ElementContext element() throws RecognitionException {
      final ElementContext _localctx = new ElementContext(_ctx, getState());
      enterRule(_localctx, 10, RULE_element);
      try {
         setState(58);
         switch (_input.LA(1)) {
            case REND:
               enterOuterAlt(_localctx, 1);
               {
                  setState(53);
                  optional();
               }
               break;
            case RBRACE:
               enterOuterAlt(_localctx, 2);
               {
                  setState(54);
                  zeroormore();
               }
               break;
            case RPAREN:
               enterOuterAlt(_localctx, 3);
               {
                  setState(55);
                  oneormore();
               }
               break;
            case STRINGLITERAL:
               enterOuterAlt(_localctx, 4);
               {
                  setState(56);
                  stringliteral();
               }
               break;
            case ID:
               enterOuterAlt(_localctx, 5);
               {
                  setState(57);
                  id();
               }
               break;
            default:
               throw new NoViableAltException(this);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   @Override
   public ATN getATN() {
      return _ATN;
   }

   @Override
   public String getGrammarFileName() {
      return "ebnf.g4";
   }

   @Override
   public String[] getRuleNames() {
      return ruleNames;
   }

   @Override
   public String getSerializedATN() {
      return _serializedATN;
   }

   @Override
   public String[] getTokenNames() {
      return tokenNames;
   }

   public final IdContext id() throws RecognitionException {
      final IdContext _localctx = new IdContext(_ctx, getState());
      enterRule(_localctx, 20, RULE_id);
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(86);
            match(ID);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final OneormoreContext oneormore() throws RecognitionException {
      final OneormoreContext _localctx = new OneormoreContext(_ctx, getState());
      enterRule(_localctx, 16, RULE_oneormore);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(76);
            match(RPAREN);
            setState(78);
            _errHandler.sync(this);
            _la = _input.LA(1);
            do {
               {
                  {
                     setState(77);
                     alternation();
                  }
               }
               setState(80);
               _errHandler.sync(this);
               _la = _input.LA(1);
            } while (((((_la) & ~0x3f) == 0) && (((1L << _la) & ((1L << ID) | (1L << RPAREN) | (1L << RBRACE) | (1L << REND) | (1L << STRINGLITERAL))) != 0)));
            setState(82);
            match(LPAREN);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final OptionalContext optional() throws RecognitionException {
      final OptionalContext _localctx = new OptionalContext(_ctx, getState());
      enterRule(_localctx, 12, RULE_optional);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(60);
            match(REND);
            setState(62);
            _errHandler.sync(this);
            _la = _input.LA(1);
            do {
               {
                  {
                     setState(61);
                     alternation();
                  }
               }
               setState(64);
               _errHandler.sync(this);
               _la = _input.LA(1);
            } while (((((_la) & ~0x3f) == 0) && (((1L << _la) & ((1L << ID) | (1L << RPAREN) | (1L << RBRACE) | (1L << REND) | (1L << STRINGLITERAL))) != 0)));
            setState(66);
            match(LEND);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final RhsContext rhs() throws RecognitionException {
      final RhsContext _localctx = new RhsContext(_ctx, getState());
      enterRule(_localctx, 6, RULE_rhs);
      try {
         int _alt;
         enterOuterAlt(_localctx, 1);
         {
            setState(39);
            _errHandler.sync(this);
            _alt = 1;
            do {
               switch (_alt) {
                  case 1: {
                     {
                        setState(38);
                        alternation();
                     }
                  }
                     break;
                  default:
                     throw new NoViableAltException(this);
               }
               setState(41);
               _errHandler.sync(this);
               _alt = getInterpreter().adaptivePredict(_input, 2, _ctx);
            } while ((_alt != 2) && (_alt != ATN.INVALID_ALT_NUMBER));
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final Rule_Context rule_() throws RecognitionException {
      final Rule_Context _localctx = new Rule_Context(_ctx, getState());
      enterRule(_localctx, 2, RULE_rule_);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(30);
            rulename();
            setState(31);
            match(ASSIGN);
            setState(32);
            rhs();
            setState(34);
            _la = _input.LA(1);
            if (_la == DOT) {
               {
                  setState(33);
                  match(DOT);
               }
            }
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final RulelistContext rulelist() throws RecognitionException {
      final RulelistContext _localctx = new RulelistContext(_ctx, getState());
      enterRule(_localctx, 0, RULE_rulelist);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(25);
            _errHandler.sync(this);
            _la = _input.LA(1);
            while (_la == ID) {
               {
                  {
                     setState(22);
                     rule_();
                  }
               }
               setState(27);
               _errHandler.sync(this);
               _la = _input.LA(1);
            }
            setState(28);
            match(EOF);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final RulenameContext rulename() throws RecognitionException {
      final RulenameContext _localctx = new RulenameContext(_ctx, getState());
      enterRule(_localctx, 4, RULE_rulename);
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(36);
            id();
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final StringliteralContext stringliteral() throws RecognitionException {
      final StringliteralContext _localctx = new StringliteralContext(_ctx, getState());
      enterRule(_localctx, 18, RULE_stringliteral);
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(84);
            match(STRINGLITERAL);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }

   public final ZeroormoreContext zeroormore() throws RecognitionException {
      final ZeroormoreContext _localctx = new ZeroormoreContext(_ctx, getState());
      enterRule(_localctx, 14, RULE_zeroormore);
      int _la;
      try {
         enterOuterAlt(_localctx, 1);
         {
            setState(68);
            match(RBRACE);
            setState(70);
            _errHandler.sync(this);
            _la = _input.LA(1);
            do {
               {
                  {
                     setState(69);
                     alternation();
                  }
               }
               setState(72);
               _errHandler.sync(this);
               _la = _input.LA(1);
            } while (((((_la) & ~0x3f) == 0) && (((1L << _la) & ((1L << ID) | (1L << RPAREN) | (1L << RBRACE) | (1L << REND) | (1L << STRINGLITERAL))) != 0)));
            setState(74);
            match(LBRACE);
         }
      } catch (final RecognitionException re) {
         _localctx.exception = re;
         _errHandler.reportError(this, re);
         _errHandler.recover(this, re);
      } finally {
         exitRule();
      }
      return _localctx;
   }
}