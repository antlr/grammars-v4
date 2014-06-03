package com.khubla.bnf2antlr.ebnf;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import com.khubla.ebnf.ebnfListener;
import com.khubla.ebnf.ebnfParser.AlternationContext;
import com.khubla.ebnf.ebnfParser.RulenameContext;
import com.khubla.ebnf.ebnfParser.StringliteralContext;

/**
 * @author tom
 */
public class EBNFListener implements ebnfListener {
   /**
    * output
    */
   private final PrintWriter antlrPrintWriter;
   /**
    * parse stack
    */
   private final Stack<String> parseStack = new Stack<String>();

   /**
    * ctor
    */
   public EBNFListener(OutputStream antlrOutput) {
      antlrPrintWriter = new PrintWriter(antlrOutput);
   }

   @Override
   public void enterAlternation(AlternationContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterElement(com.khubla.ebnf.ebnfParser.ElementContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterEveryRule(ParserRuleContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterId(com.khubla.ebnf.ebnfParser.IdContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterOneormore(com.khubla.ebnf.ebnfParser.OneormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterOptional(com.khubla.ebnf.ebnfParser.OptionalContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRhs(com.khubla.ebnf.ebnfParser.RhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRule_(com.khubla.ebnf.ebnfParser.Rule_Context ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRulelist(com.khubla.ebnf.ebnfParser.RulelistContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRulename(RulenameContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterStringliteral(StringliteralContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterZeroormore(com.khubla.ebnf.ebnfParser.ZeroormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitAlternation(AlternationContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitElement(com.khubla.ebnf.ebnfParser.ElementContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitEveryRule(ParserRuleContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitId(com.khubla.ebnf.ebnfParser.IdContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitOneormore(com.khubla.ebnf.ebnfParser.OneormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitOptional(com.khubla.ebnf.ebnfParser.OptionalContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRhs(com.khubla.ebnf.ebnfParser.RhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRule_(com.khubla.ebnf.ebnfParser.Rule_Context ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRulelist(com.khubla.ebnf.ebnfParser.RulelistContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRulename(RulenameContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitStringliteral(StringliteralContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitZeroormore(com.khubla.ebnf.ebnfParser.ZeroormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void visitErrorNode(ErrorNode node) {
      // TODO Auto-generated method stub
   }

   @Override
   public void visitTerminal(TerminalNode node) {
      // TODO Auto-generated method stub
   }
}
