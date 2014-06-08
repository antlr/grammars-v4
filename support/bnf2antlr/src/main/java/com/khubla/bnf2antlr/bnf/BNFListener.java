package com.khubla.bnf2antlr.bnf;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

import com.khubla.bnf.bnfListener;
import com.khubla.bnf.bnfParser.AlternativeContext;
import com.khubla.bnf.bnfParser.AlternativesContext;
import com.khubla.bnf.bnfParser.ElementContext;
import com.khubla.bnf.bnfParser.IdContext;
import com.khubla.bnf.bnfParser.LhsContext;
import com.khubla.bnf.bnfParser.OneormoreContext;
import com.khubla.bnf.bnfParser.OptionalContext;
import com.khubla.bnf.bnfParser.RhsContext;
import com.khubla.bnf.bnfParser.Rule_Context;
import com.khubla.bnf.bnfParser.RulelistContext;
import com.khubla.bnf.bnfParser.TextContext;
import com.khubla.bnf.bnfParser.ZeroormoreContext;

/**
 * @author tom
 */
public class BNFListener implements bnfListener {
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
   public BNFListener(OutputStream antlrOutput) {
      antlrPrintWriter = new PrintWriter(antlrOutput);
   }

   @Override
   public void enterAlternative(AlternativeContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterAlternatives(AlternativesContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterElement(ElementContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterEveryRule(ParserRuleContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterId(IdContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterLhs(LhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterOneormore(OneormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterOptional(OptionalContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRhs(RhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRule_(Rule_Context ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterRulelist(RulelistContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterText(TextContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void enterZeroormore(ZeroormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitAlternative(AlternativeContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitAlternatives(AlternativesContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitElement(ElementContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitEveryRule(ParserRuleContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitId(IdContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitLhs(LhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitOneormore(OneormoreContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitOptional(OptionalContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRhs(RhsContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRule_(Rule_Context ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitRulelist(RulelistContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitText(TextContext ctx) {
      // TODO Auto-generated method stub
   }

   @Override
   public void exitZeroormore(ZeroormoreContext ctx) {
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
