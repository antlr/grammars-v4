package com.khubla.bnf2antlr;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;

import org.antlr.abnf.AbnfListener;
import org.antlr.abnf.AbnfParser.AlternationContext;
import org.antlr.abnf.AbnfParser.ConcatenationContext;
import org.antlr.abnf.AbnfParser.ElementContext;
import org.antlr.abnf.AbnfParser.ElementsContext;
import org.antlr.abnf.AbnfParser.GroupContext;
import org.antlr.abnf.AbnfParser.OptionContext;
import org.antlr.abnf.AbnfParser.RepeatContext;
import org.antlr.abnf.AbnfParser.RepetitionContext;
import org.antlr.abnf.AbnfParser.Rule_Context;
import org.antlr.abnf.AbnfParser.RulelistContext;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * @author tom
 */
public class BNFListener implements AbnfListener {
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
   public void enterAlternation(AlternationContext ctx) {
      //
   }

   @Override
   public void enterConcatenation(ConcatenationContext ctx) {
      //
   }

   @Override
   public void enterElement(ElementContext ctx) {
      //
   }

   @Override
   public void enterElements(ElementsContext ctx) {
      //
   }

   @Override
   public void enterEveryRule(ParserRuleContext ctx) {
      //
   }

   @Override
   public void enterGroup(GroupContext ctx) {
      //
   }

   @Override
   public void enterOption(OptionContext ctx) {
      //
   }

   @Override
   public void enterRepeat(RepeatContext ctx) {
      //
   }

   @Override
   public void enterRepetition(RepetitionContext ctx) {
      //
   }

   @Override
   public void enterRule_(Rule_Context ctx) {
      antlrPrintWriter.println(ctx.getText());
   }

   @Override
   public void enterRulelist(RulelistContext ctx) {
   }

   @Override
   public void exitAlternation(AlternationContext ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void exitConcatenation(ConcatenationContext ctx) {
      //
   }

   @Override
   public void exitElement(ElementContext ctx) {
      parseStack.push(ctx.getText());
   }

   @Override
   public void exitElements(ElementsContext ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void exitEveryRule(ParserRuleContext ctx) {
   }

   @Override
   public void exitGroup(GroupContext ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void exitOption(OptionContext ctx) {
      final String optionalElement = parseStack.pop();
      parseStack.push(optionalElement + "?");
   }

   @Override
   public void exitRepeat(RepeatContext ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void exitRepetition(RepetitionContext ctx) {
      // nada
   }

   @Override
   public void exitRule_(Rule_Context ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void exitRulelist(RulelistContext ctx) {
      System.out.println(ctx.getText());
   }

   @Override
   public void visitErrorNode(ErrorNode node) {
   }

   @Override
   public void visitTerminal(TerminalNode node) {
   }
}
