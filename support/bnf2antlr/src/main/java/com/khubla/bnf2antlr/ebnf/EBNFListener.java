package com.khubla.bnf2antlr.ebnf;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;

import org.antlr.v4.runtime.misc.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.khubla.bnf2antlr.bnf.BNFListener;
import com.khubla.ebnf.ebnfBaseListener;
import com.khubla.ebnf.ebnfParser;

/**
 * @author tom
 */
public class EBNFListener extends ebnfBaseListener {
   /**
    * logger
    */
   private static Logger logger = LoggerFactory.getLogger(BNFListener.class);
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
   public void exitAlternation(@NotNull ebnfParser.AlternationContext ctx) {
      if (ctx.getChildCount() > 1) {
         final int terms = 1 + ((ctx.getChildCount() - 1) / 2);
         String v = parseStack.pop();
         for (int i = 0; i < (terms - 1); i++) {
            v = parseStack.pop() + " | " + v;
         }
         parseStack.push(v);
      }
   }

   @Override
   public void exitId(@NotNull ebnfParser.IdContext ctx) {
      String id = ctx.getText();
      id = id.replaceAll("-", "");
      parseStack.push(id);
   }

   @Override
   public void exitOneormore(@NotNull ebnfParser.OneormoreContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")+");
   }

   @Override
   public void exitOptional(@NotNull ebnfParser.OptionalContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")?");
   }

   @Override
   public void exitRhs(@NotNull ebnfParser.RhsContext ctx) {
      String v = "";
      for (int i = 0; i < ctx.getChildCount(); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push(v);
   }

   @Override
   public void exitRule_(@NotNull ebnfParser.Rule_Context ctx) {
      final String ruleRHS = parseStack.pop().trim();
      final String ruleName = parseStack.pop().trim();
      final String antlrRule = ruleName + " : " + ruleRHS + ";";
      parseStack.push(antlrRule);
      logger.debug(antlrRule);
   }

   @Override
   public void exitRulelist(@NotNull ebnfParser.RulelistContext ctx) {
      for (final String rule : parseStack) {
         antlrPrintWriter.println(rule);
      }
      antlrPrintWriter.flush();
   }

   @Override
   public void exitStringliteral(@NotNull ebnfParser.StringliteralContext ctx) {
      parseStack.push("'" + ctx.getText().substring(1, ctx.getText().length() - 1) + "'");
   }

   @Override
   public void exitZeroormore(@NotNull ebnfParser.ZeroormoreContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")*");
   }
}
