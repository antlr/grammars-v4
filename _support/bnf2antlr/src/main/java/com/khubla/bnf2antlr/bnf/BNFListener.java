package com.khubla.bnf2antlr.bnf;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.Stack;

import org.antlr.v4.runtime.misc.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.khubla.bnf.bnfBaseListener;
import com.khubla.bnf.bnfParser;
import com.khubla.bnf.bnfParser.IdContext;
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
public class BNFListener extends bnfBaseListener {
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
   public BNFListener(OutputStream antlrOutput) {
      antlrPrintWriter = new PrintWriter(antlrOutput);
   }

   @Override
   public void exitAlternative(@NotNull bnfParser.AlternativeContext ctx) {
      if (ctx.getChildCount() > 1) {
         String v = "";
         for (int i = 0; i < ctx.getChildCount(); i++) {
            v = parseStack.pop() + " " + v;
         }
         parseStack.push(v);
      }
   }

   @Override
   public void exitAlternatives(@NotNull bnfParser.AlternativesContext ctx) {
      if (ctx.getChildCount() > 1) {
         String v = "";
         for (int i = 0; i < (ctx.getChildCount() - 1); i++) {
            if (0 != i) {
               v = parseStack.pop() + " | " + v;
            } else {
               v = parseStack.pop();
            }
         }
         parseStack.push(v);
      }
   }

   @Override
   public void exitId(IdContext ctx) {
      String id = ctx.getText();
      id = sanitiseID(id);
      parseStack.push(id);
   }

   @Override
   public void exitOneormore(OneormoreContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")+");
   }

   @Override
   public void exitOptional(OptionalContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")?");
   }

   @Override
   public void exitRhs(RhsContext ctx) {
      String v = "";
      for (int i = 0; i < ctx.getChildCount(); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push(v);
   }

   @Override
   public void exitRule_(Rule_Context ctx) {
      final String ruleRHS = parseStack.pop().trim();
      final String ruleName = parseStack.pop().trim();
      final String antlrRule = ruleName + " : " + ruleRHS + ";";
      parseStack.push(antlrRule);
      logger.debug(antlrRule);
   }

   @Override
   public void exitRulelist(RulelistContext ctx) {
      for (final String rule : parseStack) {
         antlrPrintWriter.println(rule);
      }
      antlrPrintWriter.flush();
   }

   @Override
   public void exitText(TextContext ctx) {
      parseStack.push("'" + ctx.getText() + "'");
   }

   @Override
   public void exitZeroormore(ZeroormoreContext ctx) {
      String v = parseStack.pop();
      for (int i = 0; i < (ctx.getChildCount() - 3); i++) {
         v = parseStack.pop() + " " + v;
      }
      parseStack.push("(" + v + ")*");
   }

   /**
    * fix up Ids
    */
   private String sanitiseID(String id) {
      id = id.trim();
      id = id.substring(1, id.length() - 1);
      id = id.replaceAll("-", "");
      id = id.replaceAll(" ", "");
      return id;
   }
}
