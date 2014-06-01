package com.khubla.bnf2antlr;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.antlr.abnf.AbnfLexer;
import org.antlr.abnf.AbnfListener;
import org.antlr.abnf.AbnfParser;
import org.antlr.abnf.AbnfParser.RulelistContext;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

/**
 * @author tom
 */
public class BNFDocumentParser {
   /**
    * parse a HTML document
    */
   public static RulelistContext parse(InputStream inputStream) throws Exception {
      try {
         final AbnfLexer lexer = new AbnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final AbnfParser parser = new AbnfParser(tokens);
         return parser.rulelist();
      } catch (final Exception e) {
         throw new Exception("Exception in parse", e);
      }
   }

   /**
    * parse a HTML document
    */
   public static void parse(InputStream inputStream, AbnfListener abnfListener) throws Exception {
      try {
         final AbnfLexer lexer = new AbnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final AbnfParser parser = new AbnfParser(tokens);
         final RulelistContext rulelistContext = parser.rulelist();
         final ParseTreeWalker walker = new ParseTreeWalker();
         walker.walk(abnfListener, rulelistContext);
      } catch (final Exception e) {
         throw new Exception("Exception in parse", e);
      }
   }

   /**
    * parse a HTML document
    */
   public static RulelistContext parse(String bnfBlock) throws Exception {
      try {
         return parse(new ByteArrayInputStream(bnfBlock.getBytes()));
      } catch (final Exception e) {
         throw new Exception("Exception in parse", e);
      }
   }
}