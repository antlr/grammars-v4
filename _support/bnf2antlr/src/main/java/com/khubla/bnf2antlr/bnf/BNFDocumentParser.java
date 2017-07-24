package com.khubla.bnf2antlr.bnf;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import com.khubla.bnf.bnfLexer;
import com.khubla.bnf.bnfListener;
import com.khubla.bnf.bnfParser;
import com.khubla.bnf.bnfParser.RulelistContext;

/**
 * @author tom
 */
public class BNFDocumentParser {
   /**
    * parse a HTML document
    */
   public static RulelistContext parse(InputStream inputStream) throws Exception {
      try {
         final bnfLexer lexer = new bnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final bnfParser parser = new bnfParser(tokens);
         return parser.rulelist();
      } catch (final Exception e) {
         throw new Exception("Exception in parse", e);
      }
   }

   /**
    * parse a HTML document
    */
   public static void parse(InputStream inputStream, bnfListener bnfListener) throws Exception {
      try {
         final bnfLexer lexer = new bnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final bnfParser parser = new bnfParser(tokens);
         final RulelistContext rulelistContext = parser.rulelist();
         final ParseTreeWalker walker = new ParseTreeWalker();
         walker.walk(bnfListener, rulelistContext);
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