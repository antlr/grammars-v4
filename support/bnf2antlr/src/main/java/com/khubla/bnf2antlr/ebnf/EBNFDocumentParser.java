package com.khubla.bnf2antlr.ebnf;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import com.khubla.ebnf.ebnfLexer;
import com.khubla.ebnf.ebnfListener;
import com.khubla.ebnf.ebnfParser;
import com.khubla.ebnf.ebnfParser.RulelistContext;

/**
 * @author tom
 */
public class EBNFDocumentParser {
   /**
    * parse a HTML document
    */
   public static RulelistContext parse(InputStream inputStream) throws Exception {
      try {
         final ebnfLexer lexer = new ebnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final ebnfParser parser = new ebnfParser(tokens);
         return parser.rulelist();
      } catch (final Exception e) {
         throw new Exception("Exception in parse", e);
      }
   }

   /**
    * parse a HTML document
    */
   public static void parse(InputStream inputStream, ebnfListener ebnfListener) throws Exception {
      try {
         final ebnfLexer lexer = new ebnfLexer(new ANTLRInputStream(inputStream));
         final CommonTokenStream tokens = new CommonTokenStream(lexer);
         final ebnfParser parser = new ebnfParser(tokens);
         final RulelistContext rulelistContext = parser.rulelist();
         final ParseTreeWalker walker = new ParseTreeWalker();
         walker.walk(ebnfListener, rulelistContext);
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