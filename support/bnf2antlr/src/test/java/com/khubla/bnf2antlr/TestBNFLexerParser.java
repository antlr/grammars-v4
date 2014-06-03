package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.khubla.bnf.bnfParser.RulelistContext;
import com.khubla.bnf2antlr.bnf.BNFDocumentParser;

/**
 * @author tom
 */
public class TestBNFLexerParser {
   @Test
   public void testParser() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/examples/pascal.bnf");
         final RulelistContext rulelistContext = BNFDocumentParser.parse(inputStream);
         Assert.assertNotNull(rulelistContext);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
