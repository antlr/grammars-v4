package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.khubla.bnf.bnfParser.RulelistContext;

/**
 * @author tom
 */
public class TestBNFLexerParser {
   @Test
   public void testParser1() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/examples/pascal.bnf");
         final RulelistContext rulelistContext = BNFDocumentParser.parse(inputStream);
         Assert.assertNotNull(rulelistContext);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }

   @Test
   public void testParser2() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/examples/algol60.bnf");
         final RulelistContext rulelistContext = BNFDocumentParser.parse(inputStream);
         Assert.assertNotNull(rulelistContext);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
