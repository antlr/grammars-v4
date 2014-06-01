package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.antlr.abnf.AbnfParser.RulelistContext;
import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * @author tom
 */
public class TestBNFLexerParser {
   @Test
   public void testParser() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/pascal.bnf");
         final RulelistContext rulelistContext = BNFDocumentParser.parse(inputStream);
         Assert.assertNotNull(rulelistContext);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
