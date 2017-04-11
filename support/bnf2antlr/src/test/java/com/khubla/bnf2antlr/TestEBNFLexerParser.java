package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.khubla.bnf2antlr.ebnf.EBNFDocumentParser;
import com.khubla.ebnf.ebnfParser.RulelistContext;

/**
 * @author tom
 */
public class TestEBNFLexerParser {
   @Test
   public void testParser() {
      try {
         final InputStream inputStream = TestEBNFLexerParser.class.getResourceAsStream("/examples/pascal.ebnf");
         final RulelistContext rulelistContext = EBNFDocumentParser.parse(inputStream);
         Assert.assertNotNull(rulelistContext);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
