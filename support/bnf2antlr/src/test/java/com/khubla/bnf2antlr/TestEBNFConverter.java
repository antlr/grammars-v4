package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * @author tom
 */
public class TestEBNFConverter {
   @Test
   public void testConverter() {
      try {
         final InputStream inputStream = TestEBNFLexerParser.class.getResourceAsStream("/examples/pascal.ebnf");
         final EBNFConverter ebnfConverter = new EBNFConverter();
         ebnfConverter.convert(inputStream, System.out);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
