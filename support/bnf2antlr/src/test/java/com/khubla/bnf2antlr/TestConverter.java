package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

/**
 * @author tom
 */
public class TestConverter {
   @Test
   public void testConverter() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/pascal.bnf");
         BNFConverter bnfConverter = new BNFConverter();
         bnfConverter.convert(inputStream, System.out);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
