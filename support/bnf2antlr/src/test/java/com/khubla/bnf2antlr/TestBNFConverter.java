package com.khubla.bnf2antlr;

import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.khubla.bnf2antlr.bnf.BNFConverter;

/**
 * @author tom
 */
public class TestBNFConverter {
   @Test
   public void testConverter() {
      try {
         final InputStream inputStream = TestBNFLexerParser.class.getResourceAsStream("/examples/pascal.bnf");
         final BNFConverter bnfConverter = new BNFConverter();
         bnfConverter.convert(inputStream, System.out);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
