package com.khubla.bnf2antlr;

import java.io.ByteArrayOutputStream;
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
         final ByteArrayOutputStream baos = new ByteArrayOutputStream();
         bnfConverter.convert(inputStream, baos);
         final String grammar = baos.toString();
         Assert.assertTrue(grammar.length() > 0);
         System.out.println(grammar);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
