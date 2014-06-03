package com.khubla.bnf2antlr;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import org.testng.Assert;
import org.testng.annotations.Test;

import com.khubla.bnf2antlr.ebnf.EBNFConverter;

/**
 * @author tom
 */
public class TestEBNFConverter {
   @Test
   public void testConverter() {
      try {
         final InputStream inputStream = TestEBNFLexerParser.class.getResourceAsStream("/examples/pascal.ebnf");
         final EBNFConverter ebnfConverter = new EBNFConverter();
         ByteArrayOutputStream baos = new ByteArrayOutputStream();
         ebnfConverter.convert(inputStream, baos);
         String grammar = baos.toString();
         Assert.assertTrue(grammar.length() > 0);
         System.out.println(grammar);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
