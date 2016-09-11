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
   public void test1() {
      testConverter("/examples/pascal.ebnf");
   }

   @Test
   public void test2() {
      testConverter("/examples/modelica33.ebnf");
   }

   @Test
   private void testConverter(String fn) {
      try {
         final InputStream inputStream = TestEBNFLexerParser.class.getResourceAsStream(fn);
         final EBNFConverter ebnfConverter = new EBNFConverter();
         final ByteArrayOutputStream baos = new ByteArrayOutputStream();
         ebnfConverter.convert(inputStream, baos);
         final String grammar = baos.toString();
         Assert.assertTrue(grammar.length() > 0);
         System.out.println(grammar);
      } catch (final Exception e) {
         e.printStackTrace();
         Assert.fail();
      }
   }
}
