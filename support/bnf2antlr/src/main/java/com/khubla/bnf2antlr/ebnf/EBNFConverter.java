package com.khubla.bnf2antlr.ebnf;

import java.io.InputStream;
import java.io.OutputStream;

import com.khubla.bnf2antlr.Converter;

/**
 * @author tom
 */
public class EBNFConverter implements Converter {
   /**
    * perform conversion
    */
   @Override
   public void convert(InputStream bnfInput, OutputStream antlrOutput) throws Exception {
      try {
         final EBNFListener ebnfListener = new EBNFListener(antlrOutput);
         EBNFDocumentParser.parse(bnfInput, ebnfListener);
      } catch (final Exception e) {
         throw new Exception("Exception in convert", e);
      }
   }
}
