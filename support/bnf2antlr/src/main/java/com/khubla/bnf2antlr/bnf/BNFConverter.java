package com.khubla.bnf2antlr.bnf;

import java.io.InputStream;
import java.io.OutputStream;

import com.khubla.bnf2antlr.Converter;

/**
 * @author tom
 */
public class BNFConverter implements Converter {
   /**
    * perform conversion
    */
   @Override
   public void convert(InputStream bnfInput, OutputStream antlrOutput) throws Exception {
      try {
         final BNFListener bnfListener = new BNFListener(antlrOutput);
         BNFDocumentParser.parse(bnfInput, bnfListener);
      } catch (final Exception e) {
         throw new Exception("Exception in convert", e);
      }
   }
}
