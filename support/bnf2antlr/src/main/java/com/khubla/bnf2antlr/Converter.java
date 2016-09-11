package com.khubla.bnf2antlr;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * @author tom
 */
public interface Converter {
   void convert(InputStream inputStream, OutputStream outputStream) throws Exception;
}
