package com.khubla.bnf2antlr;

import java.io.FileInputStream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;

/**
 * @author tom
 */
public class BNF2ANTLR {
   /**
    * input
    */
   private static final String INPUT_FILE = "input";

   /**
    * void main
    */
   public static void main(String args[]) throws java.io.IOException, java.io.FileNotFoundException {
      try {
         /*
          * options
          */
         final Options options = new Options();
         OptionBuilder.withArgName(INPUT_FILE);
         OptionBuilder.hasArg();
         OptionBuilder.withDescription("input file");
         final Option oo = OptionBuilder.create(INPUT_FILE);
         options.addOption(oo);
         /*
          * parse
          */
         final CommandLineParser parser = new PosixParser();
         final CommandLine cmd = parser.parse(options, args);
         final String filename = cmd.getOptionValue(INPUT_FILE);
         /*
          * get file
          */
         final FileInputStream fis = new FileInputStream(filename);
         /*
          * convert
          */
         final BNFConverter bnfConverter = new BNFConverter();
         bnfConverter.convert(fis, System.out);
      } catch (final Exception e) {
         e.printStackTrace();
      }
   }
}
