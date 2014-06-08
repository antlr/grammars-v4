package com.khubla.bnf2antlr;

import java.io.File;
import java.io.FileInputStream;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;

import com.khubla.bnf2antlr.bnf.BNFConverter;
import com.khubla.bnf2antlr.ebnf.EBNFConverter;

/**
 * @author tom
 */
public class BNF2ANTLR {
   /**
    * input
    */
   private static final String INPUT_FILE = "input";
   /**
    * type
    */
   private static final String GRAMMAR_TYPE = "type";

   /**
    * void main
    */
   public static void main(String args[]) throws java.io.IOException, java.io.FileNotFoundException {
      try {
         /*
          * options
          */
         final Options options = new Options();
         /*
          * input
          */
         OptionBuilder.withArgName(INPUT_FILE);
         OptionBuilder.hasArg();
         OptionBuilder.withDescription("input file");
         OptionBuilder.isRequired(true);
         final Option iff = OptionBuilder.create(INPUT_FILE);
         options.addOption(iff);
         /*
          * type
          */
         OptionBuilder.withArgName(GRAMMAR_TYPE);
         OptionBuilder.hasArg();
         OptionBuilder.withDescription("grammar type");
         OptionBuilder.isRequired(true);
         final Option gt = OptionBuilder.create(GRAMMAR_TYPE);
         options.addOption(gt);
         /*
          * parse
          */
         final CommandLineParser parser = new PosixParser();
         final CommandLine cmd = parser.parse(options, args);
         final String filename = cmd.getOptionValue(INPUT_FILE);
         final String grammarType = cmd.getOptionValue(GRAMMAR_TYPE);
         /*
          * get file
          */
         if (new File(filename).exists()) {
            final FileInputStream fis = new FileInputStream(filename);
            /*
             * convert
             */
            if (grammarType.toLowerCase().compareTo("bnf") == 0) {
               final BNFConverter bnfConverter = new BNFConverter();
               bnfConverter.convert(fis, System.out);
            } else if (grammarType.toLowerCase().compareTo("ebnf") == 0) {
               final EBNFConverter ebnfConverter = new EBNFConverter();
               ebnfConverter.convert(fis, System.out);
            } else {
               System.out.println("Unknown grammar type '" + grammarType + "'");
            }
         } else {
            System.out.println("Could not find file '" + filename + "'");
         }
      } catch (final Exception e) {
         e.printStackTrace();
      }
   }
}
