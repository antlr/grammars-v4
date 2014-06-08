package com.khubla.bnf2antlr;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.util.Date;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.io.IOUtils;
import org.stringtemplate.v4.AutoIndentWriter;
import org.stringtemplate.v4.ST;

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
    * name
    */
   private static final String NAME = "name";
   /**
    * output
    */
   private static final String OUTPUT_FILE = "output";
   /**
    * type
    */
   private static final String GRAMMAR_TYPE = "type";
   /**
    * template
    */
   private static final String TEMPLATE = "/grammar.st";

   /**
    * perform conversion
    */
   private static void convert(Converter converter, String grammarName, InputStream inputStream, OutputStream outputStream) throws Exception {
      /*
       * convert into outputstream
       */
      final ByteArrayOutputStream baos = new ByteArrayOutputStream();
      converter.convert(inputStream, baos);
      /*
       * template
       */
      final String template = IOUtils.toString(BNF2ANTLR.class.getResourceAsStream(TEMPLATE), "UTF-8");
      /*
       * ST
       */
      final ST st = new ST(template);
      st.add("grammar", baos.toString());
      st.add("date", new Date().toString());
      st.add("grammarName", grammarName);
      /*
       * done
       */
      final Writer writer = new OutputStreamWriter(outputStream);
      st.write(new AutoIndentWriter(writer));
      writer.flush();
   }

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
          * output
          */
         OptionBuilder.withArgName(OUTPUT_FILE);
         OptionBuilder.hasArg();
         OptionBuilder.withDescription("output file");
         OptionBuilder.isRequired(true);
         final Option off = OptionBuilder.create(OUTPUT_FILE);
         options.addOption(off);
         /*
          * name
          */
         OptionBuilder.withArgName(NAME);
         OptionBuilder.hasArg();
         OptionBuilder.withDescription("grammar name");
         OptionBuilder.isRequired(true);
         final Option nn = OptionBuilder.create(NAME);
         options.addOption(nn);
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
         final String inputFileName = cmd.getOptionValue(INPUT_FILE);
         final String outputFileName = cmd.getOptionValue(OUTPUT_FILE);
         final String grammarType = cmd.getOptionValue(GRAMMAR_TYPE);
         final String grammarName = cmd.getOptionValue(NAME);
         /*
          * get file
          */
         if (new File(inputFileName).exists()) {
            final FileInputStream fis = new FileInputStream(inputFileName);
            /*
             * get converter
             */
            Converter converter = null;
            if (grammarType.toLowerCase().compareTo("bnf") == 0) {
               converter = new BNFConverter();
            } else if (grammarType.toLowerCase().compareTo("ebnf") == 0) {
               converter = new EBNFConverter();
            } else {
               System.out.println("Unknown grammar type '" + grammarType + "'");
            }
            /*
             * output
             */
            final OutputStream os = new FileOutputStream(outputFileName, false);
            /*
             * convert
             */
            convert(converter, grammarName, fis, os);
         } else {
            System.out.println("Could not find file '" + inputFileName + "'");
         }
      } catch (final Exception e) {
         e.printStackTrace();
      }
   }
}
