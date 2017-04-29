/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.parser.impl;

import java.io.File;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import io.proleap.cobol.Cobol85PreprocessorLexer;
import io.proleap.cobol.Cobol85PreprocessorParser;
import io.proleap.cobol.Cobol85PreprocessorParser.StartRuleContext;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.impl.ThrowingErrorListener;
import io.proleap.cobol.preprocessor.sub.parser.CobolParserPreprocessor;

/**
 * Preprocessor, which parses and processes COPY REPLACE and EXEC SQL
 * statements.
 */
public class CobolParserPreprocessorImpl implements CobolParserPreprocessor {

	protected final File libDirectory;

	protected final String[] triggers = new String[] { "copy", "exec sql", "exec sqlims", "exec cics", "replace" };

	public CobolParserPreprocessorImpl(final File libDirectory) {
		this.libDirectory = libDirectory;
	}

	protected boolean containsTrigger(final String line, final String[] triggers) {
		final String lineLowerCase = line.toLowerCase();
		boolean result = false;

		for (final String trigger : triggers) {
			final boolean containsTrigger = lineLowerCase.contains(trigger);

			if (containsTrigger) {
				result = true;
				break;
			}
		}

		return result;
	}

	@Override
	public String processLines(final String lines, final CobolSourceFormatEnum format, final CobolDialect dialect) {
		final boolean requiresProcessorExecution = containsTrigger(lines, triggers);
		final String result;

		if (requiresProcessorExecution) {
			result = processWithParser(lines, libDirectory, format, dialect);
		} else {
			result = lines;
		}

		return result;
	}

	protected String processWithParser(final String program, final File libDirectory,
			final CobolSourceFormatEnum format, final CobolDialect dialect) {
		// run the lexer
		final Cobol85PreprocessorLexer lexer = new Cobol85PreprocessorLexer(CharStreams.fromString(program));

		// get a list of matched tokens
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// pass the tokens to the parser
		final Cobol85PreprocessorParser parser = new Cobol85PreprocessorParser(tokens);

		// register an error listener, so that preprocessing stops on errors
		parser.removeErrorListeners();
		parser.addErrorListener(new ThrowingErrorListener());

		// specify our entry point
		final StartRuleContext startRule = parser.startRule();

		// analyze contained copy books
		final CobolParserPreprocessorListenerImpl listener = new CobolParserPreprocessorListenerImpl(libDirectory,
				format, dialect, tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, startRule);

		final String result = listener.context().read();
		return result;
	}
}
