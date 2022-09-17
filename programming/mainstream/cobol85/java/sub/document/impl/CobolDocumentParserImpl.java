/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.document.impl;

import java.io.File;
import java.util.List;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import io.proleap.cobol.Cobol85PreprocessorLexer;
import io.proleap.cobol.Cobol85PreprocessorParser;
import io.proleap.cobol.Cobol85PreprocessorParser.StartRuleContext;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.document.CobolDocumentParser;

/**
 * Preprocessor, which parses and processes COPY REPLACE and EXEC SQL
 * statements.
 */
public class CobolDocumentParserImpl implements CobolDocumentParser {

	protected final List<File> copyFiles;

	protected final String[] triggers = new String[] { "copy", "exec sql", "exec sqlims", "exec cics", "replace" };

	public CobolDocumentParserImpl(final List<File> copyFiles) {
		this.copyFiles = copyFiles;
	}

	protected boolean containsTrigger(final String code, final String[] triggers) {
		final String codeLowerCase = code.toLowerCase();
		boolean result = false;

		for (final String trigger : triggers) {
			final boolean containsTrigger = codeLowerCase.contains(trigger);

			if (containsTrigger) {
				result = true;
				break;
			}
		}

		return result;
	}

	@Override
	public String processLines(final String code, final CobolSourceFormatEnum format, final CobolDialect dialect) {
		final boolean requiresProcessorExecution = containsTrigger(code, triggers);
		final String result;

		if (requiresProcessorExecution) {
			result = processWithParser(code, copyFiles, format, dialect);
		} else {
			result = code;
		}

		return result;
	}

	protected String processWithParser(final String code, final List<File> copyFiles,
			final CobolSourceFormatEnum format, final CobolDialect dialect) {
		// run the lexer
		final Cobol85PreprocessorLexer lexer = new Cobol85PreprocessorLexer(CharStreams.fromString(code));

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
		final CobolDocumentParserListenerImpl listener = new CobolDocumentParserListenerImpl(copyFiles, format, dialect,
				tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, startRule);

		final String result = listener.context().read();
		return result;
	}
}
