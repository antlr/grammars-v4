/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolCleanLinesSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolMarkCommentEntriesSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolNormalizeLinesSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.normalizer.impl.CobolCleanLinesSubPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.normalizer.impl.CobolMarkCommentEntriesSubPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.normalizer.impl.CobolNormalizeLinesSubPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.parser.CobolParserPreprocessor;
import io.proleap.cobol.preprocessor.sub.parser.impl.CobolParserPreprocessorImpl;

public class CobolPreprocessorImpl implements CobolPreprocessor {

	private final static Logger LOG = LogManager.getLogger(CobolPreprocessorImpl.class);

	/**
	 * Normalizes lines of given COBOL source code, so that comment entries can
	 * be parsed and lines have a unified line format.
	 */
	protected String normalizeLines(final String cobolSourceCode, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final CobolCleanLinesSubPreprocessor cleanLinesPreprocessor = new CobolCleanLinesSubPreprocessorImpl();
		final CobolMarkCommentEntriesSubPreprocessor markCommentEntriesPreprocessor = new CobolMarkCommentEntriesSubPreprocessorImpl();
		final CobolNormalizeLinesSubPreprocessor normalizeLinesPreprocessor = new CobolNormalizeLinesSubPreprocessorImpl();

		final String cleanedCode = cleanLinesPreprocessor.processLines(cobolSourceCode, format, dialect);
		final String markedCode = markCommentEntriesPreprocessor.processLines(cleanedCode, format, dialect);
		final String result = normalizeLinesPreprocessor.processLines(markedCode, format, dialect);
		return result;
	}

	@Override
	public String process(final File cobolFile, final File libDirectory, final CobolSourceFormatEnum format)
			throws IOException {
		return process(cobolFile, libDirectory, format, null);
	}

	@Override
	public String process(final File cobolFile, final File libDirectory, final CobolSourceFormatEnum format,
			final CobolDialect dialect) throws IOException {
		LOG.info("Preprocessing file {}.", cobolFile.getName());

		final InputStream inputStream = new FileInputStream(cobolFile);
		final InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
		final BufferedReader bufferedInputStreamReader = new BufferedReader(inputStreamReader);
		final StringBuffer outputBuffer = new StringBuffer();

		String line = null;

		while ((line = bufferedInputStreamReader.readLine()) != null) {
			outputBuffer.append(line + NEWLINE);
		}

		bufferedInputStreamReader.close();

		final String result = process(outputBuffer.toString(), libDirectory, format, dialect);
		return result;
	}

	@Override
	public String process(final String cobolSourceCode, final File libDirectory, final CobolSourceFormatEnum format) {
		return process(cobolSourceCode, libDirectory, format, null);
	}

	@Override
	public String process(final String cobolSourceCode, final File libDirectory, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final String normalizedCobolSourceCode = normalizeLines(cobolSourceCode, format, dialect);

		final CobolParserPreprocessor parserPreprocessor = new CobolParserPreprocessorImpl(libDirectory);
		final String result = parserPreprocessor.processLines(normalizedCobolSourceCode, format, dialect);

		LOG.debug("Processed input:\n\n{}\n\n", result);

		return result;
	}

}
