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
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolDocumentParserImpl;
import io.proleap.cobol.preprocessor.sub.line.reader.impl.CobolLineReaderImpl;
import io.proleap.cobol.preprocessor.sub.line.rewriter.impl.CobolCommentEntriesMarkerImpl;
import io.proleap.cobol.preprocessor.sub.line.rewriter.impl.CobolLineIndicatorProcessorImpl;
import io.proleap.cobol.preprocessor.sub.line.writer.CobolLineWriter;
import io.proleap.cobol.preprocessor.sub.line.writer.impl.CobolLineWriterImpl;

public class CobolPreprocessorImpl implements CobolPreprocessor {

	private final static Logger LOG = LogManager.getLogger(CobolPreprocessorImpl.class);

	protected CobolCommentEntriesMarkerImpl createCommentEntriesMarker() {
		return new CobolCommentEntriesMarkerImpl();
	}

	protected CobolDocumentParserImpl createDocumentParser(final List<File> copyFiles) {
		return new CobolDocumentParserImpl(copyFiles);
	}

	protected CobolLineIndicatorProcessorImpl createLineIndicatorProcessor() {
		return new CobolLineIndicatorProcessorImpl();
	}

	protected CobolLineReaderImpl createLineReader() {
		return new CobolLineReaderImpl();
	}

	protected CobolLineWriter createLineWriter() {
		return new CobolLineWriterImpl();
	}

	protected String parseDocument(final List<CobolLine> lines, final List<File> copyFiles,
			final CobolSourceFormatEnum format, final CobolDialect dialect) {
		final String code = createLineWriter().serialize(lines);
		final String result = createDocumentParser(copyFiles).processLines(code, format, dialect);
		return result;
	}

	@Override
	public String process(final File cobolFile, final List<File> copyFiles, final CobolSourceFormatEnum format)
			throws IOException {
		return process(cobolFile, copyFiles, format, null);
	}

	@Override
	public String process(final File cobolFile, final List<File> copyFiles, final CobolSourceFormatEnum format,
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

		final String result = process(outputBuffer.toString(), copyFiles, format, dialect);
		return result;
	}

	@Override
	public String process(final String cobolSourceCode, final List<File> copyFiles,
			final CobolSourceFormatEnum format) {
		return process(cobolSourceCode, copyFiles, format, null);
	}

	@Override
	public String process(final String cobolCode, final List<File> copyFiles, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final List<CobolLine> lines = readLines(cobolCode, format, dialect);
		final List<CobolLine> rewrittenLines = rewriteLines(lines);
		final String result = parseDocument(rewrittenLines, copyFiles, format, dialect);

		LOG.debug("Processed input:\n\n{}\n\n", result);

		return result;
	}

	protected List<CobolLine> readLines(final String cobolCode, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final List<CobolLine> lines = createLineReader().processLines(cobolCode, format, dialect);
		return lines;
	}

	/**
	 * Normalizes lines of given COBOL source code, so that comment entries can
	 * be parsed and lines have a unified line format.
	 */
	protected List<CobolLine> rewriteLines(final List<CobolLine> lines) {
		final List<CobolLine> lineIndicatorProcessedLines = createLineIndicatorProcessor().processLines(lines);
		final List<CobolLine> result = createCommentEntriesMarker().processLines(lineIndicatorProcessedLines);
		return result;
	}
}
