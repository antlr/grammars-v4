/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.line.rewriter.impl;

import java.util.ArrayList;
import java.util.List;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.line.rewriter.CobolLineIndicatorProcessor;

public class CobolLineIndicatorProcessorImpl implements CobolLineIndicatorProcessor {

	protected String handleTrailingComma(final String contentArea) {
		final String result;

		/*
		 * repair trimmed whitespace after comma separator
		 */
		if (contentArea.isEmpty()) {
			result = contentArea;
		} else {
			final char lastCharAtTrimmedLineArea = contentArea.charAt(contentArea.length() - 1);

			if (lastCharAtTrimmedLineArea == ',' || lastCharAtTrimmedLineArea == ';') {
				result = contentArea + CobolPreprocessor.WS;
			} else {
				result = contentArea;
			}
		}

		return result;
	}

	/**
	 * Normalizes a line by stripping the sequence number and line indicator,
	 * and interpreting the line indicator.
	 */
	@Override
	public CobolLine processLine(final CobolLine line) {
		// trim trailing whitespace
		final String trimmedTrailWsContentArea = line.getContentArea().replaceAll("\\s+$", "");

		// handle trailing comma
		final String handledContentArea = handleTrailingComma(trimmedTrailWsContentArea);

		final CobolLine result;

		switch (line.type) {
		case DEBUG:
			result = CobolLine.with(line, CobolPreprocessor.WS, handledContentArea);
			break;
		case CONTINUATION:
			final String trimmedContentArea = handledContentArea.trim();
			final char firstCharOfContentArea = trimmedContentArea.charAt(0);

			switch (firstCharOfContentArea) {
			case '\"':
			case '\'':
				result = CobolLine.with(line, CobolPreprocessor.WS, trimmedContentArea.substring(1));
				break;
			default:
				result = CobolLine.with(line, CobolPreprocessor.WS, trimmedContentArea);
				break;
			}
			break;
		case COMMENT:
			result = CobolLine.with(line, CobolPreprocessor.COMMENT_TAG + CobolPreprocessor.WS, handledContentArea);
			break;
		case NORMAL:
		default:
			result = CobolLine.with(line, CobolPreprocessor.WS, handledContentArea);
			break;
		}

		return result;
	}

	@Override
	public List<CobolLine> processLines(final List<CobolLine> lines) {
		final List<CobolLine> result = new ArrayList<CobolLine>();

		for (final CobolLine line : lines) {
			final CobolLine processedLine = processLine(line);
			result.add(processedLine);
		}

		return result;
	}
}
