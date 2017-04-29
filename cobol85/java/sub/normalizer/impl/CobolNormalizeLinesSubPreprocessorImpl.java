/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.normalizer.impl;

import org.codehaus.plexus.util.StringUtils;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.impl.AbstractCobolSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.impl.CobolLine;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolNormalizeLinesSubPreprocessor;

public class CobolNormalizeLinesSubPreprocessorImpl extends AbstractCobolSubPreprocessor
		implements CobolNormalizeLinesSubPreprocessor {

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
	 * Normalizes the sequence and indicator area to NEWLINE and whitespace.
	 */
	protected String normalizeLineBreakAndSequenceArea(final CobolLine line, final boolean isFirstLine) {
		// newline
		final String newLine = isFirstLine ? "" : CobolPreprocessor.NEWLINE;

		// sequence area
		final String sequenceAreaPlaceholder = StringUtils.leftPad("", line.sequenceArea.length());

		final String result = newLine + sequenceAreaPlaceholder;
		return result;
	}

	/**
	 * Normalizes a line by stripping the sequence number and line indicator,
	 * and interpreting the line indicator.
	 */
	protected String processLine(final CobolLine line, final boolean isFirstLine) {
		final String result;

		// determine line prefix
		final String linePrefix = normalizeLineBreakAndSequenceArea(line, isFirstLine);

		// join content areas A and B
		final String joinedContentArea = line.contentAreaA + line.contentAreaB;

		// trim trailing whitespace
		final String trimmedTrailWsContentArea = joinedContentArea.replaceAll("\\s+$", "");

		// handle trailing comma
		final String handledContentArea = handleTrailingComma(trimmedTrailWsContentArea);

		/*
		 * switch on line indicator
		 */
		switch (line.indicatorArea) {
		// debugging line
		case CobolPreprocessor.CHAR_D:
		case CobolPreprocessor.CHAR_D_:
			result = linePrefix + CobolPreprocessor.WS + handledContentArea;
			break;
		// continuation line
		case CobolPreprocessor.CHAR_MINUS:
			final String trimmedContentArea = handledContentArea.trim();
			final char firstCharOfContentArea = trimmedContentArea.charAt(0);

			switch (firstCharOfContentArea) {
			case '\"':
			case '\'':
				result = trimmedContentArea.substring(1);
				break;
			default:
				result = trimmedContentArea;
				break;
			}
			break;
		// comment line
		case CobolPreprocessor.CHAR_ASTERISK:
		case CobolPreprocessor.CHAR_SLASH:
			result = linePrefix + CobolPreprocessor.COMMENT_TAG + CobolPreprocessor.WS + handledContentArea;
			break;
		// comment entry
		case CobolPreprocessor.CHAR_SHARP:
			result = linePrefix + CobolPreprocessor.COMMENT_ENTRY_TAG + CobolPreprocessor.WS + handledContentArea;
			break;
		case ' ':
		default:
			result = linePrefix + CobolPreprocessor.WS + handledContentArea;
			break;
		}

		return result;
	}

	@Override
	public String processLine(final String line, final int lineNumber, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final CobolLine parsedLine = parseCobolLine(line, format);

		if (parsedLine == null) {
			throwCobolLineParseException(line, lineNumber, format);
		}

		final boolean isFirstLine = lineNumber == 0;
		final String result = processLine(parsedLine, isFirstLine);
		return result;
	}

}
