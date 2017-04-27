/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.normalizer.impl;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.impl.AbstractCobolSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolCleanLinesSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.util.CobolSourceFormatUtils;

public class CobolCleanLinesSubPreprocessorImpl extends AbstractCobolSubPreprocessor
		implements CobolCleanLinesSubPreprocessor {

	@Override
	public String processLine(final String line, final int lineNumber, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		// clean line from certain ASCII chars
		final int substituteChar = 0x1A;
		final String cleanedLine = line.replace((char) substituteChar, ' ');
		final boolean isLineEmpty = cleanedLine.trim().isEmpty();
		final String result;

		if (isLineEmpty) {
			result = CobolSourceFormatUtils.getBlankLine(format) + CobolPreprocessor.NEWLINE;
		} else {
			result = cleanedLine + CobolPreprocessor.NEWLINE;
		}

		return result;
	}
}
