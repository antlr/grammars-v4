/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.line.writer.impl;

import java.util.List;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.CobolLineTypeEnum;
import io.proleap.cobol.preprocessor.sub.line.writer.CobolLineWriter;

public class CobolLineWriterImpl implements CobolLineWriter {

	@Override
	public String serialize(final List<CobolLine> lines) {
		final StringBuffer sb = new StringBuffer();

		for (final CobolLine line : lines) {
			final boolean notContinuationLine = !CobolLineTypeEnum.CONTINUATION.equals(line.type);

			if (notContinuationLine) {
				if (line.number > 0) {
					sb.append(CobolPreprocessor.NEWLINE);
				}

				sb.append(line.blankSequenceArea());
				sb.append(line.indicatorArea);
			}

			sb.append(line.getContentArea());
		}

		return sb.toString();
	}
}
