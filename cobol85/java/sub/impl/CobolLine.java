/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.impl;

import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

/**
 * Representation of a Cobol line.
 */
public class CobolLine {

	public String comment;

	public String contentAreaA;

	public String contentAreaB;

	public char indicatorArea;

	public CobolSourceFormatEnum lineFormat;

	public String sequenceArea;

	public CobolLine(final String sequenceArea, final char indicatorArea, final String contentAreaA,
			final String contentAreaB, final String comment, final CobolSourceFormatEnum lineFormat) {
		this.sequenceArea = sequenceArea;
		this.indicatorArea = indicatorArea;
		this.contentAreaA = contentAreaA;
		this.contentAreaB = contentAreaB;
		this.comment = comment;
		this.lineFormat = lineFormat;
	}

	@Override
	public String toString() {
		return sequenceArea + indicatorArea + contentAreaA + contentAreaB + comment + " [" + lineFormat + "]";
	}
}
