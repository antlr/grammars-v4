/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub;

import com.google.common.base.Strings;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

/**
 * Representation of a Cobol line.
 */
public class CobolLine {

	public static String blankSequenceArea(final CobolSourceFormatEnum format) {
		return CobolSourceFormatEnum.TANDEM.equals(format) ? "" : Strings.repeat(CobolPreprocessor.WS, 6);
	}

	private static String contentAreaA(final String contentArea) {
		return contentArea.length() > 4 ? contentArea.substring(0, 4) : contentArea;
	}

	private static String contentAreaB(final String contentArea) {
		return contentArea.length() > 4 ? contentArea.substring(4) : "";
	}

	public static CobolLine with(final CobolLine line, final String indicatorArea, final String contentArea) {
		return new CobolLine(line.sequenceArea, indicatorArea, contentAreaA(contentArea), contentAreaB(contentArea),
				line.comment, line.format, line.dialect, line.number, line.type);
	}

	public static CobolLine withContentArea(final CobolLine line, final String contentArea) {
		return new CobolLine(line.sequenceArea, line.indicatorArea, contentAreaA(contentArea),
				contentAreaB(contentArea), line.comment, line.format, line.dialect, line.number, line.type);
	}

	public String comment;

	public String contentAreaA;

	public String contentAreaB;

	public CobolDialect dialect;

	public CobolSourceFormatEnum format;

	public String indicatorArea;

	public int number;

	public String sequenceArea;

	public CobolLineTypeEnum type;

	public CobolLine(final String sequenceArea, final String indicatorArea, final String contentAreaA,
			final String contentAreaB, final String comment, final CobolSourceFormatEnum format,
			final CobolDialect dialect, final int number, final CobolLineTypeEnum type) {
		this.sequenceArea = sequenceArea;
		this.indicatorArea = indicatorArea;
		this.contentAreaA = contentAreaA;
		this.contentAreaB = contentAreaB;
		this.comment = comment;

		this.format = format;
		this.dialect = dialect;
		this.number = number;
		this.type = type;
	}

	public String blankSequenceArea() {
		return blankSequenceArea(format);
	}

	public String getContentArea() {
		return contentAreaA + contentAreaB;
	}

	public String serialize() {
		return sequenceArea + indicatorArea + contentAreaA + contentAreaB + comment;
	}

	@Override
	public String toString() {
		return serialize();
	}
}
