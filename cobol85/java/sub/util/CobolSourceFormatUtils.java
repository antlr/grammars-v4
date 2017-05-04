/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.util;

import com.google.common.base.Strings;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

public class CobolSourceFormatUtils {

	public static String getBlankComments() {
		return Strings.repeat(CobolPreprocessor.WS, 8);
	}

	public static String getBlankContentArea() {
		return Strings.repeat(CobolPreprocessor.WS, 65);
	}

	public static String getBlankIndicatorArea() {
		return CobolPreprocessor.WS;
	}

	public static String getBlankLine(final CobolSourceFormatEnum format) {
		final String result;

		if (format == null) {
			result = null;
		} else if (CobolSourceFormatEnum.TANDEM.equals(format)) {
			result = getBlankSequenceAndIndicatorArea(format);
		} else if (CobolSourceFormatEnum.VARIABLE.equals(format)) {
			result = getBlankSequenceAndIndicatorArea(format);
		} else {
			result = getBlankSequenceAndIndicatorArea(format) + getBlankContentArea() + getBlankComments();
		}

		return result;
	}

	public static String getBlankSequenceAndIndicatorArea(final CobolSourceFormatEnum format) {
		final String result;

		if (format == null) {
			result = null;
		} else {
			result = getBlankSequenceArea(format) + getBlankIndicatorArea();
		}

		return result;
	}

	public static String getBlankSequenceArea(final CobolSourceFormatEnum format) {
		final String result;

		if (format == null) {
			result = null;
		} else if (CobolSourceFormatEnum.TANDEM.equals(format)) {
			result = "";
		} else {
			result = Strings.repeat(CobolPreprocessor.WS, 6);
		}

		return result;
	}

}
