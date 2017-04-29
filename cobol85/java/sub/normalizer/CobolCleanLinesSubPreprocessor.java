/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.normalizer;

import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.CobolSubPreprocessor;

/**
 * Preprocessor, which removes problematic chars from lines.
 */
public interface CobolCleanLinesSubPreprocessor extends CobolSubPreprocessor {

	String processLine(String line, int lineNumber, CobolSourceFormatEnum format, CobolDialect dialect);
}
