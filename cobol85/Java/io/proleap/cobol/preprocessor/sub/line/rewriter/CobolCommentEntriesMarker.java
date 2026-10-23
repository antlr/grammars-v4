/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.line.rewriter;

import io.proleap.cobol.preprocessor.sub.CobolLine;

/**
 * Preprocessor, which identifies and marks comment entries depending on the
 * COBOL dialect.
 */
public interface CobolCommentEntriesMarker extends CobolLineRewriter {

	CobolLine processLine(CobolLine line);
}
