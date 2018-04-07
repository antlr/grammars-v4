/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.line.rewriter;

import java.util.List;

import io.proleap.cobol.preprocessor.sub.CobolLine;

public interface CobolLineRewriter {

	List<CobolLine> processLines(List<CobolLine> lines);

}
