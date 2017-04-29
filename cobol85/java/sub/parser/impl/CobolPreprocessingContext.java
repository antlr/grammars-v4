/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.parser.impl;

import java.util.Arrays;
import java.util.List;

import org.antlr.v4.runtime.BufferedTokenStream;

import io.proleap.cobol.Cobol85PreprocessorParser.ReplaceClauseContext;

/**
 * A replacement context that defines, which replaceables should be replaced by
 * which replacements.
 */
public class CobolPreprocessingContext {

	private CobolReplacementMapping[] currentReplaceableReplacements;

	private StringBuffer outputBuffer = new StringBuffer();

	public String read() {
		return outputBuffer.toString();
	}

	/**
	 * Replaces replaceables with replacements.
	 */
	public void replaceReplaceablesByReplacements(final BufferedTokenStream tokens) {
		if (currentReplaceableReplacements != null) {
			Arrays.sort(currentReplaceableReplacements);

			for (final CobolReplacementMapping replaceableReplacement : currentReplaceableReplacements) {
				final String currentOutput = outputBuffer.toString();
				final String replacedOutput = replaceableReplacement.replace(currentOutput, tokens);

				outputBuffer = new StringBuffer();
				outputBuffer.append(replacedOutput);
			}
		}
	}

	public void storeReplaceablesAndReplacements(final List<ReplaceClauseContext> replaceClauses) {
		if (replaceClauses == null) {
			currentReplaceableReplacements = null;
		} else {
			final int length = replaceClauses.size();
			currentReplaceableReplacements = new CobolReplacementMapping[length];

			int i = 0;

			for (final ReplaceClauseContext replaceClause : replaceClauses) {
				final CobolReplacementMapping replaceableReplacement = new CobolReplacementMapping();

				replaceableReplacement.replaceable = replaceClause.replaceable();
				replaceableReplacement.replacement = replaceClause.replacement();

				currentReplaceableReplacements[i] = replaceableReplacement;
				i++;
			}
		}
	}

	public void write(final String text) {
		outputBuffer.append(text);
	}
}
