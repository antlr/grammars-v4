/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.parser.impl;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.codehaus.plexus.util.StringUtils;

import io.proleap.cobol.Cobol85PreprocessorParser.PseudoTextContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplaceableContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplacementContext;
import io.proleap.cobol.preprocessor.sub.util.TokenUtils;

/**
 * A mapping from a replaceable to a replacement.
 */
public class CobolReplacementMapping implements Comparable<CobolReplacementMapping> {

	public ReplaceableContext replaceable;

	public ReplacementContext replacement;

	@Override
	public int compareTo(final CobolReplacementMapping o) {
		return o.replaceable.getText().length() - replaceable.getText().length();
	}

	private String extractPseudoText(final PseudoTextContext pseudoTextCtx, final BufferedTokenStream tokens) {
		final String pseudoText = TokenUtils.getTextIncludingHiddenTokens(pseudoTextCtx, tokens).trim();
		final String content = pseudoText.replaceAll("^==", "").replaceAll("==$", "").trim();
		return content;
	}

	/**
	 * Whitespace in Cobol replaceables matches line breaks. Hence, the
	 * replaceable search string has to be enhanced to a regex, which is
	 * returned by this function.
	 */
	private String getRegexFromReplaceable(final String replaceable) {
		final String result;

		if (replaceable == null) {
			result = null;
		} else {
			final String[] parts = StringUtils.split(replaceable);
			final String[] regexParts = new String[parts.length];
			final String regexSeparator = "[\\r\\n\\s]+";

			for (int i = 0; i < parts.length; i++) {
				final String part = parts[i];
				regexParts[i] = Pattern.quote(part);
			}

			result = StringUtils.join(regexParts, regexSeparator);
		}

		return result;
	}

	private String getText(final ReplaceableContext ctx, final BufferedTokenStream tokens) {
		final String result;

		if (ctx.pseudoText() != null) {
			result = extractPseudoText(ctx.pseudoText(), tokens);
		} else if (ctx.charDataLine() != null) {
			result = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
		} else if (ctx.cobolWord() != null) {
			result = ctx.getText();
		} else if (ctx.literal() != null) {
			result = ctx.literal().getText();
		} else {
			result = null;
		}

		return result;
	}

	private String getText(final ReplacementContext ctx, final BufferedTokenStream tokens) {
		final String result;

		if (ctx.pseudoText() != null) {
			result = extractPseudoText(ctx.pseudoText(), tokens);
		} else if (ctx.charDataLine() != null) {
			result = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
		} else if (ctx.cobolWord() != null) {
			result = ctx.getText();
		} else if (ctx.literal() != null) {
			result = ctx.literal().getText();
		} else {
			result = null;
		}

		return result;
	}

	protected String replace(final String string, final BufferedTokenStream tokens) {
		final String replaceableString = getText(replaceable, tokens);
		final String replacementString = getText(replacement, tokens);

		final String result;

		if (replaceableString != null && replacementString != null) {
			// regex for the replaceable
			final String replaceableRegex = getRegexFromReplaceable(replaceableString);

			// regex for the replacement
			final String quotedReplacementRegex = Matcher.quoteReplacement(replacementString);

			result = Pattern.compile(replaceableRegex).matcher(string).replaceAll(quotedReplacementRegex);
		} else {
			result = string;
		}

		return result;
	}

	@Override
	public String toString() {
		return replaceable.getText() + " -> " + replacement.getText();
	}
}
