/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.util;

import java.util.List;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;

import io.proleap.cobol.Cobol85PreprocessorLexer;
import io.proleap.cobol.preprocessor.sub.document.impl.CobolHiddenTokenCollectorListenerImpl;

public class TokenUtils {

	public static String getHiddenTokensToLeft(final int tokPos, final BufferedTokenStream tokens) {
		final List<Token> refChannel = tokens.getHiddenTokensToLeft(tokPos, Cobol85PreprocessorLexer.HIDDEN);
		final StringBuffer sb = new StringBuffer();

		if (refChannel != null) {
			for (final Token refToken : refChannel) {
				final String text = refToken.getText();
				sb.append(text);
			}
		}

		return sb.toString();
	}

	public static String getTextIncludingHiddenTokens(final ParseTree ctx, final BufferedTokenStream tokens) {
		final CobolHiddenTokenCollectorListenerImpl listener = new CobolHiddenTokenCollectorListenerImpl(tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();
		walker.walk(listener, ctx);

		return listener.read();
	}

	public static boolean isEOF(final TerminalNode node) {
		return Token.EOF == node.getSymbol().getType();
	}
}
