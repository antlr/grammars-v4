/*
Copyright (C) 2015 u.wol@wwu.de

This file is part of cobol85grammar.

cobol85grammar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

cobol85grammar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with cobol85grammar. If not, see <http://www.gnu.org/licenses/>.
 */

package org.cobol85.preprocessor.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Scanner;
import java.util.Stack;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.misc.Nullable;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.cobol85.Cobol85PreprocessorBaseListener;
import org.cobol85.Cobol85PreprocessorLexer;
import org.cobol85.Cobol85PreprocessorParser;
import org.cobol85.Cobol85PreprocessorParser.PseudoTextContext;
import org.cobol85.Cobol85PreprocessorParser.ReplaceClauseContext;
import org.cobol85.Cobol85PreprocessorParser.ReplaceableContext;
import org.cobol85.Cobol85PreprocessorParser.ReplacementContext;
import org.cobol85.Cobol85PreprocessorParser.ReplacingPhraseContext;
import org.cobol85.Cobol85PreprocessorParser.StartRuleContext;
import org.cobol85.preprocessor.Cobol85Preprocessor;
import org.codehaus.plexus.util.StringUtils;

public class Cobol85PreprocessorImpl implements Cobol85Preprocessor {

	/**
	 * ANTLR listener, which collects visible as well as hidden tokens for a
	 * given parse tree in a string buffer.
	 */
	private class Cobol85HiddenTokenCollectorImpl extends
			Cobol85PreprocessorBaseListener {

		boolean firstTerminal = true;

		private final StringBuffer outputBuffer = new StringBuffer();

		private final BufferedTokenStream tokens;

		public Cobol85HiddenTokenCollectorImpl(final BufferedTokenStream tokens) {
			this.tokens = tokens;
		}

		private String read() {
			return outputBuffer.toString();
		}

		@Override
		public void visitTerminal(@NotNull final TerminalNode node) {
			if (!firstTerminal) {
				final int tokPos = node.getSourceInterval().a;
				outputBuffer.append(getHiddenTokensToLeft(tokens, tokPos));
			}

			if (!isEOF(node)) {
				final String text = node.getText();
				outputBuffer.append(text);
			}

			firstTerminal = false;
		}
	}

	/**
	 * ANTLR visitor, which preprocesses a given COBOL program by executing COPY
	 * and REPLACE statemenets.
	 */
	private class Cobol85PreprocessingListenerImpl extends
			Cobol85PreprocessorBaseListener {

		/**
		 * A replacement context that defines, which replaceables should be
		 * replaced by which replacements.
		 */
		private class PreprocessingContext {

			/**
			 * A mapping from a replaceable to a replacement.
			 */
			private class ReplacementMapping {

				private ReplaceableContext replaceable;

				private ReplacementContext replacement;

				private String extractPseudoText(
						final PseudoTextContext pseudoTextCtx) {
					final String pseudoText = getTextIncludingHiddenTokens(
							pseudoTextCtx, tokens).trim();
					final String content = pseudoText.replaceAll("^==", "")
							.replaceAll("==$", "").trim();
					return content;
				}

				/**
				 * Whitespace in Cobol replaceables matches line breaks. Hence,
				 * the replaceable search string has to be enhanced to a regex,
				 * which is returned by this function.
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

				private String getText(final ReplaceableContext ctx) {
					final String result;

					if (ctx.pseudoText() != null) {
						result = extractPseudoText(ctx.pseudoText());
					} else if (ctx.charDataLine() != null) {
						result = getTextIncludingHiddenTokens(ctx, tokens);
					} else if (ctx.cobolWord() != null) {
						result = ctx.getText();
					} else if (ctx.literal() != null) {
						result = ctx.literal().getText();
					} else {
						result = null;
					}

					return result;
				}

				private String getText(final ReplacementContext ctx) {
					final String result;

					if (ctx.pseudoText() != null) {
						result = extractPseudoText(ctx.pseudoText());
					} else if (ctx.charDataLine() != null) {
						result = getTextIncludingHiddenTokens(ctx, tokens);
					} else if (ctx.cobolWord() != null) {
						result = ctx.getText();
					} else if (ctx.literal() != null) {
						result = ctx.literal().getText();
					} else {
						result = null;
					}

					return result;
				}

				protected String replace(final String string) {
					final String replaceableString = getText(replaceable);
					final String replacementString = getText(replacement);

					final String result;

					if (replaceableString != null && replacementString != null) {
						// regex for the replaceable
						final String replaceableRegex = getRegexFromReplaceable(replaceableString);

						// regex for the replacement
						final String quotedReplacementRegex = Matcher
								.quoteReplacement(replacementString);

						result = Pattern.compile(replaceableRegex)
								.matcher(string)
								.replaceAll(quotedReplacementRegex);
					} else {
						result = string;
					}

					return result;
				}

				@Override
				public String toString() {
					return replaceable.getText() + " -> "
							+ replacement.getText();
				}
			}

			private ReplacementMapping[] currentReplaceableReplacements;

			private StringBuffer outputBuffer = new StringBuffer();

			private String read() {
				return outputBuffer.toString();
			}

			public void replace() {
				if (currentReplaceableReplacements != null) {
					for (final ReplacementMapping replaceableReplacement : currentReplaceableReplacements) {
						final String currentOutput = outputBuffer.toString();
						final String replacedOutput = replaceableReplacement
								.replace(currentOutput);

						outputBuffer = new StringBuffer();
						outputBuffer.append(replacedOutput);
					}
				}
			}

			private void storeReplaceablesAndReplacements(
					final List<ReplaceClauseContext> replaceClauses) {
				if (replaceClauses == null) {
					currentReplaceableReplacements = null;
				} else {
					final int length = replaceClauses.size();
					currentReplaceableReplacements = new ReplacementMapping[length];

					int i = 0;

					for (final ReplaceClauseContext replaceClause : replaceClauses) {
						final ReplacementMapping replaceableReplacement = new ReplacementMapping();

						replaceableReplacement.replaceable = replaceClause
								.replaceable();
						replaceableReplacement.replacement = replaceClause
								.replacement();

						currentReplaceableReplacements[i] = replaceableReplacement;
						i++;
					}
				}
			}

			private void write(final String text) {
				outputBuffer.append(text);
			}
		}

		private final Stack<PreprocessingContext> contexts = new Stack<PreprocessingContext>();

		private final File libDirectory;

		private final BufferedTokenStream tokens;

		public Cobol85PreprocessingListenerImpl(final File libDirectory,
				final BufferedTokenStream tokens) {
			this.libDirectory = libDirectory;
			this.tokens = tokens;

			contexts.push(new PreprocessingContext());
		}

		private PreprocessingContext context() {
			return contexts.peek();
		}

		@Override
		public void enterCopyStatement(
				@NotNull final Cobol85PreprocessorParser.CopyStatementContext ctx) {
			// push a new context for COPY terminals
			push();
		}

		@Override
		public void enterReplaceArea(
				@NotNull final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
			push();
		}

		@Override
		public void enterReplaceByStatement(
				@NotNull final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
			push();
		}

		@Override
		public void enterReplaceOffStatement(
				@NotNull final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
			push();
		}

		@Override
		public void exitCopyStatement(
				@NotNull final Cobol85PreprocessorParser.CopyStatementContext ctx) {
			// throw away COPY terminals
			pop();

			// a new context for the copy file content
			push();

			/*
			 * replacement phrase
			 */
			final ReplacingPhraseContext replacingPhrase = ctx
					.replacingPhrase();

			if (replacingPhrase != null) {
				context().storeReplaceablesAndReplacements(
						replacingPhrase.replaceClause());
			}

			/*
			 * copy the copy file
			 */
			final String copyFileIdentifier = ctx.copySource().getText();
			final String fileContent = getCopyFileContent(copyFileIdentifier,
					libDirectory);

			if (fileContent != null) {
				context().write(fileContent + NEWLINE);
				context().replace();
			}

			final String content = context().read();
			pop();

			context().write(content);
		}

		@Override
		public void exitReplaceArea(
				@NotNull final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
			/*
			 * replacement phrase
			 */
			final List<ReplaceClauseContext> replaceClauses = ctx
					.replaceByStatement().replaceClause();
			context().storeReplaceablesAndReplacements(replaceClauses);

			context().replace();
			final String content = context().read();

			pop();
			context().write(content);
		}

		@Override
		public void exitReplaceByStatement(
				@NotNull final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
			// throw away REPLACE BY terminals
			pop();
		}

		@Override
		public void exitReplaceOffStatement(
				@NotNull final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
			// throw away REPLACE OFF terminals
			pop();
		}

		private PreprocessingContext pop() {
			return contexts.pop();
		}

		private PreprocessingContext push() {
			return contexts.push(new PreprocessingContext());
		}

		@Override
		public void visitTerminal(@NotNull final TerminalNode node) {
			final int tokPos = node.getSourceInterval().a;
			context().write(getHiddenTokensToLeft(tokens, tokPos));

			if (!isEOF(node)) {
				final String text = node.getText();
				context().write(text);
			}
		}
	}

	private class ThrowingErrorListener extends BaseErrorListener {

		@Override
		public void syntaxError(@NotNull final Recognizer<?, ?> recognizer,
				@Nullable final Object offendingSymbol, final int line,
				final int charPositionInLine, @NotNull final String msg,
				@Nullable final RecognitionException e) {
			throw new RuntimeException("syntax error in line " + line + ":"
					+ charPositionInLine + " " + msg);
		}
	}

	protected final static String COMMENT_TAG = ">*";

	protected final static String LINEINDICATOR_PLACEHOLDER = " ";

	protected final static String LINENUMBER_PLACEHOLDER = "      ";

	private final static Logger LOG = LogManager
			.getLogger(Cobol85PreprocessorImpl.class);

	protected final static String NEWLINE = "\n";

	protected final String[] extensions = new String[] { "", "CPY", "COB",
			"CBL" };

	protected Cobol85Format determineFormat(final String line) {
		Cobol85Format result;

		if (line.length() > 72) {
			result = Cobol85Format.FIXED;
		} else if (line.length() > 6) {
			result = Cobol85Format.VARIABLE;
		} else if (line.length() > 0) {
			result = Cobol85Format.TANDEM;
		} else {
			result = null;
		}

		return result;
	}

	private String getCopyFileContent(final String filename,
			final File libDirectory) {
		final File copyFile = identifyCopyFile(filename, libDirectory);
		String result;

		if (copyFile == null) {
			LOG.warn("Copy file {} not found.", filename);

			result = null;
		} else {
			try {
				result = process(copyFile, libDirectory);
			} catch (final IOException e) {
				result = null;
				LOG.warn(e.getMessage());
			}
		}

		return result;
	}

	private String getHiddenTokensToLeft(final BufferedTokenStream tokens,
			final int tokPos) {
		final List<Token> refChannel = tokens.getHiddenTokensToLeft(tokPos,
				Cobol85PreprocessorLexer.HIDDEN);
		final StringBuffer sb = new StringBuffer();

		if (refChannel != null) {
			for (final Token refToken : refChannel) {
				final String text = refToken.getText();
				sb.append(text);
			}
		}

		return sb.toString();
	}

	private String getTextIncludingHiddenTokens(final ParseTree ctx,
			final BufferedTokenStream tokens) {
		final Cobol85HiddenTokenCollectorImpl listener = new Cobol85HiddenTokenCollectorImpl(
				tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, ctx);

		return listener.read();
	}

	private File identifyCopyFile(final String filename, final File libDirectory) {
		File copyFile = null;

		for (final String extension : extensions) {
			final String filenameWithExtension;

			if (extension.isEmpty()) {
				filenameWithExtension = filename;
			} else {
				filenameWithExtension = filename + "." + extension;
			}

			final String canonicalPath = libDirectory.getAbsolutePath() + "/"
					+ filenameWithExtension;
			final File copyFileWithExtension = new File(canonicalPath);

			if (copyFileWithExtension.exists()) {
				copyFile = copyFileWithExtension;
				break;
			}
		}

		return copyFile;
	}

	private boolean isEOF(final TerminalNode node) {
		return Token.EOF == node.getSymbol().getType();
	}

	protected String normalizeLine(final String line,
			final Cobol85Format format, final boolean isFirstLine) {
		final String strippedLine = stripLineNumber(line, format);

		/*
		 * determine line prefix
		 */
		final String newLine = isFirstLine ? "" : NEWLINE;
		final String lineNumberPlaceholder = Cobol85Format.TANDEM
				.equals(format) ? "" : LINENUMBER_PLACEHOLDER;
		final String linePrefix = newLine + lineNumberPlaceholder
				+ LINEINDICATOR_PLACEHOLDER;

		final String result;

		/*
		 * treat line by line indicator
		 */
		if (strippedLine.isEmpty()) {
			result = strippedLine;
		} else {
			final String lineArea = strippedLine.substring(1);
			final String trimmedLineArea = lineArea.trim();
			final String cleanLineArea;

			/*
			 * repair trimmed whitespace after comma separator
			 */
			if (trimmedLineArea.isEmpty()) {
				cleanLineArea = trimmedLineArea;
			} else {
				final char lastCharAtTrimmedLineArea = trimmedLineArea
						.charAt(trimmedLineArea.length() - 1);

				if (lastCharAtTrimmedLineArea == ','
						|| lastCharAtTrimmedLineArea == ';') {
					cleanLineArea = trimmedLineArea + " ";
				} else {
					cleanLineArea = trimmedLineArea;
				}
			}

			/*
			 * switch on line indicator
			 */
			final char lineIndicator = strippedLine.charAt(0);

			switch (lineIndicator) {
			// debugging line
			case 'd':
			case 'D':
				result = linePrefix + trimmedLineArea;
				break;
			// continuation line
			case '-':
				final char firstCharOfLineArea = cleanLineArea.charAt(0);

				switch (firstCharOfLineArea) {
				case '\"':
					result = cleanLineArea.substring(1);
					break;
				default:
					result = cleanLineArea;
					break;
				}
				break;
			// comment line
			case '*':
			case '/':
				result = linePrefix + COMMENT_TAG + " " + cleanLineArea;
				break;
			case ' ':
			default:
				result = linePrefix + cleanLineArea;
				break;
			}
		}

		return result;
	}

	protected String normalizeLines(final String input) {
		final Scanner scanner = new Scanner(input);
		final StringBuffer outputBuffer = new StringBuffer();

		Cobol85Format format = null;
		String line = null;
		boolean isFirstLine = true;

		while (scanner.hasNextLine()) {
			line = scanner.nextLine();

			if (format == null) {
				format = determineFormat(line);
			}

			outputBuffer.append(normalizeLine(line, format, isFirstLine));
			isFirstLine = false;
		}

		scanner.close();

		final String result = outputBuffer.toString();

		LOG.debug("Normalized input:\n\n{}\n\n", result);

		return result;
	}

	@Override
	public String process(final File inputFile, final File libDirectory)
			throws IOException {
		LOG.info("Preprocessing file {}.", inputFile.getName());

		final InputStream inputStream = new FileInputStream(inputFile);
		final InputStreamReader inputStreamReader = new InputStreamReader(
				inputStream);
		final BufferedReader bufferedInputStreamReader = new BufferedReader(
				inputStreamReader);
		final StringBuffer outputBuffer = new StringBuffer();

		String line = null;

		while ((line = bufferedInputStreamReader.readLine()) != null) {
			outputBuffer.append(line + NEWLINE);
		}

		bufferedInputStreamReader.close();

		final String result = process(outputBuffer.toString(), libDirectory);
		return result;
	}

	@Override
	public String process(final String input, final File libDirectory) {
		final String normalizedInput = normalizeLines(input);

		final boolean requiresCopyReplaceExecution = requiresCopyReplace(normalizedInput);
		final String result;

		if (requiresCopyReplaceExecution) {
			result = processCopyReplace(normalizedInput, libDirectory);
		} else {
			result = normalizedInput;
		}

		return result;
	}

	protected String processCopyReplace(final String program,
			final File libDirectory) {
		// run the lexer
		final Cobol85PreprocessorLexer lexer = new Cobol85PreprocessorLexer(
				new ANTLRInputStream(program));

		// get a list of matched tokens
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// pass the tokens to the parser
		final Cobol85PreprocessorParser parser = new Cobol85PreprocessorParser(
				tokens);

		// register an error listener, so that preprocessing stops on errors
		parser.removeErrorListeners();
		parser.addErrorListener(new ThrowingErrorListener());

		// specify our entry point
		final StartRuleContext startRule = parser.startRule();

		// analyze contained copy books
		final Cobol85PreprocessingListenerImpl listener = new Cobol85PreprocessingListenerImpl(
				libDirectory, tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, startRule);

		final String result = listener.context().read();

		LOG.debug("Copy-replaced input:\n\n{}\n\n", result);

		return result;
	}

	private boolean requiresCopyReplace(final String input) {
		final String inputLowerCase = input.toLowerCase();
		final boolean result;

		final boolean containsCopy = inputLowerCase.contains("copy");

		if (containsCopy) {
			result = true;
		} else {
			final boolean containsReplace = inputLowerCase.contains("replace");
			result = containsReplace;
		}

		return result;
	}

	protected final String stripLineNumber(final String line,
			final Cobol85Format format) {
		final String result;

		if (format == null) {
			result = line;
		} else {
			switch (format) {
			case FIXED:
				result = StringUtils.substring(line, 6, 72);
				break;
			case VARIABLE:
				result = StringUtils.substring(line, 6);
				break;
			case TANDEM:
				result = line;
				break;
			default:
				result = line;
			}
		}

		return result;
	}

}
