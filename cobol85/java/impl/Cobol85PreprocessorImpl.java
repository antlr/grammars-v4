/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package org.cobol85.preprocessor.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
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
	protected class Cobol85HiddenTokenCollectorImpl extends Cobol85PreprocessorBaseListener {

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
	protected class Cobol85PreprocessingListenerImpl extends Cobol85PreprocessorBaseListener {

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

				private String extractPseudoText(final PseudoTextContext pseudoTextCtx) {
					final String pseudoText = getTextIncludingHiddenTokens(pseudoTextCtx, tokens).trim();
					final String content = pseudoText.replaceAll("^==", "").replaceAll("==$", "").trim();
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

			private ReplacementMapping[] currentReplaceableReplacements;

			private StringBuffer outputBuffer = new StringBuffer();

			private String read() {
				return outputBuffer.toString();
			}

			public void replace() {
				if (currentReplaceableReplacements != null) {
					for (final ReplacementMapping replaceableReplacement : currentReplaceableReplacements) {
						final String currentOutput = outputBuffer.toString();
						final String replacedOutput = replaceableReplacement.replace(currentOutput);

						outputBuffer = new StringBuffer();
						outputBuffer.append(replacedOutput);
					}
				}
			}

			private void storeReplaceablesAndReplacements(final List<ReplaceClauseContext> replaceClauses) {
				if (replaceClauses == null) {
					currentReplaceableReplacements = null;
				} else {
					final int length = replaceClauses.size();
					currentReplaceableReplacements = new ReplacementMapping[length];

					int i = 0;

					for (final ReplaceClauseContext replaceClause : replaceClauses) {
						final ReplacementMapping replaceableReplacement = new ReplacementMapping();

						replaceableReplacement.replaceable = replaceClause.replaceable();
						replaceableReplacement.replacement = replaceClause.replacement();

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

		private final Cobol85Format[] formats;

		private final File libDirectory;

		private final BufferedTokenStream tokens;

		public Cobol85PreprocessingListenerImpl(final File libDirectory, final Cobol85Format[] formats,
				final BufferedTokenStream tokens) {
			this.libDirectory = libDirectory;
			this.formats = formats;
			this.tokens = tokens;

			contexts.push(new PreprocessingContext());
		}

		private PreprocessingContext context() {
			return contexts.peek();
		}

		@Override
		public void enterCopyStatement(@NotNull final Cobol85PreprocessorParser.CopyStatementContext ctx) {
			// push a new context for COPY terminals
			push();
		};

		@Override
		public void enterExecCicsStatement(final org.cobol85.Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
			// push a new context for SQL terminals
			push();
		}

		@Override
		public void enterExecSqlStatement(final org.cobol85.Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
			// push a new context for SQL terminals
			push();
		}

		@Override
		public void enterReplaceArea(@NotNull final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
			push();
		}

		@Override
		public void enterReplaceByStatement(@NotNull final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
			push();
		}

		@Override
		public void enterReplaceOffStatement(@NotNull final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
			push();
		}

		@Override
		public void exitCopyStatement(@NotNull final Cobol85PreprocessorParser.CopyStatementContext ctx) {
			// throw away COPY terminals
			pop();

			// a new context for the copy file content
			push();

			/*
			 * replacement phrase
			 */
			final ReplacingPhraseContext replacingPhrase = ctx.replacingPhrase();

			if (replacingPhrase != null) {
				context().storeReplaceablesAndReplacements(replacingPhrase.replaceClause());
			}

			/*
			 * copy the copy file
			 */
			final String copyFileIdentifier = ctx.copySource().getText();
			final String fileContent = getCopyFileContent(copyFileIdentifier, libDirectory, formats);

			if (fileContent != null) {
				context().write(fileContent + NEWLINE);
				context().replace();
			}

			final String content = context().read();
			pop();

			context().write(content);
		}

		@Override
		public void exitExecCicsStatement(final org.cobol85.Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
			// throw away EXEC SQL terminals -> FIXME
			pop();
		}

		@Override
		public void exitExecSqlStatement(final org.cobol85.Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
			// throw away EXEC SQL terminals -> FIXME
			pop();
		}

		@Override
		public void exitReplaceArea(@NotNull final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
			/*
			 * replacement phrase
			 */
			final List<ReplaceClauseContext> replaceClauses = ctx.replaceByStatement().replaceClause();
			context().storeReplaceablesAndReplacements(replaceClauses);

			context().replace();
			final String content = context().read();

			pop();
			context().write(content);
		}

		@Override
		public void exitReplaceByStatement(@NotNull final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
			// throw away REPLACE BY terminals
			pop();
		};

		@Override
		public void exitReplaceOffStatement(@NotNull final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
			// throw away REPLACE OFF terminals
			pop();
		}

		/**
		 * Pops the current preprocessing context from the stack.
		 */
		private PreprocessingContext pop() {
			return contexts.pop();
		}

		/**
		 * Pushes a new preprocessing context onto the stack.
		 */
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

	protected class ThrowingErrorListener extends BaseErrorListener {

		@Override
		public void syntaxError(@NotNull final Recognizer<?, ?> recognizer, @Nullable final Object offendingSymbol,
				final int line, final int charPositionInLine, @NotNull final String msg,
				@Nullable final RecognitionException e) {
			throw new RuntimeException("syntax error in line " + line + ":" + charPositionInLine + " " + msg);
		}
	}

	protected final static String COMMENT_TAG = ">*";

	private final static Logger LOG = LogManager.getLogger(Cobol85PreprocessorImpl.class);

	protected final static String NEWLINE = "\n";

	protected final String[] copyFileExtensions = new String[] { "", "CPY", "cpy", "COB", "cob", "CBL", "cbl" };

	protected final Cobol85Format[] defaultFormats = new Cobol85Format[] { Cobol85FormatEnum.DEFECT,
			Cobol85FormatEnum.FIXED, Cobol85FormatEnum.VARIABLE, Cobol85FormatEnum.TANDEM };

	protected final String[] parsingTriggers = new String[] { "copy", "exec sql", "exec cics", "replace" };

	protected Map<Cobol85Format, Pattern> patterns = new HashMap<Cobol85Format, Pattern>();

	protected void assurePatternForFormat(final Cobol85Format format) {
		if (patterns.get(format) == null) {
			patterns.put(format, Pattern.compile(format.getRegex()));
		}
	}

	protected Cobol85Format[] determineFormats(final Cobol85Format[] formats) {
		return formats != null ? formats : defaultFormats;
	}

	protected String getCopyFileContent(final String filename, final File libDirectory, final Cobol85Format[] formats) {
		final File copyFile = identifyCopyFile(filename, libDirectory);
		String result;

		if (copyFile == null) {
			LOG.warn("Copy file {} not found.", filename);

			result = null;
		} else {
			try {
				result = process(copyFile, libDirectory, formats);
			} catch (final IOException e) {
				result = null;
				LOG.warn(e.getMessage());
			}
		}

		return result;
	}

	protected String getHiddenTokensToLeft(final BufferedTokenStream tokens, final int tokPos) {
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

	protected String getTextIncludingHiddenTokens(final ParseTree ctx, final BufferedTokenStream tokens) {
		final Cobol85HiddenTokenCollectorImpl listener = new Cobol85HiddenTokenCollectorImpl(tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, ctx);

		return listener.read();
	}

	protected String handleTrailingComma(final String contentArea) {
		final String result;

		/*
		 * repair trimmed whitespace after comma separator
		 */
		if (contentArea.isEmpty()) {
			result = contentArea;
		} else {
			final char lastCharAtTrimmedLineArea = contentArea.charAt(contentArea.length() - 1);

			if (lastCharAtTrimmedLineArea == ',' || lastCharAtTrimmedLineArea == ';') {
				result = contentArea + " ";
			} else {
				result = contentArea;
			}
		}

		return result;
	}

	/**
	 * Identifies a copy file by its name and directory.
	 */
	protected File identifyCopyFile(final String filename, final File libDirectory) {
		File copyFile = null;

		for (final String extension : copyFileExtensions) {
			final String filenameWithExtension;

			if (extension.isEmpty()) {
				filenameWithExtension = filename;
			} else {
				filenameWithExtension = filename + "." + extension;
			}

			final String canonicalPath = libDirectory.getAbsolutePath() + "/" + filenameWithExtension;
			final File copyFileWithExtension = new File(canonicalPath);

			if (copyFileWithExtension.exists()) {
				copyFile = copyFileWithExtension;
				break;
			}
		}

		return copyFile;
	}

	protected boolean isEOF(final TerminalNode node) {
		return Token.EOF == node.getSymbol().getType();
	}

	/**
	 * Normalizes a line by stripping the sequence number and line indicator,
	 * and interpreting the line indicator.
	 */
	@Override
	public String normalizeLine(final Cobol85Line line, final boolean isFirstLine) {
		final String result;

		// determine line prefix
		final String linePrefix = normalizeLineBreakAndSequenceArea(line, isFirstLine);

		// trim trailing whitespace
		final String trimmedTrailWsContentArea = line.contentArea.replaceAll("\\s+$", "");

		// handle trailing comma
		final String handledContentArea = handleTrailingComma(trimmedTrailWsContentArea);

		/*
		 * switch on line indicator
		 */
		switch (line.indicatorArea) {
		// debugging line
		case 'd':
		case 'D':
			result = linePrefix + ' ' + handledContentArea;
			break;
		// continuation line
		case '-':
			final String trimmedContentArea = handledContentArea.trim();
			final char firstCharOfContentArea = trimmedContentArea.charAt(0);

			switch (firstCharOfContentArea) {
			case '\"':
			case '\'':
				result = trimmedContentArea.substring(1);
				break;
			default:
				result = trimmedContentArea;
				break;
			}
			break;
		// comment line
		case '*':
		case '/':
			result = linePrefix + COMMENT_TAG + " " + handledContentArea;
			break;
		case ' ':
		default:
			result = linePrefix + ' ' + handledContentArea;
			break;
		}

		return result;
	}

	/**
	 * Normalizes the sequence and indicator area to NEWLINE and whitespace.
	 */
	protected String normalizeLineBreakAndSequenceArea(final Cobol85Line line, final boolean isFirstLine) {
		// newline
		final String newLine = isFirstLine ? "" : NEWLINE;

		// sequence area
		final String sequenceAreaPlaceholder = StringUtils.leftPad("", line.sequenceArea.length());

		final String result = newLine + sequenceAreaPlaceholder;
		return result;
	}

	protected String normalizeLines(final String input, final Cobol85Format[] formats) {
		final Scanner scanner = new Scanner(input);
		final StringBuffer outputBuffer = new StringBuffer();

		String line = null;
		int lineNumber = 0;

		while (scanner.hasNextLine()) {
			line = scanner.nextLine();

			// clean line from certain ASCII chars
			final int substituteChar = 0x1A;
			final String cleanedLine = line.replace((char) substituteChar, ' ');
			final String normalizedLine;

			// if line is empty
			if (cleanedLine.trim().isEmpty()) {
				normalizedLine = cleanedLine;
			} else {
				// parse line
				final Cobol85Line parsedLine = parseCobol85Line(cleanedLine, formats);

				// if line could not be parsed
				if (parsedLine == null) {
					LOG.warn("unknown line format in line {}: {}", lineNumber + 1, line);
					normalizedLine = cleanedLine;
				} else {
					final boolean isFirstLine = lineNumber == 0;
					normalizedLine = normalizeLine(parsedLine, isFirstLine);
				}
			}

			outputBuffer.append(normalizedLine);
			lineNumber++;
		}

		scanner.close();

		final String result = outputBuffer.toString();
		return result;
	}

	@Override
	public Cobol85Line parseCobol85Line(final String line, final Cobol85Format[] formats) {
		Cobol85Line result = null;

		final Cobol85Format[] effectiveFormats = determineFormats(formats);

		for (final Cobol85Format format : effectiveFormats) {
			assurePatternForFormat(format);

			final Pattern pattern = patterns.get(format);
			final Matcher matcher = pattern.matcher(line);

			if (matcher.matches()) {
				final String sequenceAreaGroup = matcher.group(1);
				final String indicatorAreaGroup = matcher.group(2);
				final String contentAreaGroup = matcher.group(3);
				final String commentAreaGroup = matcher.group(4);

				final String sequenceArea = sequenceAreaGroup != null ? sequenceAreaGroup : "";
				final char indicatorArea = indicatorAreaGroup != null ? indicatorAreaGroup.charAt(0) : ' ';
				final String contentArea = contentAreaGroup != null ? contentAreaGroup : "";
				final String commentArea = commentAreaGroup != null ? commentAreaGroup : "";

				result = new Cobol85Line(sequenceArea, indicatorArea, contentArea, commentArea, format);
				break;
			}
		}

		return result;
	}

	@Override
	public String process(final File inputFile, final File libDirectory, final Cobol85Format[] formats)
			throws IOException {
		LOG.info("Preprocessing file {}.", inputFile.getName());

		final InputStream inputStream = new FileInputStream(inputFile);
		final InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
		final BufferedReader bufferedInputStreamReader = new BufferedReader(inputStreamReader);
		final StringBuffer outputBuffer = new StringBuffer();

		String line = null;

		while ((line = bufferedInputStreamReader.readLine()) != null) {
			outputBuffer.append(line + NEWLINE);
		}

		bufferedInputStreamReader.close();

		final String result = process(outputBuffer.toString(), libDirectory, formats);
		return result;
	}

	@Override
	public String process(final String input, final File libDirectory, final Cobol85Format[] formats) {
		final String normalizedInput = normalizeLines(input, formats);

		final boolean requiresProcessorExecution = requiresParsing(normalizedInput);
		final String result;

		if (requiresProcessorExecution) {
			result = processWithParser(normalizedInput, libDirectory, formats);
		} else {
			result = normalizedInput;
		}

		LOG.debug("Processed input:\n\n{}\n\n", result);

		return result;
	}

	protected String processWithParser(final String program, final File libDirectory, final Cobol85Format[] formats) {
		// run the lexer
		final Cobol85PreprocessorLexer lexer = new Cobol85PreprocessorLexer(new ANTLRInputStream(program));

		// get a list of matched tokens
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// pass the tokens to the parser
		final Cobol85PreprocessorParser parser = new Cobol85PreprocessorParser(tokens);

		// register an error listener, so that preprocessing stops on errors
		parser.removeErrorListeners();
		parser.addErrorListener(new ThrowingErrorListener());

		// specify our entry point
		final StartRuleContext startRule = parser.startRule();

		// analyze contained copy books
		final Cobol85Format[] effectiveFormats = determineFormats(formats);
		final Cobol85PreprocessingListenerImpl listener = new Cobol85PreprocessingListenerImpl(libDirectory,
				effectiveFormats, tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, startRule);

		final String result = listener.context().read();
		return result;
	}

	protected boolean requiresParsing(final String input) {
		final String inputLowerCase = input.toLowerCase();
		boolean result = false;

		for (final String trigger : parsingTriggers) {
			final boolean containsTrigger = inputLowerCase.contains(trigger);

			if (containsTrigger) {
				result = true;
				break;
			}
		}

		return result;
	}

}
