/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
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
import org.codehaus.plexus.util.StringUtils;

import io.proleap.cobol.Cobol85PreprocessorBaseListener;
import io.proleap.cobol.Cobol85PreprocessorLexer;
import io.proleap.cobol.Cobol85PreprocessorParser;
import io.proleap.cobol.Cobol85PreprocessorParser.PseudoTextContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplaceClauseContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplaceableContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplacementContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplacingPhraseContext;
import io.proleap.cobol.Cobol85PreprocessorParser.StartRuleContext;
import io.proleap.cobol.preprocessor.CobolPreprocessor;

public class CobolPreprocessorImpl implements CobolPreprocessor {

	protected abstract class AbstractCobolLinesSubPreprocessor implements CobolSubPreprocessor {

		public abstract String processLine(final String line, int lineNumber, final CobolDialect dialect,
				final CobolSourceFormat format);

		@Override
		public String processLines(final String line, final CobolDialect dialect, final CobolSourceFormat format) {
			final Scanner scanner = new Scanner(line);
			final StringBuffer outputBuffer = new StringBuffer();

			String currentLine = null;
			int lineNumber = 0;

			while (scanner.hasNextLine()) {
				currentLine = scanner.nextLine();
				final String processedLine = processLine(currentLine, lineNumber, dialect, format);
				outputBuffer.append(processedLine);
				lineNumber++;
			}

			scanner.close();

			final String result = outputBuffer.toString();
			return result;
		}
	}

	protected class CobolCleanLinesSubPreprocessorImpl extends AbstractCobolLinesSubPreprocessor {

		@Override
		public String processLine(final String line, final int lineNumber, final CobolDialect dialect,
				final CobolSourceFormat format) {
			// clean line from certain ASCII chars
			final int substituteChar = 0x1A;
			final String cleanedLine = line.replace((char) substituteChar, ' ');
			final String result;

			// if line is empty
			if (cleanedLine.trim().isEmpty()) {
				result = "";
			} else {
				result = cleanedLine + NEWLINE;
			}

			return result;
		}
	}

	/**
	 * ANTLR listener, which collects visible as well as hidden tokens for a
	 * given parse tree in a string buffer.
	 */
	protected class CobolHiddenTokenCollectorListenerImpl extends Cobol85PreprocessorBaseListener {

		boolean firstTerminal = true;

		private final StringBuffer outputBuffer = new StringBuffer();

		private final BufferedTokenStream tokens;

		public CobolHiddenTokenCollectorListenerImpl(final BufferedTokenStream tokens) {
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

	protected class CobolMarkCommentEntriesSubPreprocessorImpl extends AbstractCobolLinesSubPreprocessor {

		protected final Pattern commentEntryTriggerLinePattern;

		protected boolean foundCommentEntryTriggerInPreviousLine = false;

		protected boolean inCommentEntry = false;

		protected final String[] triggersEnd = new String[] { "PROGRAM-ID.", "AUTHOR.", "INSTALLATION.",
				"DATE-WRITTEN.", "DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE." };

		protected final String[] triggersStart = new String[] { "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
				"DATE-COMPILED.", "SECURITY.", "REMARKS." };

		public CobolMarkCommentEntriesSubPreprocessorImpl() {
			final String commentEntryTriggerLineFormat = new String("(" + String.join("|", triggersStart) + ")(.+)");
			commentEntryTriggerLinePattern = Pattern.compile(commentEntryTriggerLineFormat, Pattern.CASE_INSENSITIVE);
		}

		protected boolean beginsWithTrigger(final CobolLine parsedLine, final String[] triggers) {
			final String contentAreaUpperCase = new String(parsedLine.contentAreaA + parsedLine.contentAreaB)
					.toUpperCase();

			boolean result = false;

			for (final String trigger : triggers) {
				final boolean containsTrigger = contentAreaUpperCase.startsWith(trigger);

				if (containsTrigger) {
					result = true;
					break;
				}
			}

			return result;
		}

		@Override
		public String processLine(final String line, final int lineNumber, final CobolDialect dialect,
				final CobolSourceFormat format) {
			final String result;

			if (format.isCommentEntryMultiLine()) {
				result = processSourceFormat(line, lineNumber, dialect, format);
			} else {
				result = line + NEWLINE;
			}

			return result;
		}

		/**
		 * If the Compiler directive SOURCEFORMAT is specified as or defaulted
		 * to FIXED, the comment-entry can be contained on one or more lines but
		 * is restricted to area B of those lines; the next line commencing in
		 * area A begins the next non-comment entry.
		 */
		protected String processSourceFormat(final String line, final int lineNumber, final CobolDialect dialect,
				final CobolSourceFormat format) {
			final CobolLine parsedLine = parseCobolLine(line, format);

			if (parsedLine == null) {
				throwCobolLineParseException(line, lineNumber, format);
			}

			final boolean foundCommentEntryTriggerInCurrentLine = beginsWithTrigger(parsedLine, triggersStart);

			final String result;

			if (foundCommentEntryTriggerInCurrentLine) {
				result = removeCommentEntry(line, parsedLine);
			} else if (foundCommentEntryTriggerInPreviousLine || inCommentEntry) {
				final boolean isContentAreaAEmpty = parsedLine.contentAreaA.trim().isEmpty();

				/**
				 * OSVS: The comment-entry can be contained in either area A or
				 * area B of the comment-entry lines. However, the next
				 * occurrence in area A of any one of the following COBOL words
				 * or phrases terminates the comment-entry and begin the next
				 * paragraph or division.
				 */
				final boolean inOsvsCommentEntry = CobolDialect.OSVS.equals(dialect)
						&& !beginsWithTrigger(parsedLine, triggersEnd);

				inCommentEntry = parsedLine.indicatorArea == charAsterisk || parsedLine.indicatorArea == charSlash
						|| isContentAreaAEmpty || inOsvsCommentEntry;

				if (inCommentEntry) {
					result = parsedLine.sequenceArea + charAsterisk + parsedLine.contentAreaA + parsedLine.contentAreaB
							+ parsedLine.comment + NEWLINE;
				} else {
					result = line + NEWLINE;
				}
			} else {
				result = line + NEWLINE;
			}

			foundCommentEntryTriggerInPreviousLine = foundCommentEntryTriggerInCurrentLine;

			return result;
		}

		protected String removeCommentEntry(final String line, final CobolLine parsedLine) {
			final String result;

			final Matcher matcher = commentEntryTriggerLinePattern
					.matcher(parsedLine.contentAreaA + parsedLine.contentAreaB);

			if (matcher.matches()) {
				final String trigger = matcher.group(1);
				final String commentEntry = matcher.group(2);
				final String newContentArea = trigger + StringUtils.repeat(" ", commentEntry.length());

				result = parsedLine.sequenceArea + parsedLine.indicatorArea + newContentArea + parsedLine.comment
						+ NEWLINE;
			} else {
				result = line + NEWLINE;
			}

			return result;
		}
	}

	protected class CobolNormalizeLinesSubPreprocessorImpl extends AbstractCobolLinesSubPreprocessor {

		protected final static String COMMENT_TAG = ">*";

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
		 * Normalizes the sequence and indicator area to NEWLINE and whitespace.
		 */
		protected String normalizeLineBreakAndSequenceArea(final CobolLine line, final boolean isFirstLine) {
			// newline
			final String newLine = isFirstLine ? "" : NEWLINE;

			// sequence area
			final String sequenceAreaPlaceholder = StringUtils.leftPad("", line.sequenceArea.length());

			final String result = newLine + sequenceAreaPlaceholder;
			return result;
		}

		/**
		 * Normalizes a line by stripping the sequence number and line
		 * indicator, and interpreting the line indicator.
		 */
		protected String processLine(final CobolLine line, final boolean isFirstLine) {
			final String result;

			// determine line prefix
			final String linePrefix = normalizeLineBreakAndSequenceArea(line, isFirstLine);

			// join content areas A and B
			final String joinedContentArea = line.contentAreaA + line.contentAreaB;

			// trim trailing whitespace
			final String trimmedTrailWsContentArea = joinedContentArea.replaceAll("\\s+$", "");

			// handle trailing comma
			final String handledContentArea = handleTrailingComma(trimmedTrailWsContentArea);

			/*
			 * switch on line indicator
			 */
			switch (line.indicatorArea) {
			// debugging line
			case chard:
			case charD:
				result = linePrefix + ' ' + handledContentArea;
				break;
			// continuation line
			case charMinus:
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
			case charAsterisk:
			case charSlash:
				result = linePrefix + COMMENT_TAG + " " + handledContentArea;
				break;
			case ' ':
			default:
				result = linePrefix + ' ' + handledContentArea;
				break;
			}

			return result;
		}

		@Override
		public String processLine(final String line, final int lineNumber, final CobolDialect dialect,
				final CobolSourceFormat format) {
			final CobolLine parsedLine = parseCobolLine(line, format);

			if (parsedLine == null) {
				throwCobolLineParseException(line, lineNumber, format);
			}

			final boolean isFirstLine = lineNumber == 0;
			final String result = processLine(parsedLine, isFirstLine);
			return result;
		}
	}

	protected class CobolParseLinesSubPreprocessorImpl implements CobolSubPreprocessor {

		protected final File libDirectory;

		protected final String[] triggers = new String[] { "copy", "exec sql", "exec cics", "replace" };

		public CobolParseLinesSubPreprocessorImpl(final File libDirectory) {
			this.libDirectory = libDirectory;
		}

		protected boolean containsTrigger(final String line, final String[] triggers) {
			final String lineLowerCase = line.toLowerCase();
			boolean result = false;

			for (final String trigger : triggers) {
				final boolean containsTrigger = lineLowerCase.contains(trigger);

				if (containsTrigger) {
					result = true;
					break;
				}
			}

			return result;
		}

		@Override
		public String processLines(final String line, final CobolDialect dialect, final CobolSourceFormat formats) {
			final boolean requiresProcessorExecution = containsTrigger(line, triggers);
			final String result;

			if (requiresProcessorExecution) {
				result = processWithParser(line, libDirectory, dialect, formats);
			} else {
				result = line;
			}

			return result;
		}

		protected String processWithParser(final String program, final File libDirectory, final CobolDialect dialect,
				final CobolSourceFormat formats) {
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
			final CobolParserPreprocessorListenerImpl listener = new CobolParserPreprocessorListenerImpl(libDirectory,
					dialect, formats, tokens);
			final ParseTreeWalker walker = new ParseTreeWalker();

			walker.walk(listener, startRule);

			final String result = listener.context().read();
			return result;
		}
	}

	/**
	 * ANTLR visitor, which preprocesses a given COBOL program by executing COPY
	 * and REPLACE statemenets.
	 */
	protected class CobolParserPreprocessorListenerImpl extends Cobol85PreprocessorBaseListener {

		/**
		 * A replacement context that defines, which replaceables should be
		 * replaced by which replacements.
		 */
		private class PreprocessingContext {

			/**
			 * A mapping from a replaceable to a replacement.
			 */
			private class ReplacementMapping implements Comparable<ReplacementMapping> {

				private ReplaceableContext replaceable;

				private ReplacementContext replacement;

				@Override
				public int compareTo(final ReplacementMapping o) {
					return o.replaceable.getText().length() - replaceable.getText().length();
				}

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
					Arrays.sort(currentReplaceableReplacements);

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

		private final CobolDialect dialect;

		private final CobolSourceFormat formats;

		private final File libDirectory;

		private final BufferedTokenStream tokens;

		public CobolParserPreprocessorListenerImpl(final File libDirectory, final CobolDialect dialect,
				final CobolSourceFormat formats, final BufferedTokenStream tokens) {
			this.libDirectory = libDirectory;
			this.dialect = dialect;
			this.tokens = tokens;
			this.formats = formats;

			contexts.push(new PreprocessingContext());
		}

		private PreprocessingContext context() {
			return contexts.peek();
		}

		@Override
		public void enterControlSpacingStatement(
				@NotNull final Cobol85PreprocessorParser.ControlSpacingStatementContext ctx) {
			push();
		}

		@Override
		public void enterCopyStatement(@NotNull final Cobol85PreprocessorParser.CopyStatementContext ctx) {
			// push a new context for COPY terminals
			push();
		}

		@Override
		public void enterExecCicsStatement(final Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
			// push a new context for SQL terminals
			push();
		}

		@Override
		public void enterExecSqlStatement(final Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
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
		public void exitControlSpacingStatement(
				@NotNull final Cobol85PreprocessorParser.ControlSpacingStatementContext ctx) {
			// throw away control spacing statement
			pop();
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
			final String fileContent = getCopyFileContent(copyFileIdentifier, libDirectory, dialect, formats);

			if (fileContent != null) {
				context().write(fileContent + NEWLINE);
				context().replace();
			}

			final String content = context().read();
			pop();

			context().write(content);
		};

		@Override
		public void exitExecCicsStatement(final Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
			// throw away EXEC SQL terminals -> TODO
			pop();
		}

		@Override
		public void exitExecSqlStatement(final Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
			// throw away EXEC SQL terminals -> TODO
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

	protected interface CobolSubPreprocessor {

		public String processLines(String line, final CobolDialect dialect, final CobolSourceFormat formats);
	}

	protected class ThrowingErrorListener extends BaseErrorListener {

		@Override
		public void syntaxError(@NotNull final Recognizer<?, ?> recognizer, @Nullable final Object offendingSymbol,
				final int line, final int charPositionInLine, @NotNull final String msg,
				@Nullable final RecognitionException e) {
			throw new RuntimeException("syntax error in line " + line + ":" + charPositionInLine + " " + msg);
		}
	}

	private final static Logger LOG = LogManager.getLogger(CobolPreprocessorImpl.class);

	protected final static String NEWLINE = "\n";

	protected final char charAsterisk = '*';

	protected final char chard = 'd';

	protected final char charD = 'D';

	protected final char charMinus = '-';

	protected final char charSlash = '/';

	protected final String[] copyFileExtensions = new String[] { "", "CPY", "cpy", "COB", "cob", "CBL", "cbl" };

	protected String getCopyFileContent(final String filename, final File libDirectory, final CobolDialect dialect,
			final CobolSourceFormat format) {
		final File copyFile = identifyCopyFile(filename, libDirectory);
		String result;

		if (copyFile == null) {
			LOG.warn("Copy file {} not found.", filename);

			result = null;
		} else {
			try {
				result = process(copyFile, libDirectory, format, dialect);
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
		final CobolHiddenTokenCollectorListenerImpl listener = new CobolHiddenTokenCollectorListenerImpl(tokens);
		final ParseTreeWalker walker = new ParseTreeWalker();

		walker.walk(listener, ctx);

		return listener.read();
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

	protected CobolLine parseCobolLine(final String line, final CobolSourceFormat format) {
		CobolLine result = null;

		final Pattern pattern = format.getPattern();
		final Matcher matcher = pattern.matcher(line);

		if (matcher.matches()) {
			final String sequenceAreaGroup = matcher.group(1);
			final String indicatorAreaGroup = matcher.group(2);
			final String contentAreaAGroup = matcher.group(3);
			final String contentAreaBGroup = matcher.group(4);
			final String commentAreaGroup = matcher.group(5);

			final String sequenceArea = sequenceAreaGroup != null ? sequenceAreaGroup : "";
			final char indicatorArea = indicatorAreaGroup != null ? indicatorAreaGroup.charAt(0) : ' ';
			final String contentAreaA = contentAreaAGroup != null ? contentAreaAGroup : "";
			final String contentAreaB = contentAreaBGroup != null ? contentAreaBGroup : "";
			final String commentArea = commentAreaGroup != null ? commentAreaGroup : "";

			result = new CobolLine(sequenceArea, indicatorArea, contentAreaA, contentAreaB, commentArea, format);
		}

		return result;
	}

	@Override
	public String process(final File cobolFile, final File libDirectory, final CobolSourceFormat format)
			throws IOException {
		return process(cobolFile, libDirectory, format, null);
	}

	@Override
	public String process(final File cobolFile, final File libDirectory, final CobolSourceFormat format,
			final CobolDialect dialect) throws IOException {
		LOG.info("Preprocessing file {}.", cobolFile.getName());

		final InputStream inputStream = new FileInputStream(cobolFile);
		final InputStreamReader inputStreamReader = new InputStreamReader(inputStream);
		final BufferedReader bufferedInputStreamReader = new BufferedReader(inputStreamReader);
		final StringBuffer outputBuffer = new StringBuffer();

		String line = null;

		while ((line = bufferedInputStreamReader.readLine()) != null) {
			outputBuffer.append(line + NEWLINE);
		}

		bufferedInputStreamReader.close();

		final String result = process(outputBuffer.toString(), libDirectory, format, dialect);
		return result;
	}

	@Override
	public String process(final String cobolSourceCode, final File libDirectory, final CobolSourceFormat format) {
		return process(cobolSourceCode, libDirectory, format, null);
	}

	@Override
	public String process(final String cobolSourceCode, final File libDirectory, final CobolSourceFormat format,
			final CobolDialect dialect) {
		final CobolSubPreprocessor cleanLinesPreprocessor = new CobolCleanLinesSubPreprocessorImpl();
		final CobolSubPreprocessor markCommentEntriesPreprocessor = new CobolMarkCommentEntriesSubPreprocessorImpl();
		final CobolSubPreprocessor normalizeLinesPreprocessor = new CobolNormalizeLinesSubPreprocessorImpl();
		final CobolSubPreprocessor parseLinesPreprocessor = new CobolParseLinesSubPreprocessorImpl(libDirectory);

		final String cleanedCode = cleanLinesPreprocessor.processLines(cobolSourceCode, dialect, format);
		final String markedCode = markCommentEntriesPreprocessor.processLines(cleanedCode, dialect, format);
		final String normalizedCode = normalizeLinesPreprocessor.processLines(markedCode, dialect, format);
		final String result = parseLinesPreprocessor.processLines(normalizedCode, dialect, format);

		LOG.debug("Processed input:\n\n{}\n\n", result);

		return result;
	}

	protected void throwCobolLineParseException(final String line, final int lineNumber,
			final CobolSourceFormat format) {
		throw new RuntimeException("could not parse line " + (lineNumber + 1) + " with format " + format + ": " + line);
	}

}
