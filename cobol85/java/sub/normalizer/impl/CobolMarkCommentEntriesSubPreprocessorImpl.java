/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.normalizer.impl;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codehaus.plexus.util.StringUtils;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.impl.AbstractCobolSubPreprocessor;
import io.proleap.cobol.preprocessor.sub.impl.CobolLine;
import io.proleap.cobol.preprocessor.sub.normalizer.CobolMarkCommentEntriesSubPreprocessor;

public class CobolMarkCommentEntriesSubPreprocessorImpl extends AbstractCobolSubPreprocessor
		implements CobolMarkCommentEntriesSubPreprocessor {

	protected final Pattern commentEntryTriggerLinePattern;

	protected boolean foundCommentEntryTriggerInPreviousLine = false;

	protected boolean isInCommentEntry = false;

	protected final String[] triggersEnd = new String[] { "PROGRAM-ID.", "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
			"DATE-COMPILED.", "SECURITY.", "ENVIRONMENT", "DATA.", "PROCEDURE." };

	protected final String[] triggersStart = new String[] { "AUTHOR.", "INSTALLATION.", "DATE-WRITTEN.",
			"DATE-COMPILED.", "SECURITY.", "REMARKS." };

	public CobolMarkCommentEntriesSubPreprocessorImpl() {
		final String commentEntryTriggerLineFormat = new String("(" + String.join("|", triggersStart) + ")(.+)");
		commentEntryTriggerLinePattern = Pattern.compile(commentEntryTriggerLineFormat, Pattern.CASE_INSENSITIVE);
	}

	/**
	 * Blanks in a given line a potential comment entry.
	 */
	protected String blankCommentEntry(final String line, final CobolLine parsedLine) {
		final String result;

		final Matcher matcher = commentEntryTriggerLinePattern
				.matcher(parsedLine.contentAreaA + parsedLine.contentAreaB);

		if (matcher.matches()) {
			final String trigger = matcher.group(1);
			final String commentEntry = matcher.group(2);
			final String newContentArea = trigger + StringUtils.repeat(CobolPreprocessor.WS, commentEntry.length());

			result = parsedLine.sequenceArea + parsedLine.indicatorArea + newContentArea + parsedLine.comment
					+ CobolPreprocessor.NEWLINE;
		} else {
			result = buildRegularLine(line);
		}

		return result;
	}

	protected String buildMultiLineCommentEntryLine(final CobolLine parsedLine) {
		return parsedLine.sequenceArea + CobolPreprocessor.CHAR_SHARP + parsedLine.contentAreaA
				+ parsedLine.contentAreaB + parsedLine.comment + CobolPreprocessor.NEWLINE;
	}

	protected String buildRegularLine(final String line) {
		return line + CobolPreprocessor.NEWLINE;
	}

	/**
	 * Escapes in a given line a potential comment entry.
	 */
	protected String escapeCommentEntry(final String line, final CobolLine parsedLine) {
		final String result;

		final Matcher matcher = commentEntryTriggerLinePattern
				.matcher(parsedLine.contentAreaA + parsedLine.contentAreaB);

		if (matcher.matches()) {
			final String trigger = matcher.group(1);
			final String commentEntry = matcher.group(2);
			final String newContentArea = trigger + CobolPreprocessor.WS + CobolPreprocessor.COMMENT_ENTRY_TAG
					+ commentEntry;

			result = parsedLine.sequenceArea + parsedLine.indicatorArea + newContentArea + parsedLine.comment
					+ CobolPreprocessor.NEWLINE;
		} else {
			result = buildRegularLine(line);
		}

		return result;
	}

	protected boolean isInCommentEntry(final CobolLine parsedLine, final boolean isContentAreaAEmpty,
			final boolean isInOsvsCommentEntry) {
		final boolean result = parsedLine.indicatorArea == CobolPreprocessor.CHAR_ASTERISK
				|| parsedLine.indicatorArea == CobolPreprocessor.CHAR_SLASH || isContentAreaAEmpty
				|| isInOsvsCommentEntry;
		return result;
	}

	/**
	 * OSVS: The comment-entry can be contained in either area A or area B of
	 * the comment-entry lines. However, the next occurrence in area A of any
	 * one of the following COBOL words or phrases terminates the comment-entry
	 * and begin the next paragraph or division.
	 */
	protected boolean isInOsvsCommentEntry(final CobolDialect dialect, final CobolLine parsedLine) {
		final boolean result = CobolDialect.OSVS.equals(dialect) && !startsWithTrigger(parsedLine, triggersEnd);
		return result;
	}

	@Override
	public String processLine(final String line, final int lineNumber, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final String result;

		if (format.isCommentEntryMultiLine()) {
			result = processMultiLineCommentEntry(line, lineNumber, format, dialect);
		} else {
			result = processSingleLineCommentEntry(line, lineNumber, format, dialect);
		}

		return result;
	}

	/**
	 * If the Compiler directive SOURCEFORMAT is specified as or defaulted to
	 * FIXED, the comment-entry can be contained on one or more lines but is
	 * restricted to area B of those lines; the next line commencing in area A
	 * begins the next non-comment entry.
	 */
	protected String processMultiLineCommentEntry(final String line, final int lineNumber,
			final CobolSourceFormatEnum format, final CobolDialect dialect) {
		final CobolLine parsedLine = parseCobolLine(line, format);

		if (parsedLine == null) {
			throwCobolLineParseException(line, lineNumber, format);
		}

		final boolean foundCommentEntryTriggerInCurrentLine = startsWithTrigger(parsedLine, triggersStart);
		final String result;

		if (foundCommentEntryTriggerInCurrentLine) {
			result = blankCommentEntry(line, parsedLine);
		} else if (foundCommentEntryTriggerInPreviousLine || isInCommentEntry) {
			final boolean isContentAreaAEmpty = parsedLine.contentAreaA.trim().isEmpty();
			final boolean isInOsvsCommentEntry = isInOsvsCommentEntry(dialect, parsedLine);

			isInCommentEntry = isInCommentEntry(parsedLine, isContentAreaAEmpty, isInOsvsCommentEntry);

			if (isInCommentEntry) {
				result = buildMultiLineCommentEntryLine(parsedLine);
			} else {
				result = buildRegularLine(line);
			}
		} else {
			result = buildRegularLine(line);
		}

		foundCommentEntryTriggerInPreviousLine = foundCommentEntryTriggerInCurrentLine;

		return result;
	}

	protected String processSingleLineCommentEntry(final String line, final int lineNumber,
			final CobolSourceFormatEnum format, final CobolDialect dialect) {
		final CobolLine parsedLine = parseCobolLine(line, format);

		if (parsedLine == null) {
			throwCobolLineParseException(line, lineNumber, format);
		}

		final boolean foundCommentEntryTriggerInCurrentLine = startsWithTrigger(parsedLine, triggersStart);
		final String result;

		if (foundCommentEntryTriggerInCurrentLine) {
			result = escapeCommentEntry(line, parsedLine);
		} else {
			result = buildRegularLine(line);
		}

		return result;
	}

	/**
	 * Checks, whether given line starts with a trigger keyword indicating a
	 * comment entry.
	 */
	protected boolean startsWithTrigger(final CobolLine parsedLine, final String[] triggers) {
		final String contentAreaUpperCase = new String(parsedLine.contentAreaA + parsedLine.contentAreaB).toUpperCase();

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
}
