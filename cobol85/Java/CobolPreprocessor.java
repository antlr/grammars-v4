/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.regex.Pattern;

public interface CobolPreprocessor {

	public enum CobolDialect {
		ANSI85, MF, OSVS
	}

	public enum CobolSourceFormatEnum {

		/**
		 * Fixed format, standard ANSI / IBM reference. Each line 80
		 * chars.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: area A<br />
		 * 13-72: area B<br />
		 * 73-80: comments<br />
		 */
		FIXED("(.{6})" + INDICATOR_FIELD + "(.{0,4})(.{0,61})(.*)", true),

		/**
		 * HP Tandem format.<br />
		 * <br />
		 * 1: indicator field<br />
		 * 2-5: optional area A<br />
		 * 6-132: optional area B<br />
		 */
		TANDEM("()" + INDICATOR_FIELD + "(.{0,4})(.*)()", false),

		/**
		 * Variable format.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: optional area A<br />
		 * 13-*: optional area B<br />
		 */
		VARIABLE("(.{6})(?:" + INDICATOR_FIELD + "(.{0,4})(.*)())?", true);

		private final boolean commentEntryMultiLine;

		private final Pattern pattern;

		private final String regex;

		CobolSourceFormatEnum(final String regex, final boolean commentEntryMultiLine) {
			this.regex = regex;
			pattern = Pattern.compile(regex);
			this.commentEntryMultiLine = commentEntryMultiLine;
		}

		public Pattern getPattern() {
			return pattern;
		}

		public String getRegex() {
			return regex;
		}

		public boolean isCommentEntryMultiLine() {
			return commentEntryMultiLine;
		}
	}

	final static String CHAR_ASTERISK = "*";

	final static String CHAR_D = "D";

	final static String CHAR_D_ = "d";

	final static String CHAR_MINUS = "-";

	final static String CHAR_SLASH = "/";

	final static String COMMENT_ENTRY_TAG = ">*CE";

	final static String COMMENT_TAG = ">*";

	final static String EXEC_CICS_TAG = ">*EXECCICS";

	final static String EXEC_SQL_TAG = ">*EXECSQL";

	final static String EXEC_SQLIMS_TAG = ">*EXECSQLIMS";

	final static String INDICATOR_FIELD = "([ABCdD\\t\\-/*# ])";

	final static String NEWLINE = "\n";

	final static String WS = " ";

	String process(File cobolFile, List<File> copyFiles, CobolSourceFormatEnum format) throws IOException;

	String process(File cobolFile, List<File> copyFiles, CobolSourceFormatEnum format, CobolDialect dialect)
			throws IOException;

	String process(String cobolCode, List<File> copyFiles, CobolSourceFormatEnum format);

	String process(String cobolCode, List<File> copyFiles, CobolSourceFormatEnum format, CobolDialect dialect);

}