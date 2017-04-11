/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor;

import java.io.File;
import java.io.IOException;
import java.util.regex.Pattern;

public interface CobolPreprocessor {

	public enum CobolDialect {
		ANSI85, MF, OSVS
	}

	/**
	 * Representation of a Cobol line.
	 */
	public class CobolLine {

		public String comment;

		public String contentAreaA;

		public String contentAreaB;

		public char indicatorArea;

		public CobolSourceFormat lineFormat;

		public String sequenceArea;

		public CobolLine(final String sequenceArea, final char indicatorArea, final String contentAreaA,
				final String contentAreaB, final String comment, final CobolSourceFormat lineFormat) {
			this.sequenceArea = sequenceArea;
			this.indicatorArea = indicatorArea;
			this.contentAreaA = contentAreaA;
			this.contentAreaB = contentAreaB;
			this.comment = comment;
			this.lineFormat = lineFormat;
		}

		@Override
		public String toString() {
			return sequenceArea + indicatorArea + contentAreaA + contentAreaB + comment + " [" + lineFormat + "]";
		}
	}

	public interface CobolSourceFormat {

		String indicatorField = "([ABCdD\\t\\-/* ])";

		Pattern getPattern();

		String getRegex();

		boolean isCommentEntryMultiLine();
	}

	public enum CobolSourceFormatEnum implements CobolSourceFormat {

		/**
		 * Fixed format, standard ANSI / IBM reference. Each line exactly 80
		 * chars.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: area A<br />
		 * 13-72: area B<br />
		 * 73-80: comments<br />
		 */
		FIXED("(.{6})" + indicatorField + "(.{4})(.{61})(.{8,})", true),

		/**
		 * Flexible fixed format. Each line up to 80 chars.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: optional area A<br />
		 * 13-72: optional area B<br />
		 * 73-80: optional comments<br />
		 */
		FIXED_FLEX("(.{6})" + indicatorField + "(.{0,4})(.{0,61})(.*)", true),

		/**
		 * HP Tandem format.<br />
		 * <br />
		 * 1: indicator field<br />
		 * 2-5: optional area A<br />
		 * 6-132: optional area B<br />
		 */
		TANDEM("()" + indicatorField + "(.{0,4})(.*)()", false),

		/**
		 * Variable format.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: optional area A<br />
		 * 13-*: optional area B<br />
		 */
		VARIABLE("(.{6})(?:" + indicatorField + "(.{0,4})(.*)())?", true);

		private final boolean commentEntryMultiLine;

		private final Pattern pattern;

		private final String regex;

		CobolSourceFormatEnum(final String regex, final boolean commentEntryMultiLine) {
			this.regex = regex;
			pattern = Pattern.compile(regex);
			this.commentEntryMultiLine = commentEntryMultiLine;
		}

		@Override
		public Pattern getPattern() {
			return pattern;
		}

		@Override
		public String getRegex() {
			return regex;
		}

		@Override
		public boolean isCommentEntryMultiLine() {
			return commentEntryMultiLine;
		}
	}

	String process(File cobolFile, File libDirectory, CobolSourceFormat format) throws IOException;

	String process(File cobolFile, File libDirectory, CobolSourceFormat format, CobolDialect dialect)
			throws IOException;

	String process(String cobolSourceCode, File libDirectory, CobolSourceFormat format);

	String process(String cobolSourceCode, File libDirectory, CobolSourceFormat format, CobolDialect dialect);

}