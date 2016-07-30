/*
 * Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package org.cobol85.preprocessor;

import java.io.File;
import java.io.IOException;
import java.util.regex.Pattern;

public interface Cobol85Preprocessor {

	public enum Cobol85Dialect {
		ANSI85, MF, OSVS
	}

	/**
	 * Representation of a Cobol 85 line.
	 */
	public class Cobol85Line {

		public String comment;

		public String contentAreaA;

		public String contentAreaB;

		public char indicatorArea;

		public Cobol85SourceFormat lineFormat;

		public String sequenceArea;

		public Cobol85Line(final String sequenceArea, final char indicatorArea, final String contentAreaA,
				final String contentAreaB, final String comment, final Cobol85SourceFormat lineFormat) {
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

	public interface Cobol85SourceFormat {

		String indicatorField = "([ABCdD\\-/* ])";

		Pattern getPattern();

		String getRegex();
	}

	public enum Cobol85SourceFormatEnum implements Cobol85SourceFormat {

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
		FIXED("(.{6})" + indicatorField + "(.{4})(.{61})(.{8,})"),

		/**
		 * HP Tandem format.<br />
		 * <br />
		 * 1: indicator field<br />
		 * 2-5: area A<br />
		 * 6-132: area B<br />
		 */
		TANDEM("()" + indicatorField + "(.{0,4})(.*)()"),

		/**
		 * Variable format.<br />
		 * <br />
		 * 1-6: sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: area A<br />
		 * 13-*: area B<br />
		 */
		VARIABLE("(.{6})(?:" + indicatorField + "(.{0,4})(.*)())?");

		private final Pattern pattern;

		private final String regex;

		Cobol85SourceFormatEnum(final String regex) {
			this.regex = regex;
			pattern = Pattern.compile(regex);
		}

		@Override
		public Pattern getPattern() {
			return pattern;
		}

		@Override
		public String getRegex() {
			return regex;
		}
	}

	String process(File cobolFile, File libDirectory, Cobol85Dialect dialect, Cobol85SourceFormat format)
			throws IOException;

	String process(String cobolSourceCode, File libDirectory, Cobol85Dialect dialect, Cobol85SourceFormat format);
}