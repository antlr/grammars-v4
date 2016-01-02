/*
Copyright (C) 2015 u.wol@wwu.de

This file is part of cobol85grammar.

cobol85grammar is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

cobol85grammar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with cobol85grammar. If not, see <http://www.gnu.org/licenses/>.
 */

package org.cobol85.preprocessor;

import java.io.File;
import java.io.IOException;

public interface Cobol85Preprocessor {

	public interface Cobol85Format {

		String indicatorField = "([ABCdD\\-/* ])";

		String getRegex();
	}

	public enum Cobol85FormatEnum implements Cobol85Format {

		/**
		 * Custom layout 1.
		 */
		CUSTOM_1("(\\s*[0-9]+)(?:.{7}" + indicatorField + "(.{0,65})(.*)?)?"),

		/**
		 * Format for handling irregular/defect lines.
		 */
		DEFECT("(\\s{7,})" + indicatorField + "([\\*]+)()"),

		/**
		 * Fixed format, standard ANSI / IBM reference. Each line exactly 80
		 * chars.<br />
		 * <br />
		 * 1-6 : sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: area A<br />
		 * 13-72: area B<br />
		 * 73-80: comments<br />
		 */
		FIXED("(.{6})" + indicatorField + "(.{65})(.{8})"),

		/**
		 * HP Tandem format.<br />
		 * <br />
		 * 1: indicator field<br />
		 * 2-5: area A<br />
		 * 6-132: area B<br />
		 */
		TANDEM("()" + indicatorField + "(.*)()"),

		/**
		 * Variable format.<br />
		 * <br />
		 * 1-6 : sequence area<br />
		 * 7: indicator field<br />
		 * 8-12: area A<br />
		 * 13-*: area B<br />
		 */
		VARIABLE("(.{6})(?:" + indicatorField + "(.*)())?");

		private final String regex;

		Cobol85FormatEnum(final String regex) {
			this.regex = regex;
		}

		@Override
		public String getRegex() {
			return regex;
		}
	}

	/**
	 * Representation of a Cobol 85 line.
	 */
	public class Cobol85Line {

		public String comment;

		public String contentArea;

		public char indicatorArea;

		public Cobol85Format lineFormat;

		public String sequenceArea;

		public Cobol85Line(final String sequenceArea, final char indicatorArea, final String contentArea,
				final String comment, final Cobol85Format lineFormat) {
			this.sequenceArea = sequenceArea;
			this.indicatorArea = indicatorArea;
			this.contentArea = contentArea;
			this.comment = comment;
			this.lineFormat = lineFormat;
		}

		@Override
		public String toString() {
			return sequenceArea + indicatorArea + contentArea + comment + " [" + lineFormat + "]";
		}
	}

	String normalizeLine(Cobol85Line line, boolean isFirstLine);

	Cobol85Line parseCobol85Line(String line, Cobol85Format[] formats);

	String process(File inputFile, File libDirectory, Cobol85Format[] formats) throws IOException;

	String process(String input, File libDirectory, Cobol85Format[] formats);
}