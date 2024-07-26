/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.line.reader.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.logging.log4j.util.Strings;

import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.CobolLineTypeEnum;
import io.proleap.cobol.preprocessor.sub.line.reader.CobolLineReader;

public class CobolLineReaderImpl implements CobolLineReader {

	protected CobolLineTypeEnum determineType(final String indicatorArea) {
		final CobolLineTypeEnum result;

		switch (indicatorArea) {
		case CobolPreprocessor.CHAR_D:
		case CobolPreprocessor.CHAR_D_:
			result = CobolLineTypeEnum.DEBUG;
			break;
		case CobolPreprocessor.CHAR_MINUS:
			result = CobolLineTypeEnum.CONTINUATION;
			break;
		case CobolPreprocessor.CHAR_ASTERISK:
		case CobolPreprocessor.CHAR_SLASH:
			result = CobolLineTypeEnum.COMMENT;
			break;
		case CobolPreprocessor.WS:
		default:
			result = CobolLineTypeEnum.NORMAL;
			break;
		}

		return result;
	}

	@Override
	public CobolLine parseLine(final String line, final int lineNumber, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final Pattern pattern = format.getPattern();
		final Matcher matcher = pattern.matcher(line);

		final CobolLine result;

		if (Strings.isBlank(line)) {
			result = new CobolLine(CobolLine.blankSequenceArea(format), CobolPreprocessor.WS, "", "", "", format,
					dialect, lineNumber, CobolLineTypeEnum.BLANK);
		} else if (!matcher.matches()) {
			throw new RuntimeException("Is " + format + " the correct line format? Could not parse line "
					+ (lineNumber + 1) + ": " + line);
		} else {
			final String sequenceAreaGroup = matcher.group(1);
			final String indicatorAreaGroup = matcher.group(2);
			final String contentAreaAGroup = matcher.group(3);
			final String contentAreaBGroup = matcher.group(4);
			final String commentAreaGroup = matcher.group(5);

			final String sequenceArea = sequenceAreaGroup != null ? sequenceAreaGroup : "";
			final String indicatorArea = indicatorAreaGroup != null ? indicatorAreaGroup : " ";
			final String contentAreaA = contentAreaAGroup != null ? contentAreaAGroup : "";
			final String contentAreaB = contentAreaBGroup != null ? contentAreaBGroup : "";
			final String commentArea = commentAreaGroup != null ? commentAreaGroup : "";

			final CobolLineTypeEnum type = determineType(indicatorArea);

			result = new CobolLine(sequenceArea, indicatorArea, contentAreaA, contentAreaB, commentArea, format,
					dialect, lineNumber, type);
		}

		return result;
	}

	@Override
	public List<CobolLine> processLines(final String lines, final CobolSourceFormatEnum format,
			final CobolDialect dialect) {
		final Scanner scanner = new Scanner(lines);
		final List<CobolLine> result = new ArrayList<CobolLine>();

		String currentLine = null;
		int lineNumber = 0;

		while (scanner.hasNextLine()) {
			currentLine = scanner.nextLine();

			final CobolLine parsedLine = parseLine(currentLine, lineNumber, format, dialect);
			result.add(parsedLine);

			lineNumber++;
		}

		scanner.close();
		return result;
	}
}
