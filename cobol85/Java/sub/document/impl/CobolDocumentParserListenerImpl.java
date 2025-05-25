/*
 * Copyright (C) 2017, Ulrich Wolffgang <u.wol@wwu.de>
 * All rights reserved.
 *
 * This software may be modified and distributed under the terms
 * of the BSD 3-clause license. See the LICENSE file for details.
 */

package io.proleap.cobol.preprocessor.sub.document.impl;

import java.io.File;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.List;
import java.util.Scanner;

import org.antlr.v4.runtime.BufferedTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.io.FilenameUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import io.proleap.cobol.Cobol85PreprocessorBaseListener;
import io.proleap.cobol.Cobol85PreprocessorParser;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplaceClauseContext;
import io.proleap.cobol.Cobol85PreprocessorParser.ReplacingPhraseContext;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolDialect;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl;
import io.proleap.cobol.preprocessor.sub.CobolLine;
import io.proleap.cobol.preprocessor.sub.util.TokenUtils;

/**
 * ANTLR visitor, which preprocesses a given COBOL program by executing COPY and
 * REPLACE statements.
 */
public class CobolDocumentParserListenerImpl extends Cobol85PreprocessorBaseListener {

	private final static Logger LOG = LogManager.getLogger(CobolDocumentParserListenerImpl.class);

	private final Deque<CobolDocumentContext> contexts = new ArrayDeque<>();

	private final List<File> copyFiles;

	private final CobolDialect dialect;

	private final CobolSourceFormatEnum format;

	private final BufferedTokenStream tokens;

	public CobolDocumentParserListenerImpl(final List<File> copyFiles, final CobolSourceFormatEnum format,
			final CobolDialect dialect, final BufferedTokenStream tokens) {
		this.copyFiles = copyFiles;
		this.dialect = dialect;
		this.tokens = tokens;
		this.format = format;

		contexts.push(new CobolDocumentContext());
	}

	protected String buildLines(final String text, final String linePrefix) {
		final StringBuffer sb = new StringBuffer(text.length());
		final Scanner scanner = new Scanner(text);
		boolean firstLine = true;

		while (scanner.hasNextLine()) {
			final String line = scanner.nextLine();

			if (!firstLine) {
				sb.append(CobolPreprocessor.NEWLINE);
			}

			sb.append(linePrefix + CobolPreprocessor.WS + line.trim());
			firstLine = false;
		}

		scanner.close();
		return sb.toString();
	}

	public CobolDocumentContext context() {
		return contexts.peek();
	}

	@Override
	public void enterControlSpacingStatement(final Cobol85PreprocessorParser.ControlSpacingStatementContext ctx) {
		push();
	}

	@Override
	public void enterCopyStatement(final Cobol85PreprocessorParser.CopyStatementContext ctx) {
		// push a new context for COPY terminals
		push();
	}

	@Override
	public void enterExecCicsStatement(final Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
		// push a new context for SQL terminals
		push();
	}

	@Override
	public void enterExecSqlImsStatement(final Cobol85PreprocessorParser.ExecSqlImsStatementContext ctx) {
		// push a new context for SQL IMS terminals
		push();
	}

	@Override
	public void enterExecSqlStatement(final Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
		// push a new context for SQL terminals
		push();
	}

	@Override
	public void enterReplaceArea(final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
		push();
	}

	@Override
	public void enterReplaceByStatement(final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
		push();
	}

	@Override
	public void enterReplaceOffStatement(final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
		push();
	}

	@Override
	public void exitControlSpacingStatement(final Cobol85PreprocessorParser.ControlSpacingStatementContext ctx) {
		// throw away control spacing statement
		pop();
	};

	@Override
	public void exitCopyStatement(final Cobol85PreprocessorParser.CopyStatementContext ctx) {
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

		if (copyFiles == null || copyFiles.isEmpty()) {
			LOG.warn("Could not identify copy file {} due to missing copy files.", copyFileIdentifier);
		} else {
			final String fileContent = getCopyFileContent(copyFileIdentifier, copyFiles, dialect, format);

			if (fileContent != null) {
				context().write(fileContent + CobolPreprocessor.NEWLINE);
				context().replaceReplaceablesByReplacements(tokens);
			}
		}

		final String content = context().read();
		pop();

		context().write(content);
	}

	@Override
	public void exitExecCicsStatement(final Cobol85PreprocessorParser.ExecCicsStatementContext ctx) {
		// throw away EXEC CICS terminals
		pop();

		// a new context for the CICS statement
		push();

		/*
		 * text
		 */
		final String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
		final String linePrefix = CobolLine.blankSequenceArea(format) + CobolPreprocessor.EXEC_CICS_TAG;
		final String lines = buildLines(text, linePrefix);

		context().write(lines);

		final String content = context().read();
		pop();

		context().write(content);
	}

	@Override
	public void exitExecSqlImsStatement(final Cobol85PreprocessorParser.ExecSqlImsStatementContext ctx) {
		// throw away EXEC SQLIMS terminals
		pop();

		// a new context for the SQLIMS statement
		push();

		/*
		 * text
		 */
		final String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
		final String linePrefix = CobolLine.blankSequenceArea(format) + CobolPreprocessor.EXEC_SQLIMS_TAG;
		final String lines = buildLines(text, linePrefix);

		context().write(lines);

		final String content = context().read();
		pop();

		context().write(content);
	}

	@Override
	public void exitExecSqlStatement(final Cobol85PreprocessorParser.ExecSqlStatementContext ctx) {
		// throw away EXEC SQL terminals
		pop();

		// a new context for the SQL statement
		push();

		/*
		 * text
		 */
		final String text = TokenUtils.getTextIncludingHiddenTokens(ctx, tokens);
		final String linePrefix = CobolLine.blankSequenceArea(format) + CobolPreprocessor.EXEC_SQL_TAG;
		final String lines = buildLines(text, linePrefix);

		context().write(lines);

		final String content = context().read();
		pop();

		context().write(content);
	}

	@Override
	public void exitReplaceArea(final Cobol85PreprocessorParser.ReplaceAreaContext ctx) {
		/*
		 * replacement phrase
		 */
		final List<ReplaceClauseContext> replaceClauses = ctx.replaceByStatement().replaceClause();
		context().storeReplaceablesAndReplacements(replaceClauses);

		context().replaceReplaceablesByReplacements(tokens);
		final String content = context().read();

		pop();
		context().write(content);
	}

	@Override
	public void exitReplaceByStatement(final Cobol85PreprocessorParser.ReplaceByStatementContext ctx) {
		// throw away REPLACE BY terminals
		pop();
	}

	@Override
	public void exitReplaceOffStatement(final Cobol85PreprocessorParser.ReplaceOffStatementContext ctx) {
		// throw away REPLACE OFF terminals
		pop();
	};

	protected String getCopyFileContent(final String filename, final List<File> copyFiles, final CobolDialect dialect,
			final CobolSourceFormatEnum format) {
		final File copyFile = identifyCopyFile(filename, copyFiles);
		String result;

		if (copyFile == null) {
			LOG.warn("Copy file {} not found in copy files {}.", filename, copyFiles);
			result = null;
		} else {
			try {
				result = new CobolPreprocessorImpl().process(copyFile, copyFiles, format, dialect);
			} catch (final IOException e) {
				result = null;
				LOG.warn(e.getMessage());
			}
		}

		return result;
	}

	/**
	 * Identifies a copy file by its name and directory.
	 */
	protected File identifyCopyFile(final String filename, final List<File> copyFiles) {
		File copyFile = null;

		for (final File file : copyFiles) {
			final String baseName = FilenameUtils.getBaseName(file.getName());
			final boolean matchingBaseName = filename.toLowerCase().equals(baseName.toLowerCase());

			if (matchingBaseName) {
				copyFile = file;
				break;
			}
		}

		return copyFile;
	}

	/**
	 * Pops the current preprocessing context from the stack.
	 */
	protected CobolDocumentContext pop() {
		return contexts.pop();
	}

	/**
	 * Pushes a new preprocessing context onto the stack.
	 */
	protected CobolDocumentContext push() {
		return contexts.push(new CobolDocumentContext());
	}

	@Override
	public void visitTerminal(final TerminalNode node) {
		final int tokPos = node.getSourceInterval().a;
		context().write(TokenUtils.getHiddenTokensToLeft(tokPos, tokens));

		if (!TokenUtils.isEOF(node)) {
			final String text = node.getText();
			context().write(text);
		}
	}
}
