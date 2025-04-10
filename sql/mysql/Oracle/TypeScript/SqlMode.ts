/*
 * Copyright © 2025, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

/** SQL modes that control parsing behavior. */
enum SqlMode {
	NoMode,
	AnsiQuotes,
	HighNotPrecedence,
	PipesAsConcat,
	IgnoreSpace,
	NoBackslashEscapes
}

export default SqlMode;
