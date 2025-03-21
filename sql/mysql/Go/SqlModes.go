package parser

import (
    "strings"
)

func sqlModeFromString(modes string) map[SqlMode]bool {
	result := make(map[SqlMode]bool)
	parts := strings.Split(strings.ToUpper(modes), ",")

	for _, mode := range parts {
		switch mode {
		case "ANSI", "DB2", "MAXDB", "MSSQL", "ORACLE", "POSTGRESQL":
			result[AnsiQuotes] = true
			result[PipesAsConcat] = true
			result[IgnoreSpace] = true
		case "ANSI_QUOTES":
			result[AnsiQuotes] = true
		case "PIPES_AS_CONCAT":
			result[PipesAsConcat] = true
		case "NO_BACKSLASH_ESCAPES":
			result[NoBackslashEscapes] = true
		case "IGNORE_SPACE":
			result[IgnoreSpace] = true
		case "HIGH_NOT_PRECEDENCE", "MYSQL323", "MYSQL40":
			result[HighNotPrecedence] = true
		}
	}

	return result
}

