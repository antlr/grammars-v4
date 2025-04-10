/*
 * Copyright 2024, Oracle and/or its affiliates
 */

/* eslint-disable no-underscore-dangle */
/* cspell: ignore antlr, longlong, ULONGLONG, MAXDB */

import java.util.*;

/** The base lexer class provides a number of functions needed in actions in the lexer (grammar). */
public class SqlModes {

    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    public static Set<SqlMode> sqlModeFromString(String modes) {
        Set<SqlMode> result = new HashSet<SqlMode>();

        String[] parts = modes.toUpperCase().split(",");
        for (String mode : parts) {
            switch (mode) {
                case "ANSI":
                case "DB2":
                case "MAXDB":
                case "MSSQL":
                case "ORACLE":
                case "POSTGRESQL":
                    result.add(SqlMode.AnsiQuotes);
                    result.add(SqlMode.PipesAsConcat);
                    result.add(SqlMode.IgnoreSpace);
                    break;
                case "ANSI_QUOTES":
                    result.add(SqlMode.AnsiQuotes);
                    break;
                case "PIPES_AS_CONCAT":
                    result.add(SqlMode.PipesAsConcat);
                    break;
                case "NO_BACKSLASH_ESCAPES":
                    result.add(SqlMode.NoBackslashEscapes);
                    break;
                case "IGNORE_SPACE":
                    result.add(SqlMode.IgnoreSpace);
                    break;
                case "HIGH_NOT_PRECEDENCE":
                case "MYSQL323":
                case "MYSQL40":
                    result.add(SqlMode.HighNotPrecedence);
                    break;
            }
        }
        return result;
    }
}
