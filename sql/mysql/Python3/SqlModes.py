import sys
from readchar import readchar
from SqlMode import SqlMode

class SqlModes:

    @staticmethod
    def sqlModeFromString(modes):
        sqlModes = set()
        parts = modes.upper().split(",")
        for mode in parts:
            if mode in {"ANSI", "DB2", "MAXDB", "MSSQL", "ORACLE", "POSTGRESQL"}:
                sqlModes.update({SqlMode.AnsiQuotes, SqlMode.PipesAsConcat, SqlMode.IgnoreSpace})
            elif mode == "ANSI_QUOTES":
                sqlModes.add(SqlMode.AnsiQuotes)
            elif mode == "PIPES_AS_CONCAT":
                sqlModes.add(SqlMode.PipesAsConcat)
            elif mode == "NO_BACKSLASH_ESCAPES":
                sqlModes.add(SqlMode.NoBackslashEscapes)
            elif mode == "IGNORE_SPACE":
                sqlModes.add(SqlMode.IgnoreSpace)
            elif mode in {"HIGH_NOT_PRECEDENCE", "MYSQL323", "MYSQL40"}:
                sqlModes.add(SqlMode.HighNotPrecedence)
        return sqlModes
