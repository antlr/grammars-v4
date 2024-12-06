using System.Collections.Generic;

public class SqlModes
{
    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    public static HashSet<SqlMode> sqlModeFromString(string modes)
    {
        var result = new HashSet<SqlMode>();
        var parts = modes.ToUpper().Split(",");
        foreach (var mode in parts)
        {
            if (mode == "ANSI" || mode == "DB2" || mode == "MAXDB" || mode == "MSSQL" || mode == "ORACLE" ||
                mode == "POSTGRESQL")
            {
                result.Add(SqlMode.AnsiQuotes);
                result.Add(SqlMode.PipesAsConcat);
                result.Add(SqlMode.IgnoreSpace);
            }
            else if (mode == "ANSI_QUOTES")
            {
                result.Add(SqlMode.AnsiQuotes);
            }
            else if (mode == "PIPES_AS_CONCAT")
            {
                result.Add(SqlMode.PipesAsConcat);
            }
            else if (mode == "NO_BACKSLASH_ESCAPES")
            {
                result.Add(SqlMode.NoBackslashEscapes);
            }
            else if (mode == "IGNORE_SPACE")
            {
                result.Add(SqlMode.IgnoreSpace);
            }
            else if (mode == "HIGH_NOT_PRECEDENCE" || mode == "MYSQL323" || mode == "MYSQL40")
            {
                result.Add(SqlMode.HighNotPrecedence);
            }
        }
        return result;
    }
}
