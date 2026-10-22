/** SQL modes that control parsing behavior. */
import SqlMode from "./SqlMode.js";
export class SqlModes {
    /**
     * Converts a mode string into individual mode flags.
     *
     * @param modes The input string to parse.
     */
    static sqlModeFromString(modes) {
        var result = new Set();
        const parts = modes.toUpperCase().split(",");
        parts.forEach((mode) => {
            if (mode === "ANSI" || mode === "DB2" || mode === "MAXDB" || mode === "MSSQL" || mode === "ORACLE" ||
                mode === "POSTGRESQL") {
                result.add(SqlMode.AnsiQuotes).add(SqlMode.PipesAsConcat).add(SqlMode.IgnoreSpace);
            }
            else if (mode === "ANSI_QUOTES") {
                result.add(SqlMode.AnsiQuotes);
            }
            else if (mode === "PIPES_AS_CONCAT") {
                result.add(SqlMode.PipesAsConcat);
            }
            else if (mode === "NO_BACKSLASH_ESCAPES") {
                result.add(SqlMode.NoBackslashEscapes);
            }
            else if (mode === "IGNORE_SPACE") {
                result.add(SqlMode.IgnoreSpace);
            }
            else if (mode === "HIGH_NOT_PRECEDENCE" || mode === "MYSQL323" || mode === "MYSQL40") {
                result.add(SqlMode.HighNotPrecedence);
            }
        });
        return result;
    }
}
export default SqlModes;
