/** SQL modes that control parsing behavior. */
var SqlMode;
(function (SqlMode) {
    SqlMode[SqlMode["NoMode"] = 0] = "NoMode";
    SqlMode[SqlMode["AnsiQuotes"] = 1] = "AnsiQuotes";
    SqlMode[SqlMode["HighNotPrecedence"] = 2] = "HighNotPrecedence";
    SqlMode[SqlMode["PipesAsConcat"] = 3] = "PipesAsConcat";
    SqlMode[SqlMode["IgnoreSpace"] = 4] = "IgnoreSpace";
    SqlMode[SqlMode["NoBackslashEscapes"] = 5] = "NoBackslashEscapes";
})(SqlMode || (SqlMode = {}));
export default SqlMode;
