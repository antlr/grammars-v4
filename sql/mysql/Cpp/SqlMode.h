#pragma once

/** SQL modes that control parsing behavior. */
enum SqlMode {
    NoMode,
    AnsiQuotes,
    HighNotPrecedence,
    PipesAsConcat,
    IgnoreSpace,
    NoBackslashEscapes
};
