# C++ port

**NOTE**: To use the C++ version you **MUST** make following modifications:

* In `JavaScriptLexer.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "JavaScriptLexerBase.h"
    }
    ```

* In `JavaScriptParser.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "JavaScriptParserBase.h"
    }
    ```

* In both `JavaScriptLexer.g4` and `JavaScriptParser.g4`, replace all `this.` ocurrences with `this->`

Or you can use `pitch.sed` to do convert.