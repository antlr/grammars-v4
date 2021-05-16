# C++ port

**NOTE**: To use the C++ version you **MUST** make following modifications:

* In `TypeScriptLexer.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "TypeScriptLexerBase.h"
    }
    ```

* In `TypeScriptParser.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "TypeScriptParserBase.h"
    }
    ```

* In both `TypeScriptLexer.g4` and `TypeScriptParser.g4`, replace all `this.` ocurrences with `this->`
