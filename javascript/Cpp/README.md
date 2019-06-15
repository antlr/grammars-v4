# C++ port

**NOTE**: To use the C++ version you **MUST** make following modifications:

* In `JavaScriptLexer.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "JavaScriptBaseLexer.h"
    }
    ```

* In `JavaScriptParser.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "JavaScriptBaseParser.h"
    }
    ```

* In both `JavaScriptLexer.g4` and `JavaScriptParser.g4`, replace all `this.` ocurrences with `this->`
