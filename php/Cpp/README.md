# C++ port

**NOTE**: To use the C++ version you **MUST** make following modifications:

* In `PhpLexer.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "PhpLexerBase.h"
    }
    ```

* In `PhpParser.g4`, add the following code after `options { ... }` block

    ```
    @header {
        #include "PhpParserBase.h"
    }
    ```

* In both `PhpLexer.g4` and `PhpParser.g4`, replace all `this.` ocurrences with `this->`
