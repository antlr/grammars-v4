# Kirikiri TJS2 ANTLR4 grammar

Kirikiri TJS2 is a Javascript-like script language for Kirikiri2 game engine.

Modified from [JavaScript grammar](https://github.com/antlr/grammars-v4/tree/aba1c22c7062279a3bd26f81413cf5bf72a21b91/javascript)

Parse [Kirikiri TJS2](https://krkrz.github.io/krkr2doc/tjs2doc/contents/index.html) language.

Tested on [KAG3](https://github.com/krkrz/kag3), [KAG3-HAM](https://github.com/krkrz/kag3_ham) and [some games](example/README.md)

Only Java Runtime have tested. All other runtime is directly copy-paste-rename without test.

Known issue:
- Now consider `¥`(`\u00a5`,japanese yen) as UnexpectedCharacter insteadof Backslash(`\`,`\u005c`).
- TJS2 bytecode also use *.tjs extension, this script can't parse them.
- Need an preprocessor (`preprocessor.js`) to handle preprocess command.
