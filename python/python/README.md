# Python 2 and 3 universal grammar

Universal, clear and fast Python grammar.

## How to use

* Generate lexer and parser with ANTLR4 generator
* Include both generated and base classes to project from corresponding
  directory
* Use `TabSize` property in `PythonLexerBase` to configure tab size
  (8 spaces by default)
* Use `Version` property in `PythonParserBase` to configure python version
  (`Autodetect`, `Python2` or `Python3`). If `Autodetect` is selected, it will
  be switched to certain value after code fragment parsing.

## Main contributors

* Bart Kiers, bart@big-o.nl, 2014:
    * Initial version
* Dmitriy Litovchenko, Dmitry.Litovchenko1@yandex.ru, Positive Technologies, 2019:
    * Separate lexer and parser
    * Universal grammar
* Nikita Subbotin, sub.nik.and@gmail.com, 2019
    * Samples set
    * Parser refactoring
* Ivan Kochurkin, kvanttt@gmail.com, Positive Technologies, 2019:
    * Optimized lexer
    * Error fixes
    * Java version
    * Refactoring

## License

[MIT](https://opensource.org/licenses/MIT)