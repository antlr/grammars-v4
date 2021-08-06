# HIVE grammar for ANTLR4

An ANTLR4 grammar for HIVE based on version 3.1.2 of official HIVE grammar.The vast majority of grammar come from official HIVE grammar of Apache HIVE.I just translation to ANTLR4 and modify them to adapt to ANTLR4.

The resources:
[https://mvnrepository.com/artifact/org.apache.hive/hive-exec](https://mvnrepository.com/artifact/org.apache.hive/hive-exec).
[https://cwiki.apache.org/confluence/display/Hive/LanguageManual](https://cwiki.apache.org/confluence/display/Hive/LanguageManual).

## Usage, important note

As SQL grammar are normally not case sensitive but this grammar implementation is, you must use a custom [character stream](https://github.com/antlr/antlr4/blob/master/runtime/Java/src/org/antlr/v4/runtime/CharStream.java) that converts all characters to uppercase or lowercase before sending them to the lexer.

You could find more information [here](https://github.com/antlr/antlr4/blob/master/doc/case-insensitive-lexing.md#custom-character-streams-approach) with implementations for various target languages.

## Previous Hive versions
See [this page](v2/) for the case insensitive Hive 2.x.x grammar