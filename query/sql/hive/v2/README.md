# HIVE 2.x.x grammar for ANTLR4

An ANTLR4 grammar for HIVE 2.3.8. Based on the [Hive grammar for version 3.1.2](https://github.com/antlr/grammars-v4/tree/master/sql/hive).

The resources:
[https://mvnrepository.com/artifact/org.apache.hive/hive-exec](https://mvnrepository.com/artifact/org.apache.hive/hive-exec).
[https://cwiki.apache.org/confluence/display/Hive/LanguageManual](https://cwiki.apache.org/confluence/display/Hive/LanguageManual).

## Important differences from the [3.1.2 grammar](https://github.com/antlr/grammars-v4/tree/master/sql/hive):

This implementation is case insensitive.

Also, the starting rule parses multiple statements.
