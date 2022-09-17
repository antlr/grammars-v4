# Antlr4 in Antlr4

As of 24 Feb 2021:

* mvn test builds cleanly. This will generate a parser for the Java runtime under target/.
* Java, C#, JavaScript work on all grammars in examples/. Other targets have been provided but not tested.
* All files have been organized in directories named after the target.
* The split grammar ANTLRv4Lexer.g4 and ANTLRv4Parser.g4 contain "minimum" target-specific code, done in a "target-independent" way. Target-specific code for Python3 is contained in the Python3/ directory.
* The code is no generated with the -package option, and the grammars do not contain target-specific code that declares a project.
