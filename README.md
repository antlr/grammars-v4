# grammars-v4

Grammars written for [ANTLR v4](https://github.com/antlr/antlr4)

This repository is a collection of grammars without actions where the
root directory name is the all-lowercase name of the language parsed
by the grammar. For example, java, cpp, csharp, c, etc...

Beware of the licenses on the individual grammars. **THERE IS NO COMMON
LICENSE!** When in doubt or you don't know what you're doing, please use
the BSD or MIT license.

Testing the Grammars
------------

The directory /support/antlr4test-maven-plugin contains a maven plugin which compiles the grammars and then runs them against examples in the /examples subdirectory to verify that the grammars compile and produce a clean parse for each example.

To use the plugin, you will have to compile and install it.
