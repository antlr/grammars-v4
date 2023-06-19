# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=4.13.0; runtime_version=0.9.0

<tool_grammar_tuples:{x |
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
}>

composer install

exit 0
