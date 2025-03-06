# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=`grep antlr4 package.json | awk '{print $2}' | tr -d '"' | tr -d ',' | tr -d '\r' | tr -d '\n'`

<if(antlrng_tool)>
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<endif>
} >

npm install
exit 0
