# Generated from trgen <version>

version="4.13.1"
export GO111MODULE=on
for i in {1..5}; do go get github.com/antlr4-go/antlr/v4@v$version; if [ "$?" = "0" ]; then break; fi; done; if [ "$?" != "0" ]; then exit 1; fi

set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
# Go has no version, just the latest version.

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Go <if(os_win)>-o parser<endif> -lib parser -package parser <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=Go <if(os_win)>-o parser<endif> -lib parser -package parser  <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<endif>
} >

go build Test.go
