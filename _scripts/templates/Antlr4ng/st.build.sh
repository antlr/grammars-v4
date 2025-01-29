# Generated from trgen <version>
set -e
set -x
rm -rf node_modules package-lock.json
npm install -g typescript ts-node
npm install

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=`grep antlr4 package.json | awk '{print $2}' | tr -d '"' | tr -d ',' | tr -d '\r' | tr -d '\n'`

ls node_modules
ls node_modules/.bin
find . -name '*.ps1'

<tool_grammar_tuples:{x |
tsx /tmp/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
} >

tsc -p tsconfig.json --pretty
exit 0
