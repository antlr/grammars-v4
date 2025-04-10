# Generated from trgen 0.23.7
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=`grep antlr4 package.json | awk '{print $2}' | tr -d '"' | tr -d ',' | tr -d '\r' | tr -d '\n'`

antlr4 -v $version -encoding utf-8 -Dlanguage=TypeScript   MySQLLexer.g4
antlr4 -v $version -encoding utf-8 -Dlanguage=TypeScript   MySQLParser.g4

rm -rf original

npm install -g typescript ts-node
npm install
tsc -p tsconfig.json --pretty
exit 0
