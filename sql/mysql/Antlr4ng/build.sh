# Generated from trgen 0.23.7
set -e
rm -rf node_modules package-lock.json
npm install -g typescript ts-node
npm install

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=`grep antlr4 package.json | awk '{print $2}' | tr -d '"' | tr -d ',' | tr -d '\r' | tr -d '\n'`

java -jar ./node_modules/antlr4ng-cli/*.jar -encoding utf-8 -Dlanguage=TypeScript   MySQLLexer.g4
java -jar ./node_modules/antlr4ng-cli/*.jar -encoding utf-8 -Dlanguage=TypeScript   MySQLParser.g4

rm -rf original

tsc -p tsconfig.json --pretty
exit 0
