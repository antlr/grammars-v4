# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=`grep antlr4-python3-runtime requirements.txt | awk -F= '{print $3}' | tr -d '\r' | tr -d '\n'`

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx <if(os_win)>$TEMP<else>$TMPDIR<endif>/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<endif>
}>

pip install -r requirements.txt

exit 0
