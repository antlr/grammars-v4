# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
version=4.13.1

<if(antlrng_tool)>
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
node node_modules/antlr-ng/dist/cli/runner.js --encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<endif>
}>

JAR=`python -c "import os; from pathlib import Path; print(os.path.join(Path.home() , '.m2',  'repository', 'org', 'antlr', 'antlr4', '$version', 'antlr4-$version-complete.jar'))"`
CLASSPATH="$JAR<if(path_sep_semi)>\;<else>:<endif>."
javac -cp "$CLASSPATH" *.java

exit 0
