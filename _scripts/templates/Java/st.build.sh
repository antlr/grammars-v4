# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
<if(antlr_is_dev)>
ANTLR4_DEV_DIR=<antlr_dev_dir>/antlr4
if [ ! -d "$ANTLR4_DEV_DIR/.git" ]; then
  git clone --quiet https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
fi
(cd "$ANTLR4_DEV_DIR" && git checkout dev && git pull && mvn -DskipTests install)
JAR=$(ls "$ANTLR4_DEV_DIR"/tool/target/antlr4-*-complete.jar | tail -1)
<else>
version=<antlr_version>
JAR=`python -c "import os; from pathlib import Path; print(os.path.join(Path.home() , '.m2',  'repository', 'org', 'antlr', 'antlr4', '$version', 'antlr4-$version-complete.jar'))"`
<endif>

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<elseif(antlr_is_dev)>
java -jar "$JAR" -encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<endif>
}>

CLASSPATH="$JAR<if(path_sep_semi)>\;<else>:<endif>."
javac -cp "$CLASSPATH" *.java

exit 0
