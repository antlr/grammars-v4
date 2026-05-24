# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<if(antlr_is_dev)>
ANTLR4_DEV_DIR=<antlr_dev_dir>/antlr4
if [ ! -d "$ANTLR4_DEV_DIR/.git" ]; then
  git clone https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
fi
(cd "$ANTLR4_DEV_DIR" && git checkout dev && git pull && mvn -DskipTests install)
JAR=$(ls "$ANTLR4_DEV_DIR"/tool/target/antlr4-*-complete.jar | tail -1)
<else>
version=`grep antlr4-python3-runtime requirements.txt | awk -F= '{print $3}' | tr -d '\r' | tr -d '\n'`
<endif>

python3 -m venv .venv
source .venv/bin/activate

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<else>
.venv/bin/pip install antlr4-tools
<endif>

.venv/bin/pip install -r requirements.txt

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<elseif(antlr_is_dev)>
java -jar "$JAR" -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<endif>
}>

exit 0
