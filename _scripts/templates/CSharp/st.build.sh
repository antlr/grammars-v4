# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<if(antlr_is_dev)>
ANTLR4_DEV_DIR=<antlr_dev_dir>/antlr4
if [ ! -d "$ANTLR4_DEV_DIR/.git" ]; then
  git clone --quiet https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
fi
(cd "$ANTLR4_DEV_DIR" && git checkout dev && git pull && mvn -DskipTests install)
(cd "$ANTLR4_DEV_DIR/runtime/CSharp/src" && dotnet build -c Release)
JAR=$(ls "$ANTLR4_DEV_DIR"/tool/target/antlr4-*-complete.jar | tail -1)
export ANTLR4_CS_DLL=$(ls "$ANTLR4_DEV_DIR"/runtime/CSharp/src/bin/Release/*/Antlr4.Runtime.Standard.dll 2>/dev/null | head -1)
<else>
version=<antlr_version>
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=CSharp <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<elseif(antlr_is_dev)>
java -jar "$JAR" -encoding <antlr_encoding> -Dlanguage=CSharp <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=CSharp <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
<endif>
} >

dotnet restore Test.csproj
dotnet build Test.csproj

exit 0
