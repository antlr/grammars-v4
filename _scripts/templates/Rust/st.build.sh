# Generated from trgen <version>

set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

JAR="antlr4-4.13.3-SNAPSHOT-complete.jar"
if [ ! -f "$JAR" ]
then
    curl -L -O https://github.com/antlr4rust/antlr4/releases/download/v0.5.0/$JAR
fi

<tool_grammar_tuples:{x |
java -jar $JAR -encoding <antlr_encoding> -Dlanguage=Rust -o src/gen  <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget>
} >

export RUSTFLAGS="-C link-arg=/STACK:16777216"

cargo b --release
