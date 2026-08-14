# Generated from trgen <version>

set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

# Collect all grammar files into a single antlr4-rust-gen invocation.
# (antlr4-rust-gen takes all grammars in one call, unlike the Java ANTLR tool.)
GRAMMARS=""
<tool_grammar_tuples:{x |
GRAMMARS="$GRAMMARS <x.GrammarFileNameTarget>"
}>
antlr4-rust-gen $GRAMMARS --lib . --out-dir src/gen
ls -R src/

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<if(os_win)>
export RUSTFLAGS="-C link-arg=/STACK:16777216"
<else>
ulimit -s
ulimit -s 16384
<endif>

cargo b --release
