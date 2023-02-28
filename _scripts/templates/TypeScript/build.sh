# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<tool_grammar_tuples:{x |
java -jar "<antlr_tool_path>" -encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
} >

npm install -g typescript ts-node
npm install
npm run build
exit 0
