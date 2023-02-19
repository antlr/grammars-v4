# Generated from trgen <version>
set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<tool_grammar_tuples:{x |
java -jar "<antlr_tool_path>" -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
}>

pip install -r requirements.txt

exit 0
