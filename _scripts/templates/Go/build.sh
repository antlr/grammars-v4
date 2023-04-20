# Generated from trgen <version>
export GO111MODULE=on
for i in {1..5}; do go get github.com/antlr/antlr4/runtime/Go/antlr/v4; if [ "$?" = "0" ]; then break; fi; done; if [ "$?" != "0" ]; then exit 1; fi

set -e

if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<tool_grammar_tuples:{x |
java -jar "<antlr_tool_path>" -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
} >

go build Test.go
