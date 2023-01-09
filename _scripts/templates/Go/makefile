# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .go
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) Test.go
default: classes
classes: setup $(SOURCES) program
setup:
	if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
	-go env
	-go version
	export GO111MODULE=on; for i in {1..5}; do go get github.com/antlr/antlr4/runtime/Go/antlr/v4; if [ $$? = "0" ]; then break; fi; done; if [ $$? != "0" ]; then exit 1; fi
program:
	export GO111MODULE=on; go build Test.go
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
run:
	@trwdog ./<exec_name> $(RUNARGS)
test: FORCE
	bash test.sh
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } > $\<
} >
FORCE: ;
