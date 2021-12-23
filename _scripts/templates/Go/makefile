# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .go
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) Program.go
default: classes
classes: $(SOURCES)
	@if [ "$(GOPATH)" = "" ]; then echo "GOPATH must be defined, usually c:/users/youruserid/."; exit 1; fi
	GO111MODULE=on go get -u github.com/antlr/antlr4/runtime/Go/antlr@4.9.3
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
run:
	export GO111MODULE=auto; trwdog go run Program.go $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Go <antlr_tool_args:{y | <y> } > $\<
} >
	export GO111MODULE=auto; go build Program.go
test:
	bash test.sh
