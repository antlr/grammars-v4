# Generated code from Antlr4BuildTasks.dotnet-antlr v <version>
# Makefile for Arithmetic.g4
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .go
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) Program.go
default: classes
classes: $(SOURCES)
	@if [ "$(GOPATH)" = "" ]; then echo "GOPATH must be defined, usually c:/users/youruserid/."; exit 1; fi
	export GO111MODULE=auto; go get github.com/antlr/antlr4/runtime/Go/antlr
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
run:
	export GO111MODULE=auto; trwdog go run Program.go $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -Dlanguage=Go <antlr_tool_args:{y | <y> } > $\<
} >
	export GO111MODULE=auto; go build Program.go
test:
	bash test.sh
