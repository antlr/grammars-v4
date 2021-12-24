# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .go
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) Program.go
default: classes
classes: setup $(SOURCES) program
setup:
	export GO111MODULE=on; go get -u github.com/antlr/antlr4/runtime/Go/antlr@4.9.3
program:
	export GO111MODULE=auto; go build Program.go
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
run:
	export GO111MODULE=auto; trwdog go run Program.go $(RUNARGS)
test:
	bash test.sh
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Go <antlr_tool_args:{y | <y> } > $\<
} >
