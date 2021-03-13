# Generated code from Antlr4BuildTasks.dotnet-antlr v 2.2
# Makefile for Arithmetic.g4
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .py
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    Program.py
default: classes
classes: $(SOURCES)
	pip install antlr4-python3-runtime
	pip install readchar
clean:
	rm -f *.tokens *.interp
	rm -f *.interp
	rm -f *.tokens
	rm -f $(GENERATED)
run:
	python Program.py $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -Dlanguage=Python3 $\<
} >
test:
	bash test.sh
