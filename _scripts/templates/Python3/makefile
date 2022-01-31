# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .py
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    Program.py
default: classes
classes: $(SOURCES)
	pip install -r requirements.txt
clean:
	rm -f *.tokens *.interp
	rm -f *.interp
	rm -f *.tokens
	rm -f $(GENERATED)
run:
	trwdog python Program.py $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Python3 <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
