# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .php
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) Test.php
default: setup classes
setup:
	composer install
classes: $(SOURCES)
clean:
	rm -f *.interp
	rm -f *.tokens
	rm -f $(GENERATED)
run:
	trwdog php Test.php $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=PHP <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
