# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .js
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) index.js
default: classes
classes: $(SOURCES)
	npm install
clean:
	rm -rf node_modules
	rm -f package-lock.json
	rm -f *.interp
	rm -f *.tokens
	rm -f $(GENERATED)
run:
	trwdog node index.js $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=JavaScript <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
