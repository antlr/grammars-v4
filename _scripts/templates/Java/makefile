# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .java .class
.java.class:
	javac -cp $(CLASSPATH) $*.java
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    Test.java \
    ErrorListener.java
default: classes
classes: $(GENERATED) $(SOURCES:.java=.class)
clean:
	rm -f *.class *.interp *.tokens
	rm -f $(GENERATED)
run:
	@trwdog java -classpath $(CLASSPATH) Test $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
