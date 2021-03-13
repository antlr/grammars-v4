# Generated code from Antlr4BuildTasks.dotnet-antlr v <version>
# Makefile for Arithmetic.g4
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .java .class
.java.class:
	javac -cp $(CLASSPATH) $*.java
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    Program.java \
    ErrorListener.java
default: classes
classes: $(GENERATED) $(SOURCES:.java=.class)
clean:
	rm -f *.class
	rm -f *.interp
	rm -f *.tokens
	rm -f $(GENERATED)
run:
	java -classpath $(CLASSPATH) Program $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) $\<
} >
test:
	bash test.sh
