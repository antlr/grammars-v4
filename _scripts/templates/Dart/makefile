# Generated code from Antlr4BuildTasks.dotnet-antlr v <version>
# Makefile for Arithmetic.g4
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .dart
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    cli.dart
default: classes
classes: $(SOURCES)
	dart pub get
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
	rm -f pubspec.lock
run:
	dart run cli.dart $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -Dlanguage=Dart $\<
} >
test:
	bash test.sh
