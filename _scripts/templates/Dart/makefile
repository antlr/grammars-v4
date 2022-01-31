# Template generated code from trgen <version>
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
	dart compile exe cli.dart
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
	rm -f pubspec.lock
run:
	trwdog <if(os_win)>dart.bat<else>dart<endif> run cli.dart $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
