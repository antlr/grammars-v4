# Template generated code from trgen <version>
JAR = <antlr_tool_path>
CLASSPATH = $(JAR)<if(path_sep_semi)>\;<else>:<endif>.
.SUFFIXES: .g4 .dart
ANTLRGRAMMARS ?= $(wildcard *.g4)
GENERATED = <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
SOURCES = $(GENERATED) \
    Test.dart
default: setup classes
setup:
	if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
classes: $(SOURCES)
	dart pub get
	dart compile exe Test.dart
clean:
	rm -f *.tokens *.interp
	rm -f $(GENERATED)
	rm -f pubspec.lock
run:
	@trwdog Test.exe $(RUNARGS)
<tool_grammar_tuples:{x | <x.GeneratedFileName> : <x.GrammarFileName>
	java -jar $(JAR) -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > $\<
} >
test:
	bash test.sh
