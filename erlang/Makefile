all: compile

compile:
	antlr4 Erlang.g4
	javac Erlang*.java

debug: compile
	java org.antlr.v4.gui.TestRig  Erlang forms -gui -encoding utf8

check: compile
	./check.sh test/

clean:
	rm -f *.class

distclean: clean
	rm -f *.java *.tokens *.interp
