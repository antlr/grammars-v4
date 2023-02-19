# Generated from trgen <version>
set -e

<tool_grammar_tuples:{x |
java -jar "<antlr_tool_path>" -encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
}>

JAR=<antlr_tool_path>
CLASSPATH="$JAR<if(path_sep_semi)>\;<else>:<endif>."
javac -cp "$CLASSPATH" *.java

exit 0
