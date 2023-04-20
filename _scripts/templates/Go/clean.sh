# Generated from trgen <version>
rm -f *.interp
files=()
<tool_grammar_tuples:{x |
files+=( `java -jar "<antlr_tool_path>" -depend -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName> | awk '{print $1\}' | grep -v ':'` )
} >
for i in ${files[*]}
do
    rm -f $i
done
rm -f ./<if(os_win)>Test.exe<else>Test<endif>
