# Generated from trgen <version>
version=4.13.1
rm -f *.interp
files=()
JAR=`python -c "import os; from pathlib import Path; print(os.path.join(Path.home() , '.m2',  'repository', 'org', 'antlr', 'antlr4', '$version', 'antlr4-$version-complete.jar'))"`
<tool_grammar_tuples:{x |
files+=( `java -jar "$JAR" -depend -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> | awk '{print $1\}' | grep -v ':'` )
} >
for i in ${files[*]}
do
    rm -f $i
done
rm -rf __pycache__
exit 0
