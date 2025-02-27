# Generated from trgen <version>
set -e
if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

version=`dotnet trxml2 Other.csproj | fgrep 'PackageReference/@Version' | awk -F= '{print $2}'`

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=CSharp <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<else>
antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=CSharp <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
<endif>
} >

dotnet restore Test.csproj
dotnet build Test.csproj

exit 0
