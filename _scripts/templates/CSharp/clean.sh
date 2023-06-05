# Generated from trgen <version>
dotnet clean Test.csproj
rm -rf bin obj
rm -f <tool_grammar_tuples:{x|<x.GeneratedFileName> }>
