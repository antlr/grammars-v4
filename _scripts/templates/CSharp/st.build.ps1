# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

$version = dotnet trxml2 Other.csproj `
    | Where-Object { $_ -match 'PackageReference/@Version' } `
    | ForEach-Object {
        ($_ -split '=')[1].Trim()
    }

<tool_grammar_files:{x |
<if(antlrng_tool)>
$(& node node_modules/antlr-ng/dist/cli/runner.js --encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } >  <x> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& dotnet build Test.csproj; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
