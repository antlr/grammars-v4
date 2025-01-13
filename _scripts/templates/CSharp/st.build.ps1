# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(antlrng_tool)>
npm i antlr-ng
<endif>

$version = dotnet trxml2 .\Other.csproj `
    | Where-Object { $_ -match 'PackageReference/@Version' } `
    | ForEach-Object {
        ($_ -split '=')[1].Trim()
    }

<tool_grammar_files:{x |
<if(original_tool)>
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlrng_tool)>
$(& pwsh .node_modules/.bin/antlr4ng.ps1 <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& dotnet build Test.csproj; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
