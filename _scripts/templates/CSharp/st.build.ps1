# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

$trxml2_output = dotnet trxml2 Other.csproj
if (-not $trxml2_output -or ($trxml2_output -match '^\s*$')) {
    Write-Host "trxml2 returned no output. Contents of Other.csproj:"
    Get-Content Other.csproj | Write-Host
}
$version = $trxml2_output | Select-String 'PackageReference/@Version' | ForEach-Object { ($_ -split '=')[1].Trim() }
if (-not $version -or $version -eq '') {
    Write-Host "version is empty, defaulting to 4.13.1"
    $version = "4.13.1"
}
Write-Host "trxml2 output"
dotnet trxml2 Other.csproj | Write-Host
Write-Host "trxml2 plus select string output"
dotnet trxml2 Other.csproj | Select-String 'PackageReference/@Version' | Write-Host
Write-Host "version = '$version'"

<tool_grammar_files:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } >  <x> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& dotnet build Test.csproj; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
