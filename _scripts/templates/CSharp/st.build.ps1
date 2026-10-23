# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<if(antlr_is_dev)>
$ANTLR4_DEV_DIR = "<antlr_dev_dir>/antlr4"
if (-not (Test-Path "$ANTLR4_DEV_DIR/.git")) {
    git clone --quiet https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
}
Push-Location "$ANTLR4_DEV_DIR"
git checkout dev
git pull
mvn -DskipTests install
Set-Location runtime/CSharp
dotnet build -c Release
Pop-Location
$JAR = (Get-ChildItem "$ANTLR4_DEV_DIR/tool/target/antlr4-*-complete.jar" | Select-Object -Last 1).FullName
$env:ANTLR4_CS_DLL = (Get-ChildItem "$ANTLR4_DEV_DIR/runtime/CSharp/src/bin/Release" -Recurse -Filter "Antlr4.Runtime.Standard.dll" | Select-Object -Last 1).FullName
<else>
$version = "<antlr_version>"
<endif>

<tool_grammar_files:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } >  <x> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlr_is_dev)>
$(& java -jar "$JAR" <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=CSharp <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& dotnet build Test.csproj; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
