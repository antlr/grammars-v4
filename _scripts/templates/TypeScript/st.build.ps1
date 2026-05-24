# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(antlr_is_dev)>
$ANTLR4_DEV_DIR = "<antlr_dev_dir>/antlr4"
if (-not (Test-Path "$ANTLR4_DEV_DIR/.git")) {
    git clone https://github.com/antlr/antlr4.git "$ANTLR4_DEV_DIR"
}
Push-Location "$ANTLR4_DEV_DIR"
git checkout dev
git pull
mvn -DskipTests install
Pop-Location
$JAR = (Get-ChildItem "$ANTLR4_DEV_DIR/tool/target/antlr4-*-complete.jar" | Select-Object -Last 1).FullName
<else>
# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
$version = (Select-String -Path "package.json" -Pattern "antlr4" | ForEach-Object {$_.Line.Split(" ")[5]}) -replace '"|,|\r|\n'
<endif>

<tool_grammar_tuples:{x |
<if(antlr_is_dev)>
$(& java -jar "$JAR" <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& npm install -g typescript ts-node ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}

$(& npm install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}

$(& tsc -p tsconfig.json --pretty ; $compile_exit_code = $LASTEXITCODE ) | Write-Host

exit $compile_exit_code
