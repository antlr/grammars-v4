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
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
$version = Select-String -Path "pubspec.yaml" -Pattern "antlr4" | ForEach-Object {$_.Line.Split(" ")[3]}
<endif>

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_files:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > <x> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlr_is_dev)>
$(& java -jar "$JAR" <x> -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>
For ($i=0; $i -le 5; $i++) {
	$(& dart pub get; $compile_exit_code = $LASTEXITCODE) | Write-Host
	if($compile_exit_code -eq 0){
		Break
	}
	Write-Host "dart pub get failed. Trying again."
}
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}
$(& dart compile exe Test.dart; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
