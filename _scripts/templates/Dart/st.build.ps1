# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in pubspec.yaml and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
$version = Select-String -Path "pubspec.yaml" -Pattern "antlr4" | ForEach-Object {$_.Line.Split(" ")[3]}

<tool_grammar_files:{x |
$(& antlr4 -v $version <x> -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
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
