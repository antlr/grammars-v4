# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<tool_grammar_files:{x |
$(& java -jar <antlr_tool_path> <x> -encoding <antlr_encoding> -Dlanguage=Dart <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
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
