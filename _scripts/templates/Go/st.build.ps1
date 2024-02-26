# Generated from trgen <version>

$version = Select-String -Path "go.mod" -Pattern "github.com/antlr4-go/antlr/v4" | ForEach-Object { $_.Line.Split(" ")[1] } | ForEach-Object { $_ -replace 'v' }

$env:GO111MODULE = "on"
For ($i=0; $i -le 5; $i++) {
	$(& go get github.com/antlr4-go/antlr/v4@v$version ; $compile_exit_code = $LASTEXITCODE) | Write-Host
	if($compile_exit_code -eq 0){
		Break
	}
	Write-Host "go get failed. Trying again."
}
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<tool_grammar_tuples:{x |
$(& antlr4 -v $version <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& go build Test.go; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
