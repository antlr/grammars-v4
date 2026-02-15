# Generated from trgen <version>

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

$JAR = "antlr4-4.13.3-SNAPSHOT-complete.jar"
if (-not (Test-Path -Path $JAR -PathType Leaf)) {
    For ($i=0; $i -le 5; $i++) {
	$(& curl -L -O https://github.com/antlr4rust/antlr4/releases/download/v0.5.0/$JAR ; $compile_exit_code = $LASTEXITCODE) | Write-Host
	if($compile_exit_code -eq 0){
		Break
	}
	Write-Host "go get failed. Trying again."
    }
}
if (-not (Test-Path -Path $JAR -PathType Leaf)) {
    exit 1
}

<tool_grammar_tuples:{x |
$(& java -jar $JAR -encoding <antlr_encoding> -Dlanguage=Rust -o src/gen <x.AntlrArgs>  <antlr_tool_args:{y | <y> } >  <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

<if(os_win)>
$env:RUSTFLAGS = "-C link-arg=/STACK:16777216"
<else>
<endif>

$(& cargo b --release ; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
