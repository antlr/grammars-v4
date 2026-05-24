# Generated from trgen <version>

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
$version="<antlr_version>"
<endif>
$env:GO111MODULE = "on"
<if(antlr_is_dev)>
For ($i=0; $i -le 5; $i++) {
	$(& go get github.com/antlr4-go/antlr/v4@latest ; $compile_exit_code = $LASTEXITCODE) | Write-Host
	if($compile_exit_code -eq 0){
		Break
	}
	Write-Host "go get failed. Trying again."
}
<else>
For ($i=0; $i -le 5; $i++) {
	$(& go get github.com/antlr4-go/antlr/v4@v$version ; $compile_exit_code = $LASTEXITCODE) | Write-Host
	if($compile_exit_code -eq 0){
		Break
	}
	Write-Host "go get failed. Trying again."
}
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}
For ($i=0; $i -le 5; $i++) {
	$(& go get golang.org/x/text@v0.26.0 ; $compile_exit_code = $LASTEXITCODE) | Write-Host
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

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Go --lib parser --package parser <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlr_is_dev)>
$(& java -jar "$JAR" <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=Go <if(os_win)>-o parser<endif> -lib parser -package parser  <x.AntlrArgs>  <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=Go <if(os_win)>-o parser<endif> -lib parser -package parser  <x.AntlrArgs>  <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& go build ; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
