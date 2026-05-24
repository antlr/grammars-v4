# Generated from trgen <version>
# Get-Content build.ps1 | Write-Host

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
$version = (Select-String -Path "requirements.txt" -Pattern "antlr4" | ForEach-Object {$_.Line.Split("=")[2]}) -replace '"|,|\r|\n'
<endif>

python3 -m venv .venv
<if(test.IsWindows)>.venv\Scripts\Activate.ps1<else>.venv/bin/Activate.ps1<endif>
<if(test.IsWindows)>.venv\Scripts\pip<else>.venv/bin/pip<endif> install -r requirements.txt

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<else>
<if(test.IsWindows)>.venv\Scripts\pip<else>.venv/bin/pip<endif> install antlr4-tools
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlr_is_dev)>
$(& java -jar "$JAR" <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x.GrammarFileNameTarget> -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
}>

exit $compile_exit_code
