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
# Find runtime version in composer.json file.
$runtime_version = (Get-Content -Path composer.json | Select-String 'antlr4').ToString().Split(':')[1].Trim(' ",\r\n')

# Get from online sources the commit version number that corresponds to the runtime version.
Remove-Item -Recurse -Force antlr4-php-runtime -ErrorAction SilentlyContinue
Invoke-WebRequest -Uri "https://packagist.org/packages/antlr/antlr4-php-runtime#$runtime_version" -OutFile antlr4-php-runtime
$commit_version = (Get-Content -Path antlr4-php-runtime | Select-String 'BSD-3-Clause').ToString().Split()[-1] -replace '\<.*', ''

# Checkout the sources from the php runtime repo.
Remove-Item -Recurse -Force antlr-php-runtime -ErrorAction SilentlyContinue
git clone https://github.com/antlr/antlr-php-runtime
Set-Location -Path antlr-php-runtime
git checkout $commit_version

# Extract the tool version this damn runtime version corresponds to.
$version = (Get-Content -Path src/RuntimeMetaData.php | Select-String 'public const VERSION').ToString().Split()[-1].Trim("';")

Set-Location ..
Remove-Item -Recurse -Force antlr-php-runtime -ErrorAction SilentlyContinue
<endif>

<if(antlrng_tool)>
npm init -y
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlr_is_dev)>
$(& java -jar "$JAR" -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version -encoding <antlr_encoding> -Dlanguage=PHP <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileNameTarget> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
$compile_exit_code = $LASTEXITCODE
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}

}>

$(& composer install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
exit $compile_exit_code
