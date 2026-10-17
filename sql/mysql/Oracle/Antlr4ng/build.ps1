# Generated from trgen 0.23.7

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

$(& npm install -g typescript ts-node ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}

$(& npm install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}

$jarFile = Get-ChildItem ./node_modules/antlr4ng-cli/*.jar
$(& java -jar $jarFile.FullName MySQLLexer.g4 -encoding utf-8 -Dlanguage=TypeScript   ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}
$(& java -jar $jarFile.FullName MySQLParser.g4 -encoding utf-8 -Dlanguage=TypeScript   ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
}

if (Test-Path -Path "original") {
    Remove-Item "original" -Recurse -Force
}

$(& tsc -p tsconfig.json --pretty ; $compile_exit_code = $LASTEXITCODE ) | Write-Host

exit $compile_exit_code
