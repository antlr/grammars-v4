# Generated from trgen <version>

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

$(& npm install -g typescript ts-node ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}

$(& npm install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}

$jarFile = Get-ChildItem ./node_modules/antlr4ng-cli/*.jar
<tool_grammar_tuples:{x |
$(& node node_modules/antlr-ng/dist/cli/runner.js --encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } >  <x.GrammarFileName> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& tsc -p tsconfig.json --pretty ; $compile_exit_code = $LASTEXITCODE ) | Write-Host

exit $compile_exit_code
