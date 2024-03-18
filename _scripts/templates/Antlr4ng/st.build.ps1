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

<tool_grammar_tuples:{x |
$(& java -jar ./node_modules/antlr4ng-cli/*.jar <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& tsc -p tsconfig.json --pretty ; $compile_exit_code = $LASTEXITCODE ) | Write-Host

exit $compile_exit_code
