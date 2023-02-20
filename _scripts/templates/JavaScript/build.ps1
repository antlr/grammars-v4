# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<tool_grammar_tuples:{x |
$(& java -jar <antlr_tool_path> <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& npm install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
exit $compile_exit_code
