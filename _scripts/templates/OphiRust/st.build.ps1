# Generated from trgen <version>

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

# Collect all grammar files into a single antlr4-rust-gen invocation.
$grammars = @()
<tool_grammar_tuples:{x |
$grammars += "<x.GrammarFileNameTarget>"
}>
$(& antlr4-rust-gen @grammars --lib . --out-dir src/gen ; $compile_exit_code = $LASTEXITCODE) | Write-Host
if ($compile_exit_code -ne 0) {
    exit $compile_exit_code
}

if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

<if(os_win)>
$env:RUSTFLAGS = "-C link-arg=/STACK:16777216"
<else>
<endif>

$(& cargo b --release ; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
