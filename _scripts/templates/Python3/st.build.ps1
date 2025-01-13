# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in requirements.txt and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
$version = (Select-String -Path "requirements.txt" -Pattern "antlr4" | ForEach-Object {$_.Line.Split("=")[2]}) -replace '"|,|\r|\n'

<if(antlrng_tool)>
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(original_tool)>
$(& antlr4 -v $version <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<elseif(antlrng_tool)>
$(& pwsh .node_modules/.bin/antlr4ng.ps1 <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Python3 <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& pip install -r requirements.txt ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
exit $compile_exit_code
