# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

# Because there is no integrated build script for Dart targets, we need
# to manually look at the version in package.json and extract the
# version number. We can then use this with antlr4 to generate the
# parser and lexer.
$version = (Select-String -Path "package.json" -Pattern "antlr4" | ForEach-Object {$_.Line.Split(" ")[5]}) -replace '"|,|\r|\n'

<if(antlrng_tool)>
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
$(& tsx $HOME/antlr-ng/cli/runner.ts --encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=JavaScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$(& npm install ; $compile_exit_code = $LASTEXITCODE ) | Write-Host
exit $compile_exit_code
