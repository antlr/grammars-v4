# Generated from trgen <version>
if (Test-Path -Path transformGrammar.py -PathType Leaf) {
    $(& python3 transformGrammar.py ) 2>&1 | Write-Host
}

$version = Select-String -Path "build.sh" -Pattern "version=" | ForEach-Object { $_.Line -split "=" | Select-Object -Last 1 }

<if(antlrng_tool)>
npm i antlr-ng
<endif>

<tool_grammar_tuples:{x |
<if(antlrng_tool)>
# Download Antlr4 Jar.
antlr4 -v $version
# Run tool.
$(& node node_modules/antlr-ng/dist/cli/runner.js --encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } >  <x.GrammarFileName> ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<else>
$(& antlr4 -v $version <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Java <x.AntlrArgs> <antlr_tool_args:{y | <y> } > ; $compile_exit_code = $LASTEXITCODE) | Write-Host
<endif>
if($compile_exit_code -ne 0){
    exit $compile_exit_code
\}
}>

$JAR = python -c "import os; from pathlib import Path; print(os.path.join(Path.home(), '.m2', 'repository', 'org', 'antlr', 'antlr4', '$version', ('antlr4-' + '$version' + '-complete.jar')))"
$(& javac -cp "${JAR}<if(path_sep_semi)>;<else>:<endif>." <tool_grammar_tuples:{x|<x.GeneratedFileName> }> Test.java ErrorListener.java ; $compile_exit_code = $LASTEXITCODE) | Write-Host
exit $compile_exit_code
