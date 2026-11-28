# Generated from trgen <version>
$ErrorActionPreference = "Stop"
if (Test-Path -LiteralPath transformGrammar.py) { python3 transformGrammar.py; if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE } }
$galop = if ($env:GALOP) { $env:GALOP } else { 'galop' }
foreach ($grammar in @(<tool_grammar_files:{g | "<g>" }; separator=",">)) {
    & $galop -o . -package main $grammar
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
}
& go build -o <if(os_win)>Test.exe<else>Test<endif> .
exit $LASTEXITCODE
