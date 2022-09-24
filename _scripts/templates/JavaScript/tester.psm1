# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -Dlanguage=JavaScript <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    $msg = npm install
    return @{
        Message = $msg
        Success = $LASTEXITCODE -eq 0
    }
}

function Test-Case {
    param (
        $InputFile,
        $TokenFile,
        $TreeFile,
        $ErrorFile
    )
    $treeOutFile = $TreeFile + ".out"
    $o = trwdog node index.js -file $InputFile -tree | Out-File -LiteralPath "$treeOutFile"
    $failed = $LASTEXITCODE -ne 0
    $parseOk = !$failed
    if ($failed -and $errorFile) {
        $parseOk = $true
    }
    if(!$failed -and !$errorFile){
        $parseOk = $true
    }
    $treeMatch = $true
    if (Test-Path $TreeFile) {
        $expectedData = Get-Content $TreeFile
        $actualData = Get-Content $treeOutFile
        $treeMatch = ($actualData -eq $expectedData)
    }
    Remove-Item $treeOutFile
    return $parseOk, $treeMatch
}