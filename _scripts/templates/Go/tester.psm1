# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_tuples:{x |
    $g = antlr <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    # Output version of pwsh.
    #Get-Host | Select-Object Version | Write-Host
    # Output environmental variables.
    #dir env: | Out-String | Write-Host
    # Output go version
    #go version | Write-Host
    $env:GO111MODULE = "on"
    For ($i=0; $i -le 5; $i++) {
        $g = go get github.com/antlr/antlr4/runtime/Go/antlr/v4
        if($LASTEXITCODE -eq 0){
            Break
        }
        Write-Host "go get failed. Trying again."
    }
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        }
    }
    $msg = go build Test.go
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
    # Save input and output character encodings and switch to UTF-8.
    $oldInputEncoding = [console]::InputEncoding
    $oldOutputEncoding = [console]::OutputEncoding
    $OutputEncoding = [console]::InputEncoding = [console]::OutputEncoding = New-Object System.Text.UTF8Encoding

    $treeOutFile = $TreeFile + ".out"
    $o = trwdog ./Test -file $InputFile -tree | Out-File -LiteralPath "$treeOutFile" -Encoding UTF8
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
        $expectedData = Get-Content $TreeFile -Encoding UTF8
        $actualData = Get-Content $treeOutFile -Encoding UTF8
        $treeMatch = ($actualData -eq $expectedData)
    }
    # Restore input and output character encodings.
    [console]::InputEncoding = $oldInputEncoding
    [console]::OutputEncoding = $oldOutputEncoding

    Remove-Item $treeOutFile
    return $parseOk, $treeMatch
}