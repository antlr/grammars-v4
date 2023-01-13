# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -Dlanguage=Python3 <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    $msg = pip install -r requirements.txt
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

    $parseOutFile = $InputFile + ".out"
    $o = trwdog python3 -X utf8 Program.py -file $InputFile -tree | Out-File -LiteralPath "$parseOutFile" -Encoding UTF8
    $parseOk = $LASTEXITCODE -eq 0
    $treeMatch = $true
    if ($errorFile) {
        # If we expected errors, then a failed parse was a successful test.
        if (!$parseOk) {
            Write-Host "Expected."
            # Confirm that the errors we received are the ones we expected.
            $expectedData = (Get-Content $errorFile -Encoding UTF8) -join "" -replace "\r\n",""
            $actualData = (Get-Content $parseOutFile -Encoding UTF8) -join "" -replace "\r\n",""
            $parseOk = ($actualData -eq $expectedData)
            if ($parseOk) {
                Write-Host "Error list match succeeded."
            } else {
                Write-Host "Expected error list match." -ForegroundColor Red
            }
        }
    } else {
        if ($parseOk -and (Test-Path $TreeFile)) {
            # Confirm that the parse tree we received is the one we expected.
            $expectedData = Get-Content $TreeFile -Encoding UTF8
            $actualData = Get-Content $parseOutFile -Encoding UTF8
            $treeMatch = ($actualData -eq $expectedData)
        }
    }
    # Restore input and output character encodings.
    [console]::InputEncoding = $oldInputEncoding
    [console]::OutputEncoding = $oldOutputEncoding

    Remove-Item $parseOutFile
    return $parseOk, $treeMatch
}