# Template generated code from trgen <version>
function Build-Grammar {
    $msg = dotnet build -o CSharp
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
    $o = trwdog dotnet CSharp/Test.dll -file $InputFile -tree | Out-File -LiteralPath "$treeOutFile" -Encoding UTF8
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