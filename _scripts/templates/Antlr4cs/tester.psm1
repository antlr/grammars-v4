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
    $treeOutFile = $TreeFile + ".out"
    $o = trwdog dotnet CSharp/Test.dll -file $InputFile -tree > $treeOutFile
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