# Template generated code from trgen <version>
function Test-Case {
    $TestDirectory = "<if(os_win)>../<example_files_win><else>../<example_files_unix><endif>"
    Write-Host "Test cases here: $TestDirectory"
    if (Test-Path -Path "tests.txt" -PathType Leaf) {
        Remove-Item "tests.txt"
    }
    foreach ($item in Get-ChildItem $TestDirectory -Recurse) {
        $case = $item.fullname
        $ext = $item.Extension
        if (($ext -eq ".errors") -or ($ext -eq ".tree")) {
            $shouldFail = $true
            continue
        }
        if (Test-Path $case -PathType Container) {
            continue
        }
        Write-Host "Test case: $item"
        Add-Content "tests.txt" $case
    }
    if (-not(Test-Path -Path "tests.txt" -PathType Leaf)) {
        Write-Host "No test cases provided."
    }
    else {
        get-content "tests.txt" | trwdog dotnet run -x -shunt -tree
        $status = $LASTEXITCODE
        Write-Host "exit code $status"
        $parseOk = $status -eq 0
        if ( ! $parseOK )
        {
            Write-Host "Driver program returned non-zero exit code $exitcode."
        }
    }
    return $parseOk
}

if (Test-Case)
{
    exit 0
}
else
{
    exit 1
}
