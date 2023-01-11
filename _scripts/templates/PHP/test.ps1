# Template generated code from trgen <version>
function Test-Case {

    # There's no "find" and "grep" per se in Powershell, but functions that offer
    # similar behavior.

    $TestDirectory = "<if(os_win)>../<example_files_win><else>../<example_files_unix><endif>"
    Write-Host "Test cases here: $TestDirectory"
    if (Test-Path -Path "tests.txt" -PathType Leaf) {
        Remove-Item "tests.txt"
    }
    $files = New-Object System.Collections.Generic.List[string]
    $before_parse_errors = New-Object System.Collections.Generic.List[string]
    $after_parse_errors = New-Object System.Collections.Generic.List[string]

    foreach ($item in Get-ChildItem $TestDirectory -Recurse) {
        $file = $item.fullname
        $ext = $item.Extension
        if (Test-Path $file -PathType Container) {
            continue
        } elseif ($ext -eq ".errors") {
                        $text = Get-content $file
                        if ( [String]::IsNullOrWhiteSpace($text)) {
                        } else {
                                $before_parse_errors.Add($item)
                        }
            continue
        } elseif ($ext -eq ".tree") {
            continue
        } else {
            $files.Add($item)
            Write-Host "Test case: $item"
        }
    }
    foreach ($file in $files) {
        Add-Content "tests.txt" $file
    }
    if (-not(Test-Path -Path "tests.txt" -PathType Leaf)) {
        Write-Host "No test cases provided."
        $err = 0
    } else {

        # Parse
        $(& get-content "tests.txt" | trwdog php -d memory_limit=1G Test.php -x -shunt -tree ; $status = $LASTEXITCODE ) | Write-Host

        Write-Host "exit code $status"

        foreach ($item in Get-ChildItem $TestDirectory -Recurse) {
            $file = $item.fullname
            $ext = $item.Extension
            if ($ext -eq ".errors") {
                $text = Get-content $file
                if ( [String]::IsNullOrWhiteSpace($text)) {
                                } else {
                    $after_parse_errors.Add($item)
                    ((Get-Content $file) -join "`n") + "`n" | Set-Content -NoNewline $file
                }
                continue
            }
        }

        $message = git diff --exit-code --name-only .
        $diffs = $LASTEXITCODE
                Write-Host "git exited $diffs"

                Write-Host "Before $before_parse_errors"
                Write-Host "After $after_parse_errors"
                $b = -join $before_parse_errors.ToArray()
                $a = -join $before_parse_errors.ToArray()

        if ( $diffs -eq 129 ) {
            Write-Host "Grammar outside a git repository."
            Write-Host "Defaulting to exit code of group parse."
            $err = $status
        } elseif ( $diffs -eq 1 ) {
            Write-Host $message
            Write-Host "Difference in output."
            $err = 1
        } elseif ( $a -ne $b ) {
            foreach ($item in $after_parse_errors) {
                $text = Get-Content $file
                Write-Host $text
            }
            Write-Host "Difference in output."
            $err = 1
        } else {
            $err = 0
            Write-Host "OK. $err"
        }
    }
    Write-Host "err $err"
    return $err
}

$ret = Test-Case
Write-Host "ret $ret"
if ($ret -eq 0)
{
    Write-Host "Test succeeded."
    exit 0
}
else
{
    Write-Host "Test failed."
    exit 1
}
