# Template generated code from trgen <version>

$TestDirectory = "<if(os_win)>../<example_files_win><else>../<example_files_unix><endif>"
Write-Host "Test cases here: $TestDirectory"
if (Test-Path -Path "tests.txt" -PathType Leaf) {
    Remove-Item "tests.txt"
}
$files = New-Object System.Collections.Generic.List[string]
$new_errors_txt = New-Object System.Collections.Generic.List[string]

foreach ($item in Get-ChildItem $TestDirectory -Recurse) {
    $file = $item.fullname
    $ext = $item.Extension
    if (Test-Path $file -PathType Container) {
        continue
    } elseif ($ext -eq ".errors") {
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
    exit 1
}

# Parse
$(& get-content "tests.txt" | trwdog java -cp "<antlr_tool_path><if(path_sep_semi)>;<else>:<endif>." Test -x -tee -tree ; $status = $LASTEXITCODE ) | Write-Host

# trwdog returns 255 if it cannot spawn the process.
if ( $status -eq 255 ) {
    Write-Host "Test failed."
    exit 1
}

# Check if any .errors/.tree files have changed. That's not good.
$message = git diff --exit-code --name-only $TestDirectory
$updated = $LASTEXITCODE
Write-Host $message

# Check if any untracked .errors files are not empty. That's also not good.
$new_errors2_txt = git ls-files --exclude-standard -o $TestDirectory
$new_errors = $LASTEXITCODE
if ( ! [String]::IsNullOrWhiteSpace($new_errors2_txt) ) {
    $new_errors3_txt = $new_errors2_txt.Split("\n\r\t ")
} else {
    $new_errors3_txt = [System.Collections.Arraylist]@()
}

foreach ($s in $new_errors3_txt) {
    if ( [String]::IsNullOrWhiteSpace($s) ) {
        continue
    }
    $ext = $item.Extension
    if (! $s.EndsWith(".errors")) {
        continue
    }
    $file = $s
    $size = (Get-Item -Path $file).Length
    if ( $size -eq 0 ) {
    } else {
        $new_errors_txt.Add($item)
        ((Get-Content $file) -join "`n") + "`n" | Set-Content -NoNewline $file
    }
}

if ( $updated -eq 129 ) {
    Write-Host "Grammar outside a git repository. Assuming parse exit code."
    if ( $status -eq 0 ) {
        Write-Host "Test succeeded."
    } else {
        Write-Host "Test failed."
    }
    $err = $status
    exit 1
}

if ( $updated -eq 1 ) {
    Write-Host "Difference in output."
    Write-Host "Test failed."
    exit 1
}

if ( $new_errors_txt.Count() > 0 ) {
    Write-Host "New errors in output."
    Write-Host "Test failed."
    exit 1
}

if ( $status -eq 0 ) {
    Write-Host "Test succeeded."
    exit 0
} else {
    Write-Host "Test failed."
    exit 1
}
