# Generated from trgen <version>

$workingDirectory = Get-Location
$filePath = "$workingDirectory/tests.txt"

$Tests = "<if(os_win)>../<example_files_win><else>../<example_files_unix><endif>"
Write-Host "Test cases here: $Tests"

# Get a list of test files from the test directory. Do not include any
# .errors or .tree files. Pay close attention to remove only file names
# that end with the suffix .errors or .tree.
if (Test-Path -Path "$filePath" -PathType Leaf) {
    Remove-Item "$filePath"
}

$files = New-Object System.Collections.Generic.List[string]
$allFiles = $(& dotnet trglob "$Tests" ; $last = $LASTEXITCODE )
foreach ($file in $allFiles) {
    $ext = $file | Split-Path -Extension
    if (Test-Path $file -PathType Container) {
        continue
    } elseif ($ext -eq ".errors") {
        continue
    } elseif ($ext -eq ".tree") {
        continue
    } elseif ($ext -eq ".trq") {
        continue
    } else {
        $files.Add($file)
        Write-Host "Test case: $file"
    }
}

$writer = New-Object System.IO.StreamWriter($filePath, $true) # $true for append
foreach ($file in $files) {
    $writer.WriteLine($file)
}
$writer.Dispose()

if (-not(Test-Path -Path "$filePath" -PathType Leaf)) {
    Write-Host "No test cases provided."
    exit 0
}
$size = (Get-Item -Path "$filePath").Length
if ( $size -eq 0 ) {
    Write-Host "Test cases file empty."
    exit 0
}

# Parse all input files.
<if(individual_parsing)>
# Individual parsing.
Get-Content "$filePath" | ForEach-Object { dotnet trwdog ./bin/Debug/net8.0/<if(os_win)>Test.exe<else>Test<endif> -q -tee -tree $_ 
$status = $LASTEXITCODE
Write-Host "" > parse.txt
Get-Content "../teststuff/test_delete.c.p" | Write-Host
<else>
# Group parsing.
get-content "$filePath" | dotnet trwdog ./bin/Debug/net8.0/<if(os_win)>Test.exe<else>Test<endif> -q -x -tee -tree
$status = $LASTEXITCODE
Write-Host "" > parse.txt
Get-Content "../teststuff/test_delete.c.p" | Write-Host
<endif>

# trwdog returns 255 if it cannot spawn the process. This could happen
# if the environment for running the program does not exist, or the
# program did not build.
if ( $status -eq 255 ) {
    Write-Host "Test failed."
    Get-Content $file | Write-Host
    exit 1
}

# Any parse errors will be put in .errors files. But, if there's any
# output from the program in stdout or stderr, it's all bad news.
$size = (Get-Item -Path "parse.txt").Length
if ( $size -eq 0 ) {
} else {
    Write-Host "Test failed."
    Get-Content "parse.txt" | Write-Host
    exit 1
}

# Validate parse trees via trquery assertions.
# Execute trquery parse tree validation.
Write-Host "Checking any trquery parse tree assertions..."
$assertions_err = 0
foreach ($file in $files) {
    $trq = "$file.trq"
    if (Test-Path $trq -PathType Leaf) {
        Write-Host "Assert test case: $trq"
        dotnet trparse $file | dotnet trquery -c $trq
        $xxx = $LASTEXITCODE
        if ( $xxx -ne 0 ) {
            $assertions_err = $xxx
        }
    }
}
Write-Host "Finished checking parse tree assertions."

$old = Get-Location
Set-Location "<if(os_win)>../<example_dir_win><else>../<example_dir_unix><endif>"

# Check if any .errors/.tree files have changed. That's not good.
git config --global pager.diff false
Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
$updated = 0
foreach ($item in Get-ChildItem . -Recurse) {
    $file = $item.fullname
    $ext = $item.Extension
    if ($ext -eq ".errors") {
        git diff --exit-code $file *>> $old/updated.txt
        $st = $LASTEXITCODE
        if ($st -ne 0) {
            $updated = $st
        }
    }
}
foreach ($item in Get-ChildItem . -Recurse) {
    $file = $item.fullname
    $ext = $item.Extension
    if ($ext -eq ".tree") {
        [IO.File]::WriteAllText($file, $([IO.File]::ReadAllText($file) -replace "`r`n", "`n"))
        git diff --exit-code $file *>> $old/updated.txt
        $st = $LASTEXITCODE
        if ($st -ne 0) {
            $updated = $st
        }
    }
}

# Check if any untracked .errors files are not empty.
$new_errors_txt = New-Object System.Collections.Generic.List[string]
$new_errors2_txt = git ls-files --exclude-standard -o $TestDirectory
$new_errors = $LASTEXITCODE

# Gather up all untracked .errors file output. These are new errors
# and must be reported as a parse fail.
if ( ! [String]::IsNullOrWhiteSpace($new_errors2_txt) ) {
    $new_errors3_txt = $new_errors2_txt.Split("\n\r\t ")
} else {
    $new_errors3_txt = [System.Collections.Arraylist]@()
}
if (Test-Path -Path "$old/new_errors.txt" -PathType Leaf) {
    Remove-Item "$old/new_errors.txt"
}
New-Item -Path "$old" -Name "new_errors.txt" -ItemType "file" -Value "" | Out-Null
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
        Add-Content -Path "$old/new_errors.txt" -Value "$item"
        ((Get-Content $file) -join "`n") + "`n" | Add-Content -Path "$old/new_errors.txt"
    }
}

# If "git diff" reported an exit code of 129, it is because
# the directory containing the grammar is not in a repo. In this
# case, assume parse error code as the defacto result.
if ( $updated -eq 129 ) {
    Write-Host "Grammar outside a git repository. Assuming parse exit code."
    if ( $status -eq 0 ) {
        Write-Host "Test succeeded."
    } else {
        Get-Content "$old/new_errors.txt" | Write-Host
        Write-Host "Test failed."
    }
    Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors2.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors.txt -errorAction ignore 2>&1 | Out-Null
    $err = $status
    exit 1
}

# "Git diff" reported a difference. Redo the "git diff" to print out all
# the differences. Also, output any untracked, non-zero length .errors files.
if ( $updated -eq 1 ) {
    Write-Host "Difference in output."
    git diff . | Write-Host
    Get-Content "$old/new_errors.txt" | Write-Host
    Write-Host "Test failed."
    Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors2.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors.txt -errorAction ignore 2>&1 | Out-Null
    exit 1
}

# If there's non-zero length .errors flies that are new, report them
# as errors in the parse.
if ( $new_errors_txt.Count -gt 0 ) {
    Write-Host "New errors in output."
    Get-Content "$old/new_errors.txt" | Write-Host
    Write-Host "Test failed."
    Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors2.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors.txt -errorAction ignore 2>&1 | Out-Null
    exit 1
}

# Test assertions errors.
if ( "$assertions_err" -ne 0 ) {
    Write-Host "Test failed."
    Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors2.txt -errorAction ignore 2>&1 | Out-Null
    Remove-Item -Force -Path $old/new_errors.txt -errorAction ignore 2>&1 | Out-Null
    exit 1
}

Write-Host "Test succeeded."
Remove-Item -Force -Path $old/updated.txt -errorAction ignore 2>&1 | Out-Null
Remove-Item -Force -Path $old/new_errors2.txt -errorAction ignore 2>&1 | Out-Null
Remove-Item -Force -Path $old/new_errors.txt -errorAction ignore 2>&1 | Out-Null
exit 0
