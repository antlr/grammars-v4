# Generated from trgen <version>
$generatedDir = (Get-Location).Path
$sourceRoot = '<source_root_unix>'
$sourceExamples = Join-Path $sourceRoot '<example_dir_unix>'
$outputDir = Join-Path $generatedDir 'examples'
$parseFile = Join-Path $generatedDir 'parse.txt'
New-Item -ItemType Directory -Force -Path $outputDir | Out-Null

# Resolve Trash from the original grammar directory, where its tool manifest lives.
Push-Location -LiteralPath $sourceRoot
try {
    $relativeFiles = @(& dotnet trash glob <source_glob_args_unix>)
    if ($LASTEXITCODE -ne 0) { throw 'Could not expand example-file globs.' }
} finally {
    Pop-Location
}
$files = [System.Collections.Generic.List[string]]::new()
foreach ($relativeFile in $relativeFiles) {
    if ($relativeFile -match '\.(errors|tree|trq)$') { continue }
    $file = Join-Path $sourceRoot $relativeFile
    if (Test-Path -LiteralPath $file -PathType Leaf) { $files.Add($file) }
}
if ($files.Count -eq 0) {
    Write-Host 'No test cases provided.'
    exit 0
}

# Parse inputs; all generated artifacts stay under this generated app.
$runner = @("$generatedDir/<if(os_win)>Test.exe<else>Test<endif>")
Push-Location -LiteralPath $sourceRoot
try {
<if(individual_parsing)>
    $status = 0
    foreach ($file in $files) {
        & dotnet trash wdog @runner -q -tee -tree -o $outputDir $file *> $parseFile
        if ($LASTEXITCODE -ne 0) { $status = $LASTEXITCODE }
    }
<else>
    $files.ToArray() | & dotnet trash wdog @runner -q -x -tee -tree -o $outputDir *> $parseFile
    $status = $LASTEXITCODE
<endif>
} finally {
    Pop-Location
}
if ($status -eq 255 -or ((Test-Path -LiteralPath $parseFile) -and (Get-Item -LiteralPath $parseFile).Length -gt 0)) {
    Write-Host 'Test failed.'
    if (Test-Path -LiteralPath $parseFile) { Get-Content -LiteralPath $parseFile | Write-Host }
    exit 1
}

$failed = $false
foreach ($file in $files) {
    $relative = [IO.Path]::GetRelativePath($sourceExamples, $file)
    if ($relative -eq '..' -or $relative.StartsWith('..' + [IO.Path]::DirectorySeparatorChar)) {
        Write-Host "Input is outside the example directory: $file"
        $failed = $true
        continue
    }
    foreach ($extension in @('errors', 'tree')) {
        $expected = "$file.$extension"
        $actual = Join-Path $outputDir "${relative}.$extension"
        if ([IO.File]::Exists($expected)) {
            if (-not [IO.File]::Exists($actual)) {
                Write-Host "Missing output: $actual (expected $expected)"
                $failed = $true
            } elseif ([IO.File]::ReadAllText($expected).Replace("`r`n", "`n") -cne
                      [IO.File]::ReadAllText($actual).Replace("`r`n", "`n")) {
                Write-Host "Output differs: $actual (expected $expected)"
                $failed = $true
            }
        } elseif ($extension -eq 'errors' -and [IO.File]::Exists($actual) -and (Get-Item -LiteralPath $actual).Length -gt 0) {
            Write-Host "Unexpected parse errors for ${file}:"
            Get-Content -LiteralPath $actual | Write-Host
            $failed = $true
        }
    }
}
if ($failed) {
    Write-Host 'Test failed.'
    exit 1
}
Write-Host 'Test succeeded.'
exit 0
