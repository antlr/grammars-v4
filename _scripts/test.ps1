function Get-GrammarSkip {
    param (
        $Target,
        $Grammar
    )
    Write-Host "Target $Target Grammar $Grammar"
    if (-not(Test-Path "$Grammar/desc.xml" -PathType Leaf)) {
        Write-Host "skip"
        return $True
    }
    $lines = Get-Content -Path "$Grammar/desc.xml" | Select-String $Target
    if ("$lines" -eq "") {
        Write-Host "skip"
        return $True
    }
    return $False
}

enum FailStage {
    Success
    CodeGeneration
    Compile
    Test
}

function Test-Grammar {
    param (
        $Antlrjar,
        $Directory,
        $Target = "CSharp"
    )
    Write-Host "---------- Testing grammar $Directory ----------" -ForegroundColor Green
    $cwd = Get-Location
    Set-Location $Directory

    $failStage = [FailStage]::Success
    
    $success = $true
    $start = Get-Date
    Write-Host "Building"
    # codegen
    Write-Host "trgen --antlr-tool-path $Antlrjar -t $Target --template-sources-directory $templates"
    trgen --antlr-tool-path $Antlrjar -t $Target --template-sources-directory $templates | Write-Host
    if ($LASTEXITCODE -ne 0) {
        $failStage = [FailStage]::CodeGeneration
        Write-Host "trgen failed" -ForegroundColor Red
    }
    $targetgenerated = "Generated-$Target"
    if (Test-Path $targetgenerated) {
        Set-Location $targetgenerated
    }
    else {
        $failStage = [FailStage]::CodeGeneration
        Write-Host "No code generated" -ForegroundColor Red
    }
    if ($failStage -ne [FailStage]::Success) {
        Set-Location $cwd
        return @{
            Success     = $false
            Stage       = $failStage
            FailedCases = @()
        }
    }
    $hasTransform = Test-Path transformGrammar.py
    if ($hasTransform) {
        python3 transformGrammar.py
    }

    # build

    # see _scripts/templates/*/tester.psm1
    ./build.ps1
    $buildResult = $LASTEXITCODE

    if ($buildResult -ne 0) {
        $failStage = [FailStage]::Compile
        Write-Host "Build failed" -ForegroundColor Red
    }

    Write-Host "Build completed, time: $((Get-Date) - $start)" -ForegroundColor Yellow
    if ($failStage -ne [FailStage]::Success) {
        Set-Location $cwd
        return @{
            Success     = $false
            Stage       = $failStage
            FailedCases = @()
        }
    }

    # test
    $start2 = Get-Date
    Write-Host "--- Testing files ---"
    ./test.ps1
    $passed = $LASTEXITCODE -eq 0

    if (! $passed) {
        $success = $false
        $failStage = [FailStage]::Test
    }

    Write-Host "Test completed, time: $((Get-Date) - $start2)" -ForegroundColor Yellow
    Set-Location $cwd
    return @{
        Success     = $success
        Stage       = $failStage
        FailedCases = $failedList
    }
}

function Get-Grammars {
    param (
        $Directory = ""
    )

    if (!$Directory) {
        $Directory = Get-Location
    }

    $pom = Join-Path $Directory "pom.xml"
    if (! (Test-Path $pom)) {
        return @()
    }
    
    [xml]$xml = Get-Content -Path $pom

    $module = $xml.project.modules.module
    if (!$module) {
        $module = $xml.project.profiles.profile.modules.module
    }

    if ($module) {
        $ret = @()
        foreach ($m in $module) {
            $s = Join-Path $Directory $m
            if (!(Test-Path $s)) {
                continue
            }
            $grammars = Get-Grammars $s
            foreach ($g in $grammars) {
                $ret += $g
            }
        }
        return $ret
    }
    if ($xml.project.build.plugins.plugin) {
        $conf = $xml.project.build.plugins | Where-Object { $_.plugin.groupId -eq "org.antlr" }
        if ($conf) {
            return @($Directory)
        }
        return @()
    }
}

function Get-GitChangedDirectories {
    param (
        $PreviousCommit,
        $CurrentCommit = "HEAD"
    )
    if (!$PreviousCommit) {
        Write-Error "which commit to diff?"
        exit 1
    }
    $diff = git diff $PreviousCommit $CurrentCommit --name-only
    if ($diff -is "string") {
        $diff = @($diff)
    }
    $treatAsRootDir = @(".github/", "_scripts/")
    foreach ($item in $diff) {
        foreach ($j in $treatAsRootDir) {
            if ($item.StartsWith($j)) {
                $diff += "README.md"
                break;
            }
        }
    }

    $dirs = @()
    foreach ($item in $diff) {
        $dirs += Join-Path "." $item
    }
    
    return $dirs | Split-Path | Get-Unique
}

function Get-ChangedGrammars {
    param (
        $PreviousCommit,
        $CurrentCommit = "HEAD"
    )
    $diff = Get-GitChangedDirectories $PreviousCommit $CurrentCommit
    $grammars = Get-Grammars | Resolve-Path -Relative
    $changed = @()
    foreach ($g in $grammars) {
        foreach ($d in $diff) {
            if ($d.StartsWith($g) -or $g.StartsWith($d)) {
                $changed += $g
            } 
        }
    }
    return $changed | Get-Unique
}

function Get-GrammarsNeedsTest {
    param (
        $PreviousCommit,
        $CurrentCommit = "HEAD",
        $Target = "CSharp"
    )
    if (!$PreviousCommit) {
        $allGrammars = Get-Grammars
    }
    else {
        $allGrammars = Get-ChangedGrammars -PreviousCommit $PreviousCommit -CurrentCommit $CurrentCommit
    }
    $grammars = @()
    foreach ($g in $allGrammars | Resolve-Path -Relative) {
        $shouldSkip = Get-GrammarSkip -Target $Target -Grammar $g
        if (!$shouldSkip) {
            $grammars += $g
        }
    }
    
    return $grammars
}

function Test-AllGrammars {
    param (
        $PreviousCommit,
        $CurrentCommit = "HEAD",
        $Target = "CSharp",
        $Antlrjar = "/tmp/antlr4-complete.jar"
    )
    
    $grammars = Get-GrammarsNeedsTest -PreviousCommit $PreviousCommit -CurrentCommit $CurrentCommit -Target $Target
    
    Write-Host "Grammars to be tested with $Target target:"
    Write-Host "$grammars"
    # Try adding to path for Ubuntu.
    # Write-Host "PATH is $env:PATH"
    # $env:PATH += ":/home/runner/.dotnet/tools"
    # Write-Host "PATH after update is $env:PATH"

    $success = $true
    $t = Get-Date
    $failedGrammars = @()
    $failedCases = @()
    foreach ($g in $grammars) {
        $state = Test-Grammar -Antlrjar $Antlrjar -Directory $g -Target $Target
        if (!$state.Success) {
            $success = $false
            $failedGrammars += $g
            $failedCases += $state.FailedCases
            Write-Host "$g failed" -ForegroundColor Red
        }        
    }
    Write-Host "finished in $((Get-Date)-$t)" -ForegroundColor Yellow
    if ($success) {
        exit 0
    }
    else {
        Write-Error "Failed grammars: $([String]::Join(' ', $failedGrammars))"
        Write-Error "Failed cases: $([String]::Join(' ', $failedCases))"
        exit 1
    }
}

# Setup ANTLR
if ($env:ANTLR_JAR_PATH) {
    $sep = ':';
    if ($IsWindows) {
        $sep = ';'
    }
    $cps = ('.', $env:ANTLR_JAR_PATH)
    if ($env:CLASSPATH) {
        $cps += $env:CLASSPATH
    }
    $env:CLASSPATH = [String]::Join($sep, $cps)
    function global:antlr {
        java -jar $env:ANTLR_JAR_PATH $args
    }
}

# This has to be inserted somewhere. This script requires
# the trgen tool to instantiate drivers from templates.
$Dir = Get-Location
$templates = Join-Path $Dir "/_scripts/templates/"

$antlrjar = $args[0]
$t = $args[1]
$pc = $args[2]
$cc = $args[3]

$diff = $true
if (!$t) {
    $t = "CSharp"
}
if (!$pc) {
    $diff = $false
}
if (!$cc) {
    $cc = "HEAD"
}
if ($diff) {
    Test-AllGrammars -Target $t -PreviousCommit $pc -CurrentCommit $cc -Antlrjar $antlrjar
}
else {
    Test-AllGrammars -Target $t -Antlrjar $antlrjar
}
