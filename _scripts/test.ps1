param (
    [string]$target='CSharp',
    [string]$pc,
    [string]$cc
)

function Get-GrammarSkip {
    param (
        $Target,
        $Grammar
    )
    Write-Host "Target $Target Grammar $Grammar"
    if (-not(Test-Path "$Grammar/desc.xml" -PathType Leaf)) {
        Write-Host "$Grammar/desc.xml does not exist."
        Write-Host "skip"
        return $True
    }
    $lines = Get-Content -Path "$Grammar/desc.xml" | Select-String $Target
    if ("$lines" -eq "") {
        Write-Host "Intentionally skipping grammar $Grammar target $Target."
        return $True
    }
    $desc_targets = dotnet trxml2 -- "$Grammar/desc.xml" | Select-String '/desc/targets'
    if ($LASTEXITCODE -ne 0) {
        Write-Host "The desc.xml for $testname is malformed. Skipping."
        return $True
    }
    $desc_targets = $desc_targets -replace '.*='
    $desc_targets = $desc_targets -replace ',', ' ' -replace ';', ' '
    if ($LASTEXITCODE -ne 0) {
        Write-Host "The desc.xml for $testname is malformed. Skipping."
        return $True
    }
    $desc_targets = $desc_targets -replace '.*='
    $desc_targets = $desc_targets -replace ',', ' ' -replace ';', ' '
    $yes = $false
    foreach ($t in $desc_targets.Split(' ')) {
        if ($t -eq '+all') { $yes = $true }
        if ($t -eq "-$target") { $yes = $false }
        if ($t -eq $target) { $yes = $true }
    }
    if (! $yes) { 
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
        $Directory,
        $Target = "CSharp"
    )
    Write-Host "---------- Testing grammar $Directory ----------" -ForegroundColor Green
    $cwd = Get-Location
    Set-Location $Directory
    $zzz = Get-Location

    $failStage = [FailStage]::Success
    
    $success = $true
    $start = Get-Date
    Write-Host "Building"
    # codegen
    Write-Host "dotnet trgen -- -t $Target --template-sources-directory $templates"
    dotnet trgen -- -t $Target --template-sources-directory $templates | Write-Host
    if ($LASTEXITCODE -ne 0) {
        $failStage = [FailStage]::CodeGeneration
        Write-Host "trgen failed" -ForegroundColor Red
    }
    $dirs = Get-ChildItem -Path "Generated-$Target*"
    foreach ($m in $dirs) {
        $targetgenerated = $m.Name
        Write-Host "targetgenerated is $targetgenerated"
        Set-Location $zzz
        if (Test-Path $targetgenerated) {
            Set-Location $targetgenerated
        }
        else {
            $failStage = [FailStage]::CodeGeneration
            Write-Host "No code generated" -ForegroundColor Red
            Set-Location $cwd
            return @{
                Success     = $false
                Stage       = $failStage
                FailedCases = @()
            }
        }
        if ($failStage -ne [FailStage]::Success) {
            Set-Location $cwd
            return @{
                Success     = $false
                Stage       = $failStage
                FailedCases = @()
            }
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
$workingDirectory = Get-Location
Write-Host "The pwd is  $workingDirectory"
        ./test.ps1
        $passed = $LASTEXITCODE -eq 0

        if (! $passed) {
            $success = $false
            $failStage = [FailStage]::Test
            Write-Host "Test completed, time: $((Get-Date) - $start2)" -ForegroundColor Yellow
            Set-Location $cwd
            return @{
                Success     = $success
                Stage       = $failStage
                FailedCases = $failedList
            }
        }
    }

    Write-Host "Test completed, time: $((Get-Date) - $start)" -ForegroundColor Yellow
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
    $diff = git diff $PreviousCommit $CurrentCommit --name-only | Where-Object { $_ -notmatch "_scripts" -and $_ -notmatch "\.github" }

    if ($diff -is "string") {
        $diff = @($diff)
    }

    $dirs = @()
    foreach ($item in $diff) {
        $dirs += Join-Path "." $item
    }

    $newdirs = $dirs | Split-Path | Get-Unique
    return $newdirs
}

function Get-ChangedGrammars {
    param (
        $PreviousCommit,
        $CurrentCommit = "HEAD"
    )
    $prefix = Get-Location
    $diff = Get-GitChangedDirectories $PreviousCommit $CurrentCommit
    $grammars = Get-Grammars | Resolve-Path -Relative
    $changed = @()
    foreach ($d in $diff) {
        $old = Get-Location
        if (!(Test-Path -Path "$d")) {
            continue
        }
        Set-Location $d
        while ($True) {
            if (Test-Path -Path "desc.xml" -PathType Leaf) {
                break
            }
            $cwd = Get-Location
            if ("$cwd" -eq "$prefix") {
                break
            }
            $newloc = Get-Location | Split-Path
            Set-Location "$newloc"
        }
        # g=${g##*$prefix/} not needed.
        if (! (Test-Path -Path "desc.xml" -PathType Leaf)) {
            Write-Host "No desc.xml for $d"
            Set-Location "$old"
            continue
        }
        $desc_targets = dotnet trxml2 -- desc.xml | Select-String '/desc/targets'
        if ($LASTEXITCODE -ne 0) {
            Write-Host "The desc.xml for $testname is malformed. Skipping."
            Set-Location "$old"
            continue
        }
        $desc_targets = $desc_targets -replace '.*='
        $desc_targets = $desc_targets -replace ',', ' ' -replace ';', ' '
        $yes = $false
        foreach ($t in $desc_targets.Split(' ')) {
            if ($t -eq '+all') { $yes = $true }
            if ($t -eq "-$target") { $yes = $false }
            if ($t -eq $target) { $yes = $true }
        }
        if (! $yes) { 
            Set-Location "$old"
            continue
        }
        $g = Get-Location
        Set-Location "$old"
        $pattern = "$prefix" -replace "\\","/"
        $g = $g -replace "\\","/"
        $g = $g -replace "$pattern/",""
        $g = "./" + $g
        Write-Host "Adding diff $g"
        $changed += $g
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
        $Target = "CSharp"
    )
    
Write-Host "target = $target"
Write-Host "previouscommit = $PreviousCommit"
Write-Host "CurrentCommit = $CurrentCommit"

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
        $state = Test-Grammar -Directory $g -Target $Target
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

##########################################################
##########################################################
#
# MAIN
#
##########################################################
##########################################################

$rootdir = $PSScriptRoot

# This has to be inserted somewhere. This script requires
# the trgen tool to instantiate drivers from templates.
$templates = Join-Path $rootdir "/templates/"
Write-Host "templates dir = $templates"

$diff = $true
if (!$target) {
    $target = "CSharp"
}
if (!$pc) {
    $diff = $false
} else {
    if (!$cc) {
        $cc = "HEAD"
    }
}
Write-Host "target = $target"
Write-Host "pc = $pc"
Write-Host "cc = $cc"

if ($diff) {
    Test-AllGrammars -Target $target -PreviousCommit $pc -CurrentCommit $cc
}
else {
    Test-AllGrammars -Target $target
}
