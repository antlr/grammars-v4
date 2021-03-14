function Get-GrammarSkipList {
    param (
        $Target
    )
    switch ($Target) {
        "CSharp" {
            return @(
                "_grammar-test",
                "acme", "apex", "apt", "arithmetic", "asm/masm", "asn/asn_3gpp", "atl",
                "basic",
                "calculator", "capnproto", "cpp", "csharp", "cto",
                "dcm", "dgol", "dice",
                "erlang",
                "fortran77",
                "graphql", "gtin",
                "haskell", "html", "http", "hypertalk",
                "idl", "infosapient",
                "java/java9", "javadoc", "javascript/ecmascript", "javascript/jsx", "joss",
                "kirikiri-tjs", "kotlin/kotlin",
                "logo/logo", "logo/ucb-logo", "lpc",
                "molecule", "morsecode",
                "objc",
                "pddl", "pgn", "php", "pike", "pmmn", "powerbuilder", "python/python2", "python/python2-js",
                "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts",
                "python/python3-without-actions", "python/python3alt", "python/tiny-python",
                "rcs", "rego", "restructuredtext", "rexx", "rfc1035", "rfc1960", "rfc3080",
                "sharc", "smiles", "sql/hive", "sql/mysql", "sql/plsql", "sql/sqlite", "sql/tsql",
                "stacktrace", "stringtemplate", "swift-fin", "swift/swift2", "swift/swift3",
                "tcpheader", "terraform", "thrift",
                "unicode/unicode16",
                "v",
                "wat",
                "xpath/xpath31",
                "z")
        }
        "Java" {
            return @("")
        }
        "JavaScript" {
            return @("")
        }
        Default {
            Write-Error "Unknown target $Target"
            exit 1
        }
    }
    
}

function Test-Grammar {
    param (
        $Directory,
        $Target = "CSharp"
    )
    Write-Host "---------- Testing grammar $Directory ----------" -ForegroundColor Green
    $cwd = Get-Location
    Set-Location $Directory
    $hasTest = Test-Path "./examples"
    if ($hasTest) {
        $testDir = Resolve-Path "./examples"
    }
    
    # Do rehash
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    $start = Get-Date
    Write-Host "Building"
    Write-Host "dotnet-antlr -m true -t $Target --template-sources-directory $templates"
    Write-Host (dotnet-antlr -m true -t $Target --template-sources-directory $templates)
    if ($LASTEXITCODE -ne 0) {
        $success = $false
        $hasTest = $false
        Write-Host "Code generation failed" -ForegroundColor Red
    }
    Set-Location 'Generated'
    $success = $true
    if ( $Target -eq "CSharp") {
        Write-Host (dotnet build -o "CSharp") -Separator ([Environment]::NewLine)
        if ($LASTEXITCODE -ne 0) {
            $success = $false
            $hasTest = $false
            Write-Host "Build failed" -ForegroundColor Red
        }
#        Set-Location "CSharp"
    }
    Write-Host "Build completed, time: $((Get-Date) - $start)" -ForegroundColor Yellow

    $start2 = Get-Date
    Write-Host "Testing"
    $failedList = @()
    if ($hasTest) {
        $failedList = Test-GrammarTestCases -Target $Target -TestDirectory $testDir
        if ($failedList.Length -gt 0) {
            $success = $false
        }
    }
    Write-Host "Test completed, time: $((Get-Date) - $start2)" -ForegroundColor Yellow
    Set-Location $cwd
    return @{
        Success     = $success
        FailedCases = $failedList
    }
}

function Test-GrammarTestCases {
    param (
        $TestDirectory,
        $Target = "CSharp"
    )
    $failedList = @()
    Write-Host "Test cases here: $TestDirectory"
    foreach ($item in Get-ChildItem $TestDirectory -Recurse) {

	Write-Host "Test case: $item"

        $case = $item
        $ext = $case.Extension
        if (($ext -eq ".errors") -or ($ext -eq ".tree")) {
            continue
        }
        if (Test-Path $case -PathType Container) {
            continue
        }

        $errorFile = "$case.errors"
        $shouldFail = Test-Path $errorFile
        Write-Host "--- Testing file $item ---"
        if ( $Target -eq "CSharp") {
            dotnet "CSharp/Test.dll" -file $case
        }
        else {
            make run -file $case
        }
        $ok = $LASTEXITCODE -eq 0

        if ($shouldFail) {
            if ($ok) {
                Write-Host "Text case should return error"
                Write-Host "$Directory test $case failed" -ForegroundColor Red
                $failedList += $case
                continue
            }
        }
        elseif (!$ok) { 
            Write-Host "$Directory test $case failed" -ForegroundColor Red
            $failedList += $case
            continue
        }
    }
    return $failedList
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
    $skip = Get-GrammarSkipList -Target $Target | Resolve-Path -Relative
    $grammars = @()
    foreach ($g in $allGrammars | Resolve-Path -Relative) {
        $shouldSkip = $false
        foreach ($s in $skip) {
            if ($s -eq $g) {
                $shouldSkip = $true
            }
        }
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
    
    $grammars = Get-GrammarsNeedsTest -PreviousCommit $PreviousCommit -CurrentCommit $CurrentCommit -Target $Target
    
    Write-Host "Grammars to be tested with $Target target:"
    Write-Host "$grammars"

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

# This has to be inserted somewhere. This script requires
# the dotnet-antlr tool to instantiate drivers from templates.
$Dir = Get-Location
$templates = Join-Path $Dir "/_scripts/templates/"

$t = $args[0]
$pc = $args[1]
$cc = $args[2]

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
    Test-AllGrammars -Target $t -PreviousCommit $pc -CurrentCommit $cc
}
else {
    Test-AllGrammars -Target $t 
}
