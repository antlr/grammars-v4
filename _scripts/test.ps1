function Get-GrammarSkipList {
    param (
        $Target
    )
    switch ($Target) {
        "CSharp" {
            return @(
                "_grammar-test",
                "apex", "arithmetic", "asm/masm", "asn/asn_3gpp",
                "bcpl",
                "cpp", "csharp", "dice",
                "fortran77",
                "haskell", "html", "hypertalk",
                "idl",
                "java/java9", "javadoc", "javascript/ecmascript", "javascript/jsx",
                "kirikiri-tjs", "kotlin/kotlin",
                "logo/ucb-logo", "lpc",
                "objc",
                "pgn", "php", "pike", "powerbuilder", "python/python2", "python/python2-js",
                "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts",
                "python/python3-without-actions", "python/python3alt", "python/tiny-python",
                "rego", "rexx",
                "sql/hive", "sql/plsql", "sql/sqlite", "sql/tsql",
                "stringtemplate", "swift-fin", "swift/swift2", "swift/swift3",
                "tcpheader", "thrift",
                "unicode/unicode16",
                "v",
                "wat",
                "xpath/xpath31",
                "z")
        }
        "Java" {
            return @(
                "antlr/antlr4", "arithmetic", "asm/masm", "bcpl", "cobol85", "csharp", "dice",
                "html", "hypertalk", "javascript/ecmascript", "kotlin/kotlin", "kotlin/kotlin-formal", "lambda", "less", "metric", "molecule",
                "p", "php", "powerbuilder",
                "python/python2", "python/python3-js", "python/python3-py", "python/python3-ts", "python/python3-without-actions", "python/tiny-python", 
                "scss", "sql/hive", "sql/plsql", "sql/tsql", "stringtemplate", "swift/swift2", "swift-fin", "tcpheader",
                "wat"
            )
        }
        "JavaScript" {
            return @(
                "antlr/antlr2", "antlr/antlr4", "apex", "arithmetic", "asm/masm", "asn/asn_3gpp", "basic", "bcpl", "c", "cql3", "csharp", "dice", "fortran77", "golang",
                "haskell", "html", "hypertalk", "idl", "java/java", "java/java9", "javadoc", "javascript/ecmascript", "javascript/javascript", "javascript/jsx", "javascript/typescript",
                "kotlin/kotlin", "lambda", "less", "logo/ucb-logo", "lua",
                "matlab", "metric", "molecule", "p", "pcre", "pgn", "php", "pike", "powerbuilder", "promql",
                "python/python2", "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts", "python/python3-without-actions", "python/tiny-python"
                "rego", "rexx", "ruby", "rust",
                "sql/hive", "sql/plsql", "sql/tsql", "stringtemplate", "swift/swift2", "swift/swift3", "swift-fin", "tcpheader", "thrift"
                "v", "wat", "xpath/xpath31", "z")
        }
        "Go" {
            return @(
                "abb", "algol60",
                "antlr/antlr2", "antlr/antlr3", "antlr/antlr4",
                "apex", "arithmetic",
                "asm/asm6502", "asm/asm8080", "asm/asm8086", "asm/asmMASM", "asm/asmZ80", "asm/masm", "asm/pdp7",
                "asn/asn_3gpp",
                "basic", "bcpl",
                "calculator", "capnproto", "clojure", "cobol85", "cpp", "cql3", "csharp", "css3",
                "dice",
                "edif300", "edn", "erlang",
                "flatbuffers", "focal", "fortran77",
                "gff3", "golang", "guitartab",
                "haskell", "html", "hypertalk",
                "icalendar", "inf", "informix",
                "java/java", "java/java8", "java/java9", "javadoc", "javascript/ecmascript", "javascript/javascript", "javascript/jsx", "javascript/typescript", "joss",
                "kirikiri-tjs", "kotlin/kotlin", "kotlin/kotlin-formal", "kuka",
                "lambda", "logo/logo", "logo/ucb-logo", "lolcode", "lpc", "lua",
                "mckeeman-form", "metric", "modula2pim4", "molecule", "moo", "muddb",
                "oberon", "oncrpc",
                "p", "pascal", "pddl", "pdn", "pgn", "php", "ply", "powerbuilder", "protobuf3",
                "python/python2", "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts",
                "python/python3-without-actions", "python/python3alt", "python/tiny-python",
                "quakemap",
                "r", "rego", "rexx", "rust",
                "scala", "scss", "sgf", "sieve", "smalltalk", "smtlibv2", "sparql",
                "sql/hive", "sql/plsql", "sql/sqlite", "sql/tsql",
                "stringtemplate", "swift/swift2", "swift/swift3", "swift-fin",
                "tcpheader", "terraform", "thrift", "tinymud", "toml", "trac", "ttm", "turing", "turtle", "turtle-doc",
                "url",
                "v", "vb6", "vba", "verilog/verilog",
                "wat", "webidl",
                "xpath/xpath1", "xpath/xpath31",
                "z")
        }
        "Python3" {
            return @("abb", "algol60", "antlr/antlr2", "antlr/antlr3", "antlr/antlr4", "apex", "arithmetic",
                "asm/asmMASM", "asm/masm", "asn/asn", "asn/asn_3gpp",
                "bcpl", "bnf", "brainfuck", "brainflak",
                "capnproto", "clojure", "cmake", "cpp", "cql3", "csharp", "css3",
                "dice", "dot",
                "edif300", "edn", "erlang",
                "flatbuffers", "focal", "fortran77",
                "gff3", "gml", "golang",
                "haskell", "html", "hypertalk",
                "icalendar", "idl", "informix", "infosapient",
                "java/java9", "javadoc", "javascript/ecmascript", "javascript/javascript", "javascript/jsx", "javascript/typescript", "joss",
                "kotlin/kotlin", "kotlin/kotlin-formal", "kuka",
                "lambda", "less", "logo/logo", "logo/ucb-logo", "lolcode",
                "mckeeman-form", "mdx", "metric", "microc", "modula2pim4", "molecule", "moo", "muddb", "mumath", "oberon",
                "p", "parkingsign", "pascal", "pddl", "peoplecode", "pgn", "php", "pl0", "ply", "powerbuilder", "protobuf3",
                "python/python2", "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts", "python/python3-without-actions",
                "python/python3alt", "python/tiny-python",
                "quakemap",
                "redcode", "rego", "rexx", "rfc1960", "ruby", "rust",
                "scala", "scss", "sexpression", "sgf", "smalltalk", "sparql", "sql/hive", "sql/plsql", "sql/sqlite", "sql/tsql",
                "stellaris", "stringtemplate", "swift/swift2", "swift/swift3", "swift-fin",
                "tcpheader", "terraform", "tinyc", "tinymud", "toml", "turing", "turtle", "turtle-doc",
                "v", "vb6", "vba",
                "wat", "wavefront", "webidl",
                "xpath/xpath31",
                "z")
        }
        "Dart" {
            return @("antlr/antlr2", "antlr/antlr3", "antlr/antlr4", "apex", "arithmetic", "asm/masm", "asn/asn_3gpp", "atl",
                "bcpl", "bnf",
                "clif", "clojure", "creole", "csharp", "css3",
                "dart2", "dice",
                "edif300", "edn", "erlang",
                "focal", "fortran77", "fusion-tables",
                "gff3", "golang",
                "haskell", "html", "hypertalk",
                "icalendar", "idl", "infosapient",
                "java/java9", "javadoc", "javascript/ecmascript", "javascript/javascript", "javascript/jsx", "javascript/typescript", 
                "kirikiri-tjs", "kotlin/kotlin", "kotlin/kotlin-formal",
                "lambda", "logo/logo", "logo/ucb-logo", "lpc",
                "metric", "modula2pim4", "molecule",
                "oberon",
                "pascal", "pdn", "pgn", "php", "powerbuilder", "prolog", "promql", "protobuf3",
                "python/python2", "python/python3", "python/python3-js", "python/python3-py", "python/python3-ts",
                "python/python3-without-actions", "python/python3alt", "python/tiny-python", "quakemap",
                "rego", "restructuredtext", "rexx", "ruby", "rust",
                "sql/hive", "sql/plsql", "sql/sqlite", "sql/tsql", "stringtemplate", "swift/swift2", "swift/swift3", "swift-fin",
                "tcpheader", "terraform", "thrift", "tinymud",
                "v", "vb6",
                "wat", "webidl",
                "xpath/xpath31",
                "z")
        }
        Default {
            #    Write-Error "Unknown target $Target"
            #    exit 1
        }
    }
    
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

    $failStage = [FailStage]::Success
    $hasTest = Test-Path "./examples"
    if ($hasTest) {
        $testDir = Resolve-Path "./examples"
    }
    
    $success = $true
    $start = Get-Date
    Write-Host "Building"
    # codegen
    Write-Host "dotnet-antlr -m true -t $Target --template-sources-directory $templates"
    dotnet-antlr -m true -t $Target --template-sources-directory $templates | Write-Host
    if ($LASTEXITCODE -ne 0) {
        $failStage = [FailStage]::CodeGeneration
        Write-Host "dotnet-antlr failed" -ForegroundColor Red
    }
    if (Test-Path 'Generated') {
        Set-Location 'Generated'
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

    # build

    # see _scripts/templates/*/tester.psm1
    Import-Module ./tester.psm1

    $buildResult = Build-Grammar

    if (!$buildResult.Success) {
        $failStage = [FailStage]::Compile
        Write-Host "Build failed" -ForegroundColor Red
        Write-Host $buildResult.Message
    }

    Write-Host "Build completed, time: $((Get-Date) - $start)" -ForegroundColor Yellow
    if ($failStage -ne [FailStage]::Success) {
        Remove-Module tester
        Set-Location $cwd
        return @{
            Success     = $false
            Stage       = $failStage
            FailedCases = @()
        }
    }

    # test
    $start2 = Get-Date
    Write-Host "Testing"
    $failedList = @()
    if ($hasTest) {
        $failedList = Test-GrammarTestCases -TestDirectory $testDir
        if ($failedList.Length -gt 0) {
            $success = $false
            $failStage = [FailStage]::Test
        }
    }

    Write-Host "Test completed, time: $((Get-Date) - $start2)" -ForegroundColor Yellow
    Remove-Module tester
    Set-Location $cwd
    return @{
        Success     = $success
        Stage       = $failStage
        FailedCases = $failedList
    }
}

function Test-GrammarTestCases {
    param (
        $TestDirectory
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
        if (!$shouldFail) {
            $errorFile = ""
        }
        Write-Host "--- Testing file $item ---"
        $ok = Test-Case -InputFile $case -ErrorFile $errorFile

        if (!$ok) {
            if ($shouldFail) {
                Write-Host "Text case should return error"
                Write-Host "$Directory test $case failed" -ForegroundColor Red
                $failedList += $case
                continue
            }
            else { 
                Write-Host "$Directory test $case failed" -ForegroundColor Red
                $failedList += $case
                continue
            }
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
