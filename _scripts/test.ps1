function Test-Grammar {
    # TODO: fix it
    param (
        $Directory,
        $Target = "CSharp"
    )

    $cwd = Get-Location
    Set-Location $Directory
    dotnet-antlr -m -t $Target

    Set-Location 'Generated'

    $start = Get-Date
    if ( $Target -eq "CSharp") {
        Write-Output "" | dotnet run
    }
    else {
        make test
    }
    $ok = $LASTEXITCODE -eq 0
    $duration = (Get-Date) - $start
    Write-Debug "$Directory Duration: $duration"
    Set-Location $cwd
    return $ok
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
        $conf = $xml.project.build.plugins | where-object -FilterScript { $_.plugin.groupId -eq "org.antlr" }
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
            if ($g.StartsWith($d)) {
                $changed += $g
            } 
        }
    }
    return $changed | Get-Unique
}

Get-ChangedGrammars a7cff
