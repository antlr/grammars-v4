function Test-Grammar {
    param (
        $Directory,
        $Target = "CSharp"
    )

    $cwd = Get-Location
    Set-Location $Directory
    dotnet-antlr -m -t $Target

    #$Generated = Join-Path $Directory 'Generated'
    #if (!(Test-Path $Generated)) {
    #    Write-Error "${Directory} is not a valid directory"
    #    return false
    #}
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
    if(!$module){
        $module = $xml.project.profiles.profile.modules.module
    }

    if($module){
        $ret = @()
        foreach ($m in $module) {
            $s = Join-Path $Directory $m
            if(!(Test-Path $s)){
                continue
            }
            $grammars = Get-Grammars $s
            foreach ($g in $grammars) {
                $ret += $g
            }
        }
        return $ret
    }
    if($xml.project.build.plugins.plugin){
        $conf = $xml.project.build.plugins | where-object -FilterScript {$_.plugin.groupId -eq "org.antlr"}
        if($conf){
            return @($Directory)
        }
        return @()
    }
}

Get-Grammars | Resolve-Path -Relative

Test-Grammar "./rust"