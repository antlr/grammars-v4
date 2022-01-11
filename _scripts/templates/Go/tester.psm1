# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    # Output pwsh (or powershell) version.
    # Attempt 1. Does not work--captured.
    # Get-Host | Select-Object Version
    # Attempt 2.
    # Get-Host | Select-Object Version | Out-Host
    # Attempt 3.
    # Get-Host | Select-Object Version | Out-Host | Write-Host
    # Attempt 4.
    $gm = Get-Host | Select-Object Version
    $g = -join($g, $gm)
    # Output environmental variables.
    $gm = dir env: | Out-String
    $g = -join($g, $gm)
    # Output go version
    $gm = go version | Out-String
    $g = -join($g, $gm)
    #
    $env:GO111MODULE = "on"
    $gm = go get github.com/antlr/antlr4/runtime/Go/antlr@4.9.3
    $g = -join($g, $gm)
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        }
    }
    $g2 = go build Test.go
    $g = -join($g, $gm)
    return @{
        Message = $g
        Success = $LASTEXITCODE -eq 0
    }
}

function Test-Case {
    param (
        $InputFile,
        $TokenFile,
        $TreeFile,
        $ErrorFile
    )
    $o = trwdog ./Test -file $InputFile
    $failed = $LASTEXITCODE -ne 0
    if ($failed -and $errorFile) {
        return $true
    }
    if(!$failed -and !$errorFile){
        return $true
    }
    return $false
}
