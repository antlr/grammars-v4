# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_tuples:{x |
    $g = antlr <x.GrammarFileName> -encoding <antlr_encoding> -Dlanguage=Go <x.AntlrArgs> <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    # Attempt 7. Explicitly force fail and message with known data
    # and test caller is actually outputing any damn thing.
    # Force failusre as early as possible because I don't know what
    # caller callee are doing whatsoever. Powersucks.
    return ${
        Message = "Just a bunch of garbage."
        Success = $false
    }

    # Output pwsh (or powershell) version.
    # Attempt 1. Does not work--captured.
    # Get-Host | Select-Object Version
    # Attempt 2. Does not work. No output.
    # Get-Host | Select-Object Version | Out-Host
    # Attempt 3. Does not work. No output.
    # Get-Host | Select-Object Version | Out-Host | Write-Host
    # Attempt 4. Does not work. No output.
    #$gm = Get-Host | Select-Object Version
    #$g = -join($g, $gm)
    # Attempt 5.
    Get-Host | Select-Object Version | Write-Error
    # Output environmental variables.
    dir env: | Out-String | Write-Error
    # Output go version
    go version | Write-Error
    #
    $env:GO111MODULE = "on"
    $g = go get github.com/antlr/antlr4/runtime/Go/antlr@4.9.3
    Write-Error $g
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        }
    }
    $g = go build Test.go
    Write-Error $g
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
