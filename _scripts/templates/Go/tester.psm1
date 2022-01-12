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
    # Output version of pwsh.
    #Get-Host | Select-Object Version | Write-Host
    # Output environmental variables.
    #dir env: | Out-String | Write-Host
    # Output go version
    #go version | Write-Host
    $env:GO111MODULE = "on"
    $g = go get github.com/antlr/antlr4/runtime/Go/antlr@4.9.3
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        }
    }
    $msg = go build Test.go
    return @{
        Message = $msg
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
