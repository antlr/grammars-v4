# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -Dlanguage=Dart <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    For ($i=0; $i -le 5; $i++) {
        $g = dart pub get
        if($LASTEXITCODE -eq 0){
            Break
        }
        Write-Host "dart pub get failed. Trying again."
    }
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        }
    }
    $msg = dart compile exe cli.dart
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
    $o = trwdog ./cli.exe -file $InputFile
    $failed = $LASTEXITCODE -ne 0
    if ($failed -and $errorFile) {
        return $true
    }
    if(!$failed -and !$errorFile){
        return $true
    }
    return $false
}