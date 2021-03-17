function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -Dlanguage=Dart
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    $g = dart pub get
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
    $o = ./cli.exe -file $InputFile
    $failed = $LASTEXITCODE -ne 0
    if ($failed -and $errorFile) {
        return $true
    }
    if(!$failed -and !$errorFile){
        return $true
    }
    return $false
}