# Template generated code from trgen <version>
function Build-Grammar {
<tool_grammar_files:{x |
    $g = antlr <x> -Dlanguage=Java <antlr_tool_args:{y | <y> } >
    if($LASTEXITCODE -ne 0){
        return @{
            Message = $g
            Success = $false
        \}
    \}
}>
    Write-Host "javac -cp <antlr_tool_path><if(path_sep_semi)>\;<else>:<endif>. <tool_grammar_tuples:{x|<x.GeneratedFileName> }> Program.java ErrorListener.java"
    $msg = javac -cp "<antlr_tool_path><if(path_sep_semi)>;<else>:<endif>." <tool_grammar_tuples:{x|<x.GeneratedFileName> }> Program.java ErrorListener.java
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
    $o = trwdog java Program -file $InputFile
    $failed = $LASTEXITCODE -ne 0
    if ($failed -and $errorFile) {
        return $true
    }
    if(!$failed -and !$errorFile){
        return $true
    }
    return $false
}