# Generated from trgen <version>
$(& Remove-Item *.interp -Recurse -Force ) 2>&1 | Out-Null
$files = New-Object System.Collections.Generic.List[string]
<tool_grammar_tuples:{x |
$f = java -jar "<antlr_tool_path>" -depend -encoding <antlr_encoding> -Dlanguage=TypeScript <x.AntlrArgs> <antlr_tool_args:{y | <y> } > <x.GrammarFileName>
foreach ($s in $f) {
    $j = $s.Split(" ")[0]
    $files.Add($j)
\}
foreach ($f in $files)
{
    $(& Remove-Item $f -Force ) 2>&1 | Out-Null
\}
} >
$(& Remove-Item node_modules -Recurse -Force ) 2>&1 | Out-Null
$(& Remove-Item package-lock.json -Recurse -Force ) 2>&1 | Out-Null
exit 0
