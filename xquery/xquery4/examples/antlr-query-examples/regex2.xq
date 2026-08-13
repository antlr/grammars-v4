(:
for $function in //functionCall[./funcname ~~ regex {(fn:?)? (matches|replace|tokenize|analyze-string)}]
let $patternKwarg := $function//keywordArgument[qname=>string() = 'pattern']/argument/exprSingle
let $pattern := if ($patternKwarg => empty())
	then /positionalArguments//argument[2]/exprSingle
	else $patternKwarg
let $pattern-string := getLiteralString($pattern)
return if ($pattern-string => exists()) { $pattern-string => lsp:Range() }
:)
()
