import module namespace lsp = "lsp-positions";

for $a in //arrowExpr
	let $seq := $a/(sequenceArrowTarget|mappingArrowTarget)
	let $start := lsp:start-position($a/unaryExpr treat as node()?)
	for $x in $seq
		return [ $start, lsp:end-position($x) ]
