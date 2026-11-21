import module namespace lsp = "lsp-positions";



(:
If merging
	if (item != null) {							| if (item != null && groupingVars.contains(item.name)) {
		if (groupingVars.contains(item.name)) {	|     ...
			...									| }
		}										|
	}											|
:)
array {
  let $ifs  := //ifExpr
  for $if in $ifs
    let $innerTrue := $if/trueExpr/exprSingle/ifExpr
    let $innerFalse := $if/falseExpr/exprSingle/ifExpr
    let $ending := (($innerFalse otherwise $innerTrue) treat as element(ifExpr))
    return if ($innerTrue and $innerFalse) then
        {
            "range": lsp:Range(
                $if=>lsp:start-position(),
                $ending=>lsp:end-position() treat as lsp:Position), (:TODO: refine asumptions:)
            "target-expression": $if,
            "merged-expression-true": $innerTrue,
            "merged-expression-false": $innerFalse
        }
    else ()
}
