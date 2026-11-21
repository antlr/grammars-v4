
let $simpleVariants := /ancestor-or-self::
	(
		exprSingle
		| fLWORExpr
		| quantifiedExpr
		| ifExpr
		| switchExpr
		| tryCatchExpr
		| orExpr
		| comparisonExpr
		| otherwiseExpr
		| stringConcatExpr
		| rangeExpr
		| additiveExpr
		| multiplicativeExpr
		| unionExpr
		| intersectExpr
		| instanceofExpr
		| treatExpr
		| castableExpr
		| castExpr
		| pipelineExpr
		| arrowExpr
		| postfixExpr
		| axisStep
		| reverseStep
		| forwardStep
		| functionItemExpr
		| mapConstructor
		| arrayConstructor
		| primaryExpr
		| literal
		| varRef
		| parenthesizedExpr
		| contextValueRef
		| functionCall
		| functionItemExpr
		| mapConstructor
		| arrayConstructor
		| stringConstructor
		| unaryLookup
	) ! [., .]


(:
string() => characters() => x() => y()
        ^
                        ^
                               ^
                                      ^
:)
let $arrowExpressionVariants := (
	for $arrow in //arrowExpr
		let $seq := $arrow/(sequenceArrowTarget|mappingArrowTarget)
		let $start := $arrow/unaryExpr
		for $x in $seq
			return [ $start, $x ]
	)

return ($simpleVariants, $arrowExpressionVariants)
