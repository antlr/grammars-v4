for $for in //enchancedForStatement
	let $requiredStrategy := $for/localVariableDeclaration/expression=>strategy()
	let $variables := $for/localVariableDeclaration//variableDeclaratorId
	let $strategy := switch($requiredStrategy)
		case "iterable" return ()
		case "collections" return ()
		default return ()
	return $strategy



(: forStatementNoShortIf :)
