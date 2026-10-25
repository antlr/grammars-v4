for $parenthesis in //parenthesis => reverse()
	let $previous := $parenthesis/parent::*
	let $simple := $parenthesis/expr/add/mul/unary/*
	return $parenthesis