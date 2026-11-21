(: find unused classes :)

let $classNames := //(classDeclaration|normalInterfaceDeclaration)
					/typeIdentifier ! string()
let $uses := //typeIdentifier ! string()
for $cn in $classNames
	let $usecount := $uses[$cn eq .] => count()
	return if ($usecount gt 1) { $cn }
