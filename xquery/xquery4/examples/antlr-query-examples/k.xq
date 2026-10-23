for $class in //normalClassDeclaration => outermost()
	let $className := $class/typeIdentifier =!> string()
	let $extendedClass := $class/classExtends/classType =!> string()
	let $implementedClasses := $class/classImplements//interfaceType =!> string()	
return (``[ `{$className}` --|> `{$extendedClass}` ]``,
		$implementedClasses ! ``[{$className} --|> {.}]``)