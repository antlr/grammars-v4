(: Plant UML inheritance class diagram from java files :)
for $class in //normalClassDeclaration => outermost()
let $className := $class/typeIdentifier =!> string()
let $extendedClass := $class/classExtends/classType =!> string()
let $implementedClasses := $class/classImplements//interfaceType =!> string()
let $extentions := ``[`{$className}` --|> `{$extendedClass}`]``
let $implementations := $implementedClasses ! ``[`{$className}` --|> `{.}`]``
return (
  $extentions,
  $implementations
)
