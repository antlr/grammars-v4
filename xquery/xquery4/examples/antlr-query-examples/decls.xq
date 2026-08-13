
(: Straightforward detection of unused variables :)
let $declarations := //(varNameAndType|windowVars)//varRef
let $references := //varRef except $declarations
for $declaration in $declarations
let $matching-declarations := $references[. = $declaration]
return
  if ($matching-declarations=>empty()){
    $declaration
  }
