(: find unused variables :)
let $vars
    := //varDecl/varNameAndType/varRef/qname
	|  //letClause/letBinding/varNameAndType/varRef/qname
let $varNames := $vars =!> string()
let $visibilities
    := $vars !
        ./parent::varRef/parent::varNameAndType/parent::letBinding/exprSingle
        | ./parent::varRef/parent::varNameAndType/parent::varDecl
let $varRefs := //varRef
for sliding window $varScope in $vars
  start $var at $defPosition
  end $redefinition at $redefPosition
    when $defPosition ne $redefPosition
          and $varNames[$defPosition] = $varNames[$redefPosition]
let $visibility-start := $visibilities[$defPosition]
let $visibility-end := $visibilities[$redefPosition]
let $varname := $varNames[$defPosition]
let $relevantRefsInScope := $varRefs
  [. follows $visibility-start]
  [. precedes-or-is $visibility-end]
  [./qname=!>string() = $varname]
return if ($relevantRefsInScope => empty()) { $var }
