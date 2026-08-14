import module namespace lsp = "lsp-positions";


declare function to-extractable-exprs($split-target as node()*) as node()* {
    $split-target//expr
};


(:
If Splitting
  if (item != null && groupingVars.contains(item.name)) { |   if (item != null) {
      ...                                                 |       if (groupingVars.contains(item.name)) {
  }                                                       |           ...
                                                          |       }
                                                          |   }
:)
array {
  let $ifs  := //ifExpr
  for $if in $ifs
    let $condition := $if/condition => fn:exactly-one()
    for $separate-condition-part in $condition => to-extractable-exprs()
        return
            {
                "range": lsp:range($condition),
                "target-expression": $if,
                "separated-expr": $separate-condition-part,
                "true-expr": $if/trueValue/expr => fn:exactly-one() => lsp:range(),
                "false-expr": $if/falseValue/expr => fn:exactly-one() => lsp:range()
            }
}
