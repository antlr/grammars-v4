(:
if (<expr:1> != null)
	return <1>;
a;
b;
return null;
:)

for $if in //ifStatement[
	condition//comparisonExpr
		[operator=!>string() = '!=']
		[rhs=!>string() = 'null']
	]
let $lhs := $if/lhs
let $returns-lhs-true := $if/trueBloc//returnStatement[expr=>exactly-one()=>string() = $lhs=>exactly-one()=>string()]
let $returns-null-false := $if/falseBloc//returnStatement[expr=!>string() = 'null']
let $returns-null-otherwise := $if/following-sibling::*[1]/self::returnStatement[expr=!>string() = 'null']
return {
    "if": $if,
    "returns-lhs-true": $returns-lhs-true,
    "returns-null-false": $returns-null-false,
    "returns-null-otherwise": $returns-null-otherwise
}


