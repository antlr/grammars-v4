
for $return in //returnStatement
let $conditionalExpression := $return/expression/assignmentExpression/conditionalExpression[./expression]
return if ($conditionalExpression)
    {
``[if (`{$conditionalExpression/conditionalOrExpression}`) {
    return `{$conditionalExpression/expression}`;
} else {
    return `{$conditionalExpression/lambdaExpression otherwise $conditionalExpression/conditionalExpression}`;
}]``
},

for $yield in //yieldStatement
let $conditionalExpression := $yield/expression/assignmentExpression/conditionalExpression[./expression]
return if ($conditionalExpression)
    {
``[if (`{$conditionalExpression/conditionalOrExpression}`) {
    yield `{$conditionalExpression/expression}`;
} else {
    yield `{$conditionalExpression/lambdaExpression
            otherwise $conditionalExpression/conditionalExpression}`;
}]``
    },

for $lvd in //localVariableDeclaration
let $declarators := $lvd//variableDeclarator
for $declarator at $declaratori in $declarators
let $conditionalExpression := $declarator/variableInitializer/expression/assignmentExpression/conditionalExpression[./expression]
return if ($conditionalExpression) {
    let $moved-type := $lvd/localVariableType
    let $moved-modifiers := $lvd/variableModifier
    let $moved-declarators := $declarators[1 to $declaratori - 1]
    let $remaining-declarators := $declarators[$declaratori + 1 to fn:count($declarators)]
    let $varname := $declarators/variableDeclaratorId
    let $previous-decl-line := if ($moved-declarators) {
        ``[`{($moved-modifiers=!>string(), $moved-type=!>string()) => string-join(" ")}` `{$moved-declarators=>string-join(", ")}`;]``
    }
    let $if-part :=
``[
`{($moved-modifiers[. != 'final'], $moved-type) => string-join(" ")}` `{$varname}`;
if (`{$conditionalExpression/conditionalOrExpression}`) {
    `{$varname}` = `{$conditionalExpression/expression}`;
} else {
    `{$varname}` = `{$conditionalExpression/lambdaexpression otherwise $conditionalExpression/conditionalExpression}`;
}
]``
    let $remaining-decl-line := if ($remaining-declarators) {
        ``[`{($moved-modifiers, $moved-type) => string-join(" ")}` `{$remaining-declarators=>string-join(", ")}`;]``
    }
    return
        ($previous-decl-line, $if-part, $remaining-decl-line) => string-join("\n")
}

