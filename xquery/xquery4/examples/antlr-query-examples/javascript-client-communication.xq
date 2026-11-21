(: Js client side of communication  :)

declare function is-simple-property-assignment($rule as element(propertyAssignment)) as boolean
{
  not($rule/propertyName)
  and  not($rule/LPAREN)
  and not(($rule/LBRACKET))
};

let $registeredCommands := //funccall[funcname=!>string() = 'registerCommand']
for $command in $registeredCommands
let $requests := $command/args[2]//funccall[funcname=!>string() = 'sendRequest']
let $requestNames := $requests/args[1],
  $requestMessages := $requests/args[2]
for $message in $requestMessages
let $literals := $message//objectLiteral
let $parameters :=
  $literals//propertyName
  | $literals//propertyAssignment[is-simple-property-assignment(.)]/singleExpression
return ()

