import module "./lsp-positions.antlrquery";

for $func in //functionDecl
let $args := $func//varNameAndType//varRef =!> string()
for $reference in $func/functionBody//varRef
return if (string($reference) = $args)
	then { 'type': 'variable', 'range': $reference => lsp:range() }
	else { 'type': 'parameter', 'range': $reference => lsp:range() }
