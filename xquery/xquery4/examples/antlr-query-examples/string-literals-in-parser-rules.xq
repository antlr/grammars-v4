(: Find string literals in parser rules and compute text edits :)

declare record lsp:Position(
  line as number,
  character as number
);

declare record lsp:Range(
  "start" as lsp:Position,
  "end" as lsp:Position
);

declare record lsp:TextEdit(
  range as lsp:Range,
  newText as string
);

declare function local:x($acc,$option) {
  array {
    for $prefix in $acc
    return concat(($prefix, $option))
  }
};

declare function local:permuteStrings($groups as array(array(string))) as array(string) {
  let $initial := array { "" }
  return array { ($groups ! array:fold-left(., $initial, local:x#2)) } treat as array(string)
};


declare function is-finite-token($tokenDefinition as element(lexerRuleSpec)) as boolean {
  true()
};

declare function variants($tokenDefinition as element(lexerElement)) as array(string) {
  $tokenDefinition => variants() treat as array(string)
};

declare function variants($tokenDefinition as element(lexerAlt)) as array(string) {
  $tokenDefinition/lexerElements/lexerElement => variants() treat as array(string)
};

declare function variants($tokenDefinition as element(lexerRuleSpec)) as array(string) {
  $tokenDefinition//lexerAlt => variants() => array:join() treat as array(string)
};

declare function lsp:range($def as node()) as lsp:Range {
  lsp:Range(
    { 'line': 0, 'character': 0},
    { 'line': 0, 'character': 0})
};


let $parserTokenRefs := //parserRuleDef//STRING
for $parserTokenRef in $parserTokenRefs
group by $text-representation := $parserTokenRef => string() => substring(2, string-length())
for $tokenDef in //lexerRuleSpec[is-finite-token(.)]
for member $variant in $tokenDef => variants()
let $x := $variant
return if ($text-representation = $variant) then
  let $oneTokenRef := ($parserTokenRef treat as element(STRING))
  return lsp:TextEdit(
    range := $oneTokenRef=>lsp:range(),
    newText := $variant=>string() (: refine semantics  :)
  )
else ()


