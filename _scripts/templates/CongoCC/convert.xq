(: Convert a supported subset of a combined ANTLR4 grammar to CongoCC.
   Unsupported nodes fail explicitly. General adaptive prediction and
   precedence are not translated. :)

declare variable $prefix external := '';
declare variable $scanSpecs external := '';

(: CongoCC reserves these spellings while parsing its own grammar language.
   Rename only declared lexer tokens; the built-in EOF reference stays EOF. :)
declare variable $reserved := ('FAIL', 'UNCACHE_TOKENS', 'ACTIVE_TOKENS',
  'ACTIVATE_TOKENS', 'DEACTIVATE_TOKENS', 'ASSERT', 'ENSURE', 'FLAG',
  'SCAN', 'IGNORE_CASE', 'TOKEN', 'REGULAR_TOKEN', 'SPECIAL_TOKEN',
  'UNPARSED', 'MORE', 'SKIP', 'CONTEXTUAL', 'EOF', 'ATTEMPT',
  'RECOVER', 'RECOVER_TO', 'ON_ERROR');

declare function local:token-name($name) {
  if ($name = $reserved and
      fn:exists((/grammarSpec/rules/ruleSpec/lexerRuleSpec,
                 /grammarSpec/modeSpec/lexerRuleSpec)[TOKEN_REF/text() = $name]))
    then fn:concat('ANTLR_', $name) else $name
};

declare function local:token-reference($name) {
  let $aliases := (
    /grammarSpec/rules/ruleSpec/lexerRuleSpec,
    /grammarSpec/modeSpec/lexerRuleSpec
  )[lexerRuleBlock/lexerAltList/lexerAlt/lexerCommands/lexerCommand[
      fn:string(lexerCommandName) = 'type' and
      lexerCommandExpr/identifier/*/text() = $name
    ]]/TOKEN_REF/text()
  let $names := (local:token-name($name),
    for $alias in $aliases return local:token-name(fn:string($alias)))
  return if (fn:count($names) = 1) then fn:concat('<', $names[1], '>')
    else fn:concat('(', fn:string-join(
      for $token in $names return fn:concat('<', $token, '>'), ' | '), ')')
};

declare function local:scan-token-names($text, $names) {
  if (fn:empty($names)) then $text
  else let $name := $names[1]
    return local:scan-token-names(
      fn:replace($text, fn:concat('<', $name, '>'),
        fn:concat('<', local:token-name($name), '>')),
      fn:subsequence($names, 2))
};

declare variable $rules := /grammarSpec/rules/ruleSpec/parserRuleSpec;

declare function local:unsupported($name) {
  fn:error((), fn:concat('CongoCC conversion does not support ', $name))
};

declare function local:escape-quotes($value, $position) {
  if ($position > fn:string-length($value)) then ''
  else
    let $c := fn:substring($value, $position, 1)
    return fn:concat(
      if ($c = '"') then '\"' else $c,
      local:escape-quotes($value, $position + 1))
};

declare function local:quote($value) {
  let $plain := if (fn:string-length($value) = 2 and
    fn:substring($value, 1, 1) = '\' and
    (fn:substring($value, 2, 1) = ']' or
     fn:substring($value, 2, 1) = '[' or
     fn:substring($value, 2, 1) = '-'))
    then fn:substring($value, 2, 1) else $value
  return fn:concat('"', local:escape-quotes($plain, 1), '"')
};

declare function local:literal($node) {
  let $raw := fn:string($node)
  return local:quote(fn:substring($raw, 2, fn:string-length($raw) - 2))
};

declare function local:read-char($body, $position) {
  let $c := fn:substring($body, $position, 1)
  return if ($c = '\' and fn:substring($body, $position + 1, 2) = 'p{')
    then let $tail := fn:substring($body, $position)
      return fn:substring($tail, 1,
        fn:string-length(fn:substring-before($tail, '}')) + 1)
    else if ($c = '\' and fn:substring($body, $position + 1, 1) = 'u')
    then fn:substring($body, $position, 6)
    else if ($c = '\') then fn:substring($body, $position, 2) else $c
};
declare function local:class-items($body, $position) {
  if ($position > fn:string-length($body)) then ()
  else
    let $first := local:read-char($body, $position)
    let $next := $position + fn:string-length($first)
    let $is-range := fn:substring($body, $next, 1) = '-' and
      $next + 1 <= fn:string-length($body)
    let $last := if ($is-range) then local:read-char($body, $next + 1) else ''
    let $end := if ($is-range) then $next + 1 + fn:string-length($last) else $next
    return (
      if (fn:substring($first, 1, 3) = '\p{')
        then fn:concat('__TRASH_UNICODE_',
          fn:substring($first, 4, fn:string-length($first) - 4), '__')
      else if ($is-range)
        then fn:concat(local:quote($first), '-', local:quote($last))
        else local:quote($first),
      local:class-items($body, $end)
    )
};

declare function local:char-set($raw) {
  let $body := fn:substring($raw, 2, fn:string-length($raw) - 2)
  return fn:concat('[', fn:string-join(local:class-items($body, 1), ', '), ']')
};

declare function local:suffix($node) {
  if (fn:exists($node/ebnfSuffix/STAR)) then '*'
  else if (fn:exists($node/ebnfSuffix/PLUS)) then '+'
  else if (fn:exists($node/ebnfSuffix/QUESTION)) then '?'
  else ''
};

(: Choice facts are computed by trinterp's C# grammar analysis pass. :)
declare function local:scan-follow($element) {
  let $name := fn:string($element/atom/ruleref/RULE_REF)
  let $owner := fn:string(($element/ancestor::parserRuleSpec/RULE_REF)[1])
  let $tail := fn:empty($element/following-sibling::element) and
    fn:empty($element/ancestor::element/following-sibling::element)
  let $key := fn:concat($owner, '#E#', $name, '#')
  return if (not($tail)) then () else for $line in fn:tokenize($scanSpecs, '\r?\n')
    where fn:starts-with($line, $key)
    return local:scan-token-names(fn:substring-after($line, $key), $reserved)
};

declare function local:scan-boundary($element) {
  let $name := fn:string($element/atom/ruleref/RULE_REF)
  let $owner := fn:string(($element/ancestor::parserRuleSpec/RULE_REF)[1])
  let $tail := fn:empty($element/following-sibling::element) and
    fn:empty($element/ancestor::element/following-sibling::element)
  let $key := fn:concat($owner, '#N#', $name, '#')
  return if (not($tail)) then () else for $line in fn:tokenize($scanSpecs, '\r?\n')
    where fn:starts-with($line, $key)
    return local:scan-token-names(fn:substring-after($line, $key), $reserved)
};

declare function local:scan-alt($owner, $index) {
  let $key := fn:concat($owner, '#A#', $index, '#')
  return for $line in fn:tokenize($scanSpecs, '\r?\n')
    where fn:starts-with($line, $key)
    return local:scan-token-names(fn:substring-after($line, $key), $reserved)
};
declare function local:scan-syntax-alt($owner, $index) {
  let $key := fn:concat($owner, '#S#', $index, '#')
  return fn:exists(for $line in fn:tokenize($scanSpecs, '\r?\n')
    where $line = $key return $line)
};
declare function local:scan-prefix-alt($owner, $index) {
  let $key := fn:concat($owner, '#P#', $index, '#')
  return for $line in fn:tokenize($scanSpecs, '\r?\n')
    where fn:starts-with($line, $key)
    return local:scan-token-names(fn:substring-after($line, $key), $reserved)
};
declare function local:scan-block($element) {
  let $owner := fn:string(($element/ancestor::parserRuleSpec/RULE_REF)[1])
  let $alt-index := fn:count($element/ancestor::labeledAlt[1]/preceding-sibling::labeledAlt) + 1
  let $element-index := fn:count($element/preceding-sibling::element) + 1
  let $key := fn:concat($owner, '#B#', $alt-index, '#', $element-index, '#')
  return fn:exists(for $line in fn:tokenize($scanSpecs, '\r?\n')
    where $line = $key return $line)
};
declare function local:scan-block-choice($alt) {
  let $element := $alt/ancestor::element[1]
  let $owner := fn:string(($alt/ancestor::parserRuleSpec/RULE_REF)[1])
  let $alt-index := fn:count($element/ancestor::labeledAlt[1]/preceding-sibling::labeledAlt) + 1
  let $element-index := fn:count($element/preceding-sibling::element) + 1
  let $branch-index := fn:count($alt/preceding-sibling::alternative) + 1
  let $key := fn:concat($owner, '#Q#', $alt-index, '#', $element-index,
    '#', $branch-index, '#')
  return if (fn:empty($element)) then () else
    for $line in fn:tokenize($scanSpecs, '\r?\n')
    where fn:starts-with($line, $key)
    return local:scan-token-names(fn:substring-after($line, $key), $reserved)
};
declare function local:parser-alt-list($list) {
  fn:string-join(
    for $alt in $list/alternative
    let $text := fn:string-join(
      for $element in $alt/element
      return local:parser-element($element), ' ')
    let $follow := local:scan-block-choice($alt)
    return if (fn:exists($follow))
      then fn:concat('SCAN (', $text, ' ', $follow, ') => ', $text)
      else $text,
    ' | ')
};

declare function local:parser-element($node) {
  let $atom := $node/atom
  let $block := $node/ebnf/block
  let $body :=
    if (fn:exists($atom/ruleref/RULE_REF)) then fn:concat($prefix, fn:string($atom/ruleref/RULE_REF))
    else if (fn:exists($atom/terminalDef/TOKEN_REF))
      then local:token-reference(fn:string($atom/terminalDef/TOKEN_REF))
    else if (fn:exists($atom/terminalDef/STRING_LITERAL))
      then local:literal($atom/terminalDef/STRING_LITERAL)
    else if (fn:exists($block))
      then let $block-body := local:parser-alt-list($block/altList)
        return if (local:scan-block($node))
          then fn:concat('(SCAN (', $block-body, ') => ', $block-body, ')',
            local:suffix($node/ebnf/blockSuffix))
          else fn:concat('(', $block-body, ')', local:suffix($node/ebnf/blockSuffix))
    else local:unsupported('parser element')
  let $follow := if (local:suffix($node) = '') then () else local:scan-follow($node)
  let $boundary := if (local:suffix($node) = '') then () else local:scan-boundary($node)
  return if (local:suffix($node) = '') then $body
    else if (fn:exists($boundary))
      then fn:concat('(SCAN (ENSURE ~(', fn:string-join($boundary, ' | '), ') ',
        $body, if (fn:exists($follow)) then fn:concat(' (', fn:string-join($follow, ' | '), ')') else '',
        ') => ', $body, ')', local:suffix($node))
    else if (fn:exists($follow))
      then fn:concat('(SCAN (', $body, ' (', fn:string-join($follow, ' | '), ')) => ', $body, ')', local:suffix($node))
    else fn:concat('(', $body, ')', local:suffix($node))
};

declare function local:parser-rule($rule, $nl) {
  let $name := fn:string($rule/RULE_REF)
  let $alts := $rule/ruleBlock/ruleAltList/labeledAlt
  let $recursive := $alts[alternative/element[1]/atom/ruleref/RULE_REF = $name]
  let $base := $alts[not(alternative/element[1]/atom/ruleref/RULE_REF = $name)]
  let $body :=
    if (fn:empty($recursive))
      then fn:string-join(
        for $alt in $alts
        let $text := local:parser-alt-list($alt)
        let $index := fn:count($alt/preceding-sibling::labeledAlt) + 1
        let $follow := local:scan-alt($name, $index)
        let $prefix-guard := local:scan-prefix-alt($name, $index)
        return if (local:scan-syntax-alt($name, $index))
          then fn:concat('SCAN (', $text, ') => ', $text)
          else if (fn:exists($prefix-guard))
          then fn:concat('SCAN ', $prefix-guard, ' => ', $text)
          else if (fn:exists($follow))
          then fn:concat('SCAN (', $text, ' (', $follow, ')) => ', $text)
          else $text,
        fn:concat($nl, '  | '))
    else if (fn:empty($base)) then local:unsupported('a directly left-recursive rule without a base alternative')
    else
      let $base-body := fn:string-join(
        for $alt in $base
        return local:parser-alt-list($alt), ' | ')
      let $tails := fn:string-join(
        for $alt in $recursive
        return fn:string-join(
          for $element in $alt/alternative/element[position() > 1]
          return local:parser-element($element), ' '), ' | ')
      return fn:concat('(', $base-body, ') ((', $tails, '))*')
  return fn:concat($prefix, $name, ' : ', $body, ' ;', $nl)
};
declare function local:lexer-alt-list($list) {
  fn:string-join(
    for $alt in $list/lexerAlt
    return fn:string-join(
      for $element in $alt/lexerElements/lexerElement
      return local:lexer-element($element), ' '),
    ' | ')
};

(: CongoCC rejects an empty-matching fragment definition. ANTLR allows one
   when the enclosing token still consumes input, so expand such references. :)
declare function local:nullable-rule($name, $seen) {
  if ($name = $seen) then false()
  else
    let $rule := (/grammarSpec/rules/ruleSpec/lexerRuleSpec,
                  /grammarSpec/modeSpec/lexerRuleSpec)[TOKEN_REF/text() = $name]
    return fn:exists($rule/lexerRuleBlock/lexerAltList/lexerAlt[
      local:nullable-alt(., ($seen, $name))])
};

declare function local:nullable-alt($alt, $seen) {
  fn:empty($alt/lexerElements/lexerElement[not(local:nullable-element(., $seen))])
};

declare function local:nullable-element($node, $seen) {
  if (local:suffix($node) = '?' or local:suffix($node) = '*') then true()
  else if (fn:exists($node/lexerAtom/terminalDef/TOKEN_REF))
    then local:nullable-rule(fn:string($node/lexerAtom/terminalDef/TOKEN_REF), $seen)
  else if (fn:exists($node/lexerBlock))
    then fn:exists($node/lexerBlock/lexerAltList/lexerAlt[local:nullable-alt(., $seen)])
  else false()
};

declare function local:lexer-cycle($name, $seen) {
  if ($name = $seen) then $name
  else
    let $rule := (/grammarSpec/rules/ruleSpec/lexerRuleSpec,
                  /grammarSpec/modeSpec/lexerRuleSpec)[TOKEN_REF/text() = $name]
    return (
      for $ref in $rule//lexerAtom/terminalDef/TOKEN_REF
      return local:lexer-cycle(fn:string($ref), ($seen, $name)))[1]
};

declare function local:set-element($node) {
  let $range := $node/characterRange/STRING_LITERAL
  return if (fn:exists($node/STRING_LITERAL))
    then local:literal($node/STRING_LITERAL)
    else if (fn:count($range) = 2)
      then fn:concat(local:literal($range[1]), '-', local:literal($range[2]))
    else local:unsupported('negated set element')
};

declare function local:not-set($node) {
  let $members := ($node/blockSet/setElement, $node/setElement)
  return if (fn:count($members) = 1 and fn:exists($members[1]/LEXER_CHAR_SET))
    then fn:concat('~', local:char-set(fn:string($members[1]/LEXER_CHAR_SET)))
    else fn:concat('~[', fn:string-join(
      for $member in $members return local:set-element($member), ', '), ']')
};
declare function local:lexer-element($node) {
  let $atom := $node/lexerAtom
  let $block := $node/lexerBlock
  let $range := $atom/characterRange/STRING_LITERAL
  let $body :=
    if (fn:exists($atom/LEXER_CHAR_SET)) then local:char-set(fn:string($atom/LEXER_CHAR_SET))
    else if (fn:exists($atom/terminalDef/STRING_LITERAL))
      then local:literal($atom/terminalDef/STRING_LITERAL)
    else if (fn:exists($atom/terminalDef/TOKEN_REF))
      then let $ref := fn:string($atom/terminalDef/TOKEN_REF)
        let $rule := (/grammarSpec/rules/ruleSpec/lexerRuleSpec,
                      /grammarSpec/modeSpec/lexerRuleSpec)[TOKEN_REF/text() = $ref]
        return if (fn:exists($rule/FRAGMENT) and local:nullable-rule($ref, ()))
          then fn:concat('(', local:lexer-alt-list($rule/lexerRuleBlock/lexerAltList), ')')
          else fn:concat('<', local:token-name($ref), '>')
    else if (fn:count($range) = 2)
      then fn:concat('[', local:literal($range[1]), '-', local:literal($range[2]), ']')
    else if (fn:exists($atom/notSet)) then local:not-set($atom/notSet)
    else if (fn:exists($atom/wildcard/DOT)) then '~[]'
    else if (fn:exists($block))
      then fn:concat('(', local:lexer-alt-list($block/lexerAltList), ')')
    else local:unsupported(fn:concat('lexer element ', fn:string($node),
      ' in ', fn:string(($node/ancestor::lexerRuleSpec/TOKEN_REF)[1])))
  return if (local:suffix($node) = '') then $body
    else fn:concat('(', $body, ')', local:suffix($node))
};

declare function local:lexer-rule($rule, $nl) {
  let $alts := $rule/lexerRuleBlock/lexerAltList/lexerAlt
  let $name := local:token-name(fn:string($rule/TOKEN_REF))
  let $commands := $alts/lexerCommands/lexerCommand
  let $state := fn:string($rule/parent::modeSpec/identifier/*[1])
  let $state-prefix := if ($state = '') then '' else fn:concat('<', $state, '> ')
  let $mode-commands := $commands[fn:string(lexerCommandName) = 'mode']
  let $mode-targets := fn:distinct-values(
    for $command in $mode-commands
    let $target := fn:string($command/lexerCommandExpr/identifier/*[1])
    return if ($target = 'DEFAULT_MODE') then 'DEFAULT' else $target)
  let $next-state := if (fn:empty($mode-targets)) then ''
    else fn:concat(' : ', $mode-targets[1])
  let $unsupported-commands := $commands[not(
    (fn:string(lexerCommandName) = 'skip' and not(lexerCommandExpr)) or
    (fn:string(lexerCommandName) = 'channel' and
      lexerCommandExpr/identifier/TOKEN_REF = 'HIDDEN') or
    (fn:string(lexerCommandName) = 'type' and
      fn:exists(lexerCommandExpr/identifier/TOKEN_REF)) or
    (fn:string(lexerCommandName) = 'mode' and
      fn:exists(lexerCommandExpr/identifier/*)))]
  let $skip-alts := $alts[lexerCommands/lexerCommand[
    (fn:string(lexerCommandName) = 'skip' and not(lexerCommandExpr)) or
    (fn:string(lexerCommandName) = 'channel' and
      lexerCommandExpr/identifier/TOKEN_REF = 'HIDDEN')]]
  let $nongreedy := $rule//lexerElement/ebnfSuffix[
    (fn:exists(STAR) or fn:exists(PLUS)) and fn:exists(QUESTION)
    or fn:count(QUESTION) = 2]
  let $lazy := fn:count($nongreedy) = 1 and
    fn:exists($nongreedy[fn:exists(STAR) and ../lexerAtom/wildcard/DOT])
  let $skip := fn:count($skip-alts) = fn:count($alts)
  let $regex := local:lexer-alt-list($rule/lexerRuleBlock/lexerAltList)
  return if (fn:exists($rule/FRAGMENT) and
    local:nullable-rule(fn:string($rule/TOKEN_REF), ())) then ''
  else if (fn:exists($nongreedy) and (not($lazy) or fn:exists($rule/FRAGMENT)))
    then local:unsupported(fn:concat('non-greedy lexer element in ', $name))
  else if (fn:exists($unsupported-commands) or fn:count($mode-targets) > 1 or
    (fn:exists($skip-alts) and not($skip)) or
    (fn:exists($mode-commands) and fn:count($mode-commands) != fn:count($alts)))
    then local:unsupported(fn:concat('lexer command combination in ', $name))
    else if (fn:exists($rule/FRAGMENT))
      then fn:concat($state-prefix, 'TOKEN : <#', $name, ' : ', $regex,
        '>', $next-state, ' ;', $nl)
    else if ($skip)
      then fn:concat($state-prefix, 'SKIP : <', if ($lazy) then '?' else '',
        $name, ' : ', $regex, '>', $next-state, ' ;', $nl)
    else fn:concat($state-prefix, 'TOKEN : <', if ($lazy) then '?' else '',
      $name, ' : ', $regex, '>', $next-state, ' ;', $nl)
};

let $grammar := /grammarSpec
let $name := fn:string($grammar/grammarDecl/identifier/*[1])
let $recursive := (for $rule in ($grammar/rules/ruleSpec/lexerRuleSpec,
                                $grammar/modeSpec/lexerRuleSpec)
  return local:lexer-cycle(fn:string($rule/TOKEN_REF), ()))[1]
let $nl := '
'
return if (fn:count($grammar) != 1 or
  fn:exists($grammar/grammarDecl/grammarType/LEXER) or
  fn:exists($grammar/grammarDecl/grammarType/PARSER))
  then local:unsupported('anything other than one combined ANTLR4 grammar')
  else if (fn:exists($recursive))
    then local:unsupported(fn:concat('recursive lexer rules (for example ',
      $recursive, '); CongoCC NFA generation overflows on this grammar'))
  else fn:string-join((
    'PARSER_PACKAGE=generated;', $nl,
    'PARSER_CLASS=', $name, 'Parser;', $nl, $nl,
    'SMART_NODE_CREATION=false;', $nl, $nl,
    if (fn:exists($grammar/prequelConstruct/optionsSpec/option[
      fn:string(identifier) = 'caseInsensitive' and
      fn:normalize-space(fn:string(optionValue)) = 'true']))
      then fn:concat('IGNORE_CASE=true;', $nl, $nl) else '',
    fn:string-join(
      for $rule in ($grammar/rules/ruleSpec/lexerRuleSpec,
                    $grammar/modeSpec/lexerRuleSpec)
      return local:lexer-rule($rule, $nl), ''), $nl,
    fn:string-join(
      for $rule in $grammar/rules/ruleSpec/parserRuleSpec
      return local:parser-rule($rule, $nl), $nl)
  ), '')
