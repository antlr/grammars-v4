if ($x gt 0) then $x else -$x ;
switch ($x)
  case "a" return 1
  case "b" return 2
  default return 0 ;
typeswitch ($item)
  case xs:integer return "integer"
  case xs:string return "string"
  default return "other" ;
typeswitch ($node)
  case $e as element() return name($e)
  case text() return "text"
  default return "unknown" ;
try { doc("missing.xml") } catch * { "not found" } ;
try { 1 div 0 } catch err:FOAR0001 { "division by zero" } ;
validate lax { $x } ;
validate strict { $x } ;
