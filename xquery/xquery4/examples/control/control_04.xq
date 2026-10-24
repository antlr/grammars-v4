typeswitch ($node)
  case $e as element() return name($e)
  case text() return "text"
  default return "unknown"
