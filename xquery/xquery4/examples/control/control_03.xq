typeswitch ($item)
  case xs:integer return "integer"
  case xs:string return "string"
  default return "other"
