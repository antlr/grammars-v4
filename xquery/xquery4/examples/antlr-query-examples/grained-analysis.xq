let $x as string := '' => fn:exactly-one()
return if ($x instance of enum('a'))
  then let $y := $x return $y
  else let $y := $x return $y

