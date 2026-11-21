let $contexta := .
let $test as string := "a"
let $y := upper-case($test)
let $kggg := {"key": "a", "value": "b"}
let $abcd as fn:key-value-pair := {"key": "a", "value": "b"}
let $a := $abcd ? "key"
let $k := //y (: descendant-or-self filtered by name should result in element(y)* 
                            unless the provided grammar analysis specifies otherwise :)
let $k := /y[let $x := . return true()]
let $k := /y//x[let $x := . return true()]
let $k := /*
let $k := //*
for $x in (1, 2, 3)
return $test