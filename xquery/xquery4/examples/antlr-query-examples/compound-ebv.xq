
let $x as (boolean|number)? := 1
return
if ($x) then
    let $y := $x
    return $x
else
    let $z := $x
    return 1
