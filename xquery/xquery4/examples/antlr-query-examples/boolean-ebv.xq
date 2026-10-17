
let $x as boolean? := fn:true()
return
if ($x) then
    let $y := $x
    return $x
else
    let $z := $x
    return 1
