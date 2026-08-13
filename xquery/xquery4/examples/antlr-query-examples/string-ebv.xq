
let $x as string? := "a"
return
if ($x) then
    let $y := $x
    return $x
else
    let $z := $x
    return 1
