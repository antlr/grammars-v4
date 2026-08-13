let $x as number? := 1
let $y as number? := 1
return
    if ($x and (let $a := $x return $y)) then
        let $y := $x
        let $y := $y
        return $x
    else
        let $z := $x
        let $z := $y
        return 1
