
declare namespace t = "t";
declare namespace ext = "ext";
declare namespace local = "local";

declare function local:x(
    $x as number+,
    $k as number+,
    $y as empty-sequence() := ()
) as number+
{
    1, 2, $x
};


declare function ext:x() as string external;


declare variable $zmiennazadeklarowana as string := "a";
declare variable $d as string external;

declare type a as enum("a", "b", "c");

declare record t:testowanko(a as string, b as string?, c as string+);


let $k as t:testowanko? := ()
let $p as enum('a') := "a"
let $t := t:testowanko("a", "b", "c")
return
    if (fn:true()) then
        let $x := local:x((1, 2), 3)
        let $lalala := local:x#2
        return $x || ext:x() || $zmiennazadeklarowana || $d
    else ()