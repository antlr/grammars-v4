module namespace testing = "testing";
declare function testing:recursion($x as number) as number
{
    if ($x > 0) then
        $x + testing:recursion($x - 1)
    else
        0
};

