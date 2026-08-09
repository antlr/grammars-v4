let $x := "a"
for sliding window $w in (1, 2, 3)
    start $s 
        at $si 
        previous $sp 
        next $sn 
        when $s = 2
    end $e
        at $ei 
        previous $previous
        next $next
        when $e = 2
group by $k := 3
return ($x, $w, $s, $si, $e, $ei, $k)