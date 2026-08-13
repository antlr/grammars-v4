let $x := "a"
for tumbling window $w in (1, 2, 3)
    start $s at $si previous $sp next $sn
        when $s = 2
    end $e at $ei
        when $e = 2
return ($x, $w, $s, $si, $e, $ei)
