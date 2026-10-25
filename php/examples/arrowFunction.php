<?php
$factor = 10;
$a=fn($n)=>$n * $factor;
$nums = array_map(fn($n) => $n * $factor, [1, 2, 3, 4]);