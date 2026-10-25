
<?php
$data = [
    [1, 'Tom'],
    [2, 'Fred'],
];

// list() style
list($id1, $name1) = $data[0];

// [] style
[$id1, $name1] = $data[0];

// list() style
foreach ($data as list($id, $name)) {
    // logic here with $id and $name
}

// [] style
foreach ($data as [$id, $name]) {
    // logic here with $id and $name
}


$a1=array(1,2,3,4);
$a2=array('a' => 1, 'b' => 2);
$a3=[1,2,];

[,,$b,,] = $a1;

['b' => $c,] = $a2;
['b' => $c, 'c' => $d,] = $a2;

[2 => $d,] = $a1;
[2 => $d, 3 => $e,] = $a1;

[,$a,] = f();
[,,,$a,,$b,,,] = f();
[,$a] = f();

foreach ($a1 as [,,$c,,,$d,,]) {

}

foreach ($a2 as ['a' => $n,]) {

}


class C{
    function g(){
        [self::$sf,,,$c,,,] = array(1,2);
        [,,,$this->f] = array(1,2);
    }
}
