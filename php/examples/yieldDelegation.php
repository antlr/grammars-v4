<?php
function gen()
{
    yield 1;
    yield 2;
    yield from gen2();
}

function gen2()
{
    yield 3;
    yield 4;
    yield from [5,6];
    yield from new ArrayIterator([7, 8]);
}

foreach (gen() as $val)
{
    echo $val, PHP_EOL;
}
