SELECT FILTER([{'k':'v'}], x -> x:k::STRING = 'v');

SELECT FILTER([{'k':1}], x -> x:k = 1);

SELECT FILTER([1, 2, 3], x int -> x = 1);

SELECT FILTER([1, 2, 3], (x int) -> x = 1);

SELECT FILTER([1, 2, 3], (x) -> x = 1);

SELECT FILTER([1, 2, 3], x -> x = 1);

SELECT REDUCE([1,2,3]::ARRAY(INT), 0, (acc int, val int) -> acc + val);

SELECT REDUCE([1,2,3]::ARRAY(INT), 0, (acc, val int) -> acc + val);

SELECT REDUCE([1,2,3]::ARRAY(INT), 0, (acc int, val) -> acc + val);

SELECT REDUCE([1,2,3], 0, (acc, val) -> acc + val);