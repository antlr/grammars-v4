/*
*
* Original Javascript version by David Hedbor(http://www.bagley.org/~doug/shootout/)
*
*/

function fib(n)
{
    if (n < 2) return 1
    return fib(n-2) + fib(n-1)
}

local n = vargv.len()!=0?vargv[0].tointeger():1

print(fib(n)+"\n")
