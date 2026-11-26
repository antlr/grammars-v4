// if/then/else as an expression, and plain recursion (no "while" loop
// keyword exists in Koka; iteration is ordinary recursive function
// definitions, or a library function such as std/core's "while").

fun fib(n : int) : int {
    if n < 2 then n
    else fib(n - 1) + fib(n - 2)
}

fun printFibs(n : int) : unit {
    if n > 0 then {
        println(fib(n).show);
        printFibs(n - 1)
    }
}

fun main() {
    printFibs(10)
}
