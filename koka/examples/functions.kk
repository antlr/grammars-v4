// Function declarations: a block body with typed parameters and an
// explicit return type, and a short "= expr" bodied function.

fun add(x : int, y : int) : int {
    x + y
}

fun square(x : int) : int = x * x

fun main() {
    println(add(2, 3).show)
}
