// A function-typed parameter and an "fn" lambda passed as a value.

fun applyTwice(f : (int) -> int, x : int) : int {
    f(f(x))
}

fun main() {
    val double = fn(x : int) { x * 2 };
    println(applyTwice(double, 5).show)
}
