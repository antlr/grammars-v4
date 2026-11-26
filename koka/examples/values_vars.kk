// val (immutable) vs var (mutable) bindings; reassignment uses ":=".

fun main() {
    val x = 10;
    var y = 20;
    y := y + x;
    println(y.show)
}
